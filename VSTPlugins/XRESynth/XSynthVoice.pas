unit XSynthVoice;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  DAV_Complex,classes,XMoogFilter, XOscillator,UIXSynthModel, UXPluginBase, USampleValue;

{$i Consts.inc}

type
  TADSRStage = (adsrAttack, adsrDecay, adsrSustain, adsrRelease);

  TXSynthVoice = class(TXVoice)
  private
    FNoteLevel  : Single;
    FOscillators : array [0..4] of TXOscillator;
    FADSRStageVCA: TADSRStage;
    FADSRGainVCA : Single;
    FADSRStageVCF: TADSRStage;
    FADSRGainVCF : Single;
    FFilter : TMoogFilter;
    Fstartpitch:integer;
    FTicks:integer;
    FReleaseQuickADSRRange:Single;
    FXSynthModel:IXSynthModel;

    procedure ProcessADSR(var FADSRStage: TADSRStage; VAR FADSRGain:Single; A, D, S, R: Single);
    function LFODelay(enabled: boolean; value, delay: single): single;
    function GetOscFrequency(osc: integer;VAR currPitch:single): single;
  protected
    procedure SampleRateChanged; override;
    procedure ReleaseQuick;override;

  public
    constructor Create(Xplugin:TXPluginBase;XSynthModel:IXSynthModel);
    procedure NoteOn(pitch,startpitch:integer; Amplitude: Single);override;
    function Process: TSampleValue; override;

  end;

implementation

uses
  Math, SysUtils, DAV_Common, DAV_Types, DAV_Math, XSynthModule;

{ TXSynthVoice }

constructor TXSynthVoice.Create(Xplugin:TXPluginBase;XSynthModel:IXSynthModel);
var i : Integer;
begin
  inherited Create(XPlugin);
  FXSynthModel:=XSynthModel;
  FFilter:=TMoogFilter.Create(SampleRate);
  for i := 0 to 4 do
     FOscillators[i] := TXOscillator.Create(SampleRate,false);
end;

procedure TXSynthVoice.ProcessADSR(VAR FADSRStage:TADSRStage; VAR FADSRGain:Single; A,D,S,R:Single);
begin
  if Released then FADSRStage:=adsrRelease;
 case FADSRStage of
  adsrAttack  : begin
                 FADSRGain := FADSRGain + A * (1 - FADSRGain);
                 if FADSRGain > 0.999
                  then FADSRStage := adsrDecay;
                end;
  adsrDecay   : begin
                 FADSRGain := FADSRGain - D * (FADSRGain - S);
                 if FADSRGain < S
                  then
                  begin
                    FADSRStage := adsrSustain;
                    FADSRGain:=S;
                  end;
                end;
  adsrSustain : begin
                  FADSRGain := S;
                  // make sure ADSRGAin <> 0, because the note will be ended, even if you slide sustain up
                end;
  adsrRelease : begin
                 FADSRGain := FADSRGain - R * FADSRGain;
                 if FADSRGain < 0.001
                  then FADSRGain := 0;
                end;
 end;
end;

procedure TXSynthVoice.ReleaseQuick;
begin
  inherited;
  FReleaseQuickADSRRange := FADSRGainVCA;
end;

procedure TXSynthVoice.SampleRateChanged;
VAR osc:integer;
begin
 if FOscillators[0] = nil then exit;
 for osc:=0 to 4 do
     FOscillators[osc].SampleRate := SampleRate;
 FFilter.SampleRate:=SampleRate;
end;

function TXSynthVoice.LFODelay(enabled:boolean;value:single;delay:single):single;
VAR t:single;
begin
  if (not enabled) or (delay<0.01) then result:=value
  else
  begin
    // maximum delay time = 5 seconds
    t:= FTicks * SampleReci * 0.2 / delay;
    if t>1 then t:=1;
    result:=t*value;
  end;
end;

function TXSynthVoice.GetOscFrequency(osc:integer;VAR currPitch:single):single;
VAR depth,ratio:single;
    spitch,timeToGlide:single;
begin
  if CanGlide then
  begin
    depth:=FXSynthModel.GetGlide;
    if depth < 1/128 then // No Glide...
      ratio:=1
    else
    begin
      timeToGlide:=abs(Pitch-FStartPitch) * depth / 12; // in Seconds...
      ratio:=FTicks / (timeToGlide * SampleRate);
      if ratio>=1 then ratio:=1;
    end;
  end
  else ratio:=1;
  spitch:=Pitch*ratio+FStartPitch*(1-ratio);
  currPitch:=spitch;
  result:=FXSynthModel.GetOscFrequency(osc,spitch);
end;

function TXSynthVoice.Process: TSampleValue;
var
  idummy,iosc,ilfo,noisecolor: integer;
  enabled,ZeroCrossed,sync:boolean;
  lfo:array[0..1] of single;
  gain,t,vcf,vca,A,D,S,R,rate,currPitch:single;
begin
  // LFOs
  inc(FTicks);
  for ilfo:=0 to 1 do
  begin
   lfo[ilfo]:=FXSynthModel.GetLFOValue(ilfo);
   FXSynthModel.GetLFODelay(ilfo,enabled,rate);
   lfo[ilfo]:=LFODelay(enabled,lfo[ilfo],rate);
  end;
  // Oscillators

  result.zero;
  ZeroCrossed:=false;
  for iosc:=0 to 4 do
  begin
    // OSC LFO modulation
    t:=0;
    for ilfo:=0 to 1 do
      t:=t+(lfo[ilfo])*FXSynthModel.GetOscModDepth(iosc,ilfo) / 2;

    // t ligt tussen -0.5 en 0.5 en dat betekent 1 octaaf max...
    FOscillators[iosc].WaveShape:=FXSynthModel.GetOscWaveshape(iosc,noisecolor);
    FOscillators[iosc].NoiseColor:=noisecolor;
    FOscillators[iosc].PulseWidth:=FXSynthModel.GetOscPulseWidth(iosc);
    FOscillators[iosc].Frequency:= GetOscFrequency(iosc,currPitch)*(1+t);

    // sync, see spec, onderaan
    FOscillators[iosc].DoWTF:=false;
    sync:=FXSynthModel.GetSync(iosc);
    case iosc of
      0,2:  // Flow
            if sync then
            begin
              FOscillators[iosc].DoWTF:=true;
              FOscillators[iosc].WTFWaveShape:=FXSynthModel.GetOscWaveshape(iosc+1,idummy);
            end;
      1,3: // HardSync: check if the previous oscillator crossed zero, then resetphase
           if (ZeroCrossed) and sync then FOscillators[iosc].ResetPhase;
    end;
    result.add(FOscillators[iosc].Process(ZeroCrossed)*FXSynthModel.GetOscLevel(iosc));
  end;
  // Ring Modulation
  result.add(FXSynthModel.GetRingLevel*FOscillators[0].Value*FOscillators[1].value);
  // FM Modulation, Dit is niks
  // result:=result+XSynthModel.GetRingLevel*FOscillators[1].ValueAt(FOscillators[0].value);

  // VCA
  if IsQuickReleasing then
  begin
    FReleaseQuickADSRRange:=FReleaseQuickADSRRange-0.0003;
//    if FReleaseQuickADSRRange<0 then FReleaseQuickADSRRange:=0;
    FADSRGainVCA:=FReleaseQuickADSRRange;
    gain:=1;
  end
  else
  begin
    FXSynthModel.GetConvertedVCA(A,D,S,R,gain);
    ProcessADSR(FADSRStageVCA,FADSRGainVCA,A,D,S,R);
  end;
  vca:=FADSRGainVCA*gain;
  if (FADSRGainVCA <= 0) then
  begin
    result.zero;
    FXSynthModel.OnVoiceDone(self);
    exit;
  end;
  // VCF
  FXSynthModel.GetConvertedVCF(A,D,S,R,gain);
  ProcessADSR(FADSRStageVCF,FADSRGainVCF,A,D,S,R);
  vcf:=FADSRGainVCF*gain;
  // VCF LFO & Filter
  t:=0;
  for ilfo:=0 to 1 do
      t:=t+(lfo[ilfo])*FXSynthModel.GetVCFModDepth(ilfo) / 2;
  FFilter.Cutoff:=FXSynthModel.GetCutoff(currPitch)*vcf*(1+t);
  FFilter.Resonance:=FXSynthModel.GetResonance;
  result:=FFilter.Process(result);

  // VCA LFO & Amp & Volume
  t:=0;
  for ilfo:=0 to 1 do
      t:=t+(lfo[ilfo])*FXSynthModel.GetVCAModDepth(ilfo) / 2;
  result.mul(vca*(1+t)*FXSynthModel.GetLevel*FNoteLevel);
end;

procedure TXSynthVoice.NoteOn(pitch,startpitch:integer; Amplitude: Single);
begin
  inherited;
  FFilter.Reset;
  FADSRStageVCA  := adsrAttack;
  FADSRGainVCA   := 0;
  FADSRStageVCF  := adsrAttack;
  FADSRGainVCF   := 0;
  FTicks    := 0;
  FStartPitch:=startpitch;
  FNoteLevel := Amplitude;
end;

end.
