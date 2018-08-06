unit LeslieDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, DAV_Types, DAV_VSTModule,
  DAV_DspLeslie;

type
  TLeslieDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamSpeedDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLowThrobChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighThrobChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterXOverChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLowWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLeslieSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FLeslie        : TLeslieRotator;
    FLeslieSpeed   : TLeslieSpeed;
    {$IFDEF OriginalCode}
    FLowWidth      : Single;
    FLowThrob      : Single;
    FHighWidth     : Single;
    FHighDepth     : Single;
    FHighThrob     : Single;
    FCrossover     : Single;
    FOutputGain    : Single;
    FSpeed         : Single;
    FInvSampleRate : Single;

    FGain   : Single;
    FFilo   : Single;
    FLWid   : Single;
    FLLev   : Single;
    FHWid   : Single;
    FHDep   : Single;
    FHLev   : Single;
    FHiMom  : Single;
    FLoMom  : Single;
    FHiSet  : Single;
    FLoSet  : Single;
    FHMom   : Single;
    FLMom   : Single;
    FHSet   : Single;
    FLSet   : Single;
    FLSpd   : Single;
    FHSpd   : Single;
    FLPhi   : Single;
    FHPhi   : Single;
    FSpd    : Single;
    FHPos   : Integer;
    FHBuf   : PDAVSingleFixedArray;
    FBuf    : array [0..1] of Single;

    procedure GainChanged;
    procedure SpeedParametersChanged;
    procedure MomChanged;
    {$ENDIF}
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Approximations, DAV_VSTPrograms;

procedure TLeslieDataModule.VSTModuleOpen(Sender: TObject);
begin
 FLeslie := TLeslieRotator.Create;
 FLeslie.SampleRate := SampleRate;

 {$IFDEF OriginalCode}
 FGain := 0;
 FFilo := 0;
 FLWid := 0;
 FLLev := 0;
 FHWid := 0;
 FHDep := 0;
 FHLev := 0;
 FHiMom := 0;
 FLoMom := 0;
 FHiSet := 0;
 FLoSet := 0;
 FLSpd := 0;
 FHSpd := 0;
 FLPhi := 0;
 FHPhi := 1.6;

 FHPos := 0;
 GetMem(FHBuf, 256 * SizeOf(Single));
 {$ENDIF}

 Parameter[0] := 0.66;
 Parameter[1] := 50;
 Parameter[2] := 60;
 Parameter[3] := 70;
 Parameter[4] := 60;
 Parameter[5] := 70;
 Parameter[6] := 0.48;
 Parameter[7] := 0;
 Parameter[8] := 100;

 with Programs[1] do
  begin
   Parameter[0] := 0.33;
   Parameter[1] := 0.50;
   Parameter[2] := 0.60;
   Parameter[3] := 0.70;
   Parameter[4] := 0.75;
   Parameter[5] := 0.57;
   Parameter[6] := 0.48;
   Parameter[7] := 0;
   Parameter[8] := 100;
  end;

 with Programs[2] do
  begin
   Parameter[0] := 0.66;
   Parameter[1] := 0.50;
   Parameter[2] := 0.60;
   Parameter[3] := 0.70;
   Parameter[4] := 0.6;
   Parameter[5] := 0.7;
   Parameter[6] := 0.48;
   Parameter[7] := 0;
   Parameter[8] := 100;
  end;

 {$IFDEF OriginalCode}
 if SampleRate > 0
  then FInvSampleRate := 1 / SampleRate;
 {$ENDIF}
end;

procedure TLeslieDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FLeslie);

 {$IFDEF OriginalCode}
 Dispose(FHBuf);
 {$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////

procedure TLeslieDataModule.ParameterLeslieSpeedChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Value < 0.1 then FLeslieSpeed := lsStop else
 if Value > 0.5
  then FLeslieSpeed := lsFast
  else FLeslieSpeed := lsSlow;

 if Assigned(FLeslie)
  then FLeslie.LeslieSpeed := FLeslieSpeed;

 {$IFDEF OriginalCode}
 case FLeslieSpeed of
  lsStop :
   begin
    FLoSet := 0.00; FHiSet := 0.0;
    FLoMom := 0.12; FHiMom := 0.1;
   end;
  lsSlow :
   begin
    FLoSet := 0.49; FHiSet := 0.66;
    FLoMom := 0.27; FHiMom := 0.18;
   end;
  lsFast :
   begin
    FLoSet := 5.31; FHiSet := 6.40;
    FLoMom := 0.14; FHiMom := 0.09;
   end;
 end;

 MomChanged;
 SpeedParametersChanged;
 {$ENDIF}

end;

procedure TLeslieDataModule.ParameterLowWidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLeslie)
  then FLeslie.LowWidth := Value;

 {$IFDEF OriginalCode}
 if FLowWidth <> Value then
  begin
   FLowWidth := Value;
   FLWid := Sqr(0.01 * FLowWidth);
  end;
 {$ENDIF}
end;

procedure TLeslieDataModule.ParameterLowThrobChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLeslie)
  then FLeslie.LowThrob := Value;

 {$IFDEF OriginalCode}
 if FLowThrob <> Value then
  begin
   FLowThrob := Value;
   FLLev := FGain * 0.9 * Sqr(0.01 * FLowThrob);
  end;
 {$ENDIF}
end;

procedure TLeslieDataModule.ParameterHighWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLeslie)
  then FLeslie.HighWidth := Value;

 {$IFDEF OriginalCode}
 if FHighWidth <> Value then
  begin
   FHighWidth := Value;
   FHWid := sqr(0.01 * FHighWidth);
  end;
 {$ENDIF}
end;

procedure TLeslieDataModule.ParameterHighDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLeslie)
  then FLeslie.HighDepth := Value;

 {$IFDEF OriginalCode}
 if FHighDepth <> Value then
  begin
   FHighDepth := Value;
   FHDep := sqr(0.01 * FHighDepth) * SampleRate / 760;
  end;
 {$ENDIF}
end;

procedure TLeslieDataModule.ParameterHighThrobChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLeslie)
  then FLeslie.HighThrob := Value;

 {$IFDEF OriginalCode}
 if FHighThrob <> Value then
  begin
   FHighThrob := Value;
   FHLev := FGain * 0.9 * sqr(0.01 * FHighThrob);
  end;
 {$ENDIF}
end;

procedure TLeslieDataModule.ParameterXOverChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLeslie)
  then FLeslie.Crossover := Value;

 {$IFDEF OriginalCode}
 if FCrossover <> Value then
  begin
   FCrossover := Value;
   FFilo := 1 - Power(10, FCrossover * (2.27 - 0.54 * FCrossover) - 1.92);
  end;
 {$ENDIF}
end;

procedure TLeslieDataModule.ParameterOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLeslie)
  then FLeslie.OutputGain := Value;

 {$IFDEF OriginalCode}
 if FOutputGain <> Value then
  begin
   FOutputGain := Value;
   FGain := dB_to_Amp(Value);
   GainChanged;
  end;
 {$ENDIF}
end;

procedure TLeslieDataModule.ParameterSpeedChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLeslie)
  then FLeslie.Speed := Value;

 {$IFDEF OriginalCode}
 if FSpeed <> Value then
  begin
   FSpeed := Value;
   SpeedParametersChanged;
  end;
 {$ENDIF}
end;

{$IFDEF OriginalCode}
procedure TLeslieDataModule.GainChanged;
begin
 FLLev := FGain * 0.9 * sqr(0.01 * FLowThrob);
 FHLev := FGain * 0.9 * sqr(0.01 * FHighThrob);
end;

procedure TLeslieDataModule.SpeedParametersChanged;
begin
 FSpd := 4 * Pi * (0.01 * FSpeed) * FInvSampleRate;
 FHSet := FHiSet * FSpd;
 FLSet := FLoSet * FSpd;
end;

procedure TLeslieDataModule.MomChanged;
begin
 FHMom := Power(10, -FInvSampleRate / FHiMom);
 FLMom := Power(10, -FInvSampleRate / FLoMom);
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////

procedure TLeslieDataModule.ParamSpeedDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
const
  CLeslieSpeedText : array [TLeslieSpeed] of string = ('STOP', 'SLOW', 'FAST');
begin
 Predefined := CLeslieSpeedText[FLeslieSpeed];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TLeslieDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  a, c, d, g, h, l           : Single;
  fo, fb1, fb2               : Single;
  hl, hs, ht, hm, hp, hw, hd : Single;
  ll, ls, lt, lm, lp, lw     : Single;
  hdd, hdd2, k, hps          : Integer;
  dchp, dclp, dshp, dslp     : Single;
  hint, chp, clp, shp, slp   : Single;
  Sample                     : Integer;
const
  COne32th : Single = 0.03125;
begin
 for Sample := 0 to SampleFrames - 1
  do FLeslie.Process(Inputs[0, Sample] + Inputs[1, Sample],
    Outputs[0, Sample], Outputs[1, Sample]);

 {$IFDEF OriginalCode}
 g   := FGain;
 fo  := FFilo;
 fb1 := FBuf[0];
 fb2 := FBuf[1];
 hl  := FHLev;
 hs  := FHSpd;
 hm  := FHMom;
 hp  := FHPhi;
 hw  := FHWid;
 hd  := FHDep;
 ll  := FLLev;
 ls  := FLSpd;
 lm  := FLMom;
 lp  := FLPhi;
 lw  := FLWid;
 k   := 0;
 hps := FHPos;

 // target speeds
 ht := FHSet * (1 - hm);
 lt := FLSet * (1 - lm);

 // set LFO values
 chp := cos(hp);
 chp := sqr(chp) * chp;
 clp := cos(lp);
 shp := sin(hp);
 slp := sin(lp);
 dchp := 0;
 dclp := 0;

 for Sample := 0 to SampleFrames - 1 do
  begin
   a := inputs[0, Sample] + inputs[1, Sample]; //mono input

   //linear piecewise approx to LFO waveforms
   if k > 0 then dec(k) else
    begin
     ls := (lm * ls) + lt; //tend to required speed
     hs := (hm * hs) + ht;
     lp := lp + 32 * ls;
     hp := hp + 32 * hs;

     dchp := cos(hp + COne32th * hs);
     dchp := COne32th * (sqr(dchp) * dchp - chp);  //sin^3 level mod
     dclp := COne32th * (cos(lp + 32 * ls) - clp);
     dshp := COne32th * (sin(hp + 32 * hs) - shp);
     dslp := COne32th * (sin(lp + 32 * ls) - slp);

     k := 32;
    end;

   // crossover
   fb1 := fo * (fb1 - a) + a;
   fb2 := fo * (fb2 - fb1) + fb1;

   // volume
   h := (g - hl * chp) * (a - fb2);
   l := (g - ll * clp) * fb2;

   // delay input pos
   if hps > 0
    then dec(hps)
    else hps := 200;

   // delay output pos
   hint := hps + hd * (1 + chp);
   hdd  := Round(hint);

   // linear intrpolation
   hint := hint - hdd;
   hdd2 := hdd + 1;
   if (hdd > 199) then
    begin
     if (hdd > 200)
      then hdd := hdd - 201;
     hdd2 := hdd2 - 201;
    end;

   //delay input
   FHBuf[hps] := h;
   a := FHBuf[hdd];

   //delay output
   h := h + a + hint * (FHBuf[hdd2] - a);

   c := l + h;
   d := l + h;
   h := h * hw * shp;
   l := l * lw * slp;
   d := d + l - h;
   c := c + h - l;

   // output
   outputs[0, Sample] := c;
   outputs[1, Sample] := d;

   chp := chp + dchp;
   clp := clp + dclp;
   shp := shp + dshp;
   slp := slp + dslp;
  end;

 FLSpd := ls;
 FHSpd := hs;
 FHPos := hps;
 FLPhi := (lp + (32 - k) * ls);
 while FLPhi > 2 * Pi do FLPhi := FLPhi - 2 * Pi;
 FHPhi := (hp + (32 - k) * hs);
 while FHPhi > 2 * Pi do FHPhi := FHPhi - 2 * Pi;

 // catch denormals
 if (abs(fb1) > 1E-10) then FBuf[0] := fb1 else FBuf[0] := 0;
 if (abs(fb2) > 1E-10) then FBuf[1] := fb2 else FBuf[1] := 0;
 {$ENDIF}
end;

procedure TLeslieDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Assigned(FLeslie)
  then FLeslie.SampleRate := SampleRate;

 {$IFDEF OriginalCode}
 if SampleRate > 0
  then FInvSampleRate := 1 / SampleRate;
 SpeedParametersChanged;
 MomChanged;
 {$ENDIF}
end;

end.
