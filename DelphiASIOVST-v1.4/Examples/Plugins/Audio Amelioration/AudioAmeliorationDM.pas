unit AudioAmeliorationDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, DAV_Types, DAV_VSTModule, DAV_DspExciter, DAV_DspAmbience, 
  DAV_DspCrosstalkSimulator, DAV_DspLightweightDynamics, 
  DAV_DspPsychoacousticBassEnhancer, DAV_DspFilterChebyshevType1;

const
  CNumFrequencies = 10;
  CFrequencyArray : array [0..CNumFrequencies - 1] of Single = (31.25, 62.5,
    125, 250, 500, 1000, 2000, 4000, 8000, 16000);

type
  TDownsampleFilterRecord = record
    Lowpass      : TChebyshev1LowpassFilter;
    Highpass     : TChebyshev1HighpassFilter;
    Downsampling : Integer;
    RMS          : Double;
  end;

  TAudioAmeliorationModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessSpeaker(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessHeadphones(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessBypass(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterSpeakerDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure Parameter3DSoundChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAmbienceActiveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAmbienceChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorActiveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterExciterActiveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterExciterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterExtraBassActiveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterExtraBassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterPowerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSpeakerChangedChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FAmbience           : TAmbience;
    FCompressor         : TLightweightSoftKneeCompressor;
    FLimiter            : TLightweightSoftKneeLimiter;
    FCrosstalkSimulator : TIIRCrosstalkSimulator;
    FAmbienceActive     : Boolean;
    FExciter            : array [0..1] of TExciter;
    FBassEnhancer       : array [0..1] of THarmonicBass;
    FIsSpeaker          : Boolean;
    FExciterActive      : Boolean;
    FSourroundActive    : Boolean;
    FCompressorActive   : Boolean;
    FExtraBassActive    : Boolean;
    FPowerActive        : Boolean;

    FDownSampleCount    : Integer;
    FDownSampleMax      : Integer;
    FBandReserve        : Double;
    FUseDownsampling    : Boolean;

    FFilterArray        : array [0..CNumFrequencies - 1] of TDownsampleFilterRecord;
    FFSGain             : Single;
    FSpeedConst         : array [0..1] of Single;

    function GetBandReserve: Single;
    function GetBandRMS(Index: Integer): Single;
    procedure SetBandReserve(const Value: Single);
    procedure SetUseDownsampling(const Value: Boolean);
  protected
    procedure CalculateSmoothingFactor;
    procedure ChooseProcess;
    procedure DownsamplingChanged;
    procedure UpdateFilters;
  public
    property IsSpeaker: Boolean read FIsSpeaker;
    property PowerActive: Boolean read FPowerActive;
    property ExciterActive: Boolean read FExciterActive;
    property AmbienceActive: Boolean read FAmbienceActive;
    property SourroundActive: Boolean read FSourroundActive;
    property CompressorActive: Boolean read FCompressorActive;
    property ExtraBassActive: Boolean read FExtraBassActive;

    // Analyser
    property BandReserve: Single read GetBandReserve write SetBandReserve;
    property UseDownsampling: Boolean read FUseDownsampling write SetUseDownsampling default True;
    property BandRMS[Index: Integer]: Single read GetBandRMS;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, DAV_DspDynamics, DAV_Approximations, AudioAmeliorationGUI;

procedure TAudioAmeliorationModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 // create and setup bass enhancer
 for ChannelIndex := 0 to Length(FExciter) - 1 do
  begin
   FExciter[ChannelIndex] := TExciter.Create;
   FExciter[ChannelIndex].SampleRate := SampleRate;
  end;

 // create and setup ambience
 FAmbience := TAmbience.Create;
 FAmbience.SampleRate := SampleRate;

 // create and setup crosstalk simulator
 FCrosstalkSimulator := TIIRCrosstalkSimulator.Create;
 FCrosstalkSimulator.Model := csmIRCAM;
 FCrosstalkSimulator.SampleRate := SampleRate;

 // create and setup compressor
 FCompressor := TLightweightSoftKneeCompressor.Create;
 FCompressor.SampleRate := SampleRate;

 // create and setup bass enhancer
 for ChannelIndex := 0 to Length(FBassEnhancer) - 1 do
  begin
   FBassEnhancer[ChannelIndex] := THarmonicBass.Create;
   FBassEnhancer[ChannelIndex].SampleRate := SampleRate;
  end;

 // create and setup limiter
 FLimiter := TLightweightSoftKneeLimiter.Create;
 with FLimiter do
  begin
   Threshold_dB  := -1.02;
   Knee_dB       := 0.5;
   MakeUpGain_dB := 1;
   Attack        := 0.2;
   Release       := 1300;
   SampleRate    := Self.SampleRate;
  end;

 // set default speaker type
 FIsSpeaker := True;

 // setup default speaker type analyser
 FSpeedConst[0] := 0.999;
 CalculateSmoothingFactor;
 FFSGain := 0;
 FBandReserve := 0.25;
 UpdateFilters;

 EditorFormClass := TFmAudioAmelioration;

 UseDownsampling := True;
 DownsamplingChanged;
end;

procedure TAudioAmeliorationModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 FreeAndNil(FAmbience);
 FreeAndNil(FCompressor);
 FreeAndNil(FLimiter);
 FreeAndNil(FCrosstalkSimulator);

 // free bass enhancer
 for ChannelIndex := 0 to Length(FBassEnhancer) - 1
  do FreeAndNil(FBassEnhancer[ChannelIndex]);

 // free exciter
 for ChannelIndex := 0 to Length(FExciter) - 1
  do FreeAndNil(FExciter[ChannelIndex]);

 // free filters
 for BandIndex := 0 to CNumFrequencies - 1 do
  begin
   FreeAndNil(FFilterArray[BandIndex].Lowpass);
   FreeAndNil(FFilterArray[BandIndex].Highpass);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAudioAmeliorationModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TAudioAmeliorationModule.ParameterSpeakerDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'Speaker'
  else PreDefined := 'Headphones';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAudioAmeliorationModule.ParameterPowerChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FPowerActive := Value > 0.5;
 ChooseProcess;

 // update GUI
 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdatePower;
end;

procedure TAudioAmeliorationModule.ParameterExciterActiveChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FExciterActive := Value > 0.5;

 // update GUI
 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateExciterActive;
end;

procedure TAudioAmeliorationModule.ParameterAmbienceActiveChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FAmbienceActive := Value > 0.5;

 // update GUI
 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateAmbienceActive;
end;

function TAudioAmeliorationModule.GetBandReserve: Single;
begin
 Result := 100 * FBandReserve;
end;

function TAudioAmeliorationModule.GetBandRMS(Index: Integer): Single;
begin
 if Index in [0..CNumFrequencies - 1]
  then Result := FFilterArray[Index].RMS
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TAudioAmeliorationModule.Parameter3DSoundChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FSourroundActive := Value > 0.5;

 // update GUI
 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).Update3DSurroundActive;
end;

procedure TAudioAmeliorationModule.ParameterCompressorActiveChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCompressorActive := Value > 0.5;

 // update GUI
 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateCompressorActive;
end;

procedure TAudioAmeliorationModule.ParameterExtraBassActiveChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FExtraBassActive := Value > 0.5;

 // update GUI
 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateExtraBassActive;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAudioAmeliorationModule.ParameterExciterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FExciter) - 1 do
  if Assigned(FExciter[Channel]) then
   with FExciter[Channel] do
    begin
     Frequency := 8000 - 5500 * FastSqrtBab1(0.01 * Value);
     HighFrequencyLevel := 1 + 0.005 * Value;
     LowFrequencyLevel := 1;
     HarmonicsLevel := 0.01 + 0.01 * Value;
    end;

 // update GUI
 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateExciter;
end;

procedure TAudioAmeliorationModule.ParameterAmbienceChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FAmbience) then
  begin
   FAmbience.Mix := 0.009 * Value;
   FAmbience.Damping := 0.3 + 0.6 * (1 - 0.01 * Value);
  end;

 // update GUI
 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateAmbience;
end;

procedure TAudioAmeliorationModule.ParameterCompressorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor) then
  with FCompressor do
   begin
    Threshold_dB := -0.06 * Value;
    Knee_dB := 5 * (1 - 0.01 * Value);
    MakeUpGain_dB := -Threshold_dB + (1 - 0.01 * Value) * Knee_dB;
    Ratio := FastPower2MinError4(0.05 * Value);
   end;

 // update GUI
 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateCompressor;
end;

procedure TAudioAmeliorationModule.ParameterExtraBassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FBassEnhancer) - 1 do
  if Assigned(FBassEnhancer[Channel]) then
   with FBassEnhancer[Channel] do
    begin
     HighpassSelect := hp1stOrder;
     Frequency := 80 + 1.2 * Value;
     HarmonicBassLevel := dB_To_Amp(-18 * (1 - 0.01 * Value));
     OriginalBassLevel := dB_To_Amp(-0.1 * Value);
     Decay := dB_To_Amp(-(9 + 0.1 * Value));
     InputLevel := 1;
     HighFrequencyLevel := 1;
    end;

 // update GUI
 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateExtraBass;
end;

procedure TAudioAmeliorationModule.ParameterSpeakerChangedChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FIsSpeaker := Value > 0.5;
 ChooseProcess;

 // update GUI
 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateSpeaker;
end;

procedure TAudioAmeliorationModule.ChooseProcess;
begin
 if FPowerActive
  then
   if FIsSpeaker
    then OnProcess := VSTModuleProcessSpeaker
    else OnProcess := VSTModuleProcessHeadphones
  else OnProcess := VSTModuleProcessBypass;
 OnProcess32Replacing := OnProcess;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAudioAmeliorationModule.SetBandReserve(const Value: Single);
begin
 FBandReserve := 0.01 * Value;
end;

procedure TAudioAmeliorationModule.SetUseDownsampling(const Value: Boolean);
begin
 if FUseDownsampling <> Value then
  begin
   FUseDownsampling := Value;
   DownsamplingChanged;
  end;
end;

procedure TAudioAmeliorationModule.CalculateSmoothingFactor;
begin
 FSpeedConst[1] := 1 - FSpeedConst[0];
end;

procedure TAudioAmeliorationModule.UpdateFilters;
var
  Band         : Integer;
  Downsampling : Integer;
  DesiredFreq  : Double;
const
  HalfThirdMulFak : Double = 1.1224620483093729814335330496792; // = Power(2,1/6)
begin
 Downsampling := 0;

 for Band := 0 to Length(FFilterArray) - 1 do
  begin
   // Lowpass
   DesiredFreq := CFrequencyArray[CNumFrequencies - Band - 1] * HalfThirdMulFak;
   if DesiredFreq > 0.499 * SampleRate then DesiredFreq := 0.499 * SampleRate;

   if UseDownsampling then
    while ((2 * DesiredFreq / Self.SampleRate) * (1 shl Downsampling)) < FBandReserve
     do Inc(Downsampling);

   // eventually create filter
   if not Assigned(FFilterArray[Band].Lowpass)
    then FFilterArray[Band].Lowpass := TChebyshev1LowpassFilter.Create(6);

   with FFilterArray[Band].Lowpass do
    begin
     SampleRate := Self.SampleRate / (1 shl Downsampling);
     Frequency := DesiredFreq;
    end;
   FFilterArray[Band].Downsampling := (1 shl Downsampling);

   // Highpass
   DesiredFreq := CFrequencyArray[CNumFrequencies - Band - 1] / HalfThirdMulFak;

   // eventually create filter
   if not Assigned(FFilterArray[Band].Highpass)
    then FFilterArray[Band].Highpass := TChebyshev1HighpassFilter.Create(8);
    
   with FFilterArray[Band].Highpass do
    begin
     SampleRate := Self.SampleRate / (1 shl Downsampling);
     Frequency := DesiredFreq;
    end;
  end;
 FDownSampleMax := 1 shl Downsampling;
end;

procedure TAudioAmeliorationModule.DownsamplingChanged;
begin
 if FUseDownsampling
  then FDownSampleCount := 0
  else FDownSampleCount := -1;

(*
 if FDownSampleCount = -1
  then OnProcess := VSTModuleProcessNormal
  else OnProcess := VSTModuleProcessDownSampled;

 OnProcessReplacing := OnProcess;
*)
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAudioAmeliorationModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 if Abs(SampleRate) > 0 then
  begin
   if Assigned(FAmbience) then FAmbience.SampleRate := SampleRate;
   if Assigned(FCompressor) then FCompressor.SampleRate := SampleRate;
   if Assigned(FLimiter) then FLimiter.SampleRate := SampleRate;
   if Assigned(FCrosstalkSimulator) then FCrosstalkSimulator.SampleRate := SampleRate;

   for ChannelIndex := 0 to Length(FBassEnhancer) - 1 do
    if Assigned(FBassEnhancer[ChannelIndex])
     then FBassEnhancer[ChannelIndex].SampleRate := SampleRate;

   for ChannelIndex := 0 to Length(FExciter) - 1 do
    if Assigned(FExciter[ChannelIndex])
     then FExciter[ChannelIndex].SampleRate := SampleRate;
  end;
end;

procedure TAudioAmeliorationModule.VSTModuleProcessSpeaker(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
  Band         : Integer;
  d, z, s      : Double;
const
  CDenorm = 1E-32;
begin
 for ChannelIndex := 0 to min(numInputs, numOutputs) - 1
  do Move(Inputs[ChannelIndex, 0], Outputs[ChannelIndex, 0], SampleFrames * SizeOf(Single));

 // apply exciter
 if ExciterActive then
  for ChannelIndex := 0 to Length(FExciter) - 1 do
   for SampleIndex := 0 to SampleFrames - 1
    do Outputs[ChannelIndex, SampleIndex] := FExciter[ChannelIndex].ProcessSample64(Outputs[ChannelIndex, SampleIndex]);

 // apply compression
 if CompressorActive then
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    FCompressor.InputSample(0.5 * (Outputs[0, SampleIndex] + Outputs[1, SampleIndex]));
    Outputs[0, SampleIndex] := FCompressor.GainSample(Outputs[0, SampleIndex]);
    Outputs[1, SampleIndex] := FCompressor.GainSample(Outputs[1, SampleIndex]);
   end;

 // apply ambience
 if AmbienceActive then
  for SampleIndex := 0 to SampleFrames - 1
   do FAmbience.ProcessSample(Outputs[0, SampleIndex], Outputs[1, SampleIndex]);

 // apply extra bass
 if ExtraBassActive then
  for ChannelIndex := 0 to Length(FBassEnhancer) - 1 do
   for SampleIndex := 0 to SampleFrames - 1
    do Outputs[ChannelIndex, SampleIndex] := FBassEnhancer[ChannelIndex].ProcessSample32(Outputs[ChannelIndex, SampleIndex]);

 // apply limiter
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   FLimiter.InputSample(0.5 * (Outputs[0, SampleIndex] + Outputs[1, SampleIndex]));
   Outputs[0, SampleIndex] := Limit(2.6 * FastTanhMinError4(0.5 * FLimiter.GainSample(Outputs[0, SampleIndex])));
   Outputs[1, SampleIndex] := Limit(2.6 * FastTanhMinError4(0.5 * FLimiter.GainSample(Outputs[1, SampleIndex])));
  end;

 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   d := 0.5 * (Outputs[0, SampleIndex] + Outputs[1, SampleIndex]);
   for Band := 0 to CNumFrequencies - 1 do
    begin
     if (FDownSampleCount mod FFilterArray[Band].Downsampling) <> 0
      then Break;

     d := FFilterArray[Band].Lowpass.ProcessSample64(d + CDenorm);
     z := FFilterArray[Band].Highpass.ProcessSample64(d + CDenorm);

     s := IntPower(FSpeedConst[0], 8 * FFilterArray[Band].Downsampling + 1);
     FFilterArray[Band].RMS := s * FFilterArray[Band].RMS + (1 - s) * Amp_to_dB(abs(z));
    end;
   Inc(FDownSampleCount);
   if FDownSampleCount >= FDownSampleMax then FDownSampleCount := 0;
  end;
end;

procedure TAudioAmeliorationModule.VSTModuleProcessHeadphones(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
  Band         : Integer;
  d, z, s      : Double;
const
  CDenorm = 1E-32;
begin
 for ChannelIndex := 0 to min(numInputs, numOutputs) - 1
  do Move(Inputs[ChannelIndex, 0], Outputs[ChannelIndex, 0], SampleFrames * SizeOf(Single));

 // apply ambience
 if AmbienceActive then
  for SampleIndex := 0 to SampleFrames - 1
   do FAmbience.ProcessSample(Outputs[0, SampleIndex], Outputs[1, SampleIndex]);

 // apply exciter
 if ExciterActive then
  for ChannelIndex := 0 to Length(FExciter) - 1 do
   for SampleIndex := 0 to SampleFrames - 1
    do Outputs[ChannelIndex, SampleIndex] := FExciter[ChannelIndex].ProcessSample64(Outputs[ChannelIndex, SampleIndex]);

 // apply compression
 if CompressorActive then
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    FCompressor.InputSample(0.5 * (Outputs[0, SampleIndex] + Outputs[1, SampleIndex]));
    Outputs[0, SampleIndex] := FCompressor.GainSample(Outputs[0, SampleIndex]);
    Outputs[1, SampleIndex] := FCompressor.GainSample(Outputs[1, SampleIndex]);
   end;

 // apply extra bass
 if ExtraBassActive then
  for ChannelIndex := 0 to Length(FBassEnhancer) - 1 do
   for SampleIndex := 0 to SampleFrames - 1
    do Outputs[ChannelIndex, SampleIndex] := FBassEnhancer[ChannelIndex].ProcessSample32(Outputs[ChannelIndex, SampleIndex]);

 for SampleIndex := 0 to SampleFrames - 1
  do FCrosstalkSimulator.ProcessSample(Outputs[0, SampleIndex], Outputs[1, SampleIndex]);

 // apply limiter
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   FLimiter.InputSample(0.5 * (Outputs[0, SampleIndex] + Outputs[1, SampleIndex]));
   Outputs[0, SampleIndex] := Limit(2.6 * FastTanhMinError4(0.5 * FLimiter.GainSample(Outputs[0, SampleIndex])));
   Outputs[1, SampleIndex] := Limit(2.6 * FastTanhMinError4(0.5 * FLimiter.GainSample(Outputs[1, SampleIndex])));
  end;

 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   d := 0.5 * (Outputs[0, SampleIndex] + Outputs[1, SampleIndex]);
   for Band := 0 to CNumFrequencies - 1 do
    begin
     if (FDownSampleCount mod FFilterArray[Band].Downsampling) <> 0
      then Break;

     d := FFilterArray[Band].Lowpass.ProcessSample64(d + CDenorm);
     z := FFilterArray[Band].Highpass.ProcessSample64(d + CDenorm);

     s := IntPower(FSpeedConst[0], 8 * FFilterArray[Band].Downsampling + 1);
     FFilterArray[Band].RMS := s * FFilterArray[Band].RMS + (1 - s) * Amp_to_dB(abs(z));
    end;
   Inc(FDownSampleCount);
   if FDownSampleCount >= FDownSampleMax then FDownSampleCount := 0;
  end;
end;

procedure TAudioAmeliorationModule.VSTModuleProcessBypass(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to min(numInputs, numOutputs) - 1
  do Move(Inputs[ChannelIndex, 0], Outputs[ChannelIndex, 0], SampleFrames * SizeOf(Single));
end;

end.
