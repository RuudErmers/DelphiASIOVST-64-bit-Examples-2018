unit LightweightMultibandCompressorDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspDynamics, DAV_DspLightweightDynamics, 
  DAV_DspFilterLinkwitzRiley;

type
  TLightweightMultibandCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMonoSoftClip(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterKneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterLimitChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAutoMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLowFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMidFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FLightweightMultibandCompressor : array [0..3] of TLightweightSoftKneeCompressor;
    FLinkwitzRiley                  : array [0..1, 0..2] of TLinkwitzRiley;
    function GetLightweightMultibandCompressor(Index: Integer): TLightweightSoftKneeCompressor;
    procedure ChooseProcess;
    function GetAutoGain(Index: Integer): Boolean;
    procedure SetAutoGain(Index: Integer; const Value: Boolean);
  public
    function EvaluateLowCharacteristic(const Input: Single): Single;
    function EvaluateLowMidCharacteristic(const Input: Single): Single;
    function EvaluateHighMidCharacteristic(const Input: Single): Single;
    function EvaluateHighCharacteristic(const Input: Single): Single;
    property LightweightMultibandCompressor[Index: Integer]: TLightweightSoftKneeCompressor read GetLightweightMultibandCompressor;
    property AutoGain[Index: Integer]: Boolean read GetAutoGain write SetAutoGain;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, DAV_Approximations, DAV_VSTModuleWithPrograms,
  LightweightMultibandCompressorGUI;

procedure TLightweightMultibandCompressorDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
const
  CPresets : array [1..10, 0..9] of Single = (
    (50, 500, -10, 3, 4, 3, 0, 1, 0, 100),
    (20, 100, -12, 4, 2.5, 6, 0, 0, 0, 100),
    (20,  80, -15, 8, 2, 8, 0, 1, 0, 100),
    (5, 60, -20, 7, 3, 13, 1, 0, 0, 100),
    (1, 50, -30, 6, 2, 18, 0, 0, 0, 100),
    (8, 64, -30, 12, 5, 17, 0, 0, 0, 100),
    (16, 78, -24, 15, 1.8, 19, 0, 1, 0, 100),
    (1, 20, -14, 5, 3, 8, 0, 1, 0, 100),
    (3, 44, -17, 7, 1, 9, 1, 0, 0, 100),
    (8, 56, -11, 9, 4, 5, 1, 1, 0, 100));
begin
 for Channel := 0 to Length(FLightweightMultibandCompressor) - 1 do
  begin
   FLightweightMultibandCompressor[Channel] := TLightweightSoftKneeCompressor.Create;
   FLightweightMultibandCompressor[Channel].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 0] := TLinkwitzRiley.Create;
   FLinkwitzRiley[Channel, 0].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 0].Order := 1;
   FLinkwitzRiley[Channel, 1] := TLinkwitzRiley.Create;
   FLinkwitzRiley[Channel, 1].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 1].Order := 1;
   FLinkwitzRiley[Channel, 2] := TLinkwitzRiley.Create;
   FLinkwitzRiley[Channel, 2].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 2].Order := 1;
  end;

 // initialize parameters
 Parameter[ 0] := 300;
 Parameter[ 1] := 800;
 Parameter[ 2] := 3000;
 Parameter[ 3] := 0;
 Parameter[ 4] := 5;
 Parameter[ 5] := 50;
 Parameter[ 6] := -10;
 Parameter[ 7] := 4;
 Parameter[ 8] := 3;
 Parameter[ 9] := 3;
 Parameter[10] := 0;
 Parameter[11] := 5;
 Parameter[12] := 50;
 Parameter[13] := -10;
 Parameter[14] := 4;
 Parameter[15] := 3;
 Parameter[16] := 3;
 Parameter[17] := 0;
 Parameter[18] := 5;
 Parameter[19] := 50;
 Parameter[20] := -10;
 Parameter[21] := 4;
 Parameter[22] := 3;
 Parameter[23] := 3;
 Parameter[24] := 0;
 Parameter[25] := 5;
 Parameter[26] := 50;
 Parameter[27] := -10;
 Parameter[28] := 4;
 Parameter[29] := 3;
 Parameter[30] := 3;
 Parameter[31] := 0;

 Programs[0].SetParameters(FParameter);
(*
 for Channel := 1 to numPrograms - 1
  do Programs[Channel].SetParameters(CPresets[Channel]);
*)

 // set editor class
 EditorFormClass := TFmLightweightMultibandCompressor;
end;

procedure TLightweightMultibandCompressorDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLightweightMultibandCompressor) - 1
  do FreeAndNil(FLightweightMultibandCompressor[Channel]);
end;

function TLightweightMultibandCompressorDataModule.EvaluateLowCharacteristic(
  const Input: Single): Single;
begin
 Result := FLightweightMultibandCompressor[0].CharacteristicCurve_dB(Input);
end;

function TLightweightMultibandCompressorDataModule.EvaluateLowMidCharacteristic(
  const Input: Single): Single;
begin
 Result:= FLightweightMultibandCompressor[1].CharacteristicCurve_dB(Input);
end;

procedure TLightweightMultibandCompressorDataModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val >= 1000
  then PreDefined := 'kHz';
end;

procedure TLightweightMultibandCompressorDataModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1000
  then PreDefined := FloatToStrF(RoundTo(Val, -2), ffGeneral, 3, 3)
  else PreDefined := FloatToStrF(RoundTo(1E-3 * Val, -2), ffGeneral, 3, 3);
end;

function TLightweightMultibandCompressorDataModule.EvaluateHighMidCharacteristic(
  const Input: Single): Single;
begin
 Result:= FLightweightMultibandCompressor[2].CharacteristicCurve_dB(Input);
end;

function TLightweightMultibandCompressorDataModule.EvaluateHighCharacteristic(
  const Input: Single): Single;
begin
 Result:= FLightweightMultibandCompressor[3].CharacteristicCurve_dB(Input);
end;

procedure TLightweightMultibandCompressorDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TLightweightMultibandCompressorDataModule.ParameterTimeLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := 'µs' else
 if Val >= 1000
  then PreDefined := 's';
end;

procedure TLightweightMultibandCompressorDataModule.SetAutoGain(Index: Integer;
  const Value: Boolean);
begin
 if Index in [0..Length(FLightweightMultibandCompressor) - 1] then
  begin
   if Assigned(FLightweightMultibandCompressor[Index])
    then FLightweightMultibandCompressor[Index].AutoMakeUp := Value;

   // update GUI
   if EditorForm is TFmLightweightMultibandCompressor then
    with TFmLightweightMultibandCompressor(EditorForm) do
     case Index of
      0: UpdateLowAutoMakeUpGain;
      1: UpdateLowMidAutoMakeUpGain;
      2: UpdateHighMidAutoMakeUpGain;
      3: UpdateHighAutoMakeUpGain;
     end;
  end else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TLightweightMultibandCompressorDataModule.ParameterTimeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := FloatToStrF(RoundTo(1E3 * Val, -2), ffGeneral, 3, 3) else
 if Val < 1000
  then PreDefined := FloatToStrF(RoundTo(Val, -2), ffGeneral, 3, 3)
  else PreDefined := FloatToStrF(RoundTo(1E-3 * Val, -2), ffGeneral, 3, 3);
end;

procedure TLightweightMultibandCompressorDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightMultibandCompressorDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightMultibandCompressorDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightMultibandCompressorDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightMultibandCompressorDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TLightweightMultibandCompressorDataModule.ParameterLowFreqChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLightweightMultibandCompressor) - 1 do
  if Assigned(FLinkwitzRiley[Channel, 0])
   then FLinkwitzRiley[Channel, 0].Frequency := Value;

 // update GUI
 if EditorForm is TFmLightweightMultibandCompressor then
  with TFmLightweightMultibandCompressor(EditorForm) do UpdateLowFrequency;
end;

procedure TLightweightMultibandCompressorDataModule.ParameterMidFreqChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLightweightMultibandCompressor) - 1 do
  if Assigned(FLinkwitzRiley[Channel, 1])
   then FLinkwitzRiley[Channel, 1].Frequency := Value;

 // update GUI
 if EditorForm is TFmLightweightMultibandCompressor then
  with TFmLightweightMultibandCompressor(EditorForm) do UpdateMidFrequency;
end;

procedure TLightweightMultibandCompressorDataModule.ParameterHighChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLightweightMultibandCompressor) - 1 do
  if Assigned(FLinkwitzRiley[Channel, 2])
   then FLinkwitzRiley[Channel, 2].Frequency := Value;

 // update GUI
 if EditorForm is TFmLightweightMultibandCompressor then
  with TFmLightweightMultibandCompressor(EditorForm) do UpdateHighFrequency;
end;

procedure TLightweightMultibandCompressorDataModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;

 // update GUI
 if EditorForm is TFmLightweightMultibandCompressor
  then TFmLightweightMultibandCompressor(EditorForm).UpdateLimit;
end;

procedure TLightweightMultibandCompressorDataModule.ChooseProcess;
begin
 case Round(Parameter[3]) of
  0 : OnProcess := VSTModuleProcessMono;
  1 : OnProcess := VSTModuleProcessMonoSoftClip;
 end;
 OnProcess32Replacing := OnProcess;
end;

function TLightweightMultibandCompressorDataModule.GetAutoGain(
  Index: Integer): Boolean;
begin
 if Index in [0..Length(FLightweightMultibandCompressor) - 1]
  then Result := FLightweightMultibandCompressor[Index].AutoMakeUp
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

function TLightweightMultibandCompressorDataModule.GetLightweightMultibandCompressor(Index: Integer): TLightweightSoftKneeCompressor;
begin
 if Index in [0..Length(FLightweightMultibandCompressor) - 1]
  then Result := FLightweightMultibandCompressor[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TLightweightMultibandCompressorDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 4) div 7;
 if Assigned(FLightweightMultibandCompressor[Band])
  then FLightweightMultibandCompressor[Band].Attack := Value;

 // update GUI
 if EditorForm is TFmLightweightMultibandCompressor then
  with TFmLightweightMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowAttack;
    1: UpdateLowMidAttack;
    2: UpdateHighMidAttack;
    3: UpdateHighAttack;
   end;
end;

procedure TLightweightMultibandCompressorDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 5) div 7;
 if Assigned(FLightweightMultibandCompressor[Band])
  then FLightweightMultibandCompressor[Band].Release := Value;

 // update GUI
 if EditorForm is TFmLightweightMultibandCompressor then
  with TFmLightweightMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowRelease;
    1: UpdateLowMidRelease;
    2: UpdateHighMidRelease;
    3: UpdateHighRelease;
   end;
end;

procedure TLightweightMultibandCompressorDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 6) div 7;
 if Assigned(FLightweightMultibandCompressor[Band])
  then FLightweightMultibandCompressor[Band].Threshold_dB := Value;

 // update GUI
 if EditorForm is TFmLightweightMultibandCompressor then
  with TFmLightweightMultibandCompressor(EditorForm) do
   case Band of
    0: begin
        UpdateLowThreshold;
        if Parameter[Band * 7 + 10] > 0.5
         then UpdateLowMakeUp;
       end;
    1: begin
        UpdateLowMidThreshold;
        if Parameter[Band * 7 + 10] > 0.5
         then UpdateLowMidMakeUp;
       end;
    2: begin
        UpdateHighMidThreshold;
        if Parameter[Band * 7 + 10] > 0.5
         then UpdateHighMidMakeUp;
       end;
    3: begin
        UpdateHighThreshold;
        if Parameter[Band * 7 + 10] > 0.5
         then UpdateHighMakeUp;
       end;
   end;
end;

procedure TLightweightMultibandCompressorDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 7) div 7;
 if Assigned(FLightweightMultibandCompressor[Band])
  then FLightweightMultibandCompressor[Band].Ratio := Value;

 // update GUI
 if EditorForm is TFmLightweightMultibandCompressor then
  with TFmLightweightMultibandCompressor(EditorForm) do
   case Band of
    0: begin
        UpdateLowRatio;
        if Parameter[Band * 7 + 10] > 0.5
         then UpdateLowMakeUp;
       end;
    1: begin
        UpdateLowMidRatio;
        if Parameter[Band * 7 + 10] > 0.5
         then UpdateLowMidMakeUp;
       end;
    2: begin
        UpdateHighMidRatio;
        if Parameter[Band * 7 + 10] > 0.5
         then UpdateHighMidMakeUp;
       end;
    3: begin
        UpdateHighRatio;
        if Parameter[Band * 7 + 10] > 0.5
         then UpdateHighMakeUp;
       end;
   end;
end;

procedure TLightweightMultibandCompressorDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 8) div 7;
 if Assigned(FLightweightMultibandCompressor[Band])
  then FLightweightMultibandCompressor[Band].Knee_dB := Value;

 // update GUI
 if EditorForm is TFmLightweightMultibandCompressor then
  with TFmLightweightMultibandCompressor(EditorForm) do
   case Band of
    0: begin
        UpdateLowKnee;
        if Parameter[Band * 7 + 10] > 0.5
         then UpdateLowMakeUp;
       end;
    1: begin
        UpdateLowMidKnee;
        if Parameter[Band * 7 + 10] > 0.5
         then UpdateLowMidMakeUp;
       end;
    2: begin
        UpdateHighMidKnee;
        if Parameter[Band * 7 + 10] > 0.5
         then UpdateHighMidMakeUp;
       end;
    3: begin
        UpdateHighKnee;
        if Parameter[Band * 7 + 10] > 0.5
         then UpdateHighMakeUp;
       end;
   end;
end;

procedure TLightweightMultibandCompressorDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 9) div 7;
 if Assigned(FLightweightMultibandCompressor[Band])
  then FLightweightMultibandCompressor[Band].MakeUpGain_dB := Value;

 // update GUI
 if EditorForm is TFmLightweightMultibandCompressor then
  with TFmLightweightMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowMakeUp;
    1: UpdateLowMidMakeUp;
    2: UpdateHighMidMakeUp;
    3: UpdateHighMakeUp;
   end;
end;

procedure TLightweightMultibandCompressorDataModule.ParameterAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 10) div 7;
end;

procedure TLightweightMultibandCompressorDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Temp   : array [0..3] of Single;
  FD     : array [0..1, 0..3] of Single;
const
  CDenorm32 : Single = 1E-12;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   // split mid
   FLinkwitzRiley[0, 1].ProcessSample32(CDenorm32 + Inputs[0, Sample], FD[0, 1], FD[0, 3]);
   FLinkwitzRiley[1, 1].ProcessSample32(CDenorm32 + Inputs[1, Sample], FD[1, 1], FD[1, 3]);

   // split low
   FLinkwitzRiley[0, 0].ProcessSample32(FD[0, 1] - CDenorm32, FD[0, 0], FD[0, 1]);
   FLinkwitzRiley[1, 0].ProcessSample32(FD[1, 1] - CDenorm32, FD[1, 0], FD[1, 1]);

   // split high
   FLinkwitzRiley[0, 2].ProcessSample32(FD[0, 3] - CDenorm32, FD[0, 2], FD[0, 3]);
   FLinkwitzRiley[1, 2].ProcessSample32(FD[1, 3] - CDenorm32, FD[1, 2], FD[1, 3]);

   // compress & copy gain reduction
   with FLightweightMultibandCompressor[0] do
    begin
     InputSample(CHalf32 * (FD[0, 0] + FD[1, 0]));
     Temp[0] := GainReductionFactor * MakeUpGain;
    end;
   with FLightweightMultibandCompressor[1] do
    begin
     InputSample(CHalf32 * (FD[0, 1] + FD[1, 1]));
     Temp[1] := GainReductionFactor * MakeUpGain;
    end;
   with FLightweightMultibandCompressor[2] do
    begin
     InputSample(CHalf32 * (FD[0, 2] + FD[1, 2]));
     Temp[2] := GainReductionFactor * MakeUpGain;
    end;
   with FLightweightMultibandCompressor[3] do
    begin
     InputSample(CHalf32 * (FD[0, 3] + FD[1, 3]));
     Temp[3] := GainReductionFactor * MakeUpGain;
    end;

   // gain and combine
   Outputs[0, Sample] := Temp[0] * FD[0, 0] + Temp[1] * FD[0, 1] - Temp[2] * FD[0, 2] - Temp[3] * FD[0, 3];
   Outputs[1, Sample] := Temp[0] * FD[1, 0] + Temp[1] * FD[1, 1] - Temp[2] * FD[1, 2] - Temp[3] * FD[1, 3];
  end;
end;

procedure TLightweightMultibandCompressorDataModule.VSTModuleProcessMonoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Temp   : array [0..3] of Single;
  FD     : array [0..1, 0..3] of Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   // split mid
   FLinkwitzRiley[0, 1].ProcessSample32(CDenorm32 + Inputs[0, Sample], FD[0, 1], FD[0, 3]);
   FLinkwitzRiley[1, 1].ProcessSample32(CDenorm32 + Inputs[1, Sample], FD[1, 1], FD[1, 3]);

   // split low
   FLinkwitzRiley[0, 0].ProcessSample32(FD[0, 1] - CDenorm32, FD[0, 0], FD[0, 1]);
   FLinkwitzRiley[1, 0].ProcessSample32(FD[1, 1] - CDenorm32, FD[1, 0], FD[1, 1]);

   // split low
   FLinkwitzRiley[0, 2].ProcessSample32(FD[0, 3] - CDenorm32, FD[0, 2], FD[0, 3]);
   FLinkwitzRiley[1, 2].ProcessSample32(FD[1, 3] - CDenorm32, FD[1, 2], FD[1, 3]);

   // compress
   FLightweightMultibandCompressor[0].ProcessSample64(CHalf32 * (FD[0, 0] + FD[1, 0]));
   FLightweightMultibandCompressor[1].ProcessSample64(CHalf32 * (FD[0, 1] + FD[1, 1]));
   FLightweightMultibandCompressor[2].ProcessSample64(CHalf32 * (FD[0, 2] + FD[1, 2]));
   FLightweightMultibandCompressor[3].ProcessSample64(CHalf32 * (FD[0, 3] + FD[1, 3]));

   // copy gain reduction
   Temp[0] := FLightweightMultibandCompressor[0].GainReductionFactor;
   Temp[1] := FLightweightMultibandCompressor[1].GainReductionFactor;
   Temp[2] := FLightweightMultibandCompressor[2].GainReductionFactor;
   Temp[3] := FLightweightMultibandCompressor[3].GainReductionFactor;

   // gain and combine
   Outputs[0, Sample] := FastTanhContinousError4(Temp[0] * FD[0, 0] + Temp[1] * FD[0, 1] - Temp[2] * FD[0, 2] - Temp[3] * FD[0, 3]);
   Outputs[1, Sample] := FastTanhContinousError4(Temp[0] * FD[1, 0] + Temp[1] * FD[1, 1] - Temp[2] * FD[1, 2] - Temp[3] * FD[1, 3]);
  end;
end;

procedure TLightweightMultibandCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 if Abs(SampleRate) <> 0 then
  begin
   for Channel := 0 to Length(FLightweightMultibandCompressor) - 1 do
    begin
     if Assigned(FLightweightMultibandCompressor[Channel])
      then FLightweightMultibandCompressor[Channel].SampleRate := SampleRate;
     if Assigned(FLinkwitzRiley[Channel, 0])
      then FLinkwitzRiley[Channel, 0].SampleRate := SampleRate;
     if Assigned(FLinkwitzRiley[Channel, 1])
      then FLinkwitzRiley[Channel, 1].SampleRate := SampleRate;
     if Assigned(FLinkwitzRiley[Channel, 2])
      then FLinkwitzRiley[Channel, 2].SampleRate := SampleRate;
    end;
  end;
end;

end.
