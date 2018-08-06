unit LightweightLimiterDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspDynamics, DAV_DspLightweightDynamics;

type
  TLightweightLimiterDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMonoSoftClip(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessStereoSoftClip(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterKneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterStereoChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLimitChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAutoMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FLightweightLimiter : array [0..1] of TCustomKneeLimiter;
    function GetLightweightLimiter(Index: Integer): TCustomKneeLimiter;
    procedure ChooseProcess;
  public
    function EvaluateCharacteristic(const Input: Single): Single;
    property LightweightLimiter[Index: Integer]: TCustomKneeLimiter read GetLightweightLimiter;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, DAV_Approximations, DAV_VSTModuleWithPrograms,
  LightweightLimiterGUI;

procedure TLightweightLimiterDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
const
  CPresets : array [1..10, 0..8] of Single = (
    (50, 500, -10, 100, 4, 3, 0, 1, 0),
    (20, 100, -12, 100, 2.5, 6, 0, 0, 0),
    (20,  80, -15, 100, 2, 8, 0, 1, 0),
    (5, 60, -20, 100, 3, 13, 1, 0, 0),
    (1, 50, -30, 100, 2, 18, 0, 0, 0),
    (8, 64, -30, 100, 5, 17, 0, 0, 0),
    (16, 78, -24, 100, 1.8, 19, 0, 1, 0),
    (1, 20, -14, 100, 3, 8, 0, 1, 0),
    (3, 44, -17, 100, 1, 9, 1, 0, 0),
    (8, 56, -11, 100, 4, 5, 1, 1, 0));
begin
 for Channel := 0 to Length(FLightweightLimiter) - 1 do
  begin
   FLightweightLimiter[Channel] := TLightweightSoftKneeLimiter.Create;
   FLightweightLimiter[Channel].SampleRate := SampleRate;
  end;

 Parameter[0] := 15;
 Parameter[1] := 75;
 Parameter[2] := -10;
 Parameter[3] := 100;
 Parameter[4] := 2;
 Parameter[5] := 6;
 Parameter[6] := 0;
 Parameter[7] := 0;
 Parameter[8] := 0;

 Programs[0].SetParameters(FParameter);
 for Channel := 1 to numPrograms - 1
  do Programs[Channel].SetParameters(CPresets[Channel]);
end;

procedure TLightweightLimiterDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FLightweightLimiter[0]);
 FreeAndNil(FLightweightLimiter[1]);
end;

procedure TLightweightLimiterDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmLightweightLimiter.Create(Self);
end;

function TLightweightLimiterDataModule.EvaluateCharacteristic(
  const Input: Single): Single;
begin
 Result := FLightweightLimiter[0].CharacteristicCurve_dB(Input);
end;

procedure TLightweightLimiterDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TLightweightLimiterDataModule.ParameterTimeLabel(
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

procedure TLightweightLimiterDataModule.ParameterTimeDisplay(
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

procedure TLightweightLimiterDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightLimiterDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLightweightLimiter[0]) then
  begin
   FLightweightLimiter[0].MakeUpGain_dB := Value;
   if Assigned(FLightweightLimiter[1])
    then FLightweightLimiter[1].MakeUpGain_dB := FLightweightLimiter[0].MakeUpGain_dB;
  end;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateMakeUp;
end;

procedure TLightweightLimiterDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightLimiterDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightLimiterDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightLimiterDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TLightweightLimiterDataModule.ParameterStereoChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateStereo;
end;

procedure TLightweightLimiterDataModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateLimit;
end;

procedure TLightweightLimiterDataModule.ChooseProcess;
begin
 case Round(Parameter[7]) of
  0 : case Round(Parameter[6]) of
       0 : OnProcess := VSTModuleProcessMono;
       1 : OnProcess := VSTModuleProcessStereo;
      end;
  1 : case Round(Parameter[6]) of
       0 : OnProcess := VSTModuleProcessMonoSoftClip;
       1 : OnProcess := VSTModuleProcessStereoSoftClip;
      end;
 end;
 OnProcess32Replacing := OnProcess;
end;

procedure TLightweightLimiterDataModule.ParameterAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLightweightLimiter[0]) then
  begin
   FLightweightLimiter[0].AutoMakeUp := Boolean(Round(Value));
   if Assigned(FLightweightLimiter[1])
    then FLightweightLimiter[1].AutoMakeUp := FLightweightLimiter[0].AutoMakeUp;
  end;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateAutoMakeUpGain;
end;

function TLightweightLimiterDataModule.GetLightweightLimiter(Index: Integer): TCustomKneeLimiter;
begin
 if Index in [0..Length(FLightweightLimiter) - 1]
  then Result := FLightweightLimiter[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TLightweightLimiterDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLightweightLimiter[0]) then
  begin
   FLightweightLimiter[0].Attack := Value;
   if Assigned(FLightweightLimiter[1])
    then FLightweightLimiter[1].Attack := FLightweightLimiter[0].Attack;
  end;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateAttack;
end;

procedure TLightweightLimiterDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLightweightLimiter[0]) then
  begin
   FLightweightLimiter[0].Release := Value;
   if Assigned(FLightweightLimiter[1])
    then FLightweightLimiter[1].Release := FLightweightLimiter[0].Release;
  end;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateRelease;
end;

procedure TLightweightLimiterDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLightweightLimiter[0]) then
  begin
   FLightweightLimiter[0].Threshold_dB := Value;
   if Assigned(FLightweightLimiter[1])
    then FLightweightLimiter[1].Threshold_dB := Value;
  end;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateThreshold;
end;

procedure TLightweightLimiterDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLightweightLimiter[0]) then
  begin
   FLightweightLimiter[0].Knee_dB := Value;
   if Assigned(FLightweightLimiter[1])
    then FLightweightLimiter[1].Knee_dB := Value;
  end;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateKnee;
end;

procedure TLightweightLimiterDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FLightweightLimiter[0].ProcessSample64(Inputs[0, Sample]);
   Outputs[1, Sample] := FLightweightLimiter[1].ProcessSample64(Inputs[1, Sample]);
  end;
end;

procedure TLightweightLimiterDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FLightweightLimiter[0] do
  begin
   InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := GainReductionFactor * MakeUpGain;
   Outputs[0, Sample] := Temp * Inputs[0, Sample];
   Outputs[1, Sample] := Temp * Inputs[1, Sample];
  end;
end;

procedure TLightweightLimiterDataModule.VSTModuleProcessStereoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FastTanhContinousError4(FLightweightLimiter[0].ProcessSample64(Inputs[0, Sample]));
   Outputs[1, Sample] := FastTanhContinousError4(FLightweightLimiter[1].ProcessSample64(Inputs[1, Sample]));
  end;
end;

procedure TLightweightLimiterDataModule.VSTModuleProcessMonoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FLightweightLimiter[0] do
   begin
    InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Temp := GainReductionFactor * MakeUpGain;
    Outputs[0, Sample] := FastTanhContinousError4(Temp * Inputs[0, Sample]);
    Outputs[1, Sample] := FastTanhContinousError4(Temp * Inputs[1, Sample]);
   end;
end;

procedure TLightweightLimiterDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) <> 0 then
  begin
   if Assigned(FLightweightLimiter[0])
    then FLightweightLimiter[0].SampleRate := SampleRate;
   if Assigned(FLightweightLimiter[1])
    then FLightweightLimiter[1].SampleRate := SampleRate;
  end;
end;

end.
