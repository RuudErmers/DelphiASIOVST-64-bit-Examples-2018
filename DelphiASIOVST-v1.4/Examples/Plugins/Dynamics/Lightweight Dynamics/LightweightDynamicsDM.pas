unit LightweightDynamicsDM;

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
  TLightweightDynamicsDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcessGateCompressorLimiter(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessGateCompressorLimiterSoftClip(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterCompressorAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorAutoMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterGateRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterCompressorKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterCompressorMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterCompressorMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterGateAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterGateThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLimiterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLimiterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLimiterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLimiterKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSoftClipChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FInputPeak   : array [0..1] of Single;
    FGate        : TCustomKneeCompressor;
    FCompressor  : TCustomKneeCompressor;
    FLimiter     : TCustomKneeLimiter;
    FCompMix     : Single;
    FPeakRelease : Single;
  public
    function EvaluateCharacteristic(const Input: Single): Single;
    procedure ChooseProcess;
    property Compressor: TCustomKneeCompressor read FCompressor;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, DAV_Approximations, DAV_VSTModuleWithPrograms,
  LightweightDynamicsGUI;

procedure TLightweightDynamicsDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
const
  CPresets : array [1..10, 0..17] of Single = (
    (15, 75, -60, 10, 2, 50, 500, -10, 3, 4, 3, 0, 100, 15, 75, -0.02, 5, 0),
    (15, 75, -60, 10, 2, 20, 100, -12, 4, 2.5, 6, 0, 100, 15, 75, -0.02, 5, 0),
    (15, 75, -60, 10, 2, 20,  80, -15, 8, 2, 8, 0, 100, 15, 75, -0.02, 5, 0),
    (15, 75, -60, 10, 2, 5, 60, -20, 7, 3, 13, 1, 100, 15, 75, -0.02, 5, 1),
    (15, 75, -60, 10, 2, 1, 50, -30, 6, 2, 18, 0, 100, 15, 75, -0.02, 5, 1),
    (15, 75, -60, 10, 2, 8, 64, -30, 12, 5, 17, 0, 100, 15, 75, -0.02, 5, 1),
    (15, 75, -60, 10, 2, 16, 78, -24, 15, 1.8, 19, 0, 100, 15, 75, -0.02, 5, 0),
    (15, 75, -60, 10, 2, 1, 20, -14, 5, 3, 8, 0, 100, 15, 75, -0.02, 5, 0),
    (15, 75, -60, 10, 2, 3, 44, -17, 7, 1, 9, 1, 100, 15, 75, -0.02, 5, 0),
    (15, 75, -60, 10, 2, 8, 56, -11, 9, 4, 5, 1, 100, 15, 75, -0.02, 5, 1));
begin
 // initialize gate
 FGate := TLightweightSoftKneeCompressor.Create;

 // initialize compressor
 FCompressor := TLightweightSoftKneeCompressor.Create;

 // initialize limiter
 FLimiter := TLightweightSoftKneeLimiter.Create;

 // gate
 Parameter[ 0] := 15;
 Parameter[ 1] := 75;
 Parameter[ 2] := -60;
 Parameter[ 3] := 10;
 Parameter[ 4] := 2;

 // compressor
 Parameter[ 5] := 15;
 Parameter[ 6] := 75;
 Parameter[ 7] := -10;
 Parameter[ 8] := 5;
 Parameter[ 9] := 2;
 Parameter[10] := 6;
 Parameter[11] := 0;
 Parameter[12] := 100;

 // limiter
 Parameter[13] := 15;
 Parameter[14] := 75;
 Parameter[15] := -0.02;
 Parameter[16] := 5;

 FPeakRelease := 0.9999;

 Programs[0].SetParameters(FParameter);
 for Channel := 1 to numPrograms - 1
  do Programs[Channel].SetParameters(CPresets[Channel]);
end;

procedure TLightweightDynamicsDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FGate);
 FreeAndNil(FCompressor);
 FreeAndNil(FLimiter);
end;

procedure TLightweightDynamicsDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmLightweightDynamics.Create(Self);
end;

procedure TLightweightDynamicsDataModule.ChooseProcess;
begin
 if Parameter[17] > 0.5
  then OnProcess := VSTModuleProcessGateCompressorLimiterSoftClip
  else OnProcess := VSTModuleProcessGateCompressorLimiter;
 OnProcessReplacing := OnProcess; 
end;

function TLightweightDynamicsDataModule.EvaluateCharacteristic(
  const Input: Single): Single;
begin
 Result := FLimiter.CharacteristicCurve_dB(
   FCompressor.CharacteristicCurve_dB(
   FGate.CharacteristicCurve_dB(Input)));
end;

procedure TLightweightDynamicsDataModule.ParameterGateAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FGate)
  then FGate.Attack := Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateGateAttack;
end;

procedure TLightweightDynamicsDataModule.ParameterGateReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FGate)
  then FGate.Release := Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateGateRelease;
end;

procedure TLightweightDynamicsDataModule.ParameterLimiterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLimiter)
  then FLimiter.Attack := Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateLimiterAttack;
end;

procedure TLightweightDynamicsDataModule.ParameterLimiterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLimiter)
  then FLimiter.Release := Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateLimiterRelease;
end;

procedure TLightweightDynamicsDataModule.ParameterLimiterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLimiter)
  then FLimiter.Threshold_dB := Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateLimiterThreshold;
end;

procedure TLightweightDynamicsDataModule.ParameterLimiterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FLimiter)
  then FLimiter.Knee_dB := Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateLimiterKnee;
end;

procedure TLightweightDynamicsDataModule.ParameterCompressorAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor)
  then FCompressor.Attack := Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateCompressorAttack;
end;

procedure TLightweightDynamicsDataModule.ParameterCompressorReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor)
  then FCompressor.Release := Value;

 // Update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateCompressorRelease;
end;

procedure TLightweightDynamicsDataModule.ParameterCompressorThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor)
  then FCompressor.Threshold_dB := Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateCompressorThreshold;
end;

procedure TLightweightDynamicsDataModule.ParameterCompressorRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor)
  then FCompressor.Ratio := Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateCompressorRatio;
end;

procedure TLightweightDynamicsDataModule.ParameterCompressorKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor)
  then FCompressor.Knee_dB := Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateCompressorKnee;
end;

procedure TLightweightDynamicsDataModule.ParameterGateThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FGate)
  then FGate.Threshold_dB := Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateGateThreshold;
end;

procedure TLightweightDynamicsDataModule.ParameterGateRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FGate)
  then FGate.Ratio := 1 / Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateGateRatio;
end;

procedure TLightweightDynamicsDataModule.ParameterGateKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FGate)
  then FGate.Knee_dB := Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateGateKnee;
end;

procedure TLightweightDynamicsDataModule.ParameterCompressorAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor)
  then FCompressor.AutoMakeUp := Boolean(Round(Value));

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateCompressorAutoMakeUpGain;
end;

procedure TLightweightDynamicsDataModule.ParameterCompressorMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor)
  then FCompressor.MakeUpGain_dB := Value;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateCompressorMakeUp;
end;

procedure TLightweightDynamicsDataModule.ParameterSoftClipChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;

 // update GUI
 if EditorForm is TFmLightweightDynamics
  then TFmLightweightDynamics(EditorForm).UpdateLimiterSoftClip;
end;

procedure TLightweightDynamicsDataModule.ParameterCompressorMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCompMix := 1 - 0.01 * Value;
end;

procedure TLightweightDynamicsDataModule.ParameterTimeLabel(
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

procedure TLightweightDynamicsDataModule.ParameterTimeDisplay(
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

procedure TLightweightDynamicsDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightDynamicsDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightDynamicsDataModule.ParameterCompressorRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := '1 : ' + FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightDynamicsDataModule.ParameterGateRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3) + ' : 1';
end;

procedure TLightweightDynamicsDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightDynamicsDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TLightweightDynamicsDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) <> 0 then
  begin
   if Assigned(FGate) then FGate.SampleRate := Abs(SampleRate);
   if Assigned(FCompressor) then FCompressor.SampleRate := Abs(SampleRate);
   if Assigned(FLimiter) then FLimiter.SampleRate := Abs(SampleRate);
  end;
end;

procedure TLightweightDynamicsDataModule.VSTModuleProcessGateCompressorLimiter(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  AbsInp : array [0..1] of Single;
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   AbsInp[0] := Abs(Inputs[0, Sample]);
   AbsInp[1] := Abs(Inputs[0, Sample]);

   // input peak detection
   if AbsInp[0] > FInputPeak[0]
    then FInputPeak[0] := AbsInp[0]
    else FInputPeak[0] := FInputPeak[0] * FPeakRelease;
   if AbsInp[1] > FInputPeak[1]
    then FInputPeak[1] := AbsInp[1]
    else FInputPeak[1] := FInputPeak[1] * FPeakRelease;

   with FGate do
    begin
     if Abs(Inputs[0, Sample]) > Abs(Inputs[1, Sample])
      then InputSample(Inputs[0, Sample])
      else InputSample(Inputs[1, Sample]);
     Temp := GainReductionFactor;
     if (Temp < 0) or (Temp > 1) then
      begin
       Outputs[0, Sample] := 0;
       Outputs[1, Sample] := 0;
      end
     else
      begin
       Outputs[0, Sample] := Temp * Inputs[0, Sample];
       Outputs[1, Sample] := Temp * Inputs[1, Sample];
      end;
    end;

   with FCompressor do
    begin
     InputSample(CHalf32 * (Outputs[0, Sample] + Outputs[1, Sample]));
     Temp := FCompMix + (1 - FCompMix) * MakeUpGain * GainReductionFactor;
     Outputs[0, Sample] := Temp * Outputs[0, Sample];
     Outputs[1, Sample] := Temp * Outputs[1, Sample];
    end;

   with FLimiter do
    begin
     if Abs(Outputs[0, Sample]) > Abs(Outputs[1, Sample])
      then InputSample(Outputs[0, Sample])
      else InputSample(Outputs[1, Sample]);
     Temp := MakeUpGain * GainReductionFactor;
     Outputs[0, Sample] := Temp * Outputs[0, Sample];
     Outputs[1, Sample] := Temp * Outputs[1, Sample];
    end;
  end;
end;

procedure TLightweightDynamicsDataModule.VSTModuleProcessGateCompressorLimiterSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  AbsInp : array [0..1] of Single;
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   AbsInp[0] := Abs(Inputs[0, Sample]);
   AbsInp[1] := Abs(Inputs[0, Sample]);

   // input peak detection
   if AbsInp[0] > FInputPeak[0]
    then FInputPeak[0] := AbsInp[0]
    else FInputPeak[0] := FInputPeak[0] * FPeakRelease;
   if AbsInp[1] > FInputPeak[1]
    then FInputPeak[1] := AbsInp[1]
    else FInputPeak[1] := FInputPeak[1] * FPeakRelease;

   with FGate do
    begin
     if AbsInp[0] > AbsInp[1]
      then InputSample(Inputs[0, Sample])
      else InputSample(Inputs[1, Sample]);
     Temp := GainReductionFactor;
     if (Temp < 0) or (Temp > 1) then
      begin
       Outputs[0, Sample] := 0;
       Outputs[1, Sample] := 0;
      end
     else
      begin
       Outputs[0, Sample] := Temp * Inputs[0, Sample];
       Outputs[1, Sample] := Temp * Inputs[1, Sample];
      end;
    end;

   with FCompressor do
    begin
     InputSample(CHalf32 * (Outputs[0, Sample] + Outputs[1, Sample]));
     Temp := FCompMix + (1 - FCompMix) * MakeUpGain * GainReductionFactor;
     Outputs[0, Sample] := Temp * Outputs[0, Sample];
     Outputs[1, Sample] := Temp * Outputs[1, Sample];
    end;

   with FLimiter do
    begin
     if Abs(Outputs[0, Sample]) > Abs(Outputs[1, Sample])
      then InputSample(Outputs[0, Sample])
      else InputSample(Outputs[1, Sample]);
     Temp := MakeUpGain * GainReductionFactor;
     Outputs[0, Sample] := FastTanhContinousError4(Temp * Outputs[0, Sample]);
     Outputs[1, Sample] := FastTanhContinousError4(Temp * Outputs[1, Sample]);
    end;
  end;
end;

end.
