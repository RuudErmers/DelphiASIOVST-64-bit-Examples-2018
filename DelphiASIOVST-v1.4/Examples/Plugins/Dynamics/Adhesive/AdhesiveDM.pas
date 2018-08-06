unit AdhesiveDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspLightweightDynamics, 
  DAV_DspFilterButterworth;

type
  TAdhesiveDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessBypass(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessPeakClip(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessSC(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessSCPeakClip(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameteActiveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterExtSideChsinChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterPeakClipChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSideChainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FCompressor : TLightweightSoftKneeCompressor;
    FFilter     : TButterworthHighpassFilter;
    FMix        : array [0..1] of Single;
    procedure ChooseProcess;
    procedure MixChanged;
  public
    property FastCompressor: TLightweightSoftKneeCompressor read FCompressor;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, DAV_Approximations, DAV_VSTModuleWithPrograms, AdhesiveGUI;

procedure TAdhesiveDataModule.VSTModuleOpen(Sender: TObject);
var
  Preset : Integer;
const
  CPresets : array [1..10, 0..10] of Single = (
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0));
begin
 FCompressor := TLightweightSoftKneeCompressor.Create;
 FCompressor.SampleRate := SampleRate;
 FFilter := TButterworthHighpassFilter.Create;
 FFilter.SampleRate := SampleRate;
 FFilter.Order := 3;

 // initialize parameters
 Parameter[ 0] := -22;
 Parameter[ 1] := 16;
 Parameter[ 2] := 10;
 Parameter[ 3] := 3;
 Parameter[ 4] := 0.1;
 Parameter[ 5] := 400;
 Parameter[ 6] := 100;
 Parameter[ 7] := 1;
 Parameter[ 8] := 1;
 Parameter[ 9] := 20;
 Parameter[10] := 0;

 Programs[0].SetParameters(FParameter);
 for Preset := 1 to numPrograms - 1
  do Programs[Preset].SetParameters(CPresets[Preset]);

 // set editor form class
 EditorFormClass := TFmAdhesive;
end;

procedure TAdhesiveDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FCompressor);
end;

procedure TAdhesiveDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 MixChanged;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateMix;
end;

procedure TAdhesiveDataModule.MixChanged;
begin
 FMix[1] := sqrt(0.01 * Parameter[6]);
 FMix[0] := 1 - FMix[1];
 FMix[1] := FMix[1] * dB_to_Amp(Parameter[1]);
end;

procedure TAdhesiveDataModule.ParameterTimeLabel(
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

procedure TAdhesiveDataModule.ParameterTimeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := AnsiString(FloatToStrF(RoundTo(1E3 * Val, -2), ffGeneral, 3, 3)) else
 if Val < 1000
  then PreDefined := AnsiString(FloatToStrF(RoundTo(Val, -2), ffGeneral, 3, 3))
  else PreDefined := AnsiString(FloatToStrF(RoundTo(1E-3 * Val, -2), ffGeneral, 3, 3));
end;

procedure TAdhesiveDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3));
end;

procedure TAdhesiveDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 //FCompressor.MakeUpGain_dB := Value;
 MixChanged;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateMakeUp;
end;

procedure TAdhesiveDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3));
end;

procedure TAdhesiveDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3));
end;

procedure TAdhesiveDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3));
end;

procedure TAdhesiveDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TAdhesiveDataModule.ParameteActiveChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateOnOff;
end;

procedure TAdhesiveDataModule.ParameterPeakClipChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdatePeakClip;
end;

procedure TAdhesiveDataModule.ParameterSideChainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FFilter) then FFilter.Frequency := Value;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateSideChainFilter;
end;

procedure TAdhesiveDataModule.ChooseProcess;
begin
 case Round(Parameter[7]) of
  0 : OnProcess := VSTModuleProcessBypass;
  1 : case Round(Parameter[8]) of
       0 : case Round(Parameter[10]) of
            0: OnProcess := VSTModuleProcess;
            1: OnProcess := VSTModuleProcessSC;
           end;
       1 : case Round(Parameter[10]) of
            0: OnProcess := VSTModuleProcessPeakClip;
            1: OnProcess := VSTModuleProcessSCPeakClip;
           end;
      end;
 end;
 OnProcess32Replacing := OnProcess;
end;

procedure TAdhesiveDataModule.ParameterExtSideChsinChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateExtSideChain;
end;

procedure TAdhesiveDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor) then FCompressor.Attack := Value;

 // update GUI if necessary
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateAttack;
end;

procedure TAdhesiveDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor) then FCompressor.Release := Value;

 // update GUI if necessary
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateRelease;
end;

procedure TAdhesiveDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor) then FCompressor.Threshold_dB := Value;

 // update GUI if necessary
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateThreshold;
end;

procedure TAdhesiveDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor) then FCompressor.Ratio := Value;

 // update GUI if necessary
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateRatio;
end;

procedure TAdhesiveDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor) then FCompressor.Knee_dB := Value;

 // update GUI if necessary
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateKnee;
end;

procedure TAdhesiveDataModule.VSTModuleProcessBypass(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
 Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
 Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
end;

procedure TAdhesiveDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FCompressor do
   begin
    ProcessSample64(FFilter.ProcessSample64(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample])));
    Temp := FMix[0] + FMix[1] * GainReductionFactor;
    Outputs[0, Sample] := Temp * Inputs[0, Sample];
    Outputs[1, Sample] := Temp * Inputs[1, Sample];
   end;
end;

procedure TAdhesiveDataModule.VSTModuleProcessPeakClip(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FCompressor do
   begin
    ProcessSample64(FFilter.ProcessSample64(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample])));
    Temp := FMix[0] + FMix[1] * GainReductionFactor;
    Outputs[0, Sample] := FastTanhOpt3Term(Temp * Inputs[0, Sample]);
    Outputs[1, Sample] := FastTanhOpt3Term(Temp * Inputs[1, Sample]);
   end;
end;

procedure TAdhesiveDataModule.VSTModuleProcessSC(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FCompressor do
   begin
    ProcessSample64(FFilter.ProcessSample64(Inputs[2, Sample]));
    Temp := FMix[0] + FMix[1] * GainReductionFactor;
    Outputs[0, Sample] := Temp * Inputs[0, Sample];
    Outputs[1, Sample] := Temp * Inputs[1, Sample];
   end;
end;

procedure TAdhesiveDataModule.VSTModuleProcessSCPeakClip(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FCompressor do
  begin
   ProcessSample64(FFilter.ProcessSample64(Inputs[2, Sample]));
   Temp := FMix[0] + FMix[1] * GainReductionFactor;
   Outputs[0, Sample] := FastTanhOpt3Term(Temp * Inputs[0, Sample]);
   Outputs[1, Sample] := FastTanhOpt3Term(Temp * Inputs[1, Sample]);
  end;
end;

procedure TAdhesiveDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 // skip invalid samplerates
 if abs(SampleRate) <= 0 then exit;

 if Assigned(FCompressor) then FCompressor.SampleRate := abs(SampleRate);
 if Assigned(FFilter) then FFilter.SampleRate := abs(SampleRate);
end;

end.
