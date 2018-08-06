unit SmoothMultibandCompressorDM;

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
  DAV_DspFilterLinkwitzRiley, DAV_DspFilterButterworth, 
  DAV_DspFilterLinearPhaseCrossover;

type
  TBandState = (bsBypass, bsMute, bsSmooth, bsClipped);
  TBandStates = set of TBandState;

  TSmoothMultibandCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
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
    procedure ParameterMakeUpGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterVolumeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLowFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterVolumeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterStateChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FGain                  : Single;
    FFeedForwCompressor    : array [0..2] of TLightweightSoftKneeCompressor;
    FFeedBackCompressor    : array [0..2] of TLightweightSoftKneeFeedbackLikeCompressor;
    FActualCompressor      : array [0..2] of TCustomCompressor;
    FLinkwitzRiley         : array [0..1] of TLinkwitzRiley;
    FCrossoverFilter       : array [0..1] of TButterworthSplitBandFilter;
    FLinearPhaseCrossover  : array [0..1] of TLinearPhaseCrossover;
    FStates                : array [0..2] of TBandStates;
    FMidiLearnParameter    : Integer;
    function GetLightweightCompressor(Index: Integer): TCustomCompressor;
    procedure ChooseProcess;
    function GetAutoGain(Index: Integer): Boolean;
    procedure SetAutoGain(Index: Integer; const Value: Boolean);
    function GetBandStates(Index: Integer): TBandStates;
  public
    property LightweightCompressor[Index: Integer]: TCustomCompressor read GetLightweightCompressor;
    property AutoGain[Index: Integer]: Boolean read GetAutoGain write SetAutoGain;
    property MidiLearnParameter: Integer read FMidiLearnParameter write FMidiLearnParameter;
    property BandStates[Index: Integer]: TBandStates read GetBandStates;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, Graphics, DAV_Common, DAV_Approximations, DAV_VSTModuleWithPrograms,
  SmoothMultibandCompressorGUI;

procedure TSmoothMultibandCompressorDataModule.VSTModuleOpen(Sender: TObject);
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
 for Channel := 0 to Length(FFeedForwCompressor) - 1 do
  begin
   FFeedForwCompressor[Channel] := TLightweightSoftKneeCompressor.Create;
   FFeedForwCompressor[Channel].SampleRate := SampleRate;
  end;

 for Channel := 0 to Length(FFeedBackCompressor) - 1 do
  begin
   FFeedBackCompressor[Channel] := TLightweightSoftKneeFeedbackLikeCompressor.Create;
   FFeedBackCompressor[Channel].SampleRate := SampleRate;
   FActualCompressor[Channel] := FFeedBackCompressor[Channel];
  end;

 for Channel := 0 to Length(FLinearPhaseCrossover) - 1 do
  begin
   FLinearPhaseCrossover[Channel] := TLinearPhaseCrossover.Create;
   FLinearPhaseCrossover[Channel].SampleRate := SampleRate;
   FLinearPhaseCrossover[Channel].FilterLength := 31;
  end;

 for Channel := 0 to Length(FLinkwitzRiley) - 1 do
  begin
   FLinkwitzRiley[Channel] := TLinkwitzRiley.Create(1);
   FLinkwitzRiley[Channel].SampleRate := SampleRate;
   FLinkwitzRiley[Channel].Order := 1;
  end;

 for Channel := 0 to Length(FCrossoverFilter) - 1 do
  begin
   FCrossoverFilter[Channel] := TButterworthSplitBandFilter.Create(3);
   FCrossoverFilter[Channel].SampleRate := SampleRate;
   FCrossoverFilter[Channel].Order := 3;
  end;

 Parameter[ 0] := 300;
 Parameter[ 1] := 800;
 Parameter[ 2] := 0;
 Parameter[ 3] := 5;
 Parameter[ 4] := 50;
 Parameter[ 5] := -10;
 Parameter[ 6] := 4;
 Parameter[ 7] := 3;
 Parameter[ 8] := 3;
 Parameter[ 9] := 1;
 Parameter[10] := 5;
 Parameter[11] := 50;
 Parameter[12] := -10;
 Parameter[13] := 4;
 Parameter[14] := 3;
 Parameter[15] := 3;
 Parameter[16] := 1;
 Parameter[17] := 5;
 Parameter[18] := 50;
 Parameter[19] := -10;
 Parameter[20] := 4;
 Parameter[21] := 3;
 Parameter[22] := 3;
 Parameter[23] := 1;
 Parameter[24] := 0;

 Programs[0].SetParameters(FParameter);
(*
 for Channel := 1 to numPrograms - 1
  do Programs[Channel].SetParameters(CPresets[Channel]);
*)
end;

procedure TSmoothMultibandCompressorDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFeedForwCompressor) - 1
  do FreeAndNil(FFeedForwCompressor[Channel]);
 for Channel := 0 to Length(FFeedBackCompressor) - 1
  do FreeAndNil(FFeedBackCompressor[Channel]);
end;

procedure TSmoothMultibandCompressorDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmSmoothMultibandCompressor.CreateNew(Self);
  with TFmSmoothMultibandCompressor(GUI) do
   begin
    OnCreate := FormCreate;
    OnDestroy := FormDestroy;
    OnMouseDown := FormMouseDown;
    OnMouseMove := FormMouseMove;
    OnMouseUp := FormMouseUp;
    OnMouseWheel := FormMouseWheel;
    OnResize := FormResize;
    OnPaint := FormPaint;
    OnShow := FormShow;
    BorderStyle := bsNone;
    FormCreate(Self);
    Caption := 'Smooth Multiband Compressor';
    ClientHeight := 402;
    ClientWidth := 800;
    Color := clBlack;
    Font.Charset := DEFAULT_CHARSET;
    Font.Color := clWindowText;
    Font.Height := -11;
    Font.Name := 'Arial';
    OldCreateOrder := False;
    Scaled := False;
    PixelsPerInch := 96;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val >= 1000
  then PreDefined := 'kHz';
end;

procedure TSmoothMultibandCompressorDataModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1000
  then PreDefined := FloatToStrF(RoundTo(Val, -2), ffGeneral, 3, 3)
  else PreDefined := FloatToStrF(RoundTo(1E-3 * Val, -2), ffGeneral, 3, 3);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterVolumeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FGain := db_to_Amp(Value);

 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do UpdateOutputGain;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterTimeLabel(
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

procedure TSmoothMultibandCompressorDataModule.SetAutoGain(Index: Integer;
  const Value: Boolean);
begin
 if Index in [0..Length(FActualCompressor) - 1] then
  begin
   FFeedForwCompressor[Index].AutoMakeUp := Value;
   FFeedBackCompressor[Index].AutoMakeUp := Value;
   if EditorForm is TFmSmoothMultibandCompressor then
    with TFmSmoothMultibandCompressor(EditorForm) do
     case Index of
      0: UpdateLowAutoMakeUpGain;
      1: UpdateMidAutoMakeUpGain;
      2: UpdateHighAutoMakeUpGain;
     end;
  end else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterVolumeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 Predefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 4, 4);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterTimeDisplay(
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

procedure TSmoothMultibandCompressorDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterLowFreqChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLinearPhaseCrossover) - 1 do
  begin
   FLinkwitzRiley[Channel].Frequency := Value;
   FCrossoverFilter[Channel].Frequency := Value;
  end;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do UpdateLowFrequency;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterHighChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLinearPhaseCrossover) - 1
  do FLinearPhaseCrossover[Channel].Frequency := Value;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do UpdateHighFrequency;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmSmoothMultibandCompressor
  then TFmSmoothMultibandCompressor(EditorForm).UpdateLimit;
end;

procedure TSmoothMultibandCompressorDataModule.ChooseProcess;
begin
 case Round(Parameter[2]) of
  0 : OnProcess := VSTModuleProcessMono;
  1 : OnProcess := VSTModuleProcessMonoSoftClip;
 end;
 OnProcess32Replacing := OnProcess;
end;

function TSmoothMultibandCompressorDataModule.GetAutoGain(
  Index: Integer): Boolean;
begin
 if Index in [0..Length(FActualCompressor) - 1]
  then result := FActualCompressor[Index].AutoMakeUp
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

function TSmoothMultibandCompressorDataModule.GetLightweightCompressor(Index: Integer): TCustomCompressor;
begin
 if Index in [0..Length(FActualCompressor) - 1]
  then result := FActualCompressor[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

function TSmoothMultibandCompressorDataModule.GetBandStates(
  Index: Integer): TBandStates;
begin
 if Index in [0..2]
  then result := FStates[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 3) div 6;
 FFeedForwCompressor[Band].Attack := Value;
 FFeedBackCompressor[Band].Attack := Value;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowAttack;
    1: UpdateMidAttack;
    2: UpdateHighAttack;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 4) div 6;
 FFeedForwCompressor[Band].Release := Value;
 FFeedBackCompressor[Band].Release := Value;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowRelease;
    1: UpdateMidRelease;
    2: UpdateHighRelease;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 5) div 6;
 FFeedForwCompressor[Band].Threshold_dB := Value;
 FFeedBackCompressor[Band].Threshold_dB := Value;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowThreshold;
    1: UpdateMidThreshold;
    2: UpdateHighThreshold;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 6) div 6;
 FFeedForwCompressor[Band].Ratio := Value;
 FFeedBackCompressor[Band].Ratio := Value;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowRatio;
    1: UpdateMidRatio;
    2: UpdateHighRatio;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 7) div 6;
 FFeedForwCompressor[Band].Knee_dB := Value;
 FFeedBackCompressor[Band].Knee_dB := Value;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowKnee;
    1: UpdateMidKnee;
    2: UpdateHighKnee;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 8) div 6;
 FFeedBackCompressor[Band].MakeUpGain_dB := Value;
 FFeedForwCompressor[Band].MakeUpGain_dB := Value;

 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowMakeUp;
    1: UpdateMidMakeUp;
    2: UpdateHighMakeUp;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterStateChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band    : Integer;
  State   : Cardinal;
begin
 Band := (Index - 9) div 6;
 State := Round(Value);
 if (State and 1) > 0
  then FStates[Band] := FStates[Band] + [bsBypass]
  else FStates[Band] := FStates[Band] - [bsBypass];
 if (State and 2) > 0
  then FStates[Band] := FStates[Band] + [bsMute]
  else FStates[Band] := FStates[Band] - [bsMute];
 if (State and 4) > 0
  then FStates[Band] := FStates[Band] + [bsSmooth]
  else FStates[Band] := FStates[Band] - [bsSmooth];
 if (State and 8) > 0
  then FStates[Band] := FStates[Band] + [bsClipped]
  else FStates[Band] := FStates[Band] - [bsClipped];

 if (bsSmooth in FStates[Band])
  then FActualCompressor[Band] := FFeedBackCompressor[Band]
  else FActualCompressor[Band] := FFeedForwCompressor[Band];

 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do
   begin
    case Band of
     0: UpdateLowState;
     1: UpdateMidState;
     2: UpdateHighState;
    end;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Temp   : array [0..2] of Single;
  FD     : array [0..1, 0..2] of Single;
const
  CDenorm32 : Single = 1E-12;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   // split high
   FLinearPhaseCrossover[0].ProcessSample(CDenorm32 + Inputs[0, Sample], FD[0, 1], FD[0, 2]);
   FLinearPhaseCrossover[1].ProcessSample(CDenorm32 + Inputs[1, Sample], FD[1, 1], FD[1, 2]);

   // split low
   FLinkwitzRiley[0].ProcessSample(FD[0, 1] - CDenorm32, FD[0, 0], FD[0, 1]);
   FLinkwitzRiley[1].ProcessSample(FD[1, 1] - CDenorm32, FD[1, 0], FD[1, 1]);
(*
   FCrossoverFilter[0].ProcessSample(FD[0, 1] - CDenorm32, FD[0, 0], FD[0, 1]);
   FCrossoverFilter[1].ProcessSample(FD[1, 1] - CDenorm32, FD[1, 0], FD[1, 1]);
*)

   // compress & copy gain reduction
   with FActualCompressor[0] do
    begin
     InputSample(CHalf32 * (FD[0, 0] + FD[1, 0]));
     Temp[0] := GainReductionFactor * MakeUpGain;
    end;
   with FActualCompressor[1] do
    begin
     InputSample(CHalf32 * (FD[0, 1] + FD[1, 1]));
     Temp[1] := GainReductionFactor * MakeUpGain;
    end;
   with FActualCompressor[2] do
    begin
     InputSample(CHalf32 * (FD[0, 2] + FD[1, 2]));
     Temp[2] := GainReductionFactor * MakeUpGain;
    end;

   // gain and combine
   Outputs[0, Sample] := FGain * (Temp[0] * FD[0, 0] + Temp[1] * FD[0, 1] - Temp[2] * FD[0, 2]);
   Outputs[1, Sample] := FGain * (Temp[0] * FD[1, 0] + Temp[1] * FD[1, 1] - Temp[2] * FD[1, 2]);
  end;
end;

procedure TSmoothMultibandCompressorDataModule.VSTModuleProcessMonoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Temp   : array [0..2] of Single;
  FD     : array [0..1, 0..2] of Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   // split high
   FLinearPhaseCrossover[0].ProcessSample(CDenorm32 + Inputs[0, Sample], FD[0, 1], FD[0, 2]);
   FLinearPhaseCrossover[1].ProcessSample(CDenorm32 + Inputs[1, Sample], FD[1, 1], FD[1, 2]);

   // split low
   FLinkwitzRiley[0].ProcessSample(FD[0, 1] - CDenorm32, FD[0, 0], FD[0, 1]);
   FLinkwitzRiley[1].ProcessSample(FD[1, 1] - CDenorm32, FD[1, 0], FD[1, 1]);

   // compress
   FActualCompressor[0].ProcessSample64(CHalf32 * (FD[0, 0] + FD[1, 0]));
   FActualCompressor[1].ProcessSample64(CHalf32 * (FD[0, 1] + FD[1, 1]));
   FActualCompressor[2].ProcessSample64(CHalf32 * (FD[0, 2] + FD[1, 2]));

   // copy gain reduction
   Temp[0] := FActualCompressor[0].GainReductionFactor;
   Temp[1] := FActualCompressor[1].GainReductionFactor;
   Temp[2] := FActualCompressor[2].GainReductionFactor;

   // gain and combine
   Outputs[0, Sample] := FGain * FastTanhContinousError4(Temp[0] * FD[0, 0] + Temp[1] * FD[0, 1] + Temp[2] * FD[0, 2]);
   Outputs[1, Sample] := FGain * FastTanhContinousError4(Temp[0] * FD[1, 0] + Temp[1] * FD[1, 1] + Temp[2] * FD[1, 2]);
  end;
end;

procedure TSmoothMultibandCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Band, Channel : Integer;
begin
 for Band := 0 to Length(FActualCompressor) - 1 do
  begin
   FFeedForwCompressor[Band].SampleRate := SampleRate;
   FFeedBackCompressor[Band].SampleRate := SampleRate;
  end;

 for Channel := 0 to Length(FLinearPhaseCrossover) - 1
  do FLinearPhaseCrossover[Channel].SampleRate := SampleRate;

 for Channel := 0 to Length(FLinkwitzRiley) - 1
  do FLinkwitzRiley[Channel].SampleRate := SampleRate;

 for Channel := 0 to Length(FCrossoverFilter) - 1
  do FCrossoverFilter[Channel].SampleRate := SampleRate;
end;

end.
