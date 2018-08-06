unit AdvancedClipperDM;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspUpDownsampling;

type
  TAdvancedClipperDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleProcess(const Inputs,
      Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs,
      Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParaOSFactor1Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOSFactor2Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFilterOrder1Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFilterOrder2Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamInputGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOutputGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamBW2Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamBW1Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamRoundDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamHardClipDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamHardClipChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FUpDownSampling  : array [0..1, 0..1] of TDAVUpDownsampling;
    FInputGain       : Single;
    FOutputGain      : Single;
    FHardClip        : Boolean;
    FReleaseFactor   : Single;
    FCriticalSection : TCriticalSection;
    FPeakStage1      : Single;
    FPeakStage2      : Single;
    FPeakInput       : Single;
  public
    property PeakInput: Single read FPeakInput;
    property PeakStage1: Single read FPeakStage1;
    property PeakStage2: Single read FPeakStage2;
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  Math, DAV_Common, DAV_VSTModuleWithPrograms, {$IFDEF HAS_UNIT_ANSISTRINGS} 
  AnsiStrings, {$ENDIF}AdvancedClipperGUI;

procedure TAdvancedClipperDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TAdvancedClipperDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TAdvancedClipperDataModule.VSTModuleOpen(Sender: TObject);
var
  StageIndex   : Integer;
  ChannelIndex : Integer;
begin
 FHardClip      := False;
 FInputGain     := 1;
 FOutputGain    := 1;
 FReleaseFactor := 0.999999;
 FPeakStage1    := 0;
 FPeakStage2    := 0;
 FPeakInput     := 0;

 for StageIndex := 0 to Length(FUpDownSampling) - 1 do
  for ChannelIndex := 0 to Length(FUpDownSampling[StageIndex]) - 1
   do FUpDownSampling[StageIndex, ChannelIndex] := TDAVUpDownsampling.Create;

 EditorFormClass := TFmAdvancedClipper;

 Parameter[0] := -0.1;
 Parameter[1] := 4;
 Parameter[2] := 6;
 Parameter[3] := 99.5;
 Parameter[4] := 1;
 Parameter[5] := 0;
 Parameter[6] := 99.8;
 Parameter[7] := 0;

 with ProgramByName['Default'] do
  begin
   Parameter[0] := -0.1;
   Parameter[1] := 4;
   Parameter[2] := 6;
   Parameter[3] := 99.5;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.8;
   Parameter[7] := 0;
  end;
 with ProgramByName['Bypass'] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 1;
   Parameter[2] := 0;
   Parameter[3] := 99.5;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.5;
   Parameter[7] := 0;
  end;
 with ProgramByName['Light'] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 2;
   Parameter[2] := 4;
   Parameter[3] := 99.5;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.5;
   Parameter[7] := 0;
  end;
 with ProgramByName['Normal'] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 4;
   Parameter[2] := 8;
   Parameter[3] := 99.5;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.5;
   Parameter[7] := 0;
  end;
 with ProgramByName['More'] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 8;
   Parameter[2] := 16;
   Parameter[3] := 99.5;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.5;
   Parameter[7] := 0;
  end;
 with ProgramByName['Even More'] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 8;
   Parameter[2] := 16;
   Parameter[3] := 99.5;
   Parameter[4] := 8;
   Parameter[5] := 16;
   Parameter[6] := 99.8;
   Parameter[7] := 0;
  end;
 with ProgramByName['True Bypass'] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 1;
   Parameter[2] := 0;
   Parameter[3] := 99.5;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.5;
   Parameter[7] := 0;
  end;
 with ProgramByName['Clip Art!'] do
  begin
   Parameter[0] := 6;
   Parameter[1] := 8;
   Parameter[2] := 16;
   Parameter[3] := 99.9;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.7;
   Parameter[7] := 0;
  end;
 with ProgramByName['Rippler'] do
  begin
   Parameter[0] := 6;
   Parameter[1] := 8;
   Parameter[2] := 0;
   Parameter[3] := 99.9;
   Parameter[4] := 8;
   Parameter[5] := 16;
   Parameter[6] := 99.7;
   Parameter[7] := -2;
  end;
end;

procedure TAdvancedClipperDataModule.VSTModuleClose(Sender: TObject);
var
  StageIndex   : Integer;
  ChannelIndex : Integer;
begin
 for StageIndex := 0 to Length(FUpDownSampling) - 1 do
  for ChannelIndex := 0 to Length(FUpDownSampling[StageIndex]) - 1
   do FreeAndNil(FUpDownSampling[StageIndex, ChannelIndex]);
end;

procedure TAdvancedClipperDataModule.ParaOSFactor1Change(Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FUpDownSampling[0]) - 1 do
   if Assigned(FUpDownSampling[0, ChannelIndex])
    then FUpDownSampling[0, ChannelIndex].Factor := Round(Value);
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmAdvancedClipper
  then TFmAdvancedClipper(EditorForm).UpdateOSFactor1;
end;

procedure TAdvancedClipperDataModule.ParamOSFactor2Change(Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FUpDownSampling[1]) - 1 do
   if Assigned(FUpDownSampling[1, ChannelIndex])
    then FUpDownSampling[1, ChannelIndex].Factor := Round(Value);
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmAdvancedClipper
  then TFmAdvancedClipper(EditorForm).UpdateOSFactor2;
end;

procedure TAdvancedClipperDataModule.ParamInputGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FInputGain := dB_to_Amp(Value);

 // update GUI
 if EditorForm is TFmAdvancedClipper
  then TFmAdvancedClipper(EditorForm).UpdateInputGain;
end;

procedure TAdvancedClipperDataModule.ParamRoundDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[Index])));
end;

procedure TAdvancedClipperDataModule.ParamHardClipDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Boolean(Round(Parameter[Index]))
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TAdvancedClipperDataModule.ParamHardClipChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FHardClip := Boolean(Round(Value));

 // update GUI
 if EditorForm is TFmAdvancedClipper
  then TFmAdvancedClipper(EditorForm).UpdateHardClip;
end;

procedure TAdvancedClipperDataModule.ParamBW1Change(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FUpDownSampling[0]) - 1 do
   if Assigned(FUpDownSampling[0, ChannelIndex])
    then FUpDownSampling[0, ChannelIndex].TransitionBandwidth := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAdvancedClipperDataModule.ParamBW2Change(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FUpDownSampling[1]) - 1 do
   if Assigned(FUpDownSampling[1, ChannelIndex])
    then FUpDownSampling[1, ChannelIndex].TransitionBandwidth := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAdvancedClipperDataModule.ParamOutputGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FOutputGain := dB_to_Amp(Value);

 // update GUI
 if EditorForm is TFmAdvancedClipper
  then TFmAdvancedClipper(EditorForm).UpdateOutputGain;
end;

procedure TAdvancedClipperDataModule.ParamFilterOrder1Change(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FUpDownSampling[0]) - 1 do
   if Assigned(FUpDownSampling[0, ChannelIndex])
    then FUpDownSampling[0, ChannelIndex].Order := Round(Value);
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmAdvancedClipper
  then TFmAdvancedClipper(EditorForm).UpdateOrder1;
end;

procedure TAdvancedClipperDataModule.ParamFilterOrder2Change(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FUpDownSampling[1]) - 1 do
   if Assigned(FUpDownSampling[1, ChannelIndex])
    then FUpDownSampling[1, ChannelIndex].Order := Round(Value);
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmAdvancedClipper
  then TFmAdvancedClipper(EditorForm).UpdateOrder2;
end;

procedure TAdvancedClipperDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex   : Integer;
  SampleIndex    : Integer;
  SubSampleIndex : Integer;
  InterStage     : Double;
  SubSamples     : array [0..15] of Double;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to Min(BlockSize, SampleFrames) - 1 do
   for ChannelIndex := 0 to Length(FUpDownSampling) - 1 do
    begin
     // multiply input data with input gain
     if IsNan(Inputs[ChannelIndex, SampleIndex])
      then InterStage := 0
      else InterStage := FInputGain * Inputs[ChannelIndex, SampleIndex];

     // detect input peak level
     FPeakInput := FPeakInput * FReleaseFactor;
     if Abs(InterStage) > FPeakInput
      then FPeakInput := Abs(InterStage);

     // first stage
     FUpDownSampling[0, ChannelIndex].Upsample64(InterStage, @SubSamples);
     for SubSampleIndex := 0 to FUpDownSampling[0, ChannelIndex].Factor - 1
      do SubSamples[SubSampleIndex] := (Abs(SubSamples[SubSampleIndex] + 1) - Abs(SubSamples[SubSampleIndex] - 1)) * 0.5;
     InterStage := FUpDownSampling[0, ChannelIndex].Downsample64(@SubSamples);

     // detect stage 1 peak level
     FPeakStage1 := FPeakStage1 * FReleaseFactor;
     if Abs(InterStage) > FPeakStage1
      then FPeakStage1 := Abs(InterStage);

     // second stage
     FUpDownSampling[1, ChannelIndex].Upsample64(InterStage, @SubSamples);
     for SubSampleIndex := 0 to FUpDownSampling[1, ChannelIndex].Factor - 1
      do SubSamples[SubSampleIndex] := (Abs(SubSamples[SubSampleIndex] + 1) - Abs(SubSamples[SubSampleIndex] - 1)) * 0.5;
     SubSamples[0] := FUpDownSampling[1, ChannelIndex].Downsample64(@SubSamples);

     // detect stage 2 peak level
     FPeakStage2 := FPeakStage2 * FReleaseFactor;
     if Abs(SubSamples[0]) > FPeakStage2
      then FPeakStage2 := Abs(SubSamples[0]);

     // process hard clip at 0 dBFS
     if FHardClip
      then SubSamples[0] := (Abs(SubSamples[0] + 1) - Abs(SubSamples[0] - 1)) * 0.5;

     // shift output ceiling
     Outputs[ChannelIndex, SampleIndex] := FOutputGain * SubSamples[0];
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAdvancedClipperDataModule.VSTModuleProcessDoubleReplacing(
  const Inputs, Outputs: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Cardinal);
var
  ChannelIndex   : Integer;
  SampleIndex    : Integer;
  SubSampleIndex : Integer;
  InterStage     : Double;
  SubSamples     : array [0..15] of Double;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to Min(BlockSize, SampleFrames) - 1 do
   for ChannelIndex := 0 to Length(FUpDownSampling) - 1 do
    begin
     // multiply input data with input gain
     if IsNan(Inputs[ChannelIndex, SampleIndex])
      then InterStage := 0
      else InterStage := FInputGain * Inputs[ChannelIndex, SampleIndex];

     // detect input peak level
     FPeakInput := FPeakInput * FReleaseFactor;
     if Abs(InterStage) > FPeakInput
      then FPeakInput := Abs(InterStage);

     // first stage
     FUpDownSampling[0, ChannelIndex].Upsample64(InterStage, @SubSamples);
     for SubSampleIndex := 0 to FUpDownSampling[0, ChannelIndex].Factor - 1
      do SubSamples[SubSampleIndex] := (Abs(SubSamples[SubSampleIndex] + 1) - Abs(SubSamples[SubSampleIndex] - 1)) * 0.5;
     InterStage := FUpDownSampling[0, ChannelIndex].Downsample64(@SubSamples);

     // detect stage 1 peak level
     FPeakStage1 := FPeakStage1 * FReleaseFactor;
     if Abs(InterStage) > FPeakStage1
      then FPeakStage1 := Abs(InterStage);

     // second stage
     FUpDownSampling[1, ChannelIndex].Upsample64(InterStage, @SubSamples);
     for SubSampleIndex := 0 to FUpDownSampling[1, ChannelIndex].Factor - 1
      do SubSamples[SubSampleIndex] := (Abs(SubSamples[SubSampleIndex] + 1) - Abs(SubSamples[SubSampleIndex] - 1)) * 0.5;
     SubSamples[0] := FUpDownSampling[1, ChannelIndex].Downsample64(@SubSamples);

     // detect stage 2 peak level
     FPeakStage2 := FPeakStage2 * FReleaseFactor;
     if Abs(SubSamples[0]) > FPeakStage2
      then FPeakStage2 := Abs(SubSamples[0]);

     // process hard clip at 0 dBFS
     if FHardClip
      then SubSamples[0] := (Abs(SubSamples[0] + 1) - Abs(SubSamples[0] - 1)) * 0.5;

     // shift output ceiling
     Outputs[ChannelIndex, SampleIndex] := FOutputGain * SubSamples[0];
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAdvancedClipperDataModule.VSTModuleResume(Sender: TObject);
var
  StageIndex   : Integer;
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for StageIndex := 0 to Length(FUpDownSampling) - 1 do
   for ChannelIndex := 0 to Length(FUpDownSampling[StageIndex]) - 1 do
    if Assigned(FUpDownSampling[StageIndex, ChannelIndex])
     then FUpDownSampling[StageIndex, ChannelIndex].ResetStates;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAdvancedClipperDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  StageIndex   : Integer;
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  // update samplerate
  if Abs(SampleRate) > 0 then
   for StageIndex := 0 to Length(FUpDownSampling) - 1 do
    for ChannelIndex := 0 to Length(FUpDownSampling[StageIndex]) - 1 do
     if Assigned(FUpDownSampling[StageIndex, ChannelIndex])
      then FUpDownSampling[StageIndex, ChannelIndex].SampleRate := Abs(SampleRate);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
