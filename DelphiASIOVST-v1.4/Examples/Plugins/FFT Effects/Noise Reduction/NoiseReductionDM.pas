unit NoiseReductionDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Classes, Forms,
  SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspDelayLines,
  DAV_DspSpectralNoiseReduction, DAV_DspWindowFunctions
  {$IFDEF Use_IPPS}, DAV_DspWindowFunctionsAdvanced{$ENDIF},
  DAV_VSTCustomModule;

type
  TNoiseReductionModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWindowFunctionChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWindowFunctionDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFftOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFftOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure Parameter2DigitDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterThresholdOffsetChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure VSTModuleResume(Sender: TObject);
  private
    FCriticalSection : TCriticalSection;
    FSamplesCaptured : Integer;
    FIsMatching      : Boolean;
    FAdditionalDelay : array of TDelayLineSamples32;
    FNoiseReduction  : array of TNoiseReduction32;
    function GetTimeCaptured: Single;
  public
    property TimeCaptured: Single read GetTimeCaptured;
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  SysUtils, {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF}
  NoiseReductionGui;

procedure TNoiseReductionModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;

 with ParameterProperties[2] do
  begin
   Max := Length(GWindowFunctions) - 1;
   MaxInteger := Length(GWindowFunctions) - 1;
  end;
end;

procedure TNoiseReductionModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TNoiseReductionModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 Assert(numInputs = numOutputs);

 InitialDelay := 1 shl ParameterProperties[1].MaxInteger +
   1 shl (ParameterProperties[1].MaxInteger - 1);

 SetLength(FNoiseReduction, numInputs);
 for ChannelIndex := 0 to Length(FNoiseReduction) - 1
  do FNoiseReduction[ChannelIndex] := TNoiseReduction32.Create;

 SetLength(FAdditionalDelay, numInputs);
 for ChannelIndex := 0 to Length(FAdditionalDelay) - 1
  do FAdditionalDelay[ChannelIndex] := TDelayLineSamples32.Create(512);

 FEditorRect.Bottom := 147;
 FEditorRect.Right := 348;

 // initialize parameters
 Parameter[0] := 8;
 Parameter[1] := 9;
 Parameter[2] := 4;
 Parameter[3] := 10;
 Parameter[4] := 1;
 Parameter[5] := 0.5;
 Parameter[6] := 25;
 Parameter[7] := 0;

 with Programs[0] do
  begin
   Parameter[0] := 8;
   Parameter[1] := 9;
   Parameter[2] := 4;
   Parameter[3] := 10;
   Parameter[4] := 1;
   Parameter[5] := 0.5;
   Parameter[6] := 25;
   Parameter[7] := 0;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 10;
   Parameter[1] := 9;
   Parameter[2] := 8;
   Parameter[3] := 50;
   Parameter[4] := 2;
   Parameter[5] := 0.1;
   Parameter[6] := 10;
   Parameter[7] := 0;
  end;
 with Programs[2] do
  begin
   Parameter[0] := 12;
   Parameter[1] := 9;
   Parameter[2] := 1;
   Parameter[3] := 80;
   Parameter[4] := 0.5;
   Parameter[5] := 0.02;
   Parameter[6] := 1;
   Parameter[7] := 0;
  end;
 with Programs[3] do
  begin
   Parameter[0] := 13;
   Parameter[1] := 9;
   {$IFDEF Use_IPPS}
   Parameter[2] := 11;
   {$ELSE}
   Parameter[2] := 4;
   {$ENDIF}
   Parameter[3] := 100;
   Parameter[4] := 0;
   Parameter[5] := 0.01;
   Parameter[6] := 0.5;
   Parameter[7] := 0;
  end;

 // set editor class
 EditorFormClass := TFmNoiseReduction;
end;

procedure TNoiseReductionModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FNoiseReduction) - 1
  do FreeAndNil(FNoiseReduction[ChannelIndex]);

 for ChannelIndex := 0 to Length(FAdditionalDelay) - 1
  do FreeAndNil(FAdditionalDelay[ChannelIndex]);
end;

procedure TNoiseReductionModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FIsMatching := Value > 0.5;

 // reset sample counter
 if FIsMatching
  then FSamplesCaptured := 0;

 // mark noise reduction for matching
 for ChannelIndex := 0 to Length(FNoiseReduction) - 1 do
  if Assigned(FNoiseReduction[ChannelIndex])
   then FNoiseReduction[ChannelIndex].Match := FIsMatching;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateMatchThreshold;
end;

procedure TNoiseReductionModule.ParameterWindowFunctionChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  WindowIndex  : Integer;
begin
 FCriticalSection.Enter;
 try
  WindowIndex := Round(Parameter[Index]);
  if (WindowIndex < 0) or (WindowIndex >= Length(GWindowFunctions))
   then WindowIndex := 0;

  for ChannelIndex := 0 to Length(FNoiseReduction) - 1 do
   if Assigned(FNoiseReduction[ChannelIndex])
    then FNoiseReduction[ChannelIndex].WindowFunctionClass := GWindowFunctions[WindowIndex];
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateWindowFunction;
end;

procedure TNoiseReductionModule.ParameterWindowFunctionDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := GWindowFunctions[Round(Parameter[Index])].GetWindowFunctionName;
end;

procedure TNoiseReductionModule.ParameterThresholdOffsetChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FNoiseReduction) - 1 do
  if Assigned(FNoiseReduction[ChannelIndex])
   then FNoiseReduction[ChannelIndex].ThresholdOffset := Value;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateThresholdOffset;
end;

procedure TNoiseReductionModule.ParameterTimeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 1
  then PreDefined := AnsiString(FloatToStrF(Parameter[Index] * 1E3, ffGeneral, 3, 1))
  else PreDefined := AnsiString(FloatToStrF(Parameter[Index], ffGeneral, 3, 1));
end;

procedure TNoiseReductionModule.ParameterTimeLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
if Parameter[Index] < 1
  then PreDefined := 'µs'
  else PreDefined := 'ms';
end;

function TNoiseReductionModule.GetTimeCaptured: Single;
begin
 Result := FSamplesCaptured / FSampleRate;
end;

procedure TNoiseReductionModule.Parameter2DigitDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(Parameter[Index], ffGeneral, 3, 1));
end;

procedure TNoiseReductionModule.ParameterFftOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
  Delay   : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FNoiseReduction) - 1 do
   if Assigned(FNoiseReduction[Channel])
    then FNoiseReduction[Channel].FFTOrder := Round(Value);

  Delay := InitialDelay - (1 shl Round(Value - 1) + 1 shl (Round(Value) - 2));

  for Channel := 0 to Length(FAdditionalDelay) - 1 do
   if Assigned(FAdditionalDelay[Channel])
    then FAdditionalDelay[Channel].BufferSize := Delay;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateFftOrder;
end;

procedure TNoiseReductionModule.ParameterFftOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[Index])));
end;

procedure TNoiseReductionModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FNoiseReduction) - 1 do
  if Assigned(FNoiseReduction[ChannelIndex])
   then FNoiseReduction[ChannelIndex].Ratio := 1 / Value;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateRatio;
end;

procedure TNoiseReductionModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FNoiseReduction) - 1 do
  if Assigned(FNoiseReduction[ChannelIndex])
   then FNoiseReduction[ChannelIndex].Knee := Value;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateKnee;
end;

procedure TNoiseReductionModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FNoiseReduction) - 1 do
  if Assigned(FNoiseReduction[ChannelIndex])
   then FNoiseReduction[ChannelIndex].Attack := Value;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateAttack;
end;

procedure TNoiseReductionModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FNoiseReduction) - 1 do
  if Assigned(FNoiseReduction[ChannelIndex])
   then FNoiseReduction[ChannelIndex].Release := Value;

 // update GUI
 if EditorForm is TFmNoiseReduction
  then TFmNoiseReduction(EditorForm).UpdateRelease;
end;

procedure TNoiseReductionModule.VSTModuleResume(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
   if Abs(SampleRate) > 0 then
    for ChannelIndex := 0 to Length(FNoiseReduction) - 1 do
     if Assigned(FNoiseReduction[ChannelIndex])
      then FNoiseReduction[ChannelIndex].Clear;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TNoiseReductionModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
   if Abs(SampleRate) > 0 then
    for ChannelIndex := 0 to Length(FNoiseReduction) - 1 do
     if Assigned(FNoiseReduction[ChannelIndex])
      then FNoiseReduction[ChannelIndex].SampleRate := Abs(SampleRate);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TNoiseReductionModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FNoiseReduction) - 1 do
   begin
    if Assigned(FNoiseReduction[ChannelIndex])
      then FNoiseReduction[ChannelIndex].ProcessBlock(@Inputs[ChannelIndex, 0],
        @Outputs[ChannelIndex, 0], SampleFrames);
    if Assigned(FAdditionalDelay[ChannelIndex])
      then FAdditionalDelay[ChannelIndex].ProcessBlock32(@Outputs[ChannelIndex, 0],
        SampleFrames);
   end;
 finally
  FCriticalSection.Leave;
 end;

 if FIsMatching
  then FSamplesCaptured := FSamplesCaptured + SampleFrames;
end;

end.
