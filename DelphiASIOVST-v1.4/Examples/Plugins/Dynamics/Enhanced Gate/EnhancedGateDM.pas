unit EnhancedGateDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspDynamics;

const
  CNrChannels = 2;

type
  TEnhancedGateDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessBypass(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure EAGThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure EAGPowerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGDuckChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGStereoLinkChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGHoldChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGLoCutChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGHiCutChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGSideChainSourceDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure EAGSideChainSourceChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGRangeChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FEnhancedGates : array [0..CNrChannels - 1] of TAdvancedGate;
    FLevels        : array [0..CNrChannels - 1] of Single;
  public
    property LevelLeft: Single read FLevels[0];
    property LevelRight: Single read FLevels[1];
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, EditorFrm;

procedure TEnhancedGateDataModule.VSTModuleOpen(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to CNrChannels - 1
  do FEnhancedGates[i] := TAdvancedGate.Create;

 // initial parameters 
 Parameter[ 0] :=   1.0;
 Parameter[ 1] := -60.0;
 Parameter[ 2] :=   0.1;
 Parameter[ 3] :=   0.1;
 Parameter[ 4] :=   0.1;
 Parameter[ 5] :=   0.0;
 Parameter[ 6] :=   0.0;
 Parameter[ 7] :=   0.0;
 Parameter[ 8] :=  20.0;
 Parameter[ 9] :=  20.0;
 Parameter[10] :=   1.0;
 Parameter[11] :=   1.0;
 Parameter[12] :=  40.0;

 EditorFormClass := TEditorForm;
end;

procedure TEnhancedGateDataModule.VSTModuleClose(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to CNrChannels - 1
  do FreeAndNil(FEnhancedGates[i]);
end;

procedure TEnhancedGateDataModule.EAGPowerChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Boolean(Round(Value)) then
  begin
   OnProcess := VSTModuleProcess;
   OnProcess32Replacing := OnProcess;
  end
 else
  begin
   OnProcess := VSTModuleProcessBypass;
   OnProcess32Replacing := OnProcess;
  end;
 if Assigned(EditorForm) then
  with TEditorForm(EditorForm) do
   if CBOnOff.Brightness_Percent > 90 <> Boolean(Round(Value)) then
    if Boolean(Round(Value))
     then CBOnOff.Brightness_Percent := 100
     else CBOnOff.Brightness_Percent := 20;
end;

procedure TEnhancedGateDataModule.EAGOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Boolean(Round(Parameter[index]))
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TEnhancedGateDataModule.EAGThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if Assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Threshold_dB := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm
  then TEditorForm(EditorForm).UpdateThreshold;
end;

procedure TEnhancedGateDataModule.EAGRangeChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if Assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Range_dB := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm
  then TEditorForm(EditorForm).UpdateRange;
end;

procedure TEnhancedGateDataModule.EAGRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if Assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Ratio := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm
  then TEditorForm(EditorForm).UpdateRatio;
end;

procedure TEnhancedGateDataModule.EAGSideChainSourceChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm
   do CBSideChain.ItemIndex := Integer(Round(Value));
end;

procedure TEnhancedGateDataModule.EAGSideChainSourceDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: AnsiString);
begin
 if Boolean(Round(Parameter[index]))
  then PreDefined := 'Ext'
  else PreDefined := 'Int';
end;

procedure TEnhancedGateDataModule.EAGLoCutChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if Assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].SideChainLowCut := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with EditorForm As TEditorForm do UpdateLoCut;
end;

procedure TEnhancedGateDataModule.EAGHiCutChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if Assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].SideChainHighCut := 1000 * Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with EditorForm As TEditorForm do UpdateHiCut;
end;

procedure TEnhancedGateDataModule.EAGAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if Assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Attack := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm
  then TEditorForm(EditorForm).UpdateAttack;
end;

procedure TEnhancedGateDataModule.EAGHoldChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if Assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Hold := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm
  then TEditorForm(EditorForm).UpdateHold;
end;

procedure TEnhancedGateDataModule.EAGKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if Assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Knee_dB := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with EditorForm As TEditorForm do UpdateKnee;
end;

procedure TEnhancedGateDataModule.EAGDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if Assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Release := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm
  then TEditorForm(EditorForm).UpdateDecay;
end;

procedure TEnhancedGateDataModule.EAGDuckChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 // update GUI
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm) do
   if CBDuck.Checked <> Boolean(Round(Value))
    then CBDuck.Checked := Boolean(Round(Value));
end;

procedure TEnhancedGateDataModule.EAGStereoLinkChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 // update GUI
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm) do
   if CBStereoLink.Checked <> Boolean(Round(Value))
    then CBStereoLink.Checked := Boolean(Round(Value));
end;

procedure TEnhancedGateDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to CNrChannels - 1 do
  for ChannelIndex := 0 to SampleFrames - 1 do
   begin
    FEnhancedGates[SampleIndex].InputSample(Inputs[SampleIndex,ChannelIndex]);
    Outputs[SampleIndex,ChannelIndex] := FEnhancedGates[SampleIndex].ProcessSample64(Inputs[SampleIndex, ChannelIndex]);
    FLevels[SampleIndex] := 0.99 * FLevels[SampleIndex];
    if abs(Inputs[SampleIndex, ChannelIndex]) > FLevels[SampleIndex]
     then FLevels[SampleIndex] := abs(Inputs[SampleIndex, ChannelIndex]);
   end;
end;

procedure TEnhancedGateDataModule.VSTModuleProcessBypass(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
begin
 FLevels[0] := 0;
 FLevels[1] := 0;
 for ChannelIndex := 0 to CNrChannels - 1
  do Move(Inputs[ChannelIndex, 0], Outputs[ChannelIndex, 0], SampleFrames * SizeOf(Single));
end;

procedure TEnhancedGateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 if Abs(SampleRate) > 0 then
  for Channel := 0 to CNrChannels - 1 do
   if Assigned(FEnhancedGates[Channel])
    then FEnhancedGates[Channel].SampleRate := Abs(SampleRate);
end;

end.
