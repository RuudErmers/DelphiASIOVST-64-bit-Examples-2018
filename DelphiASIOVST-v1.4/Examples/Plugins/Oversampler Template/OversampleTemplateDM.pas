unit OversampleTemplateDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Graphics, Forms,
  {$IFDEF UseCriticalSection} SyncObjs, {$ENDIF}
  DAV_Types, DAV_VSTModule, DAV_VSTEffect, DAV_VSTParameters, DAV_VstHost,
  DAV_VSTModuleWithPrograms, DAV_VSTCustomModule, DAV_DspUpDownsampling,
  DAV_VstOfflineTask, ExtCtrls;

type
  TOversampleTemplateDataModule = class(TVSTModule)
    VstHost: TVstHost;
    Timer: TTimer;
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: NativeUInt);
    procedure VSTModuleEditClose(Sender: TObject; var DestroyForm: Boolean);
    procedure VSTModuleEditIdle(Sender: TObject);
    procedure VSTModuleEditSleep(Sender: TObject);
    procedure VSTModuleEditTop(Sender: TObject);
    procedure VSTModuleGetVU(var VU: Single);
    procedure VSTModuleOfflineNotify(Sender: TObject; const AudioFile: TVstAudioFile; const numAudioFiles: Integer; const start: Boolean);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);

    procedure VSTModuleProcess32OversampleSingle(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64OversampleSingle(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);

    procedure VSTModuleProcessEvents(Sender: TObject; const Events: TVstEvents);
    procedure VSTModuleProcessVarIO(Sender: TObject; const varIo: TVstVariableIo);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleStartProcess(Sender: TObject);
    procedure VSTModuleStopProcess(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleEditorKeyDown(Sender: TObject; var keyCode: TVstKeyCode);
    procedure VSTModuleEditorKeyUp(Sender: TObject; var keyCode: TVstKeyCode);
    procedure VSTModuleAfterProgramChange(Sender: TObject);

    function VSTModuleInputProperties(Sender: TObject; const Index: Integer; var vLabel, shortLabel: string; var SpeakerArrangement: TVstSpeakerArrangementType; var Flags: TVstPinPropertiesFlags): Boolean;
    function VSTModuleOutputProperties(Sender: TObject; const Index: Integer; var vLabel, shortLabel: string; var SpeakerArrangement: TVstSpeakerArrangementType; var Flags: TVstPinPropertiesFlags): Boolean;
    function VSTModuleVendorSpecific(Sender: TObject; const lArg1, lArg2: Integer; const ptrArg: Pointer; const floatArg: Single): Integer;
    procedure VSTModuleOfflinePrepare(Sender: TObject; const OfflineTasks: array of TVstOfflineTask);
    procedure VSTModuleOfflineRun(Sender: TObject; const OfflineTasks: array of TVstOfflineTask);

    procedure CustomParameterDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure CustomParameterLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamOSFactorChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOSFactorDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOversamplingDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamOversamplingChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamPreFilterOrderValue(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPreTransBWChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamCharacterDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamCharChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPostOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPostFilterBWChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamPostCharChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure AudioMasterIdle(Sender: TObject);
    procedure PluginIdle(Sender: TObject);
    procedure AudioMasterAutomate(Sender: TObject;
      Index: Integer; ParameterValue: Single);
  private
    FUpsampler        : array of TDAVUpsampling;
    FDownsampler      : array of TDAVDownsampling;
    FIn64, FOut64     : array of PDAVDoubleFixedArray;
    FIn32, FOut32     : array of PDAVSingleFixedArray;
    FBaseParCount     : Integer;
    FOSActive         : Boolean;
    FOSFactor         : Integer;
    FMaximumBlockSize : Integer;
    FTempBufferSize   : Integer;
    FManualIdle       : Boolean;
    {$IFDEF UseCriticalSection}
    FCriticalSection  : TCriticalSection;
    {$ENDIF}
    procedure SetOSFactor(const NewOSFactor: Integer);
    procedure SetTempBufferSize(const Value: Integer);
    procedure CheckSampleFrames(const SampleFrames: Integer);
    procedure SetManualIdle(const Value: Boolean);
  protected
    procedure ManualIdleChanged; virtual;
    procedure PluginSampleRateChanged; virtual;
    procedure VSTBuffersChanged; virtual;
  public
    function HostCallIdle(const Index, Value: Integer; const ptr: Pointer; const opt: Single): Integer; override;
    function HostCallGetTailSize(const Index, Value: Integer; const ptr: Pointer; const opt: Single): Integer; override;
  published
    property TempBufferSize: Integer read FTempBufferSize write SetTempBufferSize default 0;
    property ManualIdle: Boolean read FManualIdle write SetManualIdle;
  end;

function ConvertOrderToString(Order: Integer): string;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, Dialogs, Controls, Types, OversampleTemplateGUI, DAV_VSTPrograms,
  DAV_VSTModuleWithDsp, DAV_DspFilterButterworth, DAV_DspFilterChebyshev,
  DAV_DspFilterChebyshevType1, DAV_DspBesselFilter;

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
  Result := True;
  TStringList(lParam).Add(lpName);
end;

procedure TOversampleTemplateDataModule.VSTModuleCreate(Sender: TObject);
var
  RN    : TStringList;
  RS    : TResourceStream;
  PI    : TCustomVstPlugIn;
  ch, j : Integer;
  Param : Integer;
  Prog  : Integer;
  str   : AnsiString;
begin
 FBaseParCount            := numParams;
 FOSFactor                := 1;
 FOSActive                := False;
 FTempBufferSize          := 0;
 {$IFDEF UseCriticalSection}
 FCriticalSection         := TCriticalSection.Create;
 {$ENDIF}
 FMaximumBlockSize    := VstHost.BlockSize;
 OnProcess            := VSTModuleProcess32OversampleSingle;
 OnProcess32Replacing := VSTModuleProcess32OversampleSingle;
 OnProcess64Replacing := VSTModuleProcess64OversampleSingle;

 RN := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc, DWord(RN));

  if RN.Count > 0 then
   begin
    PI := VstHost.VstPlugIns[0];

    // load plugin from resource
    RS := TResourceStream.Create(hInstance, RN[0], 'DLL');
    try
     PI.LoadFromStream(RS);
    finally
     FreeAndNil(RS);
    end;

    // set parameter count
    for Param := 0 to VstHost[0].numParams - 1 do
     with ParameterProperties.Add do
      begin
       OnParameterChange        := VSTModuleParameterChange;
       OnCustomParameterLabel   := CustomParameterLabel;
       OnCustomParameterDisplay := CustomParameterDisplay;
      end;

    // activate plugin
    PI.Active := True;

    // scan and rename parameters
    for Param := 0 to VstHost[0].numParams - 1 do
     with ParameterProperties[FBaseParCount + Param]
      do DisplayName := VstHost[0].GetParamName(Param);

    UniqueID    := VstHost[0].UniqueID[1] +
                   VstHost[0].UniqueID[2] +
                   VstHost[0].UniqueID[3] + '²';
    EffectName  := VstHost[0].EffectName + '²';
    ProductName := VstHost[0].ProductString + '²';
    VendorName  := VstHost[0].VendorString + ' (powered by Delphi ASIO & VST Packages)';

    // program replication
    Parameter[0] := 0;
    Parameter[1] := 1;
    Parameter[2] := 4;
    Parameter[3] := 99;
    Parameter[4] := 1;
    Parameter[5] := 4;
    Parameter[6] := 99;

    for Prog := 0 to VstHost[0].numPrograms - 1 do
     with Programs.Add do
      begin
       PI.GetProgramNameIndexed(0, Prog, str);
       if PI.CurrentProgram <> Prog
        then PI.CurrentProgram := Prog;
       Parameter[0] := 0;
       Parameter[1] := 2;
       Parameter[2] := 4;
       Parameter[3] := 99;
       Parameter[4] := 2;
       Parameter[5] := 4;
       Parameter[6] := 99;
       DisplayName := str;
       for j := 0 to PI.numParams - 1
        do Parameter[FBaseParCount + j] := PI.Parameter[j];
      end;
    if numPrograms > 0 then
     begin
      PI.CurrentProgram := 0;
      for j := 0 to PI.numParams - 1
       do Parameter[FBaseParCount + j] := PI.Parameter[j];
     end
    else
     with Programs.Add do
      begin
       DisplayName := 'Default';
       Parameter[0] := 0;
       Parameter[1] := 2;
       Parameter[2] := 4;
       Parameter[3] := 99;
       Parameter[4] := 2;
       Parameter[5] := 4;
       Parameter[6] := 99;
      end;

    // enable 64bit processing if supported by both plugins
    if (effFlagsCanDoubleReplacing in VstHost[0].EffectOptions)
     then
      begin
       Flags := Flags + [effFlagsCanDoubleReplacing];
       ProcessPrecisition := pp64;
      end
     else
      begin
       Flags := Flags - [effFlagsCanDoubleReplacing];
       ProcessPrecisition := pp32;
      end;
    numInputs  := VstHost[0].numInputs;
    numOutputs := VstHost[0].numOutputs;
    PlugCategory := VstHost[0].PlugCategory;
   end
  else
   begin
    Parameter[0] := 0;
    Parameter[1] := 1;
    Parameter[2] := 4;
    Parameter[3] := 99;
    Parameter[4] := 1;
    Parameter[5] := 4;
    Parameter[6] := 99;
   end;
 finally
  FreeAndNil(RN);
 end;

 SetLength(FUpsampler, numInputs);
 for ch := 0 to Length(FUpsampler) - 1
  do FUpsampler[ch] := TDAVUpsampling.Create;
 SetLength(FDownsampler, numOutputs);
 for ch := 0 to Length(FDownsampler) - 1
  do FDownsampler[ch] := TDAVDownsampling.Create;

 SetLength(FIn32, numInputs);
 SetLength(FIn64, numInputs);
 SetLength(FOut32, numOutputs);
 SetLength(FOut64, numOutputs);

 VSTBuffersChanged;
end;

procedure TOversampleTemplateDataModule.VSTModuleDestroy(Sender: TObject);
var
  Channel : Integer;
begin
 {$IFDEF UseCriticalSection}
 FreeAndNil(FCriticalSection);
 {$ENDIF}
 for Channel := 0 to Length(FIn64) - 1 do Dispose(FIn64[Channel]);
 for Channel := 0 to Length(FOut64) - 1 do Dispose(FOut64[Channel]);

 for Channel := 0 to Length(FUpsampler) - 1
  do FreeAndNil(FUpsampler[Channel]);
 for Channel := 0 to Length(FDownsampler) - 1
  do FreeAndNil(FDownsampler[Channel]);

 VSTHost.VstPlugIns.Clear;
end;

procedure TOversampleTemplateDataModule.VSTModuleEditClose(Sender: TObject;
  var DestroyForm: Boolean);
begin
 with VstHost[0] do if EditVisible then CloseEdit;
end;

procedure TOversampleTemplateDataModule.VSTModuleEditIdle(Sender: TObject);
begin
 with VstHost[0] do if EditVisible then EditIdle;
end;

procedure TOversampleTemplateDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: NativeUInt);
var
  Rct      : array [0..1] of TRect;
  Oversize : Integer;
begin
 GUI := TFmOversampler.Create(Self);

 // set plugin GUI size
 if Assigned(VstHost[0]) and VstHost[0].Active then
  with TFmOversampler(GUI) do
   begin
    PnGui.Visible    := True;
    ShBorder.Visible := True;

    if not VstHost[0].EditVisible
     then VstHost[0].ShowEdit(TForm(PnGui));
    Rct[0] := VstHost[0].GetRect;

    Oversize := PnControl.Width - (Rct[0].Right - Rct[0].Left);
    if Oversize < 0 then
     begin
      // current editor is too small, enlarge!
      PnGui.Align := alClient;
      ClientWidth := (Rct[0].Right - Rct[0].Left);
      ClientHeight := PnControl.Height + (Rct[0].Bottom - Rct[0].Top);
      ShBorder.Visible := False;
     end
    else
     begin
      PnGui.Align  := alNone;
      PnGui.Left   := Oversize div 2;
      PnGui.Width  := (Rct[0].Right - Rct[0].Left);

      // calculate new height and y position
      PnGui.Height := (Rct[0].Bottom - Rct[0].Top);
      Oversize     := Round(Oversize * (PnGui.Height) / PnGui.Width);
      PnGui.Top    := PnControl.Height + Oversize div 2;
      ClientHeight := PnControl.Height + PnGui.Height + Oversize;

      // show border
      ShBorder.Visible := True;
      ShBorder.SetBounds(PnGui.Left - ShBorder.Pen.Width,
                         PnGui.Top - ShBorder.Pen.Width,
                         PnGui.Width + 2 * ShBorder.Pen.Width,
                         PnGui.Height + 2 * ShBorder.Pen.Width);
     end;
    if VstHost[0].EditVisible then VstHost[0].CloseEdit;
   end
 else
  with TFmOversampler(GUI) do
   begin
    PnGui.Visible    := False;
    ShBorder.Visible := False;
   end;
end;

procedure TOversampleTemplateDataModule.VSTModuleEditorKeyDown(Sender: TObject;
  var keyCode: TVstKeyCode);
begin
 with VstHost[0] do if EditVisible
  then EditKeyDown(Char(keyCode.Character), keyCode.Virt, TVstModifierKeys(keyCode.Modifier)); 
end;

procedure TOversampleTemplateDataModule.VSTModuleEditorKeyUp(Sender: TObject;
  var keyCode: TVstKeyCode);
begin
 with VstHost[0] do if EditVisible
  then EditKeyUp(Char(keyCode.Character), keyCode.Virt, TVstModifierKeys(keyCode.Modifier))
end;

procedure TOversampleTemplateDataModule.VSTModuleEditSleep(Sender: TObject);
begin
 with VstHost[0] do if EditVisible then EditDeactivate;
end;

procedure TOversampleTemplateDataModule.VSTModuleEditTop(Sender: TObject);
begin
 with VstHost[0] do if EditVisible then EditActivate;
end;

procedure TOversampleTemplateDataModule.VSTModuleGetVU(var VU: Single);
begin
 if VstHost[0].Active then VU := VstHost[0].GetVu;
end;

function TOversampleTemplateDataModule.VSTModuleInputProperties(Sender: TObject;
  const Index: Integer; var vLabel, shortLabel: string;
  var SpeakerArrangement: TVstSpeakerArrangementType;
  var Flags: TVstPinPropertiesFlags): Boolean;
var
  PinProperties : TVstPinProperties;
begin
 Result := False;
 if VstHost[0].Active then
  try
   PinProperties := VstHost[0].GetInputProperties(Index);
   Flags         := PinProperties.Flags;
   vLabel        := StrPas(PAnsiChar(@PinProperties.Caption[0]));
   shortLabel    := StrPas(PAnsiChar(@PinProperties.ShortLabel[0]));
  except
   Result        := False;
  end;
end;

procedure TOversampleTemplateDataModule.VSTModuleOfflineNotify(Sender: TObject;
  const AudioFile: TVstAudioFile; const numAudioFiles: Integer; const start: Boolean);
begin
 if VstHost[0].Active then VstHost[0].OfflineNotify(AudioFile, numAudioFiles, start);
end;

procedure TOversampleTemplateDataModule.VSTModuleOfflinePrepare(Sender: TObject;
  const OfflineTasks: array of TVstOfflineTask);
var
  VstOfflineTaskRecords : array of TVstOfflineTaskRecord;
  i                     : Integer;
begin
 if VstHost[0].Active and (Length(OfflineTasks) > 0) then
  begin
   SetLength(VstOfflineTaskRecords, Length(OfflineTasks));
   for i := 0 to Length(OfflineTasks) - 1
    do VstOfflineTaskRecords[i] := OfflineTasks[i].VstOfflineTaskRecord;
   VstHost[0].OfflinePrepare(VstOfflineTaskRecords[0], Length(OfflineTasks));
  end;
end;

procedure TOversampleTemplateDataModule.VSTModuleOfflineRun(Sender: TObject;
  const OfflineTasks: array of TVstOfflineTask);
var
  VstOfflineTaskRecords : array of TVstOfflineTaskRecord;
  i                     : Integer;
begin
 if VstHost[0].Active and (Length(OfflineTasks) > 0) then
  begin
   SetLength(VstOfflineTaskRecords, Length(OfflineTasks));
   for i := 0 to Length(OfflineTasks) - 1
    do VstOfflineTaskRecords[i] := OfflineTasks[i].VstOfflineTaskRecord;
   VstHost[0].OfflineRun(VstOfflineTaskRecords[0], Length(OfflineTasks));
  end;
end;

function TOversampleTemplateDataModule.VSTModuleOutputProperties(Sender: TObject;
  const Index: Integer; var vLabel, shortLabel: string;
  var SpeakerArrangement: TVstSpeakerArrangementType;
  var Flags: TVstPinPropertiesFlags): Boolean;
var
  PinProperties : TVstPinProperties;
begin
 Result := False;
 if VstHost[0].Active then
  try
   PinProperties := VstHost[0].GetOutputProperties(Index);
   Flags         := PinProperties.Flags;
   vLabel        := StrPas(PAnsiChar(@PinProperties.Caption[0]));
   ShortLabel    := StrPas(PAnsiChar(@PinProperties.ShortLabel[0]));
  except
   Result        := False;
  end;
end;

procedure TOversampleTemplateDataModule.VSTModuleClose(Sender: TObject);
begin
 VstHost[0].Active := False;
end;

procedure TOversampleTemplateDataModule.ParamPreTransBWChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FUpsampler) - 1
  do FUpsampler[Channel].TransitionBandwidth := 0.01 * Value;
end;

procedure TOversampleTemplateDataModule.ParamOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := ConvertOrderToString(Round(Parameter[Index]));
end;

procedure TOversampleTemplateDataModule.ParamPreFilterOrderValue(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FUpsampler) - 1
  do FUpsampler[ch].Order := Round(Value);
end;

procedure TOversampleTemplateDataModule.VSTModuleAfterProgramChange(Sender: TObject);
begin
(*
 with VstHost[0] do if Active then
  begin
   SetProgram(CurrentProgram);
   EditIdle;
   if EditorForm is TFmOversampler
    then TFmOversampler(EditorForm).PnGui.Invalidate;
  end;
*)
end;

procedure TOversampleTemplateDataModule.VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
begin
 VSTBuffersChanged;
end;

procedure TOversampleTemplateDataModule.VSTBuffersChanged;
begin
 {$IFDEF UseCriticalSection}
 FCriticalSection.Enter;
 try
  {$ENDIF}
  VstHost.BlockSize := BlockSize * FOSFactor;
  with VstHost[0] do
   if Active then
    begin
     SetBlockSizeAndSampleRate(BlockSize * FOSFactor, SampleRate * FOSFactor);
     SetBlockSize(BlockSize * FOSFactor);
     SetSampleRate(SampleRate * FOSFactor);
    end;
  FMaximumBlockSize := BlockSize;
  TempBufferSize := FMaximumBlockSize * FOSFactor;
 {$IFDEF UseCriticalSection}
 finally
  FCriticalSection.Leave;
 end;
 {$ENDIF}
end;

procedure TOversampleTemplateDataModule.AudioMasterAutomate(
  Sender: TObject; Index: Integer; ParameterValue: Single);
begin
 Parameter[FBaseParCount + Index] := ParameterValue;
end;

procedure TOversampleTemplateDataModule.AudioMasterIdle(Sender: TObject);
begin
 if not ManualIdle
  then PluginIdle(Sender);
end;

procedure TOversampleTemplateDataModule.PluginIdle(Sender: TObject);
begin
 with VstHost[0] do
  if Active then
   begin
    Idle;
    EditIdle;
   end;
end;

procedure TOversampleTemplateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FUpsampler) - 1
  do FUpsampler[Channel].SampleRate := SampleRate;
 for Channel := 0 to Length(FDownsampler) - 1
  do FDownsampler[Channel].SampleRate := SampleRate;

 PluginSampleRateChanged;
end;

procedure TOversampleTemplateDataModule.PluginSampleRateChanged;
begin
 with VstHost[0] do
  if Active then
   begin
    SetSampleRate(FOSFactor * SampleRate);
    SetBlockSize(FOSFactor * FBlockSize);
   end;
end;

procedure TOversampleTemplateDataModule.VSTModuleStartProcess(Sender: TObject);
begin
 with VstHost[0] do if Active then StartProcess;
end;

procedure TOversampleTemplateDataModule.VSTModuleStopProcess(Sender: TObject);
begin
 with VstHost[0] do if Active then StopProcess;
end;

procedure TOversampleTemplateDataModule.VSTModuleSuspend(Sender: TObject);
begin
 with VstHost[0] do if Active then MainsChanged(False);
end;

function TOversampleTemplateDataModule.VSTModuleVendorSpecific(Sender: TObject;
  const lArg1, lArg2: Integer; const ptrArg: Pointer; const floatArg: Single): Integer;
begin
 Result := 0;
 with VstHost[0] do if Active then Result := VendorSpecific(lArg1, lArg2, ptrArg, floatArg);
end;

procedure TOversampleTemplateDataModule.VSTModuleProcessVarIO(Sender: TObject;
  const varIo: TVstVariableIo);
begin
 with VstHost[0] do if Active then ProcessVarIo(VarIo);
end;

procedure TOversampleTemplateDataModule.VSTModuleResume(Sender: TObject);
begin
 VSTModuleSampleRateChange(Sender, SampleRate);
 with VstHost[0] do if Active then MainsChanged(True);
end;

procedure TOversampleTemplateDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := FBaseParCount;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
   if n >= VstHost.Count then break;
  end;
 if (n < VstHost.Count) and VstHost[n].Active
  then VstHost[n].Parameter[Index - pnr] := Value;
end;

procedure TOversampleTemplateDataModule.CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := FBaseParCount;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
   if n >= VstHost.Count then break;
  end;
 if (n < VstHost.Count) and VstHost[n].Active
  then PreDefined := VstHost[n].GetParamDisplay(Index - pnr);
end;

procedure TOversampleTemplateDataModule.CustomParameterLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := FBaseParCount;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
   if n >= VstHost.Count then break;
  end;
 if (n < VstHost.Count) and VstHost[n].Active
  then PreDefined := VstHost[n].GetParamLabel(Index - pnr);
end;

function TOversampleTemplateDataModule.HostCallGetTailSize(const Index,
  Value: Integer; const ptr: Pointer; const opt: Single): Integer;
begin
 with VstHost[0] do
  if Active
   then Result := VstHost[0].GetTailSize
   else Result := -1;
end;

function TOversampleTemplateDataModule.HostCallIdle(const Index, Value: Integer;
  const ptr: Pointer; const opt: Single): Integer;
begin
 with VstHost[0] do if Active then
  begin
   Idle;
   Result := 0;
  end else Result := -1;
end;

procedure TOversampleTemplateDataModule.ParamPostCharChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FUpsampler) - 1 do
  case Round(Value) of
   4, 5, 6 : FUpsampler[ch].FilterClass := TChebyshev1LowpassFilter;
   else FUpsampler[ch].FilterClass := TButterworthLowpassFilter;
  end;
end;

procedure TOversampleTemplateDataModule.ParamPostFilterBWChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FDownsampler) - 1
  do FDownsampler[ch].TransitionBandwidth := 0.01 * Value;
end;

procedure TOversampleTemplateDataModule.ParamPostOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FDownsampler) - 1
  do FDownsampler[ch].Order := Round(Value);
end;

procedure TOversampleTemplateDataModule.ParamCharChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FUpsampler) - 1 do
  case Round(Value) of
   4, 5, 6 : FUpsampler[ch].FilterClass := TChebyshev1LowpassFilter;
   else FUpsampler[ch].FilterClass := TButterworthLowpassFilter;
  end;
end;

procedure TOversampleTemplateDataModule.ParamCharacterDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  4, 5, 6 : PreDefined := 'Chebyshev';
  else PreDefined := 'Butterworth';
 end;
end;

procedure TOversampleTemplateDataModule.ParamOSFactorDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := IntToStr(Round(Parameter[Index])) + 'x';
end;

function ConvertOrderToString(Order: Integer): string;
begin
 case Order of
   0 : Result := 'Off';
   1 : Result := IntToStr(Order) + 'st';
   2 : Result := IntToStr(Order) + 'nd';
   3 : Result := IntToStr(Order) + 'rd';
  else Result := IntToStr(Order) + 'th';
 end;
end;

procedure TOversampleTemplateDataModule.ParamOversamplingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 {$IFDEF UseCriticalSection}
 FCriticalSection.Enter;
 try
 {$ENDIF}
   FOSActive := Boolean(Round(Value));
   if FOSActive = True
    then SetOSFactor(Round(ParameterByName['OS Factor']))
    else SetOSFactor(1);
 {$IFDEF UseCriticalSection}
 finally
  FCriticalSection.Leave;
 end;
 {$ENDIF}

 if EditorForm is TFmOversampler
  then TFmOversampler(EditorForm).UpdateOverSampling;
end;

procedure TOversampleTemplateDataModule.ParamOSFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 {$IFDEF UseCriticalSection}
 FCriticalSection.Enter;
 try
 {$ENDIF}
   if FOSActive = True
    then SetOSFactor(Round(Value))
    else SetOSFactor(1);
 {$IFDEF UseCriticalSection}
 finally
  FCriticalSection.Leave;
 end;
 {$ENDIF}

 if EditorForm is TFmOversampler
  then TFmOversampler(EditorForm).UpdateOSFactor;
end;

procedure TOversampleTemplateDataModule.SetManualIdle(const Value: Boolean);
begin
 if FManualIdle <> Value then
  begin
   FManualIdle := Value;
   ManualIdleChanged;
  end;
end;

procedure TOversampleTemplateDataModule.ManualIdleChanged;
begin
 Timer.Enabled := ManualIdle;
end;

procedure TOversampleTemplateDataModule.ParameterOversamplingDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Boolean(Round(Parameter[Index]))
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TOversampleTemplateDataModule.SetOSFactor(const NewOSFactor: Integer);
var
  ch : Integer;
begin
 FOSFactor := NewOSFactor;
 for ch := 0 to Length(FDownsampler) - 1
  do FDownsampler[ch].Factor := FOSFactor;
 for ch := 0 to Length(FUpsampler) - 1
  do FUpsampler[ch].Factor := FOSFactor;
 TempBufferSize := FMaximumBlockSize * FOSFactor;
 PluginSampleRateChanged;
end;

procedure TOversampleTemplateDataModule.SetTempBufferSize(const Value: Integer);
var
  i : Integer;
begin
 if (FTempBufferSize <> Value) and
   ((Length(FIn64) > 0) or (Length(FOut64) > 0)) then
  begin
   FTempBufferSize := Value;
   {$IFDEF DELPHI10}
   SetMinimumBlockAlignment(mba16Byte);
   {$ENDIF}

   for i := 0 to numInputs - 1 do
    begin
     ReallocMem(FIn64[i], FTempBufferSize * SizeOf(Double));
     FIn32[i] := PDAVSingleFixedArray(FIn64[i]);
     ReallocMem(FOut64[i], FTempBufferSize * SizeOf(Double));
     FOut32[i] := PDAVSingleFixedArray(FOut64[i]);
    end;
  end;
end;

procedure TOversampleTemplateDataModule.VSTModuleProcessEvents(Sender: TObject;
  const Events: TVstEvents);
begin
 if VstHost[0].Active then VstHost[0].ProcessEvents(Events);
end;

procedure TOversampleTemplateDataModule.CheckSampleFrames(const SampleFrames: Integer);
begin
 if SampleFrames > FMaximumBlockSize then
  begin
   FMaximumBlockSize := SampleFrames;
   if TempBufferSize < FMaximumBlockSize * FOSFactor
    then TempBufferSize := FMaximumBlockSize * FOSFactor;
  end;
end;

procedure DontRaiseExceptionsAndSetFPUcodeword;
const
  SCRound8087CW : Word = $133F; // round FPU codeword, with exceptions disabled
asm
 fnclex                  // Don't raise pending exceptions enabled by the new flags
 fldcw   SCRound8087CW   // SCRound8087CW: Word = $133F; round FPU codeword, with exceptions disabled
end;

procedure TOversampleTemplateDataModule.VSTModuleProcess32OversampleSingle(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel, Sample : Integer;
begin
 {$IFDEF UseCriticalSection}
 FCriticalSection.Enter;
 try
 {$ENDIF}
   CheckSampleFrames(SampleFrames);

   if FOSActive then
    begin
     // upsample
     for Channel := 0 to numInputs - 1 do
      for Sample := 0 to SampleFrames - 1
       do FUpsampler[Channel].Upsample32(Inputs[Channel, Sample], @FIn32[Channel, Sample * FOSFactor]);

  (*
     for Channel := 0 to numInputs - 1 do
      for Sample := 0 to SampleFrames * FOSFactor - 1
       do Assert(not IsNaN(FIn32[Channel, Sample]));

     DontRaiseExceptionsAndSetFPUcodeword;
  *)
     // process serial chain
     if VstHost[0].Active
      then VstHost[0].Process32Replacing(@FIn32[0], @FOut32[0], SampleFrames * FOSFactor)
      else
       for Channel := 0 to min(numInputs, numOutputs) - 1
        do Move(FIn32[Channel, 0], FOut32[Channel, 0], SampleFrames * SizeOf(Single) * FOSFactor);

  (*
     for Channel := 0 to numInputs - 1 do
      for Sample := 0 to SampleFrames * FOSFactor - 1
       do Assert(not IsNaN(FOut32[Channel, Sample]));
  *)

     // downsample
     for Channel := 0 to numOutputs - 1 do
      for Sample := 0 to SampleFrames - 1
       do Outputs[Channel, Sample] := FDownsampler[Channel].Downsample32(@FOut32[Channel, Sample * FOSFactor]);
    end
   else
    begin
     if VstHost[0].Active
      then VstHost[0].Process32Replacing(@Inputs[0], @Outputs[0], SampleFrames * FOSFactor)
      else
       for Channel := 0 to min(numInputs, numOutputs) - 1
        do Move(Inputs[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Single) * FOSFactor);
    end;
 {$IFDEF UseCriticalSection}
 finally
  FCriticalSection.Leave;
 end;
 {$ENDIF}
end;

procedure TOversampleTemplateDataModule.VSTModuleProcess64OversampleSingle(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel, Sample  : Integer;
begin
 {$IFDEF UseCriticalSection}
 FCriticalSection.Enter;
 try
 {$ENDIF}
   CheckSampleFrames(SampleFrames);

   if FOSActive then
    begin
     // upsample
     for Channel := 0 to numInputs - 1 do
      for Sample := 0 to SampleFrames - 1
       do FUpsampler[Channel].Upsample64(Inputs[Channel, Sample], @FIn64[Channel, Sample * FOSFactor]);

     // process serial chain
     if VstHost[0].Active
      then VstHost[0].Process64Replacing(@FIn64[0], @FOut64[0], SampleFrames * FOSFactor)
      else
       for Channel := 0 to min(numInputs, numOutputs) - 1
        do Move(FIn64[Channel, 0], FOut64[Channel, 0], SampleFrames * SizeOf(Single) * FOSFactor);

     // downsample
     for Channel := 0 to numOutputs - 1 do
      for Sample := 0 to SampleFrames - 1
       do Outputs[Channel, Sample] := FDownsampler[Channel].Downsample64(@FOut64[Channel, Sample * FOSFactor]);
    end
   else
    if VstHost[0].Active
     then VstHost[0].Process32Replacing(@Inputs[0], @Outputs[0], SampleFrames * FOSFactor)
     else
      for Channel := 0 to min(numInputs, numOutputs) - 1
       do Move(Inputs[Channel], Outputs[Channel, 0], SampleFrames * SizeOf(Double) * FOSFactor);
 {$IFDEF UseCriticalSection}
 finally
  FCriticalSection.Leave;
 end;
 {$ENDIF}
end;

end.
