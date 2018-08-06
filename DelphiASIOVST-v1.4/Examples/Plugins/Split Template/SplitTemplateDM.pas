unit SplitTemplateDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Graphics, Forms, Themes, DAV_Types,
  DAV_VSTModule, DAV_VSTEffect, DAV_VSTParameters, DAV_VSTModuleWithPrograms,
  DAV_VSTCustomModule, DAV_DspFilterButterworth, DAV_DspUpDownsampling,
  DAV_VstHost, DAV_DspLFO;

type
  TLowPassArray = array [0..1] of TButterworthLowPassFilter;
  THighPassArray = array [0..1] of TButterworthHighPassFilter;
  TUpDownsampling = array [0..1] of TDAVUpDownsampling;

  TSplitType = (stSimple, stLiRi, stDyn, stLeftRight, stMS, stSerial,
    stTransient, stExciter, stLFO, stSpin, stDynLFO, stSingle, stBypass);
  TSplitTemplateDataModule = class(TVSTModule)
    VstHost: TVstHost;
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    function VSTModuleVendorSpecific(Sender: TObject; const lArg1, lArg2: Integer; const ptrArg: Pointer; const floatArg: Single): Integer;
    procedure VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
    procedure VSTModuleEditClose(Sender: TObject; var DestroyForm: Boolean);
    procedure VSTModuleEditIdle(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleEditSleep(Sender: TObject);
    procedure VSTModuleEditTop(Sender: TObject);
    procedure VSTModuleGetVU(var VU: Single);
    procedure VSTModuleOfflineNotify(Sender: TObject; const AudioFile: TVstAudioFile; const numAudioFiles: Integer; const start: Boolean);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);

    procedure VSTModuleProcess32SplitVST(const SampleFrames: Integer);
    procedure VSTModuleProcess64SplitVST(const SampleFrames: Integer);

    // 32 bit stuff
    procedure VSTModuleProcess32SplitFrequencySimple(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32SplitFrequencyLiRi(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32SplitExciter(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32SplitDynamic(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32SplitLeftRight(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32SplitMidSide(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32Serial(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32Bypass(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32SplitTransient(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32SplitSingle(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32SplitDynLFO(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32SplitLFO(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32SplitSpin(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);

    // 64 bit stuff
    procedure VSTModuleProcess64Serial(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64SplitDynamic(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64SplitFrequencyLiRi(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64SplitFrequencySimple(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64SplitExciter(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64SplitLeftRight(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64SplitMidSide(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64Bypass(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64SplitTransient(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64SplitSingle(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64SplitLFO(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64SplitDynLFO(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64SplitSpin(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);

    procedure VSTModuleProcessEvents(Sender: TObject; const Events: TVstEvents);
    procedure VSTModuleProcessVarIO(Sender: TObject; const varIo: TVstVariableIo);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleStartProcess(Sender: TObject);
    procedure VSTModuleStopProcess(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    function VSTModuleOutputProperties(Sender: TObject; const Index: Integer; var VstPinProperties: TVstPinProperties): Boolean;
    function VSTModuleInputProperties(Sender: TObject; const Index: Integer; var VstPinProperties: TVstPinProperties): Boolean;

    // Parameters
    procedure CustomParameterDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure CustomParameterLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamVolumeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFreqDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamFreqLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamOrderLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamModeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamOSFactorChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOSFactorDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamOversamplingChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOversamplingDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterLowAutomate(Sender: TObject; Index: Integer; ParameterValue: Single);
    procedure ParameterHighAutomate(Sender: TObject; Index: Integer; ParameterValue: Single);
  private
    FLowpass          : array of TLowPassArray;
    FHighpass         : array of THighPassArray;
    FOversampler      : array of TUpDownsampling;
    FLow64, FHigh64   : array of PDAVDoubleFixedArray;
    FTmpOutput64      : array of PDAVDoubleFixedArray;
    FLow32, FHigh32   : array of PDAVSingleFixedArray;
    FTmpOutput32      : array of PDAVSingleFixedArray;
    FEnvelope         : array of array [0..1] of Single;
    FAttackFactor     : array [0..1] of Single;
    FReleaseFactor    : array [0..1] of Single;
    FLiRiSign         : Single;
    FMaxChannels      : Integer;
    FMinChannels      : Integer;
    FSplitType        : TSplitType;
    FOSActive         : Boolean;
    FOSFactor         : Integer;
    FMaximumBlockSize : Integer;
    FTempBufferSize   : Integer;
    FSineLFO          : TLFOSine;
    FVolumeFactor     : Double;
    FPlugNr           : Integer;
    FDifferentPlugins : Boolean;
    procedure SetOSFactor(const NewOSFactor: Integer);
    procedure SetTempBufferSize(const Value: Integer);
    procedure VSTBuffersChanged;
    procedure PluginSampleRateChanged;
    procedure CheckSampleFrames(const SampleFrames: Integer);
  published
    property SplitType: TSplitType read FSplitType;
    property PluginVisible: Integer read FPlugNr write FPlugNr default 0;
    property TempBufferSize: Integer read FTempBufferSize write SetTempBufferSize default 0;
  end;

function ConvertOrderToString(Order: Integer): string;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, Dialogs, Controls, Types, DAV_Common, DAV_VSTPrograms,
  SplitTemplateGUI;

function EnumNamesFunc(hModule:THandle; lpType, lpName:PChar; lParam: DWORD): Boolean; stdcall;
begin
  Result := True;
  TStringList(lParam).Add(lpName);
end;

function EnumRCDATANamesFunc(hModule:THandle; lpType, lpName:PChar; lParam: DWORD): Boolean; stdcall;
begin
  Result := True;
  ShowMessage(lpName);
end;

function EnumTypesFunc(hModule:THandle; lpType: PChar; lParam: DWORD): Boolean; stdcall;
begin
  Result := True;
  ShowMessage(IntToStr(Integer(lpType)));
//  ShowMessage(lpType);
end;

procedure TSplitTemplateDataModule.VSTModuleCreate(Sender: TObject);
var
  RN       : TStringList;
  RS       : TResourceStream;
  PI       : TCustomVstPlugIn;
  Channel, i, n : Integer;
begin
 RN := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc, DWord(RN));
  FDifferentPlugins := RN.Count > 1;

  if RN.Count > 0 then
   begin
    for n := 0 to 1 do
     begin
      PI := VstHost.VstPlugIns[n];

      // load plugin from resource
      RS := TResourceStream.Create(hInstance, RN[n mod RN.Count], 'DLL');
      try
       PI.LoadFromStream(RS);
      finally
       FreeAndNil(RS);
      end;

      PI.Active := True;
      for i := 0 to VstHost[n].numParams - 1 do
       with ParameterProperties.Add do
        begin
         OnParameterChange        := VSTModuleParameterChange;
         OnCustomParameterLabel   := CustomParameterLabel;
         OnCustomParameterDisplay := CustomParameterDisplay;
         DisplayName              := string(VstHost[n].GetParamName(i));
        end;
      if PI.numPrograms > 0
       then PI.CurrentProgram := 0;
     end;
    UniqueID    := AnsiChar('S') + VstHost[0].UniqueID[1] + VstHost[0].UniqueID[2] + AnsiChar('2');
    EffectName  := 'Splitted ' + VstHost[0].EffectName;
    ProductName := 'Splitted ' + VstHost[0].ProductString;
    VendorName  := 'Delphi ASIO & VST Packages and ' + VstHost[0].VendorString;

(*
    // program replication
    for i := 0 to VstHost[0].numPrograms - 1 do
     with Programs.Add do
      begin
       VstHost[0].GetProgramNameIndexed(0, i, str);
       VstHost[0].SetProgram(i);
       DisplayName := str;
      end;
*)

    // enable 64bit processing if supported by both plugins
    if (effFlagsCanDoubleReplacing in VstHost[0].EffectOptions) and
       (effFlagsCanDoubleReplacing in VstHost[1].EffectOptions)
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
    if VstHost[0].numInputs = VstHost[1].numInputs then
     begin
      numInputs  := VstHost[0].numInputs;
      numOutputs := max(2, VstHost[0].numOutputs);
      if numInputs = numOutputs then
       case numInputs of
        1 : CanDos := CanDos - [vcd2in2out] + [vcd1in1out];
        2 : CanDos := CanDos - [vcd1in1out] + [vcd2in2out];
       end;
     end
    else
     begin
      numInputs  := max(VstHost[0].numInputs, VstHost[1].numInputs);
      numOutputs := max(2, max(VstHost[0].numOutputs, VstHost[1].numOutputs));
     end;
    if VstHost[0].PlugCategory = VstHost[1].PlugCategory
     then PlugCategory := VstHost[0].PlugCategory;
   end;
 finally
  FreeAndNil(RN);
 end;

 if numInputs > numOutputs then
  begin
   FMaxChannels := numInputs;
   FMinChannels := numOutputs;
  end
 else
  begin
   FMaxChannels := numOutputs;
   FMinChannels := numInputs;
  end;

 SetLength(FLowpass, numInputs);
 SetLength(FHighpass, numInputs);
 SetLength(FOversampler, numInputs);
 for Channel := 0 to numInputs - 1 do
  for n := 0 to 1 do
   begin
    FLowpass[Channel, n]     := TButterworthLowPassFilter.Create;
    FHighpass[Channel, n]    := TButterworthHighPassFilter.Create;
   end;

 for Channel := 0 to FMaxChannels - 1 do
  for n := 0 to 1
   do FOversampler[Channel, n] := TDAVUpDownsampling.Create;

 FOSFactor     := 1;
 FOSActive     := False;
 FVolumeFactor := 1;

 OnProcess := VSTModuleProcess32SplitFrequencySimple;
 OnProcess32Replacing := VSTModuleProcess32SplitFrequencySimple;
 OnProcess64Replacing := VSTModuleProcess64SplitFrequencySimple;
 FTempBufferSize := 0;
 FMaximumBlockSize := VstHost.BlockSize;
 FPlugNr := 0;
 SetLength(FEnvelope, FMaxChannels);
 SetLength(FLow32, FMaxChannels);
 SetLength(FLow64, FMaxChannels);
 SetLength(FHigh32, FMaxChannels);
 SetLength(FHigh64, FMaxChannels);
 SetLength(FTmpOutput32, numOutputs);
 SetLength(FTmpOutput64, numOutputs);

 FAttackFactor[0]  := 0.01;
 FAttackFactor[1]  := 0.001;
 FReleaseFactor[0] := 0.9999;
 FReleaseFactor[1] := 0.9999;

 FSineLFO := TLFOSine.Create;
 FSineLFO.Frequency := 1;
 FSineLFO.Amplitude := 1;

 VSTBuffersChanged;
end;

procedure TSplitTemplateDataModule.VSTModuleDestroy(Sender: TObject);
var
  Channel, n : Integer;
begin
 for Channel := 0 to FMaxChannels - 1 do Dispose(FLow64[Channel]);
 for Channel := 0 to FMaxChannels - 1 do Dispose(FHigh64[Channel]);
 for Channel := 0 to numOutputs - 1   do Dispose(FTmpOutput64[Channel]);

 for Channel := 0 to numInputs - 1 do
  for n := 0 to 1 do
   begin
    FreeAndNil(FLowpass[Channel, n]);
    FreeAndNil(FHighpass[Channel, n]);
   end;
 for Channel := 0 to FMaxChannels - 1 do
  for n := 0 to 1
   do FreeAndNil(FOversampler[Channel, n]);

 FreeAndNil(FSineLFO);

 VSTHost.VstPlugIns.Clear;
end;

procedure TSplitTemplateDataModule.VSTModuleEditClose(Sender: TObject;
  var DestroyForm: Boolean);
begin
 with VstHost[0] do if EditVisible then CloseEdit;
 with VstHost[1] do if EditVisible then CloseEdit;
end;

procedure TSplitTemplateDataModule.VSTModuleEditIdle(Sender: TObject);
begin
 with VstHost[0] do if EditVisible then EditIdle;
 with VstHost[1] do if EditVisible then EditIdle;
end;

procedure TSplitTemplateDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
var
  Rct      : array [0..1] of TRect;
  Oversize : Integer;
begin
 GUI := TFmSplitter.Create(Self);

 // set plugin GUI size
 if Assigned(VstHost[0]) and VstHost[0].Active then
  with TFmSplitter(GUI) do
   begin
    PnGui.Visible    := True;
    ShBorder.Visible := True;

    if not VstHost[0].EditVisible
     then VstHost[0].ShowEdit(TForm(PnGui));
    Rct[0]   := VstHost[0].GetRect;
    if FDifferentPlugins then
     if Assigned(VstHost[1]) and VstHost[1].Active then
      begin
       Rct[1] := VstHost[1].GetRect;
       if Rct[1].Right - Rct[1].Left > Rct[0].Right - Rct[0].Left then
        begin
         Rct[0].Right := Rct[1].Right;
         Rct[0].Left  := Rct[1].Left;
        end;
       if Rct[1].Bottom - Rct[1].Top > Rct[0].Bottom - Rct[0].Top then
        begin
         Rct[0].Bottom := Rct[1].Bottom;
         Rct[0].Top    := Rct[1].Top;
        end;
      end;
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
  with TFmSplitter(GUI) do
   begin
    PnGui.Visible    := False;
    ShBorder.Visible := False;
   end;
end;

procedure TSplitTemplateDataModule.VSTModuleEditSleep(Sender: TObject);
begin
 with VstHost[0] do if EditVisible then EditDeactivate;
 with VstHost[1] do if EditVisible then EditDeactivate;
end;

procedure TSplitTemplateDataModule.VSTModuleEditTop(Sender: TObject);
begin
 with VstHost[0] do if EditVisible then EditActivate;
 with VstHost[1] do if EditVisible then EditActivate;
end;

procedure TSplitTemplateDataModule.VSTModuleGetVU(var VU: Single);
begin
 if VstHost[0].Active then VU := VstHost[0].GetVu;
 if VstHost[1].Active then
  if VstHost[1].GetVu > VU then VU := VstHost[1].GetVu;
end;

function TSplitTemplateDataModule.VSTModuleInputProperties(Sender: TObject;
  const Index: Integer; var VstPinProperties: TVstPinProperties): Boolean;
begin
 if VstHost[0].Active
  then VstPinProperties := VstHost[0].GetInputProperties(Index)
  else
 if VstHost[1].Active
  then VstPinProperties := VstHost[0].GetInputProperties(Index);
 Result := False;
end;

procedure TSplitTemplateDataModule.VSTModuleOfflineNotify(Sender: TObject;
  const AudioFile: TVstAudioFile; const numAudioFiles: Integer; const start: Boolean);
var
  AF: TVstAudioFile;
begin
 AF := AudioFile;
 if VstHost[0].Active then VstHost[0].OfflineNotify(AF, numAudioFiles, start);
 if VstHost[1].Active then VstHost[1].OfflineNotify(AF, numAudioFiles, start);
end;

procedure TSplitTemplateDataModule.VSTModuleOpen(Sender: TObject);
begin
 Parameter[0] := 0;
 Parameter[1] := 2000;
 Parameter[2] := 4;
 Parameter[3] := 1;
 Parameter[4] := 0;
 Parameter[5] := 2;

 with Programs[0] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 2000;
   Parameter[2] := 4;
   Parameter[3] := 0;
   Parameter[4] := 0;
   Parameter[5] := 2;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 3000;
   Parameter[2] := 4;
   Parameter[3] := 0;
   Parameter[4] := 1;
   Parameter[5] := 2;
  end;
 with Programs[2] do
  begin
   Parameter[0] := 1;
   Parameter[1] := 1300;
   Parameter[2] := 4;
   Parameter[3] := 0;
   Parameter[4] := 0;
   Parameter[5] := 2;
  end;
 with Programs[3] do
  begin
   Parameter[0] := 2;
   Parameter[1] := 1300;
   Parameter[2] := 4;
   Parameter[3] := 0;
   Parameter[4] := 0;
   Parameter[5] := 2;
  end;
 with Programs[4] do
  begin
   Parameter[0] := 3;
   Parameter[1] := 200;
   Parameter[2] := 4;
   Parameter[3] := 0;
   Parameter[4] := 0;
   Parameter[5] := 2;
  end;
 with Programs[5] do
  begin
   Parameter[0] := 4;
   Parameter[1] := 400;
   Parameter[2] := 4;
   Parameter[3] := 0;
   Parameter[4] := 0;
   Parameter[5] := 2;
  end;
 with Programs[6] do
  begin
   Parameter[0] := 5;
   Parameter[1] := 800;
   Parameter[2] := 4;
   Parameter[3] := 0;
   Parameter[4] := 0;
   Parameter[5] := 2;
  end;
end;

function TSplitTemplateDataModule.VSTModuleOutputProperties(Sender: TObject;
  const Index: Integer; var VstPinProperties: TVstPinProperties): Boolean;
begin
 if VstHost[0].Active
  then VstPinProperties := VstHost[0].GetOutputProperties(Index)
  else
 if VstHost[1].Active
  then VstPinProperties := VstHost[0].GetOutputProperties(Index);
 Result := False;
end;

procedure TSplitTemplateDataModule.VSTModuleClose(Sender: TObject);
begin
 VstHost[0].Active := False;
 VstHost[1].Active := False;
end;

procedure TSplitTemplateDataModule.VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
begin
 VSTBuffersChanged;
end;

procedure TSplitTemplateDataModule.VSTBuffersChanged;
begin
 VstHost.BlockSize := BlockSize * FOSFactor;
 with VstHost[0] do if Active then SetBlockSizeAndSampleRate(BlockSize * FOSFactor, SampleRate * FOSFactor);
 with VstHost[1] do if Active then SetBlockSizeAndSampleRate(BlockSize * FOSFactor, SampleRate * FOSFactor);
 FMaximumBlockSize := BlockSize;
 TempBufferSize := FMaximumBlockSize * FOSFactor;
end;

procedure TSplitTemplateDataModule.ParameterLowAutomate(
  Sender: TObject; Index: Integer; ParameterValue: Single);
begin
 Parameter[6 + Index] := ParameterValue;
end;

procedure TSplitTemplateDataModule.ParameterHighAutomate(
  Sender: TObject; Index: Integer; ParameterValue: Single);
begin
 Parameter[6 + VstHost[0].numParams + Index] := ParameterValue;
end;

procedure TSplitTemplateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLowpass) - 1 do
  begin
   if Assigned(FLowpass[Channel, 0]) then FLowpass[Channel, 0].SampleRate := SampleRate;
   if Assigned(FLowpass[Channel, 1]) then FLowpass[Channel, 1].SampleRate := SampleRate;
  end;
 for Channel := 0 to Length(FHighpass) - 1 do
  begin
   if Assigned(FHighpass[Channel, 0]) then FHighpass[Channel, 0].SampleRate := SampleRate;
   if Assigned(FHighpass[Channel, 1]) then FHighpass[Channel, 1].SampleRate := SampleRate;
  end;
 for Channel := 0 to Length(FOversampler) - 1 do
  begin
   if Assigned(FOversampler[Channel, 0]) then FOversampler[Channel, 0].SampleRate := SampleRate;
   if Assigned(FOversampler[Channel, 0]) then FOversampler[Channel, 1].SampleRate := SampleRate;
  end;
 if Assigned(FSineLFO)
  then FSineLFO.SampleRate := SampleRate;
 PluginSampleRateChanged;
end;

procedure TSplitTemplateDataModule.PluginSampleRateChanged;
begin
 with VstHost[0] do if Active then SetSampleRate(FOSFactor * SampleRate);
 with VstHost[1] do if Active then SetSampleRate(FOSFactor * SampleRate);
end;

procedure TSplitTemplateDataModule.VSTModuleStartProcess(Sender: TObject);
begin
 with VstHost[0] do if Active then StartProcess;
 with VstHost[1] do if Active then StartProcess;
end;

procedure TSplitTemplateDataModule.VSTModuleStopProcess(Sender: TObject);
begin
 with VstHost[0] do if Active then StopProcess;
 with VstHost[1] do if Active then StopProcess;
end;

procedure TSplitTemplateDataModule.VSTModuleSuspend(Sender: TObject);
begin
 with VstHost[0] do if Active then MainsChanged(False);
 with VstHost[1] do if Active then MainsChanged(False);
end;

function TSplitTemplateDataModule.VSTModuleVendorSpecific(Sender: TObject;
  const lArg1, lArg2: Integer; const ptrArg: Pointer; const floatArg: Single): Integer;
begin
 Result := 0;
 with VstHost[0] do if Active then Result := VendorSpecific(lArg1, lArg2, ptrArg, floatArg);
 with VstHost[1] do if Active then Result := VendorSpecific(lArg1, lArg2, ptrArg, floatArg);
end;

procedure TSplitTemplateDataModule.VSTModuleProcessVarIO(Sender: TObject;
  const varIo: TVstVariableIo);
var
  vio : TVstVariableIo;
begin
 vio := varIo;
 with VstHost[0] do if Active then ProcessVarIo(vio);
 with VstHost[1] do if Active then ProcessVarIo(vio);
end;

procedure TSplitTemplateDataModule.VSTModuleResume(Sender: TObject);
begin
 with VstHost[0] do if Active then MainsChanged(True);
 with VstHost[1] do if Active then MainsChanged(True);
end;

procedure TSplitTemplateDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := 6;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
   if n >= VstHost.Count then break;
  end;
 if (n < VstHost.Count) and VstHost[n].Active
  then VstHost[n].Parameter[Index - pnr] := Value;
end;

procedure TSplitTemplateDataModule.CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := 6;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
   if n >= VstHost.Count then break;
  end;
 if (n < VstHost.Count) and VstHost[n].Active
  then PreDefined := VstHost[n].GetParamDisplay(Index - pnr);
end;

procedure TSplitTemplateDataModule.CustomParameterLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := 6;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
   if n >= VstHost.Count then break;
  end;
 if (n < VstHost.Count) and VstHost[n].Active
  then PreDefined := VstHost[n].GetParamLabel(Index - pnr);
end;

procedure TSplitTemplateDataModule.ParamOSFactorDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[Index])) + 'x');
end;

function ConvertOrderToString(Order: Integer): string;
begin
 case Order of
   1 : Result := IntToStr(Order) + 'st';
   2 : Result := IntToStr(Order) + 'nd';
   3 : Result := IntToStr(Order) + 'rd';
  else Result := IntToStr(Order) + 'th';
 end;
end;

procedure TSplitTemplateDataModule.ParamOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case FSplitType of
  stLeftRight, stMS, stSerial,
   stBypass  : PreDefined := '';
     stLiRi  : PreDefined := AnsiString(ConvertOrderToString(2 * Round(Parameter[Index])));
      stDyn,
   stSimple  : PreDefined := AnsiString(ConvertOrderToString(Round(Parameter[Index])));
 end;
end;

procedure TSplitTemplateDataModule.ParamOversamplingDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Boolean(Round(Parameter[Index]))
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TSplitTemplateDataModule.ParamOversamplingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FOSActive := Boolean(Round(Value));
 if FOSActive = True
  then SetOSFactor(Round(ParameterByName['OS Factor']))
  else SetOSFactor(1);
 if EditorForm is TFmSplitter
  then TFmSplitter(EditorForm).UpdateOverSampling;
end;

procedure TSplitTemplateDataModule.ParamOSFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if FOSActive = True
  then SetOSFactor(Round(Value))
  else SetOSFactor(1);

 if EditorForm is TFmSplitter
  then TFmSplitter(EditorForm).UpdateOSFactor;
end;

procedure TSplitTemplateDataModule.SetOSFactor(const NewOSFactor: Integer);
var
  Channel, n : Integer;
begin
 FOSFactor := NewOSFactor;
 for Channel := 0 to FMaxChannels - 1 do
  for n := 0 to 1 do
   if Assigned(FOversampler[Channel, n])
    then FOversampler[Channel, n].Factor := FOSFactor;
 TempBufferSize := FMaximumBlockSize * FOSFactor;
 PluginSampleRateChanged;
end;

procedure TSplitTemplateDataModule.SetTempBufferSize(
  const Value: Integer);
var
  i : Integer;
begin
 if FTempBufferSize <> Value then
  begin
   FTempBufferSize := Value;
   for i := 0 to numInputs - 1 do
    begin
     {$IFDEF DELPHI10}
     SetMinimumBlockAlignment(mba16Byte);
     {$ENDIF}
     ReallocMem(FLow64[i], FTempBufferSize * SizeOf(Double));
     FLow32[i] := PDAVSingleFixedArray(FLow64[i]);
     ReallocMem(FHigh64[i], FTempBufferSize * SizeOf(Double));
     FHigh32[i] := PDAVSingleFixedArray(FHigh64[i]);
     ReallocMem(FTmpOutput64[i], FTempBufferSize * SizeOf(Double));
     FTmpOutput32[i] := PDAVSingleFixedArray(FTmpOutput64[i]);
    end;
  end;
end;

procedure TSplitTemplateDataModule.ParamOrderLabel(Sender: TObject;
  const Index: Integer; var PreDefined: AnsiString);
begin
 case FSplitType of
  stLeftRight, stMS, stSerial, stBypass : PreDefined := '';
 end;
end;

procedure TSplitTemplateDataModule.ParamFreqLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case FSplitType of
  stLeftRight, stMS, stSerial, stBypass : PreDefined := '';
 end;
end;

procedure TSplitTemplateDataModule.ParamFreqDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case FSplitType of
  stLeftRight, stMS, stSerial, stBypass : PreDefined := '-';
 end;
end;

procedure TSplitTemplateDataModule.ParamVolumeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FVolumeFactor := dB_to_Amp(Value);
 FSineLFO.Amplitude := FVolumeFactor;
end;

procedure TSplitTemplateDataModule.ParamOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel, n : Integer;
begin
 FLiRiSign := 1 - 2 * (Round(Value) mod 2);
 for Channel := 0 to numInputs - 1 do
  for n := 0 to 1 do
   begin
    if Assigned(FLowpass[Channel, n]) then FLowpass[Channel, n].Order  := Round(Value);
    if Assigned(FHighpass[Channel, n]) then FHighpass[Channel, n].Order := Round(Value);
   end;
 if EditorForm is TFmSplitter
  then TFmSplitter(EditorForm).UpdateOrder;
end;

procedure TSplitTemplateDataModule.ParamFreqChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel, n : Integer;
begin
 for Channel := 0 to numInputs - 1 do
  for n := 0 to 1 do
   begin
    if Assigned(FLowpass[Channel, n]) then FLowpass[Channel, n].Frequency  := Value;
    if Assigned(FHighpass[Channel, n]) then FHighpass[Channel, n].Frequency := Value;
   end;
 if EditorForm is TFmSplitter
  then TFmSplitter(EditorForm).UpdateFrequency;
end;

procedure TSplitTemplateDataModule.ParamModeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FSplitType := TSplitType(Round(Value));
 case FSplitType of
     stSimple : begin
                 OnProcess := VSTModuleProcess32SplitFrequencySimple;
                 OnProcess32Replacing := VSTModuleProcess32SplitFrequencySimple;
                 OnProcess64Replacing := VSTModuleProcess64SplitFrequencySimple;
                end;
       stLiRi : begin
                 OnProcess := VSTModuleProcess32SplitFrequencyLiRi;
                 OnProcess32Replacing := VSTModuleProcess32SplitFrequencyLiRi;
                 OnProcess64Replacing := VSTModuleProcess64SplitFrequencyLiRi;
                end;
        stDyn : begin
                 OnProcess := VSTModuleProcess32SplitDynamic;
                 OnProcess32Replacing := VSTModuleProcess32SplitDynamic;
                 OnProcess64Replacing := VSTModuleProcess64SplitDynamic;
                end;
  stLeftRight : begin
                 OnProcess := VSTModuleProcess32SplitLeftRight;
                 OnProcess32Replacing := VSTModuleProcess32SplitLeftRight;
                 OnProcess64Replacing := VSTModuleProcess64SplitLeftRight;
                end;
         stMS : begin
                 OnProcess := VSTModuleProcess32SplitMidSide;
                 OnProcess32Replacing := VSTModuleProcess32SplitMidSide;
                 OnProcess64Replacing := VSTModuleProcess64SplitMidSide;
                end;
     stSerial : begin
                 OnProcess := VSTModuleProcess32Serial;
                 OnProcess32Replacing := VSTModuleProcess32Serial;
                 OnProcess64Replacing := VSTModuleProcess64Serial;
                end;
  stTransient : begin
                 OnProcess := VSTModuleProcess32SplitTransient;
                 OnProcess32Replacing := VSTModuleProcess32SplitTransient;
                 OnProcess64Replacing := VSTModuleProcess64SplitTransient;
                end;
    stExciter : begin
                 OnProcess := VSTModuleProcess32SplitExciter;
                 OnProcess32Replacing := VSTModuleProcess32SplitExciter;
                 OnProcess64Replacing := VSTModuleProcess64SplitExciter;
                end;
        stLFO : begin
                 OnProcess := VSTModuleProcess32SplitLFO;
                 OnProcess32Replacing := VSTModuleProcess32SplitLFO;
                 OnProcess64Replacing := VSTModuleProcess64SplitLFO;
                end;
     stDynLFO : begin
                 OnProcess := VSTModuleProcess32SplitDynLFO;
                 OnProcess32Replacing := VSTModuleProcess32SplitDynLFO;
                 OnProcess64Replacing := VSTModuleProcess64SplitDynLFO;
                end;
       stSpin : begin
                 OnProcess := VSTModuleProcess32SplitSpin;
                 OnProcess32Replacing := VSTModuleProcess32SplitSpin;
                 OnProcess64Replacing := VSTModuleProcess64SplitSpin;
                end;
     stSingle : begin
                 OnProcess := VSTModuleProcess32SplitSingle;
                 OnProcess32Replacing := VSTModuleProcess32SplitSingle;
                 OnProcess64Replacing := VSTModuleProcess64SplitSingle;
                end;
     stBypass : begin
                 OnProcess := VSTModuleProcess32Bypass;
                 OnProcess32Replacing := VSTModuleProcess32Bypass;
                 OnProcess64Replacing := VSTModuleProcess64Bypass;
                end;
 end;

 if EditorForm is TFmSplitter then
  with TFmSplitter(EditorForm) do
   begin
    UpdateMode;
    UpdateOrder;
   end;
end;

procedure TSplitTemplateDataModule.ParamModeDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
   0 : PreDefined := 'Split';
   1 : PreDefined := 'Linkwitz-Riley';
   2 : PreDefined := 'Dyn';
   3 : PreDefined := 'L/R';
   4 : PreDefined := 'M/S';
   5 : PreDefined := 'Serial';
   6 : PreDefined := 'Transient';
   7 : PreDefined := 'Exciter';
   8 : PreDefined := 'LFO';
   9 : PreDefined := 'DynLFO';
  10 : PreDefined := 'Spin';
  11 : PreDefined := 'Single';
  12 : PreDefined := 'Bypass';
 end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcessEvents(Sender: TObject;
  const Events: TVstEvents);
begin
 if VstHost[0].Active then VstHost[0].ProcessEvents(Events);
 if VstHost[1].Active then VstHost[1].ProcessEvents(Events);
end;

procedure TSplitTemplateDataModule.CheckSampleFrames(const SampleFrames: Integer);
begin
 if SampleFrames > FMaximumBlockSize then
  begin
   FMaximumBlockSize := SampleFrames;
   if TempBufferSize < FMaximumBlockSize * FOSFactor
    then TempBufferSize := FMaximumBlockSize * FOSFactor;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitVST(const SampleFrames: Integer);
var
  Channel : Integer;
begin
 if VstHost[0].Active then
  begin
   VstHost[0].Process32Replacing(@FLow32[0], @FTmpOutput32[0], SampleFrames * FOSFactor);
   for Channel := 0 to numOutputs - 1
    do Move(FTmpOutput32[Channel, 0], FLow32[Channel, 0], SampleFrames * SizeOf(Single) * FOSFactor);
  end;
 if VstHost[1].Active then
  begin
   VstHost[1].Process32Replacing(@FHigh32[0], @FTmpOutput32[0], SampleFrames * FOSFactor);
   for Channel := 0 to numOutputs - 1
    do Move(FTmpOutput32[Channel, 0], FHigh32[Channel, 0], SampleFrames * SizeOf(Single) * FOSFactor);
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitVST(const SampleFrames: Integer);
var
  Channel : Integer;
begin
 if VstHost[0].Active then
  begin
   VstHost[0].Process64Replacing(@FLow64[0], @FTmpOutput64[0], SampleFrames * FOSFactor);
   for Channel := 0 to numOutputs - 1
    do Move(FTmpOutput64[Channel, 0], FLow64[Channel, 0], SampleFrames * SizeOf(Double) * FOSFactor);
  end;
 if VstHost[1].Active then
  begin
   VstHost[1].Process64Replacing(@FHigh64[0], @FTmpOutput64[0], SampleFrames * FOSFactor);
   for Channel := 0 to numOutputs - 1
    do Move(FTmpOutput64[Channel, 0], FHigh64[Channel, 0], SampleFrames * SizeOf(Double) * FOSFactor);
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitFrequencySimple(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  L     : Double;
  Channel, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  for Channel := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     L := FLowpass[Channel, 0].ProcessSample32(Inputs[Channel, i]);
     FOversampler[Channel, 0].Upsample32(L, @FLow32[Channel, i * FOSFactor]);
     FOversampler[Channel, 1].Upsample32(Inputs[Channel, i] - L, @FHigh32[Channel, i * FOSFactor]);
    end
 else
  for Channel := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     FLow32[Channel, i]  := FLowpass[Channel, 0].ProcessSample32(Inputs[Channel, i]);
     FHigh32[Channel, i] := Inputs[Channel, i] - FLow32[Channel, i];
    end;

 VSTModuleProcess32SplitVST(SampleFrames);

 if FOSActive then
  for Channel := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     Outputs[Channel, i] := FVolumeFactor * (
        FOversampler[Channel, 0].DownSample32(@FLow32[Channel, i * FOSFactor]) +
        FOversampler[Channel, 1].DownSample32(@FHigh32[Channel, i * FOSFactor]));
    end
 else
  for Channel := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1
    do Outputs[Channel, i] := FVolumeFactor * (FLow32[Channel, i] + FHigh32[Channel, i]);
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitFrequencyLiRi(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  for Channel := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     FOversampler[Channel, 0].Upsample32(
       FLowpass[Channel, 1].ProcessSample32(
       FLowpass[Channel, 0].ProcessSample32(FLiRiSign * Inputs[Channel, i])), @FLow32[Channel, i * FOSFactor]);
     FOversampler[Channel, 1].Upsample32(
       FHighpass[Channel, 0].ProcessSample32(
       FHighpass[Channel, 1].ProcessSample32(Inputs[Channel, i])), @FHigh32[Channel, i * FOSFactor]);
    end
 else
  for Channel := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     FLow32[Channel, i]  :=
       FLowpass[Channel, 1].ProcessSample32(
       FLowpass[Channel, 0].ProcessSample32(FLiRiSign * Inputs[Channel, i]));;
     FHigh32[Channel, i] :=
       FHighpass[Channel, 0].ProcessSample32(
       FHighpass[Channel, 1].ProcessSample32(Inputs[Channel, i]));
    end;

 VSTModuleProcess32SplitVST(SampleFrames);

 if FOSActive then
  for Channel := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     Outputs[Channel, i] := FVolumeFactor *
       (FOversampler[Channel, 0].DownSample32(@FLow32[Channel, i * FOSFactor]) +
        FOversampler[Channel, 1].DownSample32(@FHigh32[Channel, i * FOSFactor]));
    end
 else
  for Channel := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1
    do Outputs[Channel, i] := FVolumeFactor * (FLow32[Channel, i] + FHigh32[Channel, i]);
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitExciter(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  for Channel := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     FOversampler[Channel, 0].Upsample32(
       FLowpass[Channel, 1].ProcessSample32(
       FLowpass[Channel, 0].ProcessSample32(FLiRiSign * Inputs[Channel, i])), @FLow32[Channel, i * FOSFactor]);
     FOversampler[Channel, 1].Upsample32(
       FHighpass[Channel, 0].ProcessSample32(
       FHighpass[Channel, 1].ProcessSample32(Inputs[Channel, i])), @FHigh32[Channel, i * FOSFactor]);
    end
 else
  for Channel := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     FLow32[Channel, i]  :=
       FLowpass[Channel, 1].ProcessSample32(
       FLowpass[Channel, 0].ProcessSample32(FLiRiSign * Inputs[Channel, i]));;
     FHigh32[Channel, i] :=
       FHighpass[Channel, 0].ProcessSample32(
       FHighpass[Channel, 1].ProcessSample32(Inputs[Channel, i]));
    end;

 VSTModuleProcess32SplitVST(SampleFrames);

 if FOSActive then
  for Channel := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     Outputs[Channel, i] := FVolumeFactor *
       (FOversampler[Channel, 0].DownSample32(@FLow32[Channel, i * FOSFactor]) +
        FOversampler[Channel, 1].DownSample32(@FHigh32[Channel, i * FOSFactor]));
    end
 else
  for Channel := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1
    do Outputs[Channel, i] := FVolumeFactor * (FLow32[Channel, i] + FHigh32[Channel, i]);
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitDynamic(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  L, H  : Double;
  Channel, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample32(Inputs[Channel, i], @FTmpOutput32[Channel, i * FOSFactor]);

   // process
   with VstHost[0] do
    if Active then Process32Replacing(@FTmpOutput32[0], @FLow32[0], SampleFrames * FOSFactor);
   with VstHost[1] do
    if Active then Process32Replacing(@FTmpOutput32[0], @FHigh32[0], SampleFrames * FOSFactor);

   for Channel := 0 to numOutputs - 1 do
    for i := 0 to SampleFrames - 1 do
     begin
      L := FOversampler[Channel, 0].DownSample32(@FLow32[Channel, i * FOSFactor]);
      H := FOversampler[Channel, 1].DownSample32(@FHigh32[Channel, i * FOSFactor]);
      FEnvelope[Channel, 0] := FLowpass[Channel, 0].ProcessSample32(Abs(Inputs[Channel, i]));
      Outputs[Channel, i]   := FVolumeFactor *
        (FEnvelope[Channel, 0] * H + (1 - FEnvelope[Channel, 0]) * L);
     end;
  end
 else
  begin
   // directly process both VSTs to the low/high buffers
   with VstHost[0] do
    if Active then Process32Replacing(@Inputs[0], @FLow32[0], SampleFrames);
   with VstHost[1] do
    if Active then Process32Replacing(@Inputs[0], @FHigh32[0], SampleFrames);

   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1 do
     begin
      FEnvelope[Channel, 0]  := FLowpass[Channel, 0].ProcessSample32(Abs(Inputs[Channel, i]));
      Outputs[Channel, i] := FVolumeFactor *
        (FEnvelope[Channel, 0] * FHigh32[Channel, i] + (1 - FEnvelope[Channel, 0]) * FLow32[Channel, i]);
     end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32Bypass(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel  : Integer;
begin
 for Channel := 0 to FMinChannels - 1
  do Move(Inputs[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Single));
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32Serial(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample32(Inputs[Channel, i], @FTmpOutput32[Channel, i * FOSFactor]);

   // process serial chain
   if VstHost[0].Active then
    begin
     VstHost[0].Process32Replacing(@FTmpOutput32[0], @FLow32[0], SampleFrames * FOSFactor);
     for Channel := 0 to numOutputs - 1
      do Move(FLow32[Channel, 0], FTmpOutput32[Channel, 0], SampleFrames * SizeOf(Single) * FOSFactor);
    end;
   if VstHost[1].Active then
    begin
     VstHost[1].Process32Replacing(@FTmpOutput32[0], @FLow32[0], SampleFrames * FOSFactor);
     for Channel := 0 to numOutputs - 1
      do Move(FLow32[Channel, 0], FTmpOutput32[Channel, 0], SampleFrames * SizeOf(Single) * FOSFactor);
    end;

   // downsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do Outputs[Channel, i] := FVolumeFactor * FOversampler[Channel, 0].Downsample32(@FTmpOutput32[Channel, i * FOSFactor]);
  end
 else
  begin
   if VstHost[0].Active then
    begin
     VstHost[0].Process32Replacing(@Inputs[0], @Outputs[0], SampleFrames * FOSFactor);

     // move output to input to prepare for the next stage
     if VstHost[1].Active then
      for Channel := 0 to FMinChannels - 1
       do Move(Outputs[Channel, 0], Inputs[Channel, 0], SampleFrames * SizeOf(Single) * FOSFactor);
    end;
   if VstHost[1].Active
    then VstHost[1].Process32Replacing(@Inputs[0], @Outputs[0], SampleFrames * FOSFactor);
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitLeftRight(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if numInputs = 1 then
  begin
   if FOSActive then
    begin
     for i := 0 to SampleFrames - 1
      do FOversampler[0, 0].Upsample32(Inputs[0, i], @FTmpOutput32[0, i * FOSFactor]);

     if VstHost[0].Active
      then VstHost[0].Process32Replacing(@FTmpOutput32[0], @FLow32[0], SampleFrames * FOSFactor);
     if VstHost[1].Active
      then VstHost[1].Process32Replacing(@FTmpOutput32[0], @FHigh32[0], SampleFrames * FOSFactor);

     for i := 0 to SampleFrames - 1 do
      begin
       Outputs[0, i] := FVolumeFactor * FOversampler[0, 0].DownSample32(@FLow32[0, i * FOSFactor]);
       Outputs[1, i] := FVolumeFactor * FOversampler[1, 0].DownSample32(@FHigh32[0, i * FOSFactor]);
      end
    end
   else
    begin
     if VstHost[0].Active
      then VstHost[0].Process32Replacing(@Inputs[0], @Outputs[0], SampleFrames);
     if VstHost[1].Active
      then VstHost[1].Process32Replacing(@Inputs[0], @Outputs[1], SampleFrames);
    end;
  end
 else
  begin
   if FOSActive then
    begin
     // upsample left channel
     for i := 0 to SampleFrames - 1
      do FOversampler[0, 0].Upsample32(Inputs[0, i], @FTmpOutput32[0, i * FOSFactor]);
     move(FTmpOutput32[0, 0], FTmpOutput32[1, 0], SampleFrames * FOSFactor * SizeOf(Single));

     if VstHost[0].Active
      then VstHost[0].Process32Replacing(@FTmpOutput32[0], @FLow32[0], SampleFrames * FOSFactor);

     // upsample right channel
     for i := 0 to SampleFrames - 1
      do FOversampler[1, 0].Upsample32(Inputs[1, i], @FTmpOutput32[0, i * FOSFactor]);
     move(FTmpOutput32[0, 0], FTmpOutput32[1, 0], SampleFrames * FOSFactor * SizeOf(Single));

     if VstHost[1].Active
      then VstHost[1].Process32Replacing(@FTmpOutput32[0], @FHigh32[0], SampleFrames * FOSFactor);

     for i := 0 to SampleFrames - 1 do
      begin
       Outputs[0, i] := FVolumeFactor * FOversampler[0, 0].DownSample32(@FLow32[0, i * FOSFactor]);
       Outputs[1, i] := FVolumeFactor * FOversampler[1, 0].DownSample32(@FHigh32[0, i * FOSFactor]);
      end
    end
   else
    begin
     // move left channel
     move(Inputs[0, 0], FTmpOutput32[0, 0], SampleFrames * SizeOf(Single));
     move(Inputs[0, 0], FTmpOutput32[1, 0], SampleFrames * SizeOf(Single));

     if VstHost[0].Active
      then VstHost[0].Process32Replacing(@FTmpOutput32[0], @FLow32[0], SampleFrames);

     // move right channel
     move(Inputs[1, 0], FTmpOutput32[0, 0], SampleFrames * SizeOf(Single));
     move(Inputs[1, 0], FTmpOutput32[1, 0], SampleFrames * SizeOf(Single));

     if VstHost[1].Active
      then VstHost[1].Process32Replacing(@FTmpOutput32[0], @FHigh32[0], SampleFrames);

     move(FLow32[0, 0],  Outputs[0, 0], SampleFrames * SizeOf(Single));
     move(FHigh32[0, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
    end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitLFO(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Data  : array [0..1, 0..1] of Double;
  Pan   : array [0..1] of Double;
  Channel, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample32(Inputs[Channel, i], @FTmpOutput32[Channel, i * FOSFactor]);

   // process
   with VstHost[0] do
    if Active then Process32Replacing(@FTmpOutput32[0], @FLow32[0], SampleFrames * FOSFactor);
   with VstHost[1] do
    if Active then Process32Replacing(@FTmpOutput32[0], @FHigh32[0], SampleFrames * FOSFactor);

   for i := 0 to SampleFrames - 1 do
    begin
     for Channel := 0 to Min(2, numOutputs) - 1 do
      begin
       Data[Channel, 0] := FOversampler[Channel, 0].DownSample32(@FLow32[Channel, i * FOSFactor]);
       Data[Channel, 1] := FOversampler[Channel, 1].DownSample32(@FHigh32[Channel, i * FOSFactor]);
      end;
     FSineLFO.CalculateNextSample;
     Pan[0] := (0.5 + 0.5 * FSineLFO.Cosine); Pan[1] := 1 - Pan[0];
     Outputs[0, i] := FVolumeFactor * (Pan[0] * Data[0, 0] + Pan[1] * Data[0, 1]);
     Outputs[1, i] := FVolumeFactor * (Pan[0] * Data[1, 0] + Pan[1] * Data[1, 1]);
    end;
  end
 else
  begin
   // directly process both VSTs to the low/high buffers
   with VstHost[0] do
    if Active then Process32Replacing(@Inputs[0], @FLow32[0], SampleFrames);
   with VstHost[1] do
    if Active then Process32Replacing(@Inputs[0], @FHigh32[0], SampleFrames);

   for i := 0 to SampleFrames - 1 do
    begin
     for Channel := 0 to min(2, numOutputs) - 1 do
      begin
       Data[Channel, 0] := FLow32[Channel, i];
       Data[Channel, 1] := FHigh32[Channel, i];
      end;
     FSineLFO.CalculateNextSample;

     Pan[0] := (0.5 + 0.5 * FSineLFO.Cosine); Pan[1] := 1 - Pan[0];
     Outputs[0, i] := FVolumeFactor * (Pan[0] * Data[0, 0] + Pan[1] * Data[0, 1]);
     Outputs[1, i] := FVolumeFactor * (Pan[0] * Data[1, 0] + Pan[1] * Data[1, 1]);
    end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitDynLFO(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Data  : array [0..1, 0..1] of Double;
  Pan   : array [0..1] of Double;
  Channel, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample32(Inputs[Channel, i], @FTmpOutput32[Channel, i * FOSFactor]);

   // process
   with VstHost[0] do
    if Active then Process32Replacing(@FTmpOutput32[0], @FLow32[0], SampleFrames * FOSFactor);
   with VstHost[1] do
    if Active then Process32Replacing(@FTmpOutput32[0], @FHigh32[0], SampleFrames * FOSFactor);

   for i := 0 to SampleFrames - 1 do
    begin
     for Channel := 0 to min(2, numOutputs) - 1 do
      begin
       Data[Channel, 0] := FOversampler[Channel, 0].DownSample32(@FLow32[Channel, i * FOSFactor]);
       Data[Channel, 1] := FOversampler[Channel, 1].DownSample32(@FHigh32[Channel, i * FOSFactor]);
      end;
     FSineLFO.CalculateNextSample;
     Pan[0] := (0.5 + 0.5 * FSineLFO.Cosine); Pan[1] := 1 - Pan[0];
     Outputs[0, i] := FVolumeFactor * (Pan[0] * Data[0, 0] + Pan[1] * Data[0, 1]);
     Outputs[1, i] := FVolumeFactor * (Pan[0] * Data[1, 0] + Pan[1] * Data[1, 1]);
    end;
  end
 else
  begin
   // directly process both VSTs to the low/high buffers
   with VstHost[0] do
    if Active then Process32Replacing(@Inputs[0], @FLow32[0], SampleFrames);
   with VstHost[1] do
    if Active then Process32Replacing(@Inputs[0], @FHigh32[0], SampleFrames);

   for i := 0 to SampleFrames - 1 do
    begin
     for Channel := 0 to min(2, numOutputs) - 1 do
      begin
       Data[Channel, 0] := FLow32[Channel, i];
       Data[Channel, 1] := FHigh32[Channel, i];
      end;
     FSineLFO.CalculateNextSample;

     Pan[0] := (0.5 + 0.5 * FSineLFO.Cosine); Pan[1] := 1 - Pan[0];
     Outputs[0, i] := FVolumeFactor * (Pan[0] * Data[0, 0] + Pan[1] * Data[0, 1]);
     Outputs[1, i] := FVolumeFactor * (Pan[0] * Data[1, 0] + Pan[1] * Data[1, 1]);
    end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitMidSide(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  L, H : Double;
  i    : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if numInputs = 1 then // process mono here
  begin
   if FOSActive then
    begin
     for i := 0 to SampleFrames - 1
      do FOversampler[0, 0].Upsample32(Inputs[0, i], @FTmpOutput32[0, i * FOSFactor]);

     if VstHost[0].Active
      then VstHost[0].Process32Replacing(@FTmpOutput32[0], @FLow32[0], SampleFrames * FOSFactor);
     if VstHost[1].Active
      then VstHost[1].Process32Replacing(@FTmpOutput32[0], @FHigh32[0], SampleFrames * FOSFactor);

     for i := 0 to SampleFrames - 1 do
      begin
       Outputs[0, i] := FVolumeFactor * (FOversampler[0, 0].DownSample32(@FLow32[0, i * FOSFactor]));
       Outputs[1, i] := FVolumeFactor * (FOversampler[1, 0].DownSample32(@FHigh32[0, i * FOSFactor]));
      end
    end
   else
    begin
     if VstHost[0].Active
      then VstHost[0].Process32Replacing(@Inputs[0], @Outputs[0], SampleFrames);
     if VstHost[1].Active
      then VstHost[1].Process32Replacing(@Inputs[0], @Outputs[1], SampleFrames);
    end;
  end
 else
  begin
   // upsample or move channels
   if FOSActive then
    for i := 0 to SampleFrames - 1 do
     begin
      FOversampler[0, 0].Upsample32(Inputs[0, i] + Inputs[1, i], @FLow32[0, i * FOSFactor]);
      FOversampler[0, 1].Upsample32(Inputs[0, i] - Inputs[1, i], @FHigh32[0, i * FOSFactor]);
     end
   else
    for i := 0 to SampleFrames - 1 do
     begin
      FLow32[0, i]  := Inputs[0, i] + Inputs[1, i];
      FHigh32[0, i] := Inputs[0, i] - Inputs[1, i];
     end;
   // dublicate internal channels
   Move(FLow32[0, 0], FLow32[1, 0], SampleFrames * SizeOf(Single) * FOSFactor);
   Move(FHigh32[0, 0], FHigh32[1, 0], SampleFrames * SizeOf(Single) * FOSFactor);

   VSTModuleProcess32SplitVST(SampleFrames);

   // downsample or move channels
   if FOSActive then
    for i := 0 to SampleFrames - 1 do
     begin
      L := FOversampler[0, 0].Downsample32(@FLow32[0, i * FOSFactor]);
      H := FOversampler[0, 1].Downsample32(@FHigh32[0, i * FOSFactor]);
      Outputs[0, i] := FVolumeFactor * 0.5 * (L + H);
      Outputs[1, i] := FVolumeFactor * 0.5 * (L - H);
     end
   else
    for i := 0 to SampleFrames - 1 do
     begin
      Outputs[0, i] := FVolumeFactor * 0.5 * (FLow32[0, i] + FHigh32[0, i]);
      Outputs[1, i] := FVolumeFactor * 0.5 * (FLow32[0, i] - FHigh32[0, i]);
     end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitSingle(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample32(Inputs[Channel, i], @FTmpOutput32[Channel, i * FOSFactor]);

   // process serial chain
   if VstHost[FPlugNr].Active then
    begin
     VstHost[FPlugNr].Process32Replacing(@FTmpOutput32[0], @FLow32[0], SampleFrames * FOSFactor);
     for Channel := 0 to numOutputs - 1
      do Move(FLow32[Channel, 0], FTmpOutput32[Channel, 0], SampleFrames * SizeOf(Single) * FOSFactor);
    end;

   // downsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do Outputs[Channel, i] := FVolumeFactor * FOversampler[Channel, 0].Downsample32(@FTmpOutput32[Channel, i * FOSFactor]);
  end
 else
  begin
   if VstHost[FPlugNr].Active
    then VstHost[FPlugNr].Process32Replacing(@Inputs[0], @Outputs[0], SampleFrames * FOSFactor)
    else
     for Channel := 0 to FMinChannels - 1
      do Move(Inputs[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Single) * FOSFactor);
  end;
end;

function SimpleDiode(const x: Single): Single;
begin
 Result := 0.5 * (Abs(x) + x);
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitSpin(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Data  : array [0..1, 0..1] of Double;
  Pan   : array [0..1, 0..1] of Double;
  Channel, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample32(Inputs[Channel, i], @FTmpOutput32[Channel, i * FOSFactor]);

   // process
   with VstHost[0] do
    if Active then Process32Replacing(@FTmpOutput32[0], @FLow32[0], SampleFrames * FOSFactor);
   with VstHost[1] do
    if Active then Process32Replacing(@FTmpOutput32[0], @FHigh32[0], SampleFrames * FOSFactor);

   for i := 0 to SampleFrames - 1 do
    begin
     for Channel := 0 to Min(2, numOutputs) - 1 do
      begin
       Data[Channel, 0] := FOversampler[Channel, 0].DownSample32(@FLow32[Channel, i * FOSFactor]);
       Data[Channel, 1] := FOversampler[Channel, 1].DownSample32(@FHigh32[Channel, i * FOSFactor]);
      end;
     FSineLFO.CalculateNextSample;

     Pan[0, 0] := sqr(SimpleDiode(Abs(FSineLFO.Cosine) - FSineLFO.Cosine));
     Pan[0, 1] := sqr(SimpleDiode(Abs(FSineLFO.Cosine) + FSineLFO.Cosine));
     Pan[1, 0] := sqr(SimpleDiode(Abs(FSineLFO.Sine) - FSineLFO.Sine));
     Pan[1, 1] := sqr(SimpleDiode(Abs(FSineLFO.Sine) + FSineLFO.Sine));

     Outputs[0, i] := Pan[0, 0] * Data[0, 0] +
                      Pan[1, 0] * Data[1, 0] +
                      Pan[0, 1] * Data[1, 1] +
                      Pan[1, 1] * Data[0, 1];
     Outputs[1, i] := Pan[0, 0] * Data[1, 0] +
                      Pan[1, 0] * Data[0, 0] +
                      Pan[0, 1] * Data[0, 1] +
                      Pan[1, 1] * Data[1, 1];
    end;
  end
 else
  begin
   // directly process both VSTs to the low/high buffers
   with VstHost[0] do
    if Active then Process32Replacing(@Inputs[0], @FLow32[0], SampleFrames);
   with VstHost[1] do
    if Active then Process32Replacing(@Inputs[0], @FHigh32[0], SampleFrames);

   for i := 0 to SampleFrames - 1 do
    begin
     for Channel := 0 to min(2, numOutputs) - 1 do
      begin
       Data[Channel, 0] := FLow32[Channel, i];
       Data[Channel, 1] := FHigh32[Channel, i];
      end;
     FSineLFO.CalculateNextSample;

     Pan[0, 0] := sqr(0.5 * SimpleDiode(Abs(FSineLFO.Cosine) - FSineLFO.Cosine));
     Pan[0, 1] := sqr(0.5 * SimpleDiode(Abs(FSineLFO.Cosine) + FSineLFO.Cosine));
     Pan[1, 0] := sqr(0.5 * SimpleDiode(Abs(FSineLFO.Sine) - FSineLFO.Sine));
     Pan[1, 1] := sqr(0.5 * SimpleDiode(Abs(FSineLFO.Sine) + FSineLFO.Sine));

     Outputs[0, i] := Pan[0, 0] * Data[0, 0] +
                      Pan[1, 0] * Data[1, 0] +
                      Pan[0, 1] * Data[1, 1] +
                      Pan[1, 1] * Data[0, 1];
     Outputs[1, i] := Pan[0, 0] * Data[1, 0] +
                      Pan[1, 0] * Data[0, 0] +
                      Pan[0, 1] * Data[0, 1] +
                      Pan[1, 1] * Data[1, 1];
    end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess32SplitTransient(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  L, H  : Double;
  Channel, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample32(Inputs[Channel, i], @FTmpOutput32[Channel, i * FOSFactor]);

   // process
   with VstHost[0] do
    if Active then Process32Replacing(@FTmpOutput32[0], @FLow32[0], SampleFrames * FOSFactor);
   with VstHost[1] do
    if Active then Process32Replacing(@FTmpOutput32[0], @FHigh32[0], SampleFrames * FOSFactor);

   for Channel := 0 to numOutputs - 1 do
    for i := 0 to SampleFrames - 1 do
     begin
      L := FOversampler[Channel, 0].DownSample32(@FLow32[Channel, i * FOSFactor]);
      H := FOversampler[Channel, 1].DownSample32(@FHigh32[Channel, i * FOSFactor]);
      FEnvelope[Channel, 0] := FReleaseFactor[0] * FEnvelope[Channel, 0] +
                          FAttackFactor[0] * SimpleDiode(Abs(Inputs[Channel, i]) - FEnvelope[Channel, 0]);
      Outputs[Channel, i] := FVolumeFactor * (FEnvelope[Channel, 0] * H + (1 - FEnvelope[Channel, 0]) * L);
     end;
  end
 else
  begin
   // directly process both VSTs to the low/high buffers
   with VstHost[0] do
    if Active then Process32Replacing(@Inputs[0], @FLow32[0], SampleFrames);
   with VstHost[1] do
    if Active then Process32Replacing(@Inputs[0], @FHigh32[0], SampleFrames);

   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1 do
     begin
      FEnvelope[Channel, 0] := FReleaseFactor[0] * FEnvelope[Channel, 0] +
        FAttackFactor[0] * SimpleDiode(Abs(Inputs[Channel, i]) - FEnvelope[Channel, 0]);
      Outputs[Channel, i]   := FVolumeFactor *
        (FEnvelope[Channel, 0] * FHigh32[Channel, i] + (1 - FEnvelope[Channel, 0]) * FLow32[Channel, i]);
     end;
  end;
end;

// 64bit

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitFrequencySimple(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  L     : Double;
  Channel, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  for Channel := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     L := FLowpass[Channel, 0].ProcessSample32(Inputs[Channel, i]);
     FOversampler[Channel, 0].Upsample64(L, @FLow64[Channel, i * FOSFactor]);
     FOversampler[Channel, 1].Upsample64(Inputs[Channel, i] - L, @FHigh64[Channel, i * FOSFactor]);
    end
 else
  for Channel := 0 to numInputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     FLow64[Channel, i]  := FLowpass[Channel, 0].ProcessSample32(Inputs[Channel, i]);
     FHigh64[Channel, i] := Inputs[Channel, i] - FLow64[Channel, i];
    end;

 VSTModuleProcess64SplitVST(SampleFrames);

 if FOSActive then
  for Channel := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1 do
    begin
     Outputs[Channel, i] := FVolumeFactor *
       (FOversampler[Channel, 0].DownSample64(@FLow64[Channel, i * FOSFactor]) +
        FOversampler[Channel, 1].DownSample64(@FHigh64[Channel, i * FOSFactor]));
    end
 else
  for Channel := 0 to numOutputs - 1 do
   for i := 0 to SampleFrames - 1
    do Outputs[Channel, i] := FVolumeFactor * (FLow64[Channel, i] + FHigh64[Channel, i]);
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitFrequencyLiRi(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex, SampleIndex  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  for ChannelIndex := 0 to numInputs - 1 do
   for SampleIndex := 0 to SampleFrames - 1 do
    begin
     FOversampler[ChannelIndex, 0].Upsample64(
       FLowpass[ChannelIndex, 1].ProcessSample32(
       FLowpass[ChannelIndex, 0].ProcessSample32(FLiRiSign * Inputs[ChannelIndex, SampleIndex])), @FLow64[ChannelIndex, SampleIndex * FOSFactor]);
     FOversampler[ChannelIndex, 1].Upsample64(
       FHighpass[ChannelIndex, 0].ProcessSample32(
       FHighpass[ChannelIndex, 1].ProcessSample32(Inputs[ChannelIndex, SampleIndex])), @FHigh64[ChannelIndex, SampleIndex * FOSFactor]);
    end
 else
  for ChannelIndex := 0 to numInputs - 1 do
   for SampleIndex := 0 to SampleFrames - 1 do
    begin
     FLow64[ChannelIndex, SampleIndex]  := FLowpass[ChannelIndex, 1].ProcessSample32(
       FLowpass[ChannelIndex, 0].ProcessSample32(FLiRiSign * Inputs[ChannelIndex, SampleIndex]));;
     FHigh64[ChannelIndex, SampleIndex] := FHighpass[ChannelIndex, 0].ProcessSample32(
       FHighpass[ChannelIndex, 1].ProcessSample32(Inputs[ChannelIndex, SampleIndex]));
    end;

 VSTModuleProcess64SplitVST(SampleFrames);

 if FOSActive then
  for ChannelIndex := 0 to numOutputs - 1 do
   for SampleIndex := 0 to SampleFrames - 1 do
    begin
     Outputs[ChannelIndex, SampleIndex] := FVolumeFactor *
       (FOversampler[ChannelIndex, 0].DownSample64(@FLow64[ChannelIndex, SampleIndex * FOSFactor]) +
        FOversampler[ChannelIndex, 1].DownSample64(@FHigh64[ChannelIndex, SampleIndex * FOSFactor]));
    end
 else
  for ChannelIndex := 0 to numOutputs - 1 do
   for SampleIndex := 0 to SampleFrames - 1
    do Outputs[ChannelIndex, SampleIndex] := FVolumeFactor * (FLow64[ChannelIndex, SampleIndex] + FHigh64[ChannelIndex, SampleIndex]);
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitExciter(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex, SampleIndex : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  for ChannelIndex := 0 to numInputs - 1 do
   for SampleIndex := 0 to SampleFrames - 1 do
    begin
     FOversampler[ChannelIndex, 0].Upsample64(
       FLowpass[ChannelIndex, 1].ProcessSample32(
       FLowpass[ChannelIndex, 0].ProcessSample32(FLiRiSign * Inputs[ChannelIndex, SampleIndex])), @FLow64[ChannelIndex, SampleIndex * FOSFactor]);
     FOversampler[ChannelIndex, 1].Upsample64(
       FHighpass[ChannelIndex, 0].ProcessSample32(
       FHighpass[ChannelIndex, 1].ProcessSample32(Inputs[ChannelIndex, SampleIndex])), @FHigh64[ChannelIndex, SampleIndex * FOSFactor]);
    end
 else
  for ChannelIndex := 0 to numInputs - 1 do
   for SampleIndex := 0 to SampleFrames - 1 do
    begin
     FLow64[ChannelIndex, SampleIndex]  := FLowpass[ChannelIndex, 1].ProcessSample32(
       FLowpass[ChannelIndex, 0].ProcessSample32(FLiRiSign * Inputs[ChannelIndex, SampleIndex]));;
     FHigh64[ChannelIndex, SampleIndex] := FHighpass[ChannelIndex, 0].ProcessSample32(
       FHighpass[ChannelIndex, 1].ProcessSample32(Inputs[ChannelIndex, SampleIndex]));
    end;

 VSTModuleProcess64SplitVST(SampleFrames);

 if FOSActive then
  for ChannelIndex := 0 to numOutputs - 1 do
   for SampleIndex := 0 to SampleFrames - 1 do
    begin
     Outputs[ChannelIndex, SampleIndex] := FVolumeFactor *
       (FOversampler[ChannelIndex, 0].DownSample64(@FLow64[ChannelIndex, SampleIndex * FOSFactor]) +
        FOversampler[ChannelIndex, 1].DownSample64(@FHigh64[ChannelIndex, SampleIndex * FOSFactor]));
    end
 else
  for ChannelIndex := 0 to numOutputs - 1 do
   for SampleIndex := 0 to SampleFrames - 1
    do Outputs[ChannelIndex, SampleIndex] := FVolumeFactor * (FLow64[ChannelIndex, SampleIndex] + FHigh64[ChannelIndex, SampleIndex]);
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitDynamic(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  L, H  : Double;
  Channel, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample64(Inputs[Channel, i], @FTmpOutput64[Channel, i * FOSFactor]);

   // process
   with VstHost[0] do
    if Active then Process64Replacing(@FTmpOutput64[0], @FLow64[0], SampleFrames * FOSFactor);
   with VstHost[1] do
    if Active then Process64Replacing(@FTmpOutput64[0], @FHigh64[0], SampleFrames * FOSFactor);

   for Channel := 0 to numOutputs - 1 do
    for i := 0 to SampleFrames - 1 do
     begin
      L := FOversampler[Channel, 0].DownSample64(@FLow64[Channel, i * FOSFactor]);
      H := FOversampler[Channel, 1].DownSample64(@FHigh64[Channel, i * FOSFactor]);
      FEnvelope[Channel, 0] := FLowpass[Channel, 0].ProcessSample32(Abs(Inputs[Channel, i]));
      Outputs[Channel, i]   := FVolumeFactor *
        (FEnvelope[Channel, 0] * H + (1 - FEnvelope[Channel, 0]) * L);
     end;
  end
 else
  begin
   // directly process both VSTs to the low/high buffers
   with VstHost[0] do
    if Active then Process64Replacing(@Inputs[0], @FLow64[0], SampleFrames);
   with VstHost[1] do
    if Active then Process64Replacing(@Inputs[0], @FHigh64[0], SampleFrames);

   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1 do
     begin
      FEnvelope[Channel, 0] := FLowpass[Channel, 0].ProcessSample32(Abs(Inputs[Channel, i]));
      Outputs[Channel, i]   := FVolumeFactor *
        (FEnvelope[Channel, 0] * FHigh64[Channel, i] + (1 - FEnvelope[Channel, 0]) * FLow64[Channel, i]);
     end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64Bypass(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel  : Integer;
begin
 for Channel := 0 to FMinChannels - 1
  do Move(Inputs[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Double));
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64Serial(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample64(Inputs[Channel, i], @FTmpOutput64[Channel, i * FOSFactor]);

   // process serial chain
   if VstHost[0].Active then
    begin
     VstHost[0].Process64Replacing(@FTmpOutput64[0], @FLow64[0], SampleFrames * FOSFactor);
     for Channel := 0 to numOutputs - 1
      do Move(FLow64[Channel, 0], FTmpOutput64[Channel, 0], SampleFrames * SizeOf(Double) * FOSFactor);
    end;
   if VstHost[1].Active then
    begin
     VstHost[1].Process64Replacing(@FTmpOutput64[0], @FLow64[0], SampleFrames * FOSFactor);
     for Channel := 0 to numOutputs - 1
      do Move(FLow64[Channel, 0], FTmpOutput64[Channel, 0], SampleFrames * SizeOf(Double) * FOSFactor);
    end;

   // downsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do Outputs[Channel, i] := FVolumeFactor * FOversampler[Channel, 0].Downsample64(@FTmpOutput64[Channel, i * FOSFactor]);
  end
 else
  begin
   if VstHost[0].Active then
    begin
     VstHost[0].Process64Replacing(@Inputs[0], @Outputs[0], SampleFrames * FOSFactor);

     // move output to input to prepare for the next stage
     if VstHost[1].Active then
      for Channel := 0 to FMinChannels - 1
       do Move(Outputs[Channel, 0], Inputs[Channel, 0], SampleFrames * SizeOf(Double) * FOSFactor);
    end;
   if VstHost[1].Active
    then VstHost[1].Process64Replacing(@Inputs[0], @Outputs[0], SampleFrames * FOSFactor);
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitLeftRight(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if numInputs = 1 then
  begin
   if FOSActive then
    begin
     for i := 0 to SampleFrames - 1
      do FOversampler[0, 0].Upsample64(Inputs[0, i], @FTmpOutput64[0, i * FOSFactor]);

     if VstHost[0].Active
      then VstHost[0].Process64Replacing(@FTmpOutput64[0], @FLow64[0], SampleFrames * FOSFactor);
     if VstHost[1].Active
      then VstHost[1].Process64Replacing(@FTmpOutput64[0], @FHigh64[0], SampleFrames * FOSFactor);

     for i := 0 to SampleFrames - 1 do
      begin
       Outputs[0, i] := FVolumeFactor * FOversampler[0, 0].DownSample64(@FLow64[0, i * FOSFactor]);
       Outputs[1, i] := FVolumeFactor * FOversampler[1, 0].DownSample64(@FHigh64[0, i * FOSFactor]);
      end
    end
   else
    begin
     if VstHost[0].Active
      then VstHost[0].Process64Replacing(@Inputs[0], @Outputs[0], SampleFrames);
     if VstHost[1].Active
      then VstHost[1].Process64Replacing(@Inputs[0], @Outputs[1], SampleFrames);
    end;
  end
 else
  begin
   if FOSActive then
    begin
     // upsample left channel
     for i := 0 to SampleFrames - 1
      do FOversampler[0, 0].Upsample64(Inputs[0, i], @FTmpOutput64[0, i * FOSFactor]);
     move(FTmpOutput64[0, 0], FTmpOutput64[1, 0], SampleFrames * FOSFactor * SizeOf(Double));

     if VstHost[0].Active
      then VstHost[0].Process64Replacing(@FTmpOutput64[0], @FLow64[0], SampleFrames * FOSFactor);

     // upsample right channel
     for i := 0 to SampleFrames - 1
      do FOversampler[1, 0].Upsample64(Inputs[1, i], @FTmpOutput64[0, i * FOSFactor]);
     move(FTmpOutput64[0, 0], FTmpOutput64[1, 0], SampleFrames * FOSFactor * SizeOf(Double));

     if VstHost[1].Active
      then VstHost[1].Process64Replacing(@FTmpOutput64[0], @FHigh64[0], SampleFrames * FOSFactor);

     for i := 0 to SampleFrames - 1 do
      begin
       Outputs[0, i] := FVolumeFactor * FOversampler[0, 0].DownSample64(@FLow64[0, i * FOSFactor]);
       Outputs[1, i] := FVolumeFactor * FOversampler[1, 0].DownSample64(@FHigh64[0, i * FOSFactor]);
      end
    end
   else
    begin
     // move left channel
     move(Inputs[0, 0], FTmpOutput64[0, 0], SampleFrames * SizeOf(Double));
     move(Inputs[0, 0], FTmpOutput64[1, 0], SampleFrames * SizeOf(Double));

     if VstHost[0].Active
      then VstHost[0].Process64Replacing(@FTmpOutput64[0], @FLow64[0], SampleFrames);

     // move right channel
     move(Inputs[1, 0], FTmpOutput64[0, 0], SampleFrames * SizeOf(Double));
     move(Inputs[1, 0], FTmpOutput64[1, 0], SampleFrames * SizeOf(Double));

     if VstHost[1].Active
      then VstHost[1].Process64Replacing(@FTmpOutput64[0], @FHigh64[0], SampleFrames);

     move(FLow64[0, 0],  Outputs[0, 0], SampleFrames * SizeOf(Double));
     move(FHigh64[0, 0], Outputs[1, 0], SampleFrames * SizeOf(Double));
    end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitLFO(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Data  : array [0..1, 0..1] of Double;
  Pan   : array [0..1] of Double;
  Channel, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample64(Inputs[Channel, i], @FTmpOutput64[Channel, i * FOSFactor]);

   // process
   with VstHost[0] do
    if Active then Process64Replacing(@FTmpOutput64[0], @FLow64[0], SampleFrames * FOSFactor);
   with VstHost[1] do
    if Active then Process64Replacing(@FTmpOutput64[0], @FHigh64[0], SampleFrames * FOSFactor);

   for i := 0 to SampleFrames - 1 do
    begin
     for Channel := 0 to min(2, numOutputs) - 1 do
      begin
       Data[Channel, 0] := FOversampler[Channel, 0].DownSample64(@FLow64[Channel, i * FOSFactor]);
       Data[Channel, 1] := FOversampler[Channel, 1].DownSample64(@FHigh64[Channel, i * FOSFactor]);
      end;
     FSineLFO.CalculateNextSample;
     Pan[0] := (0.5 + 0.5 * FSineLFO.Cosine); Pan[1] := 1 - Pan[0];
     Outputs[0, i] := FVolumeFactor * (Pan[0] * Data[0, 0] + Pan[1] * Data[0, 1]);
     Outputs[1, i] := FVolumeFactor * (Pan[0] * Data[1, 0] + Pan[1] * Data[1, 1]);
    end;
  end
 else
  begin
   // directly process both VSTs to the low/high buffers
   with VstHost[0] do
    if Active then Process64Replacing(@Inputs[0], @FLow64[0], SampleFrames);
   with VstHost[1] do
    if Active then Process64Replacing(@Inputs[0], @FHigh64[0], SampleFrames);

   for i := 0 to SampleFrames - 1 do
    begin
     for Channel := 0 to min(2, numOutputs) - 1 do
      begin
       Data[Channel, 0] := FLow64[Channel, i];
       Data[Channel, 1] := FHigh64[Channel, i];
      end;
     FSineLFO.CalculateNextSample;

     Pan[0] := (0.5 + 0.5 * FSineLFO.Cosine); Pan[1] := 1 - Pan[0];
     Outputs[0, i] := FVolumeFactor * (Pan[0] * Data[0, 0] + Pan[1] * Data[0, 1]);
     Outputs[1, i] := FVolumeFactor * (Pan[0] * Data[1, 0] + Pan[1] * Data[1, 1]);
    end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitDynLFO(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Data  : array [0..1, 0..1] of Double;
  Pan   : array [0..1] of Double;
  Channel, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample64(Inputs[Channel, i], @FTmpOutput64[Channel, i * FOSFactor]);

   // process
   with VstHost[0] do
    if Active then Process64Replacing(@FTmpOutput64[0], @FLow64[0], SampleFrames * FOSFactor);
   with VstHost[1] do
    if Active then Process64Replacing(@FTmpOutput64[0], @FHigh64[0], SampleFrames * FOSFactor);

   for i := 0 to SampleFrames - 1 do
    begin
     for Channel := 0 to min(2, numOutputs) - 1 do
      begin
       Data[Channel, 0] := FOversampler[Channel, 0].DownSample64(@FLow64[Channel, i * FOSFactor]);
       Data[Channel, 1] := FOversampler[Channel, 1].DownSample64(@FHigh64[Channel, i * FOSFactor]);
      end;
     FSineLFO.CalculateNextSample;
     Pan[0] := (0.5 + 0.5 * FSineLFO.Cosine); Pan[1] := 1 - Pan[0];
     Outputs[0, i] := FVolumeFactor * (Pan[0] * Data[0, 0] + Pan[1] * Data[0, 1]);
     Outputs[1, i] := FVolumeFactor * (Pan[0] * Data[1, 0] + Pan[1] * Data[1, 1]);
    end;
  end
 else
  begin
   // directly process both VSTs to the low/high buffers
   with VstHost[0] do
    if Active then Process64Replacing(@Inputs[0], @FLow64[0], SampleFrames);
   with VstHost[1] do
    if Active then Process64Replacing(@Inputs[0], @FHigh64[0], SampleFrames);

   for i := 0 to SampleFrames - 1 do
    begin
     for Channel := 0 to min(2, numOutputs) - 1 do
      begin
       Data[Channel, 0] := FLow64[Channel, i];
       Data[Channel, 1] := FHigh64[Channel, i];
      end;
     FSineLFO.CalculateNextSample;

     Pan[0] := (0.5 + 0.5 * FSineLFO.Cosine); Pan[1] := 1 - Pan[0];
     Outputs[0, i] := FVolumeFactor * (Pan[0] * Data[0, 0] + Pan[1] * Data[0, 1]);
     Outputs[1, i] := FVolumeFactor * (Pan[0] * Data[1, 0] + Pan[1] * Data[1, 1]);
    end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitMidSide(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  L, H : Double;
  i    : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if numInputs = 1 then // process mono here
  begin
   if FOSActive then
    begin
     for i := 0 to SampleFrames - 1
      do FOversampler[0, 0].Upsample64(Inputs[0, i], @FTmpOutput64[0, i * FOSFactor]);

     if VstHost[0].Active
      then VstHost[0].Process64Replacing(@FTmpOutput64[0], @FLow64[0], SampleFrames * FOSFactor);
     if VstHost[1].Active
      then VstHost[1].Process64Replacing(@FTmpOutput64[0], @FHigh64[0], SampleFrames * FOSFactor);

     for i := 0 to SampleFrames - 1 do
      begin
       Outputs[0, i] := FVolumeFactor * FOversampler[0, 0].DownSample64(@FLow64[0, i * FOSFactor]);
       Outputs[1, i] := FVolumeFactor * FOversampler[1, 0].DownSample64(@FHigh64[0, i * FOSFactor]);
      end
    end
   else
    begin
     if VstHost[0].Active
      then VstHost[0].Process64Replacing(@Inputs[0], @Outputs[0], SampleFrames);
     if VstHost[1].Active
      then VstHost[1].Process64Replacing(@Inputs[0], @Outputs[1], SampleFrames);
    end;
  end
 else
  begin
   // upsample or move channels
   if FOSActive then
    for i := 0 to SampleFrames - 1 do
     begin
      FOversampler[0, 0].Upsample64(Inputs[0, i] + Inputs[1, i], @FLow64[0, i * FOSFactor]);
      FOversampler[0, 1].Upsample64(Inputs[0, i] - Inputs[1, i], @FHigh64[0, i * FOSFactor]);
     end
   else
    for i := 0 to SampleFrames - 1 do
     begin
      FLow64[0, i]  := Inputs[0, i] + Inputs[1, i];
      FHigh64[0, i] := Inputs[0, i] - Inputs[1, i];
     end;
   // dublicate internal channels
   Move(FLow64[0, 0], FLow64[1, 0], SampleFrames * SizeOf(Double) * FOSFactor);
   Move(FHigh64[0, 0], FHigh64[1, 0], SampleFrames * SizeOf(Double) * FOSFactor);

   VSTModuleProcess64SplitVST(SampleFrames);

   // downsample or move channels
   if FOSActive then
    for i := 0 to SampleFrames - 1 do
     begin
      L := FOversampler[0, 0].Downsample64(@FLow64[0, i * FOSFactor]);
      H := FOversampler[0, 1].Downsample64(@FHigh64[0, i * FOSFactor]);
      Outputs[0, i] := FVolumeFactor * 0.5 * (L + H);
      Outputs[1, i] := FVolumeFactor * 0.5 * (L - H);
     end
   else
    for i := 0 to SampleFrames - 1 do
     begin
      Outputs[0, i] := FVolumeFactor * 0.5 * (FLow64[0, i] + FHigh64[0, i]);
      Outputs[1, i] := FVolumeFactor * 0.5 * (FLow64[0, i] - FHigh64[0, i]);
     end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitSingle(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel, i  : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample64(Inputs[Channel, i], @FTmpOutput64[Channel, i * FOSFactor]);

   // process serial chain
   if VstHost[FPlugNr].Active then
    begin
     VstHost[FPlugNr].Process64Replacing(@FTmpOutput64[0], @FLow64[0], SampleFrames * FOSFactor);
     for Channel := 0 to numOutputs - 1
      do Move(FLow64[Channel, 0], FTmpOutput64[Channel, 0], SampleFrames * SizeOf(Double) * FOSFactor);
    end;

   // downsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do Outputs[Channel, i] := FVolumeFactor * FOversampler[Channel, 0].Downsample64(@FTmpOutput64[Channel, i * FOSFactor]);
  end
 else
  if VstHost[FPlugNr].Active
   then VstHost[FPlugNr].Process32Replacing(@Inputs[0], @Outputs[0], SampleFrames * FOSFactor)
   else
    for Channel := 0 to FMinChannels - 1
     do Move(Inputs[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Double) * FOSFactor);
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitSpin(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Data  : array [0..1, 0..1] of Double;
  Pan   : array [0..1, 0..1] of Double;
  Channel, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample64(Inputs[Channel, i], @FTmpOutput64[Channel, i * FOSFactor]);

   // process
   with VstHost[0] do
    if Active then Process64Replacing(@FTmpOutput64[0], @FLow64[0], SampleFrames * FOSFactor);
   with VstHost[1] do
    if Active then Process64Replacing(@FTmpOutput64[0], @FHigh64[0], SampleFrames * FOSFactor);

   for i := 0 to SampleFrames - 1 do
    begin
     for Channel := 0 to numOutputs - 1 do
      begin
       Data[Channel, 0] := FOversampler[Channel, 0].DownSample64(@FLow64[Channel, i * FOSFactor]);
       Data[Channel, 1] := FOversampler[Channel, 1].DownSample64(@FHigh64[Channel, i * FOSFactor]);
      end;
     FSineLFO.CalculateNextSample;

     Pan[0, 0] := SimpleDiode(Abs(FSineLFO.Cosine) - FSineLFO.Cosine);
     Pan[0, 1] := SimpleDiode(Abs(FSineLFO.Cosine) + FSineLFO.Cosine);
     Pan[1, 0] := SimpleDiode(Abs(FSineLFO.Sine) - FSineLFO.Sine);
     Pan[1, 1] := SimpleDiode(Abs(FSineLFO.Sine) + FSineLFO.Sine);

     Outputs[0, i] := Pan[0, 0] * Data[0, 0] +
                      Pan[1, 0] * Data[1, 0] +
                      Pan[0, 1] * Data[1, 1] +
                      Pan[1, 1] * Data[0, 1];
     Outputs[1, i] := Pan[0, 0] * Data[1, 0] +
                      Pan[1, 0] * Data[0, 0] +
                      Pan[0, 1] * Data[0, 1] +
                      Pan[1, 1] * Data[1, 1];
    end;
  end
 else
  begin
   // directly process both VSTs to the low/high buffers
   with VstHost[0] do
    if Active then Process64Replacing(@Inputs[0], @FLow64[0], SampleFrames);
   with VstHost[1] do
    if Active then Process64Replacing(@Inputs[0], @FHigh64[0], SampleFrames);

   for i := 0 to SampleFrames - 1 do
    begin
     for Channel := 0 to numOutputs - 1 do
      begin
       Data[Channel, 0] := FLow64[Channel, i];
       Data[Channel, 1] := FHigh64[Channel, i];
      end;
     FSineLFO.CalculateNextSample;

     Pan[0, 0] := SimpleDiode(Abs(FSineLFO.Cosine) - FSineLFO.Cosine);
     Pan[0, 1] := SimpleDiode(Abs(FSineLFO.Cosine) + FSineLFO.Cosine);
     Pan[1, 0] := SimpleDiode(Abs(FSineLFO.Sine) - FSineLFO.Sine);
     Pan[1, 1] := SimpleDiode(Abs(FSineLFO.Sine) + FSineLFO.Sine);

     Outputs[0, i] := Pan[0, 0] * Data[0, 0] +
                      Pan[1, 0] * Data[1, 0] +
                      Pan[0, 1] * Data[1, 1] +
                      Pan[1, 1] * Data[0, 1];
     Outputs[1, i] := Pan[0, 0] * Data[1, 0] +
                      Pan[1, 0] * Data[0, 0] +
                      Pan[0, 1] * Data[0, 1] +
                      Pan[1, 1] * Data[1, 1];
    end;
  end;
end;

procedure TSplitTemplateDataModule.VSTModuleProcess64SplitTransient(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  L, H  : Double;
  Channel, i : Integer;
begin
 CheckSampleFrames(SampleFrames);

 if FOSActive then
  begin
   // upsample
   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1
     do FOversampler[Channel, 0].Upsample64(Inputs[Channel, i], @FTmpOutput64[Channel, i * FOSFactor]);

   // process
   with VstHost[0] do
    if Active then Process32Replacing(@FTmpOutput64[0], @FLow64[0], SampleFrames * FOSFactor);
   with VstHost[1] do
    if Active then Process32Replacing(@FTmpOutput64[0], @FHigh64[0], SampleFrames * FOSFactor);

   for Channel := 0 to numOutputs - 1 do
    for i := 0 to SampleFrames - 1 do
     begin
      L := FOversampler[Channel, 0].DownSample64(@FLow64[Channel, i * FOSFactor]);
      H := FOversampler[Channel, 1].DownSample64(@FHigh64[Channel, i * FOSFactor]);
      FEnvelope[Channel, 0] := FReleaseFactor[0] * FEnvelope[Channel, 0] +
                          FAttackFactor[0] * SimpleDiode(Abs(Inputs[Channel, i]) - FEnvelope[Channel, 0]);
      Outputs[Channel, i] := FVolumeFactor * (FEnvelope[Channel, 0] * H + (1 - FEnvelope[Channel, 0]) * L);
     end;
  end
 else
  begin
   // directly process both VSTs to the low/high buffers
   with VstHost[0] do
    if Active then Process64Replacing(@Inputs[0], @FLow64[0], SampleFrames);
   with VstHost[1] do
    if Active then Process64Replacing(@Inputs[0], @FHigh64[0], SampleFrames);

   for Channel := 0 to numInputs - 1 do
    for i := 0 to SampleFrames - 1 do
     begin
      FEnvelope[Channel, 0] := FReleaseFactor[0] * FEnvelope[Channel, 0] +
        FAttackFactor[0] * SimpleDiode(Abs(Inputs[Channel, i]) - FEnvelope[Channel, 0]);
      Outputs[Channel, i]   := FVolumeFactor *
        (FEnvelope[Channel, 0] * FHigh64[Channel, i] + (1 - FEnvelope[Channel, 0]) * FLow64[Channel, i]);
     end;
  end;
end;

end.
