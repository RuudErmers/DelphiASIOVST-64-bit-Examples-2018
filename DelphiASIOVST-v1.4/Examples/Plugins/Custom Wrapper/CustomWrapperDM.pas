unit CustomWrapperDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Graphics, Forms, DAV_Types,
  DAV_VSTModule, DAV_VstHost, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel,
  DAV_VSTEffect, DAV_ChunkPluginGUI;

type
  TCustomWrapperDataModule = class(TVSTModule)
    VstHost: TVstHost;
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleStartProcess(Sender: TObject);
    procedure VSTModuleStopProcess(Sender: TObject);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure CustomParameterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure CustomParameterLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleEditClose(Sender: TObject; var DestroyForm: Boolean);
    procedure VSTModuleProcessEvents(Sender: TObject; const Events: TVstEvents);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleSoftBypass(Sender: TObject; const isBypass: Boolean);
    procedure VSTModuleProcessVarIO(Sender: TObject; const varIo: TVstVariableIo);
  private
    FDials      : array of TGuiDial;
    FLabels     : array of TGuiLabel;
    FDisplays   : array of TGuiLabel;
    FMaxInputs  : Integer;
    FMaxOutputs : Integer;
    procedure DialChanged(Sender: TObject);
    procedure AssignKnobBitmap(const Dial: TGuiDial);
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, Dialogs, Controls, PNGImage, DAV_VSTParameters,
  DAV_VSTModuleWithPrograms;

resourcestring
  RCStrCouldNotLoadGUI = 'Could not load GUI definition, defaults used';

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

procedure TCustomWrapperDataModule.VSTModuleCreate(Sender: TObject);
var
  RN   : TStringList;
  RS   : TResourceStream;
  PI   : TCustomVstPlugIn;
  i, n : Integer;
begin
 FMaxInputs  := 0;
 FMaxOutputs := 0;
 RN          := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc, DWord(RN));

  for n := 0 to RN.Count - 1 do
   begin
    PI := VstHost.VstPlugIns.Add;

    // load plugin from resource
    RS := TResourceStream.Create(hInstance, RN[n], 'DLL');
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
       DisplayName              := VstHost[n].GetParamName(i);
      end;
    if PI.numInputs  > FMaxInputs  then FMaxInputs  := PI.numInputs;
    if PI.numOutputs > FMaxOutputs then FMaxOutputs := PI.numOutputs;
    if PI.numPrograms > 0 then PI.SetProgram(0);
   end;
 finally
  FreeAndNil(RN);
 end;
end;

procedure TCustomWrapperDataModule.VSTModuleDestroy(Sender: TObject);
begin
 VSTHost.VstPlugIns.Clear;
end;

procedure TCustomWrapperDataModule.VSTModuleOpen(Sender: TObject);
begin
 // Todo
end;

procedure TCustomWrapperDataModule.VSTModuleClose(Sender: TObject);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].Active := False;
end;

procedure TCustomWrapperDataModule.VSTModuleEditClose(Sender: TObject;
  var DestroyForm: Boolean);
var
  i : Integer;
begin
 for i := 0 to Length(FDials) - 1 do FreeAndNil(FDials[i]);
 SetLength(FDials, 0);

 for i := 0 to Length(FLabels) - 1 do FreeAndNil(FLabels[i]);
 SetLength(FLabels, 0);

 for i := 0 to Length(FDisplays) - 1 do FreeAndNil(FDisplays[i]);
 SetLength(FDisplays, 0);
end;

procedure TCustomWrapperDataModule.AssignKnobBitmap(const Dial: TGuiDial);
var
  i, j   : Integer;
  Aspect : Single;
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 if FindResource(hInstance, 'KNOB', 'PNG') = 0 then exit;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(HInstance, 'KNOB', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   i := 0;
   Dial.DialBitmap.Assign(PngBmp);
   with Dial.DialBitmap do
    if Width > Height then
     begin
      Dial.StitchKind := skHorizontal;
      j := Width div Height;
      while True do
       begin
        Aspect := Width / (j + i);
        if (Aspect >= 1) and (abs(Aspect - Round(Aspect)) < 1E-24)
         then break;
        Aspect := Width / (j - i);
        if (Aspect > 0) and (abs(Aspect - Round(Aspect)) < 1E-24)
         then break
         else inc(i);
       end;
     end
    else
     begin
      Dial.StitchKind := skVertical;
      j := Height div Width;
      while True do
       begin
        Aspect := Height / (j + i);
        if (Aspect >= 1) and (abs(Aspect - Round(Aspect)) < 1E-24)
         then break;
        Aspect := Height / (j - i);
        if (Aspect > 0) and (abs(Aspect - Round(Aspect)) < 1E-24)
         then break
         else inc(i);
       end;
     end;
   Dial.GlyphCount := j + i;

  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;

end;

procedure TCustomWrapperDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
var
  numElementsPerRow, i : Integer;
  RS                   : TResourceStream;
  ChunkName            : TChunkName;
  FontSize             : Byte;
  FontAntiAlias        : TGuiAntiAlias;
  PluginGUI            : TDAVPluginGuiChunk;
begin
 numElementsPerRow := 4;
 GUI := TForm.Create(Self);

 if FindResource(hInstance, 'PLUGINGUI', PChar(10)) <> 0 then
  begin
   RS := TResourceStream.Create(HInstance, 'PLUGINGUI', PChar(10));
   PluginGUI := TDAVPluginGuiChunk.Create;
   try
    RS.Read(ChunkName, 4);
    Assert(ChunkName = 'PGUI');
    PluginGUI.LoadFromStream(RS);
    GUI.Color         := PluginGUI.BackgroundColor;
    numElementsPerRow := PluginGUI.KnobsPerRow;
    FontSize          := PluginGUI.FontSize;
    FontAntiAlias     := PluginGUI.FontAntiAliasing;
   finally
    FreeAndNil(PluginGUI);
   end;
  end
 else
  begin
   FontSize      := 8;
   FontAntiAlias := gaaNone;
   ShowMessage(RCStrCouldNotLoadGUI);
  end;

 with GUI do
  begin
   BorderStyle  := bsNone;
   ClientWidth  := numElementsPerRow * 64;
   ClientHeight := ((numParams + numElementsPerRow - 1) div numElementsPerRow) * 96;

   SetLength(FDials, numParams);
   SetLength(FLabels, numParams);
   SetLength(FDisplays, numParams);
   for i := 0 to numParams - 1 do
    begin
     FDials[i] := TGuiDial.Create(Gui);
     with FDials[i] do
      begin
       Parent              := GUI;
       Width               := 48;
       Height              := 48;
       AssignKnobBitmap(FDials[i]);
       Left                := 8 + (i mod numElementsPerRow) * (FDials[i].Width + 16);
       Top                 := 24 + (i div numElementsPerRow) * (FDials[i].Height + 48);
       LineWidth           := 2;
       LineColor           := clRed;
       PointerAngles.Range := 270;
       PointerAngles.Start := 225;
       Min                 := 0;
       Max                 := 1;
       Tag                 := i;
       Position            := Parameter[i];
       OnChange            := DialChanged;
       AntiAlias           := FontAntiAlias;
      end;
     FLabels[i] := TGuiLabel.Create(Gui);
     with FLabels[i] do
      begin
       Parent    := GUI;
       Width     := (FDials[i].Width + 16);
       Height    := 16;
       Left      := (i mod numElementsPerRow) * (FDials[i].Width + 16);
       Top       := 8 + (i div numElementsPerRow) * (FDials[i].Height + 48);
       Tag       := i;
       Alignment := taCenter;
       Font.Size := FontSize;
       Caption   := ParameterProperties[i].DisplayName;
       AntiAlias := FontAntiAlias;
      end;
     FDisplays[i] := TGuiLabel.Create(Gui);
     with FDisplays[i] do
      begin
       Parent    := GUI;
       Width     := (FDials[i].Width + 16);
       Height    := 16;
       Left      := (i mod numElementsPerRow) * (FDials[i].Width + 16);
       Top       := (FDials[i].Height + 24) + (i div numElementsPerRow) * (FDials[i].Height + 48);
       Tag       := i;
       Alignment := taCenter;
       Caption   := '';
       Font.Size := FontSize;
       AntiAlias := FontAntiAlias;
      end;
    end;
   if Length(FDials) > 0 then
    begin
     ClientWidth  := min(numElementsPerRow, Length(FDials)) * (FDials[0].Width + 16);
     ClientHeight := ((numParams + numElementsPerRow - 1) div numElementsPerRow) * (FDials[0].Height + 48);
    end;

   GUI.Visible     := True;
  end;
end;

procedure TCustomWrapperDataModule.DialChanged(Sender: TObject);
begin
 if Sender is TGuiDial then
  with TGuiDial(Sender) do
   begin
    Parameter[Tag] := Position;
   end;
end;

procedure TCustomWrapperDataModule.VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].SetBlockSizeAndSampleRate(BlockSize, SampleRate)
end;

procedure TCustomWrapperDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := 0;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
  end;
 VstHost[n].Parameter[Index - pnr] := Value;
 if (Index < Length(FDials)) and Assigned(FDials[Index])
  then FDials[Index].Position := Value;
 if (Index < Length(FDisplays)) and Assigned(FDisplays[Index])
  then FDisplays[Index].Caption := VstHost[n].GetParamDisplay(Index - pnr) +
                                   ' ' + VstHost[n].GetParamLabel(Index - pnr);
end;

procedure TCustomWrapperDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].SetSampleRate(SampleRate);
end;

procedure TCustomWrapperDataModule.VSTModuleSoftBypass(Sender: TObject;
  const isBypass: Boolean);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].SetBypass(isBypass);
end;

procedure TCustomWrapperDataModule.VSTModuleStartProcess(Sender: TObject);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].StartProcess;
end;

procedure TCustomWrapperDataModule.VSTModuleStopProcess(Sender: TObject);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].StopProcess;
end;

procedure TCustomWrapperDataModule.VSTModuleSuspend(Sender: TObject);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].MainsChanged(False);
end;

procedure TCustomWrapperDataModule.CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := 0;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
  end;
 PreDefined := VstHost[n].GetParamDisplay(Index - pnr);
end;

procedure TCustomWrapperDataModule.CustomParameterLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  n, pnr : Integer;
begin
 n   := 0;
 pnr := 0;
 while (Index >= pnr + VstHost[n].numParams) do
  begin
   Inc(pnr, VstHost[n].numParams);
   Inc(n);
  end;
 PreDefined := VstHost[n].GetParamLabel(Index - pnr);
end;

procedure TCustomWrapperDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  chcnt : Integer;
  ch, n : Integer;
  Temp  : TDAVArrayOfDoubleDynArray;
  InOut : TDAVArrayOfDoubleDynArray;
begin
 SetLength(Temp, FMaxInputs, SampleFrames);
 ChCnt := min(FMaxInputs, FMaxOutputs);
 InOut := Inputs;

 for n := 0 to VstHost.Count - 1 do
  begin
   for ch := 0 to ChCnt - 1
    do Move(InOut[ch, 0], Temp[ch, 0], SampleFrames * SizeOf(Double));
   VstHost[n].ProcessDoubleReplacing(@Temp[0], @Outputs[0], SampleFrames);
   InOut := Outputs;
  end;
end;

procedure TCustomWrapperDataModule.VSTModuleProcessEvents(Sender: TObject;
  const Events: TVstEvents);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].ProcessEvents(Events);
end;

procedure TCustomWrapperDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  chcnt : Integer;
  ch, n : Integer;
  Temp  : TDAVArrayOfSingleDynArray;
  InOut : TDAVArrayOfSingleDynArray;
begin
 SetLength(Temp, FMaxInputs, SampleFrames);
 ChCnt := min(FMaxInputs, FMaxOutputs);
 InOut := Inputs;

 for n := 0 to VstHost.Count - 1 do
  begin
   for ch := 0 to ChCnt - 1
    do Move(InOut[ch, 0], Temp[ch, 0], SampleFrames * SizeOf(Single));
   VstHost[n].Process(@Temp[0], @Outputs[0], SampleFrames);
   InOut := Outputs;
  end;
end;

procedure TCustomWrapperDataModule.VSTModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  chcnt : Integer;
  ch, n : Integer;
  Temp  : TDAVArrayOfSingleDynArray;
  InOut : TDAVArrayOfSingleDynArray;
begin
 SetLength(Temp, FMaxInputs, SampleFrames);
 ChCnt := min(FMaxInputs, FMaxOutputs);
 InOut := Inputs;

 for n := 0 to VstHost.Count - 1 do
  begin
   for ch := 0 to ChCnt - 1
    do Move(InOut[ch, 0], Temp[ch, 0], SampleFrames * SizeOf(Single));
   VstHost[n].ProcessReplacing(@Temp[0], @Outputs[0], SampleFrames);
   InOut := Outputs;
  end;
end;

procedure TCustomWrapperDataModule.VSTModuleProcessVarIO(Sender: TObject;
  const varIo: TVstVariableIo);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].ProcessVarIo(VarIO);
end;

procedure TCustomWrapperDataModule.VSTModuleResume(Sender: TObject);
var
  n : Integer;
begin
 for n := 0 to VstHost.Count - 1
  do VstHost[n].MainsChanged(True);
end;

end.
