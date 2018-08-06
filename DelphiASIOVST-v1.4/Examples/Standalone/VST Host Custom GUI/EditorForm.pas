unit EditorForm;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, Buttons, {$ELSE}Windows, Messages, XPMan,
  {$ENDIF}SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, ExtCtrls,
  StdCtrls, ToolWin, Dialogs, Menus, Contnrs, DAV_Types, DAV_ASIOHost,
  DAV_VSTHost;

type
  TFmVSTEditor = class(TForm)
    ASIOHost: TASIOHost;
    MainMenu: TMainMenu;
    MiAudio: TMenuItem;
    MiExit: TMenuItem;
    MiLoadPreset: TMenuItem;
    MiProgram: TMenuItem;
    MiSavePreset: TMenuItem;
    MiSetup: TMenuItem;
    MiStandalone: TMenuItem;
    N1: TMenuItem;
    OD: TOpenDialog;
    SD: TSaveDialog;
    VstHost: TVstHost;
    XPManifest: TXPManifest;
    GUI1: TMenuItem;
    MiGuiDefault: TMenuItem;
    MiGuiList: TMenuItem;
    MiGuiSelector: TMenuItem;
    MiGuiCustom: TMenuItem;
    PnPlugin: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostReset(Sender: TObject);
    procedure BtExitClick(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure MILoadPresetClick(Sender: TObject);
    procedure MiPresetClick(Sender: TObject);
    procedure MISavePresetClick(Sender: TObject);
    procedure MiSetupClick(Sender: TObject);
    procedure MiGuiDefaultClick(Sender: TObject);
    procedure MiGuiListClick(Sender: TObject);
    procedure MiGuiSelectorClick(Sender: TObject);
    procedure MiGuiCustomClick(Sender: TObject);
    procedure ShowCustomEdit(Sender: TObject; Control: TWinControl);
    procedure CloseCustomEdit(Sender: TObject);
    procedure BackgroundPaint(Sender: TObject);
  private
    FVSTInBuffer         : array of PDAVSingleFixedArray;
    FVSTOutBuffer        : array of PDAVSingleFixedArray;
    FInputChannelOffset  : Integer;
    FOutputChannelOffset : Integer;
    FGUIStyle            : TGUIStyle;
    FGUIElements         : TObjectList;
    FBackgroundBitmap    : TBitmap;
    procedure SetGUIStyle(const Value: TGUIStyle);
    procedure ControlChangeList(Sender: TObject);
  protected
    procedure GUIStyleChanged; virtual;
  public
    property InputChannelOffset: Integer read FInputChannelOffset write FInputChannelOffset;
    property OutputChannelOffset: Integer read FOutputChannelOffset write FOutputChannelOffset;
    property GUIStyle: TGUIStyle read FGUIStyle write SetGUIStyle default gsDefault;
  end;

var
  FmVSTEditor: TFmVSTEditor;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Math, IniFiles, DAV_VSTEffect, DAV_GuiCommon, DAV_GuiLabel, DAV_GuiSlider,
  DAV_GuiFont, DAV_GuiShadow, EditorSetup;

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;

procedure TFmVSTEditor.FormCreate(Sender: TObject);
var
  theRect             : TRect;
  i                   : Integer;
  s, p                : AnsiString;
  MenuItem            : TMenuItem;
  ContainedVSTPlugins : TStringList;
  RS                  : TResourceStream;
begin
 FGUIStyle := gsDefault;

 with VstHost[0] do
  begin
   // set default GUI style
   GUIStyle := FGUIStyle;

   if ParamCount > 0
    then DLLFileName := ParamStr(1)
    else
     begin
      ContainedVSTPlugins := TStringList.Create;
      try
       EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc, LongWord(ContainedVSTPlugins));

       if ContainedVSTPlugins.Count > 0 then
        begin
         RS := TResourceStream.Create(HInstance, ContainedVSTPlugins[0], 'DLL');
         try
          LoadFromStream(RS);
         finally
          FreeAndNil(RS);
         end;
        end else

       if not FileExists(DLLFileName) then
        with TOpenDialog.Create(Self) do
         try
          DefaultExt := 'dll';
          Filter := 'VST Plugin (*.dll)|*.dll';
          Options := Options + [ofFileMustExist];
          if Execute then DLLFileName := FileName;

          if not FileExists(DLLFileName) then
           begin
            Application.Terminate;
            Exit;
           end;

         finally
          Free;
         end;
      finally
       FreeAndNil(ContainedVSTPlugins);
      end;
     end;

   Active := True;
   Idle;
   ShowEdit(PnPlugin);
   Idle;
   EditIdle;
   Caption := string(GetVendorString) + ' ' + string(GetEffectName);
  end;
 while MiProgram.Count > 3
  do MiProgram.Delete(3);

 for i := 0 to Min(64, VstHost[0].numPrograms) - 1 do
  begin
   VstHost[0].GetProgramNameIndexed(-1, i, p);
   s := AnsiString(IntToStr(i));
   if i < 10 then s := '0' + s;
   s := s + ' - ' + p;
   MenuItem := TMenuItem.Create(MiProgram);
   with MenuItem do
    begin
     Caption := string(s);
     RadioItem := True;
     Tag := i;
     OnClick := MiPresetClick;
    end;
   MiProgram.Add(MenuItem);
  end;
 if MiProgram.Count > 3
  then MiProgram.Items[3].Checked := True;

 if (effFlagsHasEditor in VstHost[0].EffectOptions) then
  begin
   theRect := VstHost[0].GetRect;
   ClientWidth := theRect.Right - theRect.Left;
   ClientHeight := theRect.Bottom - theRect.Top;
  end;
 SetLength(FVSTInBuffer, VstHost[0].numInputs);
 SetLength(FVSTOutBuffer, VstHost[0].numOutputs);

 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.ini') do
  try
   Top  := ReadInteger('Layout', 'Main Top', Top);
   Left := ReadInteger('Layout', 'Main Left', Left);
  finally
   Free;
  end;
end;

procedure TFmVSTEditor.FormDestroy(Sender: TObject);
var
  ChannelIndex: Integer;
begin
 for ChannelIndex := 0 to Length(FVSTInBuffer) - 1
  do Dispose(FVSTInBuffer[ChannelIndex]);
 for ChannelIndex := 0 to Length(FVSTOutBuffer) - 1
  do Dispose(FVSTOutBuffer[ChannelIndex]);
end;

procedure TFmVSTEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 ASIOHost.Active := False;
 VSTHost[0].Active := False;
 sleep(10);
 Application.ProcessMessages;
 ASIOHOST.Active := False;
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.INI') do
  try
   WriteInteger('Layout', 'Main Top', Top);
   WriteInteger('Layout', 'Main Left', Left);
  finally
   Free;
  end;
end;

procedure TFmVSTEditor.FormActivate(Sender: TObject);
begin
 VstHost[0].EditActivate;
end;

procedure TFmVSTEditor.FormDeactivate(Sender: TObject);
begin
 VstHost[0].EditDeActivate;
end;

procedure TFmVSTEditor.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmVSTEditor.MiGuiCustomClick(Sender: TObject);
begin
 MiGuiCustom.Checked := True;
 GUIStyle := gsCustom;
end;

procedure TFmVSTEditor.MiGuiDefaultClick(Sender: TObject);
begin
 MiGuiDefault.Checked := True;
 GUIStyle := gsDefault;
end;

procedure TFmVSTEditor.MiGuiListClick(Sender: TObject);
begin
 MiGuiList.Checked := True;
 GUIStyle := gsParameterList;
end;

procedure TFmVSTEditor.MiGuiSelectorClick(Sender: TObject);
begin
 MiGuiSelector.Checked := True;
 GUIStyle := gsParameterSelector;
end;

procedure TFmVSTEditor.MILoadPresetClick(Sender: TObject);
begin
 with OD do
  if Execute then
   case FilterIndex of
    1 : VstHost[0].LoadPreset(FileName);
    2 : VstHost[0].LoadBank(FileName);
   end;
end;

procedure TFmVSTEditor.MISavePresetClick(Sender: TObject);
begin
 with SD do
  if Execute then
   case FilterIndex of
    1 : VstHost[0].SavePreset(FileName);
    2 : VstHost[0].SaveBank(FileName);
   end;
end;

procedure TFmVSTEditor.MiSetupClick(Sender: TObject);
begin
 FmSetup.ShowModal;
end;

procedure TFmVSTEditor.SetGUIStyle(const Value: TGUIStyle);
begin
 if FGUIStyle <> Value then
  begin
   FGUIStyle := Value;
   GUIStyleChanged;
  end;
end;

procedure TFmVSTEditor.ShowCustomEdit(Sender: TObject;
  Control: TWinControl);
var
  i, j          : Integer;
  MaxParamWidth : Integer;
  x, y          : Integer;
  s             : array[0..1] of Single;
  Line          : PRGB24Array;
  h, hr         : Single;
begin
 with TCustomVSTPlugin(Sender) do
  if GuiStyle = gsCustom then
   begin
    ClientWidth := 400;
    ClientHeight := 12 + numParams * 20;

    // create component container if necessary
    if not Assigned(FGUIElements)
     then FGUIElements := TObjectList.Create
     else FGUIElements.Clear;

    with TPaintBox(FGUIElements[FGUIElements.Add(TPaintBox.Create(Control))]) do
     begin
      Parent := PnPlugin;
      Align := alClient;
      OnPaint := BackgroundPaint;
      if not Assigned(FBackgroundBitmap) then
       begin
        FBackgroundBitmap := TBitmap.Create;
        FBackgroundBitmap.Width := Width;
        FBackgroundBitmap.Height := Height;
        with FBackgroundBitmap do
         begin
          PixelFormat := pf24bit;
          hr          := 1 / Height;
          s[0]        := 0;
          s[1]        := 0;
          for y := 0 to Height - 1 do
           begin
            Line := Scanline[y];
            h    := 0.6 * (1 - sqr(2 * (y - Height div 2) * hr));
            for x := 0 to Width - 1 do
             begin
              s[1] := 0.97 * s[0] + 0.03 * (2 * random - 1);
              s[0] := s[1];
              Line[x].B := Round($C0 + $1A * (h + s[1]));
              Line[x].G := Round($C8 + $1C * (h + s[1]));
              Line[x].R := Round($CA + $1E * (h + s[1]));
             end;
           end;
         end;
        Invalidate; 
       end;
     end;

    // scan maximum parameter name length
    MaxParamWidth := 0;
    with TLabel.Create(Control) do
     try
      Parent := Control;
      Alignment := taCenter;
      for i := 0 to numParams - 1 do
       if Canvas.TextWidth(string(ParameterName[i]) + ':_') > MaxParamWidth
        then MaxParamWidth := Canvas.TextWidth(string(ParameterName[i]) + ':_');
     finally
      Free;
     end;

    for i := 0 to numParams - 1 do
     begin
      with TGuiLabel(FGUIElements[FGUIElements.Add(TGuiLabel.Create(Control))]) do
       begin
        Parent := Control; Caption := string(ParameterName[i]) + ':'; Tag := i;
        Width := Canvas.TextWidth(Caption); Alignment := taCenter; Left := 4;
        Height := 16; Top := 8 + i * (4 + Height);
        FontOversampling := fo3x;
        Transparent := True;
       end;
      with TGuiLabel(FGUIElements[FGUIElements.Add(TGuiLabel.Create(Control))]) do
       begin
        Name := 'GLV' + IntToStr(I); Tag := i; Anchors := [akTop, akRight];
        Parent  := Control; Alignment := taCenter;
        Width := Canvas.TextWidth(Caption);
        Height  := 16; Left := Control.Width - Left - 72;
        Alignment := taCenter; Width := 65; Top := 8 + i * (4 + Height);
        FontOversampling := fo3x; Transparent := True;
       end;

      j := FGUIElements.Add(TGuiSlider.Create(Control));
      with TGuiSlider(FGUIElements[j]) do
       begin
        Parent := Control; Anchors := [akLeft, akTop, akRight];
        Tag := i; Height := 16; Top := 8 + i * (4 + Height);
        Name := 'Slider' + IntToStr(I);
        Left := MaxParamWidth + 2; Width := Control.Width - Left - 72;
        Min := 0; Max := 1; TabOrder := 3 + i;
        Value := Parameter[i];
        BorderRadius := 5; BorderWidth := 2; 
        Transparent := True;
        OnChange := ControlChangeList;
        ControlChangeList(FGUIElements[j]);
       end;

     end;
   end;
end;

procedure TFmVSTEditor.ControlChangeList(Sender: TObject);
var
  Lbl    : TGuiLabel;
  Str    : AnsiString;
  ChrPos : Integer;
begin
 // ensure sender is TGuiSlider
 Assert(Sender is TGuiSlider);

 with TGuiSlider(Sender) do
  try
   // convert integer position to float
   VstHost[0].Parameter[Tag] := Value;

   // locate value label
   Lbl := TGuiLabel(VstHost[0].GUIControl.FindComponent('GLV' + IntToStr(Tag)));
   if Assigned(Lbl) then
    begin
     // eventually add parameter label
     if VstHost[0].ParameterLabel[Tag] <> ''
      then Str := VstHost[0].ParameterDisplay[Tag] + ' ' + VstHost[0].ParameterLabel[Tag]
      else Str := VstHost[0].ParameterDisplay[Tag];

     if Length(Str) < 9
      then Lbl.Caption := string(Str)
      else
       begin
        Str := VstHost[0].ParameterDisplay[Tag];
        if Pos('.', string(Str)) > 0 then
         begin
          ChrPos := Length(Str) - 1;
          while Str[ChrPos] = '0' do
           begin
            Delete(Str, ChrPos, 1);
            Dec(ChrPos);
           end;
         end;
        if VstHost[0].ParameterLabel[Tag] <> ''
         then Lbl.Caption := string(Str) + ' ' + string(VstHost[0].ParameterLabel[Tag])
         else Lbl.Caption := string(Str);
        if Length(Lbl.Caption) > 9 then Lbl.Caption := string(Str)
       end;
    end;
  except
  end;
end;

procedure TFmVSTEditor.CloseCustomEdit(Sender: TObject);
begin
 with TCustomVSTPlugin(Sender) do
  if (GuiStyle = gsCustom) and Assigned(FGUIElements)
   then FreeAndNil(FGUIElements);

 if Assigned(FBackgroundBitmap)
  then FreeAndNil(FBackgroundBitmap);
end;

procedure TFmVSTEditor.GUIStyleChanged;
var
  theRect : TRect;
begin
 with VstHost[0] do
  if EditVisible then
   begin
    CloseEdit;
    GUIStyle := FGUIStyle;
    ShowEdit(PnPlugin);

    if GuiStyle <> gsCustom then
     begin
      theRect := VstHost[0].GetRect;
      ClientWidth := theRect.Right - theRect.Left;
      ClientHeight := theRect.Bottom - theRect.Top;
     end;
   end
  else GUIStyle := FGUIStyle;
end;

procedure TFmVSTEditor.MiPresetClick(Sender: TObject);
begin
 with TMenuItem(Sender) do
  begin
   VstHost[0].CurrentProgram := Tag;
   Checked := True;
  end;
end;

procedure TFmVSTEditor.BackgroundPaint(Sender: TObject);
begin
 if (FGUIStyle = gsCustom) and Assigned(FBackgroundBitmap)
  then TPaintBox(Sender).Canvas.Draw(0, 0, FBackgroundBitmap);
end;

procedure TFmVSTEditor.BtExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmVSTEditor.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  ChannelIndex : Integer;
  AsioChannel  : Integer;
begin
 if VSTHost[0].Active then
  begin
   // copy input data
   for ChannelIndex := 0 to Length(InBuffer) - 1 do
    begin
     AsioChannel := (InputChannelOffset + ChannelIndex) mod ASIOHost.InputChannelCount;
     Move(InBuffer[AsioChannel, 0], FVSTInBuffer[ChannelIndex, 0], ASIOHost.BufferSize * SizeOf(Single));
    end;

   // process
   VSTHost[0].Process32Replacing(@FVSTInBuffer[0],
     @FVSTOutBuffer[0], ASIOHost.BufferSize);

   // copy output data
   for ChannelIndex := 0 to Length(OutBuffer) - 1 do
    begin
     AsioChannel := (OutputChannelOffset + ChannelIndex) mod ASIOHost.OutputChannelCount;
     Move(OutBuffer[AsioChannel, 0], FVSTOutBuffer[ChannelIndex, 0], ASIOHost.BufferSize * SizeOf(Single));
    end;
  end;
end;

procedure TFmVSTEditor.ASIOHostReset(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 VSTHost.BlockSize := ASIOHost.BufferSize;
 for ChannelIndex := 0 to Length(FVSTInBuffer) - 1
  do ReallocMem(FVSTInBuffer[ChannelIndex], VSTHost.BlockSize * SizeOf(Single));
 for ChannelIndex := 0 to Length(FVSTOutBuffer) - 1
  do ReallocMem(FVSTOutBuffer[ChannelIndex], VSTHost.BlockSize * SizeOf(Single));
end;

{$IFDEF FPC}
initialization
  {$i EditorForm.lrs}
{$ENDIF}

end.
