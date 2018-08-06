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

{$I DAV_Compiler.inc}
{-$DEFINE LoadFromMemory}
{-$DEFINE LoadFromResource}

uses
{$IFDEF FPC}LCLIntf, Buttons, {$ELSE}Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, ExtCtrls,
  StdCtrls, ToolWin, Dialogs, Menus, DAV_Types, DAV_ASIOHost, DAV_VSTHost;

type
  TFmVSTEditor = class(TForm)
    AsioHost: TAsioHost;
    BtnExit: TButton;
    BtnSeparator1: TToolButton;
    BtnSeparator2: TToolButton;
    BtnSeparator3: TToolButton;
    BtnSetup: TButton;
    CbxPreset: TComboBox;
    LblPreset: TLabel;
    ToolBar: TToolBar;
    VstHost: TVstHost;
    VSTPanel: TPanel;
{$IFNDEF FPC}
    MnuLoadPreset: TMenuItem;
    MnuSavePreset: TMenuItem;
    OpenDialog: TOpenDialog;
    PUPreset: TPopupMenu;
    SaveDialog: TSaveDialog;
{$ENDIF}
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure AsioHostBufferSwitch32(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure AsioHostReset(Sender: TObject);
    procedure BtnSetupClick(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
    procedure CbxPresetChange(Sender: TObject);
    procedure MnuLoadPresetClick(Sender: TObject);
    procedure MnuSavePresetClick(Sender: TObject);
  private
    FVSTInBuffer: array of PDAVSingleFixedArray;
    FVSTOutBuffer: array of PDAVSingleFixedArray;
    FInputChannelOffset: Integer;
    FOutputChannelOffset: Integer;
  public
    property InputChannelOffset: Integer read FInputChannelOffset
      write FInputChannelOffset;
    property OutputChannelOffset: Integer read FOutputChannelOffset
      write FOutputChannelOffset;
  end;

var
  FmVSTEditor: TFmVSTEditor;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  IniFiles, DAV_VSTEffect, EditorSetup;

function EnumNamesFunc(HModule: THandle; LpType, LpName: PChar; LParam: DWORD)
  : Boolean; stdcall;
begin
  Result := True;
  TStringList(LParam).Add(LpName);
end;

procedure TFmVSTEditor.FormCreate(Sender: TObject);
var
  TheRect: TRect;
  I: Integer;
  Str: string;
  S, P: AnsiString;
  ContainedVSTPlugins: TStringList;
  RS: TResourceStream;
{$IFDEF LoadFromMemory}
  FileStream: TFileStream;
{$ENDIF}
begin
  with VstHost[0] do
  begin
    if ParamCount > 0 then
    begin
{$IFDEF LoadFromMemory}
      FileStream := TFileStream.Create(ParamStr(1), FmOpenRead);
      try
        LoadFromStream(FileStream);
      finally
        FreeAndNil(FileStream);
      end;
{$ELSE}
      LoadFromFile(ParamStr(1));
{$ENDIF}
    end
    else
    begin
      ContainedVSTPlugins := TStringList.Create;
      try
{$IFDEF LoadFromResource}
        EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc,
          LongWord(ContainedVSTPlugins));

        if ContainedVSTPlugins.Count > 0 then
        begin
          RS := TResourceStream.Create(HInstance,
            ContainedVSTPlugins[0], 'DLL');
          try
            LoadFromStream(RS);
          finally
            FreeAndNil(RS);
          end;
        end
        else
{$ENDIF}

          if not FileExists(DLLFileName) then
          with TOpenDialog.Create(Self) do
            try
              DefaultExt := 'dll';
              Filter := 'VST Plugin (*.dll)|*.dll';
              Options := Options + [OfFileMustExist];
              if Execute then
                DLLFileName := FileName;

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
    ShowEdit(VSTPanel);
    Idle;
    EditIdle;
    Caption := string(GetVendorString + ' ' + GetEffectName);
  end;
  CbxPreset.Clear;

  for I := 0 to VstHost[0].NumPrograms - 1 do
  begin
    VstHost[0].GetProgramNameIndexed(-1, I, P);
    S := AnsiString(IntToStr(I));
    if I < 10 then
      S := '00' + S
    else if I < 100 then
      S := '0' + S;
    S := S + ' - ' + P;
    CbxPreset.Items.Add(string(S));
  end;
  CbxPreset.ItemIndex := 0;

  Str := string(VstHost[0].GetProgramName);
  Str := IntToStr(CbxPreset.ItemIndex) + ' - ' + Str;
  if CbxPreset.ItemIndex < 10 then
    Str := '00' + Str
  else if CbxPreset.ItemIndex < 100 then
    Str := '0' + Str;
  if (CbxPreset.Text <> Str) then
  begin
    CbxPreset.Text := Str;
    for I := 0 to VstHost[0].NumPrograms - 1 do
    begin
      VstHost[0].CurrentProgram := I;
      Str := string(VstHost[0].GetProgramName);
      Str := IntToStr(I) + ' - ' + Str;
      if I < 10 then
        Str := '00' + Str
      else if I < 100 then
        Str := '0' + Str;
      CbxPreset.Items[I] := Str;
    end;
    VstHost[0].CurrentProgram := 0;
    CbxPreset.ItemIndex := 0;
  end;
  if (EffFlagsHasEditor in VstHost[0].EffectOptions) then
  begin
    TheRect := VstHost[0].GetRect;
    ClientWidth := TheRect.Right - TheRect.Left;
    ClientHeight := TheRect.Bottom - TheRect.Top + ToolBar.Height;
  end;
  SetLength(FVSTInBuffer, 2);
  SetLength(FVSTOutBuffer, 2);
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.INI') do
    try
      Top := ReadInteger('Layout', 'Main Top', Top);
      Left := ReadInteger('Layout', 'Main Left', Left);
    finally
      Free;
    end;
end;

procedure TFmVSTEditor.FormDestroy(Sender: TObject);
var
  Channel: Integer;
begin
  for Channel := 0 to Length(FVSTInBuffer) - 1 do
    Dispose(FVSTInBuffer[Channel]);
  for Channel := 0 to Length(FVSTOutBuffer) - 1 do
    Dispose(FVSTOutBuffer[Channel]);
end;

procedure TFmVSTEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ASIOHost.Active := False;
  VSTHost[0].Active := False;
  Sleep(10);
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

procedure TFmVSTEditor.MnuLoadPresetClick(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
      case FilterIndex of
        1:
          VstHost[0].LoadPreset(FileName);
        2:
          VstHost[0].LoadBank(FileName);
      end;
end;

procedure TFmVSTEditor.MnuSavePresetClick(Sender: TObject);
begin
  with SaveDialog do
    if Execute then
      case FilterIndex of
        1:
          VstHost[0].SavePreset(FileName);
        2:
          VstHost[0].SaveBank(FileName);
      end;
end;

procedure TFmVSTEditor.BtnSetupClick(Sender: TObject);
begin
  FmSetup.Visible := not FmSetup.Visible;
end;

procedure TFmVSTEditor.BtnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFmVSTEditor.CbxPresetChange(Sender: TObject);
begin
  VstHost[0].CurrentProgram := CbxPreset.ItemIndex;
end;

procedure TFmVSTEditor.AsioHostReset(Sender: TObject);
var
  Channel: Integer;
begin
  VSTHost.BlockSize := ASIOHost.BufferSize;
  for Channel := 0 to Length(FVSTInBuffer) - 1 do
    ReallocMem(FVSTInBuffer[Channel], VSTHost.BlockSize * SizeOf(Single));
  for Channel := 0 to Length(FVSTOutBuffer) - 1 do
    ReallocMem(FVSTOutBuffer[Channel], VSTHost.BlockSize * SizeOf(Single));
end;

procedure TFmVSTEditor.AsioHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
begin
  if VSTHost[0].Active then
    VSTHost[0].Process32Replacing(@InBuffer[InputChannelOffset],
      @OutBuffer[OutputChannelOffset], ASIOHost.BufferSize);
end;

end.
