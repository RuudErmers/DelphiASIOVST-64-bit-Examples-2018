unit ASIOMP3GUI;

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

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls, DAV_Types, DAV_AsioHost, 
  DAV_MpegAudio;

type
  TFmASIOMP3 = class(TForm)
    ASIOHost: TASIOHost;
    BtControlPanel: TButton;
    BtStartStop: TButton;
    BtSelect: TButton;
    ChannelBox: TComboBox;
    DriverCombo: TComboBox;
    EdFile: TEdit;
    LbMp3File: TLabel;
    LbChannels: TLabel;
    LbDrivername: TLabel;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BtControlPanelClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure BtSelectClick(Sender: TObject);
    procedure ChannelBoxChange(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure EdFileChange(Sender: TObject);
  private
    FMP3           : TMPEGAudio;
    FVolumeFactor  : Single;
    FChannelOffset : Byte;
  end;

var
  FmASIOMP3: TFmASIOMP3;

implementation

{$R *.dfm}

uses
  IniFiles;

procedure TFmASIOMP3.FormCreate(Sender: TObject);
begin
  DriverCombo.Items := ASIOHost.DriverList;
  if DriverCombo.Items.Count = 0 then
   begin
    MessageDlg('No ASIO Driver present! Application Terminated!',
      mtError, [mbOK], 0);
    Application.Terminate;
   end;

  FVolumeFactor := 1;
  FChannelOffset := 0;

  // and make sure all controls are enabled or disabled
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
   try
    Left := ReadInteger('Layout', 'Audio Left', Left);
    Top := ReadInteger('Layout', 'Audio Top', Top);

    DriverCombo.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
    if DriverCombo.ItemIndex >= 0 then
      DriverComboChange(DriverCombo);
    ChannelBox.ItemIndex := ReadInteger('Audio', 'Channels', 0);
    EdFile.Text := ReadString('Audio', 'MP3 File', EdFile.Text);
    BtStartStop.Enabled := FileExists(EdFile.Text);
   finally
    Free;
   end;
end;

procedure TFmASIOMP3.FormDestroy(Sender: TObject);
begin
  ASIOHost.Active := False;

  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
   try
    WriteInteger('Layout', 'Audio Left', Left);
    WriteInteger('Layout', 'Audio Top', Top);
    WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
    WriteInteger('Audio', 'Channels', ChannelBox.ItemIndex);
    WriteString('Audio', 'MP3 File', EdFile.Text);
   finally
    Free;
   end;

  FreeAndNil(FMP3);
end;

procedure TFmASIOMP3.DriverComboChange(Sender: TObject);
var
  i: Integer;
begin
 BtControlPanel.Enabled := False;
 BtStartStop.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   ChannelBox.Clear;
   for i := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
     ChannelBox.Items.Add(
       ASIOHost.OutputChannelInfos[2 * i].Name + ' / ' +
       ASIOHost.OutputChannelInfos[2 * i + 1].Name);

   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;

   BtControlPanel.Enabled := True;
   BtStartStop.Enabled := FileExists(EdFile.Text);
   ChannelBox.ItemIndex := 0;
  end;
end;

procedure TFmASIOMP3.ChannelBoxChange(Sender: TObject);
begin
 FChannelOffset := ChannelBox.ItemIndex * 2;
end;

procedure TFmASIOMP3.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
begin
 if assigned(FMP3)
  then FMP3.ReadBuffer(OutBuffer[0], OutBuffer[1], ASIOHost.Buffersize)
  else
   begin
    FillChar(OutBuffer[0]^, ASIOHost.Buffersize * SizeOf(Single), 0);
    FillChar(OutBuffer[1]^, ASIOHost.Buffersize * SizeOf(Single), 0);
   end;
end;

procedure TFmASIOMP3.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmASIOMP3.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = 'Start Audio' then
  begin
   ASIOHost.Active := True;
   BtStartStop.Caption := 'Stop Audio';
//  FMP3.Reset;
  end
 else
  begin
   ASIOHost.Active := False;
   if assigned(FMP3) then FMP3.Reset; 
   BtStartStop.Caption := 'Start Audio';
  end;
end;

procedure TFmASIOMP3.BtSelectClick(Sender: TObject);
begin
 if OpenDialog.Execute then EdFile.Text := OpenDialog.FileName;
end;

procedure TFmASIOMP3.EdFileChange(Sender: TObject);
begin
 if assigned(FMP3) then FreeAndNil(FMP3);
 if Fileexists(EdFile.Text) then
  begin
   FMP3 := TMPEGAudio.Create(EdFile.Text);
  end;
end;

end.
