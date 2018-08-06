unit SarMain;

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
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, DAV_Types, 
  DAV_DspBufferedAudioFileRecorder, DAV_ASIOHost, DAV_AudioFile, 
  DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU;

type
  TFmRecordAudio = class(TForm)
    ASIOHost: TASIOHost;
    BtControlPanel: TButton;
    BtSelect: TButton;
    BtStartStop: TButton;
    ChannelBox: TComboBox;
    DriverCombo: TComboBox;
    EdFile: TEdit;
    LbBuffer: TLabel;
    LbBufferValue: TLabel;
    LbChannels: TLabel;
    LbDrivername: TLabel;
    LbRecordedFile: TLabel;
    Timer: TTimer;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure BtSelectClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure ChannelBoxChange(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure EdFileChange(Sender: TObject);
    procedure LbBufferClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FIniFile        : TFileName;
    FVolumeFactor   : Single;
    FChannelOffset  : Byte;
    FBufferedRecorder : TBufferedAudioFileRecorder;
  end;

var
  FmRecordAudio: TFmRecordAudio;

implementation

{$R *.dfm}

uses
  IniFiles;

resourcestring
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

{ TFmASIOMP3 }

procedure TFmRecordAudio.FormCreate(Sender: TObject);
begin
 FIniFile := ExtractFilePath(ParamStr(0)) + 'SimpleAsioRecorder.INI';
 DriverCombo.Items := ASIOHost.DriverList;
 if DriverCombo.Items.Count = 0 then
  begin
   MessageDlg(RCStrNoASIODriverPresent, mtError, [mbOK], 0);
   Application.Terminate;
  end;

 FVolumeFactor := 1;
 FChannelOffset := 0;

 FBufferedRecorder := TBufferedAudioFileRecorder.Create;
 with FBufferedRecorder do
  begin
   BufferSize := 65536;
   BlockSize  := 4096
  end;

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(FIniFile) do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);

   DriverCombo.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if DriverCombo.ItemIndex >= 0 then
     DriverComboChange(DriverCombo);
   ChannelBox.ItemIndex := ReadInteger('Audio', 'Channels', 0);
   EdFile.Text := ReadString('Audio', 'File', EdFile.Text);
   EdFileChange(Self);
   BtStartStop.Enabled := (EdFile.Text <> '') and (DriverCombo.ItemIndex >= 0);
  finally
   Free;
  end;
end;

procedure TFmRecordAudio.FormDestroy(Sender: TObject);
begin
  with TIniFile.Create(FIniFile) do
   try
    WriteInteger('Layout', 'Audio Left', Left);
    WriteInteger('Layout', 'Audio Top', Top);
    WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
    WriteInteger('Audio', 'Channels', ChannelBox.ItemIndex);
    WriteString('Audio', 'File', EdFile.Text);
   finally
    Free;
   end;

  ASIOHost.Active := False;
  FreeAndNil(FBufferedRecorder);
end;

procedure TFmRecordAudio.LbBufferClick(Sender: TObject);
begin
 ASIOHost.SampleRate := 48000;
end;

procedure TFmRecordAudio.TimerTimer(Sender: TObject);
begin
 LbBufferValue.Caption := IntToStr(Round(FBufferedRecorder.BufferFill)) + ' %';
end;

procedure TFmRecordAudio.DriverComboChange(Sender: TObject);
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
   for i := 0 to (ASIOHost.InputChannelCount) - 1
    do ChannelBox.Items.Add(ASIOHost.InputChannelInfos[i].Name);

   with TIniFile.Create(FIniFile) do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;

   BtControlPanel.Enabled := True;
   BtStartStop.Enabled := EdFile.Text <> '';
   ChannelBox.ItemIndex := 0;
  end;
end;

procedure TFmRecordAudio.ChannelBoxChange(Sender: TObject);
begin
 FChannelOffset := ChannelBox.ItemIndex * 2;
end;

procedure TFmRecordAudio.ASIOHostSampleRateChanged(Sender: TObject);
begin
 if assigned(FBufferedRecorder)
  then FBufferedRecorder.SampleRate := ASIOHost.SampleRate;
end;

procedure TFmRecordAudio.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmRecordAudio.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = '&Record Audio' then
  begin
   ASIOHost.Active := True;
   BtStartStop.Caption := '&Stop Audio';
  end
 else
  begin
   ASIOHost.Active := False;
   FBufferedRecorder.Reset;
   BtStartStop.Caption := '&Record Audio';
  end;
end;

procedure TFmRecordAudio.BtSelectClick(Sender: TObject);
begin
 if SaveDialog.Execute
  then EdFile.Text := SaveDialog.FileName;
end;

procedure TFmRecordAudio.EdFileChange(Sender: TObject);
begin
 DeleteFile(EdFile.Text);
 FBufferedRecorder.Filename := EdFile.Text;
 BtStartStop.Enabled := (FBufferedRecorder.Filename <> '') and (DriverCombo.ItemIndex >= 0);
end;

procedure TFmRecordAudio.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
begin
 FBufferedRecorder.PutSamples(InBuffer[0], ASIOHost.Buffersize);
end;

end.
