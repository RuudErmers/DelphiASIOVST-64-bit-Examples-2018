unit AsioBufferdAudioFilePlayerGUI;

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
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, DAV_Types, 
  DAV_ASIOHost, DAV_DspBufferedAudioFilePlayer, DAV_AudioFile, 
  DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU;

type
  TFmAsioBufferdAudioFilePlayer = class(TForm)
    ASIOHost: TASIOHost;
    BtControlPanel: TButton;
    BtStartStop: TButton;
    BtSelect: TButton;
    ChannelBox: TComboBox;
    DriverCombo: TComboBox;
    EdFile: TEdit;
    LbAudioFile: TLabel;
    LbChannels: TLabel;
    LbDrivername: TLabel;
    OpenDialog: TOpenDialog;
    LbBuffer: TLabel;
    LbBufferValue: TLabel;
    Timer: TTimer;
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
    FBufferedPlayer : TBufferedAudioFilePlayer;
  end;

var
  FmAsioBufferdAudioFilePlayer: TFmAsioBufferdAudioFilePlayer;

implementation

{$R *.dfm}

uses
  IniFiles;

resourcestring
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

{ TFmAsioBufferdAudioFilePlayer }

procedure TFmAsioBufferdAudioFilePlayer.FormCreate(Sender: TObject);
begin
 FIniFile := ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI';
 DriverCombo.Items := ASIOHost.DriverList;
 if DriverCombo.Items.Count = 0 then
  begin
   MessageDlg(RCStrNoASIODriverPresent,
     mtError, [mbOK], 0);
   Application.Terminate;
  end;

 FVolumeFactor := 1;
 FChannelOffset := 0;
 FBufferedPlayer := TBufferedAudioFilePlayer.Create;
 FBufferedPlayer.Pitch := 0;
 FBufferedPlayer.Interpolation := biBSpline6Point5thOrder;
 with FBufferedPlayer do
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
   EdFile.Text := ReadString('Audio', 'Audio File', EdFile.Text);
   BtStartStop.Enabled := FileExists(EdFile.Text);
  finally
   Free;
  end;
end;

procedure TFmAsioBufferdAudioFilePlayer.FormDestroy(Sender: TObject);
begin
  ASIOHost.Active := False;
  FreeAndNil(FBufferedPlayer);

  with TIniFile.Create(FIniFile) do
   try
    WriteInteger('Layout', 'Audio Left', Left);
    WriteInteger('Layout', 'Audio Top', Top);
    WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
    WriteInteger('Audio', 'Channels', ChannelBox.ItemIndex);
    WriteString('Audio', 'Audio File', EdFile.Text);
   finally
    Free;
   end;
end;

procedure TFmAsioBufferdAudioFilePlayer.LbBufferClick(Sender: TObject);
begin
 ASIOHost.SampleRate := 48000;
end;

procedure TFmAsioBufferdAudioFilePlayer.TimerTimer(Sender: TObject);
begin
 LbBufferValue.Caption := IntToStr(Round(FBufferedPlayer.BufferFill)) + ' %';
end;

procedure TFmAsioBufferdAudioFilePlayer.DriverComboChange(Sender: TObject);
var
  ChannelPairIndex: Integer;
begin
 BtControlPanel.Enabled := False;
 BtStartStop.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   ChannelBox.Clear;
   for ChannelPairIndex := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
     ChannelBox.Items.Add(string(
       ASIOHost.OutputChannelInfos[2 * ChannelPairIndex].Name + ' / ' +
       ASIOHost.OutputChannelInfos[2 * ChannelPairIndex + 1].Name));

   with TIniFile.Create(FIniFile) do
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

procedure TFmAsioBufferdAudioFilePlayer.ChannelBoxChange(Sender: TObject);
begin
 FChannelOffset := ChannelBox.ItemIndex * 2;
end;

procedure TFmAsioBufferdAudioFilePlayer.ASIOHostSampleRateChanged(Sender: TObject);
begin
 if assigned(FBufferedPlayer)
  then FBufferedPlayer.SampleRate := ASIOHost.SampleRate;
end;

procedure TFmAsioBufferdAudioFilePlayer.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmAsioBufferdAudioFilePlayer.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = '&Start Audio' then
  begin
   ASIOHost.Active := True;
   BtStartStop.Caption := '&Stop Audio';
  end
 else
  begin
   ASIOHost.Active := False;
   FBufferedPlayer.Reset;
   BtStartStop.Caption := '&Start Audio';
  end;
end;

procedure TFmAsioBufferdAudioFilePlayer.BtSelectClick(Sender: TObject);
begin
 if OpenDialog.Execute then EdFile.Text := OpenDialog.FileName;
end;

procedure TFmAsioBufferdAudioFilePlayer.EdFileChange(Sender: TObject);
begin
 FBufferedPlayer.Filename := EdFile.Text;
 BtStartStop.Enabled := FileExists(EdFile.Text);
end;

procedure TFmAsioBufferdAudioFilePlayer.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
begin
// FBufferedPlayer.GetSamples(OutBuffer[0], OutBuffer[1], ASIOHost.Buffersize);
 FBufferedPlayer.GetSamples(OutBuffer, ASIOHost.Buffersize);
end;

end.
