unit AbxAudio;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TFmAudioSettings = class(TForm)
    LbPreset: TLabel;
    CBDrivers: TComboBox;
    LbChannels: TLabel;
    CBChannels: TComboBox;
    BtControlPanel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBDriversChange(Sender: TObject);
    procedure CBChannelsChange(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
  end;

var
  FmAudioSettings: TFmAudioSettings;

implementation

{$R *.dfm}

uses
  IniFiles, DAV_AsioHost, AbxMain;

procedure TFmAudioSettings.FormCreate(Sender: TObject);
var
  i : Integer;
begin
 CBDrivers.Items := FmAbxAlgorithmTest.ASIOHost.DriverList;
 with TIniFile.Create(FmAbxAlgorithmTest.IniFileName) do
  try
   CBDrivers.ItemIndex := ReadInteger('Setup', 'ASIO Driver', CBDrivers.ItemIndex);
   if CBDrivers.ItemIndex = -1
    then CBDrivers.ItemIndex := CBDrivers.Items.IndexOf('ASIO4ALL v2') else
   if CBDrivers.ItemIndex = -1
    then CBDrivers.ItemIndex := CBDrivers.Items.IndexOf('ASIO4ALL');
   CBDriversChange(Self);

   // add channel list
   CBChannels.Clear;
   with FmAbxAlgorithmTest.ASIOHost do
    for i := 0 to OutputChannelCount div 2 - 1
     do CBChannels.Items.Add(OutputChannelInfos[I].Name + ' / ' +
                             OutputChannelInfos[I + 1].Name);

   CBChannels.ItemIndex := ReadInteger('Setup', 'ASIO Channels', 0);
  finally
   Free;
  end;
end;

procedure TFmAudioSettings.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(FmAbxAlgorithmTest.IniFileName) do
  try
   WriteInteger('Setup', 'ASIO Driver', CBDrivers.ItemIndex);
   WriteInteger('Setup', 'ASIO Channels', CBChannels.ItemIndex);
  finally
   Free;
  end;
end;

procedure TFmAudioSettings.FormShow(Sender: TObject);
begin
 with TIniFile.Create(FmAbxAlgorithmTest.IniFileName) do
  try
   Top := ReadInteger('Layout', 'Audio Settings Top', Top);
   Left := ReadInteger('Layout', 'Audio Settings Left', Left);
  finally
   Free;
  end;
end;

procedure TFmAudioSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TIniFile.Create(FmAbxAlgorithmTest.IniFileName) do
  try
   WriteInteger('Layout', 'Audio Settings Top', Top);
   WriteInteger('Layout', 'Audio Settings Left', Left);
  finally
   Free;
  end;
end;

procedure TFmAudioSettings.BtControlPanelClick(Sender: TObject);
begin
 FmAbxAlgorithmTest.ASIOHost.ControlPanel;
end;

procedure TFmAudioSettings.CBChannelsChange(Sender: TObject);
begin
 FmAbxAlgorithmTest.ChannelOffset := 2 * CBChannels.ItemIndex;
end;

procedure TFmAudioSettings.CBDriversChange(Sender: TObject);
begin
 with FmAbxAlgorithmTest.ASIOHost do
  if CBDrivers.ItemIndex >= 0 then
   begin
    DriverIndex := CBDrivers.ItemIndex;
    if assigned(OnReset) then OnReset(Self);
   end;
end;

end.
