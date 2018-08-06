unit AbxStandaloneAudioSetup;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2005-2011        //
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
    LbSettings: TLabel;
    MemoSettings: TMemo;
    BtControlPanel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBDriversChange(Sender: TObject);
  end;

var
  FmAudioSettings: TFmAudioSettings;

implementation

{$R *.dfm}

uses
  AbxStandaloneTest, IniFiles;

procedure TFmAudioSettings.FormCreate(Sender: TObject);
begin
 CBDrivers.Items := FmABXStandaloneTest.ASIOHost.DriverList;
 with TIniFile.Create(FmABXStandaloneTest.IniFileName) do
  try
   CBDrivers.ItemIndex := ReadInteger('Setup', 'ASIO Driver', CBDrivers.ItemIndex);
   if CBDrivers.ItemIndex = -1
    then CBDrivers.ItemIndex := CBDrivers.Items.IndexOf('ASIO4ALL v2') else
   if CBDrivers.ItemIndex = -1
    then CBDrivers.ItemIndex := CBDrivers.Items.IndexOf('ASIO4ALL');
   CBDriversChange(Self);
  finally
   Free;
  end;
end;

procedure TFmAudioSettings.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(FmABXStandaloneTest.IniFileName) do
  try
   WriteInteger('Setup', 'ASIO Driver', CBDrivers.ItemIndex);
  finally
   Free;
  end;
end;

procedure TFmAudioSettings.FormShow(Sender: TObject);
begin
 with TIniFile.Create(FmABXStandaloneTest.IniFileName) do
  try
   Top := ReadInteger('Layout', 'Audio Settings Top', Top);
   Left := ReadInteger('Layout', 'Audio Settings Left', Left);
  finally
   Free;
  end;
end;

procedure TFmAudioSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TIniFile.Create(FmABXStandaloneTest.IniFileName) do
  try
   WriteInteger('Layout', 'Audio Settings Top', Top);
   WriteInteger('Layout', 'Audio Settings Left', Left);
  finally
   Free;
  end;
end;

procedure TFmAudioSettings.CBDriversChange(Sender: TObject);
begin
 with FmABXStandaloneTest.ASIOHost do
  if CBDrivers.ItemIndex >= 0 then
   begin
    Active := False;
    DriverIndex := CBDrivers.ItemIndex;
    if assigned(OnReset) then OnReset(Self);
   end;
end;

end.
