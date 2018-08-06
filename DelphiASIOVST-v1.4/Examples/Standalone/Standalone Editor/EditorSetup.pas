unit EditorSetup;

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

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls;

type
  TFmSetup = class(TForm)
    LbAsioDriver: TLabel;
    LbInput: TLabel;
    LbOutput: TLabel;
    CBDrivers: TComboBox;
    CBInput: TComboBox;
    CBOutput: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBDriversChange(Sender: TObject);
    procedure CBInputChange(Sender: TObject);
    procedure CBOutputChange(Sender: TObject);
    procedure LbAsioDriverClick(Sender: TObject);
  end;

var
  FmSetup: TFmSetup;

implementation

uses
  IniFiles, EditorForm;

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TFmSetup.FormCreate(Sender: TObject);
begin
 CBDrivers.Items := FmVSTEditor.ASIOHost.DriverList;
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.INI') do
  try
   Top := ReadInteger('Layout', 'Setup Top', Top);
   Left := ReadInteger('Layout', 'Setup Left', Left);
   CBDrivers.ItemIndex := ReadInteger('Setup', 'ASIO Driver', CBDrivers.ItemIndex);
   CBDriversChange(Self);
  finally
   Free;
  end;
end;

procedure TFmSetup.CBDriversChange(Sender: TObject);
var
  i : Integer;
begin
 with FmVSTEditor.ASIOHost do
  if CBDrivers.ItemIndex >= 0 then
   begin
    Active := False;
    DriverIndex := CBDrivers.ItemIndex;
    CBInput.Clear;
    for i := 0 to (InputChannelCount div 2) - 1 do
     begin
      CBInput.Items.Add(string(
        AnsiString(InputChannelInfos[2 * i].name) + ' / ' +
        AnsiString(InputChannelInfos[2 * i + 1].name)));
     end;
    CBOutput.Clear;
    for i := 0 to (OutputChannelCount div 2) - 1 do
     begin
      CBOutput.Items.Add(string(
        AnsiString(OutputChannelInfos[2 * i].name) + ' / ' +
        AnsiString(OutputChannelInfos[2 * i + 1].name)));
     end;
    CBInput.ItemIndex := 0;
    CBOutput.ItemIndex := 0;
    if Assigned(OnReset)
     then OnReset(Self);
    Active := True;
   end;
end;

procedure TFmSetup.CBInputChange(Sender: TObject);
begin
 FmVSTEditor.InputChannelOffset := CBInput.ItemIndex * 2;
end;

procedure TFmSetup.CBOutputChange(Sender: TObject);
begin
 FmVSTEditor.OutputChannelOffset := CBOutput.ItemIndex * 2;
end;

procedure TFmSetup.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.INI') do
  try
   WriteInteger('Layout', 'Setup Top', Top);
   WriteInteger('Layout', 'Setup Left', Left);
   WriteInteger('Setup', 'ASIO Driver', CBDrivers.ItemIndex);
  finally
   Free;
  end;
end;

procedure TFmSetup.LbAsioDriverClick(Sender: TObject);
begin
 FmVSTEditor.AsioHost.ControlPanel;
end;

{$IFDEF FPC}
initialization
  {$i EditorSetup.lrs}
{$ENDIF}

end.

