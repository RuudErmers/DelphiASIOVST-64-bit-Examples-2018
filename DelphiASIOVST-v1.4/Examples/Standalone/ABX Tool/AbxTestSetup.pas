unit AbxTestSetup;

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
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, 
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Spin;

type
  TFmTestSetup = class(TForm)
    BtCancel: TButton;
    BtGo: TButton;
    CBAllowNavigation: TCheckBox;
    EdNameID: TEdit;
    LbNameID: TLabel;
    LbNumberOfTrials: TLabel;
    SENumberOfTrials: TSpinEdit;
    procedure BtGoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

implementation

uses
  IniFiles, AbxTest, AbxMain;

{$R *.dfm}

procedure TFmTestSetup.BtGoClick(Sender: TObject);
begin
 with TFmAbxTest.Create(Self) do
  try
   TrialCount := SENumberOfTrials.Value;
   NameID := EdNameID.Text;
   ShowModal;
  finally
   Free; 
  end;
end;

function GetUserName: string;
var
  UserName  : array [0..255] of Char;
  dw        : DWord;
begin
 dw := SizeOf(UserName);
 Windows.GetUserName(@UserName, dw);
 result := Username;
end;

procedure TFmTestSetup.FormCreate(Sender: TObject);
begin
 with TIniFile.Create(FmAbxMain.IniFileName) do
  try
   if not ValueExists('Test Setup', 'Name/ID') then
    try
     EdNameID.Text := GetUserName;
    except
    end;
   SENumberOfTrials.Value := ReadInteger('Test Setup', 'Number of Trials', SENumberOfTrials.Value);
   EdNameID.Text := ReadString('Test Setup', 'Name/ID', EdNameID.Text);
   CBAllowNavigation.Checked := ReadBool('Test Setup', 'Allow Navigation', CBAllowNavigation.Checked);
  finally
   Free;
  end;
end;

procedure TFmTestSetup.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(FmAbxMain.IniFileName) do
  try
   WriteInteger('Test Setup', 'Number of Trials', SENumberOfTrials.Value);
   WriteString('Test Setup', 'Name/ID', EdNameID.Text);
   WriteBool('Test Setup', 'Allow Navigation', CBAllowNavigation.Checked);
  finally
   Free;
  end;
end;

end.
