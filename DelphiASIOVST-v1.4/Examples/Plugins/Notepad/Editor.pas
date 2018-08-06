unit Editor;

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

{$I DAV_Compiler.inc}

interface

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, Controls, StdCtrls, DAV_Types, DAV_VSTModule;

type
  TFmNotepad = class(TForm)
    MeNotepad: TMemo;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MeNotepadChange(Sender: TObject);
  end;

implementation

uses
  PluginDM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmNotepad.FormShow(Sender: TObject);
begin
 if Owner is TPluginDataModule then
  with TPluginDataModule(Owner)
   do MeNotepad.Text := Text;
end;

procedure TFmNotepad.MeNotepadChange(Sender: TObject);
begin
 if Owner is TPluginDataModule then
  with TPluginDataModule(Owner)
   do Text := MeNotepad.Text;
end;

procedure TFmNotepad.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if Owner is TPluginDataModule then
  with TPluginDataModule(Owner)
   do Text := MeNotepad.Text;
end;

end.
