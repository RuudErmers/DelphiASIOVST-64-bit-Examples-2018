unit EditorFrm;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, Controls, StdCtrls, DAV_Types, DAV_VSTModule;

type
  TEditorForm = class(TForm)
    LbThreshold: TLabel;
    ScrollBar: TScrollBar;
    LbdB: TLabel;
    procedure ScrollBarChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateScrollBar;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  SimpleGateDM;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateScrollBar;
end;

procedure TEditorForm.ScrollBarChange(Sender: TObject);
begin
 TSimpleGateDataModule(Owner).Parameter[0] := ScrollBar.Position;
 LbdB.Caption := IntToStr(ScrollBar.Position) + ' dB';
end;

procedure TEditorForm.UpdateScrollBar;
begin
 with Owner as TSimpleGateDataModule do
  begin
   if Round(Parameter[0]) <> ScrollBar.Position
    then ScrollBar.Position := Round(Parameter[0]);
   LbdB.Caption := IntToStr(ScrollBar.Position) + ' dB';
  end;
end;

end.
