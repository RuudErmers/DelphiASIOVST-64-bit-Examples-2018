unit TetrisDM;

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
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, {$ENDIF}
  SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule, DAV_VSTEffect;

type
  TTetrisModule = class(TVSTModule)
    function VSTModuleCheckKey(Sender: TObject; Key: Char): Boolean;
    procedure VSTModuleEditorKeyDown(Sender: TObject; var keyCode: TVstKeyCode);
    procedure VSTModuleCreate(Sender: TObject);
  private
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  TetrisEditor;

function TTetrisModule.VSTModuleCheckKey(Sender: TObject; Key: Char): Boolean;
begin
 result := True;
end;

procedure TTetrisModule.VSTModuleCreate(Sender: TObject);
begin
 EditorFormClass := TFmTetris;
end;

procedure TTetrisModule.VSTModuleEditorKeyDown(Sender: TObject;
  var keyCode: TVstKeyCode);
begin
 if Assigned(EditorForm) then
  with (EditorForm As TFmTetris) do
   begin
    case keyCode.Character of
     VKEY_SPACE : Tetris.StepGame;
     VKEY_LEFT : Tetris.Left;
     VKEY_right : Tetris.right;
     VKEY_up : Tetris.Rotate;
     VKEY_DOWN : Tetris.StepGame;
    end;
    FormPaint(nil);
   end;
end;

end.
