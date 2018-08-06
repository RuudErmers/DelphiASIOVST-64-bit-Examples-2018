unit TetrisEditor;

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
  {$IFDEF FPC}LCLIntf, LResources, LCLType, {$ELSE} Windows, Types, {$ENDIF}
  SysUtils, Classes, Forms, TetrisUnit, Controls, StdCtrls, ExtCtrls, Graphics,
  DAV_Types, DAV_VSTModule;

type
  TFmTetris = class(TForm)
    TetrisTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TetrisOnTimer(Sender: TObject);
  private
    fBitmap: TBitmap;
    fTetris: TTetris;
  public
    property Tetris : TTetris read fTetris;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmTetris.FormCreate(Sender: TObject);
begin
 fTetris := TTetris.Create;
 fBitmap := TBitmap.Create;
end;

procedure TFmTetris.FormDestroy(Sender: TObject);
begin
 FreeAndNil(fTetris);
 FreeAndNil(fBitmap);
end;

procedure TFmTetris.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if key = VK_SPACE then fTetris.StepGame;
 if key  = VK_LEFT then fTetris.Left;
  if key  = VK_right then fTetris.right;
   if key  = VK_up then fTetris.Rotate;
    if key  = VK_DOWN then fTetris.StepGame;
 FormPaint(nil);
end;

procedure TFmTetris.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssleft in shift then fTetris.Left;
  if ssright in shift then fTetris.right;
  if ssMiddle in shift then fTetris.Rotate;
  FormPaint(nil);
end;

procedure TFmTetris.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 fTetris.Rotate;
 FormPaint(nil);
end;

procedure TFmTetris.FormPaint(Sender: TObject);
begin
 fTetris.DefaultBitmap(fBitmap);
 Canvas.StretchDraw(clientrect, fBitmap);
 Caption := 'lines ' + inttostr(fTetris.Lines);
end;

procedure TFmTetris.TetrisOnTimer(Sender: TObject);
begin
  fTetris.StepGame;
  FormPaint(nil);
  TetrisTimer.Interval:=TrimInt(1000 - fTetris.lines*10,100,1000);
end;

end.
