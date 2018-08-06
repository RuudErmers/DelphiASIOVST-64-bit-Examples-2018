unit ButtonTestMain;

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

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF} SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  DAV_GuiPixelMap, DAV_GuiBaseControl, DAV_GuiButton;

type
  TFmButton = class(TForm)
    ButtonA: TGuiButton;
    ButtonB: TGuiButton;
    ButtonC: TGuiButton;
    ButtonD: TGuiButton;
    CbTransparent: TCheckBox;
    TbBorderWidth: TTrackBar;
    TbRadius: TTrackBar;
    LbRadius: TLabel;
    procedure ButtonAClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
    procedure TbBorderWidthChange(Sender: TObject);
    procedure TbRadiusChange(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  end;

var
  FmButton: TFmButton;

implementation

uses
  DAV_GuiCommon;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmButton.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmButton.ButtonAClick(Sender: TObject);
begin
 CbTransparent.Checked := not CbTransparent.Checked;
end;

procedure TFmButton.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmButton.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmButton.FormResize(Sender: TObject);
var
  x, y   : Integer;
  Filter : array [0..1] of Single;
  h, hr  : Single;
  ScnLne : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLne := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       Filter[1] := 0.97 * Filter[0] + 0.03 * Random;
       Filter[0] := Filter[1];

       ScnLne[x].B := Round($70 - $34 * (Filter[1] - h));
       ScnLne[x].G := Round($84 - $48 * (Filter[1] - h));
       ScnLne[x].R := Round($8D - $50 * (Filter[1] - h));
      end;
    end;
  end;
end;

procedure TFmButton.TbBorderWidthChange(Sender: TObject);
begin
 ButtonA.BorderWidth := 0.25 *(3 + TbBorderWidth.Position);
 ButtonB.BorderWidth := 0.25 *(3 + TbBorderWidth.Position);
 ButtonC.BorderWidth := 0.25 *(3 + TbBorderWidth.Position);
 ButtonD.BorderWidth := 0.25 *(3 + TbBorderWidth.Position);
end;

procedure TFmButton.CbTransparentClick(Sender: TObject);
begin
 ButtonA.Transparent := CbTransparent.Checked;
 ButtonB.Transparent := CbTransparent.Checked;
 ButtonC.Transparent := CbTransparent.Checked;
 ButtonD.Transparent := CbTransparent.Checked;
end;

procedure TFmButton.TbRadiusChange(Sender: TObject);
begin
 ButtonA.BorderRadius := 0.5 * TbRadius.Position;
 ButtonB.BorderRadius := 0.5 * TbRadius.Position;
 ButtonC.BorderRadius := 0.5 * TbRadius.Position;
 ButtonD.BorderRadius := 0.5 * TbRadius.Position;
end;

end.
