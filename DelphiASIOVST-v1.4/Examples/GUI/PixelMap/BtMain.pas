unit BtMain;

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
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Menus,
  DAV_GuiPixelMap, DAV_GuiFilters;

{$DEFINE DIB}

type
  TFmPixelMapTest = class(TForm)
    MainMenu: TMainMenu;
    MiBasicMix: TMenuItem;
    MiClear: TMenuItem;
    MiCountTest: TMenuItem;
    MiEdit: TMenuItem;
    MiExit: TMenuItem;
    MiFile: TMenuItem;
    MiFillRect: TMenuItem;
    MiFrameRectangles: TMenuItem;
    MiLineCircle: TMenuItem;
    MiRender: TMenuItem;
    MiSave: TMenuItem;
    MiTests: TMenuItem;
    MiTurnClockwise: TMenuItem;
    MiTurnCounterclockwise: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    PaintBox: TPaintBox;
    MiFilter: TMenuItem;
    MiBoxBlur: TMenuItem;
    MiSaturation: TMenuItem;
    N4: TMenuItem;
    MiResize: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure MiBasicMixClick(Sender: TObject);
    procedure MiSaveClick(Sender: TObject);
    procedure MiLineCircleClick(Sender: TObject);
    procedure MiFillRectClick(Sender: TObject);
    procedure MiClearClick(Sender: TObject);
    procedure MiFrameRectanglesClick(Sender: TObject);
    procedure MiTurnClockwiseClick(Sender: TObject);
    procedure MiTurnCounterclockwiseClick(Sender: TObject);
    procedure MiCountTestClick(Sender: TObject);
    procedure MiBoxBlurClick(Sender: TObject);
    procedure MiSaturationClick(Sender: TObject);
    procedure MiResizeClick(Sender: TObject);
  protected
    {$IFDEF DIB}
    FGuiBitmap : TGuiPixelMapDIB;
    {$ELSE}
    FGuiBitmap : TGuiPixelMapMemory;
    {$ENDIF}
  public
    procedure ClearBitmap;
    procedure RenderFrameRectBitmap;
    procedure RenderFillRectBitmap;
    procedure RenderLineBitmap;
    procedure RenderLineCircleBitmap;
  end;

var
  FmPixelMapTest: TFmPixelMapTest;

implementation

uses
  Math, DAV_Common, DAV_Math, DAV_GuiCommon;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmPixelMapTest.FormCreate(Sender: TObject);
begin
 {$IFDEF DIB}
 FGuiBitmap := TGuiPixelMapDIB.Create;
 {$ELSE}
 FGuiBitmap := TGuiPixelMapMemory.Create;
 {$ENDIF}

 with FGuiBitmap do
  begin
   Width := PaintBox.Width;
   Height := PaintBox.Height;
  end;

 PaintBox.ControlStyle := PaintBox.ControlStyle + [csOpaque];
 {$IFDEF FPC}
 ControlStyle := ControlStyle + [csOpaque];
 DoubleBuffered := True;
 {$ENDIF}
end;

procedure TFmPixelMapTest.FormDblClick(Sender: TObject);
begin
 ClientHeight := PaintBox.Width + PaintBox.Top + 8;
end;

procedure TFmPixelMapTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FGuiBitmap);
end;

procedure TFmPixelMapTest.FormResize(Sender: TObject);
begin
 if Assigned(FGuiBitmap) then
  begin
   FGuiBitmap.SetSize(PaintBox.Width, PaintBox.Height);
   FGuiBitmap.Clear;
  end;
end;

procedure TFmPixelMapTest.MiFrameRectanglesClick(Sender: TObject);
begin
 RenderFrameRectBitmap;
 PaintBox.Invalidate;
end;

procedure TFmPixelMapTest.MiLineCircleClick(Sender: TObject);
begin
 RenderLineCircleBitmap;
 PaintBox.Invalidate;
end;

procedure TFmPixelMapTest.MiResizeClick(Sender: TObject);
begin
 with FGuiBitmap
  do Resize((2 * Width) div 3, Height);

 PaintBox.Invalidate;
end;

procedure TFmPixelMapTest.MiBasicMixClick(Sender: TObject);
begin
 ClearBitmap;
 RenderLineCircleBitmap;
 RenderFillRectBitmap;
 RenderFrameRectBitmap;
 PaintBox.Invalidate;
end;

procedure TFmPixelMapTest.MiBoxBlurClick(Sender: TObject);
begin
 with TGuiStackBlurFilter.Create do
  try
   Radius := 5;
   Filter(FGuiBitmap);
  finally
   Free;
  end;
 PaintBox.Invalidate;
end;

procedure TFmPixelMapTest.MiClearClick(Sender: TObject);
begin
 ClearBitmap;
 PaintBox.Invalidate;
end;

procedure TFmPixelMapTest.MiCountTestClick(Sender: TObject);
var
  i : Integer;
begin
 i := 0;
 while i < 1 shl 16 do
  try
   {$IFDEF DIB}
   with TGuiPixelMapDIB.Create do
   {$ELSE}
   with TGuiPixelMapMemory.Create do
   {$ENDIF}
    begin
     Width := 10;
     Height := 10;
    end;
   Inc(i);
  except
   raise Exception.CreateFmt('Only %d bitmaps created', [i]);
  end;
end;

procedure TFmPixelMapTest.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmPixelMapTest.MiFillRectClick(Sender: TObject);
begin
 RenderFillRectBitmap;
 PaintBox.Invalidate;
end;

procedure TFmPixelMapTest.MiSaturationClick(Sender: TObject);
begin
 with TGuiSaturationFilter.Create do
  try
   Value := 1;
   Filter(FGuiBitmap);
  finally
   Free;
  end;
 PaintBox.Invalidate;
end;

procedure TFmPixelMapTest.MiSaveClick(Sender: TObject);
begin
 FGuiBitmap.SaveToFile('Test.bmp');
end;

procedure TFmPixelMapTest.PaintBoxPaint(Sender: TObject);
begin
 if Assigned(FGuiBitmap)
  then FGuiBitmap.PaintTo(PaintBox.Canvas);

 with PaintBox do
  begin
   if Width > FGuiBitmap.Width
    then Canvas.FillRect(Rect(FGuiBitmap.Width, 0, Width, Height));

   if Height > FGuiBitmap.Height
    then Canvas.FillRect(Rect(0, FGuiBitmap.Height, Width, Height));
  end;
end;

procedure TFmPixelMapTest.RenderFillRectBitmap;
var
  Color : TPixel32;
  Level : Integer;
  X, Y  : Integer;
begin
 with FGuiBitmap do
  for Level := 0 to 10 do
   begin
    Color.R := Random($FF);
    Color.G := Random($FF);
    Color.B := Random($FF);
    Color.A := Random($FF);

    X := Random(Width);
    Y := Random(Height);
    FillRect(Rect(X, Y, X + Random(Width - 1 - X), Y + Random(Height - 1 - Y)), Color);
   end;
end;

procedure TFmPixelMapTest.RenderFrameRectBitmap;
var
  Color : TPixel32;
  Level : Integer;
  X, Y  : Integer;
begin
 with FGuiBitmap do
  for Level := 0 to 10 do
   begin
    Color.R := Random($FF);
    Color.G := Random($FF);
    Color.B := Random($FF);
    Color.A := Random($FF);

    X := Random(Width);
    Y := Random(Height);
    FrameRect(Rect(X, Y, X + Random(Width - X), Y + Random(Height - Y)), Color);
   end;
end;

procedure TFmPixelMapTest.RenderLineBitmap;
var
  Color : TPixel32;
  Level : Integer;
begin
 with FGuiBitmap do
  for Level := 0 to 10 do
   begin
    Color.R := Random($FF);
    Color.G := Random($FF);
    Color.B := Random($FF);
    Color.A := Random($FF);

    Line(Random(Width), Random(Height), Random(Width), Random(Height), Color);
   end;
end;

procedure TFmPixelMapTest.RenderLineCircleBitmap;
var
  Color  : TPixel32;
  Level  : Integer;
  Index  : Integer;
  Center : TPoint;
  X, Y   : Single;
begin
 with FGuiBitmap do
  for Level := 0 to 10 do
   begin
    Center.X := Width div 2;
    Center.Y := Height div 2;
    for Index := 0 to 35 do
     begin
      Color.R := Random($FF);
      Color.G := Random($FF);
      Color.B := Random($FF);
      Color.A := Random($FF);

      GetSinCos(Pi * Index / 18, X, Y);
      Line(Center.X, Center.Y, Round(Center.X * (1 + 0.9 * X )),
        Round(Center.Y * (1 + 0.9 * Y)), Color);
     end;
   end;
end;

procedure TFmPixelMapTest.MiTurnCounterclockwiseClick(Sender: TObject);
begin
 FGuiBitmap.Turn(True);
 PaintBox.Invalidate;
end;

procedure TFmPixelMapTest.MiTurnClockwiseClick(Sender: TObject);
begin
 FGuiBitmap.Turn;
 PaintBox.Invalidate;
end;

procedure TFmPixelMapTest.ClearBitmap;
begin
 FGuiBitmap.Clear;
end;

end.
