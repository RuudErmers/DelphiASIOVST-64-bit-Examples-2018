unit LedTestMain;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls,
  DAV_GuiBaseControl, DAV_GuiLED, DAV_GuiPixelMap;

type
  TFmLEDTest = class(TForm)
    LED1: TGuiLED;
    LED2: TGuiLED;
    LED3: TGuiLED;
    LED4: TGuiLED;
    LbUniformiy: TLabel;
    TbUniformity: TTrackBar;
    LbBrightness: TLabel;
    TbBrightness: TTrackBar;
    LbLineWidth: TLabel;
    TbLineWidth: TTrackBar;
    LbBorderStrength: TLabel;
    TbBorderStrength: TTrackBar;
    CbTransparent: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TbUniformityChange(Sender: TObject);
    procedure TbBrightnessChange(Sender: TObject);
    procedure TbLineWidthChange(Sender: TObject);
    procedure TbBorderStrengthChange(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
  private
    FBackBitmap : TGuiPixelMapMemory;
  end;

var
  FmLEDTest: TFmLEDTest;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  DAV_GuiCommon;

procedure TFmLEDTest.CbTransparentClick(Sender: TObject);
begin
 LED1.Transparent := CbTransparent.Checked;
 LED2.Transparent := CbTransparent.Checked;
 LED3.Transparent := CbTransparent.Checked;
 LED4.Transparent := CbTransparent.Checked;
 LbUniformiy.Transparent := CbTransparent.Checked;
 LbBrightness.Transparent := CbTransparent.Checked;
 LbBorderStrength.Transparent := CbTransparent.Checked;
 LbLineWidth.Transparent := CbTransparent.Checked;
 Invalidate;
end;

procedure TFmLEDTest.FormCreate(Sender: TObject);
begin
 FBackBitmap := TGuiPixelMapMemory.Create;
end;

procedure TFmLEDTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackBitmap);
end;

procedure TFmLEDTest.FormPaint(Sender: TObject);
begin
 if CbTransparent.Checked and Assigned(FBackBitmap)
  then FBackBitmap.PaintTo(Canvas);
end;

procedure TFmLEDTest.FormResize(Sender: TObject);
var
  x, y      : Integer;
  Filter    : array [0..1] of Single;
  h, hr     : Single;
  ScnLn     : PPixel32Array;
  NewWidth  : Integer;
  NewHeight : Integer;
begin
 NewWidth  := ClientWidth div 2 - 2 * LED1.Left;
 NewHeight := LbUniformiy.Top div 2 - 2 * LED1.Top;
 LED1.Width := NewWidth;
 LED1.Height := NewHeight;
 LED2.Width := NewWidth;
 LED2.Height := NewHeight;
 LED3.Width := NewWidth;
 LED3.Height := NewHeight;
 LED4.Width := NewWidth;
 LED4.Height := NewHeight;

 // Create Background Image
 {$IFNDEF FPC}
 with FBackBitmap do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       Filter[1] := 0.97 * Filter[0] + 0.03 * Random;
       Filter[0] := Filter[1];

       ScnLn[x].B := Round($70 - $34 * (Filter[1] - h));
       ScnLn[x].G := Round($84 - $48 * (Filter[1] - h));
       ScnLn[x].R := Round($8D - $50 * (Filter[1] - h));
      end;
    end;
  end;
 {$ENDIF}
end;

procedure TFmLEDTest.FormShow(Sender: TObject);
begin
 Width := Width + 24;
 Height := Height + 24;
end;

procedure TFmLEDTest.TbBorderStrengthChange(Sender: TObject);
begin
 LED1.BorderStrength_Percent := TbBorderStrength.Position;
 LED2.BorderStrength_Percent := TbBorderStrength.Position;
 LED3.BorderStrength_Percent := TbBorderStrength.Position;
 LED4.BorderStrength_Percent := TbBorderStrength.Position;
end;

procedure TFmLEDTest.TbBrightnessChange(Sender: TObject);
begin
 LED1.Brightness_Percent := TbBrightness.Position;
 LED2.Brightness_Percent := TbBrightness.Position;
 LED3.Brightness_Percent := TbBrightness.Position;
 LED4.Brightness_Percent := TbBrightness.Position;
end;

procedure TFmLEDTest.TbLineWidthChange(Sender: TObject);
begin
 LED1.BorderWidth := TbLineWidth.Position;
 LED2.BorderWidth := TbLineWidth.Position;
 LED3.BorderWidth := TbLineWidth.Position;
 LED4.BorderWidth := TbLineWidth.Position;
end;

procedure TFmLEDTest.TbUniformityChange(Sender: TObject);
begin
 LED1.Uniformity_Percent := TbUniformity.Position;
 LED2.Uniformity_Percent := TbUniformity.Position;
 LED3.Uniformity_Percent := TbUniformity.Position;
 LED4.Uniformity_Percent := TbUniformity.Position;
end;

end.
