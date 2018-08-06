unit LabelTestMain;

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
  SysUtils, Classes, Forms, StdCtrls, Controls, DAV_GuiCommon, DAV_GuiLabel,
  DAV_GuiPixelMap, DAV_GuiGraphicControl, DAV_GuiSlider;

type
  TFmLabelTest = class(TForm)
    LabelA: TGuiLabel;
    LabelC: TGuiLabel;
    LabelB: TGuiLabel;
    LabelD: TGuiLabel;
    CbTransparent: TCheckBox;
    SliderBlur: TGuiSlider;
    SliderOffset: TGuiSlider;
    SliderOpacity: TGuiSlider;
    SliderSaturation: TGuiSlider;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
    procedure LabelDClick(Sender: TObject);
    procedure LabelCClick(Sender: TObject);
    procedure SliderBlurChange(Sender: TObject);
    procedure SliderOffsetChange(Sender: TObject);
    procedure SliderOpacityChange(Sender: TObject);
    procedure SliderSaturationChange(Sender: TObject);
  private
    FBackground      : TGuiCustomPixelMap;
    FBackgroundColor : TPixel32;
  public
    procedure RenderBackground;
  end;

var
  FmLabelTest: TFmLabelTest;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Math;

procedure TFmLabelTest.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;

 FBackgroundColor.R := $8D;
 FBackgroundColor.G := $84;
 FBackgroundColor.B := $70;
 FBackgroundColor.A := $FF;
end;

procedure TFmLabelTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmLabelTest.FormResize(Sender: TObject);
begin
 FBackground.SetSize(ClientWidth, ClientHeight);
 RenderBackground;
end;

procedure TFmLabelTest.RenderBackground;
var
  x, y  : Integer;
  s     : array [0..1] of Single;
  Scale : Single;
  h, hr : Single;
  ScnLn : PPixel32Array;
begin
 with FBackground do
  begin
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.5 * 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.015 * Random;
       s[0] := s[1];
       Scale := 1 - (s[1] - h);

       ScnLn[x].B := Round(FBackgroundColor.B * Scale);
       ScnLn[x].G := Round(FBackgroundColor.G * Scale);
       ScnLn[x].R := Round(FBackgroundColor.R * Scale);
       ScnLn[x].A := $FF;
      end;
    end;
  end;
end;

procedure TFmLabelTest.SliderBlurChange(Sender: TObject);
begin
 LabelA.Shadow.Blur := SliderBlur.Value;
 LabelB.Shadow.Blur := SliderBlur.Value;
 LabelC.Shadow.Blur := SliderBlur.Value;
 LabelD.Shadow.Blur := SliderBlur.Value;
end;

procedure TFmLabelTest.SliderOffsetChange(Sender: TObject);
var
  NewOffset : TPoint;
begin
 NewOffset := Point(Round(SliderOffset.Value), Round(SliderOffset.Value));
 LabelA.Shadow.Offset := NewOffset;
 LabelB.Shadow.Offset := NewOffset;
 LabelC.Shadow.Offset := NewOffset;
 LabelD.Shadow.Offset := NewOffset;
end;

procedure TFmLabelTest.SliderOpacityChange(Sender: TObject);
begin
 LabelA.Shadow.Opacity := Round(SliderOpacity.Value);
 LabelB.Shadow.Opacity := Round(SliderOpacity.Value);
 LabelC.Shadow.Opacity := Round(SliderOpacity.Value);
 LabelD.Shadow.Opacity := Round(SliderOpacity.Value);
end;

procedure TFmLabelTest.SliderSaturationChange(Sender: TObject);
var
  Saturation : Integer;
begin
 LabelA.Shadow.Saturation := SliderSaturation.Value;
 LabelB.Shadow.Saturation := SliderSaturation.Value;
 LabelC.Shadow.Saturation := SliderSaturation.Value;
 LabelD.Shadow.Saturation := SliderSaturation.Value;
end;

procedure TFmLabelTest.LabelCClick(Sender: TObject);
begin
 FBackgroundColor.R := $40 + Random(100);
 FBackgroundColor.G := $40 + Random(100);
 FBackgroundColor.B := $40 + Random(100);
 RenderBackground;
 Invalidate;
end;

procedure TFmLabelTest.LabelDClick(Sender: TObject);
begin
 LabelA.Shadow.Visible := not LabelA.Shadow.Visible;
 LabelB.Shadow.Visible := not LabelB.Shadow.Visible;
 LabelC.Shadow.Visible := not LabelC.Shadow.Visible;
 LabelD.Shadow.Visible := not LabelD.Shadow.Visible;
end;

procedure TFmLabelTest.FormPaint(Sender: TObject);
begin
 FBackground.PaintTo(Canvas);
end;

procedure TFmLabelTest.CbTransparentClick(Sender: TObject);
begin
 LabelA.Transparent := CbTransparent.Checked;
 LabelB.Transparent := CbTransparent.Checked;
 LabelC.Transparent := CbTransparent.Checked;
 LabelD.Transparent := CbTransparent.Checked;
end;

{$IFDEF FPC}
initialization
  {$i LabelTestMain.lrs}
{$ENDIF}

end.
