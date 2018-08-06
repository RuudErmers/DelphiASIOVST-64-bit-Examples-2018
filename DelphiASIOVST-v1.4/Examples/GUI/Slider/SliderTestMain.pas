unit SliderTestMain;

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
  Classes, Graphics, Controls, Forms, Dialogs, DAV_GuiCommon, DAV_GuiSlider, 
  DAV_GuiPixelMap;

type
  TFmSliderTest = class(TForm)
    GuiEQSlide1: TGuiSlider;
    GuiEQSlide2: TGuiSlider;
    GuiEQSlide3: TGuiSlider;
    GuiEQSlide4: TGuiSlider;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure GuiEQSlide4Change(Sender: TObject);
    procedure GuiEQSlide2Change(Sender: TObject);
  private
    FBackground      : TGuiCustomPixelMap;
    FBackgroundColor : TPixel32;
  public
    procedure RenderBackground;
  end;

var
  FmSliderTest: TFmSliderTest;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common;

procedure TFmSliderTest.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;

 FBackgroundColor := ConvertColor(Color);
 FBackgroundColor.A := $FF;
end;

procedure TFmSliderTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmSliderTest.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmSliderTest.FormResize(Sender: TObject);
begin
 FBackground.SetSize(ClientWidth, ClientHeight);
 RenderBackground;
end;

procedure TFmSliderTest.GuiEQSlide2Change(Sender: TObject);
begin
 GuiEQSlide4.BorderRadius := 0.1 * GuiEQSlide2.Value;
end;

procedure TFmSliderTest.GuiEQSlide4Change(Sender: TObject);
begin
 GuiEQSlide4.BorderWidth := 0.1 * GuiEQSlide4.Value;
end;

procedure TFmSliderTest.RenderBackground;
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

end.
