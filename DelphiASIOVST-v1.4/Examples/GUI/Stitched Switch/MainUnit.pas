unit MainUnit;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2011-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LCLType, {$ELSE} Windows, Messages, {$ENDIF} SysUtils,
  Classes, Graphics, Controls, Forms, DAV_GuiCommon, DAV_GuiPixelMap,
  DAV_GuiStitchedControls, DAV_GuiStitchedSwitch, DAV_GuiStitchedPngList,
  DAV_GuiStitchedImageList, DAV_GuiImageControl, DAV_GuiCustomControl;

type
  TFmSwitchTest = class(TForm)
    GuiStitchedImageList: TGuiStitchedImageList;
    GuiStitchedSwitch0: TGuiStitchedSwitch;
    GuiStitchedSwitch1: TGuiStitchedSwitch;
    GuiStitchedSwitch2: TGuiStitchedSwitch;
    GuiStitchedSwitch3: TGuiStitchedSwitch;
    GuiStitchedPNGList: TGuiStitchedPNGList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure GuiStitchedSwitchChange(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  end;

var
  FmSwitchTest: TFmSwitchTest;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
  DAV_GuiFileFormats, DAV_GuiPng;

procedure TFmSwitchTest.FormCreate(Sender: TObject);
const
  CSwitchFileName : TFileName = '..\Resources\Knobs\BigStop.png';
begin
 if FileExists(CSwitchFileName) then
  with GuiStitchedImageList[0], PixelMap do
   begin
    LoadFromFile(CSwitchFileName);
    GlyphCount := 2;
   end;
 FBackground := TGuiPixelMapMemory.Create;

 GuiStitchedSwitch2.Transparent := True;
end;

procedure TFmSwitchTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmSwitchTest.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmSwitchTest.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  b      : ShortInt;
  ScnLn  : PPixel32Array;
begin
 if Assigned(FBackground) then
  with FBackground do
   begin
    SetSize(ClientWidth, ClientHeight);
    s[0] := 0;
    s[1] := 0;
    for y := 0 to Height - 1 do
     begin
      ScnLn := Scanline[y];
      for x := 0 to Width - 1 do
       begin
        s[1] := 0.97 * s[0] + 0.03 * (2 * Random - 1);
        b := Round($7F + $3F * s[1]);
        s[0] := s[1];
        ScnLn[x].B := b;
        ScnLn[x].G := b;
        ScnLn[x].R := b;
       end;
     end;
   end;
end;

procedure TFmSwitchTest.GuiStitchedSwitchChange(Sender: TObject);
begin
 with Sender as TGuiStitchedSwitch do Transparent := GlyphIndex = 1;
end;

end.
