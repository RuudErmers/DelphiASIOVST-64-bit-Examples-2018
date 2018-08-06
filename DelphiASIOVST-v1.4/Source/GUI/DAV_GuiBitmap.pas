unit DAV_GuiBitmap;

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

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LCLType, LResources, LMessages, FPImage, IntfGraphics,
  {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes, SysUtils, DAV_Common, DAV_MemoryUtils, DAV_GuiCommon,
  DAV_GuiPixelMap;

procedure Upsample2xBitmap32(var PixelMap: TGuiCustomPixelMap);
procedure Downsample2xBitmap32(var PixelMap: TGuiCustomPixelMap);

implementation

uses
  Math;

(*
type
  TFixedPoint = type Integer;
  TFloatingPoint = type Double;

const
  CFixedPointOne = 1 shl 16;
  CFixedPointHalf = 5 shl 15;

function FloatingToFixedPoint(Value: TFloatingPoint): TFixedPoint;
begin
 Result := Round(Value * CFixedPointOne);
end;

function FixedToFloatingPoint(Value: TFixedPoint): TFloatingPoint;
begin
 Result := Value / CFixedPointOne;
end;
*)


procedure Upsample2xBitmap32(var PixelMap: TGuiCustomPixelMap);
var
  x, y      : Integer;
  x2        : Integer;
  PixelLine : array [0..2] of PPixel32Array;
begin
 with PixelMap do
  begin
   for y := (Height div 2) - 1 downto 0 do
    begin
     PixelLine[0] := ScanLine[y];
     PixelLine[1] := ScanLine[y * 2];
     PixelLine[2] := ScanLine[y * 2 + 1];
     for x := (Width  div 2) - 1 downto 0 do
      begin
       x2 := 2 * x;
       PixelLine[1, x2    ] := PixelLine[0, x];
       PixelLine[2, x2    ] := PixelLine[0, x];
       PixelLine[1, x2 + 1] := PixelLine[0, x];
       PixelLine[2, x2 + 1] := PixelLine[0, x];
      end;
    end;
  end;
end;

procedure Downsample2xBitmap32(var PixelMap: TGuiCustomPixelMap);
var
  x, y      : Integer;
  PixelLine : array [0..2] of PPixel32Array;
begin
 with PixelMap do
  begin
   for y := 0 to (Height div 2) - 1 do
    begin
     PixelLine[0] := ScanLine[y];
     PixelLine[1] := ScanLine[y * 2];
     PixelLine[2] := ScanLine[y * 2 + 1];
     for x := 0 to (Width  div 2) - 1 do
      begin
       PixelLine[0, x].B := (PixelLine[1, 2 * x].B + PixelLine[2, 2 * x].B + PixelLine[1, 2 * x + 1].B + PixelLine[2, 2 * x + 1].B) div 4;
       PixelLine[0, x].G := (PixelLine[1, 2 * x].G + PixelLine[2, 2 * x].G + PixelLine[1, 2 * x + 1].G + PixelLine[2, 2 * x + 1].G) div 4;
       PixelLine[0, x].R := (PixelLine[1, 2 * x].R + PixelLine[2, 2 * x].R + PixelLine[1, 2 * x + 1].R + PixelLine[2, 2 * x + 1].R) div 4;
       PixelLine[0, x].A := (PixelLine[1, 2 * x].A + PixelLine[2, 2 * x].A + PixelLine[1, 2 * x + 1].A + PixelLine[2, 2 * x + 1].A) div 4;
      end;
    end;
  end;
end;

end.
