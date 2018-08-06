unit DAV_GuiVectorPixelLine;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Graphics, Classes, SysUtils, DAV_Common, DAV_FixedPoint, DAV_GuiCommon, 
  DAV_GuiPixelMap, DAV_GuiVector, DAV_GuiVectorPixel;

type
  TGuiPixelThinLine = class(TCustomGuiPixelSimplePrimitive)
  private
    function GetGeometricShape: TGuiLine;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;

    property GeometricShape: TGuiLine read GetGeometricShape;
  end;

  TGuiPixelLine = class(TCustomGuiPixelFramePrimitive)
  private
    function GetGeometricShape: TGuiLine;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;

    property GeometricShape: TGuiLine read GetGeometricShape;
  end;

implementation

uses
  Math, DAV_GuiBlend;

{ TGuiPixelThinLine }

constructor TGuiPixelThinLine.Create;
begin
 inherited;
 FGeometricShape := TGuiLine.Create;
end;

function TGuiPixelThinLine.GetGeometricShape: TGuiLine;
begin
 Result := TGuiLine(FGeometricShape);
end;

procedure TGuiPixelThinLine.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
var
  X, Y         : array [0..1] of Integer;
  t, dx, dy    : Integer;
  incx, incy   : Integer;
  pdx, pdy     : Integer;
  ddx, ddy     : Integer;
  es, el, err  : Integer;
  DataPointer  : PPixel32Array;
  PixelColor32 : TPixel32;
begin
 with GeometricShape do
  begin
   X[0] := ConvertFromFixed24Dot8ToInteger(XA);
   Y[0] := ConvertFromFixed24Dot8ToInteger(YA);
   X[1] := ConvertFromFixed24Dot8ToInteger(XB);
   Y[1] := ConvertFromFixed24Dot8ToInteger(YB);
  end;

 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;
 DataPointer := PixelMap.DataPointer;

 // ensure the line goes always from left to right
 if X[0] > X[1] then
  begin
   Exchange32(X[0], X[1]);
   Exchange32(Y[0], Y[1]);
  end;

 if Y[0] = Y[1] then
  begin
   // check whether the Y[0]-coordinate is outside the pixel map
   if (Y[0] < 0) or (Y[0] >= PixelMap.Height) then Exit;

   // limit X[0]-coordinate to values inside the pixel map
   X[0] := Limit(X[0], 0, PixelMap.Width - 1);
   X[1] := Limit(X[1], 0, PixelMap.Width - 1);

   // check if all coordinates are equal
   if X[0] = X[1] then
    try
     BlendPixelInplace(PixelColor32, DataPointer[Y[1] * PixelMap.Width + X[1]]);
     Exit;
    finally
     EMMS;
    end;

   // draw horizontal line
   BlendPixelLine(PixelColor32, @DataPointer[Y[1] * PixelMap.Width + X[0]], X[1] - X[0])
  end else
 if X[0] = X[1] then
  begin
   // check whether the X[0]-coordinate is outside the pixel map
   if (X[0] < 0) or (X[0] >= PixelMap.Width) then Exit;

   // limit Y[0]-coordinate to values inside the pixel map
   Y[0] := Limit(Y[0], 0, PixelMap.Height - 1);
   Y[1] := Limit(Y[1], 0, PixelMap.Height - 1);

   // check if all coordinates are equal
   if Y[0] = Y[1] then
    try
     BlendPixelInplace(PixelColor32, DataPointer[Y[1] * PixelMap.Width + X[1]]);
     Exit;
    finally
     EMMS;
    end;

   // draw vertical line
   PixelMap.VerticalLine(X[0], Y[0], Y[1], PixelColor32);
  end
 else
  try
   // calculate length in X[0] and Y[0] coordinates
   dx := X[1] - X[0];
   dy := Y[1] - Y[0];

   // check whether X[0]-coordinate is outside the pixel map
   if (X[0] < 0) then
    begin
     // check if line needs to be drawn at all
     if X[1] < 0 then Exit;

     // calculate new left offset
     Y[0] := Round(Y[0] + (-X[0] / dx) * dy);
     X[0] := 0;

     // recalculate length in X[0] and Y[0] coordinates
     dx := X[1];
     dy := Y[1] - Y[0];
    end else
   if (X[0] >= PixelMap.Width) then
    begin
     // check if line needs to be drawn at all
     if X[1] >= PixelMap.Width then Exit;

     // calculate new left offset
     Y[0] := Round(Y[0] + ((PixelMap.Width - 1 - X[0]) / dx) * dy);
     X[0] := PixelMap.Width - 1;

     // recalculate length in X[0] and Y[0] coordinates
     dx := X[1] - X[0];
     dy := Y[1] - Y[0];
    end;

   if (X[1] < 0) then
    begin
     // calculate new left offset
     Y[1] := Round(Y[1] + (-X[1] / dx) * dy);
     X[1] := 0;

     // recalculate length in X[0] and Y[0] coordinates
     dx :=    - X[0];
     dy := Y[1] - Y[0];
    end else
   if (X[1] >= PixelMap.Width) then
    begin
     // calculate new left offset
     Y[1] := Round(Y[1] + ((PixelMap.Width - 1 - X[1]) / dx) * dy);
     X[1] := PixelMap.Width - 1;

     // recalculate length in X[0] and Y[0] coordinates
     dx := X[1] - X[0];
     dy := Y[1] - Y[0];
    end;

   // check whether Y[0]-coordinate is outside the pixel map
   if (Y[0] < 0) then
    begin
     // check if line needs to be drawn at all
     if Y[1] < 0 then Exit;

     // calculate new left offset
     X[0] := Round(X[0] + (-Y[0] / dy) * dx);
     Y[0] := 0;

     // recalculate length in X[0] and Y[0] coordinates
     dx := X[1] - X[0];
     dy := Y[1];
    end else
   if (Y[0] >= PixelMap.Height) then
    begin
     // check if line needs to be drawn at all
     if Y[1] >= PixelMap.Height then Exit;

     // calculate new left offset
     X[0] := Round(X[0] + ((PixelMap.Height - 1 - Y[0]) / dy) * dx);
     Y[0] := PixelMap.Height - 1;

     // recalculate length in X[0] and Y[0] coordinates
     dx := X[1] - X[0];
     dy := Y[1] - Y[0];
    end;

   if (Y[1] < 0) then
    begin
     // recalculate new length in X[0] and Y[0] coordinates
     dx := Round(X[1] + (-Y[1] / dy) * dx) - X[0];
     dy :=                               - Y[0];
    end else
   if (Y[1] >= PixelMap.Height) then
    begin
     // recalculate new length in X[0] and Y[0] coordinates
     dx := Round(X[1] + ((PixelMap.Height - 1 - Y[1]) / dy) * dx) - X[0];
     dy := (PixelMap.Height - 1) - Y[0];
    end;

   incx := Sign(dx);
   incy := Sign(dy);
   if (dx < 0) then dx := -dx;
   if (dy < 0) then dy := -dy;

   if (dx > dy) then
    begin
     pdx := incx;
     pdy := 0;
     ddx := incx;
     ddy := incy;
     es  := dy;
     el  := dx;
    end
   else
    begin
     pdx := 0;
     pdy := incy;
     ddx := incx;
     ddy := incy;
     es  := dx;
     el  := dy;
    end;

   err := el shr 1;
   BlendPixelInplace(PixelColor32, DataPointer[Y[0] * PixelMap.Width + X[0]]);

   for t := 1 to el - 1 do
    begin
     err := err - es;
     if (err < 0) then
      begin
       err := err + el;
       X[0] := X[0] + ddx;
       Y[0] := Y[0] + ddy;
      end
     else
      begin
       X[0] := X[0] + pdx;
       Y[0] := Y[0] + pdy;
      end;
     BlendPixelInplace(PixelColor32, DataPointer[Y[0] * PixelMap.Width + X[0]]);
    end;
  finally
   EMMS;
  end;
end;

procedure TGuiPixelThinLine.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
var
  FromX, ToX      : TFixed24Dot8;
  FromY, ToY      : TFixed24Dot8;
  DeltaX, DeltaY  : TFixed24Dot8;
  PixelColor32    : TPixel32;
  Gradient        : TFixed24Dot8;
  XEnd            : TFixed24Dot8;
  YEnd            : TFixed24Dot8;
  Gap             : TFixed24Dot8;
  Temp            : TFixed24Dot8;
  XPos            : array [0..1] of Integer;
  YPos            : array [0..1] of Integer;
  X, Y            : Integer;
  OriginalAlpha   : Integer;
  Inter           : TFixed24Dot8;
begin
 with GeometricShape do
  begin
   FromX := XA;
   FromY := YA;
   ToX := XB;
   ToY := YB;
  end;

 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;
 OriginalAlpha := Alpha;

 DeltaX := FixedSub(ToX, FromX);
 DeltaY := FixedSub(ToY, FromY);

 if Abs(DeltaX.Fixed) < Abs(DeltaY.Fixed) then
  begin
   if ToY.Fixed < FromY.Fixed then
    begin
     Exchange32(FromX, ToX);
     Exchange32(FromY, ToY);
    end;

   Gradient := FixedDiv(DeltaX, DeltaY);

   // handle first endpoint
   YEnd := FixedAdd(FromY, CFixed24Dot8Half);
   YEnd.Frac := 0;
   XEnd := FixedAdd(FromX, FixedMul(Gradient, FixedSub(YEnd, FromY)));
   Gap.Fixed := $FF - FixedAdd(FromY, CFixed24Dot8Half).Frac;
   YPos[0] := FixedRound(YEnd);  // this will be used in the main loop
   XPos[0] := FixedFloor(XEnd);

   if (YPos[0] >= 0) and (YPos[0] < PixelMap.Height) then
    begin
     Temp.Fixed := $FF - XEnd.Frac;
     PixelColor32.A := (OriginalAlpha * FixedMul(Temp, Gap).Fixed + $80) shr 8;
     if (XPos[0] >= 0) and (XPos[0] < PixelMap.Width)
      then BlendPixelInplace(PixelColor32, PixelMap.PixelPointer[XPos[0]    , YPos[0]]^);
     Temp.Fixed := XEnd.Frac;
     PixelColor32.A := (OriginalAlpha * FixedMul(Temp, Gap).Fixed + $80) shr 8;
     if (XPos[0] + 1 >= 0) and (XPos[0] + 1 < PixelMap.Width)
      then BlendPixelInplace(PixelColor32, PixelMap.PixelPointer[XPos[0] + 1, YPos[0]]^);
    end;

   Inter.Fixed := XEnd.Fixed + Gradient.Fixed; // first Y-intersection for the main loop

   YEnd := FixedAdd(ToY, CFixed24Dot8Half);
   YEnd.Frac := 0;
   YPos[1] := FixedRound(YEnd);  // this will be used in the main loop

   // main loop
   for Y := YPos[0] + 1 to YPos[1] - 1 do
    begin
     if (Y >= 0) and (Y < PixelMap.Height) then
      begin
       X := FixedFloor(Inter);
       PixelColor32.A := (OriginalAlpha * ($FF - Inter.Frac) + $80) shr 8;
       if (X >= 0) and (X < PixelMap.Width)
        then BlendPixelInplace(PixelColor32, PixelMap.PixelPointer[X    , Y]^);
       PixelColor32.A := (OriginalAlpha * Inter.Frac + $80) shr 8;
       if (X + 1 >= 0) and (X + 1 < PixelMap.Width)
        then BlendPixelInplace(PixelColor32, PixelMap.PixelPointer[X + 1, Y]^);
      end;
     Inter.Fixed := Inter.Fixed + Gradient.Fixed;
    end;

   // handle second endpoint
   XEnd := Inter;
   Gap.Fixed := FixedAdd(ToY, CFixed24Dot8Half).Frac;
   XPos[1] := FixedFloor(XEnd);

   if (YPos[1] >= 0) and (YPos[1] < PixelMap.Height) then
    begin
     Temp.Fixed := $FF - XEnd.Frac;
     PixelColor32.A := (OriginalAlpha * FixedMul(Temp, Gap).Fixed + $80) shr 8;
     if (XPos[1] >= 0) and (XPos[1] < PixelMap.Width)
      then BlendPixelInplace(PixelColor32, PixelMap.PixelPointer[XPos[1]    , YPos[1]]^);
     Temp.Fixed := XEnd.Frac;
     PixelColor32.A := (OriginalAlpha * FixedMul(Temp, Gap).Fixed + $80) shr 8;
     if (XPos[1] + 1 >= 0) and (XPos[1] + 1 < PixelMap.Width)
      then BlendPixelInplace(PixelColor32, PixelMap.PixelPointer[XPos[1] + 1, YPos[1]]^);
    end;

  end
 else
  begin
   if ToX.Fixed < FromX.Fixed then
    begin
     Exchange32(FromX, ToX);
     Exchange32(FromY, ToY);
    end;

   Gradient := FixedDiv(DeltaY, DeltaX);

   // handle first endpoint
   XEnd := FixedAdd(FromX, CFixed24Dot8Half);
   XEnd.Frac := 0;
   YEnd := FixedAdd(FromY, FixedMul(Gradient, FixedSub(XEnd, FromX)));
   Gap.Fixed := $FF - FixedAdd(FromX, CFixed24Dot8Half).Frac;
   XPos[0] := FixedRound(XEnd);  // this will be used in the main loop
   YPos[0] := FixedFloor(YEnd);

   if (XPos[0] >= 0) and (XPos[0] < PixelMap.Width) then
    begin
     Temp.Fixed := $FF - YEnd.Frac;
     if (YPos[0] >= 0) and (YPos[0] < PixelMap.Height)
      then CombinePixelInplace(PixelColor32, PixelMap.PixelPointer[XPos[0], YPos[0]    ]^, FixedMul(Temp, Gap).Fixed);
     Temp.Fixed := YEnd.Frac;
     if (YPos[0] + 1 >= 0) and (YPos[0] + 1 < PixelMap.Height)
      then CombinePixelInplace(PixelColor32, PixelMap.PixelPointer[XPos[0], YPos[0] + 1]^, FixedMul(Temp, Gap).Fixed);
    end;

   Inter := FixedAdd(YEnd, Gradient); // first Y-intersection for the main loop

   XEnd := FixedAdd(ToX, CFixed24Dot8Half);
   XEnd.Frac := 0;
   XPos[1] := FixedRound(XEnd);  // this will be used in the main loop

   // main loop
   for X := XPos[0] + 1 to XPos[1] - 1 do
    begin
     if (X >= 0) and (X < PixelMap.Width) then
      begin
       Y := FixedFloor(Inter);
       if (Y >= 0) and (Y < PixelMap.Height)
        then CombinePixelInplace(PixelColor32, PixelMap.PixelPointer[X, Y    ]^, $FF - Inter.Frac);
       if (Y + 1 >= 0) and (Y + 1 < PixelMap.Height)
        then CombinePixelInplace(PixelColor32, PixelMap.PixelPointer[X, Y + 1]^, Inter.Frac);
      end;
     Inter := FixedAdd(Inter, Gradient);
    end;

   // handle second endpoint
   YEnd := Inter;
   Gap.Fixed := FixedAdd(ToX, CFixed24Dot8Half).Frac;
   YPos[1] := FixedFloor(YEnd);

   if (XPos[1] >= 0) and (XPos[1] < PixelMap.Width) then
    begin
     Temp.Fixed := $FF - YEnd.Frac;
     if (YPos[1] >= 0) and (YPos[1] < PixelMap.Height)
      then CombinePixelInplace(PixelColor32, PixelMap.PixelPointer[XPos[1], YPos[1]    ]^, FixedMul(Temp, Gap).Fixed);
     Temp.Fixed := YEnd.Frac;
     if (YPos[1] + 1 >= 0) and (YPos[1] + 1 < PixelMap.Height)
      then CombinePixelInplace(PixelColor32, PixelMap.PixelPointer[XPos[1], YPos[1] + 1]^, FixedMul(Temp, Gap).Fixed);
    end;
  end;
end;


{ TGuiPixelLine }

constructor TGuiPixelLine.Create;
begin
 inherited;
 FGeometricShape := TGuiLine.Create;
end;

function TGuiPixelLine.GetGeometricShape: TGuiLine;
begin
 Result := TGuiLine(FGeometricShape);
end;

procedure TGuiPixelLine.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
var
  X, Y          : array [0..1] of Integer;
  LineWidth     : Integer;
  dx, dy        : Integer;
  Index         : Integer;
  DataPointer   : PPixel32Array;
  PixelColor32  : TPixel32;
  Gradient      : Single;
  ScanLineRange : array [0..1] of Integer;
begin
 with GeometricShape do
  begin
   X[0] := ConvertFromFixed24Dot8ToInteger(XA);
   Y[0] := ConvertFromFixed24Dot8ToInteger(YA);
   X[1] := ConvertFromFixed24Dot8ToInteger(XB);
   Y[1] := ConvertFromFixed24Dot8ToInteger(YB);
   LineWidth := 3; //ConvertFromFixed24Dot8ToInteger(Width);
  end;

 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;

 DataPointer := PixelMap.DataPointer;

  // ensure the line goes always from left to right
 if X[0] > X[1] then
  begin
   Exchange32(X[0], X[1]);
   Exchange32(Y[0], Y[1]);
  end;

 if Y[0] = Y[1] then
  begin
   // check whether the Y[0]-coordinate is outside the pixel map
   if (Y[0] < 0) or (Y[0] >= PixelMap.Height) then Exit;

   // limit X[0]-coordinate to values inside the pixel map
   X[0] := Limit(X[0], 0, PixelMap.Width - 1);
   X[1] := Limit(X[1], 0, PixelMap.Width - 1);

   // check if all coordinates are equal
   if X[0] = X[1] then
    try
     BlendPixelInplace(PixelColor32, DataPointer[(Y[1] + Index - LineWidth div 2) * PixelMap.Width + X[1]]);
     Exit;
    finally
     EMMS;
    end;

   // draw horizontal line
//   for Index := 0 to LineWidth - 1 do
   BlendPixelLine(PixelColor32, @DataPointer[Y[1] * PixelMap.Width + X[0]], X[1] - X[0])
  end else
 if X[0] = X[1] then
  begin
   // check whether the X[0]-coordinate is outside the pixel map
   if (X[0] < 0) or (X[0] >= PixelMap.Width) then Exit;

   // limit Y[0]-coordinate to values inside the pixel map
   Y[0] := Limit(Y[0], 0, PixelMap.Height - 1);
   Y[1] := Limit(Y[1], 0, PixelMap.Height - 1);

   // check if all coordinates are equal
   if Y[0] = Y[1] then
    try
     BlendPixelInplace(PixelColor32, DataPointer[Y[1] * PixelMap.Width + X[1]]);
     Exit;
    finally
     EMMS;
    end;

   // draw vertical line
   PixelMap.VerticalLine(X[0], Y[0], Y[1], PixelColor32);
  end
 else
  try
   // calculate length in X[0] and Y[0] coordinates
   dx := X[1] - X[0];
   dy := Y[1] - Y[0];

   // check whether X[0]-coordinate is outside the pixel map
   if (X[0] < 0) then
    begin
     // check if line needs to be drawn at all
     if X[1] < 0 then Exit;

     // calculate new left offset
     Y[0] := Round(Y[0] + (-X[0] / dx) * dy);
     X[0] := 0;

     // recalculate length in X[0] and Y[0] coordinates
     dx := X[1];
     dy := Y[1] - Y[0];
    end else
   if (X[0] >= PixelMap.Width) then
    begin
     // check if line needs to be drawn at all
     if X[1] >= PixelMap.Width then Exit;

     // calculate new left offset
     Y[0] := Round(Y[0] + ((PixelMap.Width - 1 - X[0]) / dx) * dy);
     X[0] := PixelMap.Width - 1;

     // recalculate length in X[0] and Y[0] coordinates
     dx := X[1] - X[0];
     dy := Y[1] - Y[0];
    end;

   if (X[1] < 0) then
    begin
     // calculate new left offset
     Y[1] := Round(Y[1] + (-X[1] / dx) * dy);
     X[1] := 0;

     // recalculate length in X[0] and Y[0] coordinates
     dx :=    - X[0];
     dy := Y[1] - Y[0];
    end else
   if (X[1] >= PixelMap.Width) then
    begin
     // calculate new left offset
     Y[1] := Round(Y[1] + ((PixelMap.Width - 1 - X[1]) / dx) * dy);
     X[1] := PixelMap.Width - 1;

     // recalculate length in X[0] and Y[0] coordinates
     dx := X[1] - X[0];
     dy := Y[1] - Y[0];
    end;

   // check whether Y[0]-coordinate is outside the pixel map
   if (Y[0] < 0) then
    begin
     // check if line needs to be drawn at all
     if Y[1] < 0 then Exit;

     // calculate new left offset
     X[0] := Round(X[0] + (-Y[0] / dy) * dx);
     Y[0] := 0;

     // recalculate length in X[0] and Y[0] coordinates
     dx := X[1] - X[0];
     dy := Y[1];
    end else
   if (Y[0] >= PixelMap.Height) then
    begin
     // check if line needs to be drawn at all
     if Y[1] >= PixelMap.Height then Exit;

     // calculate new left offset
     X[0] := Round(X[0] + ((PixelMap.Height - 1 - Y[0]) / dy) * dx);
     Y[0] := PixelMap.Height - 1;

     // recalculate length in X[0] and Y[0] coordinates
     dx := X[1] - X[0];
     dy := Y[1] - Y[0];
    end;

   if (Y[1] < 0) then
    begin
     // recalculate new length in X[0] and Y[0] coordinates
     dx := Round(X[1] + (-Y[1] / dy) * dx) - X[0];
     dy :=                               - Y[0];
    end else
   if (Y[1] >= PixelMap.Height) then
    begin
     // recalculate new length in X[0] and Y[0] coordinates
     dx := Round(X[1] + ((PixelMap.Height - 1 - Y[1]) / dy) * dx) - X[0];
     dy := (PixelMap.Height - 1) - Y[0];
    end;

(*
   if dy > dx then
    begin
     if Y[0] < Y[1] then
      begin
       ScanLineRange[0] := Y[0] - LineWidth;
       ScanLineRange[1] := Y[1] + LineWidth;
      end;
    end;
*)

  finally
   EMMS;
  end;
end;

procedure TGuiPixelLine.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
begin

end;


{
procedure TGuiCustomPixelMap.Line(FromX, FromY, ToX, ToY: TFixedPoint; Color: TPixel32);
var
  Index        : Integer;
  dx, dy, t    : TFixedPoint;
  Gradient     : Single;
  FltEnd       : TFixedPoint;
  IntEnd       : Integer;
  gap          : TFixedPoint;
  xpxl1, ypxl1 : Integer;
  xpxl2, ypxl2 : Integer;
  Inter        : TFixedPoint;
  Offset       : TFixedPoint;
begin
 dx := ToX - FromX;
 dy := ToY - FromY;
 if Abs(dx) > Abs(dy) then
  begin
   if ToX < FromX then
    begin
     t := FromX;
     FromX := ToX;
     ToX := t;
     t := FromY;
     FromY := ToY;
     ToY := t;
    end;
   Gradient := dy / dx;
   Offset   := Round(Gradient * CFixedPointOne);
   IntEnd   := (FromX + CFixedPointHalf) shr 16;
   FltEnd   := FromY + Round(Gradient * ((FromX + CFixedPointHalf) and $FFFF0000 - FromX));
   gap      := CFixedPointOne - ((FromX + CFixedPointHalf) and $FFFF);
   xpxl1    := IntEnd;
   ypxl1    := FltEnd and $FFFF0000;

   FDataPointer[ (ypxl1                   shr 16) * Width + xpxl1] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[((ypxl1 + CFixedPointOne) shr 16) * Width + xpxl1] := Color; // weight = Frac(FltEnd) * gap
   Inter := FltEnd + Offset;

   IntEnd := (ToX + CFixedPointHalf) shr 16;
   FltEnd := ToY + Round(Gradient * ((ToX + CFixedPointHalf) and $FFFF0000 - ToX));
   gap := (ToX + CFixedPointHalf) and $FFFF;
   xpxl2 := IntEnd;
   ypxl2 := FltEnd and $FFFF0000;
   FDataPointer[ (ypxl2                   shr 16) * Width + xpxl2] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[((ypxl2 + CFixedPointOne) shr 16) * Width + xpxl2] := Color; // weight = Frac(FltEnd) * gap

   for Index := xpxl1 + 1 to xpxl2 - 1 do
    begin
     FDataPointer[ (Inter                   shr 16) * Width + Index] := Color; // weight = (1 - Frac(Inter))
     FDataPointer[((Inter + CFixedPointOne) shr 16) * Width + Index] := Color; // weight = Frac(Inter)
     Inter := Inter + Offset;
    end;
  end
 else
  begin
   if ToY < FromY then
    begin
     t := FromX;
     FromX := ToX;
     ToX := t;
     t := FromY;
     FromY := ToY;
     ToY := t;
    end;
   Gradient := dx / dy;
   Offset   := Round(Gradient * CFixedPointOne);
   IntEnd   := (FromY + CFixedPointHalf) shr 16;
   FltEnd   := FromX + Round(Gradient * ((FromY + CFixedPointHalf) and $FFFF0000 - FromY));
   gap      := CFixedPointOne - ((FromY + CFixedPointHalf) and $FFFF);
   ypxl1    := IntEnd;
   xpxl1    := FltEnd and $FFFF0000;

   FDataPointer[ (ypxl1                   shr 16) * Width + xpxl1] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[((ypxl1 + CFixedPointOne) shr 16) * Width + xpxl1] := Color; // weight = Frac(FltEnd) * gap
   Inter := FltEnd + Offset;

   IntEnd := (ToY + CFixedPointHalf) shr 16;
   FltEnd := ToX + Round(Gradient * ((ToY + CFixedPointHalf) and $FFFF0000 - ToY));
   gap := (ToY + CFixedPointHalf) and $FFFF;
   ypxl2 := IntEnd;
   xpxl2 := FltEnd and $FFFF0000;
   FDataPointer[ (ypxl2                   shr 16) * Width + xpxl2] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[((ypxl2 + CFixedPointOne) shr 16) * Width + xpxl2] := Color; // weight = Frac(FltEnd) * gap

   for Index := ypxl1 + 1 to ypxl2 - 1 do
    begin
     FDataPointer[ (Inter                   shr 16) * Width + Index] := Color; // weight = (1 - Frac(Inter))
     FDataPointer[((Inter + CFixedPointOne) shr 16) * Width + Index] := Color; // weight = Frac(Inter)
     Inter := Inter + Offset;
    end;
(*
*)
  end;
(*
var
  x, y, t      : TFixedPoint;
  dx, dy, xgap : TFixedPoint;
  xend, yend   : TFixedPoint;
  xpxl1, ypxl1 : TFixedPoint;
  xpxl2, ypxl2 : TFixedPoint;
  intery       : TFixedPoint;
  Gradient     : TFixedPoint;
begin
 dx := ToX - FromX;
 dy := ToY - FromY;
 if Abs(dx) < Abs(dy) then
  begin
   t := FromX;
   FromX := FromY;
   FromY := t;
   t := ToX;
   ToX := ToY;
   ToY := t;
   t := dx;
   dx := dy;
   dy := t;
  end;
 if ToX < FromX then
  begin
   t := FromX;
   FromX := ToX;
   ToX := t;
   t := FromY;
   FromY := ToY;
   ToY := t;
  end;
 Gradient := round((dy / dx) * CFixedPointOne);

 // handle first endpoint
 xend := (FromX + CFixedPointHalf) and $FFFF0000;
 yend := FromY + gradient * (xend - FromX);
 xgap := CFixedPointOne - (FromX + CFixedPointHalf) and $FFFF;
 xpxl1 := xend;  // this will be used in the main loop
 ypxl1 := yend and $FFFF0000;

 FDataPointer[(ypxl1 shr 16) * Width + (xpxl1 shr 16)] := Color; // weight = rfpart(yend) * xgap)
 FDataPointer[((ypxl1 + CFixedPointOne) shr 16) * Width + (xpxl1 shr 16)] := Color; // weight = fpart(yend) * xgap
 intery := yend + gradient; // first y-intersection for the main loop

 // handle second endpoint
 xend := (ToX + CFixedPointHalf) and $FFFF0000;
 yend := ToY + gradient * (xend - ToX);
 xgap := CFixedPointOne - (ToX + CFixedPointHalf) and $FFFF;
 xpxl2 := xend;  // this will be used in the main loop
 ypxl2 := yend and $FFFF0000;

// FDataPointer[(ypxl2 shr 16) * Width + (xpxl2 shr 16)] := Color; // weight = rfpart (yend) * xgap)
// FDataPointer[((ypxl2 + CFixedPointOne) shr 16) * Width + (xpxl2 shr 16)] := Color; // weight = fpart (yend) * xgap

 // main loop
 x := xpxl1;
 while x < xpxl2 do
  begin
   FDataPointer[(intery shr 16) * Width + (x shr 16)] := Color; // weight = rfpart (yend) * xgap)
   FDataPointer[((intery + CFixedPointOne) shr 16) * Width + (x shr 16)] := Color; // weight = fpart (yend) * xgap
   intery := intery + gradient;
   x := x + CFixedPointOne;
  end;
*)
end;

procedure TGuiCustomPixelMap.Line(FromX, FromY, ToX, ToY: Double; Color: TPixel32);
var
  Index        : Integer;
  dx, dy, t    : Double;
  Gradient     : Double;
  FltEnd       : Double;
  IntEnd       : Integer;
  gap          : Double;
  xpxl1, ypxl1 : Integer;
  xpxl2, ypxl2 : Integer;
  Inter        : Double;
begin
 dx := ToX - FromX;
 dy := ToY - FromY;
 if Abs(dx) > Abs(dy) then
  begin
   if ToX < FromX then
    begin
     t := FromX;
     FromX := ToX;
     ToX := t;
     t := FromY;
     FromY := ToY;
     ToY := t;
    end;
   Gradient := dy / dx;
   IntEnd   := Round(FromX);
   FltEnd   := FromY + gradient * (IntEnd - FromX);
   gap      := 1 - Frac(FromX + 0.5);
   xpxl1    := IntEnd;
   ypxl1    := Trunc(FltEnd);

   FDataPointer[Trunc(ypxl1)     * Width + xpxl1] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[Trunc(ypxl1 + 1) * Width + xpxl1] := Color; // weight = Frac(FltEnd) * gap
   Inter := FltEnd + gradient;

   IntEnd := Round(ToX);
   FltEnd := ToY + gradient * (IntEnd - ToX);
   gap := Frac(ToX + 0.5);
   xpxl2 := IntEnd;
   ypxl2 := Trunc(FltEnd);
   FDataPointer[Trunc(ypxl2)     * Width + xpxl2] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[Trunc(ypxl2 + 1) * Width + xpxl2] := Color; // weight = Frac(FltEnd) * gap

   for Index := xpxl1 + 1 to xpxl2 - 1 do
    begin
     CombineMemory(TPixel32(Color), TPixel32(FDataPointer[Trunc(Inter)     * Width + Index]), Round((1 - Frac(Inter)) * 255));
     CombineMemory(TPixel32(Color), TPixel32(FDataPointer[Trunc(Inter + 1) * Width + Index]), Round(Frac(Inter) * 255));
(*
     FDataPointer[Trunc(Inter)     * Width + Index] := Color; // weight = (1 - Frac(Inter))
     FDataPointer[Trunc(Inter + 1) * Width + Index] := Color; // weight = Frac(Inter)
*)
     Inter := Inter + gradient;
    end;
  end
 else
  begin
   if ToY < FromY then
    begin
     t := FromX;
     FromX := ToX;
     ToX := t;
     t := FromY;
     FromY := ToY;
     ToY := t;
    end;
   Gradient := dx / dy;
   IntEnd   := Round(FromY);
   FltEnd   := FromX + gradient * (IntEnd - FromY);
   gap      := 1 - Frac(FromY + 0.5);
   ypxl1    := IntEnd;
   xpxl1    := Trunc(FltEnd);

   FDataPointer[Trunc(ypxl1)     * Width + xpxl1] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[Trunc(ypxl1 + 1) * Width + xpxl1] := Color; // weight = Frac(FltEnd) * gap)
   Inter := FltEnd + gradient;

   IntEnd := Round(ToY);
   FltEnd := ToX + gradient * (IntEnd - ToY);
   gap := Frac(ToY + 0.5);
   ypxl2 := IntEnd;
   xpxl2 := Trunc(FltEnd);
   FDataPointer[Trunc(ypxl2)     * Width + xpxl2] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[Trunc(ypxl2 + 1) * Width + xpxl2] := Color; // weight = Frac(FltEnd) * gap

   for Index := ypxl1 + 1 to ypxl2 - 1 do
    begin
     FDataPointer[Index * Width + Trunc(Inter)] := Color; // weight = (1 - Frac(Inter))
     FDataPointer[Index * Width + Trunc(Inter + 1)] := Color; // weight = Frac(Inter)
     Inter := Inter + Gradient;
    end;
  end;

(*
 Line(FloatingToFixedPoint(FromX), FloatingToFixedPoint(FromY),
   FloatingToFixedPoint(ToX), FloatingToFixedPoint(ToY), Color);
*)
end;
}

end.
