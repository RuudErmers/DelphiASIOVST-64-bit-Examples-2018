unit DAV_GuiVectorPixelRectangle;

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
  Graphics, Types, Classes, SysUtils, DAV_Common, DAV_FixedPoint, DAV_GuiCommon,
  DAV_GuiFixedPoint, DAV_GuiPixelMap, DAV_GuiVector, DAV_GuiVectorPixel;

type
  TGuiPixelFilledRectangle = class(TCustomGuiPixelFillPrimitive)
  private
    function GetGeometricShape: TGuiRectangle;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;

    property GeometricShape: TGuiRectangle read GetGeometricShape;
  end;

  TGuiPixelFrameRectangle = class(TCustomGuiPixelFramePrimitive)
  private
    function GetGeometricShape: TGuiRectangle;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;

    property GeometricShape: TGuiRectangle read GetGeometricShape;
  end;

  TGuiPixelFilledRoundedRectangle = class(TCustomGuiPixelFillPrimitive)
  private
    function GetGeometricShape: TGuiRoundedRectangle;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;

    property GeometricShape: TGuiRoundedRectangle read GetGeometricShape;
  end;

  TGuiPixelFrameRoundedRectangle = class(TCustomGuiPixelFramePrimitive)
  private
    function GetGeometricShape: TGuiRoundedRectangle;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;

    property GeometricShape: TGuiRoundedRectangle read GetGeometricShape;
  end;


implementation

uses
  DAV_GuiBlend;

{ TGuiPixelFilledRectangle }

constructor TGuiPixelFilledRectangle.Create;
begin
 inherited;
 FGeometricShape := TGuiRectangle.Create;
end;

procedure TGuiPixelFilledRectangle.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
var
  X, Y         : Integer;
  XRange       : array [0..1] of Integer;
  YRange       : array [0..1] of Integer;
  ScnLne       : PPixel32Array;
  PixelColor32 : TPixel32;
begin
 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;

 with PixelMap do
  begin
   with GeometricShape do
    begin
     XRange[0] := FixedFloor(Left);
     XRange[1] := FixedCeil(Right);

     // check whether the bitmap needs to be drawn at all
     if (XRange[0] >= Width) or (XRange[1] < 0) or (XRange[0] >= XRange[1])
      then Exit;

     YRange[0] := FixedRound(Top);
     YRange[1] := FixedRound(Bottom);

     // check whether the bitmap needs to be drawn at all
     if (YRange[0] >= Height) or (YRange[1] < 0) or (YRange[0] >= YRange[1])
      then Exit;
    end;

   // eventually limit range
   if XRange[0] < 0 then XRange[0] := 0;
   if XRange[1] >= Width then XRange[1] := Width - 1;
   if YRange[0] < 0 then YRange[0] := 0;
   if YRange[1] >= Height then YRange[1] := Height - 1;

   for Y := YRange[0] to YRange[1] do
    begin
     ScnLne := PixelMap.Scanline[Y];
     for X := XRange[0] to XRange[1] - 1
      do BlendPixelInplace(PixelColor32, ScnLne[X]);
    end;
   EMMS;
  end;
end;

procedure TGuiPixelFilledRectangle.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
var
  X, Y         : Integer;
  XRange       : array [0..1] of Integer;
  YRange       : array [0..1] of Integer;
  XAntiAlias   : array [0..1] of Integer;
  ScnLne       : PPixel32Array;
  PixelColor32 : TPixel32;
  CurrentAlpha : Byte;
begin
 PixelColor32 := ConvertColor(Color);

 with PixelMap do
  begin
   with GeometricShape do
    begin
     XRange[0] := FixedFloor(Left);
     XRange[1] := FixedCeil(Right);

     // check whether the bitmap needs to be drawn at all
     if (XRange[0] >= Width) or (XRange[1] < 0) or (XRange[0] >= XRange[1])
      then Exit;

     YRange[0] := FixedFloor(Top);
     YRange[1] := FixedCeil(Bottom);

     // check whether the bitmap needs to be drawn at all
     if (YRange[0] >= Height) or (YRange[1] < 0) or (YRange[0] >= YRange[1])
      then Exit;
    end;

   // eventually limit range
   if XRange[0] < 0 then
    begin
     XRange[0] := 0;
     XAntiAlias[0] := 0;
    end else XAntiAlias[0] := 1;
   if XRange[1] >= Width then
    begin
     XRange[1] := Width - 1;
     XAntiAlias[1] := 0;
    end else XAntiAlias[1] := 1;

   if YRange[0] < 0
    then YRange[0] := 0
    else
     begin
      // draw first scanline
      ScnLne := PixelMap.Scanline[YRange[0]];
      CurrentAlpha := Fixed24Dot8Mul($FF - GeometricShape.Top.Frac, Alpha);

      // draw first pixel of this first scanline
      if XAntiAlias[0] <> 0 then
       begin
        PixelColor32.A := Fixed24Dot8Mul($FF - GeometricShape.Left.Frac, CurrentAlpha);
        BlendPixelInplace(PixelColor32, ScnLne[XRange[0]]);
       end;

      // draw middle pixels of this first scanline
      PixelColor32.A := CurrentAlpha;
      for X := XRange[0] + XAntiAlias[0] to XRange[1] - XAntiAlias[1]
       do BlendPixelInplace(PixelColor32, ScnLne[X]);

      // draw last pixel of this first scanline
      if XAntiAlias[1] <> 0 then
       begin
        PixelColor32.A := Fixed24Dot8Mul(GeometricShape.Right.Frac, CurrentAlpha);
        BlendPixelInplace(PixelColor32, ScnLne[XRange[1]]);
       end;

      // increase Y-Range (not to include this first scanline)
      Inc(YRange[0]);
     end;

   if YRange[1] >= Height
    then YRange[1] := Height - 1
    else
     begin
      // draw last scanline
      ScnLne := PixelMap.Scanline[YRange[1]];
      CurrentAlpha := Fixed24Dot8Mul(GeometricShape.Bottom.Frac, Alpha);

      // draw first pixel of this last scanline
      if XAntiAlias[0] <> 0 then
       begin
        PixelColor32.A := Fixed24Dot8Mul($FF - GeometricShape.Left.Frac, CurrentAlpha);
        BlendPixelInplace(PixelColor32, ScnLne[XRange[0]]);
       end;

      // draw middle pixels of this last scanline
      PixelColor32.A := CurrentAlpha;
      for X := XRange[0] + XAntiAlias[0] to XRange[1] - XAntiAlias[1]
       do BlendPixelInplace(PixelColor32, ScnLne[X]);

      // draw last pixel of this last scanline
      if XAntiAlias[1] <> 0 then
       begin
        PixelColor32.A := Fixed24Dot8Mul(GeometricShape.Right.Frac, CurrentAlpha);
        BlendPixelInplace(PixelColor32, ScnLne[XRange[1]]);
       end;

      // increase Y-Range (not to include this last scanline)
      Dec(YRange[1]);
     end;

   for Y := YRange[0] to YRange[1] do
    begin
     ScnLne := PixelMap.Scanline[Y];

     // draw first pixel of this scanline
     if XAntiAlias[0] <> 0 then
      begin
       PixelColor32.A := Fixed24Dot8Mul($FF - GeometricShape.Left.Frac, Alpha);
       BlendPixelInplace(PixelColor32, ScnLne[XRange[0]]);
      end;

     // draw middle pixels of this scanline
     PixelColor32.A := Alpha;
     for X := XRange[0] + XAntiAlias[0] to XRange[1] - XAntiAlias[1]
      do BlendPixelInplace(PixelColor32, ScnLne[X]);

     // draw last pixel of this scanline
     if XAntiAlias[1] <> 0 then
      begin
       PixelColor32.A := Fixed24Dot8Mul(GeometricShape.Right.Frac, Alpha);
       BlendPixelInplace(PixelColor32, ScnLne[XRange[1]]);
      end;
    end;
   EMMS;
  end;
end;

function TGuiPixelFilledRectangle.GetGeometricShape: TGuiRectangle;
begin
 Result := TGuiRectangle(FGeometricShape);
end;


{ TGuiPixelFrameRectangle }

constructor TGuiPixelFrameRectangle.Create;
begin
 inherited;
 FGeometricShape := TGuiRectangle.Create;
end;

function TGuiPixelFrameRectangle.GetGeometricShape: TGuiRectangle;
begin
 Result := TGuiRectangle(FGeometricShape)
end;

procedure TGuiPixelFrameRectangle.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
var
  X, Y         : Integer;
  XRange       : array [0..1] of Integer;
  YRange       : array [0..1] of Integer;
  XAntiAlias   : array [0..1] of Integer;
  ScnLne       : PPixel32Array;
  PixelColor32 : TPixel32;
begin
 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;

 with PixelMap do
  begin
   with GeometricShape do
    begin
     XRange[0] := FixedFloor(Left);
     XRange[1] := FixedCeil(Right);

     // check whether the bitmap needs to be drawn at all
     if (XRange[0] >= Width) or (XRange[1] < 0) or (XRange[0] >= XRange[1])
      then Exit;

     YRange[0] := FixedFloor(Top);
     YRange[1] := FixedCeil(Bottom);

     // check whether the bitmap needs to be drawn at all
     if (YRange[0] >= Height) or (YRange[1] < 0) or (YRange[0] >= YRange[1])
      then Exit;
    end;

   // eventually limit range
   if XRange[0] < 0 then
    begin
     XRange[0] := 0;
     XAntiAlias[0] := 0;
    end else XAntiAlias[0] := 1;
   if XRange[1] >= Width then
    begin
     XRange[1] := Width - 1;
     XAntiAlias[1] := 0;
    end else XAntiAlias[1] := 1;

   if YRange[0] < 0
    then YRange[0] := 0
    else
     begin
      // draw first scanline
      ScnLne := PixelMap.Scanline[YRange[0]];

      // draw first pixel of this first scanline
      if XAntiAlias[0] <> 0
       then BlendPixelInplace(PixelColor32, ScnLne[XRange[0]]);

      // draw middle pixels of this first scanline
      for X := XRange[0] + XAntiAlias[0] to XRange[1] - XAntiAlias[1]
       do BlendPixelInplace(PixelColor32, ScnLne[X]);

      // draw last pixel of this first scanline
      if XAntiAlias[1] <> 0
       then BlendPixelInplace(PixelColor32, ScnLne[XRange[1]]);

      // increase Y-Range (not to include this first scanline)
      Inc(YRange[0]);
     end;

   if YRange[1] >= Height
    then YRange[1] := Height - 1
    else
     begin
      // draw last scanline
      ScnLne := PixelMap.Scanline[YRange[1]];

      // draw first pixel of this last scanline
      if XAntiAlias[0] <> 0
       then BlendPixelInplace(PixelColor32, ScnLne[XRange[0]]);

      // draw middle pixels of this last scanline
      for X := XRange[0] + XAntiAlias[0] to XRange[1] - XAntiAlias[1]
       do BlendPixelInplace(PixelColor32, ScnLne[X]);

      // draw last pixel of this last scanline
      if XAntiAlias[1] <> 0
       then BlendPixelInplace(PixelColor32, ScnLne[XRange[1]]);

      // increase Y-Range (not to include this last scanline)
      Dec(YRange[1]);
     end;

   for Y := YRange[0] to YRange[1] do
    begin
     ScnLne := PixelMap.Scanline[Y];

     // draw first pixel of this scanline
     if XAntiAlias[0] <> 0
      then BlendPixelInplace(PixelColor32, ScnLne[XRange[0]]);

     // draw last pixel of this scanline
     if XAntiAlias[1] <> 0
      then BlendPixelInplace(PixelColor32, ScnLne[XRange[1]]);
    end;
   EMMS;
  end;
end;

procedure TGuiPixelFrameRectangle.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
begin
 DrawDraftShape(PixelMap);
end;


{ TGuiPixelFilledRoundedRectangle }

constructor TGuiPixelFilledRoundedRectangle.Create;
begin
 inherited;
 FGeometricShape := TGuiRoundedRectangle.Create;
end;

procedure TGuiPixelFilledRoundedRectangle.DrawDraftShape(
  PixelMap: TGuiCustomPixelMap);
var
  ScnLn          : PPixel32Array;
  ClipRect       : TRect;
  PixelColor32   : TPixel32;
  RoundedRadius  : Integer;
  MinYDistance   : Integer;
  SqrDist        : TFixed24Dot8;
  SqrYDist       : TFixed24Dot8;
  XOffset, Y     : Integer;
  YRange         : array [0..1] of Integer;
  XRange, XPos   : array [0..1] of Integer;

begin
 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;

 with PixelMap do
  begin
   // assign and check geometric shape properties
   with GeometricShape do
    begin
     RoundedRadius := FixedRound(BorderRadius);
     Assert(RoundedRadius >= 0);

     // assign x coordinates
     ClipRect.Left := FixedRound(Left);
     ClipRect.Right := FixedRound(Right);

     // check whether the bitmap needs to be drawn at all
     if (ClipRect.Left >= Width) or (ClipRect.Right < 0) or
        (ClipRect.Left >= ClipRect.Right)
      then Exit;

     // assign y coordinates
     ClipRect.Top := FixedRound(Top);
     ClipRect.Bottom := FixedRound(Bottom);

     if (ClipRect.Top >= Height) or (ClipRect.Bottom < 0) or
        (ClipRect.Top >= ClipRect.Bottom)
      then Exit;

     // set y-range
     if ClipRect.Top > 0
      then YRange[0] := ClipRect.Top
      else YRange[0] := 0;

     if ClipRect.Bottom < Height - 1
      then YRange[1] := ClipRect.Bottom
      else YRange[1] := Height - 1;

     // set x-range
     if ClipRect.Left > 0
      then XRange[0] := ClipRect.Left
      else XRange[0] := 0;

     if ClipRect.Right < Width - 1
      then XRange[1] := ClipRect.Right
      else XRange[1] := Width - 1;

     // eventually limit round radius
     if 2 * RoundedRadius > ClipRect.Right - ClipRect.Left
      then RoundedRadius := (ClipRect.Right - ClipRect.Left) div 2;
     if 2 * RoundedRadius > ClipRect.Bottom - ClipRect.Top
      then RoundedRadius := (ClipRect.Bottom - ClipRect.Top) div 2;
    end;

   for Y := YRange[0] to YRange[1] do
    begin
     Assert(Y >= 0);
     Assert(Y < Height);
     ScnLn := Scanline[Y];

     XPos[0] := XRange[0];
     XPos[1] := XRange[1];

     // calculate minimal y distance
     MinYDistance := Y - ClipRect.Top;
     if ClipRect.Bottom - Y < MinYDistance
      then MinYDistance := ClipRect.Bottom - Y;

     Assert(MinYDistance >= 0);

     // check whether rounded border offset needs to be applied
     if (MinYDistance < RoundedRadius) then
      begin
       SqrYDist := FixedSqr(ConvertToFixed24Dot8(Integer(RoundedRadius - MinYDistance)));
       SqrDist := FixedSub(ConvertToFixed24Dot8(Sqr(RoundedRadius)), SqrYDist);
       Assert(SqrDist.Fixed >= 0);
       XOffset := RoundedRadius - FixedRound(FixedSub(FixedSqrt(SqrDist), CFixed24Dot8Half));

       // eventually change left offset
       if XPos[0] - ClipRect.Left < RoundedRadius then
        begin
         if XPos[0] - ClipRect.Left < XOffset
          then XPos[0] := ClipRect.Left + XOffset;
        end;

       // eventually change right offset
       if ClipRect.Right - XPos[1] < RoundedRadius then
        begin
         if ClipRect.Right - XPos[1] < XOffset
          then XPos[1] := ClipRect.Right - XOffset;
        end;
      end;

     Assert(XPos[0] >= 0);
     Assert(XPos[1] < Width);
     Assert(XPos[1] - XPos[0] < Width);

     if XPos[1] > XPos[0]
      then BlendPixelLine(PixelColor32, @ScnLn[XPos[0]], XPos[1] - XPos[0] + 1);
    end;

   EMMS;
  end;
end;

procedure TGuiPixelFilledRoundedRectangle.DrawFixedPoint(
  PixelMap: TGuiCustomPixelMap);
var
  ScnLn           : PPixel32Array;
  PixelColor32    : TPixel32;
  TempColor32     : TPixel32;
  OriginalLeft    : TFixed24Dot8;
  OriginalTop     : TFixed24Dot8;
  OriginalRight   : TFixed24Dot8;
  OriginalBottom  : TFixed24Dot8;
  RoundedRadius   : TFixed24Dot8;
  RadiusPlusOne   : TFixed24Dot8;
  SqrRadius       : TFixed24Dot8;
  SqrRadPlusOne   : TFixed24Dot8;
  SqrRadMinusOne  : TFixed24Dot8;
  MinYDistance    : TFixed24Dot8;
  YDistPx2Ctr     : TFixed24Dot8;
  SqrDistPx2Ctr   : TFixed24Dot8;
  SqrYDistPx2Ctr  : TFixed24Dot8;
  Y               : Integer;
  YRange          : array [0..1] of Integer;
  YFixed          : TFixed24Dot8;
  XDistance       : TFixed24Dot8;
  XOffset         : TFixed24Dot8;
  Temp            : TFixed24Dot8;
  XPos            : array [0..1] of TFixed24Dot8;
  XRange          : array [0..1] of TFixed24Dot8;
  X               : array [0..1] of Integer;
  XAlpha, YAlpha  : Integer;

begin
 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;

 with PixelMap do
  begin
   // assign and check geometric shape properties
   with GeometricShape do
    begin
     RoundedRadius := BorderRadius;

     // assign x coordinates
     OriginalLeft := Left;
     OriginalRight := Right;

     // check whether the bitmap needs to be drawn at all
     if (FixedFloor(OriginalLeft) >= Width) or (FixedCeil(OriginalRight) < 0)
       or (FixedFloor(OriginalLeft) >= FixedCeil(OriginalRight))
      then Exit;

     // assign y coordinates
     OriginalTop := Top;
     OriginalBottom := Bottom;

     if (FixedFloor(OriginalTop) >= Height) or (FixedCeil(OriginalBottom) < 0)
       or (FixedFloor(OriginalTop) >= FixedCeil(OriginalBottom))
      then Exit;
    end;

   // set y-range
   if OriginalTop.Fixed > 0
    then YRange[0] := FixedFloor(OriginalTop)
    else YRange[0] := 0;

   if OriginalBottom.Fixed < ConvertToFixed24Dot8(Integer(Height - 1)).Fixed
    then YRange[1] := FixedCeil(OriginalBottom)
    else YRange[1] := Height - 1;

   // set x-range
   if OriginalLeft.Fixed > 0
    then XRange[0] := OriginalLeft
    else XRange[0].Fixed := 0;

   if OriginalRight.Fixed < ConvertToFixed24Dot8(Integer(Width - 1)).Fixed
    then XRange[1] := OriginalRight
    else XRange[1] := ConvertToFixed24Dot8(Integer(Width - 1));

   // eventually limit round radius
   if FixedAdd(RoundedRadius, RoundedRadius).Fixed > FixedSub(OriginalRight, OriginalLeft).Fixed
    then RoundedRadius := FixedDiv(FixedSub(OriginalRight, OriginalLeft), CFixed24Dot8Two);
   if FixedAdd(RoundedRadius, RoundedRadius).Fixed > FixedSub(OriginalBottom, OriginalTop).Fixed
    then RoundedRadius := FixedDiv(FixedSub(OriginalBottom, OriginalTop), CFixed24Dot8Two);

   // calculate squared circle radius
   RadiusPlusOne := FixedAdd(RoundedRadius, CFixed24Dot8One);
   SqrRadius := FixedSqr(RoundedRadius);
   SqrRadPlusOne := FixedSqr(RadiusPlusOne);
   SqrRadMinusOne := FixedSqr(FixedSub(RoundedRadius, CFixed24Dot8One));

   for Y := YRange[0] to YRange[1] do
    begin
     Assert(Y >= 0);
     Assert(Y < Height);
     ScnLn := Scanline[Y];

     XPos[0] := XRange[0];
     XPos[1] := XRange[1];

     // calculate fixed point Y
     YFixed := ConvertToFixed24Dot8(Y);

     // calculate y alpha
     if Y = YRange[0]
      then YAlpha := $FF - (OriginalTop.Fixed - YFixed.Fixed) else
     if Y = YRange[1]
      then YAlpha := $FF - (YFixed.Fixed - OriginalBottom.Fixed)
      else YAlpha := $FF;

     // calculate minimal y distance (either distance to upper or lower bound)
     MinYDistance := FixedSub(YFixed, OriginalTop);
     if FixedSub(OriginalBottom, YFixed).Fixed < MinYDistance.Fixed
      then MinYDistance := FixedSub(OriginalBottom, YFixed);

     // check whether rounded border offset needs to be applied
     if (MinYDistance.Fixed < RoundedRadius.Fixed) then
      begin
       // calculate squared vertical distance
       Temp := FixedSub(RoundedRadius, MinYDistance);
       SqrYDistPx2Ctr := FixedSqr(Temp);

       // calculate (squared) distance from center to current y position
       SqrDistPx2Ctr := FixedSub(SqrRadPlusOne, SqrYDistPx2Ctr);
       if SqrDistPx2Ctr.Fixed < 0
        then XDistance.Fixed := 0
        else XDistance := FixedSqrt(SqrDistPx2Ctr);
       XOffset := FixedSub(RoundedRadius, XDistance);

       // eventually change left offset
       if FixedSub(XPos[0], OriginalLeft).Fixed < RoundedRadius.Fixed then
        begin
         if FixedSub(XPos[0], OriginalLeft).Fixed < XOffset.Fixed
          then XPos[0] := FixedAdd(OriginalLeft, XOffset);
        end;

       // eventually change right offset
       if FixedSub(OriginalRight, XPos[1]).Fixed < RoundedRadius.Fixed then
        begin
         if FixedSub(OriginalRight, XPos[1]).Fixed < XOffset.Fixed
          then XPos[1] := FixedSub(OriginalRight, XOffset);
        end;

       if XPos[1].Fixed > XPos[0].Fixed then
        begin
         // define pixel range
         X[0] := FixedFloor(XPos[0]);
         X[1] := FixedCeil(XPos[1]);
         Assert(X[0] < X[1]);

         // draw first pixels
         TempColor32 := PixelColor32;
         repeat
          // calculate squared distance
          Temp.Fixed := OriginalLeft.Fixed + RoundedRadius.Fixed - X[0] shl 8;
          if Temp.Fixed < 0 then Break;
          SqrDistPx2Ctr.Fixed := FixedSqr(Temp).Fixed + SqrYDistPx2Ctr.Fixed;
          if SqrDistPx2Ctr.Fixed >= SqrRadMinusOne.Fixed then
           begin
            Temp := FixedSqrt(SqrDistPx2Ctr);
            Temp.Fixed := RadiusPlusOne.Fixed - Temp.Fixed;
            if Temp.Fixed < 0 then
             begin
              Inc(X[0]);
              Continue;
             end;
            XAlpha := Temp.Fixed;
            if Temp.Fixed >= $FF
             then Break;
            if YAlpha < $FF
             then TempColor32.A := (PixelColor32.A * YAlpha * XAlpha) div $FE01
             else TempColor32.A := (PixelColor32.A * XAlpha) shr 8;
           end;
          BlendPixelInplace(TempColor32, ScnLn[X[0]]);
          Inc(X[0]);
          if X[0] > X[1] then Exit;
         until SqrDistPx2Ctr.Fixed < SqrRadMinusOne.Fixed;

         // draw last pixels
         TempColor32 := PixelColor32;
         repeat
          // calculate squared distance
          Temp.Fixed := X[1] shl 8 - (OriginalRight.Fixed - RoundedRadius.Fixed);
          if Temp.Fixed < 0 then Break;
          SqrDistPx2Ctr.Fixed := FixedSqr(Temp).Fixed + SqrYDistPx2Ctr.Fixed;
          if SqrDistPx2Ctr.Fixed >= SqrRadMinusOne.Fixed then
           begin
            Temp := FixedSqrt(SqrDistPx2Ctr);
            Temp.Fixed := RadiusPlusOne.Fixed - Temp.Fixed;
            if Temp.Fixed < 0 then
             begin
              Dec(X[1]);
              Continue;
             end;
            XAlpha := Temp.Fixed;
            if Temp.Fixed >= $FF
             then Break;
            if YAlpha < $FF
             then TempColor32.A := (PixelColor32.A * YAlpha * XAlpha) div $FE01
             else TempColor32.A := (PixelColor32.A * XAlpha) shr 8;
           end;
          BlendPixelInplace(TempColor32, ScnLn[X[1]]);
          Dec(X[1]);
          if X[1] < X[0] then Exit;
         until SqrDistPx2Ctr.Fixed < SqrRadMinusOne.Fixed;

         TempColor32 := PixelColor32;
         if YAlpha < $FF
          then TempColor32.A := (PixelColor32.A * YAlpha + $7F) shr 8;

         if X[1] >= X[0]
          then BlendPixelLine(TempColor32, @ScnLn[X[0]], X[1] - X[0] + 1);
        end;
      end
     else
      begin
       Assert(XPos[0].Fixed >= 0);
       Assert(XPos[1].Fixed < ConvertToFixed24Dot8(Width).Fixed);

       if XPos[1].Fixed > XPos[0].Fixed then
        begin
         // define integer pixel range
         X[0] := FixedFloor(XPos[0]);
         X[1] := FixedFloor(XPos[1]) + 1;
         Assert(X[0] < X[1]);

         // draw first pixels
         XAlpha := ($FF - XPos[0].Frac);
         TempColor32 := PixelColor32;
         if YAlpha < $FF
          then TempColor32.A := (PixelColor32.A * YAlpha * XAlpha) div $FE01
          else TempColor32.A := (PixelColor32.A * XAlpha) shr 8;
         BlendPixelInplace(TempColor32, ScnLn[X[0]]);

         // draw last pixels
         XAlpha := XPos[1].Frac;
         TempColor32 := PixelColor32;
         if YAlpha < $FF
          then TempColor32.A := (PixelColor32.A * YAlpha * XAlpha) div $FE01
          else TempColor32.A := (PixelColor32.A * XAlpha) shr 8;
         BlendPixelInplace(TempColor32, ScnLn[X[1]]);

         TempColor32 := PixelColor32;
         if YAlpha < $FF
          then TempColor32.A := (PixelColor32.A * YAlpha + $7F) shr 8;

         if X[1] - X[0] - 1 > 0
          then BlendPixelLine(TempColor32, @ScnLn[X[0] + 1], X[1] - X[0] - 1);
        end;
      end;
    end;

   EMMS;
  end;
end;

function TGuiPixelFilledRoundedRectangle.GetGeometricShape: TGuiRoundedRectangle;
begin
 Result := TGuiRoundedRectangle(FGeometricShape);
end;


{ TGuiPixelFrameRoundedRectangle }

constructor TGuiPixelFrameRoundedRectangle.Create;
begin
 inherited;
 FGeometricShape := TGuiRoundedRectangle.Create;
end;

procedure TGuiPixelFrameRoundedRectangle.DrawDraftShape(
  PixelMap: TGuiCustomPixelMap);
var
  ScnLn          : PPixel32Array;
  ClipRect       : TRect;
  PixelColor32   : TPixel32;
  RoundedRadius  : Integer;
  RadiusMinusOne : Integer;
  MinYDistance   : Integer;
  SqrDist        : TFixed24Dot8;
  SqrYDist       : TFixed24Dot8;
  XOffset, Y     : Integer;
  XCount         : Integer;
  YRange         : array [0..1] of Integer;
  XRange, XPos   : array [0..1] of Integer;

begin
 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;

 with PixelMap do
  begin
   // assign and check geometric shape properties
   with GeometricShape do
    begin
     RoundedRadius := FixedRound(BorderRadius);
     RadiusMinusOne := RoundedRadius - 1;
     Assert(RoundedRadius >= 0);

     // assign x coordinates
     ClipRect.Left := FixedRound(Left);
     ClipRect.Right := FixedRound(Right);

     // check whether the bitmap needs to be drawn at all
     if (ClipRect.Left >= Width) or (ClipRect.Right < 0) or
        (ClipRect.Left >= ClipRect.Right)
      then Exit;

     // assign y coordinates
     ClipRect.Top := FixedRound(Top);
     ClipRect.Bottom := FixedRound(Bottom);

     if (ClipRect.Top >= Height) or (ClipRect.Bottom < 0) or
        (ClipRect.Top >= ClipRect.Bottom)
      then Exit;

     // set y-range
     if ClipRect.Top > 0
      then YRange[0] := ClipRect.Top
      else YRange[0] := 0;

     if ClipRect.Bottom < Height - 1
      then YRange[1] := ClipRect.Bottom
      else YRange[1] := Height - 1;

     // set x-range
     if ClipRect.Left > 0
      then XRange[0] := ClipRect.Left
      else XRange[0] := 0;

     if ClipRect.Right < Width - 1
      then XRange[1] := ClipRect.Right
      else XRange[1] := Width - 1;

     // eventually limit round radius
     if 2 * RoundedRadius > ClipRect.Right - ClipRect.Left
      then RoundedRadius := (ClipRect.Right - ClipRect.Left) div 2;
     if 2 * RoundedRadius > ClipRect.Bottom - ClipRect.Top
      then RoundedRadius := (ClipRect.Bottom - ClipRect.Top) div 2;
    end;

   for Y := YRange[0] to YRange[1] do
    begin
     Assert(Y >= 0);
     Assert(Y < Height);
     ScnLn := Scanline[Y];

     XPos[0] := XRange[0];
     XPos[1] := XRange[1];

     // calculate minimal y distance
     MinYDistance := Y - ClipRect.Top;
     if ClipRect.Bottom - Y < MinYDistance
      then MinYDistance := ClipRect.Bottom - Y;

     Assert(MinYDistance >= 0);

     // check whether rounded border offset needs to be applied
     if (MinYDistance < RoundedRadius) then
      begin
       SqrYDist := FixedSqr(ConvertToFixed24Dot8(Integer(RoundedRadius - MinYDistance)));
       SqrDist := FixedSub(ConvertToFixed24Dot8(Sqr(RoundedRadius)), SqrYDist);
       Assert(SqrDist.Fixed >= 0);
       XOffset := FixedRound(FixedSub(FixedSqrt(SqrDist), CFixed24Dot8Half));

       SqrYDist := FixedSqr(ConvertToFixed24Dot8(Integer(RoundedRadius - MinYDistance)));
       SqrDist := FixedSub(ConvertToFixed24Dot8(Sqr(RadiusMinusOne)), SqrYDist);
       if SqrDist.Fixed < 0 then SqrDist.Fixed := 0;
       XCount := XOffset - FixedRound(FixedSub(FixedSqrt(SqrDist), CFixed24Dot8Half));

       XOffset := RoundedRadius - XOffset;
       // eventually change left offset
       if XPos[0] - ClipRect.Left < RoundedRadius then
        begin
         if XPos[0] - ClipRect.Left < XOffset
          then XPos[0] := ClipRect.Left + XOffset;
        end;

       // eventually change right offset
       if ClipRect.Right - XPos[1] < RoundedRadius then
        begin
         if ClipRect.Right - XPos[1] < XOffset
          then XPos[1] := ClipRect.Right - XOffset;
        end;
      end;

     Assert(XPos[0] >= 0);
     Assert(XPos[1] < Width);
     Assert(XPos[1] - XPos[0] < Width);

     if XPos[1] > XPos[0] then
      begin
       if Y = YRange[0]
        then BlendPixelLine(PixelColor32, @ScnLn[XPos[0]], XPos[1] - XPos[0] + 1) else
       if Y = YRange[1]
        then BlendPixelLine(PixelColor32, @ScnLn[XPos[0]], XPos[1] - XPos[0] + 1)
        else
         begin
//          if ClipRect.Left  then

          BlendPixelInplace(PixelColor32, ScnLn[XPos[0]]);
          BlendPixelInplace(PixelColor32, ScnLn[XPos[1]]);

(*
          XCount := XCount - 1;
          if XCount > (XPos[1] - XPos[0] - 1)
           then XCount := (XPos[1] - XPos[0] - 1);

          if XCount > 0 then
           begin
            BlendPixelLine(PixelColor32, @ScnLn[XPos[0] + 1], XCount);
           end;
*)
         end;
      end;
    end;

   EMMS;
  end;
end;

procedure TGuiPixelFrameRoundedRectangle.DrawFixedPoint(
  PixelMap: TGuiCustomPixelMap);
var
  ScnLn           : PPixel32Array;
  PixelColor32    : TPixel32;
  TempColor32     : TPixel32;
  OriginalLeft    : TFixed24Dot8;
  OriginalTop     : TFixed24Dot8;
  OriginalRight   : TFixed24Dot8;
  OriginalBottom  : TFixed24Dot8;
  RoundedRadius   : TFixed24Dot8;
  RadiusPlusOne   : TFixed24Dot8;
  SqrRadius       : TFixed24Dot8;
  SqrRadPlusOne   : TFixed24Dot8;
  SqrRadMinusOne  : TFixed24Dot8;
  MinYDistance    : TFixed24Dot8;
  YDistPx2Ctr     : TFixed24Dot8;
  SqrDistPx2Ctr   : TFixed24Dot8;
  SqrYDistPx2Ctr  : TFixed24Dot8;
  Y               : Integer;
  YRange          : array [0..1] of Integer;
  YFixed          : TFixed24Dot8;
  XDistance       : TFixed24Dot8;
  XOffset         : TFixed24Dot8;
  Temp            : TFixed24Dot8;
  XPos            : array [0..1] of TFixed24Dot8;
  XRange          : array [0..1] of TFixed24Dot8;
  X               : array [0..1] of Integer;
  XAlpha, YAlpha  : Integer;

begin
 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;

 with PixelMap do
  begin
   // assign and check geometric shape properties
   with GeometricShape do
    begin
     RoundedRadius := BorderRadius;

     // assign x coordinates
     OriginalLeft := Left;
     OriginalRight := Right;

     // check whether the bitmap needs to be drawn at all
     if (FixedFloor(OriginalLeft) >= Width) or (FixedCeil(OriginalRight) < 0)
       or (FixedFloor(OriginalLeft) >= FixedCeil(OriginalRight))
      then Exit;

     // assign y coordinates
     OriginalTop := Top;
     OriginalBottom := Bottom;

     if (FixedFloor(OriginalTop) >= Height) or (FixedCeil(OriginalBottom) < 0)
       or (FixedFloor(OriginalTop) >= FixedCeil(OriginalBottom))
      then Exit;
    end;

   // set y-range
   if OriginalTop.Fixed > 0
    then YRange[0] := FixedFloor(OriginalTop)
    else YRange[0] := 0;

   if OriginalBottom.Fixed < ConvertToFixed24Dot8(Integer(Height - 1)).Fixed
    then YRange[1] := FixedCeil(OriginalBottom)
    else YRange[1] := Height - 1;

   // set x-range
   if OriginalLeft.Fixed > 0
    then XRange[0] := OriginalLeft
    else XRange[0].Fixed := 0;

   if OriginalRight.Fixed < ConvertToFixed24Dot8(Integer(Width - 1)).Fixed
    then XRange[1] := OriginalRight
    else XRange[1] := ConvertToFixed24Dot8(Integer(Width - 1));

   // eventually limit round radius
   if FixedAdd(RoundedRadius, RoundedRadius).Fixed > FixedSub(OriginalRight, OriginalLeft).Fixed
    then RoundedRadius := FixedDiv(FixedSub(OriginalRight, OriginalLeft), CFixed24Dot8Two);
   if FixedAdd(RoundedRadius, RoundedRadius).Fixed > FixedSub(OriginalBottom, OriginalTop).Fixed
    then RoundedRadius := FixedDiv(FixedSub(OriginalBottom, OriginalTop), CFixed24Dot8Two);

   // calculate squared circle radius
   RadiusPlusOne := FixedAdd(RoundedRadius, CFixed24Dot8One);
   SqrRadius := FixedSqr(RoundedRadius);
   SqrRadPlusOne := FixedSqr(RadiusPlusOne);
   SqrRadMinusOne := FixedSqr(FixedSub(RoundedRadius, CFixed24Dot8One));

   for Y := YRange[0] to YRange[1] do
    begin
     Assert(Y >= 0);
     Assert(Y < Height);
     ScnLn := Scanline[Y];

     XPos[0] := XRange[0];
     XPos[1] := XRange[1];

     // calculate fixed point Y
     YFixed := ConvertToFixed24Dot8(Y);

     // calculate y alpha
     if Y = YRange[0]
      then YAlpha := $FF - (OriginalTop.Fixed - YFixed.Fixed) else
     if Y = YRange[1]
      then YAlpha := $FF - (YFixed.Fixed - OriginalBottom.Fixed)
      else YAlpha := $FF;

     // calculate minimal y distance (either distance to upper or lower bound)
     MinYDistance := FixedSub(YFixed, OriginalTop);
     if FixedSub(OriginalBottom, YFixed).Fixed < MinYDistance.Fixed
      then MinYDistance := FixedSub(OriginalBottom, YFixed);

     // check whether rounded border offset needs to be applied
     if (MinYDistance.Fixed < RoundedRadius.Fixed) then
      begin
       // calculate squared vertical distance
       Temp := FixedSub(RoundedRadius, MinYDistance);
       SqrYDistPx2Ctr := FixedSqr(Temp);

       // calculate (squared) distance from center to current y position
       SqrDistPx2Ctr := FixedSub(SqrRadPlusOne, SqrYDistPx2Ctr);
       if SqrDistPx2Ctr.Fixed < 0
        then XDistance.Fixed := 0
        else XDistance := FixedSqrt(SqrDistPx2Ctr);
       XOffset := FixedSub(RoundedRadius, XDistance);

       // eventually change left offset
       if FixedSub(XPos[0], OriginalLeft).Fixed < RoundedRadius.Fixed then
        begin
         if FixedSub(XPos[0], OriginalLeft).Fixed < XOffset.Fixed
          then XPos[0] := FixedAdd(OriginalLeft, XOffset);
        end;

       // eventually change right offset
       if FixedSub(OriginalRight, XPos[1]).Fixed < RoundedRadius.Fixed then
        begin
         if FixedSub(OriginalRight, XPos[1]).Fixed < XOffset.Fixed
          then XPos[1] := FixedSub(OriginalRight, XOffset);
        end;

       if XPos[1].Fixed > XPos[0].Fixed then
        begin
         // define pixel range
         X[0] := FixedFloor(XPos[0]);
         X[1] := FixedCeil(XPos[1]);
         Assert(X[0] < X[1]);

         // draw first pixels
         TempColor32 := PixelColor32;
         repeat
          // calculate squared distance
          Temp.Fixed := OriginalLeft.Fixed + RoundedRadius.Fixed - X[0] shl 8;
          if Temp.Fixed < 0 then Break;
          SqrDistPx2Ctr.Fixed := FixedSqr(Temp).Fixed + SqrYDistPx2Ctr.Fixed;
          if SqrDistPx2Ctr.Fixed >= SqrRadMinusOne.Fixed then
           begin
            Temp := FixedSqrt(SqrDistPx2Ctr);
            Temp.Fixed := RadiusPlusOne.Fixed - Temp.Fixed;
            if Temp.Fixed < 0 then
             begin
              Inc(X[0]);
              Continue;
             end;
            XAlpha := Temp.Fixed;
            if Temp.Fixed >= $FF
             then Break;
            if YAlpha < $FF
             then TempColor32.A := (PixelColor32.A * YAlpha * XAlpha) div $FE01
             else TempColor32.A := (PixelColor32.A * XAlpha) shr 8;
           end;
          BlendPixelInplace(TempColor32, ScnLn[X[0]]);
          Inc(X[0]);
          if X[0] > X[1] then Exit;
         until SqrDistPx2Ctr.Fixed < SqrRadMinusOne.Fixed;

         // draw last pixels
         TempColor32 := PixelColor32;
         repeat
          // calculate squared distance
          Temp.Fixed := X[1] shl 8 - (OriginalRight.Fixed - RoundedRadius.Fixed);
          if Temp.Fixed < 0 then Break;
          SqrDistPx2Ctr.Fixed := FixedSqr(Temp).Fixed + SqrYDistPx2Ctr.Fixed;
          if SqrDistPx2Ctr.Fixed >= SqrRadMinusOne.Fixed then
           begin
            Temp := FixedSqrt(SqrDistPx2Ctr);
            Temp.Fixed := RadiusPlusOne.Fixed - Temp.Fixed;
            if Temp.Fixed < 0 then
             begin
              Dec(X[1]);
              Continue;
             end;
            XAlpha := Temp.Fixed;
            if Temp.Fixed >= $FF
             then Break;
            if YAlpha < $FF
             then TempColor32.A := (PixelColor32.A * YAlpha * XAlpha) div $FE01
             else TempColor32.A := (PixelColor32.A * XAlpha) shr 8;
           end;
          BlendPixelInplace(TempColor32, ScnLn[X[1]]);
          Dec(X[1]);
          if X[1] < X[0] then Exit;
         until SqrDistPx2Ctr.Fixed < SqrRadMinusOne.Fixed;

         TempColor32 := PixelColor32;
         if YAlpha < $FF
          then TempColor32.A := (PixelColor32.A * YAlpha + $7F) shr 8;

         if X[1] >= X[0] then
          begin
           if (Y = YRange[0]) or (Y = YRange[1])
            then BlendPixelLine(TempColor32, @ScnLn[X[0]], X[1] - X[0] + 1);
          end;
        end;
      end
     else
      begin
       Assert(XPos[0].Fixed >= 0);
       Assert(XPos[1].Fixed < ConvertToFixed24Dot8(Width).Fixed);

       if XPos[1].Fixed > XPos[0].Fixed then
        begin
         // define integer pixel range
         X[0] := FixedFloor(XPos[0]);
         X[1] := FixedFloor(XPos[1]) + 1;
         Assert(X[0] < X[1]);

         // draw first pixels
         XAlpha := ($FF - XPos[0].Frac);
         TempColor32 := PixelColor32;
         if YAlpha < $FF
          then TempColor32.A := (PixelColor32.A * YAlpha * XAlpha) div $FE01
          else TempColor32.A := (PixelColor32.A * XAlpha) shr 8;
         BlendPixelInplace(TempColor32, ScnLn[X[0]]);

         // draw last pixels
         XAlpha := XPos[1].Frac;
         TempColor32 := PixelColor32;
         if YAlpha < $FF
          then TempColor32.A := (PixelColor32.A * YAlpha * XAlpha) div $FE01
          else TempColor32.A := (PixelColor32.A * XAlpha) shr 8;
         BlendPixelInplace(TempColor32, ScnLn[X[1]]);

         TempColor32 := PixelColor32;
         if YAlpha < $FF
          then TempColor32.A := (PixelColor32.A * YAlpha + $7F) shr 8;
        end;
      end;
    end;

   EMMS;
  end;
end;

function TGuiPixelFrameRoundedRectangle.GetGeometricShape: TGuiRoundedRectangle;
begin
 Result := TGuiRoundedRectangle(FGeometricShape);
end;

end.
