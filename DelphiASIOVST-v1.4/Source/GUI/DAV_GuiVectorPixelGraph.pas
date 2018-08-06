unit DAV_GuiVectorPixelGraph;

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
  Graphics, Classes, SysUtils, DAV_Types, DAV_FixedPoint, DAV_GuiCommon, 
  DAV_GuiPixelMap, DAV_GuiVector, DAV_GuiVectorPixel;

type
  TGuiPixelEquallySpacedPolyline = class(TCustomGuiPixelFramePrimitive)
  private
    FYValues : array of Single;
    function GetGeometricShape: TGuiEquallySpacedPolyline;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
    procedure LineWidthChanged; override;
  public
    constructor Create; override;

    property GeometricShape: TGuiEquallySpacedPolyline read GetGeometricShape;
  end;

implementation

uses
  Math, DAV_Common, DAV_MemoryUtils, DAV_GuiBlend;

{ TGuiPixelEquallySpacedPolyline }

constructor TGuiPixelEquallySpacedPolyline.Create;
begin
 inherited;
 FGeometricShape := TGuiEquallySpacedPolyline.Create;
end;

procedure TGuiPixelEquallySpacedPolyline.LineWidthChanged;
begin
  inherited;

end;

procedure TGuiPixelEquallySpacedPolyline.DrawDraftShape(
  PixelMap: TGuiCustomPixelMap);
var
  SolidRange      : array [0..1] of Integer;
  YRange          : array [0..1] of Integer;
  IntegerRadiusX  : Integer;
  IntegerRadiusY  : Integer;
  x, y            : Integer;
  PtIndex         : Integer;

  YValues         : array of TFixed24Dot8;
  Distance        : TFixed24Dot8;
  IntLineWdth     : TFixed24Dot8;
  RadiusMinusHalf : TFixed24Dot8;
  CurrentValue    : TFixed24Dot8;
  YStartPos       : TFixed24Dot8;
  YEndPos         : TFixed24Dot8;
  WidthScale      : TFixed24Dot8;
  PointPtr        : PFixed24Dot8Array;
  PxColor         : TPixel32;
  LeftRightIdx    : Integer;


  procedure AddToSolidRange(Lower, Upper: Integer);
  begin
   if Lower < Upper then
    begin
     if Lower < SolidRange[0] then SolidRange[0] := Lower;
     if Upper > SolidRange[1] then SolidRange[1] := Upper;
    end;
  end;

begin
 if Assigned(GeometricShape.OnGetValue) then
  with PixelMap do
   begin
    PxColor   := ConvertColor(Color);
    PxColor.A := Alpha;

    IntLineWdth.Fixed := Max(FixedSub(LineWidth, CFixed24Dot8One).Fixed, 0);
    RadiusMinusHalf := FixedMul(IntLineWdth, CFixed24Dot8Half);

    // initialize temporaty variables
    IntegerRadiusX := 1 + FixedCeil(RadiusMinusHalf);
    IntegerRadiusY := 2 + FixedFloor(RadiusMinusHalf);
    SetLength(YValues, 1 + 2 * IntegerRadiusX);
    Assert(Length(YValues) mod 2 = 1);
    PointPtr := @YValues[IntegerRadiusX];

    // fill additional points
    for PtIndex := 0 to Length(YValues) - 1
     do YValues[PtIndex].Fixed := GeometricShape.OnGetValue(Self, PtIndex - IntegerRadiusX).Fixed;

    for x := GeometricShape.MarginLeft to Width - GeometricShape.MarginRight - 1 do
     begin
      // get next value
      YValues[Length(YValues) - 1].Fixed := GeometricShape.OnGetValue(Self, x + IntegerRadiusX).Fixed;

      // calculate solid range
      CurrentValue := PointPtr^[0];
      SolidRange[0] := FixedRound(FixedSub(CurrentValue, RadiusMinusHalf));
      SolidRange[1] := FixedRound(FixedAdd(CurrentValue, RadiusMinusHalf));

      // check for the solid range
      for PtIndex := 1 to IntegerRadiusY - 2 do
       begin
        // calculate distance
        Distance := FixedSqrt(FixedSub(FixedSqr(RadiusMinusHalf),
          FixedSqr(ConvertToFixed24Dot8(PtIndex))));

        for LeftRightIdx := 0 to 1 do
         begin
          CurrentValue := PointPtr^[(2 * LeftRightIdx - 1) * PtIndex];

          // quick check for rectangle
          Y := FixedRound(FixedSub(CurrentValue, Distance));
          if Y < SolidRange[0]
           then SolidRange[0] := Y
           else
            begin
             Y := FixedRound(FixedAdd(CurrentValue, Distance));
             if Y > SolidRange[1]
              then SolidRange[1] := Y;
            end;
         end;
       end;

      // calculate width scale (0 < x <= 1)
      WidthScale := FixedSub(RadiusMinusHalf, ConvertToFixed24Dot8(Integer(IntegerRadiusX - 2)));

      if IntegerRadiusY = IntegerRadiusX then
       for LeftRightIdx := 0 to 1 do
        begin
         // set start/end values (left/right)
         YStartPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusX - 2)];
         YEndPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusX - 1)];

         // calculate split point
         Distance := FixedSub(YStartPos, FixedMul(WidthScale, FixedSub(YStartPos, YEndPos)));
         CurrentValue := FixedAdd(YEndPos,
           ((FixedMul(FixedSub(CFixed24Dot8Half, WidthScale), FixedSub(Distance, YEndPos)))));

         Y := FixedRound(YStartPos);
         if YStartPos.Fixed <= YEndPos.Fixed then
          if FixedRound(CurrentValue) <= FixedRound(YEndPos)
           then AddToSolidRange(Y, FixedRound(CurrentValue))
           else AddToSolidRange(Y, FixedRound(YEndPos) - 1)
         else
          if FixedRound(CurrentValue) > FixedRound(YEndPos)
           then AddToSolidRange(FixedRound(CurrentValue) + 1, Y)
           else AddToSolidRange(FixedRound(YEndPos), Y);
        end;

       for LeftRightIdx := 0 to 1 do
        begin
         // set start/end values (left/right)
         YStartPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusX - 1)];
         YEndPos := PointPtr^[(2 * LeftRightIdx - 1) * IntegerRadiusX];

         // calculate split point
         CurrentValue := FixedSub(YStartPos, YEndPos);
         Distance := FixedSub(YStartPos, FixedMul(WidthScale, CurrentValue));
         CurrentValue := FixedAdd(Distance, FixedMul(CFixed24Dot8Half, CurrentValue));

         Y := FixedRound(YStartPos);
         if YStartPos.Fixed <= YEndPos.Fixed then
          if FixedRound(CurrentValue) < FixedRound(Distance)
           then AddToSolidRange(Y, FixedRound(CurrentValue))
           else AddToSolidRange(Y, FixedRound(Distance) - 1)
         else
          if FixedRound(CurrentValue) >= FixedRound(Distance)
           then AddToSolidRange(FixedRound(CurrentValue) + 1, Y)
           else AddToSolidRange(FixedRound(Distance), Y);
        end;

      // copy line to pixel map
      if SolidRange[0] < GeometricShape.MarginTop
       then YRange[0] := GeometricShape.MarginTop
       else YRange[0] := SolidRange[0];

      if SolidRange[1] > Height - GeometricShape.MarginBottom - 1
       then YRange[1] := Height - GeometricShape.MarginBottom - 1
       else YRange[1] := SolidRange[1];

      for y := YRange[0] to YRange[1]
       do BlendPixelInplace(PxColor, PixelPointer[x, y]^);
      EMMS;

      // shift y-values
      Move(YValues[1], YValues[0], (Length(YValues) - 1) * SizeOf(TFixed24Dot8));
     end;
   end;
end;

procedure TGuiPixelEquallySpacedPolyline.DrawFixedPoint(
  PixelMap: TGuiCustomPixelMap);
var
  Distance        : Single;
  IntLineWdth     : Single;
  Radius          : Single;
  RadiusMinusHalf : Single;
  SqrRadius       : Single;
  SqrDist         : Single;

  Sum, Mn, Mx     : Single;
  Value           : Single;
  XPos            : Single;
  YDest, YSrc     : Single;

  CurrentValue    : Single;

  IntegerRadiusX  : Integer;
  IntegerRadiusY  : Integer;
  YBounds         : array [0..1] of Integer;
  x, y            : Integer;
  PtIndex, PtSgn  : Integer;

  VertLine        : PByteArray;
  PointPtr        : PDAVSingleFixedArray;
  PixelColor32    : TPixel32;
  CombColor       : TPixel32;
  LeftRightIdx    : Integer;

begin
 if Assigned(GeometricShape.OnGetValue) then
  with PixelMap do
   begin
    PixelColor32   := ConvertColor(Color);
    PixelColor32.A := Alpha;

    IntLineWdth := Max(ConvertFromFixed24Dot8(LineWidth) - 1, 0);
    RadiusMinusHalf := 0.5 * IntLineWdth;
    Radius := RadiusMinusHalf + 1;
    SqrRadius := Sqr(Radius);

    GetAlignedMemory(VertLine, Height);
    try
     // initialize temporaty variables
     IntegerRadiusX := 1 + Ceil(RadiusMinusHalf);
     IntegerRadiusY := 2 + Trunc(RadiusMinusHalf);
     SetLength(FYValues, 1 + 2 * IntegerRadiusX);
     Assert(Length(FYValues) mod 2 = 1);
     PointPtr := @FYValues[IntegerRadiusX];

     // fill additional points
     for PtIndex := 0 to Length(FYValues) - 1
      do FYValues[PtIndex] := ConvertFromFixed24Dot8(GeometricShape.OnGetValue(Self, PtIndex - IntegerRadiusX));

     for x := GeometricShape.MarginLeft to Width - GeometricShape.MarginRight - 1 do
      begin
       // get next value
       FYValues[Length(FYValues) - 1] := ConvertFromFixed24Dot8(
         GeometricShape.OnGetValue(Self, x + IntegerRadiusX - GeometricShape.MarginLeft));

       // clear vertical line array
       FillChar(VertLine^, Height, 0);

       // determine minimum and maximum
       Mn := PointPtr[0]; // - IntLineWdth;
       Mx := PointPtr[0]; // + IntLineWdth;
       for PtIndex := 1 to IntegerRadiusX - 2 do
        begin
         if PointPtr[ PtIndex] > Mx then Mx := PointPtr[ PtIndex];
         if PointPtr[ PtIndex] < Mn then Mn := PointPtr[ PtIndex];
         if PointPtr[-PtIndex] > Mx then Mx := PointPtr[-PtIndex];
         if PointPtr[-PtIndex] < Mn then Mn := PointPtr[-PtIndex];
        end;

       // determine y bounds
       YBounds[0] := Trunc(Mn - RadiusMinusHalf);
       YBounds[1] := Ceil(Mx + RadiusMinusHalf);
       for PtIndex := Max(1, IntegerRadiusX - 2) to IntegerRadiusX do
        begin
         CurrentValue := PointPtr[PtIndex];
         if CurrentValue - RadiusMinusHalf < YBounds[0] then YBounds[0] := Trunc(CurrentValue - RadiusMinusHalf);
         if CurrentValue + RadiusMinusHalf > YBounds[1] then YBounds[1] := Ceil(CurrentValue + RadiusMinusHalf);
         CurrentValue := PointPtr[-PtIndex];
         if CurrentValue - RadiusMinusHalf < YBounds[0] then YBounds[0] := Trunc(CurrentValue - RadiusMinusHalf);
         if CurrentValue + RadiusMinusHalf > YBounds[1] then YBounds[1] := Ceil(CurrentValue + RadiusMinusHalf);
        end;

       if YBounds[0] < 0 then YBounds[0] := 0;
       if YBounds[1] > Height - 1 then YBounds[1] := Height - 1;
//       if (YBounds[0] >= YBounds[1]) then Continue;

       for y := YBounds[0] to YBounds[1] do
        begin
         // check for solid area
         if (y > Mn) and (y < Mx) then
          begin
           VertLine^[y] := $FF;
           Continue;
          end;

         // draw center
         Sum := 0;
         Value := Abs(PointPtr[0] - y);
         if Value < Radius then
          begin
           Value := Value - RadiusMinusHalf;
           if Value > 0 then
            begin
             Sum := 1 - Value;
             if Sum >= 1 then
              begin
               VertLine^[y] := $FF;
               Continue;
              end;
            end
           else
            begin
             VertLine^[y] := $FF;
             Continue;
            end;
          end;


         for PtIndex := 1 to IntegerRadiusX - 2 do
          begin
           // draw left
           CurrentValue := PointPtr[-PtIndex];
           SqrDist := Sqr(CurrentValue - y) + Sqr(PtIndex);
           if SqrDist < SqrRadius then
            begin
             Distance := Sqrt(SqrDist);
             if Distance > RadiusMinusHalf
              then
               begin
                Value := (Distance - RadiusMinusHalf);
                Sum := 1 - Value * (1 - Sum);
                if Sum > 1 then Sum := 1;
                if Sum = 1 then Break;
               end
              else
               begin
                Sum := 1;
                Break;
               end;
            end;

           // draw right
           CurrentValue := PointPtr[PtIndex];
           SqrDist := Sqr(CurrentValue - y) + Sqr(PtIndex);
           if SqrDist < SqrRadius then
            begin
             Distance := Sqrt(SqrDist);
             if Distance > RadiusMinusHalf
              then
               begin
                Value := (Distance - RadiusMinusHalf);
                Sum := 1 - Value * (1 - Sum);
                if Sum > 1 then Sum := 1;
                if Sum = 1 then Break;
               end
              else
               begin
                Sum := 1;
                Break;
               end;
            end;
          end;

         // check if sum already equals 1
         if Sum = 1 then
          begin
           VertLine^[y] := $FF;
           Continue;
          end;


         PtIndex := IntegerRadiusX - 1;
         if PtIndex > 0 then
          for LeftRightIdx := 0 to 1 do
           begin
            // initialize defaults
            PtSgn := (2 * LeftRightIdx - 1);
            XPos := PtSgn * PtIndex;
            CurrentValue := PointPtr[PtSgn * PtIndex];

            YSrc := PointPtr[PtSgn * (PtIndex - 1)];
            YDest := PointPtr[PtSgn * PtIndex];

            if YDest <> YSrc then
             begin
              if ((YDest >= Y) and (YSrc <= Y)) or
                 ((YDest <= Y) and (YSrc >= Y)) then
               begin
                XPos := PtSgn * (PtIndex - (Y - YDest) / (YSrc - YDest));

                if XPos < Radius then
                 if Abs(XPos) > RadiusMinusHalf then
                  begin
                   Value := Abs(XPos) - RadiusMinusHalf;
                   Sum := 1 - Value * (1 - Sum);
                   if Sum > 1 then Sum := 1;
                   if Sum = 1 then Break;
                  end
                 else
                  begin
                   Sum := 1;
                   Break;
                  end;
                Continue;
               end
              else
               if ((YSrc < YDest) and (PointPtr[PtSgn * (PtIndex + 1)] > YDest)) or
                  ((YSrc > YDest) and (PointPtr[PtSgn * (PtIndex + 1)] < YDest))
                then Continue;
             end;

             SqrDist := Sqr(CurrentValue - y) + Sqr(XPos);

             if SqrDist < SqrRadius then
              begin
               Distance := Sqrt(SqrDist);
               Assert(Distance > RadiusMinusHalf);
               Value := (Distance - RadiusMinusHalf);
               Sum := 1 - Value * (1 - Sum);
               if Sum > 1 then Sum := 1;
               if Sum = 1 then Break;
              end;
           end;


         // check if sum already equals 1
         if Sum = 1 then
          begin
           VertLine^[y] := $FF;
           Continue;
          end;

         YSrc := PointPtr[IntegerRadiusX - 1];
         YDest := PointPtr[IntegerRadiusX];

         if (((YDest >= Y) and (YSrc <= Y)) or
             ((YDest <= Y) and (YSrc >= Y))) and (YSrc <> YDest) then
          begin
           XPos := IntegerRadiusX - (Y - YDest) / (YSrc - YDest);

           if XPos <= Radius then
            begin
             Assert(XPos >= RadiusMinusHalf);
             Value := XPos - RadiusMinusHalf;
             Sum := 1 - Value * (1 - Sum);
             if Sum >= 1 then
              begin
               VertLine^[y] := $FF;
               Break;
              end;
            end;
          end;

         YSrc := PointPtr[1 - IntegerRadiusX];
         YDest := PointPtr[-IntegerRadiusX];

         if (((YDest >= Y) and (YSrc <= Y)) or
             ((YDest <= Y) and (YSrc >= Y))) and (YSrc <> YDest) then
          begin
           XPos := IntegerRadiusX - (Y - YDest) / (YSrc - YDest);

           if XPos <= Radius then
            begin
             Assert(XPos >= RadiusMinusHalf);
             Value := XPos - RadiusMinusHalf;
             Sum := 1 - Value * (1 - Sum);
             if Sum >= 1 then
              begin
               VertLine^[y] := $FF;
               Break;
              end;
            end;
          end;

         VertLine^[y] := Round($FF * Limit(Sum, 0, 1));
        end;

       // copy line to pixel map
       for y := GeometricShape.MarginTop to Height - GeometricShape.MarginBottom - 1 do
        if VertLine^[y] > 0 then
         begin
          CombColor := ApplyAlpha(PixelColor32, VertLine^[y]);
          BlendPixelInplace(CombColor, PixelPointer[x, y]^);
         end;
       EMMS;

       // shift y-values
       Move(FYValues[1], FYValues[0], (Length(FYValues) - 1) * SizeOf(Single));
      end;
    finally
     FreeAlignedMemory(VertLine);
    end;
   end;
end;

function TGuiPixelEquallySpacedPolyline.GetGeometricShape: TGuiEquallySpacedPolyline;
begin
 Result := TGuiEquallySpacedPolyline(FGeometricShape);
end;

end.
