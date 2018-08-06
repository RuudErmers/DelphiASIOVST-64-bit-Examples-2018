unit DAV_GuiFilters;

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
  Graphics, Classes, SysUtils, DAV_Common, DAV_GuiCommon, DAV_GuiBlend,
  DAV_GuiPixelMap, DAV_GuiByteMap, DAV_DspFilter, DAV_DspFilterSimple,
  DAV_DspDelayLines;

type
  TGuiCustomFilter = class(TPersistent)
  public
    constructor Create; virtual;

    procedure Filter(ByteMap: TGuiCustomByteMap); overload; virtual; abstract;
    procedure Filter(PixelMap: TGuiCustomPixelMap); overload; virtual; abstract;
  end;

  TGuiCustomBlurFilter = class(TGuiCustomFilter)
  private
    FRadius           : Single;
    FRadiusReciprocal : Single;
    procedure SetRadius(const Value: Single);
  protected
    procedure RadiusChanged; virtual;
  public
    constructor Create; override;

    property Radius: Single read FRadius write SetRadius;
  end;

  TGuiBlurIIRFilter = class(TGuiCustomBlurFilter)
  private
    FIIRFilter : TFirstOrderLowpassFilter;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

  TGuiBlurFractionalFIRFilter = class(TGuiCustomBlurFilter)
  private
    FDelayLine : TDelayLineFractional32;
  protected
    procedure RadiusChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

  TGuiBlurFIRFilter = class(TGuiCustomBlurFilter)
  private
    FBuffer       : PByteArray;
    FKernelSize   : Integer;
  protected
    procedure RadiusChanged; override;
    procedure ResizeBuffer; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure FilterHorizontal(Data: PByteArray; Count: Integer); overload; virtual;
    procedure FilterHorizontal(Data: PPixel32Array; Count: Integer); overload; virtual;

    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

  TGuiSaturationFilter = class(TGuiCustomFilter)
  private
    FLookUpTable : array [Byte] of Byte;
    FValue       : Single;
    procedure SetValue(const Value: Single);
  protected
    procedure ValueChanged; virtual;
    procedure CalculateLookUpTable; virtual;
  public
    constructor Create; override;

    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;

    property Value: Single read FValue write SetValue;
  end;

  TGuiNormalizeFilter = class(TGuiCustomFilter)
  public
    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

  TGuiEmbossFilter = class(TGuiCustomFilter)
  public
    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

  TGuiInvertFilter = class(TGuiCustomFilter)
  public
    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

  TGuiSharpenFilter = class(TGuiCustomFilter)
  public
    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

implementation

uses
  Math, DAV_Math;


{ TGuiCustomFilter }

constructor TGuiCustomFilter.Create;
begin
 inherited Create;
end;


{ TGuiCustomBlurFilter }

constructor TGuiCustomBlurFilter.Create;
begin
 inherited;
 FRadius := 1;
end;

procedure TGuiCustomBlurFilter.RadiusChanged;
begin
 FRadiusReciprocal := 1 / FRadius;
end;

procedure TGuiCustomBlurFilter.SetRadius(const Value: Single);
begin
 if FRadius <= 0
  then raise Exception.Create('Radius must be larger than zero!');

 if FRadius <> Value then
  begin
   FRadius := Value;
   RadiusChanged;
  end;
end;


{ TGuiBlurIIRFilter }

constructor TGuiBlurIIRFilter.Create;
begin
 FIIRFilter := TFirstOrderLowpassFilter.Create;
end;

destructor TGuiBlurIIRFilter.Destroy;
begin
 FreeAndNil(FIIRFilter);
 inherited;
end;

procedure TGuiBlurIIRFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  X, Y : Integer;
  Data : PByteArray;
begin
 with ByteMap do
  begin
   for Y := 0 to Height - 1 do
    begin
     Data := ScanLine[Y];
     FIIRFilter.Reset;
     for X := 0 to Width - 1
      do Data^[X] := Round(FIIRFilter.ProcessSample32(Data^[X]));
     for X := Width - 1 downto 0
      do Data^[X] := Round(FIIRFilter.ProcessSample32(Data^[X]));
    end;
  end;
end;

procedure TGuiBlurIIRFilter.Filter(PixelMap: TGuiCustomPixelMap);
(*
var
  X, Y : Integer;
  Data : PPixel32Array;
*)
begin
 raise Exception.Create('not yet implemented');

(*
 with PixelMap do
  begin
   for Y := 0 to Height - 1 do
    begin
     Data := DataPointer;
     for X := 0 to Width - 1 do
      begin
//       Data^[X] := Round(FIIRFilter.ProcessSample32(Data^[X]);

      end;
    end;
  end;
*)
end;


{ TGuiBlurFractionalFIRFilter }

constructor TGuiBlurFractionalFIRFilter.Create;
begin
 inherited;
 FDelayLine := TDelayLineFractional32.Create;
 FDelayLine.FractionalBufferSize := FRadius;
end;

destructor TGuiBlurFractionalFIRFilter.Destroy;
begin
 FreeAndNil(FDelayLine);
 inherited;
end;

procedure TGuiBlurFractionalFIRFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  X, Y    : Integer;
  Data    : PByteArray;
  Current : Single;
  Sum     : Single;
begin
 with ByteMap do
  begin
   for Y := 0 to Height - 1 do
    begin
     Data := ScanLine[Y];
     FDelayLine.ClearBuffer;
     Sum := 0;
     for X := 0 to Width - 1 do
      begin
       Current := Data^[X] * COne255th;
       Sum := Sum + Current;
       Current := FDelayLine.ProcessSample32(Current);
       Sum := Sum - Current;
       if Sum > 0
        then Data^[X] := Round($FF * Sum * FRadiusReciprocal)
        else Data^[X] := 0;
      end;
    end;

   Data := DataPointer;
   for X := 0 to Width - 1 do
    begin
     FDelayLine.ClearBuffer;
     Sum := 0;
     for Y := 0 to Height - 1 do
      begin
       Current := Data^[Y * Width + X] * COne255th;
       Sum := Sum + Current;
       Current := FDelayLine.ProcessSample32(Current);
       Sum := Sum - Current;
       if Sum > 0
        then Data^[Y * Width + X] := Round($FF * Sum * FRadiusReciprocal)
        else Data^[Y * Width + X] := 0;
      end;
    end;
  end;
end;

procedure TGuiBlurFractionalFIRFilter.Filter(PixelMap: TGuiCustomPixelMap);
begin
 raise Exception.Create('not yet implemented');
end;

procedure TGuiBlurFractionalFIRFilter.RadiusChanged;
begin
 inherited;
 FDelayLine.FractionalBufferSize := FRadius;
end;


{ TGuiBlurFIRFilter }

constructor TGuiBlurFIRFilter.Create;
begin
 inherited;
 FKernelSize := 1;
 ResizeBuffer;
end;

destructor TGuiBlurFIRFilter.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TGuiBlurFIRFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  X, Y    : Integer;
  Pos     : Integer;
  Data    : PByteArray;
  Sum     : Integer;
begin
 if FKernelSize <= 1 then Exit;

 with ByteMap do
  begin
   for Y := 0 to Height - 1
    do FilterHorizontal(ScanLine[Y], Width);

   Data := DataPointer;
   for X := 0 to Width - 1 do
    begin
     FillChar(FBuffer^, FKernelSize, 0);
     Pos := 0;
     Sum := 0;
     for Y := 0 to Height - 1 do
      begin
       Sum := Sum + Data^[Y * Width + X] - FBuffer^[Pos];
       FBuffer^[Pos] := Data^[Y * Width + X];
       Inc(Pos); if Pos >= FKernelSize then Pos := 0;
       Assert(Sum >= 0);
       Data^[Y * Width + X] := Sum div FKernelSize;
      end;
    end;
  end;
end;

procedure TGuiBlurFIRFilter.Filter(PixelMap: TGuiCustomPixelMap);
begin

end;

procedure TGuiBlurFIRFilter.FilterHorizontal(Data: PPixel32Array; Count: Integer);
begin

end;

procedure TGuiBlurFIRFilter.FilterHorizontal(Data: PByteArray; Count: Integer);
var
  X   : Integer;
  Pos : Integer;
  Sum : Integer;
begin
 FillChar(FBuffer^, FKernelSize, 0);
 Pos := 0;
 Sum := 0;
 for X := 0 to Count - 1 do
  begin
   Sum := Sum + Data^[X] - FBuffer^[Pos];
   FBuffer^[Pos] := Data^[X];
   Inc(Pos); if Pos >= FKernelSize then Pos := 0;
   Data^[X] := Sum div FKernelSize;
  end;
end;

procedure TGuiBlurFIRFilter.RadiusChanged;
begin
 inherited;
 FKernelSize := Round(Radius) + 1;
 ResizeBuffer;
end;

procedure TGuiBlurFIRFilter.ResizeBuffer;
begin
 ReallocMem(FBuffer, FKernelSize);
end;


{ TGuiSaturationFilter }

constructor TGuiSaturationFilter.Create;
begin
 inherited;
 FValue := 0;
 CalculateLookUpTable;
end;

procedure TGuiSaturationFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  Index : Integer;
  Data  : PByteArray;
begin
 Data := ByteMap.DataPointer;
 for Index := 0 to (ByteMap.Width * ByteMap.Height) - 1
  do Data^[Index] := FLookUpTable[Data^[Index]];
end;

procedure TGuiSaturationFilter.Filter(PixelMap: TGuiCustomPixelMap);
var
  Index : Integer;
  Data  : PPixel32Array;
begin
 Data := PixelMap.DataPointer;
 for Index := 0 to (PixelMap.Width * PixelMap.Height) - 1 do
  begin
   Data^[Index].A := FLookUpTable[Data^[Index].A];
   Data^[Index].R := FLookUpTable[Data^[Index].R];
   Data^[Index].G := FLookUpTable[Data^[Index].G];
   Data^[Index].B := FLookUpTable[Data^[Index].B];
  end;
end;

procedure TGuiSaturationFilter.CalculateLookUpTable;
var
  ExpPos : Double;
  Index  : Integer;
begin
 ExpPos := 1 - (2 * (1 + Abs(Value)) / ((1 + Abs(Value) + Value)));
 for Index := 0 to Length(FLookUpTable) - 1
  do FLookUpTable[Index] := Round($FF * Index / (Index - ExpPos * (Index xor $FF)));
end;

procedure TGuiSaturationFilter.ValueChanged;
begin
 CalculateLookupTable;
end;

procedure TGuiSaturationFilter.SetValue(const Value: Single);
begin
 if FValue <> Value then
  begin
   FValue := Value;
   ValueChanged;
  end;
end;


{ TGuiNormalizeFilter }

procedure TGuiNormalizeFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  x, y   : Integer;
  Mn, Mx : Byte;
  ptr    : PByteArray;
  Scale  : Single;
begin
 // scan minimum and maximum
 Mn := ByteMap.DataPointer^[0];
 Mx := Mn;

 ptr := ByteMap.Scanline[0];
 for x := 1 to ByteMap.Width - 1 do
  if ptr[x] > Mx then Mx := ptr[x] else
  if ptr[x] < Mn then Mn := ptr[x];

 for y := 1 to ByteMap.Height - 1 do
  begin
   ptr := ByteMap.Scanline[y];
   for x := 0 to ByteMap.Width - 1 do
    if ptr[x] > Mx then Mx := ptr[x] else
    if ptr[x] < Mn then Mn := ptr[x];
  end;

 // check whether the minimum is also the maximum
 if Mx = Mn then Exit;

 // calculate scale factor
 Scale := 255 / (Mx - Mn);

 // apply scale factor
 for y := 0 to ByteMap.Height - 1 do
  begin
   ptr := ByteMap.Scanline[y];
   for x := 0 to ByteMap.Width - 1
    do ptr[x] := Round((ptr[x] - Mn) * Scale);
  end;
end;

procedure TGuiNormalizeFilter.Filter(PixelMap: TGuiCustomPixelMap);
var
  x, y     : Integer;
  RMn, RMx : Byte;
  GMn, GMx : Byte;
  BMn, BMx : Byte;
  AMn, AMx : Byte;
  ptr      : PPixel32Array;
  Scale    : Single;
begin
 // scan minimum and maximum
 RMn := PixelMap.DataPointer^[0].R;
 RMx := RMn;
 GMn := PixelMap.DataPointer^[0].G;
 GMx := GMn;
 BMn := PixelMap.DataPointer^[0].B;
 BMx := BMn;
 AMn := PixelMap.DataPointer^[0].A;
 AMx := AMn;

 ptr := PixelMap.Scanline[0];
 for x := 1 to PixelMap.Width - 1 do
  begin
   if ptr[x].R > RMx then RMx := ptr[x].R else
   if ptr[x].R < RMn then RMn := ptr[x].R;
   if ptr[x].G > GMx then GMx := ptr[x].G else
   if ptr[x].G < GMn then GMn := ptr[x].G;
   if ptr[x].B > BMx then BMx := ptr[x].B else
   if ptr[x].B < BMn then BMn := ptr[x].B;
   if ptr[x].A > AMx then AMx := ptr[x].A else
   if ptr[x].A < AMn then AMn := ptr[x].A;
  end;

 for y := 1 to PixelMap.Height - 1 do
  begin
   ptr := PixelMap.Scanline[y];
   for x := 0 to PixelMap.Width - 1 do
    begin
     if ptr[x].R > RMx then RMx := ptr[x].R else
     if ptr[x].R < RMn then RMn := ptr[x].R;
     if ptr[x].G > GMx then GMx := ptr[x].G else
     if ptr[x].G < GMn then GMn := ptr[x].G;
     if ptr[x].B > BMx then BMx := ptr[x].B else
     if ptr[x].B < BMn then BMn := ptr[x].B;
     if ptr[x].A > AMx then AMx := ptr[x].A else
     if ptr[x].A < AMn then AMn := ptr[x].A;
    end;
  end;

 // check whether the minimum is also the maximum
 if AMx > AMn then
  begin
   // calculate scale factor
   Scale := 255 / (AMx - AMn);

   // apply scale factor
   for y := 0 to PixelMap.Height - 1 do
    begin
     ptr := PixelMap.Scanline[y];
     for x := 0 to PixelMap.Width - 1
      do ptr[x].A := Round((ptr[x].A - AMn) * Scale);
    end;
  end;

 if RMx > RMn then
  begin
   // calculate scale factor
   Scale := 255 / (RMx - RMn);

   // apply scale factor
   for y := 0 to PixelMap.Height - 1 do
    begin
     ptr := PixelMap.Scanline[y];
     for x := 0 to PixelMap.Width - 1
      do ptr[x].R := Round((ptr[x].R - RMn) * Scale);
    end;
  end;

 if GMx > GMn then
  begin
   // calculate scale factor
   Scale := 255 / (GMx - GMn);

   // apply scale factor
   for y := 0 to PixelMap.Height - 1 do
    begin
     ptr := PixelMap.Scanline[y];
     for x := 0 to PixelMap.Width - 1
      do ptr[x].G := Round((ptr[x].G - GMn) * Scale);
    end;
  end;

 if BMx > BMn then
  begin
   // calculate scale factor
   Scale := 255 / (BMx - BMn);

   // apply scale factor
   for y := 0 to PixelMap.Height - 1 do
    begin
     ptr := PixelMap.Scanline[y];
     for x := 0 to PixelMap.Width - 1
      do ptr[x].B := Round((ptr[x].B - BMn) * Scale);
    end;
  end;
end;


{ TGuiEmbossFilter }

procedure TGuiEmbossFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  x, y   : Integer;
  p1, p2 : PByteArray;
begin
 for y := 0 to ByteMap.Height - 2 do
  begin
   p1 := ByteMap.Scanline[y];
   p2 := ByteMap.Scanline[y + 1];
   for x := 0 to ByteMap.Width - 2
    do p1[x] := (p1[x] + (p2[(x + 1)] xor $FF)) shr 1;
  end;
end;

procedure TGuiEmbossFilter.Filter(PixelMap: TGuiCustomPixelMap);
var
  x, y   : Integer;
  p1, p2 : PPixel32Array;
begin
 for y := 0 to PixelMap.Height - 2 do
  begin
   p1 := PixelMap.Scanline[y];
   p2 := PixelMap.Scanline[y + 1];
   for x := 0 to PixelMap.Width - 2 do
    begin
     p1[x].R := (p1[x].R + (p2[x + 1].R xor $FF)) shr 1;
     p1[x].G := (p1[x].G + (p2[x + 1].G xor $FF)) shr 1;
     p1[x].B := (p1[x].B + (p2[x + 1].B xor $FF)) shr 1;
    end;
  end;
end;


{ TGuiInvertFilter }

procedure TGuiInvertFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  Index : Integer;
  Data  : PByteArray;
begin
 Data := ByteMap.DataPointer;
 for Index := 0 to ByteMap.Width * ByteMap.Height - 1
  do Data^[Index] := not Data^[Index];
end;

procedure TGuiInvertFilter.Filter(PixelMap: TGuiCustomPixelMap);
var
  Index : Integer;
  Data  : PPixel32Array;
begin
 Data := PixelMap.DataPointer;
 for Index := 0 to PixelMap.Width * PixelMap.Height - 1
  do Data^[Index].ARGB := not Data^[Index].ARGB;
end;


{ TGuiSharpenFilter }

procedure TGuiSharpenFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  y, x, j   : Integer;
  Dest      : PByteArray;
  ByteArray : array [0..7] of Byte;
  Sum, nx   : Cardinal;
  Reference : Byte;
  Temp      : Byte;
begin
 with ByteMap do
  for y := 0 to Height - 1 do
   begin
    Dest := ScanLine[y];
    for x := 0 to Width - 1 do
     begin
      nx := 0;

      // get surrounding pixels
      if (y > 0) then
       begin
        if (x > 0) then ByteArray[0] := Value[x - 1, y - 1];
        ByteArray[1] := Value[x, y - 1];
        if (x < Width - 1) then ByteArray[2] := Value[x + 1, y - 1];
       end
      else
       begin
        ByteArray[0] := 0;
        ByteArray[1] := 0;
        ByteArray[2] := 0;
       end;
      if (x > 0)          then ByteArray[3] := Value[x - 1, y];
      if (x < Width - 1) then ByteArray[4] := Value[x + 1, y];
      if (y < Height - 1) then
       begin
        if (x > 0) then ByteArray[5] := Value[x - 1, y + 1];
        ByteArray[6] := Value[x, y + 1];
        if (x < Height - 1) then ByteArray[7] := Value[x + 1, y + 1];
       end
      else
       begin
        ByteArray[5] := 0;
        ByteArray[6] := 0;
        ByteArray[7] := 0;
       end;

      // initialize sum
      Sum := 0;

      for j := 0 to nx - 1 do
       begin
        Temp := ByteArray[j];
        Sum  := Sum + Temp;
       end;

      if (Sum = 0)
       then Reference := 0
       else Reference := (Sum + nx shr 1) div nx;

      Temp := Value[x, y];
      if Reference <> 0
       then Temp := Limit(2 * Temp - Reference, 0, 255);
      Dest^[x] := Temp;
     end;
   end;
end;

procedure TGuiSharpenFilter.Filter(PixelMap: TGuiCustomPixelMap);
var
  y, x       : Integer;
  nx, j      : Cardinal;
  Dest       : PPixel32Array;
  PixelArray : array [0..7] of TPixel32;
  SumR, SumG : Cardinal;
  SumB, SumA : Cardinal;
  RGBdiv     : Cardinal;
  RefPixel   : TPixel32;
  TempPixel  : TPixel32;
begin
 with PixelMap do
  for y := 0 to Height - 1 do
   begin
    Dest := ScanLine[y];
    for x := 0 to Width - 1 do
     begin
      nx := 0;

      // get surrounding pixels
      if (y > 0) then
       begin
        if (x > 0) then PixelArray[0] := Pixel[x - 1, y - 1];
        PixelArray[1] := Pixel[x, y - 1];
        if (x < Width - 1) then PixelArray[2] := Pixel[x + 1, y - 1];
       end
      else
       begin
        PixelArray[0].ARGB := 0;
        PixelArray[1].ARGB := 0;
        PixelArray[2].ARGB := 0;
       end;
      if (x > 0)          then PixelArray[3] := Pixel[x - 1, y];
      if (x < Width - 1) then PixelArray[4] := Pixel[x + 1, y];
      if (y < Height - 1) then
       begin
        if (x > 0) then PixelArray[5] := Pixel[x - 1, y + 1];
        PixelArray[6] := Pixel[x, y + 1];
        if (x < Height - 1) then PixelArray[7] := Pixel[x + 1, y + 1];
       end
      else
       begin
        PixelArray[5].ARGB := 0;
        PixelArray[6].ARGB := 0;
        PixelArray[7].ARGB := 0;
       end;

      // initialize sums
      SumR := 0;
      SumG := 0;
      SumB := 0;
      SumA := 0;
      RGBdiv := 0;

      for j := 0 to nx - 1 do
       begin
        TempPixel := PixelArray[j];
        SumR      := SumR + TempPixel.R * TempPixel.A;
        SumG      := SumG + TempPixel.G * TempPixel.A;
        SumB      := SumB + TempPixel.B * TempPixel.A;
        RGBdiv    := RGBdiv + TempPixel.A;
        SumA      := SumA + TempPixel.A;
       end;

      if (RGBdiv = 0) then RefPixel.ARGB := 0 else
       begin
        RefPixel.R := (SumR + RGBdiv shr 1) div RGBdiv;
        RefPixel.G := (SumG + RGBdiv shr 1) div RGBdiv;
        RefPixel.B := (SumB + RGBdiv shr 1) div RGBdiv;
        RefPixel.A := (SumA + nx shr 1) div nx;
       end;

      TempPixel := Pixel[x, y];
      if RefPixel.ARGB <> 0 then
       begin
        TempPixel.R := Limit(2 * TempPixel.R - RefPixel.R, 0, 255);
        TempPixel.G := Limit(2 * TempPixel.G - RefPixel.G, 0, 255);
        TempPixel.B := Limit(2 * TempPixel.B - RefPixel.B, 0, 255);
        TempPixel.A := Limit(2 * TempPixel.A - RefPixel.A, 0, 255);
       end;
      Dest^[x] := TempPixel;
     end;
   end;
end;

end.
