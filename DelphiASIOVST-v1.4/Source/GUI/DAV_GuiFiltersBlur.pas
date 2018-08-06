unit DAV_GuiFiltersBlur;

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
  SysUtils, DAV_Types, DAV_MemoryUtils, DAV_GuiByteMap, DAV_GuiPixelMap,
  DAV_GuiCommon, DAV_GuiFilters;

type
  TGuiStackBlurFilter = class(TGuiCustomBlurFilter)
  private
    FKernelSize : Integer;
  protected
    procedure RadiusChanged; override;
    procedure ResizeStack; virtual;

    property KernelSize: Integer read FKernelSize;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;


  TGuiGaussianBlurFilter = class(TGuiCustomBlurFilter)
  private
    FKernelSize       : Integer;
    FActualKernelSize : Integer;
    FKernel           : PDAVSingleFixedArray;
    procedure SetKernelSize(const Value: Integer);
  protected
    procedure AllocateKernelMemory;
    procedure BuildGaussianKernel; virtual;
    procedure KernelSizeChanged; virtual;
    procedure RadiusChanged; override;

    procedure BlurPixelRow(CurrentRow, TempRow: PPixel32Array; PixelCount: Integer);
    procedure BlurByteRow(CurrentRow, TempRow: PByteArray; PixelCount: Integer);

    property KernelSize: Integer read FKernelSize write SetKernelSize;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

implementation

uses
  Math, DAV_Common, DAV_GuiBlend;

{ TGuiStackBlurFilter }

constructor TGuiStackBlurFilter.Create;
begin
 inherited;
 FKernelSize := Round(Radius);
 ResizeStack;
end;

destructor TGuiStackBlurFilter.Destroy;
begin

 inherited;
end;

procedure TGuiStackBlurFilter.Filter(ByteMap: TGuiCustomByteMap);
type
  TArrayOfInteger = array of Integer;
var
  Height, Width   : Integer;
  HeightMinus1    : Integer;
  WidthMinus1     : Integer;
  RadiusWidth     : Integer;
  I, X, Y         : Integer;
  YIncrement      : Integer;
  CurrentLine     : PByteArray;

  ASumIn          : Integer;
  ASumOut, ASum   : Integer;

  KernelSizePlus1 : Integer;
  CurrentData     : Byte;
  Data            : PByteArray;

  DivisionLUT     : TArrayOfInteger;
  Divi, DivSum    : Integer;

  StackStart      : Integer;
  StackPos        : Integer;
  Stack           : array of Byte;
  VMin            : TArrayOfInteger;
begin
 // check whether blur needs to be calculated at all
 if (FKernelSize < 1) then Exit;

 // initialize local values
 Width        := ByteMap.Width;
 Height       := ByteMap.Height;
 WidthMinus1  := Width - 1;
 HeightMinus1 := Height - 1;
 RadiusWidth  := FKernelSize * Width;

 //
 SetLength(VMin, Max(Width, Height));

 Divi := 2 * FKernelSize + 1;

 // set up division LUT
 DivSum := (Divi + 1) shr 1;
 DivSum := Sqr(DivSum);

 SetLength(DivisionLUT, 256 * DivSum);
 for I := 0 to High(DivisionLUT)
  do DivisionLUT[I] := (I div DivSum);

 SetLength(Stack, Divi);

 KernelSizePlus1 := FKernelSize + 1;

 for Y := 0 to HeightMinus1 do
  begin
   // initialize sum
   ASumIn  := 0;
   ASumOut := 0;
   ASum    := 0;

   CurrentLine := ByteMap.ScanLine[Y];

   for I := -FKernelSize to FKernelSize do
    begin
     if I < 0 then CurrentData := CurrentLine^[0] else
     if I > WidthMinus1 then CurrentData := CurrentLine^[WidthMinus1]
      else CurrentData := CurrentLine^[I];

     Stack[I + FKernelSize] := CurrentData;

     ASum := ASum + CurrentData * (KernelSizePlus1 - Abs(I));
     if I > 0
      then Inc(ASumIn, CurrentData)
      else Inc(ASumOut, CurrentData);
    end;
   StackPos := FKernelSize;

   for X := 0 to WidthMinus1 do
    begin
     CurrentLine^[X] := DivisionLUT[ASum];

     Dec(ASum, ASumOut);
     StackStart := StackPos - FKernelSize + Divi;

     CurrentData := Stack[StackStart mod Divi];
     Dec(ASumOut, CurrentData);

     if Y = 0 then VMin[X] := Min(X + KernelSizePlus1, WidthMinus1);

     CurrentData := CurrentLine^[VMin[X]];
     Stack[StackStart mod Divi] := CurrentData;
     Inc(ASumIn, CurrentData);
     Inc(ASum, ASumIn);

     StackPos := (StackPos + 1) mod Divi;
     CurrentData := Stack[StackPos];

     Inc(ASumOut, CurrentData);
     Dec(ASumIn, CurrentData);
    end;
  end;

 Data := ByteMap.DataPointer;

 for X := 0 to WidthMinus1 do
  begin
   // initialize sum
   ASumIn := 0;
   ASumOut := 0;
   ASum := 0;

   YIncrement := -RadiusWidth;

   for I := -FKernelSize to FKernelSize do
    begin
     CurrentData := Data^[Max(0, YIncrement) + X];
     Stack[I + FKernelSize] := CurrentData;

     ASum := ASum + CurrentData * (KernelSizePlus1 - Abs(I));

     if I > 0 then Inc(ASumIn, CurrentData) else
      if I < HeightMinus1 then Inc(ASumOut, CurrentData);

     if I < HeightMinus1
      then Inc(YIncrement, Width);
    end;

   YIncrement := X;
   StackPos := FKernelSize;

   for Y := 0 to HeightMinus1 do
    begin
     Data^[YIncrement] := DivisionLUT[ASum];

     Dec(ASum, ASumOut);
     StackStart := StackPos - FKernelSize + Divi;

     CurrentData := Stack[StackStart mod Divi];
     Dec(ASumOut, CurrentData);

     if X = 0 then VMin[Y] := Min(Y + KernelSizePlus1, HeightMinus1) * Width;

     CurrentData := Data^[VMin[Y] + X];
     Stack[StackStart mod Divi] := CurrentData;
     Inc(ASumIn, CurrentData);
     Inc(ASum, ASumIn);

     StackPos := (StackPos + 1) mod Divi;
     CurrentData := Stack[StackPos];

     Inc(ASumOut, CurrentData);
     Dec(ASumIn, CurrentData);
     Inc(YIncrement, Width);
   end;
 end;
end;

procedure TGuiStackBlurFilter.Filter(PixelMap: TGuiCustomPixelMap);
type
  TArrayOfInteger = array of Integer;
var
  Height, Width   : Integer;
  HeightMinus1    : Integer;
  WidthMinus1     : Integer;
  RadiusWidth     : Integer;
  I, X, Y         : Integer;
  YIncrement      : Integer;
  CurrentLine     : PPixel32Array;

  ASumIn          : Integer;
  ASumOut, ASum   : Integer;

  KernelSizePlus1 : Integer;
  CurrentData     : TPixel32;
  Data            : PPixel32Array;

  DivisionLUT     : TArrayOfInteger;
  Divi, DivSum    : Integer;

  StackStart      : Integer;
  StackPos        : Integer;
  Stack           : array of Byte;
  VMin            : TArrayOfInteger;
begin
 // check whether blur needs to be calculated at all
 if (FKernelSize < 1) then Exit;

 // initialize local values
 Width        := PixelMap.Width;
 Height       := PixelMap.Height;
 WidthMinus1  := Width - 1;
 HeightMinus1 := Height - 1;
 RadiusWidth  := FKernelSize * Width;

 //
 SetLength(VMin, Max(Width, Height));

 Divi := 2 * FKernelSize + 1;

 // set up division LUT
 DivSum := (Divi + 1) shr 1;
 DivSum := Sqr(DivSum);

 SetLength(DivisionLUT, 256 * DivSum);
 for I := 0 to High(DivisionLUT)
  do DivisionLUT[I] := (I div DivSum);

 SetLength(Stack, Divi);

 KernelSizePlus1 := FKernelSize + 1;

 for Y := 0 to HeightMinus1 do
  begin
   // initialize sum
   ASumIn  := 0;
   ASumOut := 0;
   ASum    := 0;

   CurrentLine := PixelMap.ScanLine[Y];

   for I := -FKernelSize to FKernelSize do
    begin
     if I < 0 then CurrentData := CurrentLine^[0] else
     if I > WidthMinus1 then CurrentData := CurrentLine^[WidthMinus1]
      else CurrentData := CurrentLine^[I];

     Stack[I + FKernelSize] := CurrentData.A;

     ASum := ASum + CurrentData.A * (KernelSizePlus1 - Abs(I));
     if I > 0
      then Inc(ASumIn, CurrentData.A)
      else Inc(ASumOut, CurrentData.A);
    end;
   StackPos := FKernelSize;

   for X := 0 to WidthMinus1 do
    begin
     CurrentLine^[X].A := DivisionLUT[ASum];

     Dec(ASum, ASumOut);
     StackStart := StackPos - FKernelSize + Divi;

     CurrentData.A := Stack[StackStart mod Divi];
     Dec(ASumOut, CurrentData.A);

     if Y = 0 then VMin[X] := Min(X + KernelSizePlus1, WidthMinus1);

     CurrentData := CurrentLine^[VMin[X]];
     Stack[StackStart mod Divi] := CurrentData.A;
     Inc(ASumIn, CurrentData.A);
     Inc(ASum, ASumIn);

     StackPos := (StackPos + 1) mod Divi;
     CurrentData.A := Stack[StackPos];

     Inc(ASumOut, CurrentData.A);
     Dec(ASumIn, CurrentData.A);
    end;
  end;

 Data := PixelMap.DataPointer;

 for X := 0 to WidthMinus1 do
  begin
   // initialize sum
   ASumIn := 0;
   ASumOut := 0;
   ASum := 0;

   YIncrement := -RadiusWidth;

   for I := -FKernelSize to FKernelSize do
    begin
     CurrentData := Data^[Max(0, YIncrement) + X];
     Stack[I + FKernelSize] := CurrentData.A;

     ASum := ASum + CurrentData.A * (KernelSizePlus1 - Abs(I));

     if I > 0 then Inc(ASumIn, CurrentData.A) else
      if I < HeightMinus1 then Inc(ASumOut, CurrentData.A);

     if I < HeightMinus1
      then Inc(YIncrement, Width);
    end;

   YIncrement := X;
   StackPos := FKernelSize;

   for Y := 0 to HeightMinus1 do
    begin
     Data^[YIncrement].A := DivisionLUT[ASum];

     Dec(ASum, ASumOut);
     StackStart := StackPos - FKernelSize + Divi;

     CurrentData.A := Stack[StackStart mod Divi];
     Dec(ASumOut, CurrentData.A);

     if X = 0 then VMin[Y] := Min(Y + KernelSizePlus1, HeightMinus1) * Width;

     CurrentData := Data^[VMin[Y] + X];
     Stack[StackStart mod Divi] := CurrentData.A;
     Inc(ASumIn, CurrentData.A);
     Inc(ASum, ASumIn);

     StackPos := (StackPos + 1) mod Divi;
     CurrentData.A := Stack[StackPos];

     Inc(ASumOut, CurrentData.A);
     Dec(ASumIn, CurrentData.A);
     Inc(YIncrement, Width);
   end;
 end;
end;

procedure TGuiStackBlurFilter.RadiusChanged;
begin
 inherited;
 FKernelSize := Round(Radius);
 ResizeStack;
end;

procedure TGuiStackBlurFilter.ResizeStack;
begin
end;










{ TGuiGaussianBlurFilter }

constructor TGuiGaussianBlurFilter.Create;
begin
 inherited;
 FKernel := nil;
 FKernelSize := 100;
 AllocateKernelMemory;
 BuildGaussianKernel;
end;

destructor TGuiGaussianBlurFilter.Destroy;
begin
 FreeAlignedMemory(FKernel);
 inherited;
end;

procedure TGuiGaussianBlurFilter.AllocateKernelMemory;
begin
 ReallocateAlignedMemory(FKernel, (2 * FKernelSize + 1) * SizeOf(Single));
end;

procedure TGuiGaussianBlurFilter.KernelSizeChanged;
begin
 AllocateKernelMemory;
 BuildGaussianKernel;
end;

procedure TGuiGaussianBlurFilter.RadiusChanged;
begin
 inherited;
 BuildGaussianKernel;
end;

procedure TGuiGaussianBlurFilter.BuildGaussianKernel;
(*
procedure MakeGaussianKernel(var K: TKernel; radius: Double;
                            MaxData, DataGranularity: Double);
// makes K into a gaussian kernel with standard deviation = radius.
// for the current application you set MaxData = 255,
// DataGranularity = 1. Now the procedure sets the value of
// K.Size so that when we use K we will ignore the Weights
// that are so small they can't possibly matter. (Small Size
// is good because the execution time is going to be
// propertional to K.Size.)
*)
const
  CDataGranularity = 1;
  CMaxData         = 255;
var
 j           : Integer;
 Temp, Delta : Double;
 ActualKS    : Integer;
// KernelSize  : TKernelSize;
begin
 Delta := 1 / Radius;
 for j := -FKernelSize to FKernelSize do
  begin
   Temp := j * Delta;
   FKernel[FKernelSize + j] := Exp(-0.5 * Sqr(Temp));
  end;

 // now divide by constant so sum(Weights) = 1:
 Temp := 0;
 for j := -FKernelSize to FKernelSize
  do Temp := Temp + FKernel[FKernelSize + j];
 Temp := 1 / Temp;
 for j := -FKernelSize to FKernelSize
  do FKernel[FKernelSize + j] := FKernel[FKernelSize + j] * Temp;

 // discard (or rather mark as ignorable by setting size) the entries that are
 // too small to matter - this is important, otherwise a blur with a small
 // radius will take as long as with a large radius...
 ActualKS := FKernelSize;
 Delta := CDataGranularity / (2 * CMaxData);
 Temp := 0;
 while (Temp < Delta) and (ActualKS > 1) do
  begin
   Temp := Temp + 2 * FKernel[FKernelSize + ActualKS];
   Dec(ActualKS);
  end;
 FActualKernelSize := ActualKS;

 // now just to be correct go back and jiggle again so the
 // sum of the entries we'll be using is exactly 1:
 Temp := 0;
 for j := -FActualKernelSize to FActualKernelSize
  do Temp := Temp + FKernel[FKernelSize + j];
 Temp := 1 / Temp;
 for j := -FActualKernelSize to FActualKernelSize
  do FKernel[FKernelSize + j] := FKernel[FKernelSize + j] * Temp;
end;

function TrimInt(Upper: Integer; Value: Integer): Integer;
begin
 while (Value < 0) or (Value > Upper) do
  begin
   if Value > Upper
    then Value := -Upper + Value
    else
   if Value < 0
    then Value := -Value;
  end;
 Result := Value;
end;

procedure TGuiGaussianBlurFilter.BlurByteRow(CurrentRow, TempRow: PByteArray;
  PixelCount: Integer);
var
 i, n   : Integer;
 Temp   : Single;
 Weight : Single;
begin
 for i := 0 to PixelCount - 1 do
  begin
   Temp := 0;
   for n := -FActualKernelSize to FActualKernelSize do
    begin
     Weight :=  FKernel[FKernelSize + n];

     // the TrimInt keeps us from running off the edge of the row...
     Temp := Temp + Weight * CurrentRow[TrimInt(PixelCount - 1, i - n)];
    end;
   TempRow[i] := RoundLimit(Temp, 0, 255);
  end;
 Move(TempRow[0], CurrentRow[0], PixelCount);
end;

procedure TGuiGaussianBlurFilter.BlurPixelRow(CurrentRow,
  TempRow: PPixel32Array; PixelCount: Integer);
var
 PixelIndex     : Integer;
 KernelIndex    : Integer;
 tr, tg, tb, ta : Single;   //  Temp Red, Temp Green, Temp blue
 Weight         : Single;
begin
 for PixelIndex := 0 to PixelCount - 1 do
  begin
   tb := 0;
   tg := 0;
   tr := 0;
   ta := 0;
   for KernelIndex := -FActualKernelSize to FActualKernelSize do
    begin
     Weight := FKernel[FKernelSize + KernelIndex];

     // the TrimInt keeps us from running off the edge of the row...
     with CurrentRow[TrimInt(PixelCount - 1, PixelIndex - KernelIndex)] do
      begin
       tb := tb + Weight * B;
       tg := tg + Weight * G;
       tr := tr + Weight * R;
       ta := ta + Weight * A;
      end;
    end;
   with TempRow[PixelIndex] do
    begin
     b := RoundLimit(tb, 0, $FF);
     g := RoundLimit(tg, 0, $FF);
     r := RoundLimit(tr, 0, $FF);
     a := RoundLimit(ta, 0, $FF);
     // a:=255;
    end;
  end;
 Move(TempRow[0], CurrentRow[0], PixelCount * Sizeof(TPixel32));
end;

procedure TGuiGaussianBlurFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  X, Y    : Integer;
  TempCol : PByteArray;
  TempRow : PByteArray;
begin
 inherited;

 // allocate temporary memory
 GetAlignedMemory(TempRow, Max(ByteMap.Width, ByteMap.Height));
 GetAlignedMemory(TempCol, ByteMap.Height);

 //  blur each y value
 for Y := 0 to ByteMap.Height - 1
  do BlurByteRow(ByteMap.ScanLine[Y], TempRow, ByteMap.Width);

 // blur each column
 for X := 0 to ByteMap.Width - 1 do
  begin
   //  first read the column into a temporary row
   for Y := 0 to ByteMap.Height - 1 do TempCol[Y] := ByteMap.ScanLine[Y]^[X];
   BlurByteRow(TempCol, TempRow, ByteMap.Height);

   // now translate the temporary row back to the columns
   for Y := 0 to ByteMap.Height - 1 do ByteMap.ScanLine[Y]^[X] := TempCol[Y];
  end;

 // free temporary memory
 FreeAlignedMemory(TempCol);
 FreeAlignedMemory(TempRow);
end;

procedure TGuiGaussianBlurFilter.Filter(PixelMap: TGuiCustomPixelMap);
var
  X, Y    : Integer;
  TempCol : PPixel32Array;
  TempRow : PPixel32Array;
begin
 inherited;

 // allocate temporary memory
 GetAlignedMemory(TempRow, Max(PixelMap.Width, PixelMap.Height) * SizeOf(TPixel32));
 GetAlignedMemory(TempCol, PixelMap.Height * SizeOf(TPixel32));

 //  blur each y value
 for Y := 0 to PixelMap.Height - 1
  do BlurPixelRow(PixelMap.ScanLine[Y], TempRow, PixelMap.Width);

 // blur each column
 for X := 0 to PixelMap.Width - 1 do
  begin
   //  first read the column into a temporary row
   for Y := 0 to PixelMap.Height - 1 do TempCol[Y] := PixelMap.ScanLine[Y]^[X];
   BlurPixelRow(TempCol, TempRow, PixelMap.Height);

   // now translate the temporary row back to the columns
   for Y := 0 to PixelMap.Height - 1 do PixelMap.ScanLine[Y]^[X] := TempCol[Y];
  end;

 // free temporary memory
 FreeAlignedMemory(TempCol);
 FreeAlignedMemory(TempRow);
end;

procedure TGuiGaussianBlurFilter.SetKernelSize(const Value: Integer);
begin
 if FKernelSize <> Value then
  begin
   FKernelSize := Value;
   KernelSizeChanged;
  end;
end;

end.
