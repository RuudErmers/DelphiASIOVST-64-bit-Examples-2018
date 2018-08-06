unit DAV_GuiByteMap;

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
  {$IFDEF FPC} LCLIntf, LCLType, {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF DARWIN} MacOSAll, CarbonCanvas, CarbonPrivate, {$ENDIF}
  {$IFDEF GTK} {$IFDEF LCLGtk2}gdk2, gtk2, gdk2pixbuf, glib2, {$ELSE} gdk, gtk,
  gdkpixbuf, glib, gtkdef, {$ENDIF} {$ENDIF} {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes, SysUtils, DAV_MemoryUtils, DAV_GuiCommon,
  DAV_GuiCustomMap, DAV_GuiBlend;

type
  TGuiCustomByteMap = class(TGuiCustomMap)
  private
    function GetDataPointer: PByteArray;
    function GetValue(X, Y: Integer): Byte;
    function GetValuePointer(X, Y: Integer): PByteArray;
    function GetScanLine(Y: Integer): PByteArray;
    procedure SetValue(X, Y: Integer; const Value: Byte);
  protected
    FDataPointer : PByteArray;
    FBitmapInfo  : PBitmapInfo;
    procedure HeightChanged(UpdateBitmap: Boolean = True); override;
    procedure WidthChanged(UpdateBitmap: Boolean = True); override;

    procedure AssignTo(Dest: TPersistent); override;
    function Equal(ByteMap: TGuiCustomByteMap): Boolean;

    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Clear; overload; override;
    procedure Clear(Data: Byte); reintroduce; overload; virtual;
    procedure Multiply(Factor: Byte); virtual;

    procedure Draw(Bitmap: TBitmap); overload; virtual;
    procedure Draw(Bitmap: TBitmap; X, Y: Integer); overload; virtual; abstract;
    procedure Draw(ByteMap: TGuiCustomByteMap; Alpha: Byte = $FF); overload; virtual;
    procedure Draw(ByteMap: TGuiCustomByteMap; X, Y: Integer); overload; virtual;
    procedure Draw(ByteMap: TGuiCustomByteMap; X, Y: Integer; Alpha: Byte); overload; virtual;
    procedure DrawTransparent(ByteMap: TGuiCustomByteMap); overload; virtual;
    procedure DrawTransparent(ByteMap: TGuiCustomByteMap; X, Y: Integer); overload; virtual;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    // simple painting functions
    procedure FillRect(Rect: TRect; Data: Byte);
    procedure FrameRect(Rect: TRect; Data: Byte);
    procedure Line(FromX, FromY, ToX, ToY: Integer; Data: Byte);
    procedure HorizontalLine(FromX, ToX, Y: Integer; Data: Byte); overload;
    procedure HorizontalLine(FromX, ToX, Y: Integer; FromData, ToData: Byte); overload;
    procedure VerticalLine(X, FromY, ToY: Integer; Data: Byte);

    property DataPointer: PByteArray read GetDataPointer;
    property Value[X, Y: Integer]: Byte read GetValue write SetValue;
    property ValuePointer[X, Y: Integer]: PByteArray read GetValuePointer;
    property ScanLine[Y: Integer]: PByteArray read GetScanLine;
  end;

  TGuiByteMapMemory = class(TGuiCustomByteMap)
  protected
    procedure AllocateDataPointer; virtual;
    procedure HeightChanged(UpdateBitmap: Boolean = True); override;
    procedure WidthChanged(UpdateBitmap: Boolean = True); override;
    procedure SizeChangedAtOnce; override;
  public
    destructor Destroy; override;

    procedure PaintTo(Canvas: TCanvas; X, Y: Integer); override;
    procedure PaintTo(Canvas: TCanvas; Rect: TRect; X: Integer = 0; Y: Integer = 0); override;
    procedure Draw(Bitmap: TBitmap; X, Y: Integer); override;

    procedure Resize(Width, Height: Integer); override;
    procedure Turn(CounterClockwise: Boolean = False); override;
  published
    property Width;
    property Height;
    property OnChange;
    property OnResize;
  end;

  TGuiByteMapDIB = class(TGuiCustomByteMap)
  protected
    {$IFDEF MSWINDOWS}
    FBitmapHandle : HBITMAP;
    FDC           : HDC;
    {$ENDIF}
    {$IFDEF Darwin}
(*
    FProfile      : CMProfileRef;
    FContext      : CGContextRef;
    FCanvasHandle : TCarbonDeviceContext;
*)
    {$ENDIF}
    {$IFDEF GTK}
    FPixbuf       : PGdkPixBuf;
    {$ENDIF}

    procedure AllocateDeviceIndependentBitmap;
    procedure DisposeDeviceIndependentBitmap;
    procedure HeightChanged(UpdateBitmap: Boolean = True); override;
    procedure WidthChanged(UpdateBitmap: Boolean = True); override;
    procedure SizeChangedAtOnce; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure PaintTo(Canvas: TCanvas; X, Y: Integer); override;
    procedure PaintTo(Canvas: TCanvas; Rect: TRect; X: Integer = 0; Y: Integer = 0); override;
    procedure Draw(Bitmap: TBitmap; X, Y: Integer); override;

    procedure Resize(Width, Height: Integer); override;
    procedure Turn(CounterClockwise: Boolean = False); override;

    {$IFDEF MSWINDOWS}
    property Handle: HDC read FDC;
    {$ENDIF}
  published
    property Width;
    property Height;
    property OnChange;
    property OnResize;
  end;

implementation

uses
  Math, DAV_Common, DAV_GuiFileFormats, DAV_GuiPixelMap;

{ TGuiCustomByteMap }

constructor TGuiCustomByteMap.Create;
var
  Index : Integer;
begin
 inherited;
 FDataPointer := nil;

 GetMem(FBitmapInfo, SizeOf(TBitmapInfoHeader) + 256 * SizeOf(TRGBQuad));

 // initialize header
 FillChar(FBitmapInfo^.bmiHeader, SizeOf(TBitmapInfoHeader), 0);
 with FBitmapInfo^.bmiHeader do
  begin
   biSize := SizeOf(TBitmapInfoHeader);
   biBitCount := 8;
   biPlanes := 1;
   biCompression := BI_RGB;
  end;

 for Index := 0 to 255 do
  with FBitmapInfo^.bmiColors[Index] do
   begin
    rgbBlue     := Index;
    rgbGreen    := Index;
    rgbRed      := Index;
    rgbReserved := 0;
   end;
end;

destructor TGuiCustomByteMap.Destroy;
begin
 Dispose(FBitmapInfo);
 inherited;
end;

procedure TGuiCustomByteMap.Draw(ByteMap: TGuiCustomByteMap; X, Y: Integer);
var
  ClipRect : TRect;
  Index    : Integer;
begin
 with ClipRect do
  begin
   Left := X;
   if Left < 0 then Left := 0;
   Top := Y;
   if Top < 0 then Top := 0;
   Right := X + ByteMap.Width;
   if Right > Self.Width then Right := Self.Width;
   Bottom := Y + ByteMap.Height;
   if Bottom > Self.Height then Bottom := Self.Height;

   for Index := Top to Bottom - 1
    do System.Move(ByteMap.ValuePointer[Left - X, Top - Y + Index]^,
      ValuePointer[Left, Top + Index]^, Right - Left);
  end;
end;

procedure TGuiCustomByteMap.Draw(ByteMap: TGuiCustomByteMap; X, Y: Integer;
  Alpha: Byte);
var
  ClipRect : TRect;
  Index    : Integer;
begin
 with ClipRect do
  begin
   Left := X;
   if Left < 0 then Left := 0;
   Top := Y;
   if Top < 0 then Top := 0;
   Right := X + ByteMap.Width;
   if Right > Self.Width then Right := Self.Width;
   Bottom := Y + ByteMap.Height;
   if Bottom > Self.Height then Bottom := Self.Height;

   for Index := Top to Bottom - 1
    do System.Move(ByteMap.ValuePointer[Left - X, Top - Y + Index]^,
      ValuePointer[Left, Top + Index]^, Right - Left);
  end;
end;

procedure TGuiCustomByteMap.DrawTransparent(ByteMap: TGuiCustomByteMap);
begin
 DrawTransparent(ByteMap, 0, 0);
end;

procedure TGuiCustomByteMap.DrawTransparent(ByteMap: TGuiCustomByteMap; X,
  Y: Integer);
var
  ClipRect : TRect;
  Index    : Integer;
begin
 with ClipRect do
  begin
   Left := X;
   if Left < 0 then Left := 0;
   Top := Y;
   if Top < 0 then Top := 0;
   Right := X + ByteMap.Width;
   if Right > Self.Width then Right := Self.Width;
   Bottom := Y + ByteMap.Height;
   if Bottom > Self.Height then Bottom := Self.Height;

   // blend scanlines
   for Index := Top to Bottom - 1
    do System.Move(ByteMap.ValuePointer[Left - X, Index - Y]^,
      ValuePointer[Left, Index]^, Right - Left);
  end;
end;

procedure TGuiCustomByteMap.Draw(ByteMap: TGuiCustomByteMap;
  Alpha: Byte = $FF);
begin
 Draw(ByteMap, 0, 0, Alpha);
end;

procedure TGuiCustomByteMap.Assign(Source: TPersistent);
var
  TempBitmap : TBitmap;
begin
 if Source is TGuiCustomByteMap then
  with TGuiCustomByteMap(Source) do
   begin
    Self.SetSize(Width, Height);
    System.Move(FBitmapInfo^, Self.FBitmapInfo^, SizeOf(TBitmapInfoHeader));
    Assert(Self.FDataSize = FDataSize);
    System.Move(FDataPointer^, Self.FDataPointer^, FDataSize);
    Self.FOnChange := FOnChange;
    Self.FOnResize := FOnResize;
   end else
(*
 if Source is TGuiCustomByteMap then
  with TGuiCustomByteMap(Source) do
   begin
    Self.SetSize(Width, Height);
    Self.FBitmapInfo := FBitmapInfo;

    Assert(Self.FDataSize = FDataSize);
//    Move(FDataPointer^, Self.FDataPointer^, FDataSize);

    Self.FOnChange := FOnChange;
    Self.FOnResize := FOnResize;
   end else
*)
 if Source is TBitmap then
  with TBitmap(Source) do
   begin
    Self.SetSize(Width, Height);
    Draw(TBitmap(Source));
    if Assigned(FOnChange) then FOnChange(Self);
   end else
 if Source is TGraphic then
  with TGraphic(Source) do
   begin
    Self.SetSize(Width, Height);
    TempBitmap := TBitmap.Create;
    try
     TempBitmap.Assign(Source);
     Draw(TempBitmap);
     if Assigned(FOnChange) then FOnChange(Self);
    finally
     if Assigned(TempBitmap)
      then FreeAndNil(TempBitmap);
    end;
   end
 else inherited;
end;

procedure TGuiCustomByteMap.AssignTo(Dest: TPersistent);
begin
 if Dest is TGuiCustomByteMap then
  with TGuiCustomByteMap(Dest) do
   begin
    SetSize(Self.Width, Self.Height);
    FBitmapInfo := Self.FBitmapInfo;

    Assert(FDataSize = Self.FDataSize);
    System.Move(Self.FDataPointer^, FDataPointer^, FDataSize);

    FOnChange := Self.FOnChange;
    FOnResize := Self.FOnResize;
   end
 else inherited;
end;

procedure TGuiCustomByteMap.Draw(Bitmap: TBitmap);
begin
 Draw(Bitmap, 0, 0);
end;

procedure TGuiCustomByteMap.Clear;
begin
 FillChar(FDataPointer^, FDataSize, 0);
end;

procedure TGuiCustomByteMap.Clear(Data: Byte);
var
  Index : Integer;
begin
 for Index := 0 to FWidth * FHeight - 1
  do FDataPointer^[Index] := Data;
end;

function TGuiCustomByteMap.GetDataPointer: PByteArray;
begin
 Result := FDataPointer;
end;

function TGuiCustomByteMap.GetValue(X, Y: Integer): Byte;
begin
 Result := FDataPointer^[Y * Width + X];
end;

function TGuiCustomByteMap.GetValuePointer(X, Y: Integer): PByteArray;
begin
 Result := @FDataPointer^[Y * Width + X];
end;

function TGuiCustomByteMap.GetScanLine(Y: Integer): PByteArray;
begin
 Result := @FDataPointer^[Y * Width];
end;

procedure TGuiCustomByteMap.SetValue(X, Y: Integer; const Value: Byte);
begin
 FDataPointer[Y * Width + X] := Value;
end;

procedure TGuiCustomByteMap.HeightChanged(UpdateBitmap: Boolean = True);
begin
 FBitmapInfo.bmiHeader.biHeight := -FHeight;
 if UpdateBitmap then Resized;
end;

procedure TGuiCustomByteMap.WidthChanged(UpdateBitmap: Boolean = True);
begin
 FBitmapInfo.bmiHeader.biWidth := FWidth;
 if UpdateBitmap then Resized;
end;

procedure TGuiCustomByteMap.LoadFromStream(Stream: TStream);
var
  BitmapFileHeader : TBitmapFileHeader;
  FileFormatClass  : TGuiCustomFileFormatClass;
  BitmapInfo       : TBitmapInfo;
  Bitmap           : TBitmap;
begin
 FileFormatClass := FindGraphicFileFormatByStream(Stream);
 if Assigned(FileFormatClass) then
  begin
   with FileFormatClass.Create do
    try
     LoadFromStream(Stream);
     AssignTo(Self);
     Exit;
    finally
     Free;
    end;
  end;

 // if no file format was found use the default method
 with Stream do
  begin
   if Size < SizeOf(TBitmapFileHeader)
    then raise Exception.Create('Invalid bitmap header found!');

   Read(BitmapFileHeader, SizeOf(TBitmapFileHeader));

   if BitmapFileHeader.bfType <> $4D42
    then raise Exception.Create('Invalid bitmap header found!');

   Read(BitmapInfo, SizeOf(TBitmapInfo));

   if BitmapInfo.bmiHeader.biBitCount = 32 then
    begin

    end
   else
    begin
     Stream.Seek(-(SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfo)), soFromCurrent);
     Bitmap := TBitmap.Create;
     try
      Bitmap.LoadFromStream(Stream);
      Self.Assign(Bitmap);
     finally
      FreeAndNil(Bitmap);
     end;
    end;
  end;
end;

procedure TGuiCustomByteMap.Multiply(Factor: Byte);
var
  Index : Integer;
begin
 for Index := 0 to FWidth * FHeight - 1
  do FDataPointer^[Index] := ((FDataPointer^[Index] * Factor + $80) shr 8);
end;

procedure TGuiCustomByteMap.SaveToStream(Stream: TStream);
var
  BitmapFileHeader : TBitmapFileHeader;
begin
 with Stream do
  begin
   // initialize bitmap file header
   FillChar(BitmapFileHeader, SizeOf(BitmapFileHeader), 0);
   with BitmapFileHeader do
    begin
     bfType := $4D42;
     bfSize := SizeOf(TBitmapInfo) + Width * Height * SizeOf(Cardinal);
     bfOffBits := SizeOf(BitmapFileHeader) + SizeOf(TBitmapInfo);
    end;

   // write bitmap file header to stream
   Write(BitmapFileHeader, SizeOf(TBitmapFileHeader));

   // write bitmap file header to stream
   Write(FBitmapInfo, SizeOf(TBitmapInfo));

   Write(FDataPointer^, Width * Height * SizeOf(Cardinal));
  end;
end;

function TGuiCustomByteMap.Equal(ByteMap: TGuiCustomByteMap): Boolean;
begin
 Result := (ByteMap.Width = FWidth) and (ByteMap.Height = FHeight);

 if Result
  then Result := CompareMem(FDataPointer, ByteMap.FDataPointer, FDataSize);
end;

procedure TGuiCustomByteMap.ReadData(Stream: TStream);
var
  TempWidth, TempHeight: Integer;
begin
 with Stream do
  try
   ReadBuffer(TempWidth, 4);
   ReadBuffer(TempHeight, 4);
   SetSize(TempWidth, TempHeight);
   Assert(FDataSize = FWidth * FHeight);
   ReadBuffer(FDataPointer^, FDataSize);
  finally
   Changed;
  end;
end;

procedure TGuiCustomByteMap.WriteData(Stream: TStream);
begin
 with Stream do
  begin
   WriteBuffer(FWidth, 4);
   WriteBuffer(FHeight, 4);
   Assert(FDataSize = FWidth * FHeight);
   WriteBuffer(FDataPointer^, FDataSize);
  end;
end;

procedure TGuiCustomByteMap.DefineProperties(Filer: TFiler);
var
  HasData : Boolean;
begin
 HasData := (FDataSize > 0);
 if HasData and (Filer.Ancestor <> nil)
  then HasData := not ((Filer.Ancestor is TGuiCustomByteMap) and
    Equal(TGuiCustomByteMap(Filer.Ancestor)));

 Filer.DefineBinaryProperty('Data', ReadData, WriteData, HasData);
end;

procedure TGuiCustomByteMap.FillRect(Rect: TRect; Data: Byte);
var
  X, Y : Integer;
begin
 for Y := Rect.Top to Rect.Bottom - 1 do
  for X := Rect.Left to Rect.Right - 1
   do FDataPointer[Y * Width + X] := Data
end;

procedure TGuiCustomByteMap.FrameRect(Rect: TRect; Data: Byte);
begin
 // top & bottom
 HorizontalLine(Rect.Left, Rect.Right, Rect.Top, Data);
 HorizontalLine(Rect.Left, Rect.Right, Rect.Bottom - 1, Data);

 // left & right
 VerticalLine(Rect.Left, Rect.Top + 1, Rect.Bottom - 1, Data);
 VerticalLine(Rect.Right - 1, Rect.Top + 1, Rect.Bottom - 1, Data);
end;

procedure TGuiCustomByteMap.VerticalLine(X, FromY, ToY: Integer; Data: Byte);
var
  Y : Integer;
begin
 if ToY < FromY  then
  for Y := ToY to FromY - 1
   do FDataPointer[Y * Width + X] := Data
 else
  for Y := FromY to ToY - 1
   do FDataPointer[Y * Width + X] := Data;
end;

procedure TGuiCustomByteMap.HorizontalLine(FromX, ToX, Y: Integer;
  Data: Byte);
var
  X : Integer;
begin
 if FromX > ToX
  then Exchange32(FromX, ToX);

 for X := FromX to ToX - 1
  do FDataPointer[Y * Width + X] := Data;
end;

procedure TGuiCustomByteMap.HorizontalLine(FromX, ToX, Y: Integer; FromData,
  ToData: Byte);
var
  X    : Integer;
  R, S : Single;
begin
 if FromX > ToX
  then Exchange32(FromX, ToX);

 S := 1 / (ToX - FromX);
 R := 0;
 for X := FromX to ToX - 1 do
  begin
   FDataPointer[Y * Width + X] := Trunc(FromData + R * (ToData - FromData));
   R := R + S;
   EMMS;
  end;
end;

procedure TGuiCustomByteMap.Line(FromX, FromY, ToX, ToY: Integer; Data: Byte);
var
  x, y, t     : Integer;
  dx, dy      : Integer;
  incx, incy  : Integer;
  pdx, pdy    : Integer;
  ddx, ddy    : Integer;
  es, el, err : Integer;
begin
 if FromY = ToY then HorizontalLine(FromX, ToX, FromY, Data) else
 if FromX = ToX then VerticalLine(FromX, FromY, ToY, Data) else
  begin
   dx := ToX - FromX;
   dy := ToY - FromY;

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

   x := FromX;
   y := FromY;
   err := el shr 1;
   FDataPointer[Y * Width + X] := Data;

   for t := 1 to el - 1 do
    begin
     err := err - es;
     if (err < 0) then
      begin
       err := err + el;
       x := x + ddx;
       y := y + ddy;
      end
     else
      begin
       x := x + pdx;
       y := y + pdy;
      end;
     FDataPointer[Y * Width + X] := Data;
    end;
  end;
end;


{ TGuiByteMapMemory }

destructor TGuiByteMapMemory.Destroy;
begin
 Dispose(FDataPointer);
 inherited;
end;

procedure TGuiByteMapMemory.Draw(Bitmap: TBitmap; X, Y: Integer);
begin
 if (Bitmap.Height <> 0) and (FDataPointer <> nil) then
  begin
   if GetDIBits(Bitmap.Canvas.Handle, Bitmap.Handle, 0, Bitmap.Height,
     FDataPointer, FBitmapInfo^, DIB_RGB_COLORS) = 0
    then raise Exception.Create('Error');
  end;

(*
var
  CompDC     : HDC;
  CompBitmap : HBITMAP;

 CompDC := CreateCompatibleDC(Canvas.Handle);
 try
  CompBitmap := CreateCompatibleBitmap(CompDC, Width, Height);
  SelectObject(CompDC, CompBitmap);
  if CompBitmap <> 0 then
   try
    BitBlt(CompDC, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);

    if GetDIBits(Canvas.Handle, CompBitmap, 0, Height, FDataPointer, FBitmapInfo,
      DIB_RGB_COLORS) = 0
      then raise Exception.Create('Error');
   finally
    DeleteObject(CompBitmap);
   end;
 finally
  DeleteDC(CompDC);
 end;
*)
end;

procedure TGuiByteMapMemory.PaintTo(Canvas: TCanvas; X, Y: Integer);
var
  Bitmap        : HBITMAP;
  DeviceContext : HDC;
  Buffer        : Pointer;
  OldObject     : HGDIOBJ;
begin
 {$IFDEF MSWINDOWS}
 if SetDIBitsToDevice(Canvas.Handle, X, Y, Width, Height, 0, 0, 0, Height,
   FDataPointer, FBitmapInfo^, DIB_RGB_COLORS) = 0 then
  begin
   // create compatible device context
   DeviceContext := CreateCompatibleDC(Canvas.Handle);
   if DeviceContext <> 0 then
    try
     Bitmap := CreateDIBSection(DeviceContext, FBitmapInfo^,
       DIB_RGB_COLORS, Buffer, 0, 0);

     if Bitmap <> 0 then
      begin
       OldObject := SelectObject(DeviceContext, Bitmap);
       try
        Move(FDataPointer^, Buffer^, Width * Height * SizeOf(Cardinal));
        BitBlt(Canvas.Handle, X, Y, Width, Height, DeviceContext,
          0, 0, SRCCOPY);
       finally
        if OldObject <> 0
         then SelectObject(DeviceContext, OldObject);
        DeleteObject(Bitmap);
       end;
      end;
    finally
     DeleteDC(DeviceContext);
    end;
  end;
 {$ENDIF}
end;

procedure TGuiByteMapMemory.PaintTo(Canvas: TCanvas; Rect: TRect;
  X: Integer = 0; Y: Integer = 0);
var
  Bitmap        : HBITMAP;
  DeviceContext : HDC;
  Buffer        : Pointer;
  OldObject     : HGDIOBJ;
  H, W          : Integer;
begin
 {$IFDEF MSWINDOWS}
 W := Min(Width, Rect.Right - Rect.Left);
 H := Min(Height, Rect.Bottom - Rect.Top);
 if SetDIBitsToDevice(Canvas.Handle, X, Y, W, H, Rect.Left, Rect.Top, 0, Height,
   FDataPointer, FBitmapInfo^, DIB_RGB_COLORS) = 0 then
  begin
   // create compatible device context
   DeviceContext := CreateCompatibleDC(Canvas.Handle);
   if DeviceContext <> 0 then
    try
     Bitmap := CreateDIBSection(DeviceContext, FBitmapInfo^, DIB_RGB_COLORS,
       Buffer, 0, 0);

     if Bitmap <> 0 then
      begin
       OldObject := SelectObject(DeviceContext, Bitmap);
       try
        Move(FDataPointer^, Buffer^, Width * Height * SizeOf(Cardinal));
        BitBlt(Canvas.Handle, X, Y, W, H, DeviceContext, Rect.Left, Rect.Top,
          SRCCOPY);
       finally
        if OldObject <> 0
         then SelectObject(DeviceContext, OldObject);
        DeleteObject(Bitmap);
       end;
      end;
    finally
     DeleteDC(DeviceContext);
    end;
  end;
 {$ENDIF}
end;

procedure TGuiByteMapMemory.Resize(Width, Height: Integer);
var
  NewSize  : Integer;
  NewData  : PByteArray;
  TempData : PByteArray;
  Y, Wdth  : Integer;
begin
 inherited;
 if (Width <> FWidth) or (Height <> FHeight) then
  begin
   NewSize := Width * Height;
   GetAlignedMemory(Pointer(NewData), NewSize);

   Wdth := Min(Width, FWidth);
   for Y := 0 to Min(Height, FHeight) - 1 do
     System.Move(FDataPointer^[Y * FWidth], NewData^[Y * Width], Wdth);

   // set new width (not thread safe!)
   FWidth := Width;
   FHeight := Height;
   FBitmapInfo.bmiHeader.biWidth := Width;
   FBitmapInfo.bmiHeader.biHeight := -Height;

   // exchange data pointer
   TempData := FDataPointer;
   FDataPointer := NewData;
   FDataSize := NewSize;

   // dispose old data pointer
   FreeAlignedMemory(TempData);
  end;
end;

procedure TGuiByteMapMemory.Turn(CounterClockwise: Boolean);
var
  TurnData : PByteArray;
  TempData : PByteArray;
  X, Y     : Integer;
begin
 inherited;
 GetAlignedMemory(Pointer(TurnData), FDataSize);

 // perform turn
 if CounterClockwise then
  for Y := 0 to FHeight - 1 do
   for X := 0 to FWidth - 1
    do TurnData^[(FWidth - 1 - X) * FHeight + Y] := FDataPointer^[Y * FWidth + X]
 else
  for Y := 0 to FHeight - 1 do
   for X := 0 to FWidth - 1
    do TurnData^[X * FHeight + FHeight - 1 - Y] := FDataPointer^[Y * FWidth + X];

 // exchange width and height
 with FBitmapInfo^ do
  begin
   bmiHeader.biWidth := FHeight;
   bmiHeader.biHeight := FWidth;
   FWidth := bmiHeader.biWidth;
   FHeight := bmiHeader.biHeight;
  end;

 // exchange data pointer
 TempData := FDataPointer;
 FDataPointer := TurnData;

 // dispose old data pointer
 FreeAlignedMemory(TempData);
end;

procedure TGuiByteMapMemory.HeightChanged(UpdateBitmap: Boolean);
begin
 inherited;
 if UpdateBitmap then AllocateDataPointer;
end;

procedure TGuiByteMapMemory.WidthChanged(UpdateBitmap: Boolean);
begin
 inherited;
 if UpdateBitmap then AllocateDataPointer;
end;

procedure TGuiByteMapMemory.SizeChangedAtOnce;
begin
 inherited;
 AllocateDataPointer;
end;

procedure TGuiByteMapMemory.AllocateDataPointer;
var
  NewDataSize : Integer;
begin
 NewDataSize := FWidth * FHeight;
 if FDataSize <> NewDataSize then
  begin
   FDataSize := NewDataSize;
   Assert(FDataSize >= 0);
   ReallocateAlignedMemory(Pointer(FDataPointer), FDataSize);
   Clear;
  end;
end;


{ TGuiByteMapDIB }

constructor TGuiByteMapDIB.Create;
begin
 inherited;
 {$IFDEF MSWINDOWS}
 FDC            := 0;
 FBitmapHandle  := 0;
 {$ENDIF}
end;

destructor TGuiByteMapDIB.Destroy;
begin
 DisposeDeviceIndependentBitmap;
 inherited;
end;

procedure TGuiByteMapDIB.AllocateDeviceIndependentBitmap;
var
  NewDataSize : Integer;
begin
 NewDataSize := FWidth * FHeight;
 if (FDataSize <> NewDataSize) or (FBitmapHandle = 0) then
  begin
   FDataSize := NewDataSize;
   Assert(FDataSize >= 0);

   FBitmapHandle := CreateDIBSection(0, FBitmapInfo^, DIB_RGB_COLORS,
     Pointer(FDataPointer), 0, 0);

   if FDataPointer = nil
    then raise Exception.Create(RCStrNoDibHandle);

   FDC := CreateCompatibleDC(0);
   if FDC = 0 then
    begin
     DeleteObject(FBitmapHandle);
     FBitmapHandle := 0;
     FDataPointer := nil;
     FDataSize := 0;
     raise Exception.Create(RCStrNoCompatibleDC);
    end;

   if SelectObject(FDC, FBitmapHandle) = 0 then
    begin
     DeleteDC(FDC);
     DeleteObject(FBitmapHandle);
     FDC := 0;
     FBitmapHandle := 0;
     FDataPointer := nil;
     FDataSize := 0;
     raise Exception.Create(RCStrNoSelectedDC);
    end;
  end;
end;

procedure TGuiByteMapDIB.DisposeDeviceIndependentBitmap;
begin
 if FDC <> 0 then DeleteDC(FDC);
 FDC := 0;
 if FBitmapHandle <> 0
  then DeleteObject(FBitmapHandle);
 FBitmapHandle := 0;

 FDataPointer := nil;
end;

procedure TGuiByteMapDIB.Draw(Bitmap: TBitmap; X, Y: Integer);
(*
var
  CompBitmap : HBITMAP;
*)
begin
(*
 CompBitmap := CreateCompatibleBitmap(Canvas.Handle, Width, Height);
 if CompBitmap <> 0 then
  try
   GetDIBits(Canvas.Handle, CompBitmap, 0, Height, FDataPointer, FBitmapInfo,
     DIB_RGB_COLORS)
  finally
   DeleteObject(CompBitmap);
  end;
*)
end;

procedure TGuiByteMapDIB.PaintTo(Canvas: TCanvas; X, Y: Integer);
begin
 BitBlt(Canvas.Handle, X, Y, Width, Height, FDC, X, Y, SRCCOPY);
end;

procedure TGuiByteMapDIB.PaintTo(Canvas: TCanvas; Rect: TRect; X: Integer = 0;
  Y: Integer = 0);
begin
 BitBlt(Canvas.Handle, X, Y, Min(Width, Rect.Right - Rect.Left),
   Min(Height, Rect.Bottom - Rect.Top), FDC, Rect.Left, Rect.Top, SRCCOPY);
end;

procedure TGuiByteMapDIB.SizeChangedAtOnce;
begin
 inherited;
 DisposeDeviceIndependentBitmap;
 if Width * Height <> 0
  then AllocateDeviceIndependentBitmap;
end;

procedure TGuiByteMapDIB.Resize(Width, Height: Integer);
var
  TempData : TGuiPixelMapMemory;
  Y, Wdth  : Integer;
begin
 inherited;
 if (Width <> FWidth) or (Height <> FHeight) then
  begin
   TempData := TGuiPixelMapMemory.Create;
   try
    TempData.Assign(Self);

    // set new size of this bitmap
    SetSize(Width, Height);

    Wdth := Min(Width, FWidth);
    for Y := 0 to Min(TempData.Height, Height) - 1 do
      Move(TempData.DataPointer^[Y * TempData.Width], DataPointer^[Y * Width],
        Wdth * SizeOf(TPixel32));
   finally
    FreeAndNil(TempData);
   end;
  end;
end;

procedure TGuiByteMapDIB.Turn(CounterClockwise: Boolean);
var
  TurnData : PByteArray;
  X, Y     : Integer;
begin
 inherited;
 GetAlignedMemory(Pointer(TurnData), FDataSize);

 try
  // perform turn
  if CounterClockwise then
   for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1
     do TurnData^[(FWidth - 1 - X) * FHeight + Y] := FDataPointer^[Y * FWidth + X]
  else
   for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1
     do TurnData^[X * FHeight + FHeight - 1 - Y] := FDataPointer^[Y * FWidth + X];

  // exchange width and height
  with FBitmapInfo^ do
   begin
    bmiHeader.biWidth := FHeight;
    bmiHeader.biHeight := -FWidth;
    FWidth := bmiHeader.biWidth;
    FHeight := -bmiHeader.biHeight;
   end;

  // exchange data pointer
  Move(TurnData^, FDataPointer^, FDataSize);
 finally
  FreeAlignedMemory(Pointer(TurnData));
 end;
end;

procedure TGuiByteMapDIB.HeightChanged(UpdateBitmap: Boolean);
begin
 inherited;
 if UpdateBitmap then
  begin
   DisposeDeviceIndependentBitmap;
   if Width * Height <> 0
    then AllocateDeviceIndependentBitmap;
  end;
end;

procedure TGuiByteMapDIB.WidthChanged(UpdateBitmap: Boolean);
begin
 inherited;
 if UpdateBitmap then
  begin
   DisposeDeviceIndependentBitmap;
   if Width * Height <> 0
    then AllocateDeviceIndependentBitmap;
  end;
end;

end.
