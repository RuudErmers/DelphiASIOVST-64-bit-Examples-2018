unit DAV_GuiPixelMap;

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
  Graphics, Classes, Controls, SysUtils, DAV_MemoryUtils, DAV_GuiCommon, Dialogs, Forms,
  DAV_GuiCustomMap, DAV_GuiInterface, {$IFDEF MSWINDOWS} DAV_GuiByteMap,
  {$ENDIF} DAV_GuiBlend;

{$IFDEF DARWIN}
const
  CStrGenericRGBProfilePath = '/System/Library/ColorSync/Profiles/Generic RGB Profile.icc';
{$ENDIF}

type
  TGuiCustomPixelMap = class(TGuiCustomMap, IPixel32Access)
  private
    function GetDataPointer: PPixel32Array;
    function GetPixel(X, Y: Integer): TPixel32;
    function GetPixelPointer(X, Y: Integer): PPixel32;
    function GetScanLine(Y: Integer): PPixel32Array;
    procedure SetPixel(X, Y: Integer; const Value: TPixel32);
  protected
    FDataPointer : PPixel32Array;
    FBitmapInfo  : TBitmapInfo;
    procedure HeightChanged(UpdateBitmap: Boolean = True); override;
    procedure WidthChanged(UpdateBitmap: Boolean = True); override;

    procedure AssignTo(Dest: TPersistent); override;
    function Equal(PixelMap: TGuiCustomPixelMap): Boolean;

    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create; override;

    procedure Clear; overload; override;
    procedure Clear(Color: TColor); reintroduce; overload; virtual;
    procedure Clear(Color: TPixel32); reintroduce; overload; virtual;

    procedure CopyParentImage(Control: TControl);
    procedure MakeOpaque; virtual;
    procedure ResetAlpha(Value: Byte = 0); virtual;

    procedure Draw(Bitmap: TBitmap); overload; virtual;
    procedure Draw(Bitmap: TBitmap; X, Y: Integer); overload; virtual; abstract;
    procedure Draw(PixelMap: TGuiCustomPixelMap; Alpha: Byte = $FF); overload; virtual;
    procedure Draw(PixelMap: TGuiCustomPixelMap; X, Y: Integer); overload; virtual;
    procedure Draw(PixelMap: TGuiCustomPixelMap; X, Y: Integer; Alpha: Byte); overload; virtual;
    procedure DrawTransparent(PixelMap: TGuiCustomPixelMap); overload; virtual;
    procedure DrawTransparent(PixelMap: TGuiCustomPixelMap; X, Y: Integer); overload; virtual;
    {$IFDEF MSWINDOWS}
    procedure DrawByteMap(ByteMap: TGuiCustomByteMap; Color: TPixel32); overload; virtual;
    procedure DrawByteMap(ByteMap: TGuiCustomByteMap; Color: TPixel32; X, Y: Integer); overload; virtual;
    procedure DrawByteMap(ByteMap: TGuiCustomByteMap; Color: TPixel32; Rect: TRect); overload; virtual;
    {$ENDIF}

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    // simple painting functions
    procedure FillRect(Rect: TRect; Color: TPixel32); overload;
    procedure FillRect(Left, Top, Right, Bottom: Integer; Color: TPixel32); overload;
    procedure FrameRect(Rect: TRect; Color: TPixel32);
    procedure Line(FromX, FromY, ToX, ToY: Integer; Color: TPixel32);
    procedure HorizontalLine(FromX, ToX, Y: Integer; Color: TPixel32); overload;
    procedure HorizontalLine(FromX, ToX, Y: Integer; FromColor, ToColor: TPixel32); overload;
    procedure VerticalLine(X, FromY, ToY: Integer; Color: TPixel32);
    procedure Assign(Source: TPersistent); override;
    function Equals(Obj: TObject): Boolean; {$IFDEF FPC} override; {$ENDIF} {$IFDEF DELPHI14_UP} override; {$ENDIF}

    property DataPointer: PPixel32Array read GetDataPointer;
    property Pixel[X, Y: Integer]: TPixel32 read GetPixel write SetPixel;
    property PixelPointer[X, Y: Integer]: PPixel32 read GetPixelPointer;
    property ScanLine[Y: Integer]: PPixel32Array read GetScanLine;
  end;

  TGuiPixelMapMemory = class(TGuiCustomPixelMap)
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

    procedure Resize(Width: Integer; Height: Integer); override;
    procedure Turn(CounterClockwise: Boolean = False); override;
  published
    property Width;
    property Height;
    property OnChange;
    property OnResize;
  end;

  TGuiPixelMapDIB = class(TGuiCustomPixelMap)
  private
    function GetCanvas: TCanvas;
  protected
    {$IFDEF MSWINDOWS}
    FBitmapHandle : HBITMAP;
    FDC           : HDC;
    {$ENDIF}
    {$IFDEF Darwin}
    FProfile      : CMProfileRef;
    FColorSpace   : CGColorSpaceRef;
    FContext      : CGContextRef;
    FCanvasHandle : TCarbonDeviceContext;
    {$ENDIF}
    {$IFDEF GTK}
    FPixbuf       : PGdkPixBuf;
    {$ENDIF}

    FCanvas       : TCanvas;
    procedure AllocateDeviceIndependentBitmap;
    procedure DisposeDeviceIndependentBitmap;
    procedure CanvasChanged(Sender: TObject); virtual;
    procedure HeightChanged(UpdateBitmap: Boolean = True); override;
    procedure WidthChanged(UpdateBitmap: Boolean = True); override;
    procedure SizeChangedAtOnce; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure PaintTo(Canvas: TCanvas; X, Y: Integer); override;
    procedure PaintTo(Canvas: TCanvas; LRect: TRect; X: Integer = 0; Y: Integer = 0); override;
    procedure Draw(Bitmap: TBitmap; X, Y: Integer); override;

    procedure Resize(Width, Height: Integer); override;
    procedure Turn(CounterClockwise: Boolean = False); override;

    property Canvas: TCanvas read GetCanvas;
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
  Math, DAV_Common, DAV_GuiFileFormats;

{$IFDEF Darwin}
resourcestring
  RCStrCouldntCreateGenericProfile = 'Couldn''t create the generic profile';
  RCStrCouldntCreateGenericColorSpace = 'Couldn''t create the generic RGB color space';
{$ENDIF}

{ TGuiCustomPixelMap }

constructor TGuiCustomPixelMap.Create;
begin
 inherited;
 FDataPointer := nil;
 FDataSize := 0;

 // initialize header
 FillChar(FBitmapInfo.bmiHeader, SizeOf(TBitmapInfoHeader), 0);
 with FBitmapInfo.bmiHeader do
  begin
   biSize := SizeOf(TBitmapInfoHeader);
   biBitCount := 32;
   biPlanes := 1;
   biCompression := BI_RGB;
  end;
end;

procedure TGuiCustomPixelMap.Draw(PixelMap: TGuiCustomPixelMap; X, Y: Integer);
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
   Right := X + PixelMap.Width;
   if Right > Self.Width then Right := Self.Width;
   Bottom := Y + PixelMap.Height;
   if Bottom > Self.Height then Bottom := Self.Height;

   // blend scanlines
   for Index := Top to Bottom - 1
     do BlendLine(PixelMap.PixelPointer[Left - X, Index - Y],
       PixelPointer[Left, Index], Right - Left);
   EMMS;
  end;
end;

procedure TGuiCustomPixelMap.Draw(PixelMap: TGuiCustomPixelMap; X, Y: Integer;
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
   Right := X + PixelMap.Width;
   if Right > Self.Width then Right := Self.Width;
   Bottom := Y + PixelMap.Height;
   if Bottom > Self.Height then Bottom := Self.Height;

   // combine scanlines
   for Index := Top to Bottom - 1
     do CombineLine(PixelMap.PixelPointer[Left - X, Index - Y],
       PixelPointer[Left, Index], Right - Left, Alpha);
   EMMS;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TGuiCustomPixelMap.DrawByteMap(ByteMap: TGuiCustomByteMap;
  Color: TPixel32);
begin
 DrawByteMap(ByteMap, Color, 0, 0);
end;

procedure TGuiCustomPixelMap.DrawByteMap(ByteMap: TGuiCustomByteMap;
  Color: TPixel32; X, Y: Integer);
var
  ClipRect : TRect;
  IX, IY   : Integer;
  RGB      : Cardinal;
  NewColor : TPixel32;
  ScnLn    : PPixel32Array;
  ByteLine : PByteArray;
  Alpha    : Byte;
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

   // split RGB and alpha
   RGB := Color.ARGB and $FFFFFF;
   Alpha := Color.A;

   // blend scanlines
   for IY := Top to Bottom - 1 do
    begin
     ScnLn := ScanLine[IY];
     ByteLine := ByteMap.ScanLine[IY - Y];
     for IX := Left to Right - 1 do
      begin
       NewColor.ARGB := (((ByteLine^[IX - X] * Alpha) shl 16) and $FF000000) or RGB;
       MergePixelInplace(NewColor, ScnLn^[IX]);
      end;
    end;
   EMMS;
  end;
end;

procedure TGuiCustomPixelMap.DrawByteMap(ByteMap: TGuiCustomByteMap;
  Color: TPixel32; Rect: TRect);
var
  ClipRect : TRect;
  IX, IY   : Integer;
  RGB      : Cardinal;
  NewColor : TPixel32;
  ScnLn    : PPixel32Array;
  ByteLine : PByteArray;
  Alpha    : Byte;
begin
 with ClipRect do
  begin
   Left := Rect.Left;
   if Left < 0 then Left := 0;
   Top := Rect.Top;
   if Top < 0 then Top := 0;
   Right := Min(Rect.Left + ByteMap.Width, Rect.Right);
   if Right > Self.Width then Right := Self.Width;
   Bottom := Min(Rect.Top + ByteMap.Height, Rect.Bottom);
   if Bottom > Self.Height then Bottom := Self.Height;

   // split RGB and alpha
   RGB := Color.ARGB and $FFFFFF;
   Alpha := Color.A;

   // blend scanlines
   for IY := Top to Bottom - 1 do
    begin
     ScnLn := ScanLine[IY];
     ByteLine := ByteMap.ScanLine[IY - Rect.Top];
     for IX := Left to Right - 1 do
      begin
       NewColor.ARGB := (((ByteLine^[IX - Rect.Left] * Alpha) shl 16) and $FF000000) or RGB;
       MergePixelInplace(NewColor, ScnLn^[IX]);
      end;
    end;
   EMMS;
  end;
end;
{$ENDIF}

procedure TGuiCustomPixelMap.DrawTransparent(PixelMap: TGuiCustomPixelMap);
begin
 DrawTransparent(PixelMap, 0, 0);
end;

procedure TGuiCustomPixelMap.DrawTransparent(PixelMap: TGuiCustomPixelMap; X,
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
   Right := X + PixelMap.Width;
   if Right > Self.Width then Right := Self.Width;
   Bottom := Y + PixelMap.Height;
   if Bottom > Self.Height then Bottom := Self.Height;

   // blend scanlines
   for Index := Top to Bottom - 1
     do BlendLine(PixelMap.PixelPointer[Left - X, Index - Y],
       PixelPointer[Left, Index], Right - Left);
  end;
end;

procedure TGuiCustomPixelMap.Draw(PixelMap: TGuiCustomPixelMap;
  Alpha: Byte = $FF);
begin
 Draw(PixelMap, 0, 0, Alpha);
end;

procedure TGuiCustomPixelMap.Assign(Source: TPersistent);
var
  TempBitmap : TBitmap;
begin
(*
 if Source is TGuiCustomByteMap then
  with TGuiCustomPixelMap(Source) do
   begin
    Self.SetSize(Width, Height);
    Self.FBitmapInfo := FBitmapInfo;

    Assert(Self.FDataSize = FDataSize);
//    Move(FDataPointer^, Self.FDataPointer^, FDataSize);

    Self.FOnChange := FOnChange;
    Self.FOnResize := FOnResize;
   end else
*)
 if Source is TGuiCustomPixelMap then
  with TGuiCustomPixelMap(Source) do
   begin
    Self.SetSize(Width, Height);
    Self.FBitmapInfo := FBitmapInfo;

    Assert(Self.FDataSize = FDataSize);
    System.Move(FDataPointer^, Self.FDataPointer^, FDataSize);

    Self.FOnChange := FOnChange;
    Self.FOnResize := FOnResize;
   end else
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

procedure TGuiCustomPixelMap.AssignTo(Dest: TPersistent);
begin
 if Dest is TGuiCustomPixelMap then
  with TGuiCustomPixelMap(Dest) do
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

procedure TGuiCustomPixelMap.Draw(Bitmap: TBitmap);
begin
 Draw(Bitmap, 0, 0);
end;

{$IFNDEF FPC}
type
  TParentControl = class(TWinControl);
{$ENDIF}

{-$DEFINE UsePixelMap}

procedure TGuiCustomPixelMap.CopyParentImage(Control: TControl);
{$IFNDEF FPC}
var
  I         : Integer;
  SubCount  : Integer;
  SaveIndex : Integer;
  Pnt       : TPoint;
  R, SelfR  : TRect;
  CtlR      : TRect;
  {$IFDEF UsePixelMap}
  Bmp       : TGuiPixelMapDIB;
  {$ELSE}
  Bmp       : TBitmap;
  {$ENDIF}
{$ENDIF}
begin
(*
  if Supports(Control.Parent, IPixel32Access) then
   begin
//    IPixel32Access(Control.Owner).;
    Exit;
   end;
*)

{$IFNDEF FPC}
 if (Control.Parent = nil) then Exit;
 SubCount := Control.Parent.ControlCount;

 {$IFDEF WIN32}
 with Control.Parent
  do Control.ControlState := Control.ControlState + [csPaintCopy];
 try
 {$ENDIF}

  SelfR := Bounds(Control.Left, Control.Top, Control.Width, Control.Height);
  Pnt.X := -Control.Left;
  Pnt.Y := -Control.Top;

  {$IFDEF UsePixelMap}
  Bmp := TGuiPixelMapDIB.Create;
  {$ELSE}
  Bmp := TBitmap.Create;
  {$ENDIF}
  with Bmp do
   try
    {$IFDEF UsePixelMap}
    SetSize(Self.Width, Self.Height);

    // Copy parent control image
    SaveIndex := SaveDC(Canvas.Handle);
    try
     SetViewportOrgEx(Canvas.Handle, Pnt.X, Pnt.Y, nil);
     IntersectClipRect(Canvas.Handle, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight);
     with TParentControl(Control.Parent) do
      begin
       Perform(WM_ERASEBKGND, Canvas.Handle, 0);
       PaintWindow(Canvas.Handle);
      end;
    finally
     RestoreDC(Canvas.Handle, SaveIndex);
    end;

    MakeOpaque;

    {$ELSE}

    Width := Self.Width;
    Height := Self.Height;
    PixelFormat := pf32bit;

    // Copy parent control image
    SaveIndex := SaveDC(Canvas.Handle);
    try
     SetViewportOrgEx(Canvas.Handle, Pnt.X, Pnt.Y, nil);
     IntersectClipRect(Canvas.Handle, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight);
     with TParentControl(Control.Parent) do
      begin
       Perform(WM_ERASEBKGND, Canvas.Handle, 0);
       PaintWindow(Canvas.Handle);
      end;
    finally
     RestoreDC(Canvas.Handle, SaveIndex);
    end;
    {$ENDIF}

    // Copy images of graphic controls
    for I := 0 to SubCount - 1 do
     begin
      if Control.Parent.Controls[I] = Control then Break else
       if (Control.Parent.Controls[I] <> nil) and
          (Control.Parent.Controls[I] is TGraphicControl)
        then
         with TGraphicControl(Control.Parent.Controls[I]) do
          begin
           CtlR := Bounds(Left, Top, Width, Height);
           if Boolean(IntersectRect(R, SelfR, CtlR)) and Visible then
            begin
             {$IFDEF WIN32}
             ControlState := ControlState + [csPaintCopy];
             {$ENDIF}
             SaveIndex := SaveDC(Canvas.Handle);
             try
              SaveIndex := SaveDC(Canvas.Handle);
              SetViewportOrgEx(Canvas.Handle, Left + Pnt.X, Top + Pnt.Y, nil);
              IntersectClipRect(Canvas.Handle, 0, 0, Width, Height);
              Perform(WM_PAINT, Canvas.Handle, 0);
             finally
              RestoreDC(Handle, SaveIndex);
              {$IFDEF WIN32}
              ControlState := ControlState - [csPaintCopy];
              {$ENDIF}
             end;
            end;
          end;
     end;

    Self.Draw(Bmp);
   finally
    Free;
   end;

 {$IFDEF WIN32}
 finally
   with Control.Parent do Control.ControlState := Control.ControlState - [csPaintCopy];
 end;
 {$ENDIF}
{$ENDIF}
end;

procedure TGuiCustomPixelMap.Clear;
begin
 FillChar(FDataPointer^, FDataSize, 0);
end;

procedure TGuiCustomPixelMap.Clear(Color: TPixel32);
var
  Index : Integer;
begin
 for Index := 0 to FWidth * FHeight - 1
  do FDataPointer^[Index] := Color;
end;

procedure TGuiCustomPixelMap.Clear(Color: TColor);
begin
 Clear(ConvertColor(Color));
end;

function TGuiCustomPixelMap.GetDataPointer: PPixel32Array;
begin
 Result := FDataPointer;
end;

function TGuiCustomPixelMap.GetPixel(X, Y: Integer): TPixel32;
begin
 Assert((Y >= 0) and (Y < Height));
 Assert((X >= 0) and (X < Width));
 Result := FDataPointer^[Y * Width + X];
end;

function TGuiCustomPixelMap.GetPixelPointer(X, Y: Integer): PPixel32;
begin
 Assert((Y >= 0) and (Y < Height));
 Assert((X >= 0) and (X < Width));
 Result := @FDataPointer^[Y * Width + X];
end;

function TGuiCustomPixelMap.GetScanLine(Y: Integer): PPixel32Array;
begin
 Assert((Y >= 0) and (Y < Height));
 Result := @FDataPointer^[Y * Width];
end;

procedure TGuiCustomPixelMap.SetPixel(X, Y: Integer; const Value: TPixel32);
begin
 Assert((Y >= 0) and (Y < Height));
 Assert((X >= 0) and (X < Width));
 BlendPixelInplace(Value, FDataPointer[Y * Width + X]);
end;

procedure TGuiCustomPixelMap.HeightChanged(UpdateBitmap: Boolean = True);
begin
 FBitmapInfo.bmiHeader.biHeight := -FHeight;
 if UpdateBitmap then Resized;
end;

procedure TGuiCustomPixelMap.WidthChanged(UpdateBitmap: Boolean = True);
begin
 FBitmapInfo.bmiHeader.biWidth := FWidth;
 if UpdateBitmap then Resized;
end;

procedure TGuiCustomPixelMap.LoadFromStream(Stream: TStream);
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

procedure TGuiCustomPixelMap.MakeOpaque;
begin
 ResetAlpha($FF);
end;

procedure TGuiCustomPixelMap.SaveToStream(Stream: TStream);
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

function TGuiCustomPixelMap.Equal(PixelMap: TGuiCustomPixelMap): Boolean;
begin
 Result := (PixelMap.Width = FWidth) and (PixelMap.Height = FHeight);

 if Result
  then Result := CompareMem(FDataPointer, PixelMap.FDataPointer, FDataSize);
end;

function TGuiCustomPixelMap.Equals(Obj: TObject): Boolean;
begin
 {$IFDEF DELPHI14_UP}
 Result := inherited Equals(Obj);
 {$ELSE}
 Result := False;
 {$ENDIF}
 if Obj is TGuiCustomPixelMap
  then Result := Equal(TGuiCustomPixelMap(Obj));
end;

procedure TGuiCustomPixelMap.ReadData(Stream: TStream);
var
  TempWidth, TempHeight: Integer;
begin
 with Stream do
  try
   ReadBuffer(TempWidth, 4);
   ReadBuffer(TempHeight, 4);
   SetSize(TempWidth, TempHeight);
   Assert(FDataSize = FWidth * FHeight * SizeOf(TPixel32));
   ReadBuffer(FDataPointer^, FDataSize);
  finally
   Changed;
  end;
end;

procedure TGuiCustomPixelMap.WriteData(Stream: TStream);
begin
 with Stream do
  begin
   WriteBuffer(FWidth, 4);
   WriteBuffer(FHeight, 4);
   Assert(FDataSize = FWidth * FHeight * SizeOf(TPixel32));
   WriteBuffer(FDataPointer^, FDataSize);
  end;
end;

procedure TGuiCustomPixelMap.DefineProperties(Filer: TFiler);
var
  HasData : Boolean;
begin
 HasData := (FDataSize > 0);
 if HasData and (Filer.Ancestor <> nil)
  then HasData := not ((Filer.Ancestor is TGuiCustomPixelMap) and
    Equal(TGuiCustomPixelMap(Filer.Ancestor)));

 Filer.DefineBinaryProperty('Data', ReadData, WriteData, HasData);
end;

procedure TGuiCustomPixelMap.FillRect(Rect: TRect; Color: TPixel32);
var
  X, Y : Integer;
begin
 if Color.A = $FF then
  for Y := Rect.Top to Rect.Bottom - 1 do
   for X := Rect.Left to Rect.Right - 1
    do FDataPointer[Y * Width + X] := Color
 else
  try
   for Y := Rect.Top to Rect.Bottom - 1
    do BlendPixelLine(Color, @FDataPointer[Y * Width + Rect.Left], Rect.Right - Rect.Left);
  finally
   EMMS;
  end;
end;

procedure TGuiCustomPixelMap.FillRect(Left, Top, Right, Bottom: Integer; Color: TPixel32);
var
  X, Y : Integer;
begin
 if Color.A = $FF then
  for Y := Top to Bottom - 1 do
   for X := Left to Right - 1
    do FDataPointer[Y * Width + X] := Color
 else
  try
   for Y := Top to Bottom - 1
    do BlendPixelLine(Color, @FDataPointer[Y * Width + Left], Right - Left);
  finally
   EMMS;
  end;
end;

procedure TGuiCustomPixelMap.FrameRect(Rect: TRect; Color: TPixel32);
begin
 // top & bottom
 HorizontalLine(Rect.Left, Rect.Right, Rect.Top, Color);
 HorizontalLine(Rect.Left, Rect.Right, Rect.Bottom - 1, Color);

 // left & right
 VerticalLine(Rect.Left, Rect.Top + 1, Rect.Bottom - 1, Color);
 VerticalLine(Rect.Right - 1, Rect.Top + 1, Rect.Bottom - 1, Color);
end;

procedure TGuiCustomPixelMap.VerticalLine(X, FromY, ToY: Integer; Color: TPixel32);
var
  Y : Integer;
begin
 try
  if ToY < FromY  then
   for Y := ToY to FromY - 1
    do BlendPixelInplace(Color, FDataPointer[Y * Width + X])
  else
   for Y := FromY to ToY - 1
    do BlendPixelInplace(Color, FDataPointer[Y * Width + X]);
 finally
  EMMS;
 end;
end;

procedure TGuiCustomPixelMap.HorizontalLine(FromX, ToX, Y: Integer;
  Color: TPixel32);
begin
 try
  if FromX > ToX
   then Exchange32(FromX, ToX);
  BlendPixelLine(Color, @FDataPointer[Y * Width + FromX], ToX - FromX);
 finally
  EMMS;
 end;
end;

procedure TGuiCustomPixelMap.HorizontalLine(FromX, ToX, Y: Integer; FromColor,
  ToColor: TPixel32);
var
  X     : Integer;
  R, S  : Single;
  Color : TPixel32;
begin
 if FromX > ToX
  then Exchange32(FromX, ToX);
 S := 1 / (ToX - FromX);
 R := 0;
 for X := FromX to ToX - 1 do
  begin
   Color := CombinePixel(ToColor, FromColor, Round(R * $FF));
   BlendPixelInplace(Color, FDataPointer[Y * Width + X]);
   R := R + S;
   EMMS;
  end;
end;

procedure TGuiCustomPixelMap.Line(FromX, FromY, ToX, ToY: Integer; Color: TPixel32);
var
  x, y, t     : Integer;
  dx, dy      : Integer;
  incx, incy  : Integer;
  pdx, pdy    : Integer;
  ddx, ddy    : Integer;
  es, el, err : Integer;
begin
 if FromY = ToY then HorizontalLine(FromX, ToX, FromY, Color) else
 if FromX = ToX then VerticalLine(FromX, FromY, ToY, Color) else
  try
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
   BlendPixelInplace(Color, FDataPointer[Y * Width + X]);

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
     BlendPixelInplace(Color, FDataPointer[Y * Width + X]);
    end;
  finally
   EMMS;
  end;
end;

procedure TGuiCustomPixelMap.ResetAlpha(Value: Byte = 0);
var
  Index : Integer;
begin
 for Index := 0 to FWidth * FHeight - 1
  do FDataPointer^[Index].A := Value;
end;


{ TGuiPixelMapMemory }

destructor TGuiPixelMapMemory.Destroy;
begin
 FreeAlignedMemory(FDataPointer);
 inherited;
end;

procedure TGuiPixelMapMemory.Draw(Bitmap: TBitmap; X, Y: Integer);
var
  IndexX : Integer;
  IndexY : Integer;
  Data24 : PRGB24Array;
begin
 if (Bitmap.Height <> 0) and (FDataPointer <> nil) then
  begin
   {$IFNDEF FPC}
   case Bitmap.PixelFormat of
    pf32bit :
      for IndexY := 0 to Bitmap.Height - 1 do
       begin
        Move(Bitmap.ScanLine[IndexY]^, FDataPointer^[X + (Y + IndexY) * Width],
          Bitmap.Width * SizeOf(TPixel32));
       end;

    pf24bit :
      for IndexY := 0 to Bitmap.Height - 1 do
       begin
        Data24 := Bitmap.ScanLine[IndexY];

        for IndexX := 0 to Bitmap.Width - 1 do
         begin
          FDataPointer^[X + IndexX + (Y + IndexY) * Width].R := Data24^[IndexX].R;
          FDataPointer^[X + IndexX + (Y + IndexY) * Width].G := Data24^[IndexX].G;
          FDataPointer^[X + IndexX + (Y + IndexY) * Width].B := Data24^[IndexX].B;
          FDataPointer^[X + IndexX + (Y + IndexY) * Width].A := $FF;
         end;
       end;
    else
    {$ELSE}
   begin
    {$ENDIF}
     if GetDIBits(Bitmap.Canvas.Handle, Bitmap.Handle, 0, Bitmap.Height,
       @FDataPointer^[X + Y * Width], FBitmapInfo, DIB_RGB_COLORS) = 0
      then raise Exception.Create('Error');
   end;
  end;
(*
var
  CompDC     : HDC;
  CompBitmap : HBITMAP;
begin
 CompDC := CreateCompatibleDC(Bitmap.Canvas.Handle);
 try
  CompBitmap := CreateCompatibleBitmap(CompDC, Width, Height);
  SelectObject(CompDC, CompBitmap);
  if CompBitmap <> 0 then
   try
    BitBlt(CompDC, 0, 0, Width, Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);

    if GetDIBits(Bitmap.Canvas.Handle, CompBitmap, 0, Height, FDataPointer,
      FBitmapInfo, DIB_RGB_COLORS) = 0
      then raise Exception.Create('Error');
   finally
    DeleteObject(CompBitmap);
   end;
 finally
  DeleteDC(CompDC);
 end;
*)
end;

{$IFDEF DARWIN}
procedure TGuiPixelMapMemory.PaintTo(Canvas: TCanvas; X, Y: Integer);
(*
var
  pm                : PixMapHandle;
  port              : CGrafPtr;
  src_rect          : Carbon.Rect;
  dest_rect         : Carbon.Rect;
  image_description : ImageDescriptionHandle;
*)
begin
(*
  HIViewDrawCGImage(
    TCarbonDeviceContext(Canvas.Handle).CGContext,
    GetCGRect(0, 0, FWidth, FHeight),
    CGBitmapContextCreateImage(FContext));
*)

(*
procedure pixel_map.draw(window : WindowRef; device_rect : RectPtr = NIL; bmp_rect : RectPtr = NIL );
begin
 if (m_pmap = NIL ) or (m_buf = NIL )
  then exit;

 pm := GetGWorldPixMap(GrafPtr(m_pmap));
 port := GetWindowPort(window);

 // Again, I used the Quicktime version.
 // Good old 'CopyBits' does better interpolation when scaling
 // but does not support all pixel depths.
 SetRect(dest_rect, 0, 0, _width, _height);

 MakeImageDescriptionForPixMap(ImageCompression.PixMapHandle(pm),
   image_description);

 if image_description <> NIL then
  begin
   SetRect(src_rect, 0, 0, image_description^.width, image_description^.height);

   DecompressImage(
     GetPixBaseAddr(pm),
     image_description,
     ImageCompression.PixMapHandle(GetPortPixMap(port)),
     ImageCompression.Rect(src_rect),
     ImageCompression.Rect(dest_rect),
     ditherCopy, NIL);
   DisposeHandle(Handle(image_description ) );
  end;
end;
*)
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure TGuiPixelMapMemory.PaintTo(Canvas: TCanvas; X, Y: Integer);
var
  Bitmap        : HBITMAP;
  DeviceContext : HDC;
  Buffer        : Pointer;
  OldObject     : HGDIOBJ;
begin
 if SetDIBitsToDevice(Canvas.Handle, X, Y, Width, Height, 0, 0, 0, Height,
   FDataPointer, FBitmapInfo, DIB_RGB_COLORS) = 0 then
  begin
   // create compatible device context
   DeviceContext := CreateCompatibleDC(Canvas.Handle);
   if DeviceContext <> 0 then
    try
     Bitmap := CreateDIBSection(DeviceContext, FBitmapInfo, DIB_RGB_COLORS,
       Buffer, 0, 0);

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
end;
{$ENDIF}

procedure TGuiPixelMapMemory.PaintTo(Canvas: TCanvas; Rect: TRect;
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
   FDataPointer, FBitmapInfo, DIB_RGB_COLORS) = 0 then
  begin
   // create compatible device context
   DeviceContext := CreateCompatibleDC(Canvas.Handle);
   if DeviceContext <> 0 then
    try
     Bitmap := CreateDIBSection(DeviceContext, FBitmapInfo,
       DIB_RGB_COLORS, Buffer, 0, 0);

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

procedure TGuiPixelMapMemory.HeightChanged(UpdateBitmap: Boolean);
begin
 inherited;
 if UpdateBitmap then AllocateDataPointer;
end;

procedure TGuiPixelMapMemory.WidthChanged(UpdateBitmap: Boolean);
begin
 inherited;
 if UpdateBitmap then AllocateDataPointer;
end;

procedure TGuiPixelMapMemory.SizeChangedAtOnce;
begin
 inherited;
 AllocateDataPointer;
end;

procedure TGuiPixelMapMemory.Resize(Width, Height: Integer);
var
  NewSize  : Integer;
  NewData  : PPixel32Array;
  TempData : PPixel32Array;
  Y, Wdth  : Integer;
begin
 inherited;
 if (Width <> FWidth) or (Height <> FHeight) then
  begin
   NewSize := Width * Height * SizeOf(TPixel32);
   GetAlignedMemory(Pointer(NewData), NewSize);

   Wdth := Min(Width, FWidth);
   for Y := 0 to Min(Height, FHeight) - 1 do
     Move(FDataPointer^[Y * FWidth], NewData^[Y * Width], Wdth * SizeOf(TPixel32));

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

procedure TGuiPixelMapMemory.Turn(CounterClockwise: Boolean);
var
  TurnData : PPixel32Array;
  TempData : PPixel32Array;
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
 with FBitmapInfo do
  begin
   bmiHeader.biWidth := FHeight;
   bmiHeader.biHeight := -FWidth;
   FWidth := bmiHeader.biWidth;
   FHeight := -bmiHeader.biHeight;
  end;

 // exchange data pointer
 TempData := FDataPointer;
 FDataPointer := TurnData;

 // dispose old data pointer
 FreeAlignedMemory(TempData);
end;

procedure TGuiPixelMapMemory.AllocateDataPointer;
var
  NewDataSize : Integer;
begin
 NewDataSize := FWidth * FHeight * SizeOf(TPixel32);
 if FDataSize <> NewDataSize then
  begin
   FDataSize := NewDataSize;
   Assert(FDataSize >= 0);
   ReallocateAlignedMemory(Pointer(FDataPointer), FDataSize);
   Clear;
  end;
end;

{$IFDEF MSWindows}
{ TBitmapCanvas }

type
  TGuiPixelMapCanvas = class(TCanvas)
  private
    FPixelMap  : TGuiPixelMapDIB;
    FOldBitmap : HBITMAP;
    procedure FreeContext;
  protected
    procedure CreateHandle; override;
  public
    constructor Create(PixelMap: TGuiPixelMapDIB);
    destructor Destroy; override;
  end;

{ TGuiPixelMapCanvas }

constructor TGuiPixelMapCanvas.Create(PixelMap: TGuiPixelMapDIB);
begin
 inherited Create;
 FPixelMap := PixelMap;
end;

destructor TGuiPixelMapCanvas.Destroy;
begin
 FreeContext;
 inherited Destroy;
end;

procedure TGuiPixelMapCanvas.FreeContext;
var
  H : HBITMAP;
begin
 if Handle <> 0 then
  begin
   if FOldBitmap <> 0
    then SelectObject(Handle, FOldBitmap);
   H := Handle;
   Handle := 0;
   DeleteDC(H);
  end;
end;

procedure TGuiPixelMapCanvas.CreateHandle;
var
  H: HBITMAP;
begin
 if FPixelMap <> nil then
  begin
   H := CreateCompatibleDC(0);
   if FPixelMap.Handle <> 0
    then FOldBitmap := SelectObject(H, FPixelMap.Handle)
    else FOldBitmap := 0;
   Handle := H;
  end;
end;
{$ENDIF}


{ TGuiPixelMapDIB }

constructor TGuiPixelMapDIB.Create;
{$IFDEF Darwin}
var
  ProfileLocation : CMProfileLocation;
  Status          : OSStatus;
{$ENDIF}
begin
 inherited;
 {$IFDEF Darwin}

 // Creates a generic color profile
 ProfileLocation.locType := cmPathBasedProfile;
 ProfileLocation.u.pathLoc.path := CStrGenericRGBProfilePath;

 Status := CMOpenProfile(FProfile, ProfileLocation);

 if Status <> noErr
  then raise Exception.Create(RCStrCouldntCreateGenericProfile);

 // Creates a generic color space
 FColorSpace := CGColorSpaceCreateWithPlatformColorSpace(FProfile);

 if FColorSpace = nil
  then raise Exception.Create(RCStrCouldntCreateGenericColorSpace);
 {$ENDIF}
end;

destructor TGuiPixelMapDIB.Destroy;
begin
 {$IFDEF Darwin}
 // Closes the profile
 CMCloseProfile(FProfile);
 {$ENDIF}

 DisposeDeviceIndependentBitmap;
 inherited;
end;

procedure TGuiPixelMapDIB.AllocateDeviceIndependentBitmap;
var
  NewDataSize : Integer;
begin
 NewDataSize := FWidth * FHeight * SizeOf(Cardinal);
 if FDataSize <> NewDataSize then
  begin
   FDataSize := NewDataSize;
   Assert(FDataSize >= 0);

   {$IFDEF MSWINDOWS}
   FBitmapHandle := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS,
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
   {$ENDIF}

   {$IFDEF Darwin}
   // allocate memory
   FDataPointer := GetMem(FDataSize);

   // check if data has been allocated correctly
   if FDataPointer = nil
    then raise Exception.Create(RCStrNoDibHandle);

   // Creates a device context for our raw image area
   FContext := CGBitmapContextCreate(FDataPointer,
     FWidth, FHeight, 8, 4 * FWidth, FColorSpace,
     kCGImageAlphaNoneSkipFirst or kCGBitmapByteOrder32Little);

   if FContext = nil
    then raise Exception.Create('Context not initialized');

   // flip and offset CTM to upper left corner
   CGContextTranslateCTM(FContext, 0, FHeight);
   CGContextScaleCTM(FContext, 1, -1);
   {$ENDIF}

   {$IFDEF GTK}
   // allocate memory
   FDataPointer := GetMem(FDataSize);

   // check if data has been allocated correctly
   if FDataPointer = nil
    then raise Exception.Create(RCStrNoDibHandle);

   // We didn't pass a memory freeing function, so we will have to take care
   // of that ourselves
   FPixbuf := gdk_pixbuf_new_from_data(PguChar(FBits), GDK_COLORSPACE_RGB,
     True, 8, FWidth, FHeight, 4 * FWidth, nil, nil);

   if FPixbuf = nil then
     raise Exception.Create('Can''t allocate the Pixbuf');
   {$ENDIF}
  end;
end;

procedure TGuiPixelMapDIB.DisposeDeviceIndependentBitmap;
begin
 {$IFDEF MSWINDOWS}
 if FDC <> 0 then DeleteDC(FDC);
 FDC := 0;
 if FBitmapHandle <> 0
  then DeleteObject(FBitmapHandle);
 FBitmapHandle := 0;
 {$ENDIF}

 {$IFDEF Darwin}
 if Assigned(FContext)
  then CGContextRelease(FContext);

 if Assigned(FDataPointer)
  then FreeAndNil(FDataPointer);
 {$ENDIF}

 {$IFDEF GTK}
 if Assigned(FDataPointer)
  then FreeAndNil(FDataPointer);
 {$IFDEF LCLGtk2}
 if Assigned(FPixbuf) then g_object_unref(FPixbuf);
 {$ELSE}
 if Assigned(FPixbuf) then gdk_pixbuf_unref(FPixbuf);
 {$ENDIF}
 FPixbuf := nil;
 {$ENDIF}

 FDataPointer := nil;
end;

{$IFDEF Darwin}
function GetCarbonRect(Left, Top, Width, Height: Integer): MacOSAll.Rect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

function GetCGRect(Left, Top, Width, Height: Integer): MacOSAll.CGRect; overload;
begin
  Result.Origin.X := Left;
  Result.Origin.Y := Top;
  Result.Size.Width := Width;
  Result.Size.Height := Height;
end;

function GetCGRect(SrcRect: TRect): MacOSAll.CGRect; overload;
begin
  Result.Origin.X := SrcRect.Left;
  Result.Origin.Y := SrcRect.Top;
  Result.Size.Width := SrcRect.Right - SrcRect.Left;
  Result.Size.Height := SrcRect.Bottom - SrcRect.Top;
end;
{$ENDIF}

procedure TGuiPixelMapDIB.CanvasChanged(Sender: TObject);
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGuiPixelMapDIB.Draw(Bitmap: TBitmap; X, Y: Integer);
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

function TGuiPixelMapDIB.GetCanvas: TCanvas;
begin
 {$IFDEF MSWindows}
 if FCanvas = nil then
  begin
   FCanvas := TGuiPixelMapCanvas.Create(Self);
   FCanvas.OnChange := CanvasChanged;
  end;
 {$ENDIF}

 {$IFDEF Darwin}
 if FCanvas = nil then
  begin
   FCanvas := TCanvas.Create;

   FCanvasHandle := TCarbonDeviceContext.Create;
   FCanvasHandle.CGContext := FContext;

   FCanvas.Handle := HDC(FCanvasHandle);
   //FCanvas.OnChange := FOnCanvasChange;
  end;
 {$ENDIF}

 Result := FCanvas;
end;

procedure TGuiPixelMapDIB.PaintTo(Canvas: TCanvas; X, Y: Integer);
{$IFDEF DARWIN}
var
  Original, Subsection : CGImageRef;
  CGDstRect, CGSrcRect : CGRect;
  ExternalContext      : CGContextRef;
{$ENDIF}
begin
 {$IFDEF MSWINDOWS}
 BitBlt(Canvas.Handle, X, Y, Width, Height, FDC, X, Y, SRCCOPY);
 {$ENDIF}

 {$IFDEF Darwin}
 // Gets the external context
 if (Canvas.Handle = 0) then Exit;
 ExternalContext := TCarbonDeviceContext(Canvas.Handle).CGContext;

 // Converts the rectangles to CoreGraphics rectangles
 CGDstRect := GetCGRect(Rect(X, Y, X + Width, Y + Height));
 CGSrcRect := GetCGRect(Rect(0, 0, Width, Height));

 // Gets an image handle that represents the subsection
 Original := CGBitmapContextCreateImage(FContext);
 Subsection := CGImageCreateWithImageInRect(Original, CGSrcRect);
 CGImageRelease(Original);

 // We need to make adjustments to the CTM so the painting is done correctly
 CGContextSaveGState(ExternalContext);
 try
  CGContextTranslateCTM(ExternalContext, 0, FHeight);
  CGContextScaleCTM(ExternalContext, 1, -1);
  CGContextTranslateCTM(ExternalContext, 0, -CGDstRect.origin.y);
  CGDstRect.origin.y := 0;

  // Draw the subsection
  CGContextDrawImage(ExternalContext, CGDstRect, Subsection);
 finally
  // reset the CTM to the old values
  CGContextRestoreGState(ExternalContext);
 end;

 // Release the subsection
 CGImageRelease(Subsection);
 {$ENDIF}
end;

procedure TGuiPixelMapDIB.PaintTo(Canvas: TCanvas; LRect: TRect; X: Integer = 0;
  Y: Integer = 0);
{$IFDEF DARWIN}
var
  Original, Subsection : CGImageRef;
  CGDstRect, CGSrcRect : CGRect;
  ExternalContext      : CGContextRef;
{$ENDIF}
begin
 {$IFDEF MSWINDOWS}
 BitBlt(Canvas.Handle, X, Y, Min(Width, LRect.Right - LRect.Left),
   Min(Height, LRect.Bottom - LRect.Top), FDC, LRect.Left, LRect.Top, SRCCOPY);
 {$ENDIF}

 {$IFDEF Darwin}
 // Gets the external context
 if (Canvas.Handle = 0) then Exit;
 ExternalContext := TCarbonDeviceContext(Canvas.Handle).CGContext;

 // Converts the rectangles to CoreGraphics rectangles
 CGDstRect := GetCGRect(Rect(X, Y, X + Width, Y + Height));
 CGSrcRect := GetCGRect(Rect(0, 0, Width, Height));

 // Gets an image handle that represents the subsection
 Original := CGBitmapContextCreateImage(FContext);
 Subsection := CGImageCreateWithImageInRect(Original, CGSrcRect);
 CGImageRelease(Original);

 // We need to make adjustments to the CTM so the painting is done correctly
 CGContextSaveGState(ExternalContext);
 try
  CGContextTranslateCTM(ExternalContext, 0, FHeight);
  CGContextScaleCTM(ExternalContext, 1, -1);
  CGContextTranslateCTM(ExternalContext, 0, -CGDstRect.origin.y);
  CGDstRect.origin.y := 0;

  // Draw the subsection
  CGContextDrawImage(ExternalContext, CGDstRect, Subsection);
 finally
  // reset the CTM to the old values
  CGContextRestoreGState(ExternalContext);
 end;

 // Release the subsection
 CGImageRelease(Subsection);
 {$ENDIF}
end;

procedure TGuiPixelMapDIB.SizeChangedAtOnce;
begin
 inherited;
 DisposeDeviceIndependentBitmap;
 if Width * Height <> 0
  then AllocateDeviceIndependentBitmap;
end;

procedure TGuiPixelMapDIB.Resize(Width, Height: Integer);
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

procedure TGuiPixelMapDIB.Turn(CounterClockwise: Boolean);
var
  TurnData : PPixel32Array;
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
  with FBitmapInfo do
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

procedure TGuiPixelMapDIB.HeightChanged(UpdateBitmap: Boolean);
begin
 inherited;
 if UpdateBitmap then
  begin
   DisposeDeviceIndependentBitmap;
   if Width * Height <> 0
    then AllocateDeviceIndependentBitmap;
  end;
end;

procedure TGuiPixelMapDIB.WidthChanged(UpdateBitmap: Boolean);
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
