unit DAV_GuiPngCoder;

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

{$IFDEF CPUx86_64}
  {$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Classes, Graphics, SysUtils, DAV_Classes, DAV_GuiCommon, DAV_GuiPngChunks,
  DAV_GuiPngTypes;

type
  TCustomPngCoder = class
  protected
    FStream       : TStream;
    FHeader       : TChunkPngImageHeader;
    FGamma        : TChunkPngGamma;
    FPalette      : TChunkPngPalette;

    FRowBuffer    : array [0..1] of PByteArray;
    FMappingTable : PByteArray;

    procedure BuildMappingTable; virtual;
  public
    constructor Create(Stream: TStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil); virtual;
    destructor Destroy; override;
  end;

  TScanLineCallback = function(Bitmap: TObject; Y: Integer): Pointer of object;

  TCustomPngDecoder = class(TCustomPngCoder)
  protected
    procedure FilterSub(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure FilterUp(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure FilterAverage(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure FilterPaeth(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);

    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
  public
    procedure DecodeToScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); virtual; abstract;
  end;
  TCustomPngDecoderClass = class of TCustomPngDecoder;

  TCustomPngEncoder = class(TCustomPngCoder)
  protected
    procedure FilterSub(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure FilterUp(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure FilterAverage(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure FilterPaeth(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);

    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer);
  public
    procedure EncodeFromScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); virtual; abstract;
  end;
  TCustomPngEncoderClass = class of TCustomPngEncoder;

  TCustomPngNonInterlacedDecoder = class(TCustomPngDecoder)
  protected
    FBytesPerRow : Integer;
    FRowByteSize : Integer;
    procedure TransferData(Source: Pointer; Destination: Pointer); virtual; abstract;
  public
    constructor Create(Stream: TStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil); override;
    destructor Destroy; override;
    procedure DecodeToScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); override;
  end;

  TPngGrayscale1bitRGBADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngGrayscale2bitRGBADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngGrayscale4bitRGBADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngGrayscale8bitRGBADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngGrayscale16bitRGBADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngTrueColor8bitRGBADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngTrueColor16bitRGBADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngPaletteRGBADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngPalette8bitRGBADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngGrayscaleAlpha8bitRGBADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngGrayscaleAlpha16bitRGBADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngTrueColorAlpha8bitRGBADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngTrueColorAlpha16bitRGBADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TCustomPngAdam7Decoder = class(TCustomPngDecoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); virtual; abstract;
  public
    constructor Create(Stream: TStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil); override;
    destructor Destroy; override;
    procedure DecodeToScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); override;
  end;

  TPngAdam7Grayscale1bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7Grayscale2bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7Grayscale4bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7Grayscale8bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7Grayscale16bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7TrueColor8bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7TrueColor16bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7Palette1bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7Palette2bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7Palette4bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7Palette8bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7GrayscaleAlpha8bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7GrayscaleAlpha16bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7TrueColorAlpha8bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7TrueColorAlpha16bitRGBADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngGrayscale1bitBGRADecoder = TPngGrayscale1bitRGBADecoder;
  TPngGrayscale2bitBGRADecoder = TPngGrayscale2bitRGBADecoder;
  TPngGrayscale4bitBGRADecoder = TPngGrayscale4bitRGBADecoder;
  TPngGrayscale8bitBGRADecoder = TPngGrayscale8bitRGBADecoder;
  TPngGrayscale16bitBGRADecoder = TPngGrayscale16bitRGBADecoder;

  TPngTrueColor8bitBGRADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngTrueColor16bitBGRADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngPaletteBGRADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngPalette8bitBGRADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngGrayscaleAlpha8bitBGRADecoder = TPngGrayscaleAlpha8bitRGBADecoder;
  TPngGrayscaleAlpha16bitBGRADecoder = TPngGrayscaleAlpha16bitRGBADecoder;

  TPngTrueColorAlpha8bitBGRADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngTrueColorAlpha16bitBGRADecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7Grayscale1bitBGRADecoder = TPngAdam7Grayscale1bitRGBADecoder;
  TPngAdam7Grayscale2bitBGRADecoder = TPngAdam7Grayscale2bitRGBADecoder;
  TPngAdam7Grayscale4bitBGRADecoder = TPngAdam7Grayscale4bitRGBADecoder;
  TPngAdam7Grayscale8bitBGRADecoder = TPngAdam7Grayscale8bitRGBADecoder;
  TPngAdam7Grayscale16bitBGRADecoder = TPngAdam7Grayscale16bitRGBADecoder;

  TPngAdam7TrueColor8bitBGRADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7TrueColor16bitBGRADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7Palette1bitBGRADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7Palette2bitBGRADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7Palette4bitBGRADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7Palette8bitBGRADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7GrayscaleAlpha8bitBGRADecoder = TPngAdam7GrayscaleAlpha8bitRGBADecoder;
  TPngAdam7GrayscaleAlpha16bitBGRADecoder = TPngAdam7GrayscaleAlpha16bitRGBADecoder;

  TPngAdam7TrueColorAlpha8bitBGRADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TPngAdam7TrueColorAlpha16bitBGRADecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: Pointer); override;
  end;

  TCustomPngNonInterlacedEncoder = class(TCustomPngEncoder)
  protected
    FBytesPerRow : Integer;
    FRowByteSize : Integer;
    function ColorInPalette(Color: TBGR32): Integer; virtual;
    procedure TransferData(Source: Pointer; Destination: Pointer); virtual; abstract;
  public
    constructor Create(Stream: TStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil); override;
    destructor Destroy; override;
    procedure EncodeFromScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); override;
  end;

  TPngRGBAGrayscale1bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngRGBAGrayscale2bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngRGBAGrayscale4bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngRGBAGrayscale8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngRGBATrueColor8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngRGBAPalette1bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngRGBAPalette2bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngRGBAPalette4bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngRGBAPalette8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngRGBAGrayscaleAlpha8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngRGBATrueColorAlpha8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngBGRAGrayscale1bitEncoder = TPngRGBAGrayscale1bitEncoder;
  TPngBGRAGrayscale2bitEncoder = TPngRGBAGrayscale2bitEncoder;
  TPngBGRAGrayscale4bitEncoder = TPngRGBAGrayscale4bitEncoder;
  TPngBGRAGrayscale8bitEncoder = TPngRGBAGrayscale8bitEncoder;

  TPngBGRATrueColor8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngBGRAPalette1bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngBGRAPalette2bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngBGRAPalette4bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngBGRAPalette8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

  TPngBGRAGrayscaleAlpha8bitEncoder = TPngRGBAGrayscaleAlpha8bitEncoder;

  TPngBGRATrueColorAlpha8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: Pointer; Destination: Pointer); override;
  end;

implementation

uses
  Math, DAV_GuiPngResourceStrings;

const
  CRowStart        : array[0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  CColumnStart     : array[0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  CRowIncrement    : array[0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  CColumnIncrement : array[0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);
  CGrayScaleTable1Bit : array [0..1] of Byte = (0, $FF);
  CGrayScaleTable2Bit : array [0..3] of Byte = (0, $55, $AA, $FF);
  CGrayScaleTable4Bit : array [0..15] of Byte = (0, $11, $22, $33, $44, $55,
    $66, $77, $88, $99, $AA, $BB, $CC, $DD, $EE, $FF);


{ TCustomPngCoder }

constructor TCustomPngCoder.Create(Stream: TStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma = nil;
  Palette: TChunkPngPalette = nil);
begin
 FStream       := Stream;
 FHeader       := Header;
 FGamma        := Gamma;
 FPalette      := Palette;
 FMappingTable := nil;
 BuildMappingTable;
 inherited Create;
end;

destructor TCustomPngCoder.Destroy;
begin
 Dispose(FMappingTable);
 inherited;
end;

procedure TCustomPngCoder.BuildMappingTable;
var
  Index        : Integer;
  Palette      : PRGB24Array;
  FracVal      : Single;
  Color        : TRGB24;
  MaxByte      : Byte;
  PreCalcGamma : Extended;
const
  COne255th : Extended = 1 / 255;
begin
 if FHeader.HasPalette then
  begin
   if Assigned(FPalette) then
    begin
     GetMem(FMappingTable, FPalette.Count * SizeOf(TRGB24));
     Palette := PRGB24Array(FMappingTable);

     if Assigned(FGamma) then
      begin
       PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
       for Index := 0 to FPalette.Count - 1 do
        begin
         Color := FPalette.PaletteEntry[Index];
         Palette[Index].R := Round(Power((Color.R * COne255th), PreCalcGamma) * 255);
         Palette[Index].G := Round(Power((Color.G * COne255th), PreCalcGamma) * 255);
         Palette[Index].B := Round(Power((Color.B * COne255th), PreCalcGamma) * 255);
        end;
      end
     else
      for Index := 0 to FPalette.Count - 1
       do Palette[Index] := FPalette.PaletteEntry[Index];
    end
   else
    begin
     // create gray scale palette
     GetMem(FMappingTable, 256 * SizeOf(TRGB24));
     Palette := PRGB24Array(FMappingTable);
     MaxByte := ((1 shl FHeader.BitDepth) - 1) and $FF;
     FracVal := 1 / MaxByte;

     if Assigned(FGamma) then
      begin
       PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
       for Index := 0 to FPalette.Count - 1 do
        begin
         Palette[Index].R := Round(Power(Index * FracVal, PreCalcGamma) * 255);
         Palette[Index].G := Palette[Index].R;
         Palette[Index].B := Palette[Index].B;
        end;
      end
     else
      begin
       for Index := 0 to MaxByte do
        begin
         Palette[Index].R := Round(255 * (Index * FracVal));
         Palette[Index].G := Palette[Index].R;
         Palette[Index].B := Palette[Index].R;
        end;
      end;
    end;
  end
 else
  begin
   GetMem(FMappingTable, 256);
   if Assigned(FGamma) and (FGamma.Gamma <> 0) then
    begin
     PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
     for Index := 0 to $FF
      do FMappingTable[Index] := Round(Power((Index * COne255th), PreCalcGamma) * 255);
    end
   else
    for Index := 0 to $FF
     do FMappingTable[Index] := Index;
  end;
end;


{ TCustomPngDecoder }

procedure TCustomPngDecoder.FilterSub(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
 for Index := PixelByteSize + 1 to BytesPerRow
  do CurrentRow[Index] := (CurrentRow[Index] + CurrentRow[Index - PixelByteSize]) and $FF;
{$ELSE}
asm
 ADD     EDX, 1
 MOV     EAX, EDX
 MOV     ECX, BytesPerRow
 ADD     EAX, PixelByteSize
 SUB     ECX, PixelByteSize
 LEA     EAX, EAX + ECX
 LEA     EDX, EDX + ECX
 NEG     ECX
 JNL     @Done

 PUSH    EBX

@Start:
 MOV     BL, [EAX + ECX].Byte
 ADD     BL, [EDX + ECX].Byte
 MOV     [EAX + ECX].Byte, BL

 ADD     ECX, 1
 JS      @Start

 POP     EBX

@Done:
{$ENDIF}
end;

procedure TCustomPngDecoder.FilterUp(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
 for Index := 1 to BytesPerRow
  do CurrentRow[Index] := (CurrentRow[Index] + PreviousRow[Index]) and $FF;
{$ELSE}
asm
 MOV     EAX, EDX
 MOV     EDX, ECX
 MOV     ECX, BytesPerRow
 LEA     EAX, EAX + ECX + 1
 LEA     EDX, EDX + ECX + 1
 NEG     ECX
 JNL     @Done

 PUSH    EBX

@Start:
 MOV     BL, [EAX + ECX].Byte
 ADD     BL, [EDX + ECX].Byte
 MOV     [EAX + ECX].Byte, BL

 ADD     ECX, 1
 JS      @Start

 POP     EBX

@Done:
{$ENDIF}
end;

procedure TCustomPngDecoder.FilterAverage(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 for Index := 1 to PixelByteSize
  do CurrentRow[Index] := (CurrentRow[Index] + PreviousRow[Index] shr 1) and $FF;

 for Index := PixelByteSize + 1 to BytesPerRow
  do CurrentRow[Index] := (CurrentRow[Index] + (CurrentRow[Index - PixelByteSize] + PreviousRow[Index]) shr 1) and $FF;
end;

function PaethPredictor(a, b, c: Byte): Integer; {$IFNDEF CPUx86_64} pascal; {$ENDIF}
{$IFDEF PUREPASCAL}
var
  DistA, DistB, DistC: Integer;
begin
 DistA := Abs(b - c);
 DistB := Abs(a - c);
 DistC := Abs(a + b - c * 2);

 if (DistA <= DistB) and (DistA <= DistC) then Result := a else
 if DistB <= DistC
  then Result := b
  else Result := c;
{$ELSE}
asm
{$IFDEF CPUx86_64}
  PUSH    RBX

  // calculate DistA
  MOVZX   RBX, c
  MOVZX   RAX, b
  SUB     RAX, R8
  MOV     R10, RAX
  JAE     @PositiveDistA
  NOT     RAX
  INC     RAX

  @PositiveDistA:

  // calculate DistB
  MOVZX   RBX, a
  SUB     RBX, c
  MOV     R11, RBX
  JAE     @PositiveDistB
  NOT     RBX
  INC     RBX

  @PositiveDistB:

  // calculate DistC
  ADD     R10, R11
  JAE     @PositiveDistC
  NOT     R10
  INC     R10

  @PositiveDistC:
  MOV     R11, RCX
  MOV     RCX, R10

  MOV     R12, RAX
  SUB     R12, EBX
  JA      @NextCheck
  MOV     R12, EAX
  SUB     R12, ECX
  JA      @NextCheck

  MOV     Result, R11
  JMP     @Done

  @NextCheck:
  MOV     R12, EBX
  SUB     R12, ECX
  JA      @ResultC

  MOV     Result, RDX
  JMP     @Done

  @ResultC:
  MOV     Result, R8

  @Done:
  POP     RBX
{$ELSE}
  MOVZX   EDX, c
  PUSH    EBX
  MOVZX   EAX, b
  SUB     EAX, EDX
  JAE     @PositiveDistA
  NOT     EAX
  INC     EAX

@PositiveDistA:
  MOVZX   EBX, a
  SUB     EBX, EDX
  JAE     @PositiveDistB
  NOT     EBX
  INC     EBX

@PositiveDistB:
  MOVZX   ECX, a
  SUB     ECX, EDX
  MOVZX   EDX, b
  ADD     ECX, EDX
  MOVZX   EDX, c
  SUB     ECX, EDX
  JAE     @PositiveDistC
  NOT     ECX
  INC     ECX

@PositiveDistC:
  MOV     EDX, EAX
  SUB     EDX, EBX
  JA      @NextCheck
  MOV     EDX, EAX
  SUB     EDX, ECX
  JA      @NextCheck

  MOVZX   EDX, a
  MOV     Result, EDX
  JMP     @Done

@NextCheck:
  MOV     EDX, EBX
  SUB     EDX, ECX
  JA      @ResultC

  MOVZX   EDX, b
  MOV     Result, EDX
  JMP     @Done

@ResultC:
  MOVZX   EDX, c
  MOV     Result, EDX

@Done:
  POP     EBX
{$ENDIF}
{$ENDIF}
end;

procedure TCustomPngDecoder.FilterPaeth(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 FilterUp(CurrentRow, PreviousRow, PixelByteSize, PixelByteSize);

 for Index := PixelByteSize + 1 to BytesPerRow
  do CurrentRow[Index] := (CurrentRow[Index] +
       PaethPredictor(CurrentRow[Index - PixelByteSize], PreviousRow[Index],
         PreviousRow[Index - PixelByteSize])) and $FF;
end;

procedure TCustomPngDecoder.DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod;
  CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
begin
 case FilterMethod of
  afmNone    : ;
  afmSub     : FilterSub(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
  afmUp      : FilterUp(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
  afmAverage : FilterAverage(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
  afmPaeth   : FilterPaeth(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
  else raise Exception.Create(RCStrUnsupportedFilter);
 end;
end;


{ TCustomPngEncoder }

procedure TCustomPngEncoder.FilterSub(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 // copy first pixel
 Move(CurrentRow[1], OutputRow[1], PixelByteSize);

 for Index := PixelByteSize + 1 to BytesPerRow
  do OutputRow[Index] := (CurrentRow[Index] - CurrentRow[Index - PixelByteSize]) and $FF;
end;

procedure TCustomPngEncoder.FilterUp(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 for Index := 1 to BytesPerRow
  do OutputRow[Index] := (CurrentRow[Index] - PreviousRow[Index]) and $FF;
end;

procedure TCustomPngEncoder.FilterAverage(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 for Index := 1 to PixelByteSize
  do OutputRow[Index] := (CurrentRow[Index] - PreviousRow[Index] shr 1) and $FF;

 for Index := PixelByteSize + 1 to BytesPerRow
  do OutputRow[Index] := (CurrentRow[Index] - (CurrentRow[Index - PixelByteSize] + PreviousRow[Index]) shr 1) and $FF;
end;

procedure TCustomPngEncoder.FilterPaeth(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 FilterUp(CurrentRow, PreviousRow, OutputRow, PixelByteSize, PixelByteSize);

 for Index := PixelByteSize + 1 to BytesPerRow
  do OutputRow[Index] := (CurrentRow[Index] -
       PaethPredictor(CurrentRow[Index - PixelByteSize], PreviousRow[Index],
         PreviousRow[Index - PixelByteSize])) and $FF;
end;

procedure TCustomPngEncoder.EncodeFilterRow(CurrentRow, PreviousRow,
  OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer);
var
  PixelIndex : Integer;
  CurrentSum : Cardinal;
  BestSum    : Cardinal;
begin
 BestSum := 0;
 CurrentSum := 0;
 OutputRow^[0] := 0;
 for PixelIndex := 1 to BytesPerRow
  do BestSum := BestSum + CurrentRow[PixelIndex];
 Move(CurrentRow^[0], OutputRow^[0], BytesPerRow + 1);

 // calculate sub filter
 FilterSub(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
 for PixelIndex := 1 to BytesPerRow
  do CurrentSum := CurrentSum + TempBuffer[PixelIndex];

 // check if sub filter is the current best filter
 if CurrentSum < BestSum then
  begin
   CurrentSum := BestSum;
   Move(TempBuffer^[1], OutputRow^[1], BytesPerRow + 1);
   OutputRow^[0] := 1;
  end;

 // calculate up filter
 FilterUp(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
 for PixelIndex := 1 to BytesPerRow
  do CurrentSum := CurrentSum + TempBuffer[PixelIndex];

 // check if up filter is the current best filter
 if CurrentSum < BestSum then
  begin
   CurrentSum := BestSum;
   Move(TempBuffer^[1], OutputRow^[1], BytesPerRow + 1);
   OutputRow^[0] := 2;
  end;

 // calculate average filter
 FilterAverage(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
 for PixelIndex := 1 to BytesPerRow
  do CurrentSum := CurrentSum + TempBuffer[PixelIndex];

 // check if average filter is the current best filter
 if CurrentSum < BestSum then
  begin
   CurrentSum := BestSum;
   Move(TempBuffer^[1], OutputRow^[1], BytesPerRow + 1);
   OutputRow^[0] := 3;
  end;

 // calculate paeth filter
 FilterPaeth(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
 for PixelIndex := 1 to BytesPerRow
  do CurrentSum := CurrentSum + TempBuffer[PixelIndex];

 // check if paeth filter is the current best filter
 if CurrentSum < BestSum then
  begin
   Move(TempBuffer^[1], OutputRow^[1], BytesPerRow + 1);
   OutputRow^[0] := 4;
  end;
end;


{ TCustomPngNonInterlacedDecoder }

constructor TCustomPngNonInterlacedDecoder.Create(Stream: TStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma = nil;
  Palette: TChunkPngPalette = nil);
begin
 inherited;
 FBytesPerRow := FHeader.BytesPerRow;
 FRowByteSize := FBytesPerRow + 1;
 GetMem(FRowBuffer[0], FRowByteSize);
 GetMem(FRowBuffer[1], FRowByteSize);
end;

destructor TCustomPngNonInterlacedDecoder.Destroy;
begin
 Dispose(FRowBuffer[0]);
 Dispose(FRowBuffer[1]);
 inherited;
end;

procedure TCustomPngNonInterlacedDecoder.DecodeToScanline(
  Bitmap: TObject; ScanLineCallback: TScanLineCallback);
var
  Index         : Integer;
  CurrentRow    : Integer;
  PixelByteSize : Integer;
begin
 // initialize variables
 CurrentRow := 0;
 PixelByteSize := FHeader.PixelByteSize;

 FillChar(FRowBuffer[1 - CurrentRow]^[0], FRowByteSize, 0);

 for Index := 0 to FHeader.Height - 1 do
  begin
   // read data from stream
   if FStream.Read(FRowBuffer[CurrentRow][0], FRowByteSize) <> FRowByteSize
    then raise Exception.Create(RCStrDataIncomplete);

   // filter current row
   DecodeFilterRow(TAdaptiveFilterMethod(FRowBuffer[CurrentRow]^[0]),
     FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow], FBytesPerRow, PixelByteSize);

   // transfer data from row to image
   TransferData(@FRowBuffer[CurrentRow][1], ScanLineCallback(Bitmap, Index));

   // flip current row
   CurrentRow := 1 - CurrentRow;
  end;
end;


{ TPngGrayscale1bitRGBADecoder }

procedure TPngGrayscale1bitRGBADecoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Src      : PByte absolute Source;
  Dest     : PRGB32 absolute Destination;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex);
   Dest^.R := FMappingTable[CGrayScaleTable1Bit[(Src^ shr BitIndex) and $1]];
   Dest^.G := Dest^.R;
   Dest^.B := Dest^.R;
   Dest^.A := $FF;
   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;
   Inc(Dest);
  end;
end;


{ TPngGrayscale2bitRGBADecoder }

procedure TPngGrayscale2bitRGBADecoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Src      : PByte absolute Source;
  Dest     : PRGB32 absolute Destination;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex, 2);
   Dest^.R := FMappingTable[CGrayScaleTable2Bit[(Src^ shr BitIndex) and $3]];
   Dest^.G := Dest^.R;
   Dest^.B := Dest^.R;
   Dest^.A := 255;
   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;
   Inc(Dest);
  end;
end;


{ TPngGrayscale4bitRGBADecoder }

procedure TPngGrayscale4bitRGBADecoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Src      : PByte absolute Source;
  Dest     : PRGB32 absolute Destination;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex, 4);
   Dest^.R := FMappingTable[CGrayScaleTable4Bit[(Src^ shr BitIndex) and $F]];
   Dest^.G := Dest^.R;
   Dest^.B := Dest^.R;
   Dest^.A := 255;
   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;
   Inc(Dest);
  end;
end;


{ TPngGrayscale8bitRGBADecoder }

procedure TPngGrayscale8bitRGBADecoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index : Integer;
  Src   : PByte absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := FMappingTable[Src^]; Inc(Src);
   Dest^.G := Dest^.R;
   Dest^.B := Dest^.R;
   Dest^.A := 255;
   Inc(Dest);
  end;
end;


{ TPngGrayscale16bitRGBADecoder }

procedure TPngGrayscale16bitRGBADecoder.TransferData(
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PWord absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := FMappingTable[Src^ and $FF]; Inc(Src);
   Dest^.G := Dest^.R;
   Dest^.B := Dest^.R;
   Dest^.A := 255;
   Inc(Dest);
  end;
end;


{ TPngTrueColor8bitRGBADecoder }

procedure TPngTrueColor8bitRGBADecoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB24 absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := FMappingTable[Src^.R];
   Dest^.G := FMappingTable[Src^.G];
   Dest^.B := FMappingTable[Src^.B];
   Dest^.A := 255;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngTrueColor16bitRGBADecoder }

procedure TPngTrueColor16bitRGBADecoder.TransferData(
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB24Word absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := FMappingTable[Src^.R and $FF];
   Dest^.G := FMappingTable[Src^.G and $FF];
   Dest^.B := FMappingTable[Src^.B and $FF];
   Dest^.A := 255;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngPaletteRGBADecoder }

procedure TPngPaletteRGBADecoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Src      : PByte absolute Source;
  Dest     : PRGB32 absolute Destination;
  Palette  : PRGB24Array;
  Color    : TRGB24;
  BitIndex : Byte;
  BitMask  : Byte;
  BitDepth : Byte;
begin
 BitIndex := 8;
 BitDepth := FHeader.BitDepth;
 BitMask  := (1 shl BitDepth) - 1;
 Palette  := PRGB24Array(FMappingTable);

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex, BitDepth);
   Color := Palette[(Src^ shr BitIndex) and BitMask];
   Dest^.R := Color.R;
   Dest^.G := Color.G;
   Dest^.B := Color.B;
   Dest^.A := 255;
   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;
   Inc(Dest);
  end;
end;


{ TPngPalette8bitRGBADecoder }

procedure TPngPalette8bitRGBADecoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index   : Integer;
  Src     : PByte absolute Source;
  Dest    : PRGB32 absolute Destination;
  Palette : PRGB24Array;
begin
 Palette  := PRGB24Array(FMappingTable);
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := Palette[Src^].R;
   Dest^.G := Palette[Src^].G;
   Dest^.B := Palette[Src^].B;
   Dest^.A := 255;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngGrayscaleAlpha8bitRGBADecoder }

procedure TPngGrayscaleAlpha8bitRGBADecoder.TransferData(
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PByte absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := FMappingTable[Src^]; Inc(Src);
   Dest^.G := Dest^.R;
   Dest^.B := Dest^.R;
   Dest^.A := Src^;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngGrayscaleAlpha16bitRGBADecoder }

procedure TPngGrayscaleAlpha16bitRGBADecoder.TransferData(
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PWord absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := FMappingTable[Src^ and $FF]; Inc(Src);
   Dest^.G := Dest^.R;
   Dest^.B := Dest^.R;
   Dest^.A := Src^ and $FF; Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngTrueColorAlpha8bitRGBADecoder }

procedure TPngTrueColorAlpha8bitRGBADecoder.TransferData(
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB32 absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := FMappingTable[Src^.R];
   Dest^.G := FMappingTable[Src^.G];
   Dest^.B := FMappingTable[Src^.B];
   Dest^.A := Src^.A;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngTrueColorAlpha16bitRGBADecoder }

procedure TPngTrueColorAlpha16bitRGBADecoder.TransferData(
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB32Word absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := FMappingTable[Src^.R and $FF];
   Dest^.G := FMappingTable[Src^.G and $FF];
   Dest^.B := FMappingTable[Src^.B and $FF];
   Dest^.A := Src^.A and $FF;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TCustomPngAdam7Decoder }

constructor TCustomPngAdam7Decoder.Create(Stream: TStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma = nil;
  Palette: TChunkPngPalette = nil);
begin
 inherited;

 // allocate row buffer memory
 GetMem(FRowBuffer[0], FHeader.BytesPerRow + 1);
 GetMem(FRowBuffer[1], FHeader.BytesPerRow + 1);
end;

destructor TCustomPngAdam7Decoder.Destroy;
begin
 Dispose(FRowBuffer[0]);
 Dispose(FRowBuffer[1]);
 inherited;
end;

procedure TCustomPngAdam7Decoder.DecodeToScanline(
  Bitmap: TObject; ScanLineCallback: TScanLineCallback);
var
  CurrentRow    : Integer;
  RowByteSize   : Integer;
  PixelPerRow   : Integer;
  PixelByteSize : Integer;
  CurrentPass   : Integer;
  PassRow       : Integer;
begin
 // initialize variables
 CurrentRow := 0;
 PixelByteSize := FHeader.PixelByteSize;

 // The Adam7 interlacer uses 7 passes to create the complete image
 for CurrentPass := 0 to 6 do
  begin
   // calculate some intermediate variables
   PixelPerRow := (FHeader.Width - CColumnStart[CurrentPass] + CColumnIncrement[CurrentPass] - 1) div CColumnIncrement[CurrentPass];

   with FHeader do
    case ColorType of
     ctGrayscale, ctIndexedColor: RowByteSize := (PixelPerRow * BitDepth + 7) div 8;
     ctTrueColor: RowByteSize := (PixelPerRow * BitDepth * 3) div 8;
     ctGrayscaleAlpha: RowByteSize := (PixelPerRow * BitDepth * 2) div 8;
     ctTrueColorAlpha: RowByteSize := (PixelPerRow * BitDepth * 4) div 8;
     else RowByteSize := 0;
    end;

   PassRow := CRowStart[CurrentPass];

   // clear previous row
   FillChar(FRowBuffer[1 - CurrentRow]^[0], RowByteSize, 0);

   // check whether there are any bytes to process in this pass.
   if RowByteSize > 0 then
    while PassRow < FHeader.Height do
     begin
      // get interlaced row data
      if FStream.Read(FRowBuffer[CurrentRow][0], RowByteSize + 1) <> (RowByteSize + 1)
       then raise Exception.Create(RCStrDataIncomplete);

      DecodeFilterRow(TAdaptiveFilterMethod(FRowBuffer[CurrentRow]^[0]), FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow], RowByteSize, PixelByteSize);

      // transfer and deinterlace image data
      TransferData(CurrentPass, @FRowBuffer[CurrentRow][1], ScanLineCallback(Bitmap, PassRow));

      // prepare for the next pass
      Inc(PassRow, CRowIncrement[CurrentPass]);
      CurrentRow := 1 - CurrentRow;
     end;
  end;
end;


{ TPngAdam7Grayscale1bitRGBADecoder }

procedure TPngAdam7Grayscale1bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
  Dest     : PRGB32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 BitIndex := 8;
 repeat
   Dec(BitIndex);
   Dest^.R := FMappingTable[CGrayScaleTable1Bit[(Src^ shr BitIndex) and $1]];
   Dest^.G := Dest^.R;
   Dest^.B := Dest^.R;
   Dest^.A := 255;

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;

  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7Grayscale2bitRGBADecoder }

procedure TPngAdam7Grayscale2bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
  Dest     : PRGB32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 BitIndex := 8;
 repeat
   Dec(BitIndex, 2);
   Dest^.R := FMappingTable[CGrayScaleTable2Bit[((Src^ shr BitIndex) and $3)]];
   Dest^.G := Dest^.R;
   Dest^.B := Dest^.R;
   Dest^.A := 255;

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;

  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7Grayscale4bitRGBADecoder }

procedure TPngAdam7Grayscale4bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
  Dest     : PRGB32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 BitIndex := 8;
 repeat
   Dec(BitIndex, 4);
   Dest^.R := FMappingTable[CGrayScaleTable4Bit[((Src^ shr BitIndex) and $F)]];
   Dest^.G := Dest^.R;
   Dest^.B := Dest^.R;
   Dest^.A := 255;

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;

  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;

{ TPngAdam7Grayscale8bitRGBADecoder }

procedure TPngAdam7Grayscale8bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PByte absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := FMappingTable[Src^]; Inc(Src);
  Dest^.G := Dest^.R;
  Dest^.B := Dest^.R;
  Dest^.A := 255;

  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7Grayscale16bitRGBADecoder }

procedure TPngAdam7Grayscale16bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PWord absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := FMappingTable[Src^ and $FF]; Inc(Src);
  Dest^.G := Dest^.R;
  Dest^.B := Dest^.R;
  Dest^.A := 255;

  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7TrueColor8bitRGBADecoder }

procedure TPngAdam7TrueColor8bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB24 absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := FMappingTable[Src^.R];
  Dest^.G := FMappingTable[Src^.G];
  Dest^.B := FMappingTable[Src^.B];
  Dest^.A := 255;

  Inc(Src);
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7TrueColor16bitRGBADecoder }

procedure TPngAdam7TrueColor16bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB24Word absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := FMappingTable[Src^.R and $FF];
  Dest^.G := FMappingTable[Src^.G and $FF];
  Dest^.B := FMappingTable[Src^.B and $FF];
  Dest^.A := 255;

  Inc(Src);
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7Palette1bitRGBADecoder }

procedure TPngAdam7Palette1bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
  Dest     : PRGB32 absolute Destination;
  Palette  : PRGB24Array;
  Color    : TRGB24;
begin
 BitIndex := 8;
 Palette  := PRGB24Array(FMappingTable);
 Index    := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dec(BitIndex);
  Color := Palette[(Src^ shr BitIndex) and $1];
  Dest^.R := Color.R;
  Dest^.G := Color.G;
  Dest^.B := Color.B;
  Dest^.A := 255;

  if BitIndex = 0 then
   begin
    BitIndex := 8;
    Inc(Src);
   end;
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7Palette2bitRGBADecoder }

procedure TPngAdam7Palette2bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
  Dest     : PRGB32 absolute Destination;
  Palette  : PRGB24Array;
  Color    : TRGB24;
begin
 BitIndex := 8;
 Palette  := PRGB24Array(FMappingTable);
 Index    := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dec(BitIndex, 2);
  Color := Palette[(Src^ shr BitIndex) and $3];
  Dest^.R := Color.R;
  Dest^.G := Color.G;
  Dest^.B := Color.B;
  Dest^.A := 255;

  if BitIndex = 0 then
   begin
    BitIndex := 8;
    Inc(Src);
   end;
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7Palette4bitRGBADecoder }

procedure TPngAdam7Palette4bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
  Dest     : PRGB32 absolute Destination;
  Palette  : PRGB24Array;
  Color    : TRGB24;
begin
 BitIndex := 8;
 Palette  := PRGB24Array(FMappingTable);
 Index    := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dec(BitIndex, 4);
  Color := Palette[(Src^ shr BitIndex) and $F];
  Dest^.R := Color.R;
  Dest^.G := Color.G;
  Dest^.B := Color.B;
  Dest^.A := 255;

  if BitIndex = 0 then
   begin
    BitIndex := 8;
    Inc(Src);
   end;
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7Palette8bitRGBADecoder }

procedure TPngAdam7Palette8bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index   : Integer;
  Src     : PByte absolute Source;
  Dest    : PRGB32 absolute Destination;
  Palette : PRGB24Array;
begin
 Palette := PRGB24Array(FMappingTable);
 Index   := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := Palette[Src^].R;
  Dest^.G := Palette[Src^].G;
  Dest^.B := Palette[Src^].B;
  Dest^.A := 255;

  Inc(Src);
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7GrayscaleAlpha8bitRGBADecoder }

procedure TPngAdam7GrayscaleAlpha8bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PByte absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := FMappingTable[Src^]; Inc(Src);
  Dest^.G := Dest^.R;
  Dest^.B := Dest^.R;
  Dest^.A := Src^;
  Inc(Src);

  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7GrayscaleAlpha16bitRGBADecoder }

procedure TPngAdam7GrayscaleAlpha16bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PWord absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := FMappingTable[Src^ and $FF]; Inc(Src);
  Dest^.G := Dest^.R;
  Dest^.B := Dest^.R;
  Dest^.A := Src^ and $FF; Inc(Src);

  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7TrueColorAlpha8bitRGBADecoder }

procedure TPngAdam7TrueColorAlpha8bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB32 absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := FMappingTable[Src^.R];
  Dest^.G := FMappingTable[Src^.G];
  Dest^.B := FMappingTable[Src^.B];
  Dest^.A := Src^.A;

  Inc(Src);
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7TrueColorAlpha16bitRGBADecoder }

procedure TPngAdam7TrueColorAlpha16bitRGBADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB32Word absolute Source;
  Dest  : PRGB32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := FMappingTable[Src^.R and $FF];
  Dest^.G := FMappingTable[Src^.G and $FF];
  Dest^.B := FMappingTable[Src^.B and $FF];
  Dest^.A := Src^.A and $FF;

  Inc(Src);
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngTrueColor8bitBGRADecoder }

procedure TPngTrueColor8bitBGRADecoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB24 absolute Source;
  Dest  : PBGR32 absolute Destination;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := FMappingTable[Src^.R];
   Dest^.G := FMappingTable[Src^.G];
   Dest^.B := FMappingTable[Src^.B];
   Dest^.A := 255;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngTrueColor16bitBGRADecoder }

procedure TPngTrueColor16bitBGRADecoder.TransferData(
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB24Word absolute Source;
  Dest  : PBGR32 absolute Destination;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := FMappingTable[Src^.R and $FF];
   Dest^.G := FMappingTable[Src^.G and $FF];
   Dest^.B := FMappingTable[Src^.B and $FF];
   Dest^.A := 255;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngPaletteBGRADecoder }

procedure TPngPaletteBGRADecoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Src      : PByte absolute Source;
  Dest     : PBGR32 absolute Destination;
  Palette  : PRGB24Array;
  Color    : TRGB24;
  BitIndex : Byte;
  BitMask  : Byte;
  BitDepth : Byte;
begin
 BitIndex := 8;
 BitDepth := FHeader.BitDepth;
 BitMask  := (1 shl BitDepth) - 1;
 Palette  := PRGB24Array(FMappingTable);

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex, BitDepth);
   Color := Palette[(Src^ shr BitIndex) and BitMask];
   Dest^.R := Color.R;
   Dest^.G := Color.G;
   Dest^.B := Color.B;
   Dest^.A := 255;
   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Src);
    end;
   Inc(Dest);
  end;
end;


{ TPngPalette8bitBGRADecoder }

procedure TPngPalette8bitBGRADecoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index   : Integer;
  Src     : PByte absolute Source;
  Dest    : PBGR32 absolute Destination;
  Palette : PRGB24Array;
begin
 Palette  := PRGB24Array(FMappingTable);
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := Palette[Src^].R;
   Dest^.G := Palette[Src^].G;
   Dest^.B := Palette[Src^].B;
   Dest^.A := 255;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngTrueColorAlpha8bitBGRADecoder }

procedure TPngTrueColorAlpha8bitBGRADecoder.TransferData(
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB32 absolute Source;
  Dest  : PBGR32 absolute Destination;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := FMappingTable[Src^.R];
   Dest^.G := FMappingTable[Src^.G];
   Dest^.B := FMappingTable[Src^.B];
   Dest^.A := Src^.A;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngTrueColorAlpha16bitBGRADecoder }

procedure TPngTrueColorAlpha16bitBGRADecoder.TransferData(
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB32Word absolute Source;
  Dest  : PBGR32 absolute Destination;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := FMappingTable[Src^.R and $FF];
   Dest^.G := FMappingTable[Src^.G and $FF];
   Dest^.B := FMappingTable[Src^.B and $FF];
   Dest^.A := Src^.A and $FF;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngAdam7TrueColor8bitBGRADecoder }

procedure TPngAdam7TrueColor8bitBGRADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB24 absolute Source;
  Dest  : PBGR32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := FMappingTable[Src^.R];
  Dest^.G := FMappingTable[Src^.G];
  Dest^.B := FMappingTable[Src^.B];
  Dest^.A := 255;

  Inc(Src);
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7TrueColor16bitBGRADecoder }

procedure TPngAdam7TrueColor16bitBGRADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB24Word absolute Source;
  Dest  : PBGR32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := FMappingTable[Src^.R and $FF];
  Dest^.G := FMappingTable[Src^.G and $FF];
  Dest^.B := FMappingTable[Src^.B and $FF];
  Dest^.A := 255;

  Inc(Src);
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7Palette1bitBGRADecoder }

procedure TPngAdam7Palette1bitBGRADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
  Dest     : PBGR32 absolute Destination;
  Palette  : PRGB24Array;
  Color    : TRGB24;
begin
 BitIndex := 8;
 Palette  := PRGB24Array(FMappingTable);
 Index    := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dec(BitIndex);
  Color := Palette[(Src^ shr BitIndex) and $1];
  Dest^.R := Color.R;
  Dest^.G := Color.G;
  Dest^.B := Color.B;
  Dest^.A := 255;

  if BitIndex = 0 then
   begin
    BitIndex := 8;
    Inc(Src);
   end;
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7Palette2bitBGRADecoder }

procedure TPngAdam7Palette2bitBGRADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
  Dest     : PBGR32 absolute Destination;
  Palette  : PRGB24Array;
  Color    : TRGB24;
begin
 BitIndex := 8;
 Palette  := PRGB24Array(FMappingTable);
 Index    := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dec(BitIndex, 2);
  Color := Palette[(Src^ shr BitIndex) and $3];
  Dest^.R := Color.R;
  Dest^.G := Color.G;
  Dest^.B := Color.B;
  Dest^.A := 255;

  if BitIndex = 0 then
   begin
    BitIndex := 8;
    Inc(Src);
   end;
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7Palette4bitBGRADecoder }

procedure TPngAdam7Palette4bitBGRADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index    : Integer;
  BitIndex : Integer;
  Src      : PByte absolute Source;
  Dest     : PBGR32 absolute Destination;
  Palette  : PRGB24Array;
  Color    : TRGB24;
begin
 BitIndex := 8;
 Palette  := PRGB24Array(FMappingTable);
 Index    := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dec(BitIndex, 4);
  Color := Palette[(Src^ shr BitIndex) and $F];
  Dest^.R := Color.R;
  Dest^.G := Color.G;
  Dest^.B := Color.B;
  Dest^.A := 255;

  if BitIndex = 0 then
   begin
    BitIndex := 8;
    Inc(Src);
   end;
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7Palette8bitBGRADecoder }

procedure TPngAdam7Palette8bitBGRADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index   : Integer;
  Src     : PByte absolute Source;
  Dest    : PBGR32 absolute Destination;
  Palette : PRGB24Array;
begin
 Palette := PRGB24Array(FMappingTable);
 Index   := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := Palette[Src^].R;
  Dest^.G := Palette[Src^].G;
  Dest^.B := Palette[Src^].B;
  Dest^.A := 255;

  Inc(Src);
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7TrueColorAlpha8bitBGRADecoder }

procedure TPngAdam7TrueColorAlpha8bitBGRADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB32 absolute Source;
  Dest  : PBGR32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := FMappingTable[Src^.R];
  Dest^.G := FMappingTable[Src^.G];
  Dest^.B := FMappingTable[Src^.B];
  Dest^.A := Src^.A;

  Inc(Src);
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TPngAdam7TrueColorAlpha16bitBGRADecoder }

procedure TPngAdam7TrueColorAlpha16bitBGRADecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: Pointer);
var
  Index : Integer;
  Src   : PRGB32Word absolute Source;
  Dest  : PBGR32 absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(Dest, Index);
 repeat
  Dest^.R := FMappingTable[Src^.R and $FF];
  Dest^.G := FMappingTable[Src^.G and $FF];
  Dest^.B := FMappingTable[Src^.B and $FF];
  Dest^.A := Src^.A and $FF;

  Inc(Src);
  Inc(Dest, CColumnIncrement[Pass]);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= FHeader.Width;
end;


{ TCustomPngNonInterlacedEncoder }

constructor TCustomPngNonInterlacedEncoder.Create(Stream: TStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma;
  Palette: TChunkPngPalette);
begin
 inherited;
 FBytesPerRow := FHeader.BytesPerRow;
 FRowByteSize := FBytesPerRow + 1;
 GetMem(FRowBuffer[0], FRowByteSize);
 GetMem(FRowBuffer[1], FRowByteSize);
end;

destructor TCustomPngNonInterlacedEncoder.Destroy;
begin
 Dispose(FRowBuffer[0]);
 Dispose(FRowBuffer[1]);
 inherited;
end;

function TCustomPngNonInterlacedEncoder.ColorInPalette(
  Color: TBGR32): Integer;
var
  Palette : PPalette24;
  Color24 : TRGB24;
begin
 Palette := FPalette.PaletteEntriesPointer;
 for Result := 0 to FPalette.Count - 1 do
  begin
   Color24 := Palette^[Result];
   if (Color.R = Color24.R) and
      (Color.G = Color24.G) and
      (Color.B = Color24.B)
    then Exit;
  end;
 Result := -1;
end;

procedure TCustomPngNonInterlacedEncoder.EncodeFromScanline(Bitmap: TObject;
  ScanLineCallback: TScanLineCallback);
var
  Index      : Integer;
  CurrentRow : Integer;
  OutputRow  : PByteArray;
  TempBuffer : PByteArray;
begin
 // initialize variables
 CurrentRow := 0;
 FillChar(FRowBuffer[1 - CurrentRow]^[0], FRowByteSize, 0);

 // check if pre filter is used and eventually calculate pre filter
 if FHeader.ColorType <> ctIndexedColor then
  begin
   Assert(FRowByteSize = FBytesPerRow + 1);
   GetMem(OutputRow, FRowByteSize);
   GetMem(TempBuffer, FRowByteSize);
   try
    for Index := 0 to FHeader.Height - 1 do
     begin
      // transfer data from image to current row
      TransferData(ScanLineCallback(Bitmap, Index), @FRowBuffer[CurrentRow][1]);

      // filter current row
      EncodeFilterRow(FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow],
        OutputRow, TempBuffer, FBytesPerRow, FHeader.PixelByteSize);

      // write data to data stream
      FStream.Write(OutputRow[0], FRowByteSize);

      // flip current row used
      CurrentRow := 1 - CurrentRow;
     end;
   finally
    Dispose(OutputRow);
    Dispose(TempBuffer);
   end;
  end
 else
  for Index := 0 to FHeader.Height - 1 do
   begin
    // transfer data from image to current row
    TransferData(ScanLineCallback(Bitmap, Index), @FRowBuffer[CurrentRow][1]);

    // set filter method to none
    FRowBuffer[CurrentRow][0] := 0;

    // write data to data stream
    FStream.Write(FRowBuffer[CurrentRow][0], FRowByteSize);

    // flip current row used
    CurrentRow := 1 - CurrentRow;
   end;
end;


{ TPngRGBAGrayscale1bitEncoder }

procedure TPngRGBAGrayscale1bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  Src      : PBGR32 absolute Source;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex);
   Dest^ := (Dest^ and not ($1 shl BitIndex)) or
     (((Src^.R shr 7) and $1) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Src);
  end;
end;


{ TPngRGBAGrayscale2bitEncoder }

procedure TPngRGBAGrayscale2bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  Src      : PBGR32 absolute Source;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex, 2);
   Dest^ := (Dest^ and not ($3 shl BitIndex)) or
     ((((Src)^.R shr 6) and $3) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Src);
  end;
end;


{ TPngRGBAGrayscale4bitEncoder }

procedure TPngRGBAGrayscale4bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  Src      : PBGR32 absolute Source;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex, 4);
   Dest^ := (Dest^ and not ($F shl BitIndex)) or
     (((Src^.R shr 4) and $F) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Src);
  end;
end;


{ TPngRGBAGrayscale8bitEncoder }

procedure TPngRGBAGrayscale8bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index : Integer;
  Dest  : PByte absolute Destination;
  Src   : PBGR32 absolute Source;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^ := Src^.R;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngRGBATrueColor8bitEncoder }

procedure TPngRGBATrueColor8bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index : Integer;
  Dest  : PRGB24 absolute Destination;
  Src   : PBGR32 absolute Source;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := Src^.R;
   Dest^.G := Src^.G ;
   Dest^.B := Src^.B;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngRGBAPalette1bitEncoder }

procedure TPngRGBAPalette1bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  Src      : PBGR32 absolute Source;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex);
   Dest^ := (Dest^ and not ($1 shl BitIndex)) or
     ((ColorInPalette(Src^) and $1) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Src);
  end;
end;


{ TPngRGBAPalette2bitEncoder }

procedure TPngRGBAPalette2bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  Src      : PBGR32 absolute Source;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex, 2);
   Dest^ := (Dest^ and not ($3 shl BitIndex)) or
     ((ColorInPalette(Src^) and $3) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Src);
  end;
end;


{ TPngRGBAPalette4bitEncoder }

procedure TPngRGBAPalette4bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  Src      : PBGR32 absolute Source;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex, 4);
   Dest^ := (Dest^ and not ($F shl BitIndex)) or
     ((ColorInPalette(Src^) and $F) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Src);
  end;
end;


{ TPngRGBAPalette8bitEncoder }

procedure TPngRGBAPalette8bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index : Integer;
  Dest  : PByte absolute Destination;
  Src   : PBGR32 absolute Source;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^ := ColorInPalette(Src^);
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngRGBAGrayscaleAlpha8bitEncoder }

procedure TPngRGBAGrayscaleAlpha8bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index : Integer;
  Dest  : PByte absolute Destination;
  Src   : PBGR32 absolute Source;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^ := Src^.R; Inc(Dest);
   Dest^ := Src^.A; Inc(Dest);
   Inc(Src);
  end;
end;


{ TPngRGBATrueColorAlpha8bitEncoder }

procedure TPngRGBATrueColorAlpha8bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
begin
 Move(Source^, Destination^, FHeader.Width * SizeOf(TRGB32));
end;


{ TPngBGRATrueColor8bitEncoder }

procedure TPngBGRATrueColor8bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index : Integer;
  Dest  : PRGB24 absolute Destination;
  Src   : PBGR32 absolute Source;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := Src^.R;
   Dest^.G := Src^.G ;
   Dest^.B := Src^.B;
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngBGRAPalette1bitEncoder }

procedure TPngBGRAPalette1bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  Src      : PBGR32 absolute Source;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex);
   Dest^ := (Dest^ and not ($1 shl BitIndex)) or
     ((ColorInPalette(Src^) and $1) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Src);
  end;
end;


{ TPngBGRAPalette2bitEncoder }

procedure TPngBGRAPalette2bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  Src      : PBGR32 absolute Source;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex, 2);
   Dest^ := (Dest^ and not ($3 shl BitIndex)) or
     ((ColorInPalette(Src^) and $3) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Src);
  end;
end;


{ TPngBGRAPalette4bitEncoder }

procedure TPngBGRAPalette4bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index    : Integer;
  Dest     : PByte absolute Destination;
  Src      : PBGR32 absolute Source;
  BitIndex : Byte;
begin
 BitIndex := 8;

 for Index := 0 to FHeader.Width - 1 do
  begin
   Dec(BitIndex, 4);
   Dest^ := (Dest^ and not ($F shl BitIndex)) or
     ((ColorInPalette(Src^) and $F) shl BitIndex);

   if BitIndex = 0 then
    begin
     BitIndex := 8;
     Inc(Dest);
    end;
   Inc(Src);
  end;
end;


{ TPngBGRAPalette8bitEncoder }

procedure TPngBGRAPalette8bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index : Integer;
  Dest  : PByte absolute Destination;
  Src   : PBGR32 absolute Source;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^ := ColorInPalette(Src^);
   Inc(Src);
   Inc(Dest);
  end;
end;


{ TPngBGRATrueColorAlpha8bitEncoder }

procedure TPngBGRATrueColorAlpha8bitEncoder.TransferData(Source: Pointer;
  Destination: Pointer);
var
  Index : Integer;
  Dest  : PRGB32 absolute Destination;
  Src   : PBGR32 absolute Source;
begin
 for Index := 0 to FHeader.Width - 1 do
  begin
   Dest^.R := Src^.R;
   Dest^.G := Src^.G;
   Dest^.B := Src^.B;
   Dest^.A := Src^.A;
   Inc(Src);
   Inc(Dest);
  end;
end;

end.
