unit DAV_GuiPngChunks;

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
  Classes, Graphics, SysUtils, DAV_Types, DAV_ChunkClasses, DAV_GuiCommon,
  DAV_GuiPngTypes, DAV_GuiPngClasses;

type
  TCustomChunkPng = class(TDefinedChunk)
  public
    constructor Create; override;
  end;

  TChunkPngImageHeader = class(TCustomChunkPng)
  private
    FWidth             : Integer;
    FHeight            : Integer;
    FBitDepth          : Byte;
    FColorType         : TColorType;
    FCompressionMethod : Byte;
    FFilterMethod      : TFilterMethod;
    FInterlaceMethod   : TInterlaceMethod;
    function GetHasPalette: Boolean;
    function GetBytesPerRow: Integer;
    function GetPixelByteSize: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure ResetToDefault; virtual;

    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property BitDepth: Byte read FBitDepth write FBitDepth;
    property ColorType: TColorType read FColorType write FColorType;
    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
    property FilterMethod: TFilterMethod read FFilterMethod write FFilterMethod;
    property InterlaceMethod: TInterlaceMethod read FInterlaceMethod write FInterlaceMethod;
    property HasPalette: Boolean read GetHasPalette;

    property BytesPerRow: Integer read GetBytesPerRow;
    property PixelByteSize: Integer read GetPixelByteSize;
  end;

  TCustomChunkPngWithHeader = class(TCustomChunkPng)
  protected
    FHeader : TChunkPngImageHeader;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); reintroduce; virtual;
    procedure HeaderChanged; virtual;

    property Header: TChunkPngImageHeader read FHeader;
  end;
  TCustomChunkPngWithHeaderClass = class of TCustomChunkPngWithHeader;

  TChunkPngImageData = class(TCustomChunkPngWithHeader)
  private
    FData : TMemoryStream;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Data: TMemoryStream read FData;
  end;

  TPalette24 = array of TRGB24;
  PPalette24 = PRGB24Array;

  TChunkPngPalette = class(TCustomChunkPngWithHeader)
  private
    FPaletteEntries : TPalette24;
    function GetPaletteEntry(Index: Integer): TRGB24;
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    procedure SetPaletteEntry(Index: Integer; const Value: TRGB24);
    function GetPaletteEntriesPointer: PPalette24;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure PaletteEntriesChanged; virtual;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property PaletteEntry[Index: Integer]: TRGB24 read GetPaletteEntry write SetPaletteEntry; default;
    property PaletteEntriesPointer: PPalette24 read GetPaletteEntriesPointer;
    property Count: Integer read GetCount write SetCount;
  end;

  TChunkPngGamma = class(TCustomChunkPngWithHeader)
  private
    FGamma : Cardinal;
    function GetGammaAsSingle: Single;
    procedure SetGammaAsSingle(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Gamma: Cardinal read FGamma write FGamma;
    property GammaAsSingle: Single read GetGammaAsSingle write SetGammaAsSingle;
  end;

  TChunkPngStandardColorSpaceRGB = class(TCustomChunkPngWithHeader)
  private
    FRenderingIntent : Byte;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property RenderingIntent: Byte read FRenderingIntent write FRenderingIntent;
  end;

  TChunkPngPrimaryChromaticities = class(TCustomChunkPngWithHeader)
  private
    FWhiteX : Integer;
    FWhiteY : Integer;
    FRedX   : Integer;
    FRedY   : Integer;
    FGreenX : Integer;
    FGreenY : Integer;
    FBlueX  : Integer;
    FBlueY  : Integer;
    function GetBlueX: Single;
    function GetBlueY: Single;
    function GetGreenX: Single;
    function GetGreenY: Single;
    function GetRedX: Single;
    function GetRedY: Single;
    function GetWhiteX: Single;
    function GetWhiteY: Single;
    procedure SetBlueX(const Value: Single);
    procedure SetBlueY(const Value: Single);
    procedure SetGreenX(const Value: Single);
    procedure SetGreenY(const Value: Single);
    procedure SetRedX(const Value: Single);
    procedure SetRedY(const Value: Single);
    procedure SetWhiteX(const Value: Single);
    procedure SetWhiteY(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property WhiteX: Integer read FWhiteX write FWhiteX;
    property WhiteY: Integer read FWhiteY write FWhiteY;
    property RedX: Integer read FRedX write FRedX;
    property RedY: Integer read FRedY write FRedY;
    property GreenX: Integer read FGreenX write FGreenX;
    property GreenY: Integer read FGreenY write FGreenY;
    property BlueX: Integer read FBlueX write FBlueX;
    property BlueY: Integer read FBlueY write FBlueY;

    property WhiteXAsSingle: Single read GetWhiteX write SetWhiteX;
    property WhiteYAsSingle: Single read GetWhiteY write SetWhiteY;
    property RedXAsSingle: Single read GetRedX write SetRedX;
    property RedYAsSingle: Single read GetRedY write SetRedY;
    property GreenXAsSingle: Single read GetGreenX write SetGreenX;
    property GreenYAsSingle: Single read GetGreenY write SetGreenY;
    property BlueXAsSingle: Single read GetBlueX write SetBlueX;
    property BlueYAsSingle: Single read GetBlueY write SetBlueY;
  end;

  TChunkPngTime = class(TCustomChunkPngWithHeader)
  private
    FYear   : Word;
    FMonth  : Byte;
    FDay    : Byte;
    FHour   : Byte;
    FMinute : Byte;
    FSecond : Byte;
    function GetModifiedDateTime: TDateTime;
    procedure SetModifiedDateTime(const Value: TDateTime);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Year: Word read FYear write FYear;
    property Month: Byte read FMonth write FMonth;
    property Day: Byte read FDay write FDay;
    property Hour: Byte read FHour write FHour;
    property Minute: Byte read FMinute write FMinute;
    property Second: Byte read FSecond write FSecond;
    property ModifiedDateTime: TDateTime read GetModifiedDateTime write SetModifiedDateTime;
  end;

  TChunkPngEmbeddedIccProfile = class(TCustomChunkPngWithHeader)
  private
    FProfileName       : AnsiString;
    FCompressionMethod : Byte;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property ProfileName: AnsiString read FProfileName write FProfileName;
    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
  end;

  TCustomPngSignificantBits = class(TPersistent)
  protected
    class function GetChunkSize: Integer; virtual; abstract;
  public
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Integer read GetChunkSize;
  end;

  TPngSignificantBitsFormat0 = class(TCustomPngSignificantBits)
  private
    FGrayBits : Byte;
  protected
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property GrayBits: Byte read FGrayBits write FGrayBits;
  end;

  TPngSignificantBitsFormat23 = class(TCustomPngSignificantBits)
  private
    FRedBits   : Byte;
    FBlueBits  : Byte;
    FGreenBits : Byte;
  protected
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property RedBits: Byte read FRedBits write FRedBits;
    property BlueBits: Byte read FBlueBits write FBlueBits;
    property GreenBits: Byte read FGreenBits write FGreenBits;
  end;

  TPngSignificantBitsFormat4 = class(TCustomPngSignificantBits)
  private
    FGrayBits  : Byte;
    FAlphaBits : Byte;
  protected
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property GrayBits: Byte read FGrayBits write FGrayBits;
    property AlphaBits: Byte read FAlphaBits write FAlphaBits;
  end;

  TPngSignificantBitsFormat6 = class(TCustomPngSignificantBits)
  private
    FRedBits   : Byte;
    FBlueBits  : Byte;
    FGreenBits : Byte;
    FAlphaBits : Byte;
  protected
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property RedBits: Byte read FRedBits write FRedBits;
    property BlueBits: Byte read FBlueBits write FBlueBits;
    property GreenBits: Byte read FGreenBits write FGreenBits;
    property AlphaBits: Byte read FAlphaBits write FAlphaBits;
  end;

  TChunkPngSignificantBits = class(TCustomChunkPngWithHeader)
  private
    FSignificantBits : TCustomPngSignificantBits;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure HeaderChanged; override;

    property SignificantBits: TCustomPngSignificantBits read FSignificantBits;
  end;

  TCustomPngBackgroundColor = class(TPersistent)
  protected
    class function GetChunkSize: Integer; virtual; abstract;
  public
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Integer read GetChunkSize;
  end;

  TPngBackgroundColorFormat04 = class(TCustomPngBackgroundColor)
  private
    FGraySampleValue : Word;
  protected
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property GraySampleValue: Word read FGraySampleValue write FGraySampleValue;
  end;

  TPngBackgroundColorFormat26 = class(TCustomPngBackgroundColor)
  private
    FRedSampleValue : Word;
    FBlueSampleValue : Word;
    FGreenSampleValue : Word;
  protected
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property RedSampleValue: Word read FRedSampleValue write FRedSampleValue;
    property BlueSampleValue: Word read FBlueSampleValue write FBlueSampleValue;
    property GreenSampleValue: Word read FGreenSampleValue write FGreenSampleValue;
  end;

  TPngBackgroundColorFormat3 = class(TCustomPngBackgroundColor)
  protected
    FIndex : Byte;
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property PaletteIndex: Byte read FIndex write FIndex;
  end;

  TChunkPngBackgroundColor = class(TCustomChunkPngWithHeader)
  protected
    FBackground : TCustomPngBackgroundColor;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure HeaderChanged; override;

    property Background: TCustomPngBackgroundColor read FBackground;
  end;

  TChunkPngImageHistogram = class(TCustomChunkPngWithHeader)
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngSuggestedPalette = class(TCustomChunkPngWithHeader)
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TCustomPngTransparency = class(TPersistent)
  protected
    function GetChunkSize: Integer; virtual; abstract;
  public
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Integer read GetChunkSize;
  end;

  TPngTransparencyFormat0 = class(TCustomPngTransparency)
  private
    FGraySampleValue : Word;
  protected
    function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property GraySampleValue: Word read FGraySampleValue write FGraySampleValue;
  end;

  TPngTransparencyFormat2 = class(TCustomPngTransparency)
  private
    FRedSampleValue : Word;
    FBlueSampleValue : Word;
    FGreenSampleValue : Word;
  protected
    function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property RedSampleValue: Word read FRedSampleValue write FRedSampleValue;
    property BlueSampleValue: Word read FBlueSampleValue write FBlueSampleValue;
    property GreenSampleValue: Word read FGreenSampleValue write FGreenSampleValue;
  end;

  TPngTransparencyFormat3 = class(TCustomPngTransparency)
  private
    function GetCount: Integer;
    function GetTransparency(Index: Integer): Byte;
  protected
    FTransparency : array of Byte;
    function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Count: Integer read GetCount;
    property Transparency[Index: Integer]: Byte read GetTransparency;
  end;

  TChunkPngTransparency = class(TCustomChunkPngWithHeader)
  protected
    FTransparency : TCustomPngTransparency;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure HeaderChanged; override;

    property Transparency: TCustomPngTransparency read FTransparency;
  end;

  TChunkPngPhysicalPixelDimensions = class(TCustomChunkPngWithHeader)
  private
    FPixelsPerUnitX : Cardinal;
    FPixelsPerUnitY : Cardinal;
    FUnit           : Byte;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property PixelsPerUnitX: Cardinal read FPixelsPerUnitX write FPixelsPerUnitX;
    property PixelsPerUnitY: Cardinal read FPixelsPerUnitY write FPixelsPerUnitY;
    property PixelUnit: Byte read FUnit write FUnit;
  end;

  TChunkPngPhysicalScale = class(TCustomChunkPngWithHeader)
  private
    FUnitSpecifier  : Byte;
    FUnitsPerPixelX : Single;
    FUnitsPerPixelY : Single;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property UnitSpecifier: Byte read FUnitSpecifier write FUnitSpecifier;
    property UnitsPerPixelX: Single read FUnitsPerPixelX write FUnitsPerPixelX;
    property UnitsPerPixelY: Single read FUnitsPerPixelY write FUnitsPerPixelY;
  end;

  TChunkPngImageOffset = class(TCustomChunkPngWithHeader)
  private
    FImagePositionX : Integer;
    FImagePositionY : Integer;
    FUnitSpecifier  : Byte;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property UnitSpecifier: Byte read FUnitSpecifier write FUnitSpecifier;
    property ImagePositionX: Integer read FImagePositionX write FImagePositionX;
    property ImagePositionY: Integer read FImagePositionY write FImagePositionY;
  end;

  TChunkPngPixelCalibrator = class(TCustomChunkPngWithHeader)
  private
    FCalibratorName : AnsiString;
    FOriginalZeroes : array [0..1] of Integer;
    FEquationType   : Byte;
    FNumberOfParams : Byte;
    FUnitName       : AnsiString;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property CalibratorName: AnsiString read FCalibratorName write FCalibratorName;
    property OriginalZeroMin: Integer read FOriginalZeroes[0] write FOriginalZeroes[0];
    property OriginalZeroMax: Integer read FOriginalZeroes[1] write FOriginalZeroes[1];
    property EquationType: Byte read FEquationType write FEquationType;
    property NumberOfParams: Byte read FNumberOfParams write FNumberOfParams;
  end;

  TCustomDefinedChunkTextChunk = class(TCustomChunkPngWithHeader)
  protected
    FKeyword : AnsiString;
    FText    : AnsiString;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Keyword: AnsiString read FKeyword write FKeyword;
    property Text: AnsiString read FText write FText;
  end;

  TChunkPngTextChunk = class(TCustomDefinedChunkTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngCompressedTextChunk = class(TCustomDefinedChunkTextChunk)
  private
    FCompressionMethod : Byte;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
  end;

  TChunkPngInternationalTextChunk = class(TCustomDefinedChunkTextChunk)
  private
    FCompressionMethod : Byte;
    FCompressionFlag   : Byte;
    FLanguageString    : AnsiString;
    FTranslatedKeyword : string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
    property CompressionFlag: Byte read FCompressionFlag write FCompressionFlag;
    property LanguageString: AnsiString read FLanguageString write FLanguageString;
    property TranslatedKeyword: string read FTranslatedKeyword write FTranslatedKeyword;
  end;

  TUnknownPngChunk = class(TUnknownChunk)
  public
    constructor Create; override;
  end;

  TPngChunkList = class(TPersistent)
  private
    FChunks : array of TCustomChunk;
    function GetCount: Integer;
  protected
    function GetChunk(Index: Integer): TCustomChunk;
    procedure AssignTo(Dest: TPersistent); override;
  public
    destructor Destroy; override;

    procedure Add(Item: TCustomChunk);
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    function IndexOf(Item: TCustomChunk): Integer;
    procedure Remove(Item: TCustomChunk);

    property Count: Integer read GetCount;
    property Chunks[Index: Integer]: TCustomChunk read GetChunk; default;
  end;

procedure RegisterPngChunk(ChunkClass: TCustomChunkPngWithHeaderClass);
procedure RegisterPngChunks(ChunkClasses: array of TCustomChunkPngWithHeaderClass);
function FindPngChunkByChunkName(ChunkName: TChunkName): TCustomChunkPngWithHeaderClass;

implementation

uses
  DAV_GuiPngResourceStrings;

var
  GPngChunkClasses: array of TCustomChunkPngWithHeaderClass;


function IsPngChunkRegistered(ChunkClass: TCustomChunkPngWithHeaderClass): Boolean;
var
  ChunkClassIndex : Integer;
begin
 Result := False;
 for ChunkClassIndex := 0 to Length(GPngChunkClasses) - 1 do
  if GPngChunkClasses[ChunkClassIndex] = ChunkClass then
   begin
    Result := True;
    Exit;
   end;
end;

procedure RegisterPngChunk(ChunkClass: TCustomChunkPngWithHeaderClass);
begin
 Assert(IsPngChunkRegistered(ChunkClass) = False);
 SetLength(GPngChunkClasses, Length(GPngChunkClasses) + 1);
 GPngChunkClasses[Length(GPngChunkClasses) - 1] := ChunkClass;
end;

procedure RegisterPngChunks(ChunkClasses: array of TCustomChunkPngWithHeaderClass);
var
  ChunkClassIndex : Integer;
begin
 for ChunkClassIndex := 0 to Length(ChunkClasses) - 1
  do RegisterPngChunk(ChunkClasses[ChunkClassIndex]);
end;

function FindPngChunkByChunkName(ChunkName: TChunkName): TCustomChunkPngWithHeaderClass;
var
  ChunkClassIndex : Integer;
begin
 Result := nil;
 for ChunkClassIndex := 0 to Length(GPngChunkClasses) - 1 do
  if GPngChunkClasses[ChunkClassIndex].GetClassChunkName = ChunkName then
   begin
    Result := GPngChunkClasses[ChunkClassIndex];
    Exit;
   end;
end;


{ TCustomChunkPng }

constructor TCustomChunkPng.Create;
begin
 inherited;
 ChunkFlags := [cfSizeFirst, cfReversedByteOrder];
end;


{ TChunkPngImageHeader }

procedure TChunkPngImageHeader.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngImageHeader then
  with TChunkPngImageHeader(Dest) do
   begin
    FWidth             := Self.FWidth;
    FHeight            := Self.FHeight;
    FBitDepth          := Self.FBitDepth;
    FColorType         := Self.FColorType;
    FCompressionMethod := Self.FCompressionMethod;
    FFilterMethod      := Self.FFilterMethod;
    FInterlaceMethod   := Self.FInterlaceMethod;
   end
 else inherited;
end;

constructor TChunkPngImageHeader.Create;
begin
 inherited;

 ResetToDefault;
end;

function TChunkPngImageHeader.GetBytesPerRow: Integer;
begin
 case FColorType of
  ctGrayscale,
  ctIndexedColor   : Result := ((FWidth * FBitDepth + $7) and not $7) shr 3;
  ctGrayscaleAlpha : Result := 2 * (FBitDepth shr 3) * FWidth;
  ctTrueColor      : Result := 3 * (FBitDepth shr 3) * FWidth;
  ctTrueColorAlpha : Result := 4 * (FBitDepth shr 3) * FWidth;
  else raise EPngError.Create(RCStrUnknownColorType);
 end;
end;

class function TChunkPngImageHeader.GetClassChunkName: TChunkName;
begin
 Result := 'IHDR';
end;

function TChunkPngImageHeader.GetPixelByteSize: Integer;
begin
 case ColorType of
  ctGrayscale :
   if FBitDepth = 16
    then Result := 2
    else Result := 1;
  ctTrueColor : Result := 3 * FBitDepth div 8;
  ctIndexedColor : Result := 1;
  ctGrayscaleAlpha : Result := 2 * FBitDepth div 8;
  ctTrueColorAlpha : Result := 4 * FBitDepth div 8;
  else Result := 0;
 end;
end;

function TChunkPngImageHeader.GetHasPalette: Boolean;
begin
 Result := FColorType in [ctIndexedColor];
end;

procedure TChunkPngImageHeader.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 13
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read width
   FWidth := ReadSwappedCardinal(Stream);

   // read height
   FHeight := ReadSwappedCardinal(Stream);

   // read bit depth
   Read(FBitDepth, 1);

   // read Color type
   Read(FColorType, 1);

   // check consistency between Color type and bit depth
   case FColorType of
    ctGrayscale :
      if not (FBitDepth in [1, 2, 4, 8, 16]) then raise EPngError.Create(RCStrWrongBitdepth);
    ctTrueColor,
    ctGrayscaleAlpha,
    ctTrueColorAlpha :
      if not (FBitDepth in [8, 16]) then raise EPngError.Create(RCStrWrongBitdepth);
    ctIndexedColor :
      if not (FBitDepth in [1, 2, 4, 8]) then raise EPngError.Create(RCStrWrongBitdepth);
   end;

   // read compression method
   Read(FCompressionMethod, 1);

   // check for compression method
   if FCompressionMethod <> 0
    then raise EPngError.Create(RCStrUnsupportedCompressMethod);

   // read filter method
   Read(FFilterMethod, 1);

   // check for filter method
   if FFilterMethod <> fmAdaptiveFilter
    then raise EPngError.Create(RCStrUnsupportedFilterMethod);

   // read interlace method
   Read(FInterlaceMethod, 1);

   // check for interlace method
   if not (FInterlaceMethod in [imNone, imAdam7])
    then raise EPngError.Create(RCStrUnsupportedInterlaceMethod);
  end;
end;

procedure TChunkPngImageHeader.ResetToDefault;
begin
 FWidth             := 0;
 FHeight            := 0;
 FBitDepth          := 8;
 FColorType         := ctTrueColor;
 FCompressionMethod := 0;
 FFilterMethod      := fmAdaptiveFilter;
 FInterlaceMethod   := imNone;
end;

procedure TChunkPngImageHeader.SaveToStream(Stream: TStream);
begin
 FChunkSize := 13;

 inherited;

 with Stream do
  begin
   // write width
   WriteSwappedCardinal(Stream, FWidth);

   // write height
   WriteSwappedCardinal(Stream, FHeight);

   // write bit depth
   Write(FBitDepth, 1);

   // write Color type
   Write(FColorType, 1);

   // write compression method
   Write(FCompressionMethod, 1);

   // write filter method
   Write(FFilterMethod, 1);

   // write interlace method
   Write(FInterlaceMethod, 1);
  end;
end;


{ TCustomChunkPngWithHeader }

constructor TCustomChunkPngWithHeader.Create(Header: TChunkPngImageHeader);
begin
 if not (Header is TChunkPngImageHeader)
  then raise EPngError.Create(RCStrHeaderInvalid);

 FHeader := Header;
 inherited Create;
end;

procedure TCustomChunkPngWithHeader.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomChunkPngWithHeader then
  with TCustomChunkPngWithHeader(Dest) do
   begin
    FHeader.Assign(Self.FHeader);
   end
 else inherited;
end;

procedure TCustomChunkPngWithHeader.HeaderChanged;
begin
 // purely virtual, do nothing by default
end;


{ TChunkPngPalette }

procedure TChunkPngPalette.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngPalette then
  with TChunkPngPalette(Dest) do
   begin
    SetLength(FPaletteEntries, Length(Self.FPaletteEntries));
    Move(Self.FPaletteEntries[0], FPaletteEntries[0], Length(Self.FPaletteEntries) * SizeOf(TRGB24));
   end
 else inherited;
end;

class function TChunkPngPalette.GetClassChunkName: TChunkName;
begin
 Result := 'PLTE';
end;

function TChunkPngPalette.GetPaletteEntriesPointer: PPalette24;
begin
 Result := @FPaletteEntries[0];
end;

function TChunkPngPalette.GetPaletteEntry(Index: Integer): TRGB24;
begin
 if (Index >= 0) and (Index < Count)
  then Result := FPaletteEntries[Index]
  else raise EPngError.Create(RCStrIndexOutOfBounds);
end;

procedure TChunkPngPalette.SetPaletteEntry(Index: Integer; const Value: TRGB24);
begin
 if (Index >= 0) and (Index < Count)
  then FPaletteEntries[Index] := Value
  else raise EPngError.Create(RCStrIndexOutOfBounds);
end;

function TChunkPngPalette.GetCount: Integer;
begin
 Result := Length(FPaletteEntries);
end;

procedure TChunkPngPalette.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if ((Size - Position) mod SizeOf(TRGB24)) <> 0
    then raise EPngError.Create(RCStrIncompletePalette);

   SetLength(FPaletteEntries, (Size - Position) div SizeOf(TRGB24));

   Read(FPaletteEntries[0], Length(FPaletteEntries) * SizeOf(TRGB24));
  end;
end;

procedure TChunkPngPalette.PaletteEntriesChanged;
begin
 // nothing todo here yet
end;

procedure TChunkPngPalette.SaveToStream(Stream: TStream);
begin
 // determine chunk size
 FChunkSize := Length(FPaletteEntries) * SizeOf(TRGB24);

 inherited;

 Stream.Write(FPaletteEntries[0], FChunkSize);
end;

procedure TChunkPngPalette.SetCount(const Value: Integer);
begin
 if Value > 256
  then raise EPngError.Create(RCStrPaletteLimited);

 if Value <> Length(FPaletteEntries) then
  begin
   SetLength(FPaletteEntries, Value);
   PaletteEntriesChanged;
  end;
end;


{ TChunkPngTransparency }

procedure TChunkPngTransparency.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngTransparency then
  with TChunkPngTransparency(Dest) do
   begin
    FTransparency.Assign(Self.FTransparency);
   end
 else inherited;
end;

constructor TChunkPngTransparency.Create(Header: TChunkPngImageHeader);
begin
 inherited;
 case Header.ColorType of
  ctGrayscale    : FTransparency := TPngTransparencyFormat0.Create;
  ctTrueColor    : FTransparency := TPngTransparencyFormat2.Create;
  ctIndexedColor : FTransparency := TPngTransparencyFormat3.Create;
 end;
end;

destructor TChunkPngTransparency.Destroy;
begin
 if Assigned(FTransparency)
  then FreeAndNil(FTransparency);
 inherited;
end;

class function TChunkPngTransparency.GetClassChunkName: TChunkName;
begin
 Result := 'tRNS';
end;

procedure TChunkPngTransparency.HeaderChanged;
var
  OldTransparency : TCustomPngTransparency;
begin
 inherited;

 // store old transparency object
 OldTransparency := FTransparency;

 // change transparency object class
 case FHeader.ColorType of
  ctGrayscale     : if not (FTransparency is TPngTransparencyFormat0) then
                     begin
                      FTransparency := TPngTransparencyFormat0.Create;
                      if Assigned(OldTransparency) then
                       begin
                        FTransparency.Assign(OldTransparency);
                        FreeAndNil(OldTransparency);
                       end;
                     end;
  ctTrueColor     : if not (FTransparency is TPngTransparencyFormat2) then
                     begin
                      FTransparency := TPngTransparencyFormat2.Create;
                      if Assigned(OldTransparency) then
                       begin
                        FTransparency.Assign(OldTransparency);
                        FreeAndNil(OldTransparency);
                       end;
                     end;
  ctIndexedColor  : if not (FTransparency is TPngTransparencyFormat3) then
                     begin
                      FTransparency := TPngTransparencyFormat3.Create;
                      if Assigned(OldTransparency) then
                       begin
                        FTransparency.Assign(OldTransparency);
                        FreeAndNil(OldTransparency);
                       end;
                     end;
  else if Assigned(FTransparency) then FreeAndNil(FTransparency);

 end;
end;

procedure TChunkPngTransparency.LoadFromStream(Stream: TStream);
begin
 inherited;

 if Assigned(FTransparency)
  then FTransparency.LoadFromStream(Stream);
end;

procedure TChunkPngTransparency.SaveToStream(Stream: TStream);
begin
 if Assigned(FTransparency)
  then FChunkSize := FTransparency.ChunkSize
  else FChunkSize := 0;

 inherited;

 // check consistency
 case FHeader.ColorType of
  ctGrayscale    : if not (FTransparency is TPngTransparencyFormat0)
                    then raise EPngError.Create(RCStrWrongTransparencyFormat);
  ctTrueColor    : if not (FTransparency is TPngTransparencyFormat2)
                    then raise EPngError.Create(RCStrWrongTransparencyFormat);
  ctIndexedColor : if not (FTransparency is TPngTransparencyFormat3)
                    then raise EPngError.Create(RCStrWrongTransparencyFormat);
 end;

 if Assigned(FTransparency)
  then FTransparency.SaveToStream(Stream);
end;


{ TPngTransparencyFormat0 }

procedure TPngTransparencyFormat0.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngTransparencyFormat0 then
  with TPngTransparencyFormat0(Dest) do
   begin
    FGraySampleValue := Self.FGraySampleValue;
   end
 else inherited;
end;

function TPngTransparencyFormat0.GetChunkSize: Integer;
begin
 Result := 2;
end;

procedure TPngTransparencyFormat0.LoadFromStream(Stream: TStream);
begin
 inherited;

 FGraySampleValue := ReadSwappedWord(Stream);
end;

procedure TPngTransparencyFormat0.SaveToStream(Stream: TStream);
begin
 inherited;

 WriteSwappedWord(Stream, FGraySampleValue);
end;


{ TPngTransparencyFormat2 }

procedure TPngTransparencyFormat2.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngTransparencyFormat2 then
  with TPngTransparencyFormat2(Dest) do
   begin
    FRedSampleValue := Self.FRedSampleValue;
    FBlueSampleValue := Self.FBlueSampleValue;
    FGreenSampleValue := Self.FGreenSampleValue;
   end
 else inherited;
end;

function TPngTransparencyFormat2.GetChunkSize: Integer;
begin
 Result := 6;
end;

procedure TPngTransparencyFormat2.LoadFromStream(Stream: TStream);
begin
 inherited;

 FRedSampleValue  := ReadSwappedWord(Stream);
 FBlueSampleValue  := ReadSwappedWord(Stream);
 FGreenSampleValue  := ReadSwappedWord(Stream);
end;

procedure TPngTransparencyFormat2.SaveToStream(Stream: TStream);
begin
 inherited;

 WriteSwappedWord(Stream, FRedSampleValue);
 WriteSwappedWord(Stream, FBlueSampleValue);
 WriteSwappedWord(Stream, FGreenSampleValue);
end;


{ TPngTransparencyFormat3 }

procedure TPngTransparencyFormat3.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngTransparencyFormat3 then
  with TPngTransparencyFormat3(Dest) do
   begin
    SetLength(FTransparency, Length(Self.FTransparency));
    Move(Self.FTransparency[0], FTransparency, Length(FTransparency));
   end
 else inherited;
end;

function TPngTransparencyFormat3.GetChunkSize: Integer;
begin
 Result := Count;
end;

function TPngTransparencyFormat3.GetCount: Integer;
begin
 Result := Length(FTransparency);
end;

function TPngTransparencyFormat3.GetTransparency(Index: Integer): Byte;
begin
 if (Index >= 0) and (Index < Count)
  then Result := FTransparency[Index]
  else raise EPngError.Create(RCStrIndexOutOfBounds);
end;

procedure TPngTransparencyFormat3.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   SetLength(FTransparency, Size - Position);
   Read(FTransparency[0], Length(FTransparency));
  end;
end;

procedure TPngTransparencyFormat3.SaveToStream(Stream: TStream);
begin
 inherited;

 Stream.Write(FTransparency[0], Length(FTransparency));
end;


{ TChunkPngPhysicalPixelDimensions }

procedure TChunkPngPhysicalPixelDimensions.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngPhysicalPixelDimensions then
  with TChunkPngPhysicalPixelDimensions(Dest) do
   begin
    FPixelsPerUnitX := Self.FPixelsPerUnitX;
    FPixelsPerUnitY := Self.FPixelsPerUnitY;
    FUnit           := Self.FUnit;
   end
 else inherited;
end;

class function TChunkPngPhysicalPixelDimensions.GetClassChunkName: TChunkName;
begin
 Result := 'pHYs';
end;

procedure TChunkPngPhysicalPixelDimensions.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 9
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read pixels per unit, X axis
   FPixelsPerUnitX := ReadSwappedCardinal(Stream);

   // read pixels per unit, Y axis
   FPixelsPerUnitY := ReadSwappedCardinal(Stream);

   // read unit
   Read(FUnit, 1);
  end;
end;

procedure TChunkPngPhysicalPixelDimensions.SaveToStream(Stream: TStream);
begin
 FChunkSize := 9;

 inherited;

 with Stream do
  begin
   // write pixels per unit, X axis
   WriteSwappedCardinal(Stream, FPixelsPerUnitX);

   // write pixels per unit, Y axis
   WriteSwappedCardinal(Stream, FPixelsPerUnitY);

   // write unit
   Write(FUnit, 1);
  end;
end;


{ TChunkPngPhysicalScale }

procedure TChunkPngPhysicalScale.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngPhysicalScale then
  with TChunkPngPhysicalScale(Dest) do
   begin
    FUnitSpecifier  := Self.FUnitSpecifier;
    FUnitsPerPixelX := Self.FUnitsPerPixelX;
    FUnitsPerPixelY := Self.FUnitsPerPixelY;
   end
 else inherited;
end;

class function TChunkPngPhysicalScale.GetClassChunkName: TChunkName;
begin
 Result := 'sCAL';
end;

procedure TChunkPngPhysicalScale.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 4
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read unit specifier
   Read(FUnitSpecifier, 1);

   // yet todo, see http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.9
  end;
end;

procedure TChunkPngPhysicalScale.SaveToStream(Stream: TStream);
begin
 inherited;

 raise EPngError.Create(RCStrNotYetImplemented);
 // yet todo, see http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.9
end;


{ TChunkPngImageOffset }

procedure TChunkPngImageOffset.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngImageOffset then
  with TChunkPngImageOffset(Dest) do
   begin
    FImagePositionX := Self.FImagePositionX;
    FImagePositionY := Self.FImagePositionY;
    FUnitSpecifier  := Self.FUnitSpecifier;
   end
 else inherited;
end;

class function TChunkPngImageOffset.GetClassChunkName: TChunkName;
begin
 Result := 'oFFs';
end;

procedure TChunkPngImageOffset.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 9
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read image positions
   FImagePositionX := ReadSwappedCardinal(Stream);
   FImagePositionY := ReadSwappedCardinal(Stream);

   // read unit specifier
   Read(FUnitSpecifier, 1);
  end;
end;

procedure TChunkPngImageOffset.SaveToStream(Stream: TStream);
begin
 FChunkSize := 9;

 inherited;

 // read image positions
 WriteSwappedCardinal(Stream, FImagePositionX);
 WriteSwappedCardinal(Stream, FImagePositionY);

 // read unit specifier
 Write(FUnitSpecifier, 1);
end;


{ TChunkPngPixelCalibrator }

procedure TChunkPngPixelCalibrator.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngPixelCalibrator then
  with TChunkPngPixelCalibrator(Dest) do
   begin
    FCalibratorName    := Self.FCalibratorName;
    FOriginalZeroes[0] := Self.FOriginalZeroes[0];
    FOriginalZeroes[1] := Self.FOriginalZeroes[1];
    FEquationType      := Self.FEquationType;
    FNumberOfParams    := Self.FNumberOfParams;
    FUnitName          := Self.FUnitName;
   end
 else inherited;
end;

class function TChunkPngPixelCalibrator.GetClassChunkName: TChunkName;
begin
 Result := 'pCAL';
end;

procedure TChunkPngPixelCalibrator.LoadFromStream(Stream: TStream);
var
  Index      : Integer;
  ParamIndex : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FCalibratorName, 80);
   while (Position < Size) do
    begin
     Read(FCalibratorName[Index], SizeOf(Byte));
     if FCalibratorName[Index] = #0 then
      begin
       SetLength(FCalibratorName, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read original zeros
   FOriginalZeroes[0] := ReadSwappedCardinal(Stream);
   FOriginalZeroes[1] := ReadSwappedCardinal(Stream);

   // read equation type
   Stream.Read(FEquationType, 1);

   // read number of parameters
   Stream.Read(FNumberOfParams, 1);

   // read keyword
   Index := 1;
   SetLength(FUnitName, 80);
   while (Position < Size) do
    begin
     Read(FUnitName[Index], SizeOf(Byte));
     if FUnitName[Index] = #0 then
      begin
       SetLength(FUnitName, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   for ParamIndex := 0 to FNumberOfParams - 2 do
    begin
     // yet todo
    end;
  end;
end;

procedure TChunkPngPixelCalibrator.SaveToStream(Stream: TStream);
begin
  inherited;

end;


{ TChunkPngTextChunk }

class function TChunkPngTextChunk.GetClassChunkName: TChunkName;
begin
 Result := 'tEXt';
end;

procedure TChunkPngTextChunk.LoadFromStream(Stream: TStream);
var
  Index : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FKeyword, 80);
   while (Position < Size) do
    begin
     Read(FKeyword[Index], SizeOf(Byte));
     if FKeyword[Index] = #0 then
      begin
       SetLength(FKeyword, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read text
   Index := 1;
   SetLength(FText, Size - Position);
   while (Position < Size) do
    begin
     Read(FText[Index], SizeOf(Byte));
     Inc(Index);
    end;
  end;
end;

procedure TChunkPngTextChunk.SaveToStream(Stream: TStream);
var
  Temp  : Byte;
begin
 FChunkSize := Length(FKeyword) + Length(FText) + 1;

 inherited;

 with Stream do
  begin
   // write keyword
   Write(FKeyword[1], Length(FKeyword));

   // write separator
   Temp := 0;
   Write(Temp, 1);

   // write text
   Write(FText[1], Length(FText));
  end;
end;


{ TChunkPngCompressedTextChunk }

procedure TChunkPngCompressedTextChunk.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngCompressedTextChunk then
  with TChunkPngCompressedTextChunk(Dest) do
   begin
    FCompressionMethod := Self.FCompressionMethod;
   end
 else inherited;
end;

class function TChunkPngCompressedTextChunk.GetClassChunkName: TChunkName;
begin
 Result := 'zTXt';
end;

procedure TChunkPngCompressedTextChunk.LoadFromStream(Stream: TStream);
var
  DataIn     : Pointer;
  DataInSize : Integer;
  Output     : TMemoryStream;
  Index      : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FKeyword, 80);
   while (Position < Size) do
    begin
     Read(FKeyword[Index], SizeOf(Byte));
     if FKeyword[Index] = #0 then
      begin
       SetLength(FKeyword, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read compression method
   Read(FCompressionMethod, SizeOf(Byte));

   // read text
   if FCompressionMethod = 0 then
    begin
     DataInSize := Size - Position;
     GetMem(DataIn, DataInSize);
     try
      Read(DataIn^, DataInSize);

      Output := TMemoryStream.Create;
      try
       ZDecompress(DataIn, DataInSize, Output);
       SetLength(FText, Output.Size);
       Move(Output.Memory^, FText[1], Output.Size);
      finally
       FreeAndNil(Output);
      end;
     finally
      Dispose(DataIn);
     end;
    end;
  end;
end;

procedure TChunkPngCompressedTextChunk.SaveToStream(Stream: TStream);
var
  OutputStream : TMemoryStream;
  Temp         : Byte;
begin
 OutputStream := TMemoryStream.Create;
 try
  // compress text
  ZCompress(@FText[1], Length(FText), OutputStream);

  // calculate chunk size
  FChunkSize := Length(FKeyword) + OutputStream.Size + 1;

  inherited;

  with Stream do
   begin
    // write keyword
    Write(FKeyword[1], Length(FKeyword));

    // write separator
    Temp := 0;
    Write(Temp, 1);

    // write text
    Write(FText[1], Length(FText));

    // write compression method
    Write(FCompressionMethod, SizeOf(Byte));

    // write text
    Write(OutputStream.Memory^, OutputStream.Size);
   end;
 finally
  FreeAndNil(OutputStream);
 end;
end;


{ TChunkPngInternationalTextChunk }

procedure TChunkPngInternationalTextChunk.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngInternationalTextChunk then
  with TChunkPngInternationalTextChunk(Dest) do
   begin
    FCompressionMethod := Self.FCompressionMethod;
    FCompressionFlag   := Self.FCompressionFlag;
    FLanguageString    := Self.FLanguageString;
    FTranslatedKeyword := Self.FTranslatedKeyword;
   end
 else inherited;
end;

class function TChunkPngInternationalTextChunk.GetClassChunkName: TChunkName;
begin
 Result := 'iTXt';
end;

procedure TChunkPngInternationalTextChunk.LoadFromStream(Stream: TStream);
var
  Index : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FKeyword, 80);
   while (Position < Size) do
    begin
     Read(FKeyword[Index], SizeOf(Byte));
     if FKeyword[Index] = #0 then
      begin
       SetLength(FKeyword, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read compression flag
   Read(FCompressionFlag, SizeOf(Byte));

   // read compression method
   Read(FCompressionMethod, SizeOf(Byte));

   // read language string
   Index := 1;
   SetLength(FLanguageString, 10);
   while (Position < Size) do
    begin
     Read(FLanguageString[Index], SizeOf(Byte));
     if FLanguageString[Index] = #0 then
      begin
       SetLength(FLanguageString, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // yet todo!
   Exit;
  end;
end;

procedure TChunkPngInternationalTextChunk.SaveToStream(Stream: TStream);
begin
 raise EPngError.Create(RCStrNotYetImplemented);
end;


{ TChunkPngImageData }

constructor TChunkPngImageData.Create(Header: TChunkPngImageHeader);
begin
 inherited;
 FData := TMemoryStream.Create;
end;

destructor TChunkPngImageData.Destroy;
begin
 FreeAndNil(FData);
 inherited;
end;

procedure TChunkPngImageData.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngImageData then
  with TChunkPngImageData(Dest) do
   begin
    FData.Seek(0, soFromBeginning);
    Self.FData.Seek(0, soFromBeginning);
    FData.CopyFrom(Self.FData, Self.FData.Size);
    FData.Seek(0, soFromBeginning);
   end
 else inherited;
end;

class function TChunkPngImageData.GetClassChunkName: TChunkName;
begin
 Result := 'IDAT';
end;

procedure TChunkPngImageData.LoadFromStream(Stream: TStream);
begin
 inherited;

 FData.CopyFrom(Stream, Stream.Size - Stream.Position);
end;

procedure TChunkPngImageData.SaveToStream(Stream: TStream);
begin
 FChunkSize := FData.Size;
 inherited;

 FData.Seek(0, soFromBeginning);
 Stream.CopyFrom(FData, FChunkSize);
end;


{ TChunkPngTime }

procedure TChunkPngTime.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngTime then
  with TChunkPngTime(Dest) do
   begin
    FYear   := Self.FYear;
    FMonth  := Self.FMonth;
    FDay    := Self.FDay;
    FHour   := Self.FHour;
    FMinute := Self.FMinute;
    FSecond := Self.FSecond;
   end
 else inherited;
end;

class function TChunkPngTime.GetClassChunkName: TChunkName;
begin
 Result := 'tIME';
end;

function TChunkPngTime.GetModifiedDateTime: TDateTime;
begin
 Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
end;

procedure TChunkPngTime.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 7
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read year
   FYear := ReadSwappedWord(Stream);

   // read month
   Read(FMonth, SizeOf(Byte));

   // read day
   Read(FDay, SizeOf(Byte));

   // read hour
   Read(FHour, SizeOf(Byte));

   // read minute
   Read(FMinute, SizeOf(Byte));

   // read second
   Read(FSecond, SizeOf(Byte));
  end;
end;

procedure TChunkPngTime.SaveToStream(Stream: TStream);
begin
 FChunkSize := 7;

 inherited;

 with Stream do
  begin
   // write year
   WriteSwappedWord(Stream, FYear);

   // write month
   Write(FMonth, SizeOf(Byte));

   // write day
   Write(FDay, SizeOf(Byte));

   // write hour
   Write(FHour, SizeOf(Byte));

   // write minute
   Write(FMinute, SizeOf(Byte));

   // write second
   Write(FSecond, SizeOf(Byte));
  end;
end;

procedure TChunkPngTime.SetModifiedDateTime(const Value: TDateTime);
var
  mnth : Word;
  day  : Word;
  hour : Word;
  min  : Word;
  sec  : Word;
  msec : Word;
begin
 DecodeDate(Value, FYear, mnth, day);
 FMonth := mnth;
 FDay := day;
 DecodeTime(Value, hour, min, sec, msec);
 FHour := hour;
 FMinute := min;
 FSecond := sec;
end;


{ TChunkPngEmbeddedIccProfile }

procedure TChunkPngEmbeddedIccProfile.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngEmbeddedIccProfile then
  with TChunkPngEmbeddedIccProfile(Dest) do
   begin
    FProfileName       := Self.FProfileName;
    FCompressionMethod := Self.FCompressionMethod;
   end
 else inherited;
end;

class function TChunkPngEmbeddedIccProfile.GetClassChunkName: TChunkName;
begin
 Result := 'iCCP';
end;

procedure TChunkPngEmbeddedIccProfile.LoadFromStream(Stream: TStream);
var
  Index : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FProfileName, 80);
   while (Position < Size) do
    begin
     Read(FProfileName[Index], SizeOf(Byte));
     if FProfileName[Index] = #0 then
      begin
       SetLength(FProfileName, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read compression method
   Read(FCompressionMethod, 1);

   // not yet completed
  end;
end;

procedure TChunkPngEmbeddedIccProfile.SaveToStream(Stream: TStream);
var
  Temp  : Byte;
begin
 FChunkSize := Length(FProfileName) + 2;

 inherited;

 with Stream do
  begin
   // write keyword
   Write(FProfileName[1], Length(FProfileName));

   // write separator
   Temp := 0;
   Write(Temp, 1);

   // write compression method
   Write(FCompressionMethod, 1);
  end;
end;


{ TChunkPngGamma }

procedure TChunkPngGamma.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngGamma then
  with TChunkPngGamma(Dest) do
   begin
    FGamma := Self.FGamma;
   end
 else inherited;
end;

class function TChunkPngGamma.GetClassChunkName: TChunkName;
begin
 Result := 'gAMA';
end;

function TChunkPngGamma.GetGammaAsSingle: Single;
begin
 Result := FGamma * 1E-5;
end;

procedure TChunkPngGamma.SetGammaAsSingle(const Value: Single);
begin
 FGamma := Round(Value * 1E5);
end;

procedure TChunkPngGamma.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 4
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read gamma
   FGamma := ReadSwappedCardinal(Stream);
  end;
end;

procedure TChunkPngGamma.SaveToStream(Stream: TStream);
begin
 FChunkSize := 4;

 inherited;

 with Stream do
  begin
   // write gamma
   WriteSwappedCardinal(Stream, FGamma);
  end;
end;


{ TChunkPngStandardColorSpaceRGB }

procedure TChunkPngStandardColorSpaceRGB.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngStandardColorSpaceRGB then
  with TChunkPngStandardColorSpaceRGB(Dest) do
   begin
    FRenderingIntent := Self.FRenderingIntent;
   end
 else inherited;
end;

class function TChunkPngStandardColorSpaceRGB.GetClassChunkName: TChunkName;
begin
 Result := 'sRGB';
end;

procedure TChunkPngStandardColorSpaceRGB.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 1
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read rendering intent
   Read(FRenderingIntent, SizeOf(Byte));
  end;
end;

procedure TChunkPngStandardColorSpaceRGB.SaveToStream(Stream: TStream);
begin
 FChunkSize := 1;

 inherited;

 // write rendering intent
 Stream.Write(FRenderingIntent, SizeOf(Byte));
end;


{ TChunkPngPrimaryChromaticities }

class function TChunkPngPrimaryChromaticities.GetClassChunkName: TChunkName;
begin
 Result := 'cHRM';
end;

procedure TChunkPngPrimaryChromaticities.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngPrimaryChromaticities then
  with TChunkPngPrimaryChromaticities(Dest) do
   begin
    FWhiteX := Self.FWhiteX;
    FWhiteY := Self.FWhiteY;
    FRedX   := Self.FRedX;
    FRedY   := Self.FRedY;
    FGreenX := Self.FGreenX;
    FGreenY := Self.FGreenY;
    FBlueX  := Self.FBlueX;
    FBlueY  := Self.FBlueY;
   end
 else inherited;
end;

function TChunkPngPrimaryChromaticities.GetBlueX: Single;
begin
 Result := FBlueX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetBlueY: Single;
begin
 Result := FBlueY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetGreenX: Single;
begin
 Result := FGreenX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetGreenY: Single;
begin
 Result := FGreenY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetRedX: Single;
begin
 Result := FRedX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetRedY: Single;
begin
 Result := FRedY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetWhiteX: Single;
begin
 Result := FWhiteX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetWhiteY: Single;
begin
 Result := FWhiteY * 1E-6;
end;

procedure TChunkPngPrimaryChromaticities.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 32
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read white point x
   FWhiteX := ReadSwappedCardinal(Stream);

   // read white point y
   FWhiteY := ReadSwappedCardinal(Stream);

   // read red x
   FRedX := ReadSwappedCardinal(Stream);

   // read red y
   FRedY := ReadSwappedCardinal(Stream);

   // read green x
   FGreenX := ReadSwappedCardinal(Stream);

   // read green y
   FGreenY := ReadSwappedCardinal(Stream);

   // read blue x
   FBlueX := ReadSwappedCardinal(Stream);

   // read blue y
   FBlueY := ReadSwappedCardinal(Stream);
  end;
end;

procedure TChunkPngPrimaryChromaticities.SaveToStream(Stream: TStream);
begin
 FChunkSize := 32;

 inherited;


 with Stream do
  begin
   // write white point x
   WriteSwappedCardinal(Stream, FWhiteX);

   // write white point y
   WriteSwappedCardinal(Stream, FWhiteY);

   // write red x
   WriteSwappedCardinal(Stream, FRedX);

   // write red y
   WriteSwappedCardinal(Stream, FRedY);

   // write green x
   WriteSwappedCardinal(Stream, FGreenX);

   // write green y
   WriteSwappedCardinal(Stream, FGreenY);

   // write blue x
   WriteSwappedCardinal(Stream, FBlueX);

   // write blue y
   WriteSwappedCardinal(Stream, FBlueY);
  end;
end;

procedure TChunkPngPrimaryChromaticities.SetBlueX(const Value: Single);
begin
 FBlueX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetBlueY(const Value: Single);
begin
 FBlueY := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetGreenX(const Value: Single);
begin
 FGreenX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetGreenY(const Value: Single);
begin
 FGreenY := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetRedX(const Value: Single);
begin
 FRedX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetRedY(const Value: Single);
begin
 FRedY := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetWhiteX(const Value: Single);
begin
 FWhiteX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetWhiteY(const Value: Single);
begin
 FWhiteY := Round(Value * 1E6);
end;


{ TPngSignificantBitsFormat0 }

procedure TPngSignificantBitsFormat0.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngSignificantBitsFormat0 then
  with TPngSignificantBitsFormat0(Dest) do
   begin
    FGrayBits := Self.FGrayBits;
   end
 else inherited;
end;

class function TPngSignificantBitsFormat0.GetChunkSize: Integer;
begin
 Result := 1;
end;

procedure TPngSignificantBitsFormat0.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FGrayBits, 1);
end;

procedure TPngSignificantBitsFormat0.SaveToStream(Stream: TStream);
begin
 Stream.Write(FGrayBits, 1);
end;


{ TPngSignificantBitsFormat23 }

procedure TPngSignificantBitsFormat23.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngSignificantBitsFormat23 then
  with TPngSignificantBitsFormat23(Dest) do
   begin
    FRedBits   := Self.FRedBits;
    FBlueBits  := Self.FBlueBits;
    FGreenBits := Self.FGreenBits;
   end
 else inherited;
end;

class function TPngSignificantBitsFormat23.GetChunkSize: Integer;
begin
 Result := 3;
end;

procedure TPngSignificantBitsFormat23.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FRedBits, 1);
 Stream.Read(FGreenBits, 1);
 Stream.Read(FBlueBits, 1);
end;

procedure TPngSignificantBitsFormat23.SaveToStream(Stream: TStream);
begin
 Stream.Write(FRedBits, 1);
 Stream.Write(FGreenBits, 1);
 Stream.Write(FBlueBits, 1);
end;


{ TPngSignificantBitsFormat4 }

procedure TPngSignificantBitsFormat4.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngSignificantBitsFormat4 then
  with TPngSignificantBitsFormat4(Dest) do
   begin
    FGrayBits  := Self.FGrayBits;
    FAlphaBits := Self.FAlphaBits;
   end
 else inherited;
end;

class function TPngSignificantBitsFormat4.GetChunkSize: Integer;
begin
 Result := 2;
end;

procedure TPngSignificantBitsFormat4.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FGrayBits, 1);
 Stream.Read(FAlphaBits, 1);
end;

procedure TPngSignificantBitsFormat4.SaveToStream(Stream: TStream);
begin
 Stream.Write(FGrayBits, 1);
 Stream.Write(FAlphaBits, 1);
end;


{ TPngSignificantBitsFormat6 }

procedure TPngSignificantBitsFormat6.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngSignificantBitsFormat6 then
  with TPngSignificantBitsFormat6(Dest) do
   begin
    FRedBits   := Self.FRedBits;
    FBlueBits  := Self.FBlueBits;
    FGreenBits := Self.FGreenBits;
    FAlphaBits := Self.FAlphaBits;
   end
 else inherited;
end;

class function TPngSignificantBitsFormat6.GetChunkSize: Integer;
begin
 Result := 4;
end;

procedure TPngSignificantBitsFormat6.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FRedBits, 1);
 Stream.Read(FGreenBits, 1);
 Stream.Read(FBlueBits, 1);
 Stream.Read(FAlphaBits, 1);
end;

procedure TPngSignificantBitsFormat6.SaveToStream(Stream: TStream);
begin
 Stream.Write(FRedBits, 1);
 Stream.Write(FGreenBits, 1);
 Stream.Write(FBlueBits, 1);
 Stream.Write(FAlphaBits, 1);
end;


{ TChunkPngSignificantBits }

constructor TChunkPngSignificantBits.Create(Header: TChunkPngImageHeader);
begin
 inherited;

 case Header.ColorType of
  ctGrayscale      : FSignificantBits := TPngSignificantBitsFormat0.Create;
  ctTrueColor,
  ctIndexedColor   : FSignificantBits := TPngSignificantBitsFormat23.Create;
  ctGrayscaleAlpha : FSignificantBits := TPngSignificantBitsFormat4.Create;
  ctTrueColorAlpha : FSignificantBits := TPngSignificantBitsFormat6.Create;
 end;
end;

destructor TChunkPngSignificantBits.Destroy;
begin
 if Assigned(FSignificantBits)
  then FreeAndNil(FSignificantBits);

 inherited;
end;

procedure TChunkPngSignificantBits.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngSignificantBits then
  with TChunkPngSignificantBits(Dest) do
   begin
    FSignificantBits.Assign(Self.FSignificantBits);
   end
 else inherited;
end;

class function TChunkPngSignificantBits.GetClassChunkName: TChunkName;
begin
 Result := 'sBIT';
end;

procedure TChunkPngSignificantBits.HeaderChanged;
var
  OldSignificantBits : TCustomPngSignificantBits;
begin
 inherited;

 // store old SignificantBits object
 OldSignificantBits := FSignificantBits;

 // change SignificantBits object class
 case FHeader.ColorType of
  ctGrayscale :
   if not (FSignificantBits is TPngSignificantBitsFormat0) then
    begin
     FSignificantBits := TPngSignificantBitsFormat0.Create;
     if Assigned(OldSignificantBits) then
      begin
       FSignificantBits.Assign(OldSignificantBits);
       FreeAndNil(OldSignificantBits);
      end;
    end;
  ctTrueColor, ctIndexedColor :
   if not (FSignificantBits is TPngSignificantBitsFormat23) then
    begin
     FSignificantBits := TPngSignificantBitsFormat23.Create;
     if Assigned(OldSignificantBits) then
      begin
       FSignificantBits.Assign(OldSignificantBits);
       FreeAndNil(OldSignificantBits);
      end;
    end;
   ctTrueColorAlpha :
   if not (FSignificantBits is TPngSignificantBitsFormat4) then
    begin
     FSignificantBits := TPngSignificantBitsFormat4.Create;
     if Assigned(OldSignificantBits) then
      begin
       FSignificantBits.Assign(OldSignificantBits);
       FreeAndNil(OldSignificantBits);
      end;
    end;
  ctGrayscaleAlpha :
   if not (FSignificantBits is TPngSignificantBitsFormat6) then
    begin
     FSignificantBits := TPngSignificantBitsFormat6.Create;
     if Assigned(OldSignificantBits) then
      begin
       FSignificantBits.Assign(OldSignificantBits);
       FreeAndNil(OldSignificantBits);
      end;
    end;
  else if Assigned(FSignificantBits) then FreeAndNil(FSignificantBits);
 end;
end;

procedure TChunkPngSignificantBits.LoadFromStream(Stream: TStream);
begin
 inherited;

 if Assigned(FSignificantBits) then
  begin
   if Stream.Size < FSignificantBits.ChunkSize
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   FSignificantBits.LoadFromStream(Stream);
  end;
end;

procedure TChunkPngSignificantBits.SaveToStream(Stream: TStream);
begin
 // determine chunk size
 if Assigned(FSignificantBits)
  then FChunkSize := FSignificantBits.GetChunkSize
  else FChunkSize := 0;

 inherited;

 if Assigned(FSignificantBits)
  then FSignificantBits.SaveToStream(Stream);
end;


{ TPngBackgroundColorFormat04 }

procedure TPngBackgroundColorFormat04.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngBackgroundColorFormat04 then
  with TPngBackgroundColorFormat04(Dest) do
   begin
    FGraySampleValue := Self.FGraySampleValue;
   end
 else inherited;
end;

class function TPngBackgroundColorFormat04.GetChunkSize: Integer;
begin
 Result := 2;
end;

procedure TPngBackgroundColorFormat04.LoadFromStream(Stream: TStream);
begin
 FGraySampleValue := ReadSwappedWord(Stream);
end;

procedure TPngBackgroundColorFormat04.SaveToStream(Stream: TStream);
begin
 WriteSwappedWord(Stream, FGraySampleValue);
end;


{ TPngBackgroundColorFormat26 }

procedure TPngBackgroundColorFormat26.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngBackgroundColorFormat26 then
  with TPngBackgroundColorFormat26(Dest) do
   begin
    FRedSampleValue := Self.FRedSampleValue;
    FBlueSampleValue := Self.FBlueSampleValue;
    FGreenSampleValue := Self.FGreenSampleValue;
   end
 else inherited;
end;

class function TPngBackgroundColorFormat26.GetChunkSize: Integer;
begin
 Result := 6;
end;

procedure TPngBackgroundColorFormat26.LoadFromStream(Stream: TStream);
begin
 FRedSampleValue := ReadSwappedWord(Stream);
 FGreenSampleValue := ReadSwappedWord(Stream);
 FBlueSampleValue := ReadSwappedWord(Stream);
end;

procedure TPngBackgroundColorFormat26.SaveToStream(Stream: TStream);
begin
 WriteSwappedWord(Stream, FRedSampleValue);
 WriteSwappedWord(Stream, FGreenSampleValue);
 WriteSwappedWord(Stream, FBlueSampleValue);
end;


{ TPngBackgroundColorFormat3 }

procedure TPngBackgroundColorFormat3.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngBackgroundColorFormat3 then
  with TPngBackgroundColorFormat3(Dest) do
   begin
    FIndex := Self.FIndex;
   end
 else inherited;
end;

class function TPngBackgroundColorFormat3.GetChunkSize: Integer;
begin
 Result := 1;
end;

procedure TPngBackgroundColorFormat3.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FIndex, 1);
end;

procedure TPngBackgroundColorFormat3.SaveToStream(Stream: TStream);
begin
 Stream.Write(FIndex, 1);
end;


{ TChunkPngBackgroundColor }

procedure TChunkPngBackgroundColor.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngBackgroundColor then
  with TChunkPngBackgroundColor(Dest) do
   begin
    FBackground.Assign(Self.FBackground);
   end
 else inherited;
end;

constructor TChunkPngBackgroundColor.Create(Header: TChunkPngImageHeader);
begin
 inherited;

 case Header.ColorType of
  ctGrayscale, ctGrayscaleAlpha : FBackground := TPngBackgroundColorFormat04.Create;
  ctTrueColor, ctTrueColorAlpha: FBackground := TPngBackgroundColorFormat26.Create;
  ctIndexedColor: FBackground := TPngBackgroundColorFormat3.Create;
 end;
end;

destructor TChunkPngBackgroundColor.Destroy;
begin
 if Assigned(FBackground)
  then FreeAndNil(FBackground);
 inherited;
end;

class function TChunkPngBackgroundColor.GetClassChunkName: TChunkName;
begin
 Result := 'bKGD';
end;

procedure TChunkPngBackgroundColor.HeaderChanged;
var
  OldBackground : TCustomPngBackgroundColor;
begin
 inherited;

 // store old background object
 OldBackground := FBackground;

 // change background object class
 case FHeader.ColorType of
  ctGrayscale, ctGrayscaleAlpha :
   if not (FBackground is TPngBackgroundColorFormat04) then
    begin
     FBackground := TPngBackgroundColorFormat04.Create;
     if Assigned(OldBackground) then
      begin
       FBackground.Assign(OldBackground);
       FreeAndNil(OldBackground);
      end;
    end;
  ctTrueColor, ctTrueColorAlpha :
   if not (FBackground is TPngBackgroundColorFormat26) then
    begin
     FBackground := TPngBackgroundColorFormat26.Create;
     if Assigned(OldBackground) then
      begin
       FBackground.Assign(OldBackground);
       FreeAndNil(OldBackground);
      end;
    end;
  ctIndexedColor :
   if not (FBackground is TPngBackgroundColorFormat3) then
    begin
     FBackground := TPngBackgroundColorFormat3.Create;
     if Assigned(OldBackground) then
      begin
       FBackground.Assign(OldBackground);
       FreeAndNil(OldBackground);
      end;
    end;
  else if Assigned(FBackground) then FreeAndNil(FBackground);

 end;
end;

procedure TChunkPngBackgroundColor.LoadFromStream(Stream: TStream);
begin
 inherited;

 if Assigned(FBackground) then
  begin
   if Stream.Size < FBackground.ChunkSize
    then raise EPngError.Create(RCStrChunkSizeTooSmall);
  end;
end;

procedure TChunkPngBackgroundColor.SaveToStream(Stream: TStream);
begin
 // determine chunk size
 if Assigned(FBackground)
  then FChunkSize := FBackground.GetChunkSize
  else FChunkSize := 0;

 inherited;

 if Assigned(FBackground)
  then FBackground.SaveToStream(Stream);
end;


{ TChunkPngImageHistogram }

class function TChunkPngImageHistogram.GetClassChunkName: TChunkName;
begin
 Result := 'hIST';
end;

procedure TChunkPngImageHistogram.LoadFromStream(Stream: TStream);
begin
 inherited;

 // yet todo
end;

procedure TChunkPngImageHistogram.SaveToStream(Stream: TStream);
begin
 inherited;


 raise Exception.Create(RCStrNotYetImplemented);
 // yet todo
end;


{ TChunkPngSuggestedPalette }

class function TChunkPngSuggestedPalette.GetClassChunkName: TChunkName;
begin

end;

procedure TChunkPngSuggestedPalette.LoadFromStream(Stream: TStream);
begin
  inherited;

end;

procedure TChunkPngSuggestedPalette.SaveToStream(Stream: TStream);
begin
  inherited;

end;


{ TUnknownPngChunk }

constructor TUnknownPngChunk.Create;
begin
 inherited;
 ChunkFlags := [cfSizeFirst, cfReversedByteOrder];
end;


{ TPngChunkList }

destructor TPngChunkList.Destroy;
begin
 Clear;
 inherited;
end;

procedure TPngChunkList.Add(Item: TCustomChunk);
begin
 SetLength(FChunks, Length(FChunks) + 1);
 FChunks[Length(FChunks) - 1] := Item;
end;

procedure TPngChunkList.AssignTo(Dest: TPersistent);
var
  Index      : Integer;
  ChunkClass : TCustomChunkPngWithHeaderClass;
begin
 if Dest is TPngChunkList then
  with TPngChunkList(Dest) do
   begin
    Clear;
    SetLength(FChunks, Self.Count);
    for Index := 0 to Self.Count - 1 do
     if Self.FChunks[Index] is TCustomChunkPngWithHeader then
      begin
       ChunkClass := TCustomChunkPngWithHeaderClass(Self.FChunks[Index].ClassType);
       FChunks[Index] := ChunkClass.Create(TCustomChunkPngWithHeader(Self.FChunks[Index]).FHeader);
       FChunks[Index].Assign(Self.FChunks[Index]);
      end
     else inherited;
   end else inherited;
end;

procedure TPngChunkList.Clear;
var
  Index : Integer;
begin
 for Index := 0 to Count - 1
  do FreeAndNil(FChunks[Index]);
 SetLength(FChunks, 0)
end;

procedure TPngChunkList.Delete(Index: Integer);
begin
 if (Index < 0) or (Index >= Count)
  then raise EPngError.Create(RCStrEmptyChunkList);
 FreeAndNil(FChunks[Index]);
 if Index < Count
  then System.Move(FChunks[Index + 1], FChunks[Index], (Count - Index) * SizeOf(Pointer));
 SetLength(FChunks, Length(FChunks) - 1);
end;

function TPngChunkList.GetChunk(Index: Integer): TCustomChunk;
begin
 if Cardinal(Index) >= Cardinal(Count)
  then raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else Result := FChunks[Index];
end;

function TPngChunkList.GetCount: Integer;
begin
 Result := Length(FChunks);
end;

function TPngChunkList.IndexOf(Item: TCustomChunk): Integer;
begin
 for Result := 0 to Count - 1 do
  if FChunks[Result] = Item
   then Exit;
 Result := -1;
end;

procedure TPngChunkList.Remove(Item: TCustomChunk);
begin
 Delete(IndexOf(Item));
end;


{ TCustomDefinedChunkTextChunk }

procedure TCustomDefinedChunkTextChunk.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDefinedChunkTextChunk then
  with TCustomDefinedChunkTextChunk(Dest) do
   begin
    FKeyword := Self.FKeyword;
    FText    := Self.FText;
   end
 else inherited;
end;

initialization
  RegisterPngChunks([TChunkPngImageData, TChunkPngPalette, TChunkPngGamma,
    TChunkPngStandardColorSpaceRGB, TChunkPngPrimaryChromaticities,
    TChunkPngTime, TChunkPngTransparency, TChunkPngEmbeddedIccProfile,
    TChunkPngPhysicalPixelDimensions, TChunkPngTextChunk,
    TChunkPngCompressedTextChunk, TChunkPngInternationalTextChunk,
    TChunkPngImageHistogram, TChunkPngBackgroundColor,
    TChunkPngSignificantBits, TChunkPngImageOffset, TChunkPngPixelCalibrator]);

end.
