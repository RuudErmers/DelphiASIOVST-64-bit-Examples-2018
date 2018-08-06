unit DAV_ChannelDataCoder;

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
  {$IFDEF FPC}LCLIntf, {$ELSE} {$IFDEF MSWindows} Windows,
  {$ENDIF} {$ENDIF} Classes, DAV_Types, DAV_HalfFloat;

type
  TDAVByteArray = array [0..0] of Byte;
  PDAVByteArray = ^TDAVByteArray;
  TDAVSmallIntArray = array [0..0] of Smallint;
  PDAVSmallIntArray = ^TDAVSmallIntArray;

  TDitherType = (dtNone, dtUniform, dtTriangular, dtGauss);
  TByteAlign = (baLeft, baRight);

  TCustomChannelDataCoder = class(TInterfacedPersistent, IStreamPersist)
  private
    procedure SetBlockSize(Value: Cardinal);
    procedure SetChannelCount(const Value: Cardinal);
    procedure SetSampleFrames(const Value: Cardinal);
    procedure AllocateBlockBuffer;
  protected
    FBlockSize    : Cardinal;
    FBlockBuffer  : PDAVByteArray;
    FSampleFrames : Cardinal;
    FChannelCount : Cardinal;
    function CorrectBlocksize(const Value: Cardinal): Cardinal; virtual;
    procedure BlockSizeChanged; virtual;
    procedure ReallocateChannelMemory; virtual; abstract;
    procedure ChannelCountChanged; virtual; abstract;
    procedure SampleFramesChanged; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function SampleToByte(Sample: Cardinal): Cardinal; virtual; abstract;
    procedure SetBlockSizeAndChannelCount(const BlockSize, ChannelCount: Cardinal); virtual;

    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure LoadFromPointer(const Data: Pointer); virtual; abstract;
    procedure SaveToPointer(const Data: Pointer); virtual; abstract;

    property ChannelCount: Cardinal read FChannelCount write SetChannelCount;
    property BlockSize: Cardinal read FBlockSize write SetBlockSize;
    property SampleFrames: Cardinal read FSampleFrames write SetSampleFrames;
  end;

  TCustomChannel32DataCoder = class(TCustomChannelDataCoder)
  private
    function GetChannelPointer(Index: Integer): PDAVSingleFixedArray;
  protected
    FChannelArray : array of PDAVSingleFixedArray;
    procedure ChannelCountChanged; override;
    procedure ReallocateChannelMemory; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    destructor Destroy; override;
    property ChannelPointer[Index: Integer]: PDAVSingleFixedArray read GetChannelPointer;
  end;

  TCustomPCMChannel32DataCoder = class(TCustomChannel32DataCoder)
  protected
    procedure InterleaveData; virtual; abstract;
    procedure DeinterleaveData; virtual; abstract;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromPointer(const Data: Pointer); override;
    procedure SaveToPointer(const Data: Pointer); override;
  end;

  TCustomChannel32DataCoderFixedPoint = class(TCustomPCMChannel32DataCoder)
  private
    procedure SetDitherType(const Value: TDitherType);
    procedure SetByteAlign(const Value: TByteAlign);
  protected
    FDitherType  : TDitherType;
    FBits        : Byte;
    FSampleSize  : Byte;
    FScaleFactor : TDAV2DoubleArray;
    FByteAlign   : TByteAlign;
    function CorrectBlocksize(const Value: Cardinal): Cardinal; override;
    procedure CalculateSampleFrames; virtual;
    procedure CalculateScaleFactors; virtual;
    procedure CalculateBlockSize; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure ChannelCountChanged; override;
  public
    constructor Create; override;

    function SampleToByte(Sample: Cardinal): Cardinal; override;

    property ByteAlign: TByteAlign read FByteAlign write SetByteAlign default baLeft;
    property Dither: TDitherType read FDitherType write SetDitherType default dtNone;
  end;

  TChannel32DataCoderFixedPoint = class(TCustomChannel32DataCoderFixedPoint)
  private
    procedure SetBits(const Value: Byte);
    procedure SetSampleSize(const Value: Byte);
  protected
    procedure BitsChanged; virtual;
    procedure SampleSizeChanged; virtual;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  public
    constructor Create; override;
    procedure SetBitsAndSampleSize(const Bits, SampleSize: Byte);
  published
    property Dither;
    property Bits: Byte read FBits write SetBits;
    property SampleSize: Byte read FSampleSize write SetSampleSize;
  end;

  TChannel32DataCoderALaw = class(TCustomChannel32DataCoderFixedPoint)
  protected
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  public
    constructor Create; override;
  published
    property Dither;
  end;

  TChannel32DataCoderMuLaw = class(TCustomChannel32DataCoderFixedPoint)
  protected
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  public
    constructor Create; override;
  published
    property Dither;
  end;

  TChannel32DataCoderFloat16 = class(TCustomPCMChannel32DataCoder)
  protected
    function CorrectBlocksize(const Value: Cardinal): Cardinal; override;

    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  public
    function SampleToByte(Sample: Cardinal): Cardinal; override;
  end;

  TChannel32DataCoderFloat32 = class(TCustomPCMChannel32DataCoder)
  private
    procedure CalculateBlocksize;
  protected
    function CorrectBlocksize(const Value: Cardinal): Cardinal; override;

    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
    procedure ChannelCountChanged; override;
  public
    function SampleToByte(Sample: Cardinal): Cardinal; override;
  end;

  TChannel32DataCoderFloat64 = class(TCustomPCMChannel32DataCoder)
  protected
    function CorrectBlocksize(const Value: Cardinal): Cardinal; override;

    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  public
    function SampleToByte(Sample: Cardinal): Cardinal; override;
  end;

  TCustomADPCMChannel32DataCoder = class(TCustomChannel32DataCoder)
  private
(*
    PrevSamp : array [0..1] of SmallInt;
    Index    : array [0..1] of Byte;
*)
  protected
    procedure EncodeData; virtual; abstract;
    procedure DecodeData; virtual; abstract;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromPointer(const Data: Pointer); override;
    procedure SaveToPointer(const Data: Pointer); override;
  end;

  TMicrosoftADPCMChannel32DataCoder = class(TCustomADPCMChannel32DataCoder)
  protected
    procedure EncodeData; override;
    procedure DecodeData; override;
  end;

  TImaDviADPCMChannel32DataCoder = class(TCustomADPCMChannel32DataCoder)
  protected
    procedure EncodeData; override;
    procedure DecodeData; override;
  end;

(*
  TACMWaveFormat = packed record
   case Integer of
    0 : (Format : TWaveFormatEx);
    1 : (RawData : Array[0..128] of byte);
   end;

  TWaveConverter = class(TMemoryStream)
  private
    fMaxFmtSize: DWord;
  public
    CurrentFormat: TACMWaveFormat;
    NewFormat: TACMWaveFormat;
    function LoadStream(Stream : TStream): integer;
    function Convert: integer;
    function SaveWavToStream(MS: TStream): Integer;
    constructor Create;
    destructor Destroy; override;
  end;
*)

  TCustomACMChannel32DataCoder = class(TCustomChannel32DataCoder)
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromPointer(const Data: Pointer); override;
    procedure SaveToPointer(const Data: Pointer); override;
  end;

////////////////////////////////////////////////////////////////////////////////

  TCustomChannel64DataCoder = class(TCustomChannelDataCoder)
  private
    function GetChannelPointer(Index: Integer): PDAVDoubleFixedArray;
  protected
    FChannelArray : array of PDAVDoubleFixedArray;
    procedure ChannelCountChanged; override;
    procedure ReallocateChannelMemory; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    property ChannelPointer[Index: Integer]: PDAVDoubleFixedArray read GetChannelPointer;
  end;

  TCustomPCMChannel64DataCoder = class(TCustomChannel64DataCoder)
  protected
    procedure InterleaveData; virtual; abstract;
    procedure DeinterleaveData; virtual; abstract;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromPointer(const Data: Pointer); override;
    procedure SaveToPointer(const Data: Pointer); override;
  end;

  TCustomChannel64DataCoderFixedPoint = class(TCustomPCMChannel32DataCoder)
  private
    procedure SetDitherType(const Value: TDitherType);
    procedure SetByteAlign(const Value: TByteAlign);
  protected
    FDitherType  : TDitherType;
    FBits        : Byte;
    FSampleSize  : Byte;
    FScaleFactor : TDAV2DoubleArray;
    FByteAlign   : TByteAlign;
    function CorrectBlocksize(const Value: Cardinal): Cardinal; override;
    procedure CalculateSampleFrames; virtual;
    procedure CalculateScaleFactors; virtual;
    procedure CalculateBlockSize; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure ChannelCountChanged; override;
  public
    constructor Create; override;

    function SampleToByte(Sample: Cardinal): Cardinal; override;

    property ByteAlign: TByteAlign read FByteAlign write SetByteAlign default baLeft;
    property Dither: TDitherType read FDitherType write SetDitherType default dtNone;
  end;

  TChannel64DataCoderFixedPoint = class(TCustomChannel64DataCoderFixedPoint)
  private
    procedure SetBits(const Value: Byte);
    procedure SetSampleSize(const Value: Byte);
  protected
    procedure BitsChanged; virtual;
    procedure SampleSizeChanged; virtual;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  public
    constructor Create; override;

    procedure SetBitsAndSampleSize(const Bits, SampleSize: Byte);
  published
    property Dither;
    property Bits: Byte read FBits write SetBits;
    property SampleSize: Byte read FSampleSize write SetSampleSize;
  end;

  TChannel64DataCoderFloat32 = class(TCustomPCMChannel64DataCoder)
  private
    procedure CalculateBlocksize;
  protected
    function CorrectBlocksize(const Value: Cardinal): Cardinal; override;

    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
    procedure ChannelCountChanged; override;
  public
    function SampleToByte(Sample: Cardinal): Cardinal; override;
  end;

  TChannel64DataCoderFloat64 = class(TCustomPCMChannel64DataCoder)
  protected
    function CorrectBlocksize(const Value: Cardinal): Cardinal; override;

    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  public
    function SampleToByte(Sample: Cardinal): Cardinal; override;
  end;

implementation

uses
  SysUtils;


{ TCustomChannelDataCoder }

constructor TCustomChannelDataCoder.Create;
begin
 FBlockSize := 16384;
 FChannelCount := 2;
 ChannelCountChanged;
 BlockSizeChanged;
end;

destructor TCustomChannelDataCoder.Destroy;
begin
 Dispose(FBlockBuffer);
 inherited;
end;

procedure TCustomChannelDataCoder.AllocateBlockBuffer;
begin
 ReallocMem(FBlockBuffer, FBlockSize);
 FillChar(FBlockBuffer^, FBlockSize, 0);
end;

procedure TCustomChannelDataCoder.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomChannelDataCoder then
  begin
   TCustomChannelDataCoder(Dest).BlockSize := FBlockSize;
   TCustomChannelDataCoder(Dest).SampleFrames := FSampleFrames;
   TCustomChannelDataCoder(Dest).ChannelCount := FChannelCount;
   Move(FBlockBuffer[0], TCustomChannelDataCoder(Dest).FBlockBuffer[0], FBlockSize);
  end else inherited;
end;

procedure TCustomChannelDataCoder.BlockSizeChanged;
begin
 AllocateBlockBuffer;
end;

procedure TCustomChannelDataCoder.SetBlockSizeAndChannelCount(const BlockSize,
  ChannelCount: Cardinal);
begin
 Self.ChannelCount := ChannelCount;
 Self.BlockSize := BlockSize;
end;

function TCustomChannelDataCoder.CorrectBlocksize(const Value: Cardinal): Cardinal;
begin
 Result := Value;
end;

procedure TCustomChannelDataCoder.SetBlockSize(Value: Cardinal);
begin
 Value := CorrectBlocksize(Value);
 if FBlockSize <> Value then
  begin
   FBlockSize := Value;
   BlockSizeChanged;
  end;
end;

procedure TCustomChannelDataCoder.SetChannelCount(const Value: Cardinal);
begin
 if FChannelCount <> Value then
  begin
   FChannelCount := Value;
   ChannelCountChanged;
  end;
end;

procedure TCustomChannelDataCoder.SetSampleFrames(const Value: Cardinal);
begin
 if FSampleFrames <> Value then
  begin
   FSampleFrames := Value;
   SampleFramesChanged;
  end;
end;

{ TCustomChannel32DataCoder }

procedure TCustomChannel32DataCoder.AssignTo(Dest: TPersistent);
var
  Channel : Integer;
  Sample  : Integer;
begin
 inherited;
 if Dest is TCustomChannel32DataCoder then
  begin
   Assert(Length(FChannelArray) = Length(TCustomChannel32DataCoder(Dest).FChannelArray));
   for Channel := 0 to Length(FChannelArray) - 1 do
    begin
     Move(FChannelArray[Channel, 0],
       TCustomChannel32DataCoder(Dest).FChannelArray[Channel, 0],
       FSampleFrames * SizeOf(Single));
    end;
  end else
 if Dest is TCustomChannel64DataCoder then
  begin
   Assert(Length(FChannelArray) = Length(TCustomChannel64DataCoder(Dest).FChannelArray));
   for Channel := 0 to Length(FChannelArray) - 1 do
    for Sample := 0 to FSampleFrames - 1
     do TCustomChannel64DataCoder(Dest).FChannelArray[Channel, Sample] := FChannelArray[Channel, Sample];
  end;
end;

procedure TCustomChannel32DataCoder.ChannelCountChanged;
var
  Channel : Integer;
begin
 inherited;
 if FChannelCount < Cardinal(Length(FChannelArray)) then
  for Channel := FChannelCount to Length(FChannelArray) - 1
   do Dispose(FChannelArray[Channel]);

 SetLength(FChannelArray, FChannelCount);
 ReallocateChannelMemory;
end;

destructor TCustomChannel32DataCoder.Destroy;
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FChannelArray) - 1
  do Dispose(FChannelArray[Channel]);
 inherited;
end;

function TCustomChannel32DataCoder.GetChannelPointer(
  Index: Integer): PDAVSingleFixedArray;
begin
 if (Index >= 0) and (Index < Length(FChannelArray))
  then Result := FChannelArray[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TCustomChannel32DataCoder.ReallocateChannelMemory;
var
  i : Integer;
begin
 for i := 0 to Length(FChannelArray) - 1 do
  begin
   ReallocMem(FChannelArray[i], FSampleFrames * SizeOf(Single));
   FillChar(FChannelArray[i]^, FSampleFrames * SizeOf(Single), 0);
  end;
end;

{ TCustomPCMChannel32DataCoder }

procedure TCustomPCMChannel32DataCoder.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FBlockBuffer^[0], FBlockSize);
 DeinterleaveData;
end;

procedure TCustomPCMChannel32DataCoder.LoadFromPointer(const Data: Pointer);
begin
 Move(Data^, FBlockBuffer^[0], FBlockSize);
 DeinterleaveData;
end;

procedure TCustomPCMChannel32DataCoder.SaveToStream(Stream: TStream);
begin
 InterleaveData;
 Stream.Write(FBlockBuffer^[0], FBlockSize);
end;

procedure TCustomPCMChannel32DataCoder.SaveToPointer(const Data: Pointer);
begin
 InterleaveData;
 Move(FBlockBuffer^[0], Data^, FBlockSize);
end;

{ TCustomChannel32DataCoderFixedPoint }

constructor TCustomChannel32DataCoderFixedPoint.Create;
begin
 FDitherType := dtNone;
 FByteAlign  := baLeft;
 inherited;
 CalculateScaleFactors;
end;

procedure TCustomChannel32DataCoderFixedPoint.CalculateSampleFrames;
begin
 inherited;
 SampleFrames := FBlockSize div FChannelCount div FSampleSize;
end;

procedure TCustomChannel32DataCoderFixedPoint.CalculateScaleFactors;
begin
 case FByteAlign of
  baLeft:
   begin
    FScaleFactor[0] := 1 shl (8 * ((FBits + 7) div 8) - 1) - 1;
    FScaleFactor[1] := 1 / FScaleFactor[0];
   end;
  baRight:
   begin
    if FBits = 32
     then FScaleFactor[0] := MaxInt
     else FScaleFactor[0] := (1 shl (FBits - 1)) - 1;
    FScaleFactor[1] := 1 / FScaleFactor[0];
   end;
  else Assert(False);
 end;
end;

procedure TCustomChannel32DataCoderFixedPoint.SetByteAlign(const Value: TByteAlign);
begin
 if FByteAlign <> Value then
  begin
   FByteAlign := Value;
   CalculateScaleFactors;
  end;
end;

procedure TCustomChannel32DataCoderFixedPoint.SetDitherType(const Value: TDitherType);
begin
 if FDitherType <> Value then
  begin
   FDitherType := Value;
  end;
end;

function TCustomChannel32DataCoderFixedPoint.CorrectBlocksize(const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * FSampleSize;
 Result := Granularity * (Value div Granularity);
end;

procedure TCustomChannel32DataCoderFixedPoint.CalculateBlockSize;
begin
 BlockSize := FSampleFrames * ChannelCount * FSampleSize;
end;

procedure TCustomChannel32DataCoderFixedPoint.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomChannel32DataCoderFixedPoint then
  with TCustomChannel32DataCoderFixedPoint(Dest) do
   begin
    FDitherType  := Self.FDitherType;
    FBits        := Self.FBits;
    FSampleSize  := Self.FSampleSize;
    FScaleFactor := Self.FScaleFactor;
    FByteAlign   := Self.FByteAlign;
   end else
 if Dest is TCustomChannel64DataCoderFixedPoint then
  with TCustomChannel64DataCoderFixedPoint(Dest) do
   begin
    FDitherType  := Self.FDitherType;
    FBits        := Self.FBits;
    FSampleSize  := Self.FSampleSize;
    FScaleFactor := Self.FScaleFactor;
    FByteAlign   := Self.FByteAlign;
   end;
end;

procedure TCustomChannel32DataCoderFixedPoint.BlockSizeChanged;
begin
 CalculateSampleFrames;
 inherited;
end;

procedure TCustomChannel32DataCoderFixedPoint.ChannelCountChanged;
begin
 inherited;
 CalculateSampleFrames;
end;

procedure TCustomChannel32DataCoderFixedPoint.SampleFramesChanged;
begin
 CalculateBlockSize;
 ReallocateChannelMemory;
end;


function TCustomChannel32DataCoderFixedPoint.SampleToByte(Sample: Cardinal): Cardinal;
begin
 Result := Sample * FSampleSize;
end;

{ TChannel32DataCoderFixedPoint }

constructor TChannel32DataCoderFixedPoint.Create;
begin
 FBits       := 32;
 FSampleSize := 4;
 inherited;
end;

procedure TChannel32DataCoderFixedPoint.SetSampleSize(const Value: Byte);
begin
 if SampleSize <> Value then
  begin
   FSampleSize := Value;
   SampleSizeChanged;
  end;
end;

procedure TChannel32DataCoderFixedPoint.SampleSizeChanged;
begin
 if FBits > 8 * FSampleSize then
  begin
   FBits := FSampleSize * 8;
   BitsChanged;
  end;
 SampleFramesChanged;
end;

procedure TChannel32DataCoderFixedPoint.SetBits(const Value: Byte);
begin
 if Bits <> Value then
  begin
   FBits := Value;
   BitsChanged;
  end;
end;

procedure TChannel32DataCoderFixedPoint.BitsChanged;
begin
 if FBits > 8 * FSampleSize then
  begin
   FSampleSize := (FBits + 7) div 8;
   SampleSizeChanged;
  end;
 CalculateScaleFactors;
end;

procedure TChannel32DataCoderFixedPoint.SetBitsAndSampleSize(const Bits,
  SampleSize: Byte);
begin
 if (Bits > 8 * SampleSize)
  then raise Exception.Create('Number of bits must fit into the sample size!');

 FBits := Bits;
 FSampleSize := SampleSize;

 CalculateBlockSize;
 if BlockSize <> CorrectBlocksize(BlockSize)
  then BlockSize := CorrectBlocksize(BlockSize);

 CalculateScaleFactors;
end;

procedure TChannel32DataCoderFixedPoint.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
  DataInt : Integer;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * FSampleSize));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 case SampleSize of
  1: for Sample := 0 to FSampleFrames - 1 do
      for Channel := 0 to FChannelCount - 1
       do PDAVByteArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := 127 + round(FChannelArray[Channel]^[Sample] * FScaleFactor[0]);
  2: for Sample := 0 to FSampleFrames - 1 do
      for Channel := 0 to FChannelCount - 1
       do PDAVSmallIntArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := round(FChannelArray[Channel]^[Sample] * FScaleFactor[0]);
  3: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1 do
       begin
        DataInt := round(FChannelArray[Channel]^[Sample] * FScaleFactor[0]);// shl 8;
        Move(DataInt, PDAVByteArray(FBlockBuffer)^[3 * (Sample * FChannelCount + Channel)], 3);
       end;
  4: for Sample := 0 to FSampleFrames - 1 do
      for Channel := 0 to FChannelCount - 1
       do PIntegerArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := round(FChannelArray[Channel]^[Sample] * FScaleFactor[0]);
 end;
end;

procedure TChannel32DataCoderFixedPoint.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * FSampleSize));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 case SampleSize of
  1: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1
       do FChannelArray[Channel]^[Sample] := (PDAVByteArray(FBlockBuffer)^[Sample * FChannelCount + Channel] - 127) * FScaleFactor[1];
  2: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1
       do FChannelArray[Channel]^[Sample] := PDAVSmallIntArray(FBlockBuffer)^[Sample * FChannelCount + Channel] * FScaleFactor[1];
  3: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1
       do FChannelArray[Channel]^[Sample] := (ShortInt(PDAVByteArray(FBlockBuffer)^[3 * (Sample * FChannelCount + Channel) + 2]) shl 16 +
                                                       PDAVByteArray(FBlockBuffer)^[3 * (Sample * FChannelCount + Channel) + 1]  shl  8 +
                                                       PDAVByteArray(FBlockBuffer)^[3 * (Sample * FChannelCount + Channel)]) * FScaleFactor[1];
  4: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1
       do FChannelArray[Channel]^[Sample] := PIntegerArray(FBlockBuffer)^[Sample * FChannelCount + Channel] * FScaleFactor[1];
 end;
end;

{ TChannel32DataCoderALaw }

constructor TChannel32DataCoderALaw.Create;
begin
 FBits        := 8;
 FSampleSize  := 1;
 inherited;
end;

function EncodeALaw(Input: Single): Byte;
var
  IntValue : Integer;
  SignInt  : Integer;
  Exponent : Integer;
  ExpMask  : Integer;
  Mantissa : Integer;
begin
 // Get the sign bit. Shift it for later use without further modification
 if Input < 0
  then SignInt := 1 shl 8
  else SignInt := 0;

 // If the number is negative, make it positive (now it's a magnitude)
 Input := abs(Input);

 // The magnitude must fit in 15 bits to avoid overflow
 if (Input > 1)
  then IntValue := $7FFF
  else IntValue := round($7FFF * Input);

 // Finding the "exponent"
 // Bits:
 // 1 2 3 4 5 6 7 8 9 A B C D E F G
 // S 7 6 5 4 3 2 1 0 0 0 0 0 0 0 0
 // We want to find where the first 1 after the sign bit is.
 // We take the corresponding value
 // from the second row as the exponent value.
 // (i.e. if first 1 at position 7 -> exponent = 2)
 // The exponent is 0 if the 1 is not found in bits 2 through 8.
 // This means the exponent is 0 even if the "first 1" doesn't exist.
 Exponent := 7;

 // Move to the right and decrement exponent until we hit the 1 or the
 // exponent hits 0
 ExpMask := $4000;
 while ((IntValue and ExpMask) = 0) and (Exponent > 0) do
  begin
   ExpMask := ExpMask shr 1;
   dec(Exponent);
  end;

 // The last part - the "mantissa"
 // We need to take the four bits after the 1 we just found.
 // To get it, we shift $0F :
 // 1 2 3 4 5 6 7 8 9 A B C D E F G
 // S 0 0 0 0 0 1 . . . . . . . . . (say that exponent is 2)
 // . . . . . . . . . . . . 1 1 1 1
 // We shift it 5 times for an exponent of two, meaning we will shift our
 // four bits (exponent + 3) bits.
 // For convenience, we will actually just shift the number, then AND with $0F.
 //
 // NOTE: If the exponent is 0:
 // 1 2 3 4 5 6 7 8 9 A B C D E F G
 // S 0 0 0 0 0 0 0 Z Y X W V U T S (we know nothing about bit 9)
 // . . . . . . . . . . . . 1 1 1 1
 // We want to get ZYXW, which means a shift of 4 instead of 3

 if (Exponent = 0)
  then Mantissa := IntValue shr 4
  else Mantissa := IntValue shr (Exponent + 3);

 // The a-law byte bit arrangement is SEEEMMMM (Sign, Exponent, and Mantissa.)
 // Last is to flip every other bit, and the sign bit ($D5 = 1101 0101)
 Result := ((SignInt or (Exponent shl 4) or (Mantissa and $0F))) xor $D5;
end;

procedure TChannel32DataCoderALaw.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = SampleFrames * FChannelCount * FSampleSize);
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Sample := 0 to FSampleFrames - 1 do
  for Channel := 0 to FChannelCount - 1
   do PDAVByteArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := EncodeALaw(FChannelArray[Channel]^[Sample]);
end;

function DecodeALaw(Input: Byte): Single; overload;
var
  Sign     : Integer;
  Exponent : Integer;
  Data     : Integer;
const
  CScale: Single = 3.0518509475997192297128208258309e-5;
begin
 //Invert every other bit,
 //and the sign bit ($D5 = 1101 0101)
 Input := Input xor $D5;

 //Pull out the value of the sign bit
 Sign := Input and $80;

 //Pull out and shift over the value of the exponent
 Exponent := (Input and $70) shr 4;

 //Pull out the four bits of data and shift the data four bits to the left
 //Add 8 to put the Result in the middle of the range (like adding a half)
 Data := ((Input and $0F) shl 4) + 8;

 //If the exponent is not 0, then we know the four bits followed a 1,
 //and can thus add this implicit 1 with 0x100.
 if (Exponent <> 0) then Data := Data + $100;

 // Shift the bits to where they need to be: left (exponent - 1) places
 // Why (exponent - 1) ?
 // 1 2 3 4 5 6 7 8 9 A B C D E F G
 // . 7 6 5 4 3 2 1 . . . . . . . . <-- starting bit (based on exponent)
 // . . . . . . . Z x x x x 1 0 0 0 <-- our data (Z is 0 only when
 // exponent is 0)
 // We need to move the one under the value of the exponent,
 // which means it must move (exponent - 1) times
 // It also means shifting is unnecessary if exponent is 0 or 1.

 if (Exponent > 1)
  then Data := Data shl (Exponent - 1);

 Result := Data * CScale;
 if sign <> 0 then Result := -Result;
end;

const
  CALawDecompressTable : array [0..255] of SmallInt = (
     -5504, -5248, -6016, -5760, -4480, -4224, -4992, -4736,
     -7552, -7296, -8064, -7808, -6528, -6272, -7040, -6784,
     -2752, -2624, -3008, -2880, -2240, -2112, -2496, -2368,
     -3776, -3648, -4032, -3904, -3264, -3136, -3520, -3392,
     -22016,-20992,-24064,-23040,-17920,-16896,-19968,-18944,
     -30208,-29184,-32256,-31232,-26112,-25088,-28160,-27136,
     -11008,-10496,-12032,-11520,-8960, -8448, -9984, -9472,
     -15104,-14592,-16128,-15616,-13056,-12544,-14080,-13568,
     -344,  -328,  -376,  -360,  -280,  -264,  -312,  -296,
     -472,  -456,  -504,  -488,  -408,  -392,  -440,  -424,
     -88,   -72,   -120,  -104,  -24,   -8,    -56,   -40,
     -216,  -200,  -248,  -232,  -152,  -136,  -184,  -168,
     -1376, -1312, -1504, -1440, -1120, -1056, -1248, -1184,
     -1888, -1824, -2016, -1952, -1632, -1568, -1760, -1696,
     -688,  -656,  -752,  -720,  -560,  -528,  -624,  -592,
     -944,  -912,  -1008, -976,  -816,  -784,  -880,  -848,
      5504,  5248,  6016,  5760,  4480,  4224,  4992,  4736,
      7552,  7296,  8064,  7808,  6528,  6272,  7040,  6784,
      2752,  2624,  3008,  2880,  2240,  2112,  2496,  2368,
      3776,  3648,  4032,  3904,  3264,  3136,  3520,  3392,
      22016, 20992, 24064, 23040, 17920, 16896, 19968, 18944,
      30208, 29184, 32256, 31232, 26112, 25088, 28160, 27136,
      11008, 10496, 12032, 11520, 8960,  8448,  9984,  9472,
      15104, 14592, 16128, 15616, 13056, 12544, 14080, 13568,
      344,   328,   376,   360,   280,   264,   312,   296,
      472,   456,   504,   488,   408,   392,   440,   424,
      88,    72,   120,   104,    24,     8,    56,    40,
      216,   200,   248,   232,   152,   136,   184,   168,
      1376,  1312,  1504,  1440,  1120,  1056,  1248,  1184,
      1888,  1824,  2016,  1952,  1632,  1568,  1760,  1696,
      688,   656,   752,   720,   560,   528,   624,   592,
      944,   912,  1008,   976,   816,   784,   880,   848);

procedure TChannel32DataCoderALaw.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
const
  CScale: Single = 3.0518509475997192297128208258309e-5;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * FSampleSize));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do FChannelArray[Channel]^[Sample] := CALawDecompressTable[PDAVByteArray(FBlockBuffer)^[Sample * FChannelCount + Channel]] * CScale;
end;


{ TChannel32DataCoderMuLaw }

(*
http://hazelware.luggle.com/tutorials/mulawcompression.html
http://www.codeproject.com/KB/security/g711audio.aspx
*)

constructor TChannel32DataCoderMuLaw.Create;
begin
 FBits        := 8;
 FSampleSize  := 1;
 inherited;
end;

const
  CBias : Integer = $84;
  CClip : Integer = 32635;
  CMuLawCompressTable : array [0..255] of Byte =
    (0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
     4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
     5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
     5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
     6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
     6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
     6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
     6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7);

function LinearToMuLawSample(Input: Single): Byte;
var
  InputInt : SmallInt;
  Sign     : Integer;
  Exponent : Integer;
  Mantissa : Integer;
begin
 InputInt := round(Input * $7FFF);
 Sign := (InputInt shr 8) and $80;
 if Sign <> 0 then InputInt := -InputInt;
 if InputInt > CClip then InputInt := cClip;
 InputInt := InputInt + cBias;
 Exponent := CMuLawCompressTable[(InputInt shr 7) and $FF];
 Mantissa := (InputInt shr (Exponent + 3)) and $0F;
 Result := not (Sign or (Exponent shl 4) or Mantissa);
end;

procedure TChannel32DataCoderMuLaw.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = SampleFrames * FChannelCount * FSampleSize);
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Sample := 0 to FSampleFrames - 1 do
  for Channel := 0 to FChannelCount - 1
   do PDAVByteArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := LinearToMuLawSample(FChannelArray[Channel]^[Sample]);
end;

const
  CMuLawDecompressTable : array [0..255] of SmallInt = (
     -32124, -31100, -30076, -29052, -28028, -27004, -25980, -24956,
     -23932, -22908, -21884, -20860, -19836, -18812, -17788, -16764,
     -15996, -15484, -14972, -14460, -13948, -13436, -12924, -12412,
     -11900, -11388, -10876, -10364,  -9852,  -9340,  -8828,  -8316,
      -7932,  -7676,  -7420,  -7164,  -6908,  -6652,  -6396,  -6140,
      -5884,  -5628,  -5372,  -5116,  -4860,  -4604,  -4348,  -4092,
      -3900,  -3772,  -3644,  -3516,  -3388,  -3260,  -3132,  -3004,
      -2876,  -2748,  -2620,  -2492,  -2364,  -2236,  -2108,  -1980,
      -1884,  -1820,  -1756,  -1692,  -1628,  -1564,  -1500,  -1436,
      -1372,  -1308,  -1244,  -1180,  -1116,  -1052,   -988,   -924,
       -876,   -844,   -812,   -780,   -748,   -716,   -684,   -652,
       -620,   -588,   -556,   -524,   -492,   -460,   -428,   -396,
       -372,   -356,   -340,   -324,   -308,   -292,   -276,   -260,
       -244,   -228,   -212,   -196,   -180,   -164,   -148,   -132,
       -120,   -112,   -104,    -96,    -88,    -80,    -72,    -64,
        -56,    -48,    -40,    -32,    -24,    -16,     -8,      0,
      32124,  31100,  30076,  29052,  28028,  27004,  25980,  24956,
      23932,  22908,  21884,  20860,  19836,  18812,  17788,  16764,
      15996,  15484,  14972,  14460,  13948,  13436,  12924,  12412,
      11900,  11388,  10876,  10364,   9852,   9340,   8828,   8316,
       7932,   7676,   7420,   7164,   6908,   6652,   6396,   6140,
       5884,   5628,   5372,   5116,   4860,   4604,   4348,   4092,
       3900,   3772,   3644,   3516,   3388,   3260,   3132,   3004,
       2876,   2748,   2620,   2492,   2364,   2236,   2108,   1980,
       1884,   1820,   1756,   1692,   1628,   1564,   1500,   1436,
       1372,   1308,   1244,   1180,   1116,   1052,    988,    924,
        876,    844,    812,    780,    748,    716,    684,    652,
        620,    588,    556,    524,    492,    460,    428,    396,
        372,    356,    340,    324,    308,    292,    276,    260,
        244,    228,    212,    196,    180,    164,    148,    132,
        120,    112,    104,     96,     88,     80,     72,     64,
         56,     48,     40,     32,     24,     16,      8,      0
);

procedure TChannel32DataCoderMuLaw.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
const
  CScale: Single = 3.0518509475997192297128208258309e-5;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * FSampleSize));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do FChannelArray[Channel]^[Sample] := CMuLawDecompressTable[(PDAVByteArray(FBlockBuffer)^[Sample * FChannelCount + Channel])] * CScale;
end;


{ TChannel32DataCoderFloat16 }

function TChannel32DataCoderFloat16.CorrectBlocksize(
  const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * SizeOf(THalfFloat);
 Assert(Granularity >= 1);
 Result := Granularity * ((Value + Granularity - 1) div Granularity);
end;

procedure TChannel32DataCoderFloat16.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(THalfFloat)));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do PDAVHalfFloatFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := SingleToHalfFloat(FChannelArray[Channel]^[Sample]);
end;

procedure TChannel32DataCoderFloat16.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(THalfFloat)));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do FChannelArray[Channel]^[Sample] := HalfFloatToSingle(PDAVHalfFloatFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel]);
end;

procedure TChannel32DataCoderFloat16.BlockSizeChanged;
begin
 inherited;
 SampleFrames := FBlockSize div FChannelCount div SizeOf(THalfFloat);
end;

procedure TChannel32DataCoderFloat16.SampleFramesChanged;
begin
 BlockSize := FSampleFrames * ChannelCount * SizeOf(THalfFloat);
 ReallocateChannelMemory;
end;

function TChannel32DataCoderFloat16.SampleToByte(Sample: Cardinal): Cardinal;
begin
 Result := Sample * SizeOf(THalfFloat);
end;

{ TChannel32DataCoderFloat32 }

function TChannel32DataCoderFloat32.CorrectBlocksize(const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * SizeOf(Single);
 Assert(Granularity >= 1);
 Result := Granularity * ((Value + Granularity - 1) div Granularity);
end;

procedure TChannel32DataCoderFloat32.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(Single)));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do PDAVSingleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := FChannelArray[Channel]^[Sample];
end;

procedure TChannel32DataCoderFloat32.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(Single)));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do FChannelArray[Channel]^[Sample] := PDAVSingleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel];
end;

procedure TChannel32DataCoderFloat32.BlockSizeChanged;
begin
 inherited;
 SampleFrames := FBlockSize div FChannelCount div SizeOf(Single);
end;

procedure TChannel32DataCoderFloat32.SampleFramesChanged;
begin
 CalculateBlocksize;
end;

function TChannel32DataCoderFloat32.SampleToByte(Sample: Cardinal): Cardinal;
begin
 Result := Sample * SizeOf(Single);
end;

procedure TChannel32DataCoderFloat32.ChannelCountChanged;
begin
 inherited;
 CalculateBlocksize;
end;

procedure TChannel32DataCoderFloat32.CalculateBlocksize;
begin
 BlockSize := FSampleFrames * ChannelCount * SizeOf(Single);
 ReallocateChannelMemory;
end;

{ TChannel32DataCoderFloat64 }

function TChannel32DataCoderFloat64.CorrectBlocksize(const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * SizeOf(Double);
 Assert(Granularity >= 1);
 Result := Granularity * ((Value + Granularity - 1) div Granularity);
end;

procedure TChannel32DataCoderFloat64.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(Double)));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do PDAVDoubleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := FChannelArray[Channel]^[Sample];
end;

procedure TChannel32DataCoderFloat64.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(Double)));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do FChannelArray[Channel]^[Sample] := PDAVDoubleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel];
end;

procedure TChannel32DataCoderFloat64.BlockSizeChanged;
begin
 inherited;
 SampleFrames := FBlockSize div FChannelCount div SizeOf(Double);
end;

procedure TChannel32DataCoderFloat64.SampleFramesChanged;
begin
 BlockSize := FSampleFrames * ChannelCount * SizeOf(Double);
 ReallocateChannelMemory;
end;

function TChannel32DataCoderFloat64.SampleToByte(Sample: Cardinal): Cardinal;
begin
 Result := Sample * SizeOf(Double);
end;

{ TCustomADPCMChannel32DataCoder }

(*
procedure TMFWaveFile.DecodeDVIADPCM(InData : TByteBuffer; OutData : TSmallIntBuffer; var Len : Integer);
var
  i, j, SP      : Integer;
  Diff, PSample : Integer;
  Code          : Byte;
  Index         : Integer;
begin
 OutData[0] := FADPCM_State.PrevSamp_l;
 SP := 0;
 PSample := FADPCM_State.PrevSamp_l;
 Index := FADPCM_State.index_l;
 for i := 0 to (Len shl 1) -1 do
  begin
   j := i shr 1;
   Code := InData[j];
   if (i and 1) = 0
    then Code := Code and 15
    else Code := Code shr 4;
   Diff := (StepTab[Index] shr 3);
   if (Code and 4) <> 0 then Diff := Diff + StepTab[Index];
   if (Code and 2) <> 0 then Diff := Diff + (StepTab[Index] shr 1);
   if (Code and 1) <> 0 then Diff := Diff + (StepTab[Index] shr 2);
   if (Code and 8) <> 0 then Diff := -Diff;
   PSample := PSample + Diff;
   if PSample > 32767 then PSample := 32767;
   if PSample < -32767 then PSample := -32767;
   SP := SP+1;
   OutData[SP] := PSample;
   Index := Index + IndexTab[Code];
   if Index > 88 then Index := 88;
   if Index < 0 then Index := 0;
  end;
 Len := SP + 1;
end;
*)

procedure TCustomADPCMChannel32DataCoder.LoadFromPointer(const Data: Pointer);
begin
 Move(Data^, FBlockBuffer^[0], FBlockSize);
 DecodeData;
end;

procedure TCustomADPCMChannel32DataCoder.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FBlockBuffer^[0], FBlockSize);
 DecodeData;
end;

procedure TCustomADPCMChannel32DataCoder.SaveToPointer(const Data: Pointer);
begin
 EncodeData;
 Move(FBlockBuffer^[0], Data^, FBlockSize);
end;

procedure TCustomADPCMChannel32DataCoder.SaveToStream(Stream: TStream);
begin
 EncodeData;
 Stream.Write(FBlockBuffer^[0], FBlockSize);
end;


{ TMicrosoftADPCMChannel32DataCoder }

(*
int AdaptationTable [] = {
  230, 230, 230, 230, 307, 409, 512, 614,
  768, 614, 512, 409, 307, 230, 230, 230
} ;
int AdaptCoeff1 [] = { 256, 512, 0, 192, 240, 460, 392 } ;
int AdaptCoeff2 [] = { 0, -256, 0, 64, 0, -208, -232 } ;
*)

procedure TMicrosoftADPCMChannel32DataCoder.DecodeData;
{
var
  pos, i     : Integer;
  PredSamp   : Integer;
  ErrorDelta : Integer;
}
begin
(*
 pos := 0;
 OutData[pos] := fADPCM_MS.Samp2[0];
 Inc(pos);
 OutData[pos] := fADPCM_MS.Samp1[0];
 Inc(pos);
 for i := 0 to (Len shr 1) - 1 do
  begin
   PredSamp := (fADPCM_MS.Samp1[0]*fFormatChunk.ADPCM.CoefSets[fADPCM_MS.predictor[0]].Coef1 +
          fADPCM_MS.Samp2[0]*fFormatChunk.ADPCM.CoefSets[fADPCM_MS.predictor[0]].Coef2) div 256;
   ErrorDelta := InData[i] shr 4;
   if (ErrorDelta and 8) <> 0
    then PredSamp := PredSamp + fADPCM_MS.Delta[0]*(ErrorDelta - 16)
    else PredSamp := PredSamp + fADPCM_MS.Delta[0]*(ErrorDelta);
   if PredSamp > 32767 then PredSamp := 32767;
   if PredSamp < -32768 then PredSamp := -32768;
   OutData[pos] := PredSamp;
   Inc(pos);
   fADPCM_MS.Delta[0] := (fADPCM_MS.Delta[0]*adaptive[ErrorDelta]) div 256;
   if fADPCM_MS.Delta[0] < 16 then fADPCM_MS.Delta[0] := 16;
   fADPCM_MS.Samp2[0] := fADPCM_MS.Samp1[0];
   fADPCM_MS.Samp1[0] := PredSamp;

   PredSamp := (fADPCM_MS.Samp1[0]*fFormatChunk.ADPCM.CoefSets[fADPCM_MS.predictor[0]].Coef1 +
                fADPCM_MS.Samp2[0]*fFormatChunk.ADPCM.CoefSets[fADPCM_MS.predictor[0]].Coef2) div 256;
   ErrorDelta := InData[i] and 15;
   if (ErrorDelta and 8) <> 0
    then PredSamp := PredSamp + fADPCM_MS.Delta[0]*(ErrorDelta - 16)
    else PredSamp := PredSamp + fADPCM_MS.Delta[0]*(ErrorDelta);
   if PredSamp > 32767 then PredSamp := 32767;
   if PredSamp < -32768 then PredSamp := -32768;
   OutData[pos] := PredSamp;
   Inc(pos);
   fADPCM_MS.Delta[0] := (fADPCM_MS.Delta[0]*adaptive[ErrorDelta]) div 256;
   if fADPCM_MS.Delta[0] < 16 then fADPCM_MS.Delta[0] := 16;
   fADPCM_MS.Samp2[0] := fADPCM_MS.Samp1[0];
   fADPCM_MS.Samp1[0] := PredSamp;
  end;
  Len := pos * 2;
end;
*)
end;

procedure TMicrosoftADPCMChannel32DataCoder.EncodeData;
begin
 raise Exception.Create('not yet implemented');
end;


{ TImaDviADPCMChannel32DataCoder }

procedure TImaDviADPCMChannel32DataCoder.DecodeData;
begin
 raise Exception.Create('not yet implemented');
end;

procedure TImaDviADPCMChannel32DataCoder.EncodeData;
begin
 raise Exception.Create('not yet implemented');
end;




{ TCustomACMChannel32DataCoder }

constructor TCustomACMChannel32DataCoder.Create;
begin
 inherited;
(*
 acmMetrics(nil, ACM_METRIC_MAX_SIZE_FORMAT, fMaxFmtSize);
 FillChar(CurrentFormat.Format, fMaxFmtSize, 0);
 FillChar(NewFormat.Format, fMaxFmtSize, 0);
*)
end;

(*
procedure TCustomACMChannel32DataCoder.Decode;
var
 Header  : TRiffHeader;
 ID      : TRiffID;
 Mem     : Pointer;
 Data    : PByteArray;
 NumRead : Integer;
 Pos     : Integer;
begin
 Result := 0;
 try
  //read the header
  NumRead := Stream.Read(Header, SizeOf(Header));
  Pos := NumRead;
  NumRead := Stream.Read(ID, SizeOf(ID));
  Pos := Pos + NumRead;
  if (Header.ID <> 'RIFF') or (ID <> 'WAVE')
   then Exit;

  while Pos < Stream.Size -1 do
   begin
    Dec(Pos,7);
    Stream.Seek(Pos, soFromBeginning);

    NumRead := Stream.Read(Header, SizeOf(Header));
    Pos := Pos + NumRead;

    if Header.ID = 'fmt ' then
     begin
      GetMem(Mem, Header.BytesFollowing);
      try
       NumRead := Stream.Read(Mem^, Header.BytesFollowing);
       Pos := Pos + NumRead;

       if Header.BytesFollowing < SizeOf(TWaveFormatEx)
        then Move(Mem^, CurrentFormat.Format , SizeOf(TWaveFormatEx))
        else Move(Mem^, CurrentFormat.Format, Header.BytesFollowing);
      finally
       FreeMem(Mem);
      end;
     end
    else
     if Header.ID = 'fact' then
      begin
       GetMem(Data, Header.BytesFollowing);
       try
        NumRead := Stream.Read(Data^, Header.BytesFollowing);
        Pos := Pos + NumRead;
       finally
        FreeMem(Data);
       end;
      end
    else
     if Header.ID = 'data' then
      begin
       if Header.BytesFollowing > 0 then
        begin
         GetMem(Data, Header.BytesFollowing);
         try
          NumRead := Stream.Read(Data^, Header.BytesFollowing);
          Pos := Pos + NumRead;

          Self.SetSize(Header.BytesFollowing);
          Self.Seek(0,0);
          Self.Write(Data^, {Header.BytesFollowing}self.Size);
         finally
          FreeMem(Data);
         end;
        end;
      end;
   end;

  Seek(0,0);
 finally
  // FileStream.Free;
 end;
end;

function TWaveConverter.SaveWavToStream(MS: TStream): Integer;
var CurrentPos  : Integer;
    H           : TRiffHeader;
    ID          : TRiffID;
begin
 Result := 0;
 try
  CurrentPos := Position;

  H.ID := 'RIFF';
  H.BytesFollowing := 0;
  MS.Write(H, SizeOf(H));

  ID := 'WAVE';
  MS.Write(ID, SizeOf(ID));

  H.ID := 'fmt ';
  H.BytesFollowing := SizeOf(TWaveFormat) + 2;
  MS.Write(H, SizeOf(H));
  MS.Write(CurrentFormat.Format, SizeOf(TWaveFormat) + 2);

  H.ID := 'data';
  H.BytesFollowing := Size;
  MS.Write(H, SizeOf(H));
  Seek(0,0);
//  MS.CopyFrom(Self, Size);
//  ms.Write( Self, Size);
  self.SaveToStream(MS);

  MS.Seek(0,0);
  H.ID := 'RIFF';
  H.BytesFollowing := MS.Size - SizeOf(H) +1;
  MS.Write(H,SizeOf(H));

  Position := CurrentPos;
//  MS.Free;
 except
  on E: Exception do
  begin
   Result := MCIERR_FILE_NOT_SAVED;
//   SetError('SaveFile', MCIERR_FILE_NOT_SAVED);
  end;
 end;
end;

function TWaveConverter.Convert: integer;
var
 fStreamHandle: HACMStream;
 OutputBufferSize: DWord;
 fStreamHeader: TACMStreamHeader;
 OutPut: Pointer;
begin
 fStreamHandle := nil;

 // Open the stream we're going to use to convert from the current to the new format
 Result := acmStreamOpen(fStreamhandle, nil, CurrentFormat.Format, NewFormat.Format, nil, 0, 0, ACM_STREAMOPENF_NONREALTIME);
 if Result <> 0 then
 begin
  //SetError('acmStreamOpen', Result);
  Exit;
 end;

 // Calculate the size of the converted data
 Result := acmStreamSize(fStreamHandle, self. Size, OutputBufferSize, ACM_STREAMSIZEF_SOURCE);

 if Result <> 0 then
 begin
//  SetError('acmStreamSize', Result);
  Exit;
 end;

 // Allocate memory for the converted data
 GetMem(OutPut, OutputBufferSize);
 FillChar(OutPut^,OutputBufferSize,#0);

 Self.Seek(0,0);

 // Initialize and prepare a header
 with fStreamHeader do
  begin
   cbStruct := SizeOf(TACMStreamHeader);
   fdwStatus := 0;
   dwUser := 0;
   pbSrc := self.Memory;
   cbSrcLength := self.Size;
   cbSrcLengthUsed := 0;
   dwSrcUser := 0;
   pbDst := OutPut;
   cbDstLength := OutputBufferSize;
   cbDstLengthUsed := 0;
   dwDstUser := 0;
  end;
 Result := acmStreamPrepareHeader(fStreamHandle,fStreamHeader, 0);
 if Result <> 0 then
  begin
//   SetError('acmStreamPrepareHeader', Result);
   Exit;
  end;

 // Tell acm to convert the stream
 Result := acmStreamConvert(fStreamHandle,fStreamHeader, ACM_STREAMCONVERTF_BLOCKALIGN);
 if Result <> 0 then
  begin
//   SetError('acmStreamConvert', Result);
   Exit;
  end;

  // Set the format eqaul to the newformat and copy the data over to the streams memory
  Move(NewFormat.RawData, CurrentFormat.RawData, fMaxFmtSize);
  Self.SetSize(OutputBufferSize);
  Self.Seek(0,0);
  Self.Write(Output^, OutputBufferSize);

  // Unprepeare the header
  Result := acmStreamUnprepareHeader(fStreamHandle,fStreamHeader, 0);
  if Result <> 0 then
   begin
//   SetError('acmStreamUnprepareHeader', Result);
    Exit;
   end;

  // Close the stream
  Result := acmStreamClose(fStreamHandle, 0);
  if Result <> 0 then
   begin
//  SetError('acmStreamClose', Result);
    Exit;
   end;

 FreeMem(OutPut);
end;


*)

procedure TCustomACMChannel32DataCoder.LoadFromPointer(const Data: Pointer);
begin
 Move(Data^, FBlockBuffer^[0], FBlockSize);
// DeinterleaveData;
end;

procedure TCustomACMChannel32DataCoder.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FBlockBuffer^[0], FBlockSize);
// DeinterleaveData;
end;

procedure TCustomACMChannel32DataCoder.SaveToPointer(const Data: Pointer);
begin
// InterleaveData;
 Move(FBlockBuffer^[0], Data^, FBlockSize);
end;

procedure TCustomACMChannel32DataCoder.SaveToStream(Stream: TStream);
begin
// InterleaveData;
 Stream.Write(FBlockBuffer^[0], FBlockSize);
end;



{ TCustomChannel64DataCoder }

procedure TCustomChannel64DataCoder.AssignTo(Dest: TPersistent);
var
  Channel : Integer;
  Sample  : Integer;
begin
 inherited;
 if Dest is TCustomChannel32DataCoder then
  begin
   Assert(Length(FChannelArray) = Length(TCustomChannel32DataCoder(Dest).FChannelArray));
   for Channel := 0 to Length(FChannelArray) - 1 do
    for Sample := 0 to FSampleFrames - 1
     do TCustomChannel32DataCoder(Dest).FChannelArray[Channel, Sample] := FChannelArray[Channel, Sample];
  end else
 if Dest is TCustomChannel64DataCoder then
  begin
   Assert(Length(FChannelArray) = Length(TCustomChannel64DataCoder(Dest).FChannelArray));
   for Channel := 0 to Length(FChannelArray) - 1 do
    begin
     Move(FChannelArray[Channel, 0],
       TCustomChannel64DataCoder(Dest).FChannelArray[Channel, 0],
       FSampleFrames * SizeOf(Double));
    end;
  end;
end;

procedure TCustomChannel64DataCoder.ChannelCountChanged;
begin
 inherited;
 SetLength(FChannelArray, FChannelCount);
 ReallocateChannelMemory;
end;

constructor TCustomChannel64DataCoder.Create;
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FChannelArray) - 1
  do Dispose(FChannelArray[Channel]);
 inherited;
end;

function TCustomChannel64DataCoder.GetChannelPointer(
  Index: Integer): PDAVDoubleFixedArray;
begin
 if (Index >= 0) and (Index < Length(FChannelArray))
  then Result := FChannelArray[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TCustomChannel64DataCoder.ReallocateChannelMemory;
var
  i : Integer;
begin
 for i := 0 to Length(FChannelArray) - 1 do
  begin
   ReallocMem(FChannelArray[i], FSampleFrames * SizeOf(Double));
   FillChar(FChannelArray[i]^, FSampleFrames * SizeOf(Double), 0);
  end;
end;

{ TCustomPCMChannel64DataCoder }

procedure TCustomPCMChannel64DataCoder.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FBlockBuffer^[0], FBlockSize);
 DeinterleaveData;
end;

procedure TCustomPCMChannel64DataCoder.LoadFromPointer(const Data: Pointer);
begin
 Move(Data^, FBlockBuffer^[0], FBlockSize);
 DeinterleaveData;
end;

procedure TCustomPCMChannel64DataCoder.SaveToStream(Stream: TStream);
begin
 InterleaveData;
 Stream.Write(FBlockBuffer^[0], FBlockSize);
end;

procedure TCustomPCMChannel64DataCoder.SaveToPointer(const Data: Pointer);
begin
 InterleaveData;
 Move(FBlockBuffer^[0], Data^, FBlockSize);
end;

{ TCustomChannel64DataCoderFixedPoint }

constructor TCustomChannel64DataCoderFixedPoint.Create;
begin
 FDitherType := dtNone;
 FByteAlign  := baLeft;
 inherited;
 CalculateScaleFactors;
end;

procedure TCustomChannel64DataCoderFixedPoint.CalculateScaleFactors;
begin
 case FByteAlign of
  baLeft:
   begin
    FScaleFactor[0] := 1 shl (8 * ((FBits + 7) div 8) - 1) - 1;
    FScaleFactor[1] := 1 / FScaleFactor[0];
   end;
  baRight:
   begin
    if FBits = 32
     then FScaleFactor[0] := MaxInt
     else FScaleFactor[0] := (1 shl (FBits - 1)) - 1;
    FScaleFactor[1] := 1 / FScaleFactor[0];
   end;
  else Assert(False);
 end;
end;

procedure TCustomChannel64DataCoderFixedPoint.SetByteAlign(const Value: TByteAlign);
begin
 if FByteAlign <> Value then
  begin
   FByteAlign := Value;
   CalculateScaleFactors;
  end;
end;

procedure TCustomChannel64DataCoderFixedPoint.SetDitherType(const Value: TDitherType);
begin
 if FDitherType <> Value then
  begin
   FDitherType := Value;
  end;
end;

function TCustomChannel64DataCoderFixedPoint.CorrectBlocksize(const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * FSampleSize;
 Result := Granularity * (Value div Granularity);
end;

procedure TCustomChannel64DataCoderFixedPoint.CalculateSampleFrames;
begin
 SampleFrames := FBlockSize div FChannelCount div FSampleSize;
end;

procedure TCustomChannel64DataCoderFixedPoint.CalculateBlockSize;
begin
 BlockSize := FSampleFrames * ChannelCount * FSampleSize;
end;

procedure TCustomChannel64DataCoderFixedPoint.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomChannel32DataCoderFixedPoint then
  with TCustomChannel32DataCoderFixedPoint(Dest) do
   begin
    FDitherType  := Self.FDitherType;
    FBits        := Self.FBits;
    FSampleSize  := Self.FSampleSize;
    FScaleFactor := Self.FScaleFactor;
    FByteAlign   := Self.FByteAlign;
   end else
 if Dest is TCustomChannel64DataCoderFixedPoint then
  with TCustomChannel64DataCoderFixedPoint(Dest) do
   begin
    FDitherType  := Self.FDitherType;
    FBits        := Self.FBits;
    FSampleSize  := Self.FSampleSize;
    FScaleFactor := Self.FScaleFactor;
    FByteAlign   := Self.FByteAlign;
   end;
end;

procedure TCustomChannel64DataCoderFixedPoint.BlockSizeChanged;
begin
 CalculateSampleFrames;
 inherited;
end;

procedure TCustomChannel64DataCoderFixedPoint.ChannelCountChanged;
begin
 inherited;
 CalculateSampleFrames;
end;

procedure TCustomChannel64DataCoderFixedPoint.SampleFramesChanged;
begin
 CalculateBlockSize;
 ReallocateChannelMemory;
end;


function TCustomChannel64DataCoderFixedPoint.SampleToByte(
  Sample: Cardinal): Cardinal;
begin
 Result := Sample * FSampleSize;
end;

{ TChannel64DataCoderFixedPoint }

constructor TChannel64DataCoderFixedPoint.Create;
begin
 FBits       := 64;
 FSampleSize := 8;
 inherited;
end;

procedure TChannel64DataCoderFixedPoint.SetSampleSize(const Value: Byte);
begin
 if SampleSize <> Value then
  begin
   FSampleSize := Value;
   SampleSizeChanged;
  end;
end;

procedure TChannel64DataCoderFixedPoint.SampleSizeChanged;
begin
 if FBits > 8 * FSampleSize then
  begin
   FBits := FSampleSize * 8;
   BitsChanged;
  end;
 SampleFramesChanged;
end;

procedure TChannel64DataCoderFixedPoint.SetBits(const Value: Byte);
begin
 if Bits <> Value then
  begin
   FBits := Value;
   BitsChanged;
  end;
end;

procedure TChannel64DataCoderFixedPoint.BitsChanged;
begin
 if FBits > 8 * FSampleSize then
  begin
   FSampleSize := (FBits + 7) div 8;
   SampleSizeChanged;
  end;
 CalculateScaleFactors;
end;

procedure TChannel64DataCoderFixedPoint.SetBitsAndSampleSize(const Bits,
  SampleSize: Byte);
begin
 if (Bits > 8 * SampleSize)
  then raise Exception.Create('Number of bits must fit into the sample size!');

 FBits := Bits;
 FSampleSize := SampleSize;

 CalculateBlockSize;
 if BlockSize <> CorrectBlocksize(BlockSize)
  then BlockSize := CorrectBlocksize(BlockSize);

 CalculateScaleFactors;
end;

procedure TChannel64DataCoderFixedPoint.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
  DataInt : Integer;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * FSampleSize));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 case SampleSize of
  1: for Sample := 0 to FSampleFrames - 1 do
      for Channel := 0 to FChannelCount - 1
       do PDAVByteArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := 127 + round(FChannelArray[Channel]^[Sample] * FScaleFactor[0]);
  2: for Sample := 0 to FSampleFrames - 1 do
      for Channel := 0 to FChannelCount - 1
       do PDAVSmallIntArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := round(FChannelArray[Channel]^[Sample] * FScaleFactor[0]);
  3: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1 do
       begin
        DataInt := round(FChannelArray[Channel]^[Sample] * FScaleFactor[0]);// shl 8;
        Move(DataInt, PDAVByteArray(FBlockBuffer)^[3 * (Sample * FChannelCount + Channel)], 3);
       end;
  4: for Sample := 0 to FSampleFrames - 1 do
      for Channel := 0 to FChannelCount - 1
       do PIntegerArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := round(FChannelArray[Channel]^[Sample] * FScaleFactor[0]);
 end;
end;

procedure TChannel64DataCoderFixedPoint.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * FSampleSize));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 case SampleSize of
  1: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1
       do FChannelArray[Channel]^[Sample] := (PDAVByteArray(FBlockBuffer)^[Sample * FChannelCount + Channel] - 127) * FScaleFactor[1];
  2: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1
       do FChannelArray[Channel]^[Sample] := PDAVSmallIntArray(FBlockBuffer)^[Sample * FChannelCount + Channel] * FScaleFactor[1];
  3: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1
       do FChannelArray[Channel]^[Sample] := (ShortInt(PDAVByteArray(FBlockBuffer)^[3 * (Sample * FChannelCount + Channel) + 2]) shl 16 +
                                                       PDAVByteArray(FBlockBuffer)^[3 * (Sample * FChannelCount + Channel) + 1]  shl  8 +
                                                       PDAVByteArray(FBlockBuffer)^[3 * (Sample * FChannelCount + Channel)]) * FScaleFactor[1];
  4: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1
       do FChannelArray[Channel]^[Sample] := PIntegerArray(FBlockBuffer)^[Sample * FChannelCount + Channel] * FScaleFactor[1];
 end;
end;


{ TChannel64DataCoderFloat32 }

function TChannel64DataCoderFloat32.CorrectBlocksize(const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * SizeOf(Single);
 Assert(Granularity >= 1);
 Result := Granularity * ((Value + Granularity - 1) div Granularity);
end;

procedure TChannel64DataCoderFloat32.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(Single)));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do PDAVSingleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := FChannelArray[Channel]^[Sample];
end;

procedure TChannel64DataCoderFloat32.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(Single)));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do FChannelArray[Channel]^[Sample] := PDAVSingleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel];
end;

procedure TChannel64DataCoderFloat32.BlockSizeChanged;
begin
 inherited;
 SampleFrames := FBlockSize div FChannelCount div SizeOf(Single);
end;

procedure TChannel64DataCoderFloat32.SampleFramesChanged;
begin
 CalculateBlocksize;
end;

function TChannel64DataCoderFloat32.SampleToByte(Sample: Cardinal): Cardinal;
begin
 Result := Sample * SizeOf(Single);
end;

procedure TChannel64DataCoderFloat32.ChannelCountChanged;
begin
 inherited;
 CalculateBlocksize;
end;

procedure TChannel64DataCoderFloat32.CalculateBlocksize;
begin
 BlockSize := FSampleFrames * ChannelCount * SizeOf(Single);
 ReallocateChannelMemory;
end;

{ TChannel64DataCoderFloat64 }

function TChannel64DataCoderFloat64.CorrectBlocksize(const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * SizeOf(Double);
 Assert(Granularity >= 1);
 Result := Granularity * ((Value + Granularity - 1) div Granularity);
end;

procedure TChannel64DataCoderFloat64.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(Double)));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do PDAVDoubleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := FChannelArray[Channel]^[Sample];
end;

procedure TChannel64DataCoderFloat64.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 Assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(Double)));
 Assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do FChannelArray[Channel]^[Sample] := PDAVDoubleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel];
end;

procedure TChannel64DataCoderFloat64.BlockSizeChanged;
begin
 inherited;
 SampleFrames := FBlockSize div FChannelCount div SizeOf(Double);
end;

procedure TChannel64DataCoderFloat64.SampleFramesChanged;
begin
 BlockSize := FSampleFrames * ChannelCount * SizeOf(Double);
 ReallocateChannelMemory;
end;

function TChannel64DataCoderFloat64.SampleToByte(Sample: Cardinal): Cardinal;
begin
 Result := Sample * SizeOf(Double);
end;

end.

