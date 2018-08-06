{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_ZLib;

interface

{$I DAV_Compiler.inc}

uses
  Classes;

type
  TZCompressionMethod = (cmDeflate = 8);
  TZCompressionLevel = (clFastest = 0, clFast = 1, clDefault = 2,
    clMaximumCompression = 3);

  TBlockFormatType = (bfStored = 0, bfFixedHuffman = 1, bfDynamicHuffman = 2,
    bfReserved = 3);

  TCustomZStream = class(TStream)
  private
    FStream            : TStream;
    FStreamPos         : Int64;
  protected
    FCompressionMethod : TZCompressionMethod;
    FCompressionInfo   : Byte;
    FCompressionLevel  : TZCompressionLevel;
    FDictionaryPresent : Boolean;
    FDictionary        : Cardinal;
    FAdler32           : Cardinal;
  protected
    constructor Create(Stream: TStream);
  end;

(*
  TInflateBlock = class(TObject)
  private
    function ReadBitsFromStream(Bits: Byte): Byte;
    procedure ReadStoredFromStream;
  public
    constructor Create(Source: TStream; BitOffset: Byte);
    destructor Destroy; override;

    procedure ReadFromStream;

    property BlockFormatType: TBlockFormatType read FBlockFormatType;
    property IsFinalBlock: Boolean read FIsFinalBlock;
  end;
*)

  TZDecompressionStream = class(TCustomZStream)
  private
    procedure ReadHeader(Stream: TStream);
    procedure ReadAdler32(Stream: TStream);
    function GetBytesInBuffer: Cardinal;
    procedure BuildFixedHuffmanTables;
  protected
    FBuffer       : TMemoryStream;
    FBitPosition  : Integer;
    FCurrentByte  : Byte;
    FIsFinalBlock : Boolean;

    function ReadBitsFromStream8(Bits: Byte): Byte;
    function ReadBitsFromStream16(Bits: Byte): Word;
    procedure ReadBlockFromStream;
    procedure ReadStoredBlockFromStream;
    procedure ReadFixedHuffmanBlockFromStream;
    procedure ReadDynamicHuffmanBlockFromStream;
  public
    constructor Create(Source: TStream);
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Integer; Origin: Word): Integer; override;

    property CompressionMethod: TZCompressionMethod read FCompressionMethod;
    property CompressionInfo: Byte read FCompressionInfo;
    property CompressionLevel: TZCompressionLevel read FCompressionLevel;
    property DictionaryPresent: Boolean read FDictionaryPresent;
  end;

implementation

uses
  Math, SysUtils;

type
  PInflateHuft = ^TInflateHuft;
  TInflateHuft = record
    Exop : Byte;     // number of extra bits or operation
    Bits : Byte;     // number of bits in this code or subcode
    Base : Cardinal; // literal, length base, or distance base or table offset
  end;

const
  CCopyLengths: array [0..30] of Cardinal = (
    3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59,
    67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0);

  CCopyLiteralExtraBits: array [0..28] of Cardinal = (
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4,
    5, 5, 5, 5, 0);

  CCopyDistanceOffsets: array [0..29] of Cardinal = (
    1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385,
    513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577
  );

  CCopyDistanceExtraBits: array [0..29] of Cardinal = (
    0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7,
    7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13
  );

var
  GFixedLiteralTable: PInflateHuft;
  GFixedDistanceTable: PInflateHuft;


{ TCustomZStream }

constructor TCustomZStream.Create(Stream: TStream);
begin
 inherited Create;
 FStream := Stream;
 FStreamPos := Stream.Position;
end;


{ TZDecompressionStream }

constructor TZDecompressionStream.Create(Source: TStream);
begin
 inherited Create(Source);
 ReadHeader(Source);

 FBitPosition := 0;
 FBuffer := TMemoryStream.Create;

// ReadAdler32(Source);
end;

destructor TZDecompressionStream.Destroy;
begin
 // dispose buffer
 FreeAndNil(FBuffer);

 inherited;
end;

function TZDecompressionStream.GetBytesInBuffer: Cardinal;
begin
 Result := FBuffer.Size - FBuffer.Position;
end;

procedure TZDecompressionStream.ReadHeader(Stream: TStream);
var
  Values : array [0..1] of Byte;
begin
 with Stream do
  begin
   if Size - Position < 2
    then raise Exception.Create('Stream size too small');

   // read CMF value
   Read(Values[0], 1);

   // read and check compression method
   if (Values[0] and $F) <> 8
    then raise Exception.Create('Only the deflate compression method is supported so far');
   FCompressionMethod := cmDeflate;

   // read and check compression info
   FCompressionInfo := (Values[0] shr 4);
   if FCompressionInfo > 7
    then raise Exception.Create('Compression info values above 7 are not allowed');

   // read FLG value
   Read(Values[1], 1);

   // check header
   if (Values[0] * 256 + Values[1]) mod 31 <> 0
    then raise Exception.Create('The header seems to be corrupted');

   // check if a dictionary is present
   FDictionaryPresent := (Values[1] and (1 shl 5)) <> 0;

   // read compression level
   FCompressionLevel := TZCompressionLevel(Values[1] shr 6);

   // eventually read dictionary
   if FDictionaryPresent
    then Read(FDictionary, 4)
    else FDictionary := 0;
  end;
end;

procedure TZDecompressionStream.ReadAdler32(Stream: TStream);
begin
 with Stream do
  begin
   // seek end of the stream
   Seek(-4, soFromEnd);

   // read adler 32 value
   Read(FAdler32, 4);
  end;
end;

function TZDecompressionStream.ReadBitsFromStream8(Bits: Byte): Byte;
begin
 Assert(Bits <= 8);

 if Bits = 0 then Exit;

 if FBitPosition = 0
  then FStream.Read(FCurrentByte, 1);

 if FBitPosition + Bits > 8 then
  begin
   Result := FCurrentByte shr FBitPosition;
   Bits := Bits - (8 - FBitPosition);
   FStream.Read(FCurrentByte, 1);
   Result := Result or (FCurrentByte and (1 shl (Bits + 1) - 1));
   FBitPosition := Bits;
   Exit;
  end;

 Result := (FCurrentByte shr FBitPosition) and (1 shl (Bits + 1) - 1);
 FBitPosition := (FBitPosition + Bits) mod 8;



(*
 while Bits > 0 do
  begin
   if (FBitPosition mod 8 = 0) then
    begin
     FStream.Read(FCurrentByte, 1);
     FBitPosition := 0;
    end;

   Result := Result or (((FCurrentByte shr FBitPosition) and 1) shl Shift);
   Dec(Bits);
   Inc(FBitPosition);
   Inc(Shift);
  end;
*)
end;

function TZDecompressionStream.ReadBitsFromStream16(Bits: Byte): Word;
begin
 Assert(Bits <= 9);

 if Bits = 0 then Exit;

 if FBitPosition = 0
  then FStream.Read(FCurrentByte, 1);

 if FBitPosition + Bits > 8 then
  begin
   Result := FCurrentByte shr FBitPosition;
   Bits := Bits - FBitPosition;
   FStream.Read(FCurrentByte, 1);
   Result := Result or (FCurrentByte and (1 shl (Bits + 1) - 1));
   FBitPosition := Bits mod 8;
   Exit;
  end;

 Result := (FCurrentByte shr FBitPosition) and (1 shl (Bits + 1) - 1);
 FBitPosition := (FBitPosition + Bits) mod 8;
end;

procedure TZDecompressionStream.ReadBlockFromStream;
var
  BlockFormatType : TBlockFormatType;
begin
 FBuffer.Size := 0;
 FIsFinalBlock := ReadBitsFromStream8(1) <> 0;
 BlockFormatType := TBlockFormatType(ReadBitsFromStream8(2));

 case BlockFormatType of
  bfStored         : ReadStoredBlockFromStream;
  bfFixedHuffman   : ReadFixedHuffmanBlockFromStream;
  bfDynamicHuffman : ReadDynamicHuffmanBlockFromStream;
 end;
end;

procedure TZDecompressionStream.ReadDynamicHuffmanBlockFromStream;
begin

end;

procedure TZDecompressionStream.ReadStoredBlockFromStream;
var
  Length : Word;
  InvertedLength : Word;
begin
 FStream.Read(Length, 2);
 FStream.Read(InvertedLength, 2);
 if Length <> (not InvertedLength)
  then raise Exception.Create('Stored block size error!');

 FBuffer.CopyFrom(FStream, Length);
 FBuffer.Position := FBuffer.Position - (Length);
end;

procedure TZDecompressionStream.ReadFixedHuffmanBlockFromStream;
var
  BitValue     : Word;
  LiteralValue : Word;
  ByteValue    : Byte;
begin
// BuildFixedHuffmanTables;

 repeat
  // read current value
  BitValue := ReadBitsFromStream8(7);
  if BitValue > 23 then
   begin
    BitValue := (BitValue shl 1) or ReadBitsFromStream8(1);

    if (BitValue in [48..192])then
     begin
      LiteralValue := BitValue - 48;
     end else
    if (BitValue in [193..199])then
     begin
      LiteralValue := BitValue + 87;
     end
    else
     begin
      BitValue := (BitValue shl 1) or ReadBitsFromStream8(1);
      LiteralValue := BitValue - 400 + 144;
     end;
   end
  else LiteralValue := 256 + BitValue;

  if LiteralValue < 256 then
   begin
    ByteValue := LiteralValue;
    FBuffer.Write(ByteValue, 1);
   end;


 until LiteralValue = 256;

end;

procedure TZDecompressionStream.BuildFixedHuffmanTables;
var
  Index              : Integer;
  HuffmanTableLength : array [0..287] of Byte;
begin
 if not Assigned(GFixedLiteralTable) then
  begin
   for Index := 0   to 143 do HuffmanTableLength[Index] := 8;
   for Index := 144 to 255 do HuffmanTableLength[Index] := 9;
   for Index := 256 to 279 do HuffmanTableLength[Index] := 7;
   for Index := 280 to 287 do HuffmanTableLength[Index] := 8;
  end;

end;

function TZDecompressionStream.Read(var Buffer; Count: Integer): Integer;
var
  Value         : Byte;
  BytesCopied   : Cardinal;
  BytesInBuffer : Cardinal;
begin
 Result := 0;
 while Count > 0 do
  begin
   // check whether buffer contains data
   if FBuffer.Size = 0
    then ReadBlockFromStream;

   // check if decoded bytes are already stored in the buffer
   BytesInBuffer := FBuffer.Size - FBuffer.Position;
   if BytesInBuffer > 0 then
    begin
     // read bytes from the buffer
     BytesCopied := FBuffer.Read(Buffer, Min(BytesInBuffer, Count));
     Count := Count - BytesCopied;
     Result := Result + BytesCopied;

     // eventually clear the buffer in case the buffer has been read entirely
     if FBuffer.Position = FBuffer.Size then FBuffer.Clear;
    end;
  end;

(*
 with TInflateBlock.Create(FStream, 0) do
  try
   ReadFromStream;

  finally
   Free;
  end;
*)

end;

function TZDecompressionStream.Seek(Offset: Integer; Origin: Word): Integer;
begin
 if (Origin = soFromBeginning) and (Offset = 0) then
  begin
   // reset decompression
  end else
 if (Origin = soFromCurrent) and (Offset >= 0) then
  begin
   // locate position
  end else
 if (Offset = 0) and (Origin = soFromEnd) then
  begin
   // locate end of stream
  end
 else raise Exception.Create('Invalid seek operation');

 // not implemented yet
 // result := ?
end;

function TZDecompressionStream.Write(const Buffer; Count: Integer): Integer;
begin
 raise Exception.Create('The decompression stream is read-only!');
end;

(*
repeat
  read block header from input stream.

  if stored with no compression then
   begin
    skip any remaining bits in current partially
    processed byte
    read LEN and NLEN (see next section)
    copy LEN bytes of data to output
   end else
  if compressed with dynamic Huffman codes then
   begin
    read representation of code trees (see
    subsection below)
   end;
  loop (until end of block code recognized)
   begin
    decode literal/length value from input stream
    if value < 256
     then copy value (literal byte) to output stream
     else
    if value = end of block (256)
     then break from loop
     else (value = 257..285)
    decode distance from input stream
    move backwards distance bytes in the output
    stream, and copy length bytes from this
    position to the output stream.
   end loop
until last block
*)


(*
{ TInflateBlock }

constructor TInflateBlock.Create(Source: TStream; BitOffset: Byte);
begin
 inherited Create;
 FBuffer := TMemoryStream.Create;
 FSource := Source;
 FBitPosition := BitOffset;
end;

destructor TInflateBlock.Destroy;
begin
 FreeAndNil(FBuffer);
 inherited;
end;

function TInflateBlock.ReadBitsFromStream(Bits: Byte): Byte;
begin
 Assert(Bits > 0);
 Assert(Bits <= 8);
 Result := 0;

 while Bits > 0 do
  begin
   if (FBitPosition mod 8 = 0) then
    begin
     FSource.Read(FCurrentByte, 1);
     FBitPosition := 0;
    end;

   Result := Result + ((FCurrentByte and (1 shl FBitPosition)) shr FBitPosition) shl (8 - Bits);
   Dec(Bits);
   Inc(FBitPosition);
  end;
end;

procedure TInflateBlock.ReadFromStream;
var
  Value : Byte;
begin
 FBuffer.Size := 0;
 FIsFinalBlock := ReadBitsFromStream(1) <> 0;
 FBlockFormatType := TBlockFormatType(ReadBitsFromStream(2));

 if BlockFormatType = bfStored
  then ReadStoredFromStream;

end;

procedure TInflateBlock.ReadStoredFromStream;
var
  Length : Word;
  InvertedLength : Word;
begin
 FSource.Read(Length, 2);
 FSource.Read(InvertedLength, 2);
 Assert(Length = not InvertedLength);
end;
*)

end.
