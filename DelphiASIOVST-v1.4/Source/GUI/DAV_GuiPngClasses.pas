unit DAV_GuiPngClasses;

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
  Classes, SysUtils, {$IFDEF FPC} ZBase, ZDeflate, ZInflate; {$ELSE}ZLib; {$ENDIF}

type
  EPngError = class(Exception);
  {$IFDEF FPC}
  TZStreamRec = z_stream;
  {$ENDIF}

function ReadSwappedWord(Stream: TStream): Word;
function ReadSwappedSmallInt(Stream: TStream): SmallInt;
function ReadSwappedCardinal(Stream: TStream): Cardinal;
procedure WriteSwappedWord(Stream: TStream; Value: Word);
procedure WriteSwappedSmallInt(Stream: TStream; Value: SmallInt);
procedure WriteSwappedCardinal(Stream: TStream; Value: Cardinal);

procedure ZCompress(Data: Pointer; Size: Integer; const Output: TStream;
  Level: Byte = Z_BEST_COMPRESSION); overload;
procedure ZCompress(const Input: TMemoryStream; const Output: TStream;
  Level: Byte = Z_BEST_COMPRESSION); overload;
procedure ZDecompress(Data: Pointer; Size: Integer; const Output: TStream); overload;
procedure ZDecompress(const Input: TMemoryStream; const Output: TStream); overload;

implementation

uses
  DAV_Common;

function ReadSwappedWord(Stream: TStream): Word;
begin
 {$IFDEF ValidateEveryReadOperation}
 if Stream.Read(Result, SizeOf(Word)) <> SizeOf(Word)
  then raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
 {$ELSE}
 Stream.Read(Result, SizeOf(Word));
 {$ENDIF}
 Result := Swap16(Result);
end;

function ReadSwappedSmallInt(Stream: TStream): SmallInt;
begin
 {$IFDEF ValidateEveryReadOperation}
 if Stream.Read(Result, SizeOf(SmallInt)) <> SizeOf(SmallInt)
  then raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
 {$ELSE}
 Stream.Read(Result, SizeOf(SmallInt));
 {$ENDIF}
 Result := Swap16(Result);
end;

function ReadSwappedCardinal(Stream: TStream): Cardinal;
begin
 {$IFDEF ValidateEveryReadOperation}
 if Stream.Read(Result, SizeOf(Cardinal)) <> SizeOf(Cardinal)
  then raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
 {$ELSE}
 Stream.Read(Result, SizeOf(Cardinal));
 {$ENDIF}
 Result := Swap32(Result);
end;

procedure WriteSwappedWord(Stream: TStream; Value: Word);
begin
 Value := Swap16(Value);
 Stream.Write(Value, SizeOf(Word));
end;

procedure WriteSwappedSmallInt(Stream: TStream; Value: SmallInt);
begin
 Value := Swap16(Value);
 Stream.Write(Value, SizeOf(SmallInt));
end;

procedure WriteSwappedCardinal(Stream: TStream; Value: Cardinal);
begin
 Value := Swap32(Value);
 Stream.Write(Value, SizeOf(Cardinal));
end;


{ zlib functions }

procedure ZCompress(Data: Pointer; Size: Integer; const Output: TStream;
  Level: Byte = Z_BEST_COMPRESSION); overload;
const
  CBufferSize = $8000;
var
  ZStreamRecord : TZStreamRec;
  ZResult       : Integer;
  TempBuffer    : Pointer;
begin
 FillChar(ZStreamRecord, SizeOf(TZStreamRec), 0);

 with ZStreamRecord do
  begin
   next_in := Data;
   avail_in := Size;
   {$IFNDEF FPC}
   {$IFNDEF ZLibEx}
   zalloc := zlibAllocMem;
   zfree := zlibFreeMem;
   {$ENDIF}
   {$ENDIF}
  end;

 {$IFDEF FPC}
 if DeflateInit_(@ZStreamRecord, Level, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0
  then raise EPngError.Create('Error during compression');
 {$ELSE}
 if DeflateInit_(ZStreamRecord, Level, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0
  then raise EPngError.Create('Error during compression');
 {$ENDIF}

 GetMem(TempBuffer, CBufferSize);
 try
  while ZStreamRecord.avail_in > 0 do
   begin
    ZStreamRecord.next_out := TempBuffer;
    ZStreamRecord.avail_out := CBufferSize;

    deflate(ZStreamRecord, Z_NO_FLUSH);

    Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
   end;

  repeat
   ZStreamRecord.next_out := TempBuffer;
   ZStreamRecord.avail_out := CBufferSize;

   ZResult := deflate(ZStreamRecord, Z_FINISH);

   Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
  until (ZResult = Z_STREAM_END) and (ZStreamRecord.avail_out > 0);
 finally
  Dispose(TempBuffer);
 end;

 if deflateEnd(ZStreamRecord) > 0
  then raise EPngError.Create('Error on stream validation');
end;

procedure ZCompress(const Input: TMemoryStream; const Output: TStream;
  Level: Byte = Z_BEST_COMPRESSION); overload;
begin
 ZCompress(Input.Memory, Input.Size, Output, Level);
end;

procedure ZDecompress(Data: Pointer; Size: Integer; const Output: TStream); overload;
const
  CBufferSize = $8000;
var
  ZStreamRecord : TZStreamRec;
  ZResult       : Integer;
  TempBuffer    : Pointer;
begin
 FillChar(ZStreamRecord, SizeOf(TZStreamRec), 0);

 with ZStreamRecord do
  begin
   next_in := Data;
   avail_in := Size;
   {$IFNDEF FPC}
   {$IFNDEF ZLibEx}
   zalloc := zlibAllocMem;
   zfree := zlibFreeMem;
   {$ENDIF}
   {$ENDIF}
  end;

 {$IFDEF FPC}
 if inflateInit_(@ZStreamRecord, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0
  then raise EPngError.Create('Error during decompression');
 {$ELSE}
 if inflateInit_(ZStreamRecord, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0
  then raise EPngError.Create('Error during decompression');
 {$ENDIF}

 GetMem(TempBuffer, CBufferSize);
 try
  while ZStreamRecord.avail_in > 0 do
   begin
    ZStreamRecord.next_out := TempBuffer;
    ZStreamRecord.avail_out := CBufferSize;

    inflate(ZStreamRecord, Z_NO_FLUSH);

    Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
   end;

  repeat
   ZStreamRecord.next_out := TempBuffer;
   ZStreamRecord.avail_out := CBufferSize;

   ZResult := inflate(ZStreamRecord, Z_FINISH);

   Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
  until (ZResult = Z_STREAM_END) and (ZStreamRecord.avail_out > 0);
 finally
  Dispose(TempBuffer);
 end;

 if inflateEnd(ZStreamRecord) > 0
  then raise EPngError.Create('Error on stream validation');
end;

procedure ZDecompress(const Input: TMemoryStream; const Output: TStream); overload;
begin
 ZDecompress(Input.Memory, Input.Size, Output);
end;

end.
