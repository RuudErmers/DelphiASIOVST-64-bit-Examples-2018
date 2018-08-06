unit TestDAV_ZLib;

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

{$I DAV_Compiler.inc}

uses
  Classes, {$IFDEF FPC}fpcunit, testutils, testregistry, {$ELSE}
  TestFramework, {$ENDIF}DAV_ZLib;

type
  TestTZDecompressionStream = class(TTestCase)
  strict private
    FCompressedDataStream : TMemoryStream;
    FZDecompressionStream : TZDecompressionStream;

  protected
    procedure InitializeCompressedData;
    function TestPolynomial(Input: Word): Byte;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMZlib;
    procedure TestProperties;
    procedure TestRead;
    procedure TestSeek;
  end;

implementation

uses
  SysUtils, zlib, mzlib;

procedure TestTZDecompressionStream.SetUp;
begin
 // create and initialize compressed data stream
 FCompressedDataStream := TMemoryStream.Create;
 InitializeCompressedData;

 FCompressedDataStream.Position := 0;
 FZDecompressionStream := DAV_ZLib.TZDecompressionStream.Create(FCompressedDataStream);
end;

procedure TestTZDecompressionStream.TearDown;
begin
 FreeAndNil(FCompressedDataStream);
 FreeAndNil(FZDecompressionStream);
end;

procedure TestTZDecompressionStream.InitializeCompressedData;
var
  Index             : Integer;
  Value             : Byte;
  CompressionStream : zlib.TZCompressionStream;
begin
 CompressionStream := TZCompressionStream.Create(FCompressedDataStream, zcDefault, 15);
 for Index := 0 to High(Word) do
  begin
   Value := TestPolynomial(Index);
   CompressionStream.Write(Value, 1);
  end;
 FreeAndNil(CompressionStream);

// FCompressedDataStream.SaveToFile(ExtractFilePath(ParamStr(0)) + 'Dummy.z');
end;

procedure TestTZDecompressionStream.TestMZlib;
var
  ZState : mzlib.TZState;
  Index  : Integer;
  Buffer : array [Word] of Byte;
begin
 FillChar(ZState, SizeOf(ZState), 0);
 ZState.NextInput := FCompressedDataStream.Memory;
 ZState.AvailableInput := FCompressedDataStream.Size;
 InflateInit(ZState);

 ZState.NextOutput := @Buffer;
 ZState.AvailableOutput := Length(Buffer);
 mzlib.Inflate(ZState, Z_PARTIAL_FLUSH);

 // check whether the decompressed bytes have the expected value
 for Index := 0 to Length(Buffer) - 1 do
  begin
   CheckEquals(TestPolynomial(Index), Buffer[Index]);
  end;
end;

function TestTZDecompressionStream.TestPolynomial(Input: Word): Byte;
begin
// Result := Input xor $AC;
// Result := Input mod 4;
 Result := Input mod 64;
end;

procedure TestTZDecompressionStream.TestProperties;
begin
 CheckTrue(FZDecompressionStream.CompressionMethod = cmDeflate);
 CheckTrue(FZDecompressionStream.CompressionLevel = DAV_ZLib.clDefault);
end;

procedure TestTZDecompressionStream.TestRead;
var
 ReturnValue : Integer;
 Index       : Integer;
 Buffer      : array [Word] of Byte;
begin
 FillChar(Buffer[0], Length(Buffer), 0);

 ReturnValue := FZDecompressionStream.Read(Buffer[0], Length(Buffer));

 // check whether all bytes were decompressed
 CheckEquals(Length(Buffer), ReturnValue, 'A wrong amount of bytes were read');

 // check whether the decompressed bytes have the expected value
 for Index := 0 to Length(Buffer) - 1 do
  begin
   CheckEquals(TestPolynomial(Index), Buffer[Index]);
  end;
end;

procedure TestTZDecompressionStream.TestSeek;
var
 ReturnValue : Integer;
begin
 ReturnValue := FZDecompressionStream.Seek(0, soBeginning);

 // Todo: Check return value!
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTZDecompressionStream.Suite);

end.
