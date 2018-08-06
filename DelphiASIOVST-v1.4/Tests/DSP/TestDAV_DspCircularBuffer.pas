unit TestDAV_DspCircularBuffer;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  TestFramework, Classes, DAV_Types, DAV_Classes, DAV_Complex,
  DAV_DspCircularBuffer;

type
  // Test methods for class TCircularBuffer32
  TestTCircularBuffer32 = class(TTestCase)
  strict private
    FCircularBuffer32: TCircularBuffer32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
    procedure TestReadWriteBuffer;
  end;

  // Test methods for class TCircularBuffer64
  TestTCircularBuffer64 = class(TTestCase)
  strict private
    FCircularBuffer64: TCircularBuffer64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
    procedure TestReadWriteBuffer;
  end;

  // Test methods for class TCircularStereoBuffer32
  TestTCircularStereoBuffer32 = class(TTestCase)
  strict private
    FCircularStereoBuffer32: TCircularStereoBuffer32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
    procedure TestReadWriteBuffer;
  end;

  // Test methods for class TCircularStereoBuffer64
  TestTCircularStereoBuffer64 = class(TTestCase)
  strict private
    FCircularStereoBuffer64: TCircularStereoBuffer64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
    procedure TestReadWriteBuffer;
  end;

  // Test methods for class TCircularMultiBuffer32
  TestTCircularMultiBuffer32 = class(TTestCase)
  strict private
    FCircularMultiBuffer32: TCircularMultiBuffer32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
    procedure TestReadWriteBuffer;
  end;

  // Test methods for class TCircularMultiBuffer64
  TestTCircularMultiBuffer64 = class(TTestCase)
  strict private
    FCircularMultiBuffer64: TCircularMultiBuffer64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
    procedure TestReadWriteBuffer;
  end;

  // Test methods for class TCircularReserveMultiBuffer32
  TestTCircularReserveMultiBuffer32 = class(TTestCase)
  strict private
    FCircularReserveMultiBuffer32: TCircularReserveMultiBuffer32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
    procedure TestReadWriteBuffer;
    procedure TestReadWriteReserveBuffer;
  end;

  // Test methods for class TCircularReserveMultiBuffer64
  TestTCircularReserveMultiBuffer64 = class(TTestCase)
  strict private
    FCircularReserveMultiBuffer64: TCircularReserveMultiBuffer64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
    procedure TestReadWriteBuffer;
    procedure TestReadWriteReserveBuffer;
  end;

implementation

uses
  SysUtils;

{ TestTCircularBuffer32 }

procedure TestTCircularBuffer32.SetUp;
begin
 FCircularBuffer32 := TCircularBuffer32.Create(256);
end;

procedure TestTCircularBuffer32.TearDown;
begin
 FreeAndNil(FCircularBuffer32);
end;

procedure TestTCircularBuffer32.TestReset;
var
  ReturnValue : Integer;
  Sample      : Integer;
  Data        : PDAVSingleFixedArray;
begin
 with FCircularBuffer32 do
  begin
   GetMem(Data, BufferSize * SizeOf(Double));
   for Sample := 0 to BufferSize div 2 - 1
    do Data[Sample] := 2 * Random - 1;

   try
    ReturnValue := WriteBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    CheckEquals(BufferSize div 2, SamplesInBuffer);
    FCircularBuffer32.Reset;
    CheckEquals(0, SamplesInBuffer);
   finally
    Dispose(Data);
   end;
  end;
end;

procedure TestTCircularBuffer32.TestReadWriteBuffer;
var
  ReturnValue: Integer;
  Data: PDAVSingleFixedArray;
begin
 with FCircularBuffer32 do
  begin
   GetMem(Data, BufferSize * SizeOf(Double));
   try
    // write and read half a buffer
    ReturnValue := WriteBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    ReturnValue := ReadBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);

    // write and read an entire buffer
    ReturnValue := WriteBuffer(Data, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
    ReturnValue := ReadBuffer(Data, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
   finally
    Dispose(Data);
   end;
  end;
end;


{ TestTCircularBuffer64 }

procedure TestTCircularBuffer64.SetUp;
begin
 FCircularBuffer64 := TCircularBuffer64.Create(256);
end;

procedure TestTCircularBuffer64.TearDown;
begin
 FreeAndNil(FCircularBuffer64);
end;

procedure TestTCircularBuffer64.TestReset;
var
  ReturnValue : Integer;
  Sample      : Integer;
  Data        : PDAVDoubleFixedArray;
begin
 with FCircularBuffer64 do
  begin
   GetMem(Data, BufferSize * SizeOf(Double));
   for Sample := 0 to BufferSize div 2 - 1
    do Data[Sample] := 2 * Random - 1;
   try
    ReturnValue := WriteBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    CheckEquals(BufferSize div 2, SamplesInBuffer);
    Reset;
    CheckEquals(0, SamplesInBuffer);
   finally
    Dispose(Data);
   end;
  end;
end;

procedure TestTCircularBuffer64.TestReadWriteBuffer;
var
  ReturnValue: Integer;
  Data: PDAVDoubleFixedArray;
begin
 with FCircularBuffer64 do
  begin
   GetMem(Data, BufferSize * SizeOf(Double));
   try
    // write and read half a buffer
    ReturnValue := WriteBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    ReturnValue := ReadBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);

    // write and read an entire buffer
    ReturnValue := WriteBuffer(Data, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
    ReturnValue := ReadBuffer(Data, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
   finally
    Dispose(Data);
   end;
  end;
end;


{ TestTCircularStereoBuffer32 }

procedure TestTCircularStereoBuffer32.SetUp;
begin
 FCircularStereoBuffer32 := TCircularStereoBuffer32.Create(256);
end;

procedure TestTCircularStereoBuffer32.TearDown;
begin
 FreeAndNil(FCircularStereoBuffer32);
end;

procedure TestTCircularStereoBuffer32.TestReset;
var
  ReturnValue: Integer;
  Right: PDAVSingleFixedArray;
  Left: PDAVSingleFixedArray;
begin
 // TODO: Setup method call parameters
 with FCircularStereoBuffer32 do
  begin
   GetMem(Left, BufferSize * SizeOf(Single));
   GetMem(Right, BufferSize * SizeOf(Single));
   try
    ReturnValue := WriteBuffer(Left, Right, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    CheckEquals(BufferSize div 2, SamplesInBuffer);
    Reset;
    CheckEquals(0, SamplesInBuffer);
   finally
    Dispose(Left);
    Dispose(Right);
   end;
  end;
end;

procedure TestTCircularStereoBuffer32.TestReadWriteBuffer;
var
  ReturnValue: Integer;
  Right: PDAVSingleFixedArray;
  Left: PDAVSingleFixedArray;
begin
 with FCircularStereoBuffer32 do
  begin
   GetMem(Left, BufferSize * SizeOf(Single));
   GetMem(Right, BufferSize * SizeOf(Single));
   try
    // write and read half a buffer
    ReturnValue := WriteBuffer(Left, Right, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    ReturnValue := ReadBuffer(Left, Right, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);

    // write and read an entire buffer
    ReturnValue := WriteBuffer(Left, Right, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
    ReturnValue := ReadBuffer(Left, Right, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
   finally
    Dispose(Left);
    Dispose(Right);
   end;
  end;
end;


{ TestTCircularStereoBuffer64 }

procedure TestTCircularStereoBuffer64.SetUp;
begin
 FCircularStereoBuffer64 := TCircularStereoBuffer64.Create(256);
end;

procedure TestTCircularStereoBuffer64.TearDown;
begin
 FreeAndNil(FCircularStereoBuffer64);
end;

procedure TestTCircularStereoBuffer64.TestReset;
var
  ReturnValue: Integer;
  Right: PDAVDoubleFixedArray;
  Left: PDAVDoubleFixedArray;
begin
 with FCircularStereoBuffer64 do
  begin
   GetMem(Left, BufferSize * SizeOf(Double));
   GetMem(Right, BufferSize * SizeOf(Double));
   try
    ReturnValue := WriteBuffer(Left, Right, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    CheckEquals(BufferSize div 2, SamplesInBuffer);
    Reset;
    CheckEquals(0, SamplesInBuffer);
   finally
    Dispose(Left);
    Dispose(Right);
   end;
  end;
 // TODO: Validate method results
end;

procedure TestTCircularStereoBuffer64.TestReadWriteBuffer;
var
  ReturnValue: Integer;
  Right: PDAVDoubleFixedArray;
  Left: PDAVDoubleFixedArray;
begin
 with FCircularStereoBuffer64 do
  begin
   GetMem(Left, BufferSize * SizeOf(Double));
   GetMem(Right, BufferSize * SizeOf(Double));
   try
    // write and read half a buffer
    ReturnValue := WriteBuffer(Left, Right, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    ReturnValue := ReadBuffer(Left, Right, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);

    // write and read an entire buffer
    ReturnValue := WriteBuffer(Left, Right, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
    ReturnValue := ReadBuffer(Left, Right, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
   finally
    Dispose(Left);
    Dispose(Right);
   end;
  end;
end;


{ TestTCircularMultiBuffer32 }

procedure TestTCircularMultiBuffer32.SetUp;
begin
 FCircularMultiBuffer32 := TCircularMultiBuffer32.Create(256);
end;

procedure TestTCircularMultiBuffer32.TearDown;
begin
 FreeAndNil(FCircularMultiBuffer32);
end;

procedure TestTCircularMultiBuffer32.TestReset;
begin
 FCircularMultiBuffer32.Reset;
 // TODO: Validate method results
end;

procedure TestTCircularMultiBuffer32.TestReadWriteBuffer;
var
  ReturnValue: Integer;
  Data: TDAVArrayOfSingleFixedArray;
begin
 // TODO: Setup method call parameters
 with FCircularMultiBuffer32 do
  begin
   SetLength(Data, ChannelCount);
   GetMem(Data[0], BufferSize * SizeOf(Single));
   try
    // write and read half a buffer
    ReturnValue := WriteBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    ReturnValue := ReadBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);

    // write and read an entire buffer
    ReturnValue := WriteBuffer(Data, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
    ReturnValue := ReadBuffer(Data, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
   finally
    Dispose(Data[0]);
   end;
  end;
 // TODO: Validate method results
end;


{ TestTCircularMultiBuffer64 }

procedure TestTCircularMultiBuffer64.SetUp;
begin
 FCircularMultiBuffer64 := TCircularMultiBuffer64.Create(256);
end;

procedure TestTCircularMultiBuffer64.TearDown;
begin
 FreeAndNil(FCircularMultiBuffer64);
end;

procedure TestTCircularMultiBuffer64.TestReset;
begin
 FCircularMultiBuffer64.Reset;
 // TODO: Validate method results
end;

procedure TestTCircularMultiBuffer64.TestReadWriteBuffer;
var
  ReturnValue: Integer;
  Data: TDAVArrayOfDoubleFixedArray;
begin
 // TODO: Setup method call parameters
 with FCircularMultiBuffer64 do
  begin
   SetLength(Data, ChannelCount);
   GetMem(Data[0], BufferSize * SizeOf(Double));
   try
    // write and read half a buffer
    ReturnValue := WriteBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    ReturnValue := ReadBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);

    // write and read an entire buffer
    ReturnValue := WriteBuffer(Data, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
    ReturnValue := ReadBuffer(Data, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
   finally
    Dispose(Data[0]);
   end;
  end;
 // TODO: Validate method results
end;


{ TestTCircularReserveMultiBuffer32 }

procedure TestTCircularReserveMultiBuffer32.SetUp;
begin
 FCircularReserveMultiBuffer32 := TCircularReserveMultiBuffer32.Create(256);
end;

procedure TestTCircularReserveMultiBuffer32.TearDown;
begin
 FreeAndNil(FCircularReserveMultiBuffer32)
end;

procedure TestTCircularReserveMultiBuffer32.TestReset;
begin
 FCircularReserveMultiBuffer32.Reset;
 // TODO: Validate method results
end;

procedure TestTCircularReserveMultiBuffer32.TestReadWriteBuffer;
var
  ReturnValue: Integer;
  Data: TDAVArrayOfSingleFixedArray;
begin
 // TODO: Setup method call parameters
 with FCircularReserveMultiBuffer32 do
  begin
   SetLength(Data, ChannelCount);
   GetMem(Data[0], BufferSize * SizeOf(Single));
   try
    ReturnValue := WriteBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    ReturnValue := ReadBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
   finally
    Dispose(Data[0]);
   end;
  end;
 // TODO: Validate method results
end;

procedure TestTCircularReserveMultiBuffer32.TestReadWriteReserveBuffer;
var
  ReturnValue: Integer;
  Data: TDAVArrayOfSingleFixedArray;
begin
 // TODO: Setup method call parameters
 with FCircularReserveMultiBuffer32 do
  begin
   SetLength(Data, ChannelCount);
   GetMem(Data[0], BufferSize * SizeOf(Single));
   try
    // write and read half a buffer
    ReturnValue := WriteReserveBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    ReturnValue := ReadReserveBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);

    // write and read an entire buffer
    ReturnValue := WriteBuffer(Data, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
    ReturnValue := ReadBuffer(Data, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
   finally
    Dispose(Data[0]);
   end;
  end;
 // TODO: Validate method results
end;


{ TestTCircularReserveMultiBuffer64 }

procedure TestTCircularReserveMultiBuffer64.SetUp;
begin
 FCircularReserveMultiBuffer64 := TCircularReserveMultiBuffer64.Create(256);
end;

procedure TestTCircularReserveMultiBuffer64.TearDown;
begin
 FreeAndNil(FCircularReserveMultiBuffer64);
end;

procedure TestTCircularReserveMultiBuffer64.TestReset;
begin
 FCircularReserveMultiBuffer64.Reset;
 // TODO: Validate method results
end;

procedure TestTCircularReserveMultiBuffer64.TestReadWriteBuffer;
var
  ReturnValue: Integer;
  Data: TDAVArrayOfDoubleFixedArray;
begin
 // TODO: Setup method call parameters
 with FCircularReserveMultiBuffer64 do
  begin
   SetLength(Data, ChannelCount);
   GetMem(Data[0], BufferSize * SizeOf(Double));
   try
    // write and read half a buffer
    ReturnValue := WriteBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    ReturnValue := ReadBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);

    // write and read an entire buffer
    ReturnValue := WriteBuffer(Data, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
    ReturnValue := ReadBuffer(Data, BufferSize);
    CheckEquals(BufferSize, ReturnValue);
   finally
    Dispose(Data[0]);
   end;
  end;
 // TODO: Validate method results
end;

procedure TestTCircularReserveMultiBuffer64.TestReadWriteReserveBuffer;
var
  ReturnValue: Integer;
  Data: TDAVArrayOfDoubleFixedArray;
begin
 // TODO: Setup method call parameters
 with FCircularReserveMultiBuffer64 do
  begin
   SetLength(Data, ChannelCount);
   GetMem(Data[0], BufferSize * SizeOf(Double));
   try
    ReturnValue := WriteReserveBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
    ReturnValue := ReadReserveBuffer(Data, BufferSize div 2);
    CheckEquals(BufferSize div 2, ReturnValue);
   finally
    Dispose(Data[0]);
   end;
  end;
 // TODO: Validate method results
end;

procedure RegisterTestSuite;
var
  TS : TTestSuite;
begin
 TS := TTestSuite.Create('Circular Buffer Tests');
 TS.AddSuite(TestTCircularBuffer32.Suite);
 TS.AddSuite(TestTCircularBuffer64.Suite);
 TS.AddSuite(TestTCircularStereoBuffer32.Suite);
 TS.AddSuite(TestTCircularStereoBuffer64.Suite);
 TS.AddSuite(TestTCircularMultiBuffer32.Suite);
 TS.AddSuite(TestTCircularMultiBuffer64.Suite);
 TS.AddSuite(TestTCircularReserveMultiBuffer32.Suite);
 TS.AddSuite(TestTCircularReserveMultiBuffer64.Suite);
 RegisterTest(TS);
end;

initialization
  RegisterTestSuite;

end.
