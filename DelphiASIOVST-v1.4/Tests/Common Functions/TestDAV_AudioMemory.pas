unit TestDAV_AudioMemory;

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

uses
  Classes, SysUtils, TestFramework, DAV_AudioMemory, DAV_Classes, DAV_Types;

type
 TestTAudioMemory32 = class(TTestCase)
 strict private
   FAudioMemory32: TAudioMemory32;
 public
   procedure SetUp; override;
   procedure TearDown; override;
 published
   procedure TestClear;
   procedure TestScale32;
   procedure TestScale64;
   procedure TestOffset32;
   procedure TestOffset64;
   procedure TestExternal;
 end;

 TestTAudioMemory64 = class(TTestCase)
 strict private
   FAudioMemory64: TAudioMemory64;
 public
   procedure SetUp; override;
   procedure TearDown; override;
 published
   procedure TestClear;
   procedure TestScale32;
   procedure TestScale64;
   procedure TestOffset32;
   procedure TestOffset64;
   procedure TestExternal;
 end;

implementation

const
  CTestSampleCount = 1 shl 16;

{ TestTAudioMemory32 }

procedure TestTAudioMemory32.SetUp;
begin
 FAudioMemory32 := TAudioMemory32.Create;
 FAudioMemory32.SampleCount := CTestSampleCount;
end;

procedure TestTAudioMemory32.TearDown;
begin
 FreeAndNil(FAudioMemory32);
end;

procedure TestTAudioMemory32.TestClear;
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do FAudioMemory32.DataPointer^[SampleIndex] := 1;

 FAudioMemory32.Clear;

 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do CheckEquals(0, FAudioMemory32.DataPointer^[SampleIndex]);
end;

procedure TestTAudioMemory32.TestExternal;
var
  Data        : PDAVSingleFixedArray;
  SampleIndex : Integer;
begin
 GetMem(Data, CTestSampleCount * SizeOf(Single));
 try
  FAudioMemory32.DataPointer := @Data^[0];

  // perform a simple test
  TestClear;

  // perform some more tests
  TestScale32;
  TestOffset32;

  // store some data
  for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
   do FAudioMemory32.DataPointer^[SampleIndex] := 2;

  // change to internal memory
  FAudioMemory32.ExternalData := False;

  // check if data is transfered correctly
  for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
   do CheckEquals(2, FAudioMemory32.DataPointer^[SampleIndex]);

  // perform a simple test
  TestClear;

  // perform some more tests
  TestScale32;
  TestOffset32;

 finally
  Dispose(Data);
 end;
end;

procedure TestTAudioMemory32.TestScale32;
var
  SampleIndex : Integer;
  Value       : Single;
begin
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do FAudioMemory32.DataPointer^[SampleIndex] := 1;

 Value := 2;
 FAudioMemory32.Scale32(Value);
 Value := 0.5;
 FAudioMemory32.Scale32(Value);

 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do CheckEquals(1, FAudioMemory32.DataPointer^[SampleIndex]);
end;

procedure TestTAudioMemory32.TestScale64;
var
  SampleIndex : Integer;
  Value       : Double;
begin
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do FAudioMemory32.DataPointer^[SampleIndex] := 1;

 Value := 2;
 FAudioMemory32.Scale64(Value);
 Value := 0.5;
 FAudioMemory32.Scale64(Value);

 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do CheckEquals(1, FAudioMemory32.DataPointer^[SampleIndex]);
end;

procedure TestTAudioMemory32.TestOffset32;
var
  SampleIndex : Integer;
  Value       : Single;
begin
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do FAudioMemory32.DataPointer^[SampleIndex] := 1;

 Value := 1;
 FAudioMemory32.Offset32(Value);
 Value := -1;
 FAudioMemory32.Offset32(Value);

 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do CheckEquals(1, FAudioMemory32.DataPointer^[SampleIndex]);
end;

procedure TestTAudioMemory32.TestOffset64;
var
  SampleIndex : Integer;
  Value       : Double;
begin
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do FAudioMemory32.DataPointer^[SampleIndex] := 1;

 Value := 1;
 FAudioMemory32.Offset64(Value);
 Value := -1;
 FAudioMemory32.Offset64(Value);

 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do CheckEquals(1, FAudioMemory32.DataPointer^[SampleIndex]);
end;


{ TestTAudioMemory64 }

procedure TestTAudioMemory64.SetUp;
begin
 FAudioMemory64 := TAudioMemory64.Create;
 FAudioMemory64.SampleCount := CTestSampleCount;
end;

procedure TestTAudioMemory64.TearDown;
begin
 FreeAndNil(FAudioMemory64);
end;

procedure TestTAudioMemory64.TestClear;
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do FAudioMemory64.DataPointer^[SampleIndex] := 1;

 FAudioMemory64.Clear;

 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals(0, FAudioMemory64.DataPointer^[SampleIndex]);
end;

procedure TestTAudioMemory64.TestExternal;
var
  Data        : PDAVDoubleFixedArray;
  SampleIndex : Integer;
begin
 GetMem(Data, CTestSampleCount * SizeOf(Double));
 try
  FAudioMemory64.DataPointer := @Data^[0];

  // perform a simple test
  TestClear;

  // perform some more tests
  TestScale64;
  TestOffset64;

  // store some data
  for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
   do FAudioMemory64.DataPointer^[SampleIndex] := 2;

  // change to internal memory
  FAudioMemory64.ExternalData := False;

  // check if data is transfered correctly
  for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
   do CheckEquals(2, FAudioMemory64.DataPointer^[SampleIndex]);

  // perform a simple test
  TestClear;

  // perform some more tests
  TestScale64;
  TestOffset64;

 finally
  Dispose(Data);
 end;
end;

procedure TestTAudioMemory64.TestScale32;
var
  SampleIndex : Integer;
  Value       : Single;
begin
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do FAudioMemory64.DataPointer^[SampleIndex] := 1;

 Value := 2;
 FAudioMemory64.Scale32(Value);
 Value := 0.5;
 FAudioMemory64.Scale32(Value);

 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals(1, FAudioMemory64.DataPointer^[SampleIndex]);
end;

procedure TestTAudioMemory64.TestScale64;
var
  SampleIndex : Integer;
  Value       : Double;
begin
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do FAudioMemory64.DataPointer^[SampleIndex] := 1;

 Value := 2;
 FAudioMemory64.Scale64(Value);
 Value := 0.5;
 FAudioMemory64.Scale64(Value);

 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals(1, FAudioMemory64.DataPointer^[SampleIndex]);
end;

procedure TestTAudioMemory64.TestOffset32;
var
  SampleIndex : Integer;
  Value       : Single;
begin
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do FAudioMemory64.DataPointer^[SampleIndex] := 1;

 Value := 1;
 FAudioMemory64.Offset32(Value);
 Value := -1;
 FAudioMemory64.Offset32(Value);

 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals(1, FAudioMemory64.DataPointer^[SampleIndex]);
end;

procedure TestTAudioMemory64.TestOffset64;
var
  SampleIndex : Integer;
  Value       : Double;
begin
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do FAudioMemory64.DataPointer^[SampleIndex] := 1;

 Value := 1;
 FAudioMemory64.Offset64(Value);
 Value := -1;
 FAudioMemory64.Offset64(Value);

 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals(1, FAudioMemory64.DataPointer^[SampleIndex]);
end;

initialization
  RegisterTest(TestTAudioMemory32.Suite);
  RegisterTest(TestTAudioMemory64.Suite);

end.
