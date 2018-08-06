unit TestDAV_BlockConvert64;

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
  Classes, SysUtils, TestFramework, DAV_AudioMemory, DAV_Classes, DAV_Types,
  DAV_Bindings;

type
  TTestFunction = procedure(Destination, Source: Pointer; Count: Integer);

  TCustomTestConvertFloat64 = class(TTestCase)
  strict protected
    FAudioMemory64   : TAudioMemory64;
    FData            : Pointer;
    FFunctionBinding : TFunctionBinding;
    FMaxIntValue     : Integer;
    FDelta           : Double;
    FScale           : Double;
  protected
    procedure FillData; virtual; abstract;
    procedure PerformZeroTest; virtual; abstract;
    procedure PerformSimpleTest; virtual; abstract;
    procedure PerformSpeedTest; virtual; abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TCustomTestConvertToFloat64 = class(TCustomTestConvertFloat64)
  protected
    procedure PerformZeroTest; override;
    procedure PerformSimpleTest; override;
    procedure PerformSpeedTest; override;
  end;

  TCustomTestConvertInt16ToFloat64 = class(TCustomTestConvertToFloat64)
  protected
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure BasicTest;
    procedure NativeTest;
    procedure UnalignedBufferTest;
    procedure SpeedTestNative;
    procedure SpeedTestSSE2;
  end;

  TTestConvertInt16LSBToFloat64 = class(TCustomTestConvertInt16ToFloat64)
  protected
    procedure FillData; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt16MSBToFloat64 = class(TCustomTestConvertInt16ToFloat64)
  protected
    procedure FillData; override;
  public
    procedure SetUp; override;
  end;

  TCustomTestConvertInt24LSBToFloat64 = class(TCustomTestConvertToFloat64)
  protected
    procedure PerformSimpleTest; override;
    procedure PerformSpeedTest; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure BasicTest;
    procedure NativeTest;
    procedure UnalignedBufferTest;
    procedure SpeedTestNative;
    procedure SpeedTestSSE2;
  end;

  TTestConvertInt24LSBToFloat64 = class(TCustomTestConvertInt24LSBToFloat64)
  protected
    procedure FillData; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt24MSBToFloat64 = class(TCustomTestConvertInt24LSBToFloat64)
  protected
    procedure FillData; override;
  public
    procedure SetUp; override;
  end;

  TCustomTestConvertInt32ToFloat64 = class(TCustomTestConvertToFloat64)
  protected
    procedure PerformSimpleTest; override;
    procedure PerformSpeedTest; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure BasicTest;
    procedure NativeTest;
    procedure UnalignedBufferTest;
    procedure SpeedTestNative;
    procedure SpeedTestSSE2;
  end;

  TCustomTestConvertInt32LSBToFloat64 = class(TCustomTestConvertInt32ToFloat64)
  protected
    procedure FillData; override;
  end;

  TCustomTestConvertInt32MSBToFloat64 = class(TCustomTestConvertInt32ToFloat64)
  protected
    procedure FillData; override;
  end;

  TTestConvertInt32LSBToFloat64 = class(TCustomTestConvertInt32LSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB16ToFloat64 = class(TCustomTestConvertInt32LSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB18ToFloat64 = class(TCustomTestConvertInt32LSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB20ToFloat64 = class(TCustomTestConvertInt32LSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB24ToFloat64 = class(TCustomTestConvertInt32LSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSBToFloat64 = class(TCustomTestConvertInt32MSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB16ToFloat64 = class(TCustomTestConvertInt32MSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB18ToFloat64 = class(TCustomTestConvertInt32MSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB20ToFloat64 = class(TCustomTestConvertInt32MSBToFloat64)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB24ToFloat64 = class(TCustomTestConvertInt32MSBToFloat64)
  public
    procedure SetUp; override;
  end;


  // from float

  TCustomTestConvertFromFloat64 = class(TCustomTestConvertFloat64)
  protected
    procedure FillData; override;
    procedure PerformZeroTest; override;
  end;

  TCustomTestConvertInt16FromFloat64 = class(TCustomTestConvertFromFloat64)
  protected
    procedure PerformSimpleTest; override;
    function DataPostProcessing(Data: Word): Word; virtual; abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure BasicTest;
    procedure NativeTest;
    procedure UnalignedBufferTest;
    procedure SpeedTestNative;
    procedure SpeedTestSSE2;
  end;

  TTestConvertInt16LSBFromFloat64 = class(TCustomTestConvertInt16FromFloat64)
  protected
    function DataPostProcessing(Data: Word): Word; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt16MSBFromFloat64 = class(TCustomTestConvertInt16FromFloat64)
  protected
    function DataPostProcessing(Data: Word): Word; override;
  public
    procedure SetUp; override;
  end;

  TCustomTestConvertInt24LSBFromFloat64 = class(TCustomTestConvertFromFloat64)
  protected
    procedure PerformSimpleTest; override;
    function DataPostProcessing(Data: Integer): Integer; virtual; abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure BasicTest;
    procedure NativeTest;
    procedure UnalignedBufferTest;
    procedure SpeedTestNative;
    procedure SpeedTestSSE2;
  end;

  TTestConvertInt24LSBFromFloat64 = class(TCustomTestConvertInt24LSBFromFloat64)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt24MSBFromFloat64 = class(TCustomTestConvertInt24LSBFromFloat64)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TCustomTestConvertInt32FromFloat64 = class(TCustomTestConvertFromFloat64)
  protected
    procedure PerformSimpleTest; override;
    function DataPostProcessing(Data: Integer): Integer; virtual; abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure BasicTest;
    procedure NativeTest;
    procedure UnalignedBufferTest;
    procedure SpeedTestNative;
    procedure SpeedTestSSE2;
  end;

  TTestConvertInt32LSBFromFloat64 = class(TCustomTestConvertInt32FromFloat64)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB16FromFloat64 = class(TCustomTestConvertInt32FromFloat64)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB18FromFloat64 = class(TCustomTestConvertInt32FromFloat64)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB20FromFloat64 = class(TCustomTestConvertInt32FromFloat64)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB24FromFloat64 = class(TCustomTestConvertInt32FromFloat64)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSBFromFloat64 = class(TCustomTestConvertInt32FromFloat64)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB16FromFloat64 = class(TCustomTestConvertInt32FromFloat64)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB18FromFloat64 = class(TCustomTestConvertInt32FromFloat64)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB20FromFloat64 = class(TCustomTestConvertInt32FromFloat64)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB24FromFloat64 = class(TCustomTestConvertInt32FromFloat64)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

implementation

uses
  DAV_Common, DAV_BlockConvert64;

const
  CTestSampleCount = 1 shl 15;
  CSpeedTestCount  = 1 shl 13;


{ TCustomTestConvertFloat64 }

procedure TCustomTestConvertFloat64.SetUp;
begin
 // define default test delta
 FDelta := 1E-5;

 inherited;

 // calculate scale
 Assert(FMaxIntValue <> 0);
 FScale := 1 / FMaxIntValue;

 FAudioMemory64 := TAudioMemory64.Create;
 FAudioMemory64.SampleCount := CTestSampleCount;
end;

procedure TCustomTestConvertFloat64.TearDown;
begin
 FreeAndNil(FAudioMemory64);
 inherited;
end;


{ TCustomTestConvertToFloat64 }

procedure TCustomTestConvertToFloat64.PerformZeroTest;
var
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 0);
end;

procedure TCustomTestConvertToFloat64.PerformSimpleTest;
var
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // clear audio memory
 FAudioMemory64.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData,
   FAudioMemory64.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals((SampleIndex * FScale),
       FAudioMemory64.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertToFloat64.PerformSpeedTest;
var
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(PDouble(FAudioMemory64.DataPointer), FData,
       FAudioMemory64.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals(SampleIndex * FScale,
    FAudioMemory64.DataPointer^[SampleIndex], FDelta);
end;


{ TCustomTestConvertInt16ToFloat64 }

procedure TCustomTestConvertInt16ToFloat64.SetUp;
begin
 // set maximum integer value
 FMaxIntValue := $7FFF;

 inherited;

 // define test delta
 FDelta := 1E-7;

 // allocate data memory
 GetMem(FData, FAudioMemory64.SampleCount * SizeOf(Word));

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt16ToFloat64.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt16ToFloat64.BasicTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt16ToFloat64.NativeTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt16ToFloat64.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PWord;
  FloatPointer : PDouble;
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // clear audio memory
 FAudioMemory64.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 3);

 // check if data has been converted correctly
 for SampleIndex := 0 to 2 do
   CheckEquals(SampleIndex * FScale,
     FAudioMemory64.DataPointer^[SampleIndex], FDelta);

 // get unaligned pointers
 DataPointer := FData;
 Inc(DataPointer);
 FloatPointer := PDouble(FAudioMemory64.DataPointer);
 Inc(FloatPointer);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer, 1);

 // check if sample has been converted correctly
 CheckEquals(1 * FScale, FAudioMemory64.Data[1], FDelta);

 Inc(DataPointer);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer,
   FAudioMemory64.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory64.SampleCount - 17 do
   CheckEquals(((SampleIndex + 1) * FScale),
     FAudioMemory64.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertInt16ToFloat64.SpeedTestNative;
begin
 // use compatibility binding
 FFunctionBinding.Rebind([]);

 // perform speed test
 PerformSpeedTest;
end;

procedure TCustomTestConvertInt16ToFloat64.SpeedTestSSE2;
begin
 // use SSE/SSE2 binding
 FFunctionBinding.Rebind([pfSSE, pfSSE2]);

 // perform speed test
 PerformSpeedTest;
end;


{ TTestConvertInt16LSBToFloat64 }

procedure TTestConvertInt16LSBToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt16LSBToFloat64;

 inherited;
end;

procedure TTestConvertInt16LSBToFloat64.FillData;
var
  SampleIndex : Integer;
  Data        : PWord;
begin
 inherited;

 Data := FData;

 // fill data with an increasing sequence
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   Data^ := SampleIndex;
   Inc(Data);
  end;
end;


{ TTestConvertInt16MSBToFloat64 }

procedure TTestConvertInt16MSBToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt16MSBToFloat64;

 inherited;
end;

procedure TTestConvertInt16MSBToFloat64.FillData;
var
  SampleIndex : Integer;
  Data        : PWord;
begin
 inherited;

 Data := FData;

 // fill data with an increasing sequence
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   Data^ := Swap16(SampleIndex);
   Inc(Data);
  end;
end;


{ TCustomTestConvertInt24LSBToFloat64 }

procedure TCustomTestConvertInt24LSBToFloat64.SetUp;
begin
 // set maximum integer
 FMaxIntValue := $7FFFFF;

 inherited;

 // define test delta
 FDelta := 1E-7;

 // allocate data memory
 GetMem(FData, FAudioMemory64.SampleCount * 3);

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt24LSBToFloat64.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt24LSBToFloat64.PerformSimpleTest;
var
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // clear audio memory
 FAudioMemory64.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData,
   FAudioMemory64.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals((SampleIndex * FScale), FAudioMemory64.DataPointer^[SampleIndex], 1E-7);
end;

procedure TCustomTestConvertInt24LSBToFloat64.PerformSpeedTest;
var
  LoopIndex : Integer;
begin
 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do BlockConvertInt24LSBToFloat64(PDouble(FAudioMemory64.DataPointer),
       FData, FAudioMemory64.SampleCount);
end;

procedure TCustomTestConvertInt24LSBToFloat64.BasicTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt24LSBToFloat64.NativeTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt24LSBToFloat64.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PByte;
  FloatPointer : PDouble;
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // clear audio memory
 FAudioMemory64.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 3);

 // check if data has been converted correctly
 for SampleIndex := 0 to 2
  do CheckEquals((SampleIndex * FScale), FAudioMemory64.DataPointer^[SampleIndex], 1E-7);

 // get unaligned pointers
 DataPointer := FData;
 Inc(DataPointer, 3);
 FloatPointer := PDouble(FAudioMemory64.DataPointer);
 Inc(FloatPointer);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer, 1);

 // check if sample has been converted correctly
 CheckEquals((1 * FScale), FAudioMemory64.Data[1], 1E-7);

 Inc(DataPointer, 3);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer,
   FAudioMemory64.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory64.SampleCount - 17
  do CheckEquals(((SampleIndex + 1) * FScale), FAudioMemory64.DataPointer^[SampleIndex], 1E-7);
end;

procedure TCustomTestConvertInt24LSBToFloat64.SpeedTestNative;
begin
 // use compatibility binding
 FFunctionBinding.Rebind([]);

 // perform speed test
 PerformSpeedTest;
end;

procedure TCustomTestConvertInt24LSBToFloat64.SpeedTestSSE2;
begin
 // use SSE/SSE2 binding
 FFunctionBinding.Rebind([pfSSE, pfSSE2]);

 // perform speed test
 PerformSpeedTest;
end;


{ TTestConvertInt24LSBToFloat64 }

procedure TTestConvertInt24LSBToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt24LSBToFloat64;

 inherited;
end;

procedure TTestConvertInt24LSBToFloat64.FillData;
var
  SampleIndex : Integer;
  Data        : PByte;
begin
 Data := FData;

 // fill data with an increasing sequence (little endian)
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   Data^ :=  SampleIndex         and $FF; Inc(Data);
   Data^ := (SampleIndex shr  8) and $FF; Inc(Data);
   Data^ := (SampleIndex shr 16) and $FF; Inc(Data);
  end;
end;


{ TTestConvertInt24MSBToFloat64 }

procedure TTestConvertInt24MSBToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt24MSBToFloat64;

 inherited;
end;

procedure TTestConvertInt24MSBToFloat64.FillData;
var
  SampleIndex : Integer;
  Data        : PByte;
begin
 Data := FData;

 // fill data with an increasing sequence (big endian)
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   Data^ := (SampleIndex shr 16) and $FF; Inc(Data);
   Data^ := (SampleIndex shr  8) and $FF; Inc(Data);
   Data^ :=  SampleIndex         and $FF; Inc(Data);
  end;
end;


{ TCustomTestConvertInt32ToFloat64 }

procedure TCustomTestConvertInt32ToFloat64.SetUp;
begin
 inherited;

 // define test delta
 FDelta := 1E-9;

 // allocate data memory
 GetMem(FData, FAudioMemory64.SampleCount * SizeOf(Integer));

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt32ToFloat64.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt32ToFloat64.PerformSimpleTest;
var
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // clear audio memory
 FAudioMemory64.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData,
   FAudioMemory64.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals(SampleIndex * FScale, FAudioMemory64.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertInt32ToFloat64.PerformSpeedTest;
var
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(PDouble(FAudioMemory64.DataPointer),
       FData, FAudioMemory64.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1
  do CheckEquals(SampleIndex * FScale, FAudioMemory64.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertInt32ToFloat64.BasicTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt32ToFloat64.NativeTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.Rebind([]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt32ToFloat64.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PInteger;
  FloatPointer : PDouble;
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // clear audio memory
 FAudioMemory64.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 3);

 // check if data has been converted correctly
 for SampleIndex := 0 to 2
  do CheckEquals(SampleIndex * FScale, FAudioMemory64.DataPointer^[SampleIndex], FDelta);

 // get unaligned pointers
 DataPointer := FData;
 Inc(DataPointer);
 FloatPointer := PDouble(FAudioMemory64.DataPointer);
 Inc(FloatPointer);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer, 1);

 // check if sample has been converted correctly
 CheckEquals(1 * FScale, FAudioMemory64.Data[1], FDelta);

 Inc(DataPointer);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer, FAudioMemory64.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory64.SampleCount - 17
  do CheckEquals(((SampleIndex + 1) * FScale), FAudioMemory64.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertInt32ToFloat64.SpeedTestNative;
begin
 // use compatibility binding
 FFunctionBinding.Rebind([]);

 // perform speed test
 PerformSpeedTest;
end;

procedure TCustomTestConvertInt32ToFloat64.SpeedTestSSE2;
begin
 // use SSE/SSE2 binding
 FFunctionBinding.Rebind([pfSSE, pfSSE2]);

 // perform speed test
 PerformSpeedTest;
end;


{ TCustomTestConvertInt32LSBToFloat64 }

procedure TCustomTestConvertInt32LSBToFloat64.FillData;
var
  SampleIndex : Integer;
  Data        : PInteger;
begin
 Data := FData;

 // fill data with an increasing sequence
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   Data^ := SampleIndex;
   Inc(Data);
  end;
end;

{ TCustomTestConvertInt32MSBToFloat64 }

procedure TCustomTestConvertInt32MSBToFloat64.FillData;
var
  SampleIndex : Integer;
  Data        : PInteger;
begin
 Data := FData;

 // fill data with an increasing sequence
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   Data^ := Swap32(SampleIndex);
   Inc(Data);
  end;
end;


{ TTestConvertInt32LSBToFloat64 }

procedure TTestConvertInt32LSBToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSBToFloat64;
 FMaxIntValue     := $7FFFFFFF;

 inherited;
end;


{ TTestConvertInt32LSB16ToFloat64 }

procedure TTestConvertInt32LSB16ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB16ToFloat64;
 FMaxIntValue     := $7FFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB18ToFloat64 }

procedure TTestConvertInt32LSB18ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB18ToFloat64;
 FMaxIntValue     := $1FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB20ToFloat64 }

procedure TTestConvertInt32LSB20ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB20ToFloat64;
 FMaxIntValue     := $7FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB24ToFloat64 }

procedure TTestConvertInt32LSB24ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB24ToFloat64;
 FMaxIntValue     := $7FFFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSBToFloat64 }

procedure TTestConvertInt32MSBToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSBToFloat64;
 FMaxIntValue     := $7FFFFFFF;
 inherited;
end;


{ TTestConvertInt32MSB16ToFloat64 }

procedure TTestConvertInt32MSB16ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB16ToFloat64;
 FMaxIntValue     := $7FFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB18ToFloat64 }

procedure TTestConvertInt32MSB18ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB18ToFloat64;
 FMaxIntValue     := $1FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB20ToFloat64 }

procedure TTestConvertInt32MSB20ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB20ToFloat64;
 FMaxIntValue     := $7FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB24ToFloat64 }

procedure TTestConvertInt32MSB24ToFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB24ToFloat64;
 FMaxIntValue     := $7FFFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;



{ TCustomTestConvertFromFloat64 }

procedure TCustomTestConvertFromFloat64.FillData;
var
  SampleIndex : Integer;
begin
 inherited;

 // fill data with an increasing sequence
 with FAudioMemory64 do
  for SampleIndex := 0 to SampleCount - 1
   do DataPointer^[SampleIndex] := SampleIndex * FScale;
end;

procedure TCustomTestConvertFromFloat64.PerformZeroTest;
var
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(FData, PDouble(FAudioMemory64.DataPointer), 0);
end;

{ TCustomTestConvertInt16FromFloat64 }

procedure TCustomTestConvertInt16FromFloat64.SetUp;
begin
 // set maximum amplitude
 FMaxIntValue := $7FFF;

 inherited;

 // define test delta
 FDelta := 1E-7;

 // allocate data memory
 GetMem(FData, FAudioMemory64.SampleCount * SizeOf(Word));

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt16FromFloat64.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt16FromFloat64.PerformSimpleTest;
var
  DataPtr      : PWord;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert float data to integer
 TestFunction(FData, PDouble(FAudioMemory64.DataPointer),
   FAudioMemory64.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPtr^), FDelta);
   Inc(DataPtr);
  end;
end;

procedure TCustomTestConvertInt16FromFloat64.BasicTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt16FromFloat64.NativeTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt16FromFloat64.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PWord;
  FloatPointer : PDouble;
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(FData, PDouble(FAudioMemory64.DataPointer), 3);

 // assign data pointer
 DataPointer := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to 2 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPointer^), FDelta);
   Inc(DataPointer);
  end;

 // get unaligned pointers
 DataPointer := FData;
 Inc(DataPointer);
 FloatPointer := PDouble(FAudioMemory64.DataPointer);
 Inc(FloatPointer);

 // convert integer data to float
 TestFunction(DataPointer, FloatPointer, 1);

 // check if sample has been converted correctly
 CheckEquals(1, DataPostProcessing(DataPointer^), FDelta);

 Inc(DataPointer);

 // convert integer data to float
 TestFunction(DataPointer, FloatPointer, FAudioMemory64.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory64.SampleCount - 17 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPointer^), FDelta);
   Inc(DataPointer);
  end;
end;

procedure TCustomTestConvertInt16FromFloat64.SpeedTestNative;
var
  DataPtr      : PWord;
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // use compatibility binding
 FFunctionBinding.Rebind([]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(FData, PDouble(FAudioMemory64.DataPointer),
       FAudioMemory64.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPtr^), FDelta);
   Inc(DataPtr);
  end;
end;

procedure TCustomTestConvertInt16FromFloat64.SpeedTestSSE2;
var
  DataPtr      : PWord;
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // use SSE/SSE2 binding
 FFunctionBinding.Rebind([pfSSE, pfSSE2]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(FData, PDouble(FAudioMemory64.DataPointer),
       FAudioMemory64.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPtr^), FDelta);
   Inc(DataPtr);
  end;
end;


{ TTestConvertInt16LSBFromFloat64 }

function TTestConvertInt16LSBFromFloat64.DataPostProcessing(
  Data: Word): Word;
begin
 Result := Data;
end;

procedure TTestConvertInt16LSBFromFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt16LSBFromFloat64;

 inherited;
end;


{ TTestConvertInt16MSBFromFloat64 }

function TTestConvertInt16MSBFromFloat64.DataPostProcessing(
  Data: Word): Word;
begin
 Result := Swap16(Data);
end;

procedure TTestConvertInt16MSBFromFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt16MSBFromFloat64;

 inherited;
end;


{ TCustomTestConvertInt24LSBFromFloat64 }

procedure TCustomTestConvertInt24LSBFromFloat64.SetUp;
begin
 // set maximum amplitude
 FMaxIntValue := $7FFFFF;

 inherited;

 // define test delta
 FDelta := 1E-7;

 // allocate data memory
 GetMem(FData, FAudioMemory64.SampleCount * 3);

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt24LSBFromFloat64.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt24LSBFromFloat64.PerformSimpleTest;
var
  DataPtr      : PByte;
  DataValue    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert float data to integer
 TestFunction(FData, PDouble(FAudioMemory64.DataPointer),
   FAudioMemory64.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   DataValue := DataPtr^;                    Inc(DataPtr);
   DataValue := DataValue + DataPtr^ shl  8; Inc(DataPtr);
   DataValue := DataValue + DataPtr^ shl 16; Inc(DataPtr);
   CheckEquals(SampleIndex, DataPostProcessing(DataValue), 1E-7);
  end;
end;

procedure TCustomTestConvertInt24LSBFromFloat64.BasicTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt24LSBFromFloat64.NativeTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt24LSBFromFloat64.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PByte;
  FloatPointer : PDouble;
  TestFunction : TTestFunction;
  DataValue    : Integer;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(FData, PDouble(FAudioMemory64.DataPointer), 3);

 // assign data pointer
 DataPointer := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to 2 do
  begin
   DataValue := DataPointer^;                    Inc(DataPointer);
   DataValue := DataValue + DataPointer^ shl  8; Inc(DataPointer);
   DataValue := DataValue + DataPointer^ shl 16; Inc(DataPointer);

   CheckEquals(SampleIndex, DataValue, FDelta);
  end;

 // get unaligned pointers
 DataPointer := FData;
 Inc(DataPointer, 3);
 FloatPointer := PDouble(FAudioMemory64.DataPointer);
 Inc(FloatPointer);

 // convert integer data to float
 TestFunction(DataPointer, FloatPointer, 1);

 DataValue := DataPointer^;                    Inc(DataPointer);
 DataValue := DataValue + DataPointer^ shl  8; Inc(DataPointer);
 DataValue := DataValue + DataPointer^ shl 16; Inc(DataPointer);

 // check if sample has been converted correctly
 CheckEquals(1, DataValue, FDelta);

 // convert integer data to float
 TestFunction(DataPointer, FloatPointer, FAudioMemory64.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory64.SampleCount - 17 do
  begin
   DataValue := DataPointer^;                    Inc(DataPointer);
   DataValue := DataValue + DataPointer^ shl  8; Inc(DataPointer);
   DataValue := DataValue + DataPointer^ shl 16; Inc(DataPointer);

   CheckEquals(SampleIndex, DataPostProcessing(DataValue), FDelta);
  end;
end;

procedure TCustomTestConvertInt24LSBFromFloat64.SpeedTestNative;
var
  DataPtr      : PByte;
  DataValue    : Integer;
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // use compatibility binding
 FFunctionBinding.Rebind([]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(FData, PDouble(FAudioMemory64.DataPointer),
       FAudioMemory64.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   DataValue := DataPtr^;                    Inc(DataPtr);
   DataValue := DataValue + DataPtr^ shl  8; Inc(DataPtr);
   DataValue := DataValue + DataPtr^ shl 16; Inc(DataPtr);
   CheckEquals(SampleIndex, DataPostProcessing(DataValue), 1E-7);
  end;
end;

procedure TCustomTestConvertInt24LSBFromFloat64.SpeedTestSSE2;
var
  DataPtr      : PByte;
  DataValue    : Integer;
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // use SSE/SSE2 binding
 FFunctionBinding.Rebind([pfSSE, pfSSE2]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(FData, PDouble(FAudioMemory64.DataPointer),
       FAudioMemory64.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   DataValue := DataPtr^;                    Inc(DataPtr);
   DataValue := DataValue + DataPtr^ shl  8; Inc(DataPtr);
   DataValue := DataValue + DataPtr^ shl 16; Inc(DataPtr);
   CheckEquals(SampleIndex, DataPostProcessing(DataValue), 1E-7);
  end;
end;


{ TTestConvertInt24LSBFromFloat64 }

function TTestConvertInt24LSBFromFloat64.DataPostProcessing(Data: Integer): Integer;
begin
 Result := Data;
end;

procedure TTestConvertInt24LSBFromFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt24LSBFromFloat64;

 inherited;
end;


{ TTestConvertInt24MSBFromFloat64 }

function TTestConvertInt24MSBFromFloat64.DataPostProcessing(
  Data: Integer): Integer;
var
  DataPtr : PByte;
begin
 DataPtr := @Data;                   //Inc(DataPtr);
 Result := DataPtr^;                 Inc(DataPtr);
 Result := Result + DataPtr^ shl  8; Inc(DataPtr);
 Result := Result + DataPtr^ shl 16; Inc(DataPtr);
end;

procedure TTestConvertInt24MSBFromFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt24MSBFromFloat64;

 inherited;
end;


{ TCustomTestConvertInt32FromFloat64 }

procedure TCustomTestConvertInt32FromFloat64.SetUp;
begin
 inherited;

 // define test delta
 FDelta := 1E-9;

 // allocate data memory
 GetMem(FData, FAudioMemory64.SampleCount * SizeOf(Integer));

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt32FromFloat64.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt32FromFloat64.PerformSimpleTest;
var
  DataPtr      : PInteger;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(FData, PDouble(FAudioMemory64.DataPointer),
   FAudioMemory64.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPtr^), 1E-7);
   Inc(DataPtr);
  end;
end;

procedure TCustomTestConvertInt32FromFloat64.BasicTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt32FromFloat64.NativeTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.Rebind([]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PDouble(FAudioMemory64.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt32FromFloat64.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PInteger;
  FloatPointer : PDouble;
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(FData, PDouble(FAudioMemory64.DataPointer), 3);

 // assign data pointer
 DataPointer := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to 2 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPointer^), 1E-7);
   Inc(DataPointer);
  end;

 // get unaligned pointers
 DataPointer := FData;
 Inc(DataPointer);
 FloatPointer := PDouble(FAudioMemory64.DataPointer);
 Inc(FloatPointer);

 // convert integer data to float
 TestFunction(DataPointer, FloatPointer, 1);

 // check if sample has been converted correctly
 CheckEquals(1, DataPostProcessing(DataPointer^), 1E-7);

 Inc(DataPointer);

 // convert integer data to float
 TestFunction(DataPointer, FloatPointer,
   FAudioMemory64.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory64.SampleCount - 17 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPointer^), 1E-7);
   Inc(DataPointer);
  end;
end;

procedure TCustomTestConvertInt32FromFloat64.SpeedTestNative;
var
  DataPtr      : PInteger;
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // use compatibility binding
 FFunctionBinding.Rebind([]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(FData, PDouble(FAudioMemory64.DataPointer),
       FAudioMemory64.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPtr^), 1E-7);
   Inc(DataPtr);
  end;
end;

procedure TCustomTestConvertInt32FromFloat64.SpeedTestSSE2;
var
  DataPtr      : PInteger;
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // use SSE/SSE2 binding
 FFunctionBinding.Rebind([pfSSE, pfSSE2]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(FData, PDouble(FAudioMemory64.DataPointer),
       FAudioMemory64.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory64.SampleCount - 1 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPtr^), 1E-7);
   Inc(DataPtr);
  end;
end;


{ TTestConvertInt32LSBFromFloat64 }

function TTestConvertInt32LSBFromFloat64.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Data;
end;

procedure TTestConvertInt32LSBFromFloat64.SetUp;
begin
 // set function binding
 FFunctionBinding := BindingBlockConvertInt32LSBFromFloat64;

 // set maximum integer
 FMaxIntValue := $7FFFFFFF;

 inherited;
end;


{ TTestConvertInt32LSB16FromFloat64 }

function TTestConvertInt32LSB16FromFloat64.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Data;
end;

procedure TTestConvertInt32LSB16FromFloat64.SetUp;
begin
 // set function binding
 FFunctionBinding := BindingBlockConvertInt32LSB16FromFloat64;

 // set maximum integer
 FMaxIntValue     := $7FFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB18FromFloat64 }

function TTestConvertInt32LSB18FromFloat64.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Data;
end;

procedure TTestConvertInt32LSB18FromFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB18FromFloat64;
 FMaxIntValue     := $1FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB20FromFloat64 }

function TTestConvertInt32LSB20FromFloat64.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Data;
end;

procedure TTestConvertInt32LSB20FromFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB20FromFloat64;
 FMaxIntValue     := $7FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB24FromFloat64 }

function TTestConvertInt32LSB24FromFloat64.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Data;
end;

procedure TTestConvertInt32LSB24FromFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB24FromFloat64;
 FMaxIntValue     := $7FFFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSBFromFloat64 }

function TTestConvertInt32MSBFromFloat64.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Swap32(Data);
end;

procedure TTestConvertInt32MSBFromFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSBFromFloat64;
 FMaxIntValue     := $7FFFFFFF;
 inherited;
end;


{ TTestConvertInt32MSB16FromFloat64 }

function TTestConvertInt32MSB16FromFloat64.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Swap32(Data);
end;

procedure TTestConvertInt32MSB16FromFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB16FromFloat64;
 FMaxIntValue     := $7FFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB18FromFloat64 }

function TTestConvertInt32MSB18FromFloat64.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Swap32(Data);
end;

procedure TTestConvertInt32MSB18FromFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB18FromFloat64;
 FMaxIntValue     := $1FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB20FromFloat64 }

function TTestConvertInt32MSB20FromFloat64.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Swap32(Data);
end;

procedure TTestConvertInt32MSB20FromFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB20FromFloat64;
 FMaxIntValue     := $7FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB24FromFloat64 }

function TTestConvertInt32MSB24FromFloat64.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Swap32(Data);
end;

procedure TTestConvertInt32MSB24FromFloat64.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB24FromFloat64;
 FMaxIntValue     := $7FFFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


var
  TestSuiteFloat: TTestSuite;

initialization
  TestSuiteFloat := TTestSuite.Create('-> Float 64-bit (LSB)');
  with TestSuiteFloat do
   begin
    AddTest(TTestConvertInt16LSBToFloat64.Suite);
    AddTest(TTestConvertInt24LSBToFloat64.Suite);
    AddTest(TTestConvertInt32LSBToFloat64.Suite);
    AddTest(TTestConvertInt32LSB16ToFloat64.Suite);
    AddTest(TTestConvertInt32LSB18ToFloat64.Suite);
    AddTest(TTestConvertInt32LSB20ToFloat64.Suite);
    AddTest(TTestConvertInt32LSB24ToFloat64.Suite);
   end;
  RegisterTest(TestSuiteFloat);

  TestSuiteFloat := TTestSuite.Create('-> Float 64-bit (MSB)');
  with TestSuiteFloat do
   begin
    AddTest(TTestConvertInt16MSBToFloat64.Suite);
    AddTest(TTestConvertInt24MSBToFloat64.Suite);
    AddTest(TTestConvertInt32MSBToFloat64.Suite);
    AddTest(TTestConvertInt32MSB16ToFloat64.Suite);
    AddTest(TTestConvertInt32MSB18ToFloat64.Suite);
    AddTest(TTestConvertInt32MSB20ToFloat64.Suite);
    AddTest(TTestConvertInt32MSB24ToFloat64.Suite);
   end;
  RegisterTest(TestSuiteFloat);

  TestSuiteFloat := TTestSuite.Create('<- Float 64-bit (LSB)');
  with TestSuiteFloat do
   begin
    AddTest(TTestConvertInt16LSBFromFloat64.Suite);
    AddTest(TTestConvertInt24LSBFromFloat64.Suite);
    AddTest(TTestConvertInt32LSBFromFloat64.Suite);
    AddTest(TTestConvertInt32LSB16FromFloat64.Suite);
    AddTest(TTestConvertInt32LSB18FromFloat64.Suite);
    AddTest(TTestConvertInt32LSB20FromFloat64.Suite);
    AddTest(TTestConvertInt32LSB24FromFloat64.Suite);
   end;
  RegisterTest(TestSuiteFloat);

  TestSuiteFloat := TTestSuite.Create('<- Float 64-bit (MSB)');
  with TestSuiteFloat do
   begin
    AddTest(TTestConvertInt16MSBFromFloat64.Suite);
    AddTest(TTestConvertInt24MSBFromFloat64.Suite);
    AddTest(TTestConvertInt32MSBFromFloat64.Suite);
    AddTest(TTestConvertInt32MSB16FromFloat64.Suite);
    AddTest(TTestConvertInt32MSB18FromFloat64.Suite);
    AddTest(TTestConvertInt32MSB20FromFloat64.Suite);
    AddTest(TTestConvertInt32MSB24FromFloat64.Suite);
   end;
  RegisterTest(TestSuiteFloat);

end.
