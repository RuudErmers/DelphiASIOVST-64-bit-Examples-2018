unit TestDAV_BlockConvert32;

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

  TCustomTestConvertFloat32 = class(TTestCase)
  strict protected
    FAudioMemory32   : TAudioMemory32;
    FData            : Pointer;
    FFunctionBinding : TFunctionBinding;
    FMaxIntValue     : Integer;
    FDelta           : Single;
    FScale           : Single;
  protected
    procedure FillData; virtual; abstract;
    procedure PerformZeroTest; virtual; abstract;
    procedure PerformSimpleTest; virtual; abstract;
    procedure PerformSpeedTest; virtual; abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TCustomTestConvertToFloat32 = class(TCustomTestConvertFloat32)
  protected
    procedure PerformZeroTest; override;
    procedure PerformSimpleTest; override;
    procedure PerformSpeedTest; override;
  end;

  TCustomTestConvertInt16ToFloat32 = class(TCustomTestConvertToFloat32)
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

  TTestConvertInt16LSBToFloat32 = class(TCustomTestConvertInt16ToFloat32)
  protected
    procedure FillData; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt16MSBToFloat32 = class(TCustomTestConvertInt16ToFloat32)
  protected
    procedure FillData; override;
  public
    procedure SetUp; override;
  end;

  TCustomTestConvertInt24ToFloat32 = class(TCustomTestConvertToFloat32)
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

  TTestConvertInt24LSBToFloat32 = class(TCustomTestConvertInt24ToFloat32)
  protected
    procedure FillData; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt24MSBToFloat32 = class(TCustomTestConvertInt24ToFloat32)
  protected
    procedure FillData; override;
  public
    procedure SetUp; override;
  end;

  TCustomTestConvertInt32ToFloat32 = class(TCustomTestConvertToFloat32)
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

  TCustomTestConvertInt32LSBToFloat32 = class(TCustomTestConvertInt32ToFloat32)
  protected
    procedure FillData; override;
  end;

  TCustomTestConvertInt32MSBToFloat32 = class(TCustomTestConvertInt32ToFloat32)
  protected
    procedure FillData; override;
  end;

  TTestConvertInt32LSBToFloat32 = class(TCustomTestConvertInt32LSBToFloat32)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB16ToFloat32 = class(TCustomTestConvertInt32LSBToFloat32)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB18ToFloat32 = class(TCustomTestConvertInt32LSBToFloat32)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB20ToFloat32 = class(TCustomTestConvertInt32LSBToFloat32)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB24ToFloat32 = class(TCustomTestConvertInt32LSBToFloat32)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSBToFloat32 = class(TCustomTestConvertInt32MSBToFloat32)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB16ToFloat32 = class(TCustomTestConvertInt32MSBToFloat32)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB18ToFloat32 = class(TCustomTestConvertInt32MSBToFloat32)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB20ToFloat32 = class(TCustomTestConvertInt32MSBToFloat32)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB24ToFloat32 = class(TCustomTestConvertInt32MSBToFloat32)
  public
    procedure SetUp; override;
  end;


  // from float

  TCustomTestConvertFromFloat32 = class(TCustomTestConvertFloat32)
  protected
    procedure FillData; override;
    procedure PerformZeroTest; override;
  end;

  TCustomTestConvertInt16FromFloat32 = class(TCustomTestConvertFromFloat32)
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

  TTestConvertInt16LSBFromFloat32 = class(TCustomTestConvertInt16FromFloat32)
  protected
    function DataPostProcessing(Data: Word): Word; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt16MSBFromFloat32 = class(TCustomTestConvertInt16FromFloat32)
  protected
    function DataPostProcessing(Data: Word): Word; override;
  public
    procedure SetUp; override;
  end;

  TCustomTestConvertInt24LSBFromFloat32 = class(TCustomTestConvertFromFloat32)
  protected
    procedure PerformSimpleTest; override;
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

  TTestConvertInt24LSBFromFloat32 = class(TCustomTestConvertInt24LSBFromFloat32)
  public
    procedure SetUp; override;
  end;

  TTestConvertInt24MSBFromFloat32 = class(TCustomTestConvertInt24LSBFromFloat32)
  public
    procedure SetUp; override;
  end;

  TCustomTestConvertInt32FromFloat32 = class(TCustomTestConvertFromFloat32)
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

  TTestConvertInt32LSBFromFloat32 = class(TCustomTestConvertInt32FromFloat32)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB16FromFloat32 = class(TCustomTestConvertInt32FromFloat32)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB18FromFloat32 = class(TCustomTestConvertInt32FromFloat32)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB20FromFloat32 = class(TCustomTestConvertInt32FromFloat32)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32LSB24FromFloat32 = class(TCustomTestConvertInt32FromFloat32)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSBFromFloat32 = class(TCustomTestConvertInt32FromFloat32)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB16FromFloat32 = class(TCustomTestConvertInt32FromFloat32)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB18FromFloat32 = class(TCustomTestConvertInt32FromFloat32)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB20FromFloat32 = class(TCustomTestConvertInt32FromFloat32)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

  TTestConvertInt32MSB24FromFloat32 = class(TCustomTestConvertInt32FromFloat32)
  protected
    function DataPostProcessing(Data: Integer): Integer; override;
  public
    procedure SetUp; override;
  end;

implementation

uses
  DAV_Common, DAV_BlockConvert32;

const
  CTestSampleCount = 1 shl 15;
  CSpeedTestCount  = 1 shl 13;


{ TCustomTestConvertFloat32 }

procedure TCustomTestConvertFloat32.SetUp;
begin
 // define default test delta
 FDelta := 1E-5;

 inherited;

 // calculate scale
 Assert(FMaxIntValue <> 0);
 FScale := 1 / FMaxIntValue;

 FAudioMemory32 := TAudioMemory32.Create;
 FAudioMemory32.SampleCount := CTestSampleCount;
end;

procedure TCustomTestConvertFloat32.TearDown;
begin
 FreeAndNil(FAudioMemory32);
 inherited;
end;


{ TCustomTestConvertToFloat32 }

procedure TCustomTestConvertToFloat32.PerformZeroTest;
var
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PSingle(FAudioMemory32.DataPointer), FData, 0);
end;

procedure TCustomTestConvertToFloat32.PerformSimpleTest;
var
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // clear audio memory
 FAudioMemory32.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PSingle(FAudioMemory32.DataPointer), FData,
   FAudioMemory32.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do CheckEquals((SampleIndex * FScale),
       FAudioMemory32.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertToFloat32.PerformSpeedTest;
var
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(PSingle(FAudioMemory32.DataPointer), FData,
       FAudioMemory32.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do CheckEquals(SampleIndex * FScale,
    FAudioMemory32.DataPointer^[SampleIndex], FDelta);
end;


{ TCustomTestConvertInt16ToFloat32 }

procedure TCustomTestConvertInt16ToFloat32.SetUp;
begin
 // set maximum integer value
 FMaxIntValue := $7FFF;

 inherited;

 // define test delta
 FDelta := 1E-7;

 // allocate data memory
 GetMem(FData, FAudioMemory32.SampleCount * SizeOf(Word));

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt16ToFloat32.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt16ToFloat32.BasicTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt16ToFloat32.NativeTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt16ToFloat32.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PWord;
  FloatPointer : PSingle;
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // clear audio memory
 FAudioMemory32.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PSingle(FAudioMemory32.DataPointer), FData, 3);

 // check if data has been converted correctly
 for SampleIndex := 0 to 2 do
   CheckEquals(SampleIndex * FScale,
     FAudioMemory32.DataPointer^[SampleIndex], FDelta);

 // get unaligned pointers
 DataPointer := FData;
 Inc(DataPointer);
 FloatPointer := PSingle(FAudioMemory32.DataPointer);
 Inc(FloatPointer);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer, 1);

 // check if sample has been converted correctly
 CheckEquals(1 * FScale, FAudioMemory32.Data[1], FDelta);

 Inc(DataPointer);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer,
   FAudioMemory32.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory32.SampleCount - 17 do
   CheckEquals(((SampleIndex + 1) * FScale),
     FAudioMemory32.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertInt16ToFloat32.SpeedTestNative;
begin
 // use compatibility binding
 FFunctionBinding.Rebind([]);

 // perform speed test
 PerformSpeedTest;
end;

procedure TCustomTestConvertInt16ToFloat32.SpeedTestSSE2;
begin
 // use SSE/SSE2 binding
 FFunctionBinding.Rebind([pfSSE, pfSSE2]);

 // perform speed test
 PerformSpeedTest;
end;


{ TTestConvertInt16LSBToFloat32 }

procedure TTestConvertInt16LSBToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt16LSBToFloat32;

 inherited;
end;

procedure TTestConvertInt16LSBToFloat32.FillData;
var
  SampleIndex : Integer;
  Data        : PWord;
begin
 inherited;

 Data := FData;

 // fill data with an increasing sequence
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   Data^ := SampleIndex;
   Inc(Data);
  end;
end;


{ TTestConvertInt16MSBToFloat32 }

procedure TTestConvertInt16MSBToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt16MSBToFloat32;

 inherited;
end;

procedure TTestConvertInt16MSBToFloat32.FillData;
var
  SampleIndex : Integer;
  Data        : PWord;
begin
 inherited;

 Data := FData;

 // fill data with an increasing sequence
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   Data^ := Swap16(SampleIndex);
   Inc(Data);
  end;
end;


{ TCustomTestConvertInt24ToFloat32 }

procedure TCustomTestConvertInt24ToFloat32.SetUp;
begin
 // set maximum integer
 FMaxIntValue := $7FFFFF;

 inherited;

 // define test delta
 FDelta := 1E-7;

 // allocate data memory
 GetMem(FData, FAudioMemory32.SampleCount * 3);

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt24ToFloat32.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt24ToFloat32.PerformSimpleTest;
var
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // clear audio memory
 FAudioMemory32.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PSingle(FAudioMemory32.DataPointer), FData,
   FAudioMemory32.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do CheckEquals((SampleIndex * FScale), FAudioMemory32.DataPointer^[SampleIndex], 1E-7);
end;

procedure TCustomTestConvertInt24ToFloat32.PerformSpeedTest;
var
  LoopIndex : Integer;
begin
 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do BlockConvertInt24LSBToFloat32(PSingle(FAudioMemory32.DataPointer),
       FData, FAudioMemory32.SampleCount);
end;

procedure TCustomTestConvertInt24ToFloat32.BasicTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt24ToFloat32.NativeTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt24ToFloat32.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PByte;
  FloatPointer : PSingle;
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // clear audio memory
 FAudioMemory32.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PSingle(FAudioMemory32.DataPointer), FData, 3);

 // check if data has been converted correctly
 for SampleIndex := 0 to 2
  do CheckEquals(SampleIndex * FScale, FAudioMemory32.DataPointer^[SampleIndex], FDelta);

 // get unaligned pointers
 DataPointer := FData;
 Inc(DataPointer, 3);
 FloatPointer := PSingle(FAudioMemory32.DataPointer);
 Inc(FloatPointer);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer, 1);

 // check if sample has been converted correctly
 CheckEquals(FScale, FAudioMemory32.Data[1], FDelta);

 Inc(DataPointer, 3);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer,
   FAudioMemory32.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory32.SampleCount - 17
  do CheckEquals(((SampleIndex + 1) * FScale), FAudioMemory32.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertInt24ToFloat32.SpeedTestNative;
begin
 // use compatibility binding
 FFunctionBinding.Rebind([]);

 // perform speed test
 PerformSpeedTest;
end;

procedure TCustomTestConvertInt24ToFloat32.SpeedTestSSE2;
begin
 // use SSE/SSE2 binding
 FFunctionBinding.Rebind([pfSSE, pfSSE2]);

 // perform speed test
 PerformSpeedTest;
end;


{ TTestConvertInt24LSBToFloat32 }

procedure TTestConvertInt24LSBToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt24LSBToFloat32;

 inherited;
end;

procedure TTestConvertInt24LSBToFloat32.FillData;
var
  SampleIndex : Integer;
  Data        : PByte;
begin
 Data := FData;

 // fill data with an increasing sequence (little endian)
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   Data^ :=  SampleIndex         and $FF; Inc(Data);
   Data^ := (SampleIndex shr  8) and $FF; Inc(Data);
   Data^ := (SampleIndex shr 16) and $FF; Inc(Data);
  end;
end;


{ TTestConvertInt24MSBToFloat32 }

procedure TTestConvertInt24MSBToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt24MSBToFloat32;

 inherited;
end;

procedure TTestConvertInt24MSBToFloat32.FillData;
var
  SampleIndex : Integer;
  Data        : PByte;
begin
 Data := FData;

 // fill data with an increasing sequence (big endian)
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   Data^ := (SampleIndex shr 16) and $FF; Inc(Data);
   Data^ := (SampleIndex shr  8) and $FF; Inc(Data);
   Data^ :=  SampleIndex         and $FF; Inc(Data);
  end;
end;


{ TCustomTestConvertInt32ToFloat32 }

procedure TCustomTestConvertInt32ToFloat32.SetUp;
begin
 inherited;

 // define test delta
 FDelta := 1E-9;

 // allocate data memory
 GetMem(FData, FAudioMemory32.SampleCount * SizeOf(Integer));

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt32ToFloat32.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt32ToFloat32.PerformSimpleTest;
var
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // clear audio memory
 FAudioMemory32.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PSingle(FAudioMemory32.DataPointer), FData,
   FAudioMemory32.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do CheckEquals(SampleIndex * FScale, FAudioMemory32.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertInt32ToFloat32.PerformSpeedTest;
var
  LoopIndex    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // process a sequence of block converts
 for LoopIndex := 1 to CSpeedTestCount
  do TestFunction(PSingle(FAudioMemory32.DataPointer),
       FData, FAudioMemory32.SampleCount);

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1
  do CheckEquals(SampleIndex * FScale, FAudioMemory32.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertInt32ToFloat32.BasicTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PSingle(FAudioMemory32.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt32ToFloat32.NativeTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.Rebind([]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PSingle(FAudioMemory32.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt32ToFloat32.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PInteger;
  FloatPointer : PSingle;
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // clear audio memory
 FAudioMemory32.Clear;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(PSingle(FAudioMemory32.DataPointer), FData, 3);

 // check if data has been converted correctly
 for SampleIndex := 0 to 2
  do CheckEquals(SampleIndex * FScale, FAudioMemory32.DataPointer^[SampleIndex], FDelta);

 // get unaligned pointers
 DataPointer := FData;
 Inc(DataPointer);
 FloatPointer := PSingle(FAudioMemory32.DataPointer);
 Inc(FloatPointer);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer, 1);

 // check if sample has been converted correctly
 CheckEquals(1 * FScale, FAudioMemory32.Data[1], FDelta);

 Inc(DataPointer);

 // convert integer data to float
 TestFunction(FloatPointer, DataPointer, FAudioMemory32.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory32.SampleCount - 17
  do CheckEquals(((SampleIndex + 1) * FScale), FAudioMemory32.DataPointer^[SampleIndex], FDelta);
end;

procedure TCustomTestConvertInt32ToFloat32.SpeedTestNative;
begin
 // use compatibility binding
 FFunctionBinding.Rebind([]);

 // perform speed test
 PerformSpeedTest;
end;

procedure TCustomTestConvertInt32ToFloat32.SpeedTestSSE2;
begin
 // use SSE/SSE2 binding
 FFunctionBinding.Rebind([pfSSE, pfSSE2]);

 // perform speed test
 PerformSpeedTest;
end;


{ TCustomTestConvertInt32LSBToFloat32 }

procedure TCustomTestConvertInt32LSBToFloat32.FillData;
var
  SampleIndex : Integer;
  Data        : PInteger;
begin
 Data := FData;

 // fill data with an increasing sequence
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   Data^ := SampleIndex;
   Inc(Data);
  end;
end;

{ TCustomTestConvertInt32MSBToFloat32 }

procedure TCustomTestConvertInt32MSBToFloat32.FillData;
var
  SampleIndex : Integer;
  Data        : PInteger;
begin
 Data := FData;

 // fill data with an increasing sequence
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   Data^ := Swap32(SampleIndex);
   Inc(Data);
  end;
end;


{ TTestConvertInt32LSBToFloat32 }

procedure TTestConvertInt32LSBToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSBToFloat32;
 FMaxIntValue     := $7FFFFFFF;

 inherited;
end;


{ TTestConvertInt32LSB16ToFloat32 }

procedure TTestConvertInt32LSB16ToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB16ToFloat32;
 FMaxIntValue     := $7FFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB18ToFloat32 }

procedure TTestConvertInt32LSB18ToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB18ToFloat32;
 FMaxIntValue     := $1FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB20ToFloat32 }

procedure TTestConvertInt32LSB20ToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB20ToFloat32;
 FMaxIntValue     := $7FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB24ToFloat32 }

procedure TTestConvertInt32LSB24ToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB24ToFloat32;
 FMaxIntValue     := $7FFFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSBToFloat32 }

procedure TTestConvertInt32MSBToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSBToFloat32;
 FMaxIntValue     := $7FFFFFFF;
 inherited;
end;


{ TTestConvertInt32MSB16ToFloat32 }

procedure TTestConvertInt32MSB16ToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB16ToFloat32;
 FMaxIntValue     := $7FFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB18ToFloat32 }

procedure TTestConvertInt32MSB18ToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB18ToFloat32;
 FMaxIntValue     := $1FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB20ToFloat32 }

procedure TTestConvertInt32MSB20ToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB20ToFloat32;
 FMaxIntValue     := $7FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB24ToFloat32 }

procedure TTestConvertInt32MSB24ToFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB24ToFloat32;
 FMaxIntValue     := $7FFFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;



{ TCustomTestConvertFromFloat32 }

procedure TCustomTestConvertFromFloat32.FillData;
var
  SampleIndex : Integer;
begin
 inherited;

 // fill data with an increasing sequence
 with FAudioMemory32 do
  for SampleIndex := 0 to SampleCount - 1
   do DataPointer^[SampleIndex] := SampleIndex * FScale;
end;

procedure TCustomTestConvertFromFloat32.PerformZeroTest;
var
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(FData, PSingle(FAudioMemory32.DataPointer), 0);
end;

{ TCustomTestConvertInt16FromFloat32 }

procedure TCustomTestConvertInt16FromFloat32.SetUp;
begin
 // set maximum amplitude
 FMaxIntValue := $7FFF;

 inherited;

 // define test delta
 FDelta := 1E-7;

 // allocate data memory
 GetMem(FData, FAudioMemory32.SampleCount * SizeOf(Word));

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt16FromFloat32.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt16FromFloat32.PerformSimpleTest;
var
  DataPtr      : PWord;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert float data to integer
 TestFunction(FData, PSingle(FAudioMemory32.DataPointer),
   FAudioMemory32.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPtr^), FDelta);
   Inc(DataPtr);
  end;
end;

procedure TCustomTestConvertInt16FromFloat32.BasicTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt16FromFloat32.NativeTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt16FromFloat32.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PWord;
  FloatPointer : PSingle;
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(FData, PSingle(FAudioMemory32.DataPointer), 3);

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
 FloatPointer := PSingle(FAudioMemory32.DataPointer);
 Inc(FloatPointer);

 // convert integer data to float
 TestFunction(DataPointer, FloatPointer, 1);

 // check if sample has been converted correctly
 CheckEquals(1, DataPostProcessing(DataPointer^), FDelta);

 Inc(DataPointer);

 // convert integer data to float
 TestFunction(DataPointer, FloatPointer, FAudioMemory32.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory32.SampleCount - 17 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPointer^), FDelta);
   Inc(DataPointer);
  end;
end;

procedure TCustomTestConvertInt16FromFloat32.SpeedTestNative;
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
  do TestFunction(FData, PSingle(FAudioMemory32.DataPointer),
       FAudioMemory32.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPtr^), FDelta);
   Inc(DataPtr);
  end;
end;

procedure TCustomTestConvertInt16FromFloat32.SpeedTestSSE2;
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
  do TestFunction(FData, PSingle(FAudioMemory32.DataPointer),
       FAudioMemory32.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPtr^), FDelta);
   Inc(DataPtr);
  end;
end;


{ TTestConvertInt16LSBFromFloat32 }

function TTestConvertInt16LSBFromFloat32.DataPostProcessing(
  Data: Word): Word;
begin
 Result := Data;
end;

procedure TTestConvertInt16LSBFromFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt16LSBFromFloat32;

 inherited;
end;


{ TTestConvertInt16MSBFromFloat32 }

function TTestConvertInt16MSBFromFloat32.DataPostProcessing(
  Data: Word): Word;
begin
 Result := Swap16(Data);
end;

procedure TTestConvertInt16MSBFromFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt16MSBFromFloat32;

 inherited;
end;


{ TCustomTestConvertInt24LSBFromFloat32 }

procedure TCustomTestConvertInt24LSBFromFloat32.SetUp;
begin
 // set maximum amplitude
 FMaxIntValue := $7FFFFF;

 inherited;

 // define test delta
 FDelta := 1E-7;

 // allocate data memory
 GetMem(FData, FAudioMemory32.SampleCount * 3);

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt24LSBFromFloat32.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt24LSBFromFloat32.PerformSimpleTest;
var
  DataPtr      : PByte;
  DataValue    : Integer;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert float data to integer
 TestFunction(FData, PSingle(FAudioMemory32.DataPointer),
   FAudioMemory32.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   DataValue := DataPtr^;                    Inc(DataPtr);
   DataValue := DataValue + DataPtr^ shl  8; Inc(DataPtr);
   DataValue := DataValue + DataPtr^ shl 16; Inc(DataPtr);
   CheckEquals(SampleIndex, DataValue, 1E-7);
  end;
end;

procedure TCustomTestConvertInt24LSBFromFloat32.BasicTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt24LSBFromFloat32.NativeTest;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // perform zero test
 PerformZeroTest;

 // perform simple test
 PerformSimpleTest;
end;

procedure TCustomTestConvertInt24LSBFromFloat32.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PByte;
  FloatPointer : PSingle;
  TestFunction : TTestFunction;
  DataValue    : Integer;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(FData, PSingle(FAudioMemory32.DataPointer), 3);

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
 FloatPointer := PSingle(FAudioMemory32.DataPointer);
 Inc(FloatPointer);

 // convert integer data to float
 TestFunction(DataPointer, FloatPointer, 1);

 DataValue := DataPointer^;                    Inc(DataPointer);
 DataValue := DataValue + DataPointer^ shl  8; Inc(DataPointer);
 DataValue := DataValue + DataPointer^ shl 16; Inc(DataPointer);

 // check if sample has been converted correctly
 CheckEquals(1, DataValue, FDelta);

 // convert integer data to float
 TestFunction(DataPointer, FloatPointer, FAudioMemory32.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory32.SampleCount - 17 do
  begin
   DataValue := DataPointer^;                    Inc(DataPointer);
   DataValue := DataValue + DataPointer^ shl  8; Inc(DataPointer);
   DataValue := DataValue + DataPointer^ shl 16; Inc(DataPointer);

   CheckEquals(SampleIndex, DataValue, FDelta);
  end;
end;

procedure TCustomTestConvertInt24LSBFromFloat32.SpeedTestNative;
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
  do TestFunction(FData, PSingle(FAudioMemory32.DataPointer),
       FAudioMemory32.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   DataValue := DataPtr^;                    Inc(DataPtr);
   DataValue := DataValue + DataPtr^ shl  8; Inc(DataPtr);
   DataValue := DataValue + DataPtr^ shl 16; Inc(DataPtr);
   CheckEquals(SampleIndex, DataValue, 1E-7);
  end;
end;

procedure TCustomTestConvertInt24LSBFromFloat32.SpeedTestSSE2;
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
  do TestFunction(FData, PSingle(FAudioMemory32.DataPointer),
       FAudioMemory32.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   DataValue := DataPtr^;                    Inc(DataPtr);
   DataValue := DataValue + DataPtr^ shl  8; Inc(DataPtr);
   DataValue := DataValue + DataPtr^ shl 16; Inc(DataPtr);
   CheckEquals(SampleIndex, DataValue, 1E-7);
  end;
end;


{ TTestConvertInt24LSBFromFloat32 }

procedure TTestConvertInt24LSBFromFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt24LSBFromFloat32;

 inherited;
end;


{ TTestConvertInt24MSBFromFloat32 }

procedure TTestConvertInt24MSBFromFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt24MSBFromFloat32;

 inherited;
end;


{ TCustomTestConvertInt32FromFloat32 }

procedure TCustomTestConvertInt32FromFloat32.SetUp;
begin
 inherited;

 // define test delta
 FDelta := 1E-9;

 // allocate data memory
 GetMem(FData, FAudioMemory32.SampleCount * SizeOf(Integer));

 // fill data memory
 FillData;
end;

procedure TCustomTestConvertInt32FromFloat32.TearDown;
begin
 Dispose(FData);
 inherited;
end;

procedure TCustomTestConvertInt32FromFloat32.PerformSimpleTest;
var
  DataPtr      : PInteger;
  SampleIndex  : Integer;
  TestFunction : TTestFunction;
begin
 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(FData, PSingle(FAudioMemory32.DataPointer),
   FAudioMemory32.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPtr^), 1E-7);
   Inc(DataPtr);
  end;
end;

procedure TCustomTestConvertInt32FromFloat32.BasicTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PSingle(FAudioMemory32.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt32FromFloat32.NativeTest;
var
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.Rebind([]);

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // check if a sample count of 0 can be passed
 TestFunction(PSingle(FAudioMemory32.DataPointer), FData, 0);

 PerformSimpleTest;
end;

procedure TCustomTestConvertInt32FromFloat32.UnalignedBufferTest;
var
  SampleIndex  : Integer;
  DataPointer  : PInteger;
  FloatPointer : PSingle;
  TestFunction : TTestFunction;
begin
 // use processor specific binding
 FFunctionBinding.RebindProcessorSpecific;

 // assign function under test
 TestFunction := Pointer(FFunctionBinding.Prototype^);

 // convert integer data to float
 TestFunction(FData, PSingle(FAudioMemory32.DataPointer), 3);

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
 FloatPointer := PSingle(FAudioMemory32.DataPointer);
 Inc(FloatPointer);

 // convert integer data to float
 TestFunction(DataPointer, FloatPointer, 1);

 // check if sample has been converted correctly
 CheckEquals(1, DataPostProcessing(DataPointer^), 1E-7);

 Inc(DataPointer);

 // convert integer data to float
 TestFunction(DataPointer, FloatPointer,
   FAudioMemory32.SampleCount - 17);

 // check if data has been converted correctly
 for SampleIndex := 1 to FAudioMemory32.SampleCount - 17 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPointer^), 1E-7);
   Inc(DataPointer);
  end;
end;

procedure TCustomTestConvertInt32FromFloat32.SpeedTestNative;
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
  do TestFunction(FData, PSingle(FAudioMemory32.DataPointer),
       FAudioMemory32.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPtr^), 1E-7);
   Inc(DataPtr);
  end;
end;

procedure TCustomTestConvertInt32FromFloat32.SpeedTestSSE2;
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
  do TestFunction(FData, PSingle(FAudioMemory32.DataPointer),
       FAudioMemory32.SampleCount);

 // assign data pointer
 DataPtr := FData;

 // check if data has been converted correctly
 for SampleIndex := 0 to FAudioMemory32.SampleCount - 1 do
  begin
   CheckEquals(SampleIndex, DataPostProcessing(DataPtr^), 1E-7);
   Inc(DataPtr);
  end;
end;


{ TTestConvertInt32LSBFromFloat32 }

function TTestConvertInt32LSBFromFloat32.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Data;
end;

procedure TTestConvertInt32LSBFromFloat32.SetUp;
begin
 // set function binding
 FFunctionBinding := BindingBlockConvertInt32LSBFromFloat32;

 // set maximum integer
 FMaxIntValue := $7FFFFFFF;

 inherited;
end;


{ TTestConvertInt32LSB16FromFloat32 }

function TTestConvertInt32LSB16FromFloat32.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Data;
end;

procedure TTestConvertInt32LSB16FromFloat32.SetUp;
begin
 // set function binding
 FFunctionBinding := BindingBlockConvertInt32LSB16FromFloat32;

 // set maximum integer
 FMaxIntValue     := $7FFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB18FromFloat32 }

function TTestConvertInt32LSB18FromFloat32.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Data;
end;

procedure TTestConvertInt32LSB18FromFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB18FromFloat32;
 FMaxIntValue     := $1FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB20FromFloat32 }

function TTestConvertInt32LSB20FromFloat32.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Data;
end;

procedure TTestConvertInt32LSB20FromFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB20FromFloat32;
 FMaxIntValue     := $7FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32LSB24FromFloat32 }

function TTestConvertInt32LSB24FromFloat32.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Data;
end;

procedure TTestConvertInt32LSB24FromFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32LSB24FromFloat32;
 FMaxIntValue     := $7FFFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSBFromFloat32 }

function TTestConvertInt32MSBFromFloat32.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Swap32(Data);
end;

procedure TTestConvertInt32MSBFromFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSBFromFloat32;
 FMaxIntValue     := $7FFFFFFF;
 inherited;
end;


{ TTestConvertInt32MSB16FromFloat32 }

function TTestConvertInt32MSB16FromFloat32.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Swap32(Data);
end;

procedure TTestConvertInt32MSB16FromFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB16FromFloat32;
 FMaxIntValue     := $7FFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB18FromFloat32 }

function TTestConvertInt32MSB18FromFloat32.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Swap32(Data);
end;

procedure TTestConvertInt32MSB18FromFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB18FromFloat32;
 FMaxIntValue     := $1FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB20FromFloat32 }

function TTestConvertInt32MSB20FromFloat32.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Swap32(Data);
end;

procedure TTestConvertInt32MSB20FromFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB20FromFloat32;
 FMaxIntValue     := $7FFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


{ TTestConvertInt32MSB24FromFloat32 }

function TTestConvertInt32MSB24FromFloat32.DataPostProcessing(
  Data: Integer): Integer;
begin
 Result := Swap32(Data);
end;

procedure TTestConvertInt32MSB24FromFloat32.SetUp;
begin
 FFunctionBinding := BindingBlockConvertInt32MSB24FromFloat32;
 FMaxIntValue     := $7FFFFF;

 inherited;

 // use custom delta
 FDelta := 1E-7;
end;


var
  TestSuiteFloat: TTestSuite;

initialization
  TestSuiteFloat := TTestSuite.Create('-> Float 32-bit (LSB)');
  with TestSuiteFloat do
   begin
    AddTest(TTestConvertInt16LSBToFloat32.Suite);
    AddTest(TTestConvertInt24LSBToFloat32.Suite);
    AddTest(TTestConvertInt32LSBToFloat32.Suite);
    AddTest(TTestConvertInt32LSB16ToFloat32.Suite);
    AddTest(TTestConvertInt32LSB18ToFloat32.Suite);
    AddTest(TTestConvertInt32LSB20ToFloat32.Suite);
    AddTest(TTestConvertInt32LSB24ToFloat32.Suite);
   end;
  RegisterTest(TestSuiteFloat);

  TestSuiteFloat := TTestSuite.Create('-> Float 32-bit (MSB)');
  with TestSuiteFloat do
   begin
    AddTest(TTestConvertInt16MSBToFloat32.Suite);
    AddTest(TTestConvertInt24MSBToFloat32.Suite);
    AddTest(TTestConvertInt32MSBToFloat32.Suite);
    AddTest(TTestConvertInt32MSB16ToFloat32.Suite);
    AddTest(TTestConvertInt32MSB18ToFloat32.Suite);
    AddTest(TTestConvertInt32MSB20ToFloat32.Suite);
    AddTest(TTestConvertInt32MSB24ToFloat32.Suite);
   end;
  RegisterTest(TestSuiteFloat);

  TestSuiteFloat := TTestSuite.Create('<- Float 32-bit (LSB)');
  with TestSuiteFloat do
   begin
    AddTest(TTestConvertInt16LSBFromFloat32.Suite);
    AddTest(TTestConvertInt24LSBFromFloat32.Suite);
    AddTest(TTestConvertInt32LSBFromFloat32.Suite);
    AddTest(TTestConvertInt32LSB16FromFloat32.Suite);
    AddTest(TTestConvertInt32LSB18FromFloat32.Suite);
    AddTest(TTestConvertInt32LSB20FromFloat32.Suite);
    AddTest(TTestConvertInt32LSB24FromFloat32.Suite);
   end;
  RegisterTest(TestSuiteFloat);

  TestSuiteFloat := TTestSuite.Create('<- Float 32-bit (MSB)');
  with TestSuiteFloat do
   begin
    AddTest(TTestConvertInt16MSBFromFloat32.Suite);
    AddTest(TTestConvertInt24MSBFromFloat32.Suite);
    AddTest(TTestConvertInt32MSBFromFloat32.Suite);
    AddTest(TTestConvertInt32MSB16FromFloat32.Suite);
    AddTest(TTestConvertInt32MSB18FromFloat32.Suite);
    AddTest(TTestConvertInt32MSB20FromFloat32.Suite);
    AddTest(TTestConvertInt32MSB24FromFloat32.Suite);
   end;
  RegisterTest(TestSuiteFloat);

end.
