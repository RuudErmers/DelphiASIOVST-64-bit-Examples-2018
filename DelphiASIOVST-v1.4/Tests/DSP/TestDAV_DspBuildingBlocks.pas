unit TestDAV_DspBuildingBlocks;

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

{$I ..\DAV_Compiler.inc}

uses
  TestFramework, DAV_DspBuildingBlocks, DAV_Types, DAV_Classes;

type
  // Test methods for class TBuildingBlocks32
  TestTBuildingBlocks32 = class(TTestCase)
  strict private
    FBuildingBlocks32 : TBuildingBlocks32;
    FOnProcessCount   : Integer;
  private
    procedure OnProcessHandler(Sender: TObject; const Input: PDAVSingleFixedArray);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBasicSampleProcesssing;
    procedure TestBasicBlockProcesssing;
    procedure TestPowerOf2BlocksizesSampleProcesssing;
    procedure TestPowerOf2BlocksizesBlockProcesssing;
    procedure TestOverlapSampleProcesssing;
  end;

  // Test methods for class TBuildingBlocksCircular32
  TestTBuildingBlocksCircular32 = class(TTestCase)
  strict private
    FBuildingBlocksCircular32 : TBuildingBlocksCircular32;
    FOnProcessCount           : Integer;
  private
    procedure OnProcessHandler(Sender: TObject; const Input: PDAVSingleFixedArray);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBasicSampleProcesssing;
    procedure TestBasicBlockProcesssing;
    procedure TestPowerOf2BlocksizesSampleProcessing;
    procedure TestPowerOf2BlocksizesBlockProcessing;
  end;

  // Test methods for class TBuildingBlocks64
  TestTBuildingBlocks64 = class(TTestCase)
  strict private
    FBuildingBlocks64 : TBuildingBlocks64;
    FOnProcessCount   : Integer;
  private
    procedure OnProcessHandler(Sender: TObject; const Input: PDAVDoubleFixedArray);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBasicSampleProcesssing;
    procedure TestBasicBlockProcesssing;
    procedure TestPowerOf2BlocksizesSampleProcessing;
    procedure TestPowerOf2BlocksizesBlockProcessing;
  end;

  // Test methods for class TBuildingBlocksCircular64
  TestTBuildingBlocksCircular64 = class(TTestCase)
  strict private
    FBuildingBlocksCircular64 : TBuildingBlocksCircular64;
    FOnProcessCount           : Integer;
  private
    procedure OnProcessHandler(Sender: TObject; const Input: PDAVDoubleFixedArray);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBasicSampleProcesssing;
    procedure TestBasicBlockProcesssing;
    procedure TestPowerOf2BlocksizesSampleProcessing;
    procedure TestPowerOf2BlocksizesBlockProcessing;
  end;

implementation

uses
  SysUtils;

const
  CmaxOrder = 15;

{ TestTBuildingBlocks32 }

procedure TestTBuildingBlocks32.SetUp;
begin
 FBuildingBlocks32 := TBuildingBlocks32.Create;
 with FBuildingBlocks32 do
  begin
   OnProcess := OnProcessHandler;
   BlockSize := 1 shl 8;
   OverlapSize := BlockSize shr 1;
  end;
end;

procedure TestTBuildingBlocks32.TearDown;
begin
 FreeAndNil(FBuildingBlocks32);
end;

procedure TestTBuildingBlocks32.TestBasicSampleProcesssing;
var
  Sample : Integer;
begin
 with FBuildingBlocks32 do
  begin
   FOnProcessCount := 0;
   for Sample := 0 to BlockSize - 1 do ProcessSample32(FOnProcessCount);
   CheckEquals(FOnProcessCount, 2);
  end;
end;

procedure TestTBuildingBlocks32.TestBasicBlockProcesssing;
var
  Sample : Integer;
  Data   : PDAVSingleFixedArray;
begin
 with FBuildingBlocks32 do
  begin
   // process block
   GetMem(Data, BlockSize * SizeOf(Single));
   try
    FOnProcessCount := 0;

    // prefill data
    for Sample := 0 to (BlockSize div 2) - 1
     do Data[Sample] := 0;
    for Sample := (BlockSize div 2) to BlockSize - 1
     do Data[Sample] := 1;

    ProcessBlock32(Data, BlockSize);

    CheckEquals(FOnProcessCount, 2);
   finally
    Dispose(Data);
   end;
  end;
end;

procedure TestTBuildingBlocks32.TestPowerOf2BlocksizesSampleProcesssing;
var
  Order  : Integer;
  Sample : Integer;
begin
 with FBuildingBlocks32 do
  for Order := 4 to CmaxOrder do
   begin
    // setup test
    BlockSize := 1 shl Order;
    OverlapSize := BlockSize shr 1;
    FOnProcessCount := 0;
    Reset;

    for Sample := 0 to BlockSize - 1 do ProcessSample32(FOnProcessCount);
    CheckEquals(FOnProcessCount, 2);
   end;
end;

procedure TestTBuildingBlocks32.TestPowerOf2BlocksizesBlockProcesssing;
var
  Order  : Integer;
  Sample : Integer;
  Data   : PDAVSingleFixedArray;
begin
 with FBuildingBlocks32 do
  for Order := 4 to CmaxOrder do
   begin
    // setup test
    BlockSize := 1 shl Order;
    OverlapSize := BlockSize shr 1;
    FOnProcessCount := 0;
    Reset;

    // process block
    GetMem(Data, BlockSize * SizeOf(Single));
    try
     // prefill data
     for Sample := 0 to (BlockSize div 2) - 1
      do Data[Sample] := 0;
     for Sample := (BlockSize div 2) to BlockSize - 1
      do Data[Sample] := 1;

     ProcessBlock32(Data, BlockSize);
    finally
     Dispose(Data);
    end;

    CheckEquals(FOnProcessCount, 2);
   end;
end;

procedure TestTBuildingBlocks32.TestOverlapSampleProcesssing;
var
  Divider : Integer;
  Sample : Integer;
begin
 with FBuildingBlocks32 do
  begin
   BlockSize := 1 shl 13;

   for Divider := 0 to 7 do
    begin
     // setup test
     OverlapSize := BlockSize - (BlockSize shr Divider);
     FOnProcessCount := 0;
     Reset;

     for Sample := 0 to BlockSize - 1 do ProcessSample32(FOnProcessCount);
     CheckEquals(FOnProcessCount, 1 shl Divider);
    end;
  end;
end;

procedure TestTBuildingBlocks32.OnProcessHandler(Sender: TObject;
  const Input: PDAVSingleFixedArray);
var
  Sample : Integer;
begin
 with FBuildingBlocks32 do
  for Sample := OverlapSize to BlockSize - 1
   do CheckEquals(Input^[Sample], FOnProcessCount);
 Inc(FOnProcessCount);
end;


{ TestTBuildingBlocksCircular32 }

procedure TestTBuildingBlocksCircular32.SetUp;
begin
 FBuildingBlocksCircular32 := TBuildingBlocksCircular32.Create;
 with FBuildingBlocksCircular32 do
  begin
   OnProcess := OnProcessHandler;
   BlockSize := 1 shl 8;
   OverlapSize := BlockSize shr 1;
  end;
end;

procedure TestTBuildingBlocksCircular32.TearDown;
begin
 FreeAndNil(FBuildingBlocksCircular32);
end;

procedure TestTBuildingBlocksCircular32.TestBasicSampleProcesssing;
var
  Sample : Integer;
begin
 with FBuildingBlocksCircular32 do
  begin
   FOnProcessCount := 0;
   for Sample := 0 to BlockSize - 1 do ProcessSample32(FOnProcessCount);
   CheckEquals(FOnProcessCount, 2);
  end;
end;

procedure TestTBuildingBlocksCircular32.TestBasicBlockProcesssing;
var
  Sample : Integer;
  Data   : PDAVSingleFixedArray;
begin
 with FBuildingBlocksCircular32 do
  begin
   // process block
   GetMem(Data, BlockSize * SizeOf(Single));
   try
    FOnProcessCount := 0;

    // prefill data
    for Sample := 0 to (BlockSize div 2) - 1
     do Data[Sample] := 0;
    for Sample := (BlockSize div 2) to BlockSize - 1
     do Data[Sample] := 1;

    ProcessBlock32(Data, BlockSize);

    CheckEquals(FOnProcessCount, 2);
   finally
    Dispose(Data);
   end;
  end;
end;

procedure TestTBuildingBlocksCircular32.TestPowerOf2BlocksizesSampleProcessing;
var
  Order  : Integer;
  Sample : Integer;
begin
 with FBuildingBlocksCircular32 do
  for Order := 4 to CMaxOrder - 1 do
   begin
    // setup test
    BlockSize := 1 shl Order;
    OverlapSize := BlockSize shr 1;
    FOnProcessCount := 0;
    Reset;

    for Sample := 0 to BlockSize - 1 do ProcessSample32(FOnProcessCount);
    CheckEquals(FOnProcessCount, 2);
   end;
end;

procedure TestTBuildingBlocksCircular32.TestPowerOf2BlocksizesBlockProcessing;
var
  Order  : Integer;
  Sample : Integer;
  Data   : PDAVSingleFixedArray;
begin
 with FBuildingBlocksCircular32 do
  for Order := 4 to CmaxOrder do
   begin
    // setup test
    BlockSize := 1 shl Order;
    OverlapSize := BlockSize shr 1;
    FOnProcessCount := 0;
    Reset;

    // process block
    GetMem(Data, BlockSize * SizeOf(Single));
    try
     // prefill data
     for Sample := 0 to (BlockSize div 2) - 1
      do Data[Sample] := 0;
     for Sample := (BlockSize div 2) to BlockSize - 1
      do Data[Sample] := 1;

     ProcessBlock32(Data, BlockSize);
    finally
     Dispose(Data);
    end;

    CheckEquals(FOnProcessCount, 2);
   end;
end;

procedure TestTBuildingBlocksCircular32.OnProcessHandler(Sender: TObject;
  const Input: PDAVSingleFixedArray);
var
  Sample : Integer;
begin
 with FBuildingBlocksCircular32 do
  for Sample := OverlapSize to BlockSize - 1
   do CheckEquals(Input^[Sample], FOnProcessCount);
 Inc(FOnProcessCount);
end;



{ TestTBuildingBlocks64 }

procedure TestTBuildingBlocks64.SetUp;
begin
 FBuildingBlocks64 := TBuildingBlocks64.Create;
 with FBuildingBlocks64 do
  begin
   OnProcess := OnProcessHandler;
   BlockSize := 1 shl 8;
   OverlapSize := BlockSize shr 1;
  end;
end;

procedure TestTBuildingBlocks64.TearDown;
begin
 FreeAndNil(FBuildingBlocks64);
end;

procedure TestTBuildingBlocks64.TestBasicSampleProcesssing;
var
  Sample : Integer;
begin
 with FBuildingBlocks64 do
  begin
   FOnProcessCount := 0;
   for Sample := 0 to BlockSize - 1 do ProcessSample64(FOnProcessCount);
   CheckEquals(FOnProcessCount, 2);
  end;
end;

procedure TestTBuildingBlocks64.TestBasicBlockProcesssing;
var
  Sample : Integer;
  Data   : PDAVDoubleFixedArray;
begin
 with FBuildingBlocks64 do
  begin
   // process block
   GetMem(Data, 3 * (BlockSize div 2) * SizeOf(Double));
   try
    FOnProcessCount := 0;

    // prefill data
    for Sample := 0 to (BlockSize div 2) - 1
     do Data[Sample] := 0;
    for Sample := (BlockSize div 2) to BlockSize - 1
     do Data[Sample] := 1;

    ProcessBlock64(Data, BlockSize);

    CheckEquals(FOnProcessCount, 2);
   finally
    Dispose(Data);
   end;
  end;
end;

procedure TestTBuildingBlocks64.TestPowerOf2BlocksizesSampleProcessing;
var
  Order  : Integer;
  Sample : Integer;
begin
 with FBuildingBlocks64 do
  for Order := 4 to CmaxOrder do
   begin
    // setup test
    BlockSize := 1 shl Order;
    OverlapSize := BlockSize shr 1;
    FOnProcessCount := 0;
    Reset;

    for Sample := 0 to BlockSize - 1 do ProcessSample64(FOnProcessCount);
    CheckEquals(FOnProcessCount, 2);
   end;
end;

procedure TestTBuildingBlocks64.TestPowerOf2BlocksizesBlockProcessing;
var
  Order  : Integer;
  Sample : Integer;
  Data   : PDAVDoubleFixedArray;
begin
 with FBuildingBlocks64 do
  for Order := 4 to CmaxOrder do
   begin
    // setup test
    BlockSize := 1 shl Order;
    OverlapSize := BlockSize shr 1;
    FOnProcessCount := 0;
    Reset;

    // process block
    GetMem(Data, BlockSize * SizeOf(Double));
    try
     // prefill data
     for Sample := 0 to (BlockSize div 2) - 1
      do Data[Sample] := 0;
     for Sample := (BlockSize div 2) to BlockSize - 1
      do Data[Sample] := 1;

     ProcessBlock64(Data, BlockSize);
    finally
     Dispose(Data);
    end;

    CheckEquals(FOnProcessCount, 2);
   end;
end;

procedure TestTBuildingBlocks64.OnProcessHandler(Sender: TObject;
  const Input: PDAVDoubleFixedArray);
var
  Sample : Integer;
begin
 with FBuildingBlocks64 do
  for Sample := OverlapSize to BlockSize - 1
   do CheckEquals(Input^[Sample], FOnProcessCount);
 Inc(FOnProcessCount);
end;



{ TestTBuildingBlocksCircular64 }

procedure TestTBuildingBlocksCircular64.SetUp;
begin
 FBuildingBlocksCircular64 := TBuildingBlocksCircular64.Create;
 with FBuildingBlocksCircular64 do
  begin
   OnProcess := OnProcessHandler;
   BlockSize := 1 shl 8;
   OverlapSize := BlockSize shr 1;
  end;
end;

procedure TestTBuildingBlocksCircular64.TearDown;
begin
 FreeAndNil(FBuildingBlocksCircular64);
end;

procedure TestTBuildingBlocksCircular64.TestBasicSampleProcesssing;
var
  Sample : Integer;
begin
 with FBuildingBlocksCircular64 do
  begin
   FOnProcessCount := 0;
   for Sample := 0 to BlockSize - 1 do ProcessSample64(FOnProcessCount);
   CheckEquals(FOnProcessCount, 2);
  end;
end;

procedure TestTBuildingBlocksCircular64.TestBasicBlockProcesssing;
var
  Sample : Integer;
  Data   : PDAVDoubleFixedArray;
begin
 with FBuildingBlocksCircular64 do
  begin
   // process block
   GetMem(Data, BlockSize * SizeOf(Double));
   try
    FOnProcessCount := 0;

    // prefill data
    for Sample := 0 to (BlockSize div 2) - 1
     do Data[Sample] := 0;
    for Sample := (BlockSize div 2) to BlockSize - 1
     do Data[Sample] := 1;

    ProcessBlock64(Data, BlockSize);

    CheckEquals(FOnProcessCount, 2);
   finally
    Dispose(Data);
   end;
  end;
end;

procedure TestTBuildingBlocksCircular64.TestPowerOf2BlocksizesSampleProcessing;
var
  Order  : Integer;
  Sample : Integer;
begin
 with FBuildingBlocksCircular64 do
  for Order := 4 to CmaxOrder do
   begin
    // setup test
    BlockSize := 1 shl Order;
    OverlapSize := BlockSize shr 1;
    FOnProcessCount := 0;
    Reset;

    for Sample := 0 to BlockSize - 1 do ProcessSample64(FOnProcessCount);
    CheckEquals(FOnProcessCount, 2);
   end;
end;

procedure TestTBuildingBlocksCircular64.TestPowerOf2BlocksizesBlockProcessing;
var
  Order  : Integer;
  Sample : Integer;
  Data   : PDAVDoubleFixedArray;
begin
 with FBuildingBlocksCircular64 do
  for Order := 4 to CmaxOrder do
   begin
    // setup test
    BlockSize := 1 shl Order;
    OverlapSize := BlockSize shr 1;
    FOnProcessCount := 0;
    Reset;

    // process block
    GetMem(Data, BlockSize * SizeOf(Double));
    try
     // prefill data
     for Sample := 0 to (BlockSize div 2) - 1
      do Data[Sample] := 0;
     for Sample := (BlockSize div 2) to BlockSize - 1
      do Data[Sample] := 1;

     ProcessBlock64(Data, BlockSize);
    finally
     Dispose(Data);
    end;

    CheckEquals(FOnProcessCount, 2);
   end;
end;

procedure TestTBuildingBlocksCircular64.OnProcessHandler(Sender: TObject;
  const Input: PDAVDoubleFixedArray);
var
  Sample : Integer;
begin
 with FBuildingBlocksCircular64 do
  for Sample := OverlapSize to BlockSize - 1
   do CheckEquals(Input^[Sample], FOnProcessCount);
 Inc(FOnProcessCount);
end;

procedure RegisterTestSuite;
var
  TS : TTestSuite;
begin
 TS := TTestSuite.Create('Building Blocks');
 TS.AddSuite(TestTBuildingBlocks32.Suite);
 TS.AddSuite(TestTBuildingBlocksCircular32.Suite);
 TS.AddSuite(TestTBuildingBlocks64.Suite);
 TS.AddSuite(TestTBuildingBlocksCircular64.Suite);
 RegisterTest(TS);
end;

initialization
  RegisterTestSuite;

end.
