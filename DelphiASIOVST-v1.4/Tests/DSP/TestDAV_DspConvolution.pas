unit TestDAV_DspConvolution;

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
  DAV_DspFftReal2Complex, DAV_DspConvolution;

type
  // Test methods for class TConvolution32
  TestTConvolution32 = class(TTestCase)
  strict private
    FConvolution32: TConvolution32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessBlock;
    procedure TestLoadImpulseResponse;
  end;

  // Test methods for class TConvolution64
  TestTConvolution64 = class(TTestCase)
  strict private
    FConvolution64: TConvolution64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessBlock;
    procedure TestLoadImpulseResponse;
  end;

  // Test methods for class TLowLatencyConvolution32
  TestTLowLatencyConvolution32 = class(TTestCase)
  strict private
    FLowLatencyConvolution32: TLowLatencyConvolution32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessBlock;
    procedure TestLoadImpulseResponse;
  end;

  // Test methods for class TLowLatencyConvolutionStereo32
  TestTLowLatencyConvolutionStereo32 = class(TTestCase)
  strict private
    FLowLatencyConvolutionStereo32: TLowLatencyConvolutionStereo32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessBlock;
  end;

  // Test methods for class TLowLatencyConvolution64
  TestTLowLatencyConvolution64 = class(TTestCase)
  strict private
    FLowLatencyConvolution64: TLowLatencyConvolution64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessBlock;
    procedure TestLoadImpulseResponse;
  end;

  // Test methods for class TLowLatencyConvolutionStereo64
  TestTLowLatencyConvolutionStereo64 = class(TTestCase)
  strict private
    FLowLatencyConvolutionStereo64: TLowLatencyConvolutionStereo64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessBlock;
  end;

implementation

uses
  SysUtils;


{ TestTConvolution32 }

procedure TestTConvolution32.SetUp;
begin
 FConvolution32 := TConvolution32.Create;
end;

procedure TestTConvolution32.TearDown;
begin
 FreeAndNil(FConvolution32);
end;

procedure TestTConvolution32.TestProcessBlock;
var
  Input, Output : TDAVSingleDynArray;
const
  CDataSize        : Cardinal = 32;
  CImpulseResponse : array [0..3] of Single = (1, 0.1, -0.1, -0.1);
begin
 // check if no SampleFrame is handed well
 FConvolution32.ProcessBlock(nil, nil, 0);

 // load constant impulse response
 FConvolution32.LoadImpulseResponse(@CImpulseResponse, 4);

 // create data arrays
 SetLength(Input, CDataSize);
 SetLength(Output, CDataSize);

 // check if data is processed
 FConvolution32.ProcessBlock(@Input[0], @Output[0], CDataSize);
end;

procedure TestTConvolution32.TestLoadImpulseResponse;
var
  Data       : TDAVSingleDynArray;
  DataFrames : Integer;
begin
  // check if no data handled correctly
  FConvolution32.LoadImpulseResponse(nil, 0);

  // load increasing impulse responses
  for DataFrames := 0 to 16 do
   begin
    SetLength(Data, DataFrames * 111);

    // check if dyn array handled correctly
    FConvolution32.LoadImpulseResponse(Data);

    // check if pointer input handled correctly
    FConvolution32.LoadImpulseResponse(@Data[0], Length(Data));
   end;

  // load random impulse responses
  for DataFrames := 0 to 16 do
   begin
    SetLength(Data, 1 + Random(8192));

    // check if dyn array handled correctly
    FConvolution32.LoadImpulseResponse(Data);
   end;
end;


{ TestTConvolution64 }

procedure TestTConvolution64.SetUp;
begin
  FConvolution64 := TConvolution64.Create;
end;

procedure TestTConvolution64.TearDown;
begin
 FreeAndNil(FConvolution64);
end;

procedure TestTConvolution64.TestProcessBlock;
var
  Input, Output : TDAVSingleDynArray;
const
  CDataSize        : Cardinal = 32;
  CImpulseResponse : array [0..3] of Single = (1, 0.1, -0.1, -0.1);
begin
 // load constant impulse response
 FConvolution64.LoadImpulseResponse(@CImpulseResponse, 4);

 // check if no SampleFrame is handed well
 FConvolution64.ProcessBlock(nil, nil, 0);

 // create data arrays
 SetLength(Input, CDataSize);
 SetLength(Output, CDataSize);

 // check if data is processed
 FConvolution64.ProcessBlock(@Input[0], @Output[0], CDataSize);
end;

procedure TestTConvolution64.TestLoadImpulseResponse;
var
  Data       : TDAVDoubleDynArray;
  DataFrames : Integer;
begin
  // Check if no data handled correctly
  FConvolution64.LoadImpulseResponse(nil, 0);

  // load increasing impulse responses
  for DataFrames := 0 to 16 do
   begin
    SetLength(Data, DataFrames * 111);

    // Check if dyn array handled correctly
    FConvolution64.LoadImpulseResponse(Data);

    // Check if pointer input handled correctly
    FConvolution64.LoadImpulseResponse(@Data[0], Length(Data));
   end;

  // load random impulse responses
  for DataFrames := 0 to 16 do
   begin
    SetLength(Data, 1 + Random(8192));

    // Check if dyn array handled correctly
    FConvolution64.LoadImpulseResponse(Data);
   end;
end;


{ TestTLowLatencyConvolution32 }

procedure TestTLowLatencyConvolution32.SetUp;
begin
  FLowLatencyConvolution32 := TLowLatencyConvolution32.Create;
end;

procedure TestTLowLatencyConvolution32.TearDown;
begin
 FreeAndNil(FLowLatencyConvolution32);
end;

procedure TestTLowLatencyConvolution32.TestProcessBlock;
begin
 // Check if no SampleFrame is handed well
 FLowLatencyConvolution32.ProcessBlock(nil, nil, 0);
 FLowLatencyConvolution32.ProcessBlock(nil, 0);
end;

procedure TestTLowLatencyConvolution32.TestLoadImpulseResponse;
var
  Data       : TDAVSingleDynArray;
  DataFrames : Integer;
begin
  // Check if no data handled correctly
  FLowLatencyConvolution32.LoadImpulseResponse(nil, 0);

  for DataFrames := 0 to 16 do
   begin
    SetLength(Data, DataFrames * 111);

    // Check if dyn array handled correctly
    FLowLatencyConvolution32.LoadImpulseResponse(Data);

    // Check if pointer input handled correctly
    FLowLatencyConvolution32.LoadImpulseResponse(@Data[0], Length(Data));
   end;
end;


{ TestTLowLatencyConvolutionStereo32 }

procedure TestTLowLatencyConvolutionStereo32.SetUp;
begin
 FLowLatencyConvolutionStereo32 := TLowLatencyConvolutionStereo32.Create;
end;

procedure TestTLowLatencyConvolutionStereo32.TearDown;
begin
 FreeAndNil(FLowLatencyConvolutionStereo32);
end;

procedure TestTLowLatencyConvolutionStereo32.TestProcessBlock;
begin
 // Check if no SampleFrame is handed well
 FLowLatencyConvolutionStereo32.ProcessBlock(nil, nil, 0);
end;


{ TestTLowLatencyConvolution64 }

procedure TestTLowLatencyConvolution64.SetUp;
begin
  FLowLatencyConvolution64 := TLowLatencyConvolution64.Create;
end;

procedure TestTLowLatencyConvolution64.TearDown;
begin
 FreeAndNil(FLowLatencyConvolution64);
end;

procedure TestTLowLatencyConvolution64.TestProcessBlock;
begin
 // Check if no SampleFrame is handed well
 FLowLatencyConvolution64.ProcessBlock(nil, nil, 0);
 FLowLatencyConvolution64.ProcessBlock(nil, 0);
end;

procedure TestTLowLatencyConvolution64.TestLoadImpulseResponse;
var
  Data       : TDAVDoubleDynArray;
  DataFrames : Integer;
begin
  // Check if no data handled correctly
  FLowLatencyConvolution64.LoadImpulseResponse(nil, 0);

  for DataFrames := 0 to 16 do
   begin
    SetLength(Data, DataFrames * 111);

    // Check if dyn array handled correctly
    FLowLatencyConvolution64.LoadImpulseResponse(Data);

    // Check if pointer input handled correctly
    FLowLatencyConvolution64.LoadImpulseResponse(@Data[0], Length(Data));
   end;
end;


{ TestTLowLatencyConvolutionStereo64 }

procedure TestTLowLatencyConvolutionStereo64.SetUp;
begin
  FLowLatencyConvolutionStereo64 := TLowLatencyConvolutionStereo64.Create;
end;

procedure TestTLowLatencyConvolutionStereo64.TearDown;
begin
 FreeAndNil(FLowLatencyConvolutionStereo64);
end;

procedure TestTLowLatencyConvolutionStereo64.TestProcessBlock;
begin
 // Check if no SampleFrame is handed well
 FLowLatencyConvolutionStereo64.ProcessBlock(nil, nil, 0);
end;

procedure RegisterTestSuite;
var
  TS : TTestSuite;
begin
 TS := TTestSuite.Create('Convolution Tests');
 TS.AddSuite(TestTConvolution32.Suite);
 TS.AddSuite(TestTConvolution64.Suite);
 TS.AddSuite(TestTLowLatencyConvolution32.Suite);
 TS.AddSuite(TestTLowLatencyConvolutionStereo32.Suite);
 TS.AddSuite(TestTLowLatencyConvolution64.Suite);
 TS.AddSuite(TestTLowLatencyConvolutionStereo64.Suite);
 RegisterTest(TS);
end;

initialization
  RegisterTestSuite;

end.
