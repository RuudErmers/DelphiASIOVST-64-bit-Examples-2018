unit TestDAV_DspBarberpole;

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
  TestFramework, DAV_DspLFO, DAV_DspBarberpole, Classes, DAV_Common, DAV_DspCommon;

type
  // Test methods for class TDspBarberpole32
  TestTDspBarberpole32 = class(TTestCase)
  strict private
    FDspBarberpole32: TDspBarberpole32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
    procedure TestReset;
  end;

  // Test methods for class TDspBarberpole64
  TestTDspBarberpole64 = class(TTestCase)
  strict private
    FDspBarberpole64: TDspBarberpole64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
    procedure TestReset;
  end;

implementation

uses
  SysUtils;

{ TestTDspBarberpole32 }

procedure TestTDspBarberpole32.SetUp;
begin
  FDspBarberpole32 := TDspBarberpole32.Create;
end;

procedure TestTDspBarberpole32.TearDown;
begin
 FreeAndNil(FDspBarberpole32);
end;

procedure TestTDspBarberpole32.TestProcess;
var
  ReturnValue : Single;
  Sample      : Integer;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspBarberpole32 do
  begin
   Mix   := 1;
   Depth := 1;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 ReturnValue := FDspBarberpole32.ProcessSample32(1);

 // Validate method results
 CheckTrue(ReturnValue <> 0);

 // Test barberpole flanger process series is stable
 for Sample := 0 to CSampleFrames do
  begin
   ReturnValue := FDspBarberpole32.ProcessSample32(0);
   CheckTrue((ReturnValue > -1) and (ReturnValue < 1));
  end;
end;

procedure TestTDspBarberpole32.TestReset;
var
  Input  : Single;
  Sample : Integer;
  Value  : Single;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspBarberpole32 do
  begin
   Mix   := 1;
   Depth := 1;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 Input := 1;
 FDspBarberpole32.ProcessSample32(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspBarberpole32.ProcessSample32(Input);

 // store current value
 Value := FDspBarberpole32.ProcessSample32(Input);

 // reset quque
 FDspBarberpole32.Reset;

 // Call barberpole flanger function
 Input := 1;
 FDspBarberpole32.ProcessSample32(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspBarberpole32.ProcessSample32(Input);

 CheckEquals(Value, FDspBarberpole32.ProcessSample32(Input));
end;


{ TestTDspBarberpole64 }

procedure TestTDspBarberpole64.SetUp;
begin
  FDspBarberpole64 := TDspBarberpole64.Create;
end;

procedure TestTDspBarberpole64.TearDown;
begin
 FreeAndNil(FDspBarberpole64);
end;

procedure TestTDspBarberpole64.TestProcess;
var
  ReturnValue : Single;
  Sample      : Integer;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspBarberpole64 do
  begin
   Mix   := 1;
   Depth := 1;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 ReturnValue := FDspBarberpole64.ProcessSample64(1);

 // Validate method results
 CheckTrue(ReturnValue <> 0);

 // Test chorus process series
 for Sample := 0 to CSampleFrames do
  begin
   ReturnValue := FDspBarberpole64.ProcessSample64(0);
   CheckTrue((ReturnValue > -1) and (ReturnValue < 1));
  end;
end;

procedure TestTDspBarberpole64.TestReset;
var
  Input  : Single;
  Sample : Integer;
  Value  : Single;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspBarberpole64 do
  begin
   Mix   := 1;
   Depth := 1;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 Input := 1;
 FDspBarberpole64.ProcessSample64(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspBarberpole64.ProcessSample64(Input);

 // store current value
 Value := FDspBarberpole64.ProcessSample64(Input);

 // reset quque
 FDspBarberpole64.Reset;

 // Call barberpole flanger function
 Input := 1;
 FDspBarberpole64.ProcessSample64(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspBarberpole64.ProcessSample64(Input);

 CheckEquals(Value, FDspBarberpole64.ProcessSample64(Input));
end;

procedure RegisterTestSuite;
var
  TS : TTestSuite;
begin
 TS := TTestSuite.Create('Barberpole Flangers');
 TS.AddSuite(TestTDspBarberpole32.Suite);
 TS.AddSuite(TestTDspBarberpole64.Suite);
 RegisterTest(TS);
end;

initialization
  RegisterTestSuite;

end.
