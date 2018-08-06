unit TestDAV_DspChorus;

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
  TestFramework, DAV_DspLFO, DAV_Common, Classes, DAV_DspChorus, DAV_DspCommon;

type
  // Test methods for class TCustomDspChorus
  TestTCustomDspChorus = class(TTestCase)
  strict private
    FCustomDspChorus: TCustomDspChorus;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
  end;

  // Test methods for class TDspChorus32
  TestTDspChorus32 = class(TTestCase)
  strict private
    FDspChorus32: TDspChorus32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
    procedure TestReset;
  end;

  // Test methods for class TDspChorus64
  TestTDspChorus64 = class(TTestCase)
  strict private
    FDspChorus64: TDspChorus64;
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

procedure TestTCustomDspChorus.SetUp;
begin
 FCustomDspChorus := TCustomDspChorus.Create;
end;

procedure TestTCustomDspChorus.TearDown;
begin
 FreeAndNil(FCustomDspChorus);
end;

procedure TestTCustomDspChorus.TestReset;
begin
 FCustomDspChorus.Reset;
 // TODO: Validate method results
end;


{ TestTDspChorus32 }

procedure TestTDspChorus32.SetUp;
begin
  FDspChorus32 := TDspChorus32.Create;
end;

procedure TestTDspChorus32.TearDown;
begin
  FDspChorus32.Free;
  FDspChorus32 := nil;
end;

procedure TestTDspChorus32.TestProcess;
var
  ReturnValue : Single;
  Input       : Single;
  Sample      : Integer;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspChorus32 do
  begin
   Mix   := 1;
   Depth := 1;
   Drift := 0.1;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 Input := 1;
 ReturnValue := FDspChorus32.ProcessSample32(Input);

 // Validate method results
 CheckTrue(ReturnValue = 0);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames do
  begin
   ReturnValue := FDspChorus32.ProcessSample32(Input);
   CheckTrue((ReturnValue > -1) and (ReturnValue < 1));
  end;
end;

procedure TestTDspChorus32.TestReset;
var
  Input       : Single;
  Sample      : Integer;
  Value       : Single;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspChorus32 do
  begin
   Mix   := 1;
   Depth := 1;
   Drift := 0;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 Input := 1;
 FDspChorus32.ProcessSample32(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspChorus32.ProcessSample32(Input);

 // store current value
 Value := FDspChorus32.ProcessSample32(Input);

 // reset quque
 FDspChorus32.Reset;

 // Call chorus function
 Input := 1;
 FDspChorus32.ProcessSample32(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspChorus32.ProcessSample32(Input);

 CheckEquals(Value, FDspChorus32.ProcessSample32(Input));
end;


{ TestTDspChorus64 }

procedure TestTDspChorus64.SetUp;
begin
  FDspChorus64 := TDspChorus64.Create;
end;

procedure TestTDspChorus64.TearDown;
begin
  FDspChorus64.Free;
  FDspChorus64 := nil;
end;

procedure TestTDspChorus64.TestProcess;
var
  ReturnValue : Single;
  Input       : Single;
  Sample      : Integer;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspChorus64 do
  begin
   Mix   := 1;
   Depth := 1;
   Drift := 0.1;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 Input := 1;
 ReturnValue := FDspChorus64.ProcessSample64(Input);

 // Validate method results
 CheckTrue(ReturnValue = 0);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to 1000 do
  begin
   ReturnValue := FDspChorus64.ProcessSample64(Input);
   CheckTrue((ReturnValue > -1) and (ReturnValue < 1));
  end;
end;

procedure TestTDspChorus64.TestReset;
var
  Input       : Single;
  Sample      : Integer;
  Value       : Single;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspChorus64 do
  begin
   Mix   := 1;
   Depth := 1;
   Drift := 0;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 Input := 1;
 FDspChorus64.ProcessSample64(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspChorus64.ProcessSample64(Input);

 // store current value
 Value := FDspChorus64.ProcessSample64(Input);

 // reset quque
 FDspChorus64.Reset;

 // Call chorus function
 Input := 1;
 FDspChorus64.ProcessSample64(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspChorus64.ProcessSample64(Input);

 CheckEquals(Value, FDspChorus64.ProcessSample64(Input));
end;

procedure RegisterTestSuite;
var
  TS : TTestSuite;
begin
 TS := TTestSuite.Create('Chorus Tests');
 TS.AddSuite(TestTCustomDspChorus.Suite);
 TS.AddSuite(TestTDspChorus32.Suite);
 TS.AddSuite(TestTDspChorus64.Suite);
 RegisterTest(TS);
end;

initialization
  RegisterTestSuite;

end.
