unit TestDAV_DspFilterBasics;

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
  TestFramework, DAV_Common, Classes, DAV_DspCommon, DAV_Complex, DAV_DspFilterBasics,
  DAV_DspFilter;

type
  // Test methods for class TBasicGainFilter
  TestTBasicGainFilter = class(TTestCase)
  strict private
    FBasicGainFilter: TBasicGainFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TBasicPeakFilter
  TestTBasicPeakFilter = class(TTestCase)
  strict private
    FBasicPeakFilter: TBasicPeakFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TBasicAllpassFilter
  TestTBasicAllpassFilter = class(TTestCase)
  strict private
    FBasicAllpassFilter: TBasicAllpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TBasicLowShelfFilter
  TestTBasicLowShelfFilter = class(TTestCase)
  strict private
    FBasicLowShelfFilter: TBasicLowShelfFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TBasicLowShelfAFilter
  TestTBasicLowShelfAFilter = class(TTestCase)
  strict private
    FBasicLowShelfAFilter: TBasicLowShelfAFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TBasicLowShelfBFilter
  TestTBasicLowShelfBFilter = class(TTestCase)
  strict private
    FBasicLowShelfBFilter: TBasicLowShelfBFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TBasicHighShelfFilter
  TestTBasicHighShelfFilter = class(TTestCase)
  strict private
    FBasicHighShelfFilter: TBasicHighShelfFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TBasicHighShelfAFilter
  TestTBasicHighShelfAFilter = class(TTestCase)
  strict private
    FBasicHighShelfAFilter: TBasicHighShelfAFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TBasicHighShelfBFilter
  TestTBasicHighShelfBFilter = class(TTestCase)
  strict private
    FBasicHighShelfBFilter: TBasicHighShelfBFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TBasicHighcutFilter
  TestTBasicHighcutFilter = class(TTestCase)
  strict private
    FBasicHighcutFilter: TBasicHighcutFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TBasicLowcutFilter
  TestTBasicLowcutFilter = class(TTestCase)
  strict private
    FBasicLowcutFilter: TBasicLowcutFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TBasicBandpassFilter
  TestTBasicBandpassFilter = class(TTestCase)
  strict private
    FBasicBandpassFilter: TBasicBandpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TBasicNotchFilter
  TestTBasicNotchFilter = class(TTestCase)
  strict private
    FBasicNotchFilter: TBasicNotchFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

implementation

uses
  SysUtils;

{ TestTBasicGainFilter }

procedure TestTBasicGainFilter.SetUp;
begin
  FBasicGainFilter := TBasicGainFilter.Create;
end;

procedure TestTBasicGainFilter.TearDown;
begin
  FBasicGainFilter.Free;
  FBasicGainFilter := nil;
end;

procedure TestTBasicGainFilter.TestProcessSample;
begin
 with FBasicGainFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(abs(ProcessSample64(CHalf32) - dB_to_Amp(Gain) * CHalf32) < 1E-7, 'ProcessSample64(0.5)  <> 0.5');
   CheckTrue(abs(ProcessSample64(1.0) - dB_to_Amp(Gain)) < 1E-7, 'ProcessSample64(1.0)  <> 1');
  end;
end;


{ TestTBasicPeakFilter }

procedure TestTBasicPeakFilter.SetUp;
begin
 FBasicPeakFilter := TBasicPeakFilter.Create;
end;

procedure TestTBasicPeakFilter.TearDown;
begin
 FreeAndNil(FBasicPeakFilter);
end;

procedure TestTBasicPeakFilter.TestProcessSample;
begin
 with FBasicPeakFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1.0) > dB_to_Amp(Gain), 'ProcessSample64(1.0) > 1');
  end;
end;


{ TestTBasicAllpassFilter }

procedure TestTBasicAllpassFilter.SetUp;
begin
  FBasicAllpassFilter := TBasicAllpassFilter.Create;
end;

procedure TestTBasicAllpassFilter.TearDown;
begin
 FreeAndNil(FBasicAllpassFilter);
end;

procedure TestTBasicAllpassFilter.TestProcessSample;
begin
 with FBasicAllpassFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1.0) <= dB_to_Amp(Gain), 'ProcessSample64(1.0) > 1');
  end;
end;


{ TestTBasicLowShelfFilter }

procedure TestTBasicLowShelfFilter.SetUp;
begin
 FBasicLowShelfFilter := TBasicLowShelfFilter.Create;
end;

procedure TestTBasicLowShelfFilter.TearDown;
begin
 FreeAndNil(FBasicLowShelfFilter);
end;

procedure TestTBasicLowShelfFilter.TestProcessSample;
begin
 with FBasicLowShelfFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1.0) > dB_to_Amp(Gain), 'ProcessSample64(1.0) > 1');
  end;
end;


{ TestTBasicLowShelfAFilter }

procedure TestTBasicLowShelfAFilter.SetUp;
begin
 FBasicLowShelfAFilter := TBasicLowShelfAFilter.Create;
end;

procedure TestTBasicLowShelfAFilter.TearDown;
begin
 FreeAndNil(FBasicLowShelfAFilter);
end;

procedure TestTBasicLowShelfAFilter.TestProcessSample;
begin
 with FBasicLowShelfAFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1.0) > dB_to_Amp(Gain), 'ProcessSample64(1.0) > 1');
  end;
end;

{ TestTBasicLowShelfBFilter }

procedure TestTBasicLowShelfBFilter.SetUp;
begin
 FBasicLowShelfBFilter := TBasicLowShelfBFilter.Create;
end;

procedure TestTBasicLowShelfBFilter.TearDown;
begin
 FreeAndNil(FBasicLowShelfBFilter);
end;

procedure TestTBasicLowShelfBFilter.TestProcessSample;
begin
 with FBasicLowShelfBFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1.0) > dB_to_Amp(Gain), 'ProcessSample64(1.0) > 1');
  end;
end;


{ TestTBasicHighShelfFilter }

procedure TestTBasicHighShelfFilter.SetUp;
begin
 FBasicHighShelfFilter := TBasicHighShelfFilter.Create;
end;

procedure TestTBasicHighShelfFilter.TearDown;
begin
 FreeAndNil(FBasicHighShelfFilter);
end;

procedure TestTBasicHighShelfFilter.TestProcessSample;
begin
 with FBasicHighShelfFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1.0) > dB_to_Amp(Gain), 'ProcessSample64(1.0) > 1');
  end;
end;


{ TestTBasicHighShelfAFilter }

procedure TestTBasicHighShelfAFilter.SetUp;
begin
 FBasicHighShelfAFilter := TBasicHighShelfAFilter.Create;
end;

procedure TestTBasicHighShelfAFilter.TearDown;
begin
 FreeAndNil(FBasicHighShelfAFilter);
end;

procedure TestTBasicHighShelfAFilter.TestProcessSample;
begin
 with FBasicHighShelfAFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1.0) > dB_to_Amp(Gain), 'ProcessSample64(1.0) > 1');
  end;
end;


{ TestTBasicHighShelfBFilter }

procedure TestTBasicHighShelfBFilter.SetUp;
begin
 FBasicHighShelfBFilter := TBasicHighShelfBFilter.Create;
end;

procedure TestTBasicHighShelfBFilter.TearDown;
begin
 FreeAndNil(FBasicHighShelfBFilter);
end;

procedure TestTBasicHighShelfBFilter.TestProcessSample;
begin
 with FBasicHighShelfBFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1.0) > dB_to_Amp(Gain), 'ProcessSample64(1.0) > 1');
  end;
end;


{ TestTBasicHighcutFilter }

procedure TestTBasicHighcutFilter.SetUp;
begin
 FBasicHighcutFilter := TBasicHighcutFilter.Create;
end;

procedure TestTBasicHighcutFilter.TearDown;
begin
 FreeAndNil(FBasicHighcutFilter);
end;

procedure TestTBasicHighcutFilter.TestProcessSample;
begin
 with FBasicHighcutFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1.0) > dB_to_Amp(Gain), 'ProcessSample64(1.0) > 1');
  end;
end;


{ TestTBasicLowcutFilter }

procedure TestTBasicLowcutFilter.SetUp;
begin
 FBasicLowcutFilter := TBasicLowcutFilter.Create;
end;

procedure TestTBasicLowcutFilter.TearDown;
begin
 FreeAndNil(FBasicLowcutFilter);
end;

procedure TestTBasicLowcutFilter.TestProcessSample;
begin
 with FBasicLowcutFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1.0) > dB_to_Amp(Gain), 'ProcessSample64(1.0) > 1');
  end;
end;


{ TestTBasicBandpassFilter }

procedure TestTBasicBandpassFilter.SetUp;
begin
 FBasicBandpassFilter := TBasicBandpassFilter.Create;
end;

procedure TestTBasicBandpassFilter.TearDown;
begin
 FreeAndNil(FBasicBandpassFilter);
end;

procedure TestTBasicBandpassFilter.TestProcessSample;
begin
 with FBasicBandpassFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1.0) > dB_to_Amp(Gain), 'ProcessSample64(1.0) > 1');
  end;
end;


{ TestTBasicNotchFilter }

procedure TestTBasicNotchFilter.SetUp;
begin
 FBasicNotchFilter := TBasicNotchFilter.Create;
end;

procedure TestTBasicNotchFilter.TearDown;
begin
 FreeAndNil(FBasicNotchFilter);
end;

procedure TestTBasicNotchFilter.TestProcessSample;
begin
 with FBasicNotchFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1.0) > dB_to_Amp(Gain), 'ProcessSample64(1.0) > 1');
  end;
end;

procedure RegisterTestSuite;
var
  TS : TTestSuite;
begin
 TS := TTestSuite.Create('Basic Filter Tests');
 TS.AddSuite(TestTBasicGainFilter.Suite);
 TS.AddSuite(TestTBasicPeakFilter.Suite);
 TS.AddSuite(TestTBasicAllpassFilter.Suite);
 TS.AddSuite(TestTBasicLowShelfFilter.Suite);
 TS.AddSuite(TestTBasicLowShelfAFilter.Suite);
 TS.AddSuite(TestTBasicLowShelfBFilter.Suite);
 TS.AddSuite(TestTBasicHighShelfFilter.Suite);
 TS.AddSuite(TestTBasicHighShelfAFilter.Suite);
 TS.AddSuite(TestTBasicHighShelfBFilter.Suite);
 TS.AddSuite(TestTBasicHighcutFilter.Suite);
 TS.AddSuite(TestTBasicLowcutFilter.Suite);
 TS.AddSuite(TestTBasicBandpassFilter.Suite);
 TS.AddSuite(TestTBasicNotchFilter.Suite);
 RegisterTest(TS);
end;

initialization
  RegisterTestSuite;

end.
