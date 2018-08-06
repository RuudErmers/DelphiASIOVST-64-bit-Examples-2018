unit TestDAV_DspFilterBasicsAutomatable;

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
  TestFramework, DAV_DspFilterBasicsAutomatable, Classes, DAV_DspCommon,
  DAV_Complex, DAV_Common, DAV_DspFilter;

type
  // Test methods for class TAutomatableGainFilter
  TestTAutomatableGainFilter = class(TTestCase)
  strict private
    FAutomatableGainFilter: TAutomatableGainFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatablePeakFilter
  TestTAutomatablePeakFilter = class(TTestCase)
  strict private
    FAutomatablePeakFilter: TAutomatablePeakFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableAllpassFilter
  TestTAutomatableAllpassFilter = class(TTestCase)
  strict private
    FAutomatableAllpassFilter: TAutomatableAllpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableLowShelfFilter
  TestTAutomatableLowShelfFilter = class(TTestCase)
  strict private
    FAutomatableLowShelfFilter: TAutomatableLowShelfFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableLowShelfAFilter
  TestTAutomatableLowShelfAFilter = class(TTestCase)
  strict private
    FAutomatableLowShelfAFilter: TAutomatableLowShelfAFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableLowShelfBFilter
  TestTAutomatableLowShelfBFilter = class(TTestCase)
  strict private
    FAutomatableLowShelfBFilter: TAutomatableLowShelfBFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableHighShelfFilter
  TestTAutomatableHighShelfFilter = class(TTestCase)
  strict private
    FAutomatableHighShelfFilter: TAutomatableHighShelfFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableHighShelfAFilter
  TestTAutomatableHighShelfAFilter = class(TTestCase)
  strict private
    FAutomatableHighShelfAFilter: TAutomatableHighShelfAFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableHighShelfBFilter
  TestTAutomatableHighShelfBFilter = class(TTestCase)
  strict private
    FAutomatableHighShelfBFilter: TAutomatableHighShelfBFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableHighcutFilter
  TestTAutomatableHighcutFilter = class(TTestCase)
  strict private
    FAutomatableHighcutFilter: TAutomatableHighcutFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableLowcutFilter
  TestTAutomatableLowcutFilter = class(TTestCase)
  strict private
    FAutomatableLowcutFilter: TAutomatableLowcutFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableBandpassFilter
  TestTAutomatableBandpassFilter = class(TTestCase)
  strict private
    FAutomatableBandpassFilter: TAutomatableBandpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableNotchFilter
  TestTAutomatableNotchFilter = class(TTestCase)
  strict private
    FAutomatableNotchFilter: TAutomatableNotchFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

implementation

uses
  SysUtils;

{ TestTAutomatableGainFilter }

procedure TestTAutomatableGainFilter.SetUp;
begin
  FAutomatableGainFilter := TAutomatableGainFilter.Create;
end;

procedure TestTAutomatableGainFilter.TearDown;
begin
 FreeAndNil(FAutomatableGainFilter);
end;

procedure TestTAutomatableGainFilter.TestProcessSample;
begin
 with FAutomatableGainFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(abs(ProcessSample64(CHalf32) - dB_to_Amp(Gain) * CHalf32) < 1E-7, 'ProcessSample64(0.5)  <> 0.5');
   CheckTrue(abs(ProcessSample64(1.0) - dB_to_Amp(Gain)) < 1E-7, 'ProcessSample64(1)  <> 1');
  end;
end;


{ TestTAutomatablePeakFilter }

procedure TestTAutomatablePeakFilter.SetUp;
begin
 FAutomatablePeakFilter := TAutomatablePeakFilter.Create;
end;

procedure TestTAutomatablePeakFilter.TearDown;
begin
 FreeAndNil(FAutomatablePeakFilter);
end;

procedure TestTAutomatablePeakFilter.TestProcessSample;
begin
 with FAutomatablePeakFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1) > dB_to_Amp(Gain), 'ProcessSample64(1) > 1');
  end;
end;


{ TestTAutomatableAllpassFilter }

procedure TestTAutomatableAllpassFilter.SetUp;
begin
 FAutomatableAllpassFilter := TAutomatableAllpassFilter.Create;
end;

procedure TestTAutomatableAllpassFilter.TearDown;
begin
 FreeAndNil(FAutomatableAllpassFilter);
end;

procedure TestTAutomatableAllpassFilter.TestProcessSample;
begin
 with FAutomatableAllpassFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1) > dB_to_Amp(Gain), 'ProcessSample64(1) > 1');
  end;
end;

{ TestTAutomatableLowShelfFilter }

procedure TestTAutomatableLowShelfFilter.SetUp;
begin
 FAutomatableLowShelfFilter := TAutomatableLowShelfFilter.Create;
end;

procedure TestTAutomatableLowShelfFilter.TearDown;
begin
 FreeAndNil(FAutomatableLowShelfFilter);
end;

procedure TestTAutomatableLowShelfFilter.TestProcessSample;
begin
 with FAutomatableLowShelfFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1) > dB_to_Amp(Gain), 'ProcessSample64(1) > 1');
  end;
end;


{ TestTAutomatableLowShelfAFilter }

procedure TestTAutomatableLowShelfAFilter.SetUp;
begin
 FAutomatableLowShelfAFilter := TAutomatableLowShelfAFilter.Create;
end;

procedure TestTAutomatableLowShelfAFilter.TearDown;
begin
 FreeAndNil(FAutomatableLowShelfAFilter);
end;

procedure TestTAutomatableLowShelfAFilter.TestProcessSample;
begin
 with FAutomatableLowShelfAFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1) > dB_to_Amp(Gain), 'ProcessSample64(1) > 1');
  end;
end;


{ TestTAutomatableLowShelfBFilter }

procedure TestTAutomatableLowShelfBFilter.SetUp;
begin
 FAutomatableLowShelfBFilter := TAutomatableLowShelfBFilter.Create;
end;

procedure TestTAutomatableLowShelfBFilter.TearDown;
begin
 FreeAndNil(FAutomatableLowShelfBFilter);
end;

procedure TestTAutomatableLowShelfBFilter.TestProcessSample;
begin
 with FAutomatableLowShelfBFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1) > dB_to_Amp(Gain), 'ProcessSample64(1) > 1');
  end;
end;


{ TestTAutomatableHighShelfFilter }

procedure TestTAutomatableHighShelfFilter.SetUp;
begin
 FAutomatableHighShelfFilter := TAutomatableHighShelfFilter.Create;
end;

procedure TestTAutomatableHighShelfFilter.TearDown;
begin
 FreeAndNil(FAutomatableHighShelfFilter);
end;

procedure TestTAutomatableHighShelfFilter.TestProcessSample;
begin
 with FAutomatableHighShelfFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1) > dB_to_Amp(Gain), 'ProcessSample64(1) > 1');
  end;
end;


{ TestTAutomatableHighShelfAFilter }

procedure TestTAutomatableHighShelfAFilter.SetUp;
begin
 FAutomatableHighShelfAFilter := TAutomatableHighShelfAFilter.Create;
end;

procedure TestTAutomatableHighShelfAFilter.TearDown;
begin
 FreeAndNil(FAutomatableHighShelfAFilter);
end;

procedure TestTAutomatableHighShelfAFilter.TestProcessSample;
begin
 with FAutomatableHighShelfAFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1) > dB_to_Amp(Gain), 'ProcessSample64(1) > 1');
  end;
end;


{ TestTAutomatableHighShelfBFilter }

procedure TestTAutomatableHighShelfBFilter.SetUp;
begin
 FAutomatableHighShelfBFilter := TAutomatableHighShelfBFilter.Create;
end;

procedure TestTAutomatableHighShelfBFilter.TearDown;
begin
 FreeAndNil(FAutomatableHighShelfBFilter);
end;

procedure TestTAutomatableHighShelfBFilter.TestProcessSample;
begin
 with FAutomatableHighShelfBFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1) > dB_to_Amp(Gain), 'ProcessSample64(1) > 1');
  end;
end;


{ TestTAutomatableHighcutFilter }

procedure TestTAutomatableHighcutFilter.SetUp;
begin
 FAutomatableHighcutFilter := TAutomatableHighcutFilter.Create;
end;

procedure TestTAutomatableHighcutFilter.TearDown;
begin
 FreeAndNil(FAutomatableHighcutFilter);
end;

procedure TestTAutomatableHighcutFilter.TestProcessSample;
begin
 with FAutomatableHighcutFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1) > dB_to_Amp(Gain), 'ProcessSample64(1) > 1');
  end;
end;


{ TestTAutomatableLowcutFilter }

procedure TestTAutomatableLowcutFilter.SetUp;
begin
  FAutomatableLowcutFilter := TAutomatableLowcutFilter.Create;
end;

procedure TestTAutomatableLowcutFilter.TearDown;
begin
 FreeAndNil(FAutomatableLowcutFilter);
end;

procedure TestTAutomatableLowcutFilter.TestProcessSample;
begin
 with FAutomatableLowcutFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1) > dB_to_Amp(Gain), 'ProcessSample64(1) > 1');
  end;
end;


{ TestTAutomatableBandpassFilter }

procedure TestTAutomatableBandpassFilter.SetUp;
begin
  FAutomatableBandpassFilter := TAutomatableBandpassFilter.Create;
end;

procedure TestTAutomatableBandpassFilter.TearDown;
begin
 FreeAndNil(FAutomatableBandpassFilter);
end;

procedure TestTAutomatableBandpassFilter.TestProcessSample;
begin
 with FAutomatableBandpassFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1) > dB_to_Amp(Gain), 'ProcessSample64(1) > 1');
  end;
end;


{ TestTAutomatableNotchFilter }

procedure TestTAutomatableNotchFilter.SetUp;
begin
  FAutomatableNotchFilter := TAutomatableNotchFilter.Create;
end;

procedure TestTAutomatableNotchFilter.TearDown;
begin
 FreeAndNil(FAutomatableNotchFilter);
end;

procedure TestTAutomatableNotchFilter.TestProcessSample;
begin
 with FAutomatableNotchFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample64(0.0), 0, 'ProcessSample64(0.0) <> 0');
   CheckTrue(ProcessSample64(1) > dB_to_Amp(Gain), 'ProcessSample64(1) > 1');
  end;
end;

procedure RegisterTestSuite;
var
  TS : TTestSuite;
begin
 TS := TTestSuite.Create('Automatable Filter Tests');
 TS.AddSuite(TestTAutomatableGainFilter.Suite);
 TS.AddSuite(TestTAutomatablePeakFilter.Suite);
 TS.AddSuite(TestTAutomatableAllpassFilter.Suite);
 TS.AddSuite(TestTAutomatableLowShelfFilter.Suite);
 TS.AddSuite(TestTAutomatableLowShelfAFilter.Suite);
 TS.AddSuite(TestTAutomatableLowShelfBFilter.Suite);
 TS.AddSuite(TestTAutomatableHighShelfFilter.Suite);
 TS.AddSuite(TestTAutomatableHighShelfAFilter.Suite);
 TS.AddSuite(TestTAutomatableHighShelfBFilter.Suite);
 TS.AddSuite(TestTAutomatableHighcutFilter.Suite);
 TS.AddSuite(TestTAutomatableLowcutFilter.Suite);
 TS.AddSuite(TestTAutomatableBandpassFilter.Suite);
 TS.AddSuite(TestTAutomatableNotchFilter.Suite);
 RegisterTest(TS);
end;

initialization
  RegisterTestSuite;

end.
