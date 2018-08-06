unit TestDAV_DspFilterButterworth;

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

uses
  TestFramework, DAV_DspFilterButterworth, DAV_DspFilter, DAV_Common;

type
  // Test methods for class TButterworthFilter
  TestTButterworthLowPassFilter = class(TTestCase)
  strict private
    FButterworthLowPassFilter: TButterworthLowPassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestProcessSample;
    procedure TestMagnitudeSquared;
    procedure TestPhase;
    procedure TestComplexDouble;
  end;

  // Test methods for class TButterworthHighpassFilter
  TestTButterworthHighpassFilter = class(TTestCase)
  strict private
    FButterworthHighpassFilter: TButterworthHighpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestProcessSample;
    procedure TestMagnitudeSquared;
    procedure TestPhase;
    procedure TestComplexDouble;
  end;

  // Test methods for class TButterworthSplitBandFilter
  TestTButterworthSplitBandFilter = class(TTestCase)
  strict private
    FButterworthSplitBandFilter: TButterworthSplitBandFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestProcessSampleDouble;
    procedure TestProcessSampleSingle;
    procedure TestMagnitudeSquared;
  end;

implementation

uses
  Math, SysUtils;

{ TestTButterworthLowPassFilter }

procedure TestTButterworthLowPassFilter.SetUp;
begin
 FButterworthLowPassFilter := TButterworthLowPassFilter.Create;
end;

procedure TestTButterworthLowPassFilter.TearDown;
begin
 FreeAndNil(FButterworthLowPassFilter);
end;

procedure TestTButterworthLowPassFilter.TestCalculateCoefficients;
begin
 FButterworthLowPassFilter.CalculateCoefficients;
 // TODO: Validate method results
end;

procedure TestTButterworthLowPassFilter.TestProcessSample;
var
  ReturnValue : Double;
  Input       : Double;
begin
  // Initialize filter
  with FButterworthLowPassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 1;
   end;

  // Calculate filter
  Input := 1;
  ReturnValue := FButterworthLowPassFilter.ProcessSample64(Input);

  // Validate results
  CheckTrue((ReturnValue > 0) and (ReturnValue < 1));
end;

procedure TestTButterworthLowPassFilter.TestMagnitudeSquared;
var
  ReturnValue : Double;
begin
  // Initialize filter
  with FButterworthLowPassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 1;
   end;

  // Calculate filter
  with FButterworthLowPassFilter
   do ReturnValue := MagnitudeSquared(Frequency);

  // Validate results
  CheckTrue(abs(0.5 - ReturnValue) < 1E-15);
end;

procedure TestTButterworthLowPassFilter.TestPhase;
var
  ReturnValue : Double;
begin
  // Initialize filter
  with FButterworthLowPassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 1;
   end;

  // Calculate filter
  with FButterworthLowPassFilter
   do ReturnValue := Phase(Frequency);

  // Validate results
  CheckTrue(abs(ReturnValue) < 1E-15);
end;

procedure TestTButterworthLowPassFilter.TestComplexDouble;
var
  Imaginary : Double;
  Real      : Double;
  Frequency : Double;
begin
  // Initialize filter
  with FButterworthLowPassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 1;
   end;

  // Calculate filter
  Frequency := 1000;
  FButterworthLowPassFilter.Complex(Frequency, Real, Imaginary);

  // Validate results
  CheckTrue(abs(0.5 - (sqr(Real) + sqr(Imaginary))) < 1E-5, 'Magnitude calculation from complex failed');

  // Validate results
  CheckTrue(abs(ArcTan2(Imaginary, Real)) < 1E-5, 'Phase calculation from complex failed');
end;


{ TestTButterworthHighpassFilter }

procedure TestTButterworthHighpassFilter.SetUp;
begin
 FButterworthHighpassFilter := TButterworthHighpassFilter.Create;
end;

procedure TestTButterworthHighpassFilter.TearDown;
begin
 FreeAndNil(FButterworthHighpassFilter);
end;

procedure TestTButterworthHighpassFilter.TestCalculateCoefficients;
begin
 FButterworthHighpassFilter.CalculateCoefficients;
 // TODO: Validate method results
end;

procedure TestTButterworthHighpassFilter.TestProcessSample;
var
  ReturnValue : Double;
  Input       : Double;
begin
  with FButterworthHighPassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 1;
   end;

  // Calculate filter
  Input := 1;
  ReturnValue := FButterworthHighPassFilter.ProcessSample64(Input);

  // Validate results
  CheckTrue((ReturnValue > 0) and (ReturnValue < 1));
end;

procedure TestTButterworthHighpassFilter.TestMagnitudeSquared;
var
  ReturnValue : Double;
begin
  // Initialize filter
  with FButterworthHighPassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 1;
   end;

  // Calculate filter
  with FButterworthHighPassFilter
   do ReturnValue := MagnitudeSquared(Frequency);

  // Validate results
  CheckTrue(abs(0.5 - ReturnValue) < 1E-15);
end;

procedure TestTButterworthHighpassFilter.TestPhase;
var
  ReturnValue : Double;
begin
  // Initialize filter
  with FButterworthHighPassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 1;
   end;

  // Calculate filter
  with FButterworthHighPassFilter
   do ReturnValue := Phase(Frequency);

  // Validate results
  CheckTrue(abs(ReturnValue + Pi * 0.25) < 1E-15);
end;

procedure TestTButterworthHighpassFilter.TestComplexDouble;
var
  Im : Double;
  Re      : Double;
begin
  // Initialize filter
  with FButterworthHighPassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 1;
   end;

  // Calculate filter
  with FButterworthHighPassFilter
   do Complex(Frequency, Re, Im);

  // TODO: Validate method results
end;


{ TestTButterworthSplitBandFilter }

procedure TestTButterworthSplitBandFilter.SetUp;
begin
 FButterworthSplitBandFilter := TButterworthSplitBandFilter.Create;
end;

procedure TestTButterworthSplitBandFilter.TearDown;
begin
 FreeAndNil(FButterworthSplitBandFilter);
end;

procedure TestTButterworthSplitBandFilter.TestCalculateCoefficients;
begin
 FButterworthSplitBandFilter.CalculateCoefficients;
 // TODO: Validate method results
end;

procedure TestTButterworthSplitBandFilter.TestProcessSampleDouble;
var
  Highpass : Double;
  Lowpass  : Double;
  Input    : Double;
begin
  // Initialize filter
  with FButterworthSplitBandFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 1;
   end;

  // Calculate filter
  Input := 1;
  with FButterworthSplitBandFilter
   do ProcessSample64(Input, Lowpass, Highpass);

  // TODO: Validate method results
end;

procedure TestTButterworthSplitBandFilter.TestProcessSampleSingle;
var
  Highpass : Single;
  Lowpass  : Single;
  Input    : Single;
begin
  // Initialize filter
  with FButterworthSplitBandFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 1;
   end;

  // Calculate filter
  Input := 1;
  FButterworthSplitBandFilter.ProcessSample32(Input, Lowpass, Highpass);

  // TODO: Validate method results
end;

procedure TestTButterworthSplitBandFilter.TestMagnitudeSquared;
var
  ReturnValue : Double;
begin
  // Initialize filter
  with FButterworthSplitBandFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 1;
   end;

  // Calculate filter
  with FButterworthSplitBandFilter
   do ReturnValue := MagnitudeSquared(Frequency);

  // Validate results
  CheckTrue(abs(0.5 - ReturnValue) < 1E-15);
end;

procedure RegisterTestSuite;
var
  TS : TTestSuite;
begin
 TS := TTestSuite.Create('Butterworth Filter Tests');
 TS.AddSuite(TestTButterworthLowPassFilter.Suite);
 TS.AddSuite(TestTButterworthHighpassFilter.Suite);
 TS.AddSuite(TestTButterworthSplitBandFilter.Suite);
 RegisterTest(TS);
end;

initialization
  RegisterTestSuite;

end.
