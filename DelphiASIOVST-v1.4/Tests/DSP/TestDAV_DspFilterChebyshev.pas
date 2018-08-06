unit TestDAV_DspFilterChebyshev;

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
  TestFramework, DAV_Common, DAV_Complex, DAV_DspFilter,
  DAV_DspFilterChebyshev, DAV_DspFilterChebyshevType1;

type
  // Test methods for class TChebyshev1LowpassFilter
  TestTChebyshev1LowpassFilter = class(TTestCase)
  strict private
    FChebyshev1LowpassFilter: TChebyshev1LowpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestMagnitudeSquared;
  end;

  // Test methods for class TChebyshev1LowpassFilterAutomatable
  TestTChebyshev1LowpassFilterAutomatable = class(TTestCase)
  strict private
    FChebyshev1LowpassFilterAutomatable: TChebyshev1LowpassFilterAutomatable;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestMagnitudeSquared;
  end;

  // Test methods for class TChebyshev1HighpassFilter
  TestTChebyshev1HighpassFilter = class(TTestCase)
  strict private
    FChebyshev1HighpassFilter: TChebyshev1HighpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestMagnitudeSquared;
  end;

  // Test methods for class TChebyshev1HighpassFilterAutomatable
  TestTChebyshev1HighpassFilterAutomatable = class(TTestCase)
  strict private
    FChebyshev1HighpassFilterAutomatable: TChebyshev1HighpassFilterAutomatable;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestMagnitudeSquared;
  end;

implementation

uses
  SysUtils;

{ TestTChebyshev1LowpassFilter }

procedure TestTChebyshev1LowpassFilter.SetUp;
begin
 FChebyshev1LowpassFilter := TChebyshev1LowpassFilter.Create;
end;

procedure TestTChebyshev1LowpassFilter.TearDown;
begin
 FreeAndNil(FChebyshev1LowpassFilter);
end;

procedure TestTChebyshev1LowpassFilter.TestCalculateCoefficients;
begin
 FChebyshev1LowpassFilter.CalculateCoefficients;
end;

procedure TestTChebyshev1LowpassFilter.TestMagnitudeSquared;
var
  ReturnValue: Double;
begin
  // Initialize filter
  with FChebyshev1LowpassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0, 0.5);
    Order := 2;
   end;

  // Calculate Filter
  with FChebyshev1LowpassFilter
   do ReturnValue := MagnitudeSquared(Frequency);

  CheckTrue(abs(ReturnValue - 1) < 1E-15);
end;


{ TestTChebyshev1LowpassFilterAutomatable }

procedure TestTChebyshev1LowpassFilterAutomatable.SetUp;
begin
 FChebyshev1LowpassFilterAutomatable := TChebyshev1LowpassFilterAutomatable.Create;
end;

procedure TestTChebyshev1LowpassFilterAutomatable.TearDown;
begin
 FreeAndNil(FChebyshev1LowpassFilterAutomatable);
end;

procedure TestTChebyshev1LowpassFilterAutomatable.TestCalculateCoefficients;
begin
 FChebyshev1LowpassFilterAutomatable.CalculateCoefficients;
end;

procedure TestTChebyshev1LowpassFilterAutomatable.TestMagnitudeSquared;
var
  ReturnValue: Double;
begin
 // Initialize filter
 with FChebyshev1LowpassFilterAutomatable do
  begin
   SampleRate := 44100;
   SetFilterValues(1000, 0, 0.5);
   Order := 2;
  end;

  // Calculate Filter
  with FChebyshev1LowpassFilterAutomatable
   do ReturnValue := MagnitudeSquared(Frequency);

  CheckTrue(abs(ReturnValue - 1) < 1E-2);
end;


{ TestTChebyshev1HighpassFilter }

procedure TestTChebyshev1HighpassFilter.SetUp;
begin
 FChebyshev1HighpassFilter := TChebyshev1HighpassFilter.Create;
end;

procedure TestTChebyshev1HighpassFilter.TearDown;
begin
 FreeAndNil(FChebyshev1HighpassFilter);
end;

procedure TestTChebyshev1HighpassFilter.TestCalculateCoefficients;
begin
 FChebyshev1HighpassFilter.CalculateCoefficients;
end;

procedure TestTChebyshev1HighpassFilter.TestMagnitudeSquared;
var
  ReturnValue: Double;
begin
 // Initialize filter
 with FChebyshev1HighpassFilter do
  begin
   SampleRate := 44100;
   SetFilterValues(1000, 0, 0.5);
   Order := 2;
  end;

  // Calculate Filter
  with FChebyshev1HighpassFilter
   do ReturnValue := MagnitudeSquared(Frequency);

  CheckTrue(abs(ReturnValue - 1) < 1E-13);
end;


{ TestTChebyshev1HighpassFilterAutomatable }

procedure TestTChebyshev1HighpassFilterAutomatable.SetUp;
begin
 FChebyshev1HighpassFilterAutomatable := TChebyshev1HighpassFilterAutomatable.Create;
end;

procedure TestTChebyshev1HighpassFilterAutomatable.TearDown;
begin
 FreeAndNil(FChebyshev1HighpassFilterAutomatable);
end;

procedure TestTChebyshev1HighpassFilterAutomatable.TestCalculateCoefficients;
begin
 FChebyshev1HighpassFilterAutomatable.CalculateCoefficients;
end;

procedure TestTChebyshev1HighpassFilterAutomatable.TestMagnitudeSquared;
var
  ReturnValue: Double;
begin
 // Initialize filter
 with FChebyshev1HighpassFilterAutomatable do
  begin
   SampleRate := 44100;
   SetFilterValues(1000, 0, 0.5);
   Order := 2;
  end;

  // Calculate Filter
  with FChebyshev1HighpassFilterAutomatable
   do ReturnValue := MagnitudeSquared(Frequency);

  CheckTrue(abs(ReturnValue - 1) < 1E-2);
end;

procedure RegisterTestSuite;
var
  TS : TTestSuite;
begin
 TS := TTestSuite.Create('Chebyshev Filter Tests');
 TS.AddSuite(TestTChebyshev1LowpassFilter.Suite);
 TS.AddSuite(TestTChebyshev1LowpassFilterAutomatable.Suite);
 TS.AddSuite(TestTChebyshev1HighpassFilter.Suite);
 TS.AddSuite(TestTChebyshev1HighpassFilterAutomatable.Suite);
 RegisterTest(TS);
end;

initialization
  RegisterTestSuite;

end.
