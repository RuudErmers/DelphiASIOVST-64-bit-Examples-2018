unit TestDAV_DspBesselFilter;

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
  TestFramework, DAV_DspBesselFilter, DAV_Common, DAV_DspFilter;

type
  // Test methods for class TBesselLowpassFilter
  TestTBesselLowpassFilter = class(TTestCase)
  strict private
    FBesselLowpassFilter: TBesselLowpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestProcessSample;
    procedure TestMagnitudeSquared;
    procedure TestPhase;
  end;

  // Test methods for class TBesselHighpassFilter
  TestTBesselHighpassFilter = class(TTestCase)
  strict private
    FBesselHighpassFilter: TBesselHighpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestProcessSample;
    procedure TestMagnitudeSquared;
  end;

implementation

uses
  SysUtils;

procedure TestTBesselLowpassFilter.SetUp;
begin
 FBesselLowpassFilter := TBesselLowpassFilter.Create;
end;

procedure TestTBesselLowpassFilter.TearDown;
begin
 FreeAndNil(FBesselLowpassFilter);
end;

procedure TestTBesselLowpassFilter.TestCalculateCoefficients;
begin
 FBesselLowpassFilter.CalculateCoefficients;
end;

procedure TestTBesselLowpassFilter.TestProcessSample;
var
  ReturnValue : Double;
  Input       : Double;
const
  CSampleFrames = 1000;
begin
  // Initialize filter
  with FBesselLowpassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 2;
   end;

  // Calculate filter
  Input := 1;
  ReturnValue := FBesselLowpassFilter.ProcessSample64(Input);

  // Validate results
  CheckTrue((ReturnValue > -1) and (ReturnValue < 1));
end;

procedure TestTBesselLowpassFilter.TestMagnitudeSquared;
var
  ReturnValue : Double;
begin
 // Initialize filter
 with FBesselLowpassFilter do
  begin
   SampleRate := 44100;
   SetFilterValues(1000, 0);
   Order := 2;
  end;

 // Calculate Filter
 with FBesselLowpassFilter
  do ReturnValue := MagnitudeSquared(Frequency);

 // Validate result
 CheckTrue(abs(ReturnValue - 1) < 1E-10);
end;

procedure TestTBesselLowpassFilter.TestPhase;
var
  ReturnValue : Double;
begin
  // Initialize filter
  with FBesselLowpassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 2;
   end;

  with FBesselLowpassFilter
   do ReturnValue := Phase(Frequency);

 // Validate result
 CheckTrue(abs(ReturnValue - 0) < 1E-10);
end;

procedure TestTBesselHighpassFilter.SetUp;
begin
 FBesselHighpassFilter := TBesselHighpassFilter.Create;
end;

procedure TestTBesselHighpassFilter.TearDown;
begin
 FreeAndNil(FBesselHighpassFilter);
end;

procedure TestTBesselHighpassFilter.TestCalculateCoefficients;
begin
 FBesselHighpassFilter.CalculateCoefficients;
end;

procedure TestTBesselHighpassFilter.TestProcessSample;
var
  ReturnValue : Double;
  Input       : Double;
begin
  // Initialize filter
  with FBesselHighpassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 2;
   end;
  Input := 1;

  // Calculate Filter
  with FBesselHighpassFilter
   do ReturnValue := ProcessSample64(Input);

 // Validate result
 CheckTrue(abs(ReturnValue - 0) < 1E-10);
end;

procedure TestTBesselHighpassFilter.TestMagnitudeSquared;
var
  ReturnValue : Double;
begin
  // Initialize filter
  with FBesselHighpassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 2;
   end;

  // Calculate Filter
  with FBesselHighpassFilter
   do ReturnValue := MagnitudeSquared(Frequency);

 // Validate result
 CheckTrue(abs(ReturnValue - 1) < 1E-10);
end;

procedure RegisterTestSuite;
var
  TS : TTestSuite;
begin
 TS := TTestSuite.Create('Bessel Filters');
 TS.AddSuite(TestTBesselLowpassFilter.Suite);
 TS.AddSuite(TestTBesselHighpassFilter.Suite);
 RegisterTest(TS);
end;

initialization
  RegisterTestSuite;

end.
