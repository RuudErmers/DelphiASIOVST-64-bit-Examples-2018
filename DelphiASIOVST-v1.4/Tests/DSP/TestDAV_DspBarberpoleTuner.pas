unit TestDAV_DspBarberpoleTuner;

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
  TestFramework, DAV_Common, DAV_DspFilterButterworth, DAV_DspTuner,
  DAV_DspCommon, DAV_DspLfo, DAV_DspBarberpoleTuner;

type
  // Test methods for class TBarberpoleFilter
  TestTBarberpoleFilter = class(TTestCase)
  strict private
    FBarberpoleFilter: TBarberpoleFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
  end;

  // Test methods for class TBarberpoleTuner
  TestTBarberpoleTuner = class(TTestCase)
  strict private
    FBarberpoleTuner: TBarberpoleTuner;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
  end;

implementation

uses
  Math, SysUtils; 

{ TestTBarberpoleFilter }

procedure TestTBarberpoleFilter.SetUp;
begin
 FBarberpoleFilter := TBarberpoleFilter.Create;
end;

procedure TestTBarberpoleFilter.TearDown;
begin
 FreeAndNil(FBarberpoleFilter);
end;


procedure TestTBarberpoleFilter.TestProcess;
var
  Sample : Integer;
begin
 CheckTrue(abs(FBarberpoleFilter.ProcessSample32(1)) <= 1, 'Filter seems to be a bit unstable');
 for Sample := 0 to 1000
  do CheckTrue(abs(FBarberpoleFilter.ProcessSample32(1)) <= 1, 'Filter seems to be a bit unstable');
end;

{ TestTBarberpoleTuner }

procedure TestTBarberpoleTuner.SetUp;
begin
 FBarberpoleTuner := TBarberpoleTuner.Create;
end;

procedure TestTBarberpoleTuner.TearDown;
begin
 FreeAndNil(FBarberpoleTuner);
end;

procedure TestTBarberpoleTuner.TestProcess;
begin
 FBarberpoleTuner.ProcessSample32(Random);
end;

procedure RegisterTestSuite;
var
  TS : TTestSuite;
begin
 TS := TTestSuite.Create('Barberpole Tuners');
 TS.AddSuite(TestTBarberpoleFilter.Suite);
 TS.AddSuite(TestTBarberpoleTuner.Suite);
 RegisterTest(TS);
end;

initialization
  RegisterTestSuite;

end.
