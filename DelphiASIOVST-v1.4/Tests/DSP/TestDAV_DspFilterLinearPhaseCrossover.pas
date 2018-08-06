unit TestDAV_DspFilterLinearPhaseCrossover;

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
  TestFramework, DAV_DspWindowing, Classes, DAV_DspCommon, DAV_Common,
  DAV_DspFilter, DAV_DspFilterLinearPhaseCrossover;

type
  // Test methods for class TLinearPhaseCrossover
  TestTLinearPhaseCrossover = class(TTestCase)
  strict private
    FLinearPhaseCrossover: TLinearPhaseCrossover;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSampleSingle;
    procedure TestProcessSampleDouble;
  end;

implementation

uses
  SysUtils;

procedure TestTLinearPhaseCrossover.SetUp;
begin
 FLinearPhaseCrossover := TLinearPhaseCrossover.Create;
end;

procedure TestTLinearPhaseCrossover.TearDown;
begin
 FreeAndNil(FLinearPhaseCrossover);
end;

procedure TestTLinearPhaseCrossover.TestProcessSampleSingle;
var
  High   : Single;
  Low    : Single;
  Sample : Integer;
begin
 with FLinearPhaseCrossover do
  begin
   SampleRate := 44100;
   Frequency  := 1000;
   FilterLength := 64;
   ProcessSample(1, Low, High);
   CheckTrue((Low = 0) and (High = 0), 'First sample output <> 0');

   for Sample := 0 to FilterLength div 2 - 1
    do ProcessSample(0, Low, High);

   CheckTrue(Low > 0, 'Low is negative');
   CheckTrue(High > 0, 'High is negative');
  end;
end;

procedure TestTLinearPhaseCrossover.TestProcessSampleDouble;
var
  High  : Double;
  Low   : Double;
  Sample : Integer;
begin
 with FLinearPhaseCrossover do
  begin
   SampleRate := 44100;
   Frequency  := 1000;
   FilterLength := 64;
   ProcessSample(1, Low, High);
   CheckTrue((Low = 0) and (High = 0), 'First sample output <> 0');

   for Sample := 0 to FilterLength div 2 - 1
    do ProcessSample(0, Low, High);

   CheckTrue(Low > 0, 'Low is negative');
   CheckTrue(High > 0, 'High is negative');
  end;
end;

initialization
  RegisterTest(TestTLinearPhaseCrossover.Suite);

end.
