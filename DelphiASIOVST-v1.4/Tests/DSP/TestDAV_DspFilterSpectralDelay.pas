unit TestDAV_DspFilterSpectralDelay;

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

{$I ..\DAV_Compiler.inc}

uses
  TestFramework, Classes, DAV_DspFilterSpectralDelay, DAV_DspFilter,
  DAV_Common, DAV_Classes;

type
  // Test methods for class TSpectralDelayFilter
  TestSpectralDelayFilter = class(TTestCase)
  strict private
    FSpectralDelayFilter: TSpectralDelayFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample32;
    procedure TestProcessSample64;
    procedure TestMagnitudeLog10;
    procedure TestMagnitudeSquared;
    procedure TestReset;
    procedure TestResetStates;
    procedure TestResetStatesInt64;
    procedure TestPushPopStates;
  end;

implementation

uses
  SysUtils;

procedure TestSpectralDelayFilter.SetUp;
begin
 FSpectralDelayFilter := TSpectralDelayFilter.Create;
end;

procedure TestSpectralDelayFilter.TearDown;
begin
 FreeAndNil(FSpectralDelayFilter);
end;

procedure TestSpectralDelayFilter.TestProcessSample32;
var
  ReturnValue : Single;
  Input       : Single;
begin
 // TODO: Setup method call parameters
 ReturnValue := FSpectralDelayFilter.ProcessSample32(Input);
 // TODO: Validate method results
end;

procedure TestSpectralDelayFilter.TestProcessSample64;
var
  ReturnValue: Double;
  Input: Double;
begin
 // TODO: Setup method call parameters
 ReturnValue := FSpectralDelayFilter.ProcessSample64(Input);
 // TODO: Validate method results
end;

procedure TestSpectralDelayFilter.TestMagnitudeLog10;
var
  ReturnValue: Double;
  Frequency: Double;
begin
 // TODO: Setup method call parameters
 ReturnValue := FSpectralDelayFilter.MagnitudeLog10(Frequency);
 // TODO: Validate method results
end;

procedure TestSpectralDelayFilter.TestMagnitudeSquared;
var
  ReturnValue: Double;
  Frequency: Double;
begin
 // TODO: Setup method call parameters
 ReturnValue := FSpectralDelayFilter.MagnitudeSquared(Frequency);
 // TODO: Validate method results
end;

procedure TestSpectralDelayFilter.TestReset;
begin
 FSpectralDelayFilter.Reset;
 // TODO: Validate method results
end;

procedure TestSpectralDelayFilter.TestResetStates;
begin
 FSpectralDelayFilter.ResetStates;
 // TODO: Validate method results
end;

procedure TestSpectralDelayFilter.TestResetStatesInt64;
begin
 FSpectralDelayFilter.ResetStatesInt64;
 // TODO: Validate method results
end;

procedure TestSpectralDelayFilter.TestPushPopStates;
begin
 FSpectralDelayFilter.PushStates;
 FSpectralDelayFilter.PopStates;
 // TODO: Validate method results
end;

initialization
 // Alle Testfälle beim Test-Runner registrieren
 RegisterTest(TestSpectralDelayFilter.Suite);

end.
