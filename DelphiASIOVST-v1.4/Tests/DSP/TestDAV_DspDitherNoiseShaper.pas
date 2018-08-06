unit TestDAV_DspDitherNoiseShaper;

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
  TestFramework, DAV_DspDitherNoiseShaper, DAV_Common, DAV_DspCommon;

type
  // Test methods for class TDitherNoiseShaper
  TestTDitherNoiseShaper32 = class(TTestCase)
  strict private
    FDitherNoiseShaper: TDitherNoiseShaper32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessInteger;
    procedure TestProcessFloat;
    procedure TestReset;
  end;

implementation

uses
  Math, SysUtils;

procedure TestTDitherNoiseShaper32.SetUp;
begin
 FDitherNoiseShaper := TDitherNoiseShaper32.Create;
end;

procedure TestTDitherNoiseShaper32.TearDown;
begin
 FreeAndNil(FDitherNoiseShaper);
end;

procedure TestTDitherNoiseShaper32.TestProcessInteger;
var
  Input       : Double;
begin
 with FDitherNoiseShaper do
  begin
   BitDepth        := 8;
   Input           := 1 / (2 shl BitDepth - 1);
   DitherType      := dtNone;
   DitherAmplitude := 0;
   NoiseshaperType := nsNone;
   Limit           := False;

   // test zero input
   CheckTrue(ProcessInteger(0) = 0, 'No dither, no noiseshaper and no input (0) but still a result <> 0!');

   DitherType      := dtTriangular;
   DitherAmplitude := 1;
   NoiseshaperType := ns9Fc;

   // process impulse
   ProcessInteger(1 shl 15);

   // check any processing
   CheckTrue(ProcessInteger(0) <> 0, 'Neither dither or noiseshaper does work!');

   CheckTrue(ProcessInteger(Input) <> Input);
  end;
end;

procedure TestTDitherNoiseShaper32.TestProcessFloat;
var
  Input  : Double;
  Sample : Integer;
const
  CSampleFrames = 1000;
begin
 with FDitherNoiseShaper do
  begin
   BitDepth        := 8;
   Input           := 1 / (2 shl BitDepth - 1);
   DitherType      := dtNone;
   DitherAmplitude := 0;
   NoiseshaperType := nsNone;
   Limit           := False;

   // test zero input
   CheckTrue(abs(ProcessFloat(0)) <= 2 * Power(2, -BitDepth), 'No dither, no noiseshaper and no input (0) but still a result <> 0!');

   DitherType      := dtTriangular;
   DitherAmplitude := 1;
   NoiseshaperType := ns9Fc;

   // process impulse
   ProcessFloat(1);

   // check any processing
   CheckTrue(ProcessFloat(0) <> 0, 'Neither dither or noiseshaper does work!');

   // check limit stability
   Limit := True;

   // process very loud random noise (will clip for sure!)
   for Sample := 0 to CSampleFrames do ProcessFloat(100 * Random);

   CheckFalse(IsNan(ProcessFloat(100 * Random)), 'Return value is not a number');

   CheckTrue(ProcessInteger(Input) <> Input);
  end;
end;

procedure TestTDitherNoiseShaper32.TestReset;
var
  Sample      : Integer;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDitherNoiseShaper do
  begin
   BitDepth        := 31;
   DitherAmplitude := 0;
   DitherType      := dtNone;
   Limit           := False;
   NoiseshaperType := nsEFB;
  end;

 // Test chorus process series
 for Sample := 0 to CSampleFrames
  do FDitherNoiseShaper.ProcessFloat(Random);

 // reset quque
 FDitherNoiseShaper.Reset;

 // check whether process call of 0 results in anything else then 0
 CheckEquals(0, FDitherNoiseShaper.ProcessInteger(0), 'Reset was not successful!');
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTDitherNoiseShaper32.Suite);
  
end.
