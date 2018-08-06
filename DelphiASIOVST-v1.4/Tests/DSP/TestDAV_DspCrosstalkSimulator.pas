unit TestDAV_DspCrosstalkSimulator;

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
  TestFramework, DAV_DspFilterBasics, DAV_DspFilter, DAV_DspCrosstalkSimulator,
  DAV_Common, DAV_DspCommon;
type
  // Test methods for class TIIRCrosstalkSimulator
  TestTIIRCrosstalkSimulator = class(TTestCase)
  strict private
    FIIRCrosstalkSimulator: TIIRCrosstalkSimulator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess32;
    procedure TestProcess64;
  end;

implementation

uses
  SysUtils;

{ TestTIIRCrosstalkSimulator }

procedure TestTIIRCrosstalkSimulator.SetUp;
begin
 FIIRCrosstalkSimulator := TIIRCrosstalkSimulator.Create;
end;

procedure TestTIIRCrosstalkSimulator.TearDown;
begin
 FreeAndNil(FIIRCrosstalkSimulator);
end;

procedure TestTIIRCrosstalkSimulator.TestProcess32;
var
  Right  : Single;
  Left   : Single;
  Sum    : array [0..1] of Single;
  Sample : Integer;
begin
 with FIIRCrosstalkSimulator do
  begin
   Left := 1;
   Right := 0;
   ProcessSample(Left, Right);
   Sum[0] := abs(Left);
   Sum[1] := abs(Right);
   for Sample := 1 to 1024 do
    begin
     Left := 0;
     Right := 0;
     ProcessSample(Left, Right);
     Sum[0] := Sum[0] + abs(Left);
     Sum[1] := Sum[1] + abs(Right);
    end;

   CheckTrue(Sum[0] > Sum[1]);
   CheckTrue(Sum[1] > 0);
  end;
end;

procedure TestTIIRCrosstalkSimulator.TestProcess64;
var
  Right  : Double;
  Left   : Double;
  Sum    : array [0..1] of Double;
  Sample : Integer;
begin
 with FIIRCrosstalkSimulator do
  begin
   Left := 1;
   Right := 0;
   ProcessSample(Left, Right);
   Sum[0] := abs(Left);
   Sum[1] := abs(Right);
   for Sample := 1 to 1024 do
    begin
     Left := 0;
     Right := 0;
     ProcessSample(Left, Right);
     Sum[0] := Sum[0] + abs(Left);
     Sum[1] := Sum[1] + abs(Right);
    end;

   CheckTrue(Sum[0] > Sum[1]);
   CheckTrue(Sum[1] > 0);
  end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTIIRCrosstalkSimulator.Suite);

end.
