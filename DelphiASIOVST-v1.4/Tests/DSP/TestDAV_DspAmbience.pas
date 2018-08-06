unit TestDAV_DspAmbience;

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
  TestFramework, DAV_Common, DAV_DspFilterLinkwitzRiley, DAV_DspFilterButterworth,
  DAV_DspFilter, DAV_DspAmbience;

type
  // Test methods for class TAmbience
  TestTAmbience = class(TTestCase)
  strict private
    FAmbience: TAmbience;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
    procedure TestProcess32;
  end;

implementation

uses
  Math, SysUtils;

procedure TestTAmbience.SetUp;
begin
 FAmbience := TAmbience.Create;
end;

procedure TestTAmbience.TearDown;
begin
 FreeAndNil(FAmbience);
end;

procedure TestTAmbience.TestProcess;
var
  ReturnValue : Single;
  Sample      : Integer;
const
  CSampleFrames = 1000;
begin
 FAmbience.ProcessSample32(1);

 for Sample := 0 to CSampleFrames - 1
  do ReturnValue := FAmbience.ProcessSample32(0);

 CheckFalse(IsNan(ReturnValue), 'Return value is not a number');
end;

procedure TestTAmbience.TestProcess32;
var
  Right  : Single;
  Left   : Single;
  Sample : Integer;
const
  CSampleFrames = 1000;  
begin
 Left := 1;
 Right := 1;
 FAmbience.ProcessSample(Left, Right);

 Left := 0;
 Right := 0;
 for Sample := 0 to CSampleFrames - 1
  do FAmbience.ProcessSample(Left, Right);

 CheckFalse(IsNan(Left), 'Left channel result is not a number');
 CheckFalse(IsNan(Right), 'Right channel result is not a number');
end;

initialization
  RegisterTest(TestTAmbience.Suite);

end.
