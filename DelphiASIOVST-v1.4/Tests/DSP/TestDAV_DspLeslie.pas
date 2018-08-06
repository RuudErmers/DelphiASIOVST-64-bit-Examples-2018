unit TestDAV_DspLeslie;

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
  TestFramework, DAV_Common, DAV_DspCommon, DAV_DspLeslie;

type
  // Test methods for class TLeslieRotator
  TestTLeslieRotator = class(TTestCase)
  strict private
    FLeslieRotator: TLeslieRotator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

implementation

uses
  SysUtils;

procedure TestTLeslieRotator.SetUp;
begin
  FLeslieRotator := TLeslieRotator.Create;
end;

procedure TestTLeslieRotator.TearDown;
begin
 FreeAndNil(FLeslieRotator);
end;

procedure TestTLeslieRotator.TestProcessSample;
var
  Right: Single;
  Left: Single;
begin
 with FLeslieRotator do
  begin
   ProcessSample(0, Left, Right);
   CheckTrue(Left = 0);
   CheckTrue(Right  = 0);

   ProcessSample(1, Left, Right);
   CheckTrue(Left < 1);
   CheckTrue(Right < 1);
  end;
end;

initialization
  RegisterTest(TestTLeslieRotator.Suite);

end.
