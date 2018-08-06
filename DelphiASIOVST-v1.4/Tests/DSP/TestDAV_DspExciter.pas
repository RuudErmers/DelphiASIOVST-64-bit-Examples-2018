unit TestDAV_DspExciter;

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
  TestFramework, DAV_DspPolyphaseUpsampler, DAV_DspExciter,
  DAV_DspFilterLinkwitzRiley, DAV_Common, Classes, DAV_DspFilterButterworth, 
  DAV_DspCommon, DAV_DspDynamics, DAV_DspPolyphaseDownsampler, 
  DAV_DspLightweightDynamics, DAV_DspFilter;

type
  // Test methods for class TestTExciter
  TestTExciter = class(TTestCase)
  strict private
    FExciter: TExciter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample32;
    procedure TestProcessSample64;
  end;

implementation

uses
  SysUtils;

procedure TestTExciter.SetUp;
begin
  FExciter := TExciter.Create;
end;

procedure TestTExciter.TearDown;
begin
 FreeAndNil(FExciter);
end;

procedure TestTExciter.TestProcessSample32;
begin
 FExciter.ProcessSample32(1);
end;

procedure TestTExciter.TestProcessSample64;
begin
 FExciter.ProcessSample64(1);
end;

initialization
  RegisterTest(TestTExciter.Suite);
  
end.
