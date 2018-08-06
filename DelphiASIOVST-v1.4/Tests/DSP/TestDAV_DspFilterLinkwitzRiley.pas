unit TestDAV_DspFilterLinkwitzRiley;

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
  TestFramework, Classes, DAV_DspFilterButterworth, DAV_DspFilterLinkwitzRiley,
  DAV_Common, DAV_DspCommon, DAV_DspFilter;

type
  // Test methods for class TLinkwitzRiley
  TestTLinkwitzRiley = class(TTestCase)
  strict private
    FLinkwitzRiley: TLinkwitzRiley;
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

procedure TestTLinkwitzRiley.SetUp;
begin
 FLinkwitzRiley := TLinkwitzRiley.Create;
end;

procedure TestTLinkwitzRiley.TearDown;
begin
 FreeAndNil(FLinkwitzRiley);
end;

procedure TestTLinkwitzRiley.TestProcessSampleSingle;
var
  High  : Single;
  Low   : Single;
begin
 with FLinkwitzRiley do
  begin
   SampleRate := 44100;
   Frequency  := 1000;
   ProcessSample32(1, Low, High);
   CheckTrue(Low > 0);
   CheckTrue(High > 0);
  end;
end;

procedure TestTLinkwitzRiley.TestProcessSampleDouble;
var
  High  : Double;
  Low   : Double;
begin
 with FLinkwitzRiley do
  begin
   SampleRate := 44100;
   Frequency  := 1000;
   ProcessSample64(1, Low, High);
   CheckTrue(Low > 0);
   CheckTrue(High > 0);
  end;
end;

initialization
  RegisterTest(TestTLinkwitzRiley.Suite);
  
end.

