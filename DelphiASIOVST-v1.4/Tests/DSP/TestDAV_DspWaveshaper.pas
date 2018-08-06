unit TestDAV_DspWaveshaper;

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
  TestFramework, DAV_DspCommon, DAV_Common, Classes, DAV_DspWaveshaper;

type
  TestTChebyshevWaveshaper = class(TTestCase)
  strict private
    FChebyshevWaveshaper: TChebyshevWaveshaper;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample64;
  end;

implementation

uses
  SysUtils;

procedure TestTChebyshevWaveshaper.SetUp;
begin
 FChebyshevWaveshaper := TChebyshevWaveshaper.Create;
end;

procedure TestTChebyshevWaveshaper.TearDown;
begin
 FreeAndNil(FChebyshevWaveshaper);
end;

procedure TestTChebyshevWaveshaper.TestProcessSample64;
begin
 with FChebyshevWaveshaper do
  begin
   Order := 2;
   CheckEquals(0, ProcessSample64(0), 'ProcessSample64(0) <> 0!');

   Order := 1;
   Gain[0] := 0;
   CheckEquals(0, ProcessSample64(1), 'No coefficients, but output detected');
  end;
end;

initialization
  RegisterTest(TestTChebyshevWaveshaper.Suite);

end.
