unit TestDAV_DspAudioToMidiTrigger;

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
  TestFramework, Classes, DAV_Common, DAV_DspCommon, DAV_DspAudioToMidiTrigger,
  DAV_DspFilter, DAV_DspFilterBasics;

type
  // Test methods for class TAudio2MidiTrigger
  TestTAudio2MidiTrigger = class(TTestCase)
  strict private
    FCustomAudio2MidiTrigger: TCustomAudio2MidiTrigger;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample32;
    procedure TestProcessSample64;
    procedure TestFilters;
  end;

implementation

uses
  SysUtils;

procedure TestTAudio2MidiTrigger.SetUp;
begin
 FCustomAudio2MidiTrigger := TCustomAudio2MidiTrigger.Create;
end;

procedure TestTAudio2MidiTrigger.TearDown;
begin
 FreeAndNil(FCustomAudio2MidiTrigger);
end;

procedure TestTAudio2MidiTrigger.TestProcessSample32;
var
  ReturnValue: Single;
begin
 with FCustomAudio2MidiTrigger do
  begin
   Flags := [amFilterOutput];
   ReturnValue := ProcessSample32(1);
   CheckEquals(ReturnValue, 1);
  end;
end;

procedure TestTAudio2MidiTrigger.TestProcessSample64;
var
  ReturnValue: Double;
begin
 with FCustomAudio2MidiTrigger do
  begin
   Flags := [amFilterOutput];
   ReturnValue := ProcessSample32(1);
   CheckEquals(ReturnValue, 1);
  end;
end;

procedure TestTAudio2MidiTrigger.TestFilters;
var
  GainFilter: TCustomFilter;
begin
 GainFilter := TBasicGainFilter.Create;
 try
  with FCustomAudio2MidiTrigger do
   begin
    AddFilter(GainFilter);
    DeleteFilter(GainFilter);
    AddFilter(GainFilter);
    DeleteFilter(0);
   end;
 finally
  FreeAndNil(GainFilter);
 end;
end;

initialization
  RegisterTest(TestTAudio2MidiTrigger.Suite);

end.
