unit TestDAV_DspSoundTouch;

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
  TestFramework, DAV_Types, DAV_Classes, DAV_DspSoundTouch, DAV_SoundTouchDLL;

type
  // Test methods for class TDspSoundTouch
  TestTDspSoundTouch = class(TTestCase)
  strict private
    FDspSoundTouch: TDspSoundTouch;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetVersionString;
    procedure TestGetVersionId;
    procedure TestFlush;
    procedure TestClear;
    procedure TestProcessSamples;
    procedure TestSetting;
  end;

implementation

uses
  SysUtils;

procedure TestTDspSoundTouch.SetUp;
begin
 FDspSoundTouch := TDspSoundTouch.Create;
end;

procedure TestTDspSoundTouch.TearDown;
begin
 FreeAndNil(FDspSoundTouch);
end;

procedure TestTDspSoundTouch.TestGetVersionString;
var
  ReturnValue: AnsiString;
begin
  ReturnValue := FDspSoundTouch.GetVersionString;
  CheckEquals(AnsiString('1.4.1'), ReturnValue);
end;

procedure TestTDspSoundTouch.TestGetVersionId;
var
  ReturnValue: Cardinal;
begin
  ReturnValue := FDspSoundTouch.GetVersionId;
  CheckEquals(10401, ReturnValue);
end;

procedure TestTDspSoundTouch.TestFlush;
begin
  FDspSoundTouch.Flush;
  // TODO: Validate method results
end;

procedure TestTDspSoundTouch.TestClear;
begin
  FDspSoundTouch.Clear;
  // TODO: Validate method results
end;

procedure TestTDspSoundTouch.TestProcessSamples;
var
  SampleFrames : Cardinal;
  Data         : PDAVSingleFixedArray;
  ReturnValue  : Cardinal;
begin
 SampleFrames := 1024;
 GetMem(Data, SampleFrames * SizeOf(Single));
 try
  FDspSoundTouch.WriteSamples(Data, SampleFrames);
  ReturnValue := FDspSoundTouch.ReadSamples(Data, SampleFrames);
  CheckEquals(ReturnValue, SampleFrames);
 finally
  Dispose(Data);
 end;
end;

procedure TestTDspSoundTouch.TestSetting;
var
  ReturnValue: Boolean;
  Value: Integer;
  SettingId: Integer;
begin
  ReturnValue := FDspSoundTouch.SetSetting(0, 0);
  CheckTrue(ReturnValue);
  Value := FDspSoundTouch.GetSetting(SettingId);
  CheckEquals(0, Value);
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTDspSoundTouch.Suite);
end.

