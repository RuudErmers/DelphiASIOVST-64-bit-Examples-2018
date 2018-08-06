unit PluginVST;
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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  DAV_Types, DAV_VSTModule, DAV_DspSimpleOscillator;

type
  TProcessingTestModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess32Replacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64Replacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterFreqDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFreqLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFreq32Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFreq64Change(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FOsc32 : TSimpleOscillator32;
    FOsc64 : TSimpleOscillator64;
  public
  end;

implementation

uses
  {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} Math;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TProcessingTestModule.VSTModuleOpen(Sender: TObject);
begin
  FOsc32 := TSimpleOscillator32.Create;
  FOsc32.SampleRate := SampleRate;
  FOsc64 := TSimpleOscillator64.Create;
  FOsc64.SampleRate := SampleRate;

  // Initialize Parameters
  Parameter[0] := 320;
  Parameter[1] := 6400;
end;

procedure TProcessingTestModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FOsc32);
 FreeAndNil(FOsc64);
end;

procedure TProcessingTestModule.ParameterFreqDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Value : Single;
begin
 Value := Parameter[Index];
 if Value < 1000
  then PreDefined := AnsiString(FloatToStrF(RoundTo(Value, -3), ffGeneral, 3, 3))
  else PreDefined := AnsiString(FloatToStrF(RoundTo(1E-3 * Value, -3), ffGeneral, 3, 3));
end;

procedure TProcessingTestModule.ParameterFreqLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 1000
  then PreDefined := 'Hz'
  else PreDefined := 'kHz'
end;

procedure TProcessingTestModule.ParameterFreq64Change(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FOsc64)
  then FOsc64.Frequency := Value;
end;

procedure TProcessingTestModule.ParameterFreq32Change(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FOsc32)
  then FOsc32.Frequency := Value;
end;

procedure TProcessingTestModule.VSTModuleProcess32Replacing(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
begin
 if Assigned(FOsc32) then
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    FOsc32.CalculateNextSample;
    Outputs[0, SampleIndex] := FOsc32.Sine;
   end;
end;

procedure TProcessingTestModule.VSTModuleProcess64Replacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
begin
 if Assigned(FOsc64) then
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    FOsc64.CalculateNextSample;
    Outputs[0, SampleIndex] := FOsc64.Sine;
   end;
end;

procedure TProcessingTestModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) > 0 then
  begin
   if Assigned(FOsc32)
    then FOsc32.SampleRate := SampleRate;
   if Assigned(FOsc64)
    then FOsc64.SampleRate := SampleRate;
  end;
end;

end.
