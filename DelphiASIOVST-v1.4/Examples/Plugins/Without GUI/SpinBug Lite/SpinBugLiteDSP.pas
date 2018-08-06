unit SpinBugLiteDSP;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Forms, SyncObjs, DAV_Types, DAV_DspPolyphaseHilbert,
  DAV_VSTModule, DAV_DspLFO;

type
  TSpinBugLiteModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender : TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessStereoA(const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessStereoB(const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessStereoC(const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessStereoD(const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMono(const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames : Cardinal);
    procedure VSTModuleProcessMonoL(const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMonoR(const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMS(const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessSpecial(const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessOldOne(const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender : TObject; const SampleRate : Single);
    procedure SBMCoefficientsDisplay(Sender : TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure SBMCoefficientsChange(Sender : TObject; const Index: Integer; var Value: Single);
    procedure SBMProcessTypeDisplay(Sender : TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure SBMProcessTypeChange(Sender : TObject; const Index: Integer; var Value: Single);
    procedure SBMLFOSpeedChange(Sender : TObject; const Index: Integer; var Value: Single);
    procedure SBMTBWChange(Sender : TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection : TCriticalSection;
    FHilbert         : array [0..1] of TPhaseHalfPi32;
    FSineLFO         : array [0..1] of TLFOSine;
    FTBW             : Single;
  public
    property BasicSineLFO: TLFOSine read FSineLFO[0];
    property AdditionalSineLFO: TLFOSine read FSineLFO[1];
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Approximations;

procedure TSpinBugLiteModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSpinBugLiteModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSpinBugLiteModule.VSTModuleOpen(Sender : TObject);
begin
 FTBW := 0.01;

 FHilbert[0] := TPhaseHalfPi32.Create;
 FHilbert[1] := TPhaseHalfPi32.Create;
 FSineLFO[0] := TLFOSine.Create;
 FSineLFO[1] := TLFOSine.Create;

 Parameter[0] := 8;
 OnProcess := VSTModuleProcessMono;
 OnProcess32Replacing := VSTModuleProcessMono;

 if Assigned(Programs) then
  try
   // default preset
   with programs[0] do
    begin
     Parameter[0] := 8;
     Parameter[1] := 5;
     Parameter[2] := 0.5;
     Parameter[3] := 0.01;
    end;

   // preset 1
   with programs[1] do
    begin
     Parameter[0] := 32;
     Parameter[1] := 3;
     Parameter[2] := 0.5;
     Parameter[3] := 0.01;
    end;

   // preset 2
   with programs[2] do
    begin
     Parameter[0] := 7;
     Parameter[1] := 2;
     Parameter[2] := 1.3;
     Parameter[3] := 0.01;
    end;

   // preset 3
   with programs[3] do
    begin
     Parameter[0] := 12;
     Parameter[1] := 4;
     Parameter[2] := 0.8;
     Parameter[3] := 0.01;
    end;

   // preset 4
   with programs[4] do
    begin
     Parameter[0] := 16;
     Parameter[1] := 9;
     Parameter[2] := 1;
     Parameter[3] := 0.01;
    end;

   // preset 5
   with programs[5] do
    begin
     Parameter[0] := 8;
     Parameter[1] := 10;
     Parameter[2] := 1.7;
     Parameter[3] := 0.01;
    end;
  except
  end;

 // default parameters
 Parameter[0] := 8;
 Parameter[1] := 5;
 Parameter[2] := 0.6;
 Parameter[3] := 0.01;
end;

procedure TSpinBugLiteModule.VSTModuleClose(Sender: TObject);
begin
 if Assigned(FHilbert[0]) then FreeAndNil(FHilbert[0]);
 if Assigned(FHilbert[1]) then FreeAndNil(FHilbert[1]);
 if Assigned(FSineLFO[0]) then FreeAndNil(FSineLFO[0]);
 if Assigned(FSineLFO[1]) then FreeAndNil(FSineLFO[1]);
end;

function GetProcessTypeCaption(ParameterIndex : Integer): AnsiString;
begin
 case ParameterIndex of
  0 : Result := 'illegal';
  1 : Result := 'stereo a';
  2 : Result := 'stereo b';
  3 : Result := 'stereo c';
  4 : Result := 'stereo d';
  5 : Result := 'mono';
  6 : Result := 'mono l';
  7 : Result := 'mono r';
  8 : Result := 'm+s';
  9 : Result := 'special';
  10 : Result := 'old one';
 end;
end;

function GetColourCaption(ParameterIndex : Integer): AnsiString;
begin
  case ParameterIndex of
    1 : Result := 'rough';
    4 : Result := 'firm';
    8 : Result := 'medium';
   16 : Result := 'soft';
   32 : Result := 'smooth';
   else Result := 'untitled';
  end;
end;

procedure TSpinBugLiteModule.SBMCoefficientsDisplay(Sender : TObject;
  const Index : Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[0])));
end;

procedure TSpinBugLiteModule.SBMCoefficientsChange(Sender : TObject;
  const Index : Integer; var Value : Single);
var
  ival : Integer;
begin
 ival := Round(Value);
 if (ival < 1) or (ival > 32) then Exit;
 if (ival = 17) or (ival = 18) or (ival = 19) or (ival = 20) or (ival = 21) or
    (ival = 22) or (ival = 23) or (ival = 25) or (ival = 26) or (ival = 27) or
    (ival = 28) or (ival = 29) or (ival = 30) or (ival = 31) then Exit;

 FCriticalSection.Enter;
 try
  if Assigned(FHilbert[0]) then FHilbert[0].NumberOfCoefficients := ival;
  if Assigned(FHilbert[1]) then FHilbert[1].NumberOfCoefficients := ival;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpinBugLiteModule.SBMProcessTypeDisplay(Sender : TObject;
  const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := GetProcessTypeCaption(Round(Parameter[1]));
end;

procedure TSpinBugLiteModule.SBMProcessTypeChange(Sender : TObject;
  const Index : Integer; var Value : Single);
begin
 Value := Round(Value);
 if (Value < 1)
  then Value := 10 else
 if (Value > 10) then Value := 1;

 FCriticalSection.Enter;
 try
  case Round(Value) of
     1 : OnProcess := VSTModuleProcessStereoA;
     2 : OnProcess := VSTModuleProcessStereoB;
     3 : OnProcess := VSTModuleProcessStereoC;
     4 : OnProcess := VSTModuleProcessStereoD;
     5 : OnProcess := VSTModuleProcessMono;
     6 : OnProcess := VSTModuleProcessMonoL;
     7 : OnProcess := VSTModuleProcessMonoR;
     8 : OnProcess := VSTModuleProcessMS;
     9 : OnProcess := VSTModuleProcessSpecial;
    10 : OnProcess := VSTModuleProcessOldOne;
   end;

  OnProcess32Replacing := OnProcess;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpinBugLiteModule.SBMLFOSpeedChange(Sender : TObject;
  const Index : Integer; var Value : Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FSineLFO[0]) then FSineLFO[0].Frequency := Value;
  if Assigned(FSineLFO[1]) then FSineLFO[1].Frequency := 1.01 * Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpinBugLiteModule.SBMTBWChange(Sender : TObject;
  const Index : Integer; var Value : Single);
begin
 if (Value <= 0) or (Value >= 0.5) then Exit;

 if Value <> FTBW then
  begin
   FTBW := Value;
   FCriticalSection.Enter;
   try
    if Assigned(FHilbert[0]) then FHilbert[0].Transition := FTBW;
    if Assigned(FHilbert[1]) then FHilbert[1].Transition := FTBW;
   finally
    FCriticalSection.Leave;
   end;
  end;
end;

procedure TSpinBugLiteModule.VSTModuleSampleRateChange(Sender : TObject;
  const SampleRate : Single);
begin
 if Abs(SampleRate) > 0 then
  begin
   FCriticalSection.Enter;
   try
    if Assigned(FSineLFO[0]) then FSineLFO[0].SampleRate := Abs(SampleRate);
    if Assigned(FSineLFO[1]) then FSineLFO[1].SampleRate := Abs(SampleRate);
   finally
    FCriticalSection.Leave;
   end;
  end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessStereoA(
  const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i      : Integer;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   begin
    FHilbert[0].ProcessHilbertSample(Inputs[0, i], a1, b1);
    FHilbert[1].ProcessHilbertSample(Inputs[1, i], a2, b2);
    a1 := a1 * BasicSineLFO.Cosine;
    b1 := b1 * BasicSineLFO.Sine;
    a2 := a2 * BasicSineLFO.Cosine;
    b2 := b2 * BasicSineLFO.Sine;
    BasicSineLFO.CalculateNextSample;
    Outputs[0, i] := a1 + b1 + Inputs[0, i];
    Outputs[1, i] := a1 - b1 + Inputs[0, i];
    Outputs[0, i] := 0.25 * (Outputs[0, i] + a2 - b2 + Inputs[1, i]);
    Outputs[1, i] := 0.25 * (Outputs[1, i] + a2 + b2 + Inputs[1, i]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessStereoB(
  const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i      : Integer;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   begin
    FHilbert[0].ProcessHilbertSample(Inputs[0, i], a1, b1);
    FHilbert[1].ProcessHilbertSample(Inputs[1, i], a2, b2);
    a1 := a1 * BasicSineLFO.Cosine;
    b1 := b1 * BasicSineLFO.Sine;
    a2 := a2 * BasicSineLFO.Cosine;
    b2 := b2 * BasicSineLFO.Sine;
    BasicSineLFO.CalculateNextSample;
    Outputs[0, i] := a1 + b1 + Inputs[0, i];
    Outputs[1, i] := a1 - b1 + Inputs[0, i];
    Outputs[0, i] := 0.25 * (Outputs[0, i] + a2 + b2 + Inputs[1, i]);
    Outputs[1, i] := 0.25 * (Outputs[1, i] + a2 - b2 + Inputs[1, i]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessStereoC(
  const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i      : Integer;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   begin
    FHilbert[0].ProcessHilbertSample(Inputs[0, i], a1, b1);
    FHilbert[1].ProcessHilbertSample(Inputs[1, i], a2, b2);
    a1 := a1 * BasicSineLFO.Cosine;
    b1 := b1 * BasicSineLFO.Sine;
    a2 := a2 * BasicSineLFO.Cosine;
    b2 := b2 * BasicSineLFO.Sine;
    BasicSineLFO.CalculateNextSample;
    Outputs[0, i] := 0.5 * (a1 + b1 + Inputs[0, i]);
    Outputs[1, i] := 0.5 * (a2 - b2 + Inputs[1, i]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessStereoD(
  const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i      : Integer;
  s, c   : Double;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   begin
    s := BasicSineLFO.Sine;
    c := 1 - 2 * s * s;
    BasicSineLFO.CalculateNextSample;
    FHilbert[0].ProcessHilbertSample(Inputs[0, i], a1, b1);
    FHilbert[1].ProcessHilbertSample(Inputs[1, i], a2, b2);
    a1 := a1 * c;
    b1 := b1 * s;
    a2 := a2 * c;
    b2 := b2 * s;
    Outputs[0, i] := a1 + b1 + Inputs[0, i];
    Outputs[1, i] := a1 - b1 + Inputs[0, i];
    Outputs[0, i] := 0.25 * (Outputs[0, i] + a2 + b2 + Inputs[1, i]);
    Outputs[1, i] := 0.25 * (Outputs[1, i] + a2 - b2 + Inputs[1, i]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessMono(
  const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i    : Integer;
  a, b : Single;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   begin
    FHilbert[0].ProcessHilbertSample(0.5 * (Inputs[0, i] + Inputs[1, i]), a, b);
    a := a * BasicSineLFO.Sine;
    b := b * BasicSineLFO.Cosine;
    BasicSineLFO.CalculateNextSample;
    Outputs[0, i] := 0.5 * (a + b + Inputs[0, i]);
    Outputs[1, i] := 0.5 * (a - b + Inputs[1, i]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessMonoL(
  const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i    : Integer;
  a, b : Single;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   begin
    FHilbert[0].ProcessHilbertSample(Inputs[0, i], a, b);
    a := a * BasicSineLFO.Sine;
    b := b * BasicSineLFO.Cosine;
    BasicSineLFO.CalculateNextSample;
    Outputs[0, i] := 0.5 * (a + b + Inputs[0, i]);
    Outputs[1, i] := 0.5 * (a - b + Inputs[0, i]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessMonoR(
  const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i    : Integer;
  a, b : Single;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   begin
    FHilbert[0].ProcessHilbertSample(Inputs[1, i], a, b);
    a := a * BasicSineLFO.Sine;
    b := b * BasicSineLFO.Cosine;
    BasicSineLFO.CalculateNextSample;
    Outputs[0, i] := 0.5 * (a + b + Inputs[0, i]);
    Outputs[1, i] := 0.5 * (a - b + Inputs[0, i]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessMS(
  const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i      : Integer;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   begin
    FHilbert[0].ProcessHilbertSample(0.5 * (Inputs[0, i] + Inputs[1, i]), a1, b1);
    FHilbert[1].ProcessHilbertSample(0.5 * (Inputs[0, i] - Inputs[1, i]), a2, b2);
    a1 := a1 * BasicSineLFO.Sine;
    b1 := b1 * BasicSineLFO.Cosine;
    a2 := a2 * BasicSineLFO.Sine;
    b2 := b2 * BasicSineLFO.Cosine;
    BasicSineLFO.CalculateNextSample;
    Outputs[0, i] := a1 + b1 + Inputs[0, i];
    Outputs[1, i] := a1 - b1 + Inputs[0, i];
    Outputs[0, i] := 0.25 * (Outputs[0, i] + a2 + b2 + Inputs[1, i]);
    Outputs[1, i] := 0.25 * (Outputs[1, i] + a2 - b2 + Inputs[1, i]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessSpecial(
  const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i      : Integer;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   begin
    FHilbert[0].ProcessHilbertSample(Inputs[0, i], a1, b1);
    FHilbert[0].ProcessHilbertSample(-Inputs[1, i], a2, b2);
    a1 := a1 * BasicSineLFO.Sine;
    b1 := b1 * BasicSineLFO.Cosine;
    a2 := a2 * AdditionalSineLFO.Sine;
    b2 := b2 * AdditionalSineLFO.Cosine;
    BasicSineLFO.CalculateNextSample;
    AdditionalSineLFO.CalculateNextSample;
    Outputs[0, i] := a1 + b1 + Inputs[0, i];
    Outputs[1, i] := a1 - b1 + Inputs[0, i];
    Outputs[0, i] := 0.25 * (Outputs[0, i] + a2 - b2 + Inputs[1, i]);
    Outputs[1, i] := 0.25 * (Outputs[1, i] + a2 + b2 + Inputs[1, i]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessOldOne(
  const Inputs, Outputs : TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i      : Integer;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   begin
    FHilbert[0].ProcessHilbertSample(FastTanh2Like3Term(2.1 * Inputs[0, i]), a1, b1);
    FHilbert[1].ProcessHilbertSample(FastTanh2Like3Term(2.2 * Inputs[1, i]), a2, b2);
    a1 := a1 * BasicSineLFO.Sine;
    b1 := b1 * BasicSineLFO.Cosine;
    a2 := a2 * BasicSineLFO.Sine;
    b2 := b2 * BasicSineLFO.Cosine;
    BasicSineLFO.CalculateNextSample;
    Outputs[0, i] := a1 + b1 + Inputs[0, i];
    Outputs[1, i] := a1 - b1 + Inputs[1, i];
    Outputs[0, i] := 0.25 * (Outputs[0, i] + a2 + b2 + Inputs[1, i]);
    Outputs[1, i] := 0.25 * (Outputs[1, i] + a2 - b2 + Inputs[0, i]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
