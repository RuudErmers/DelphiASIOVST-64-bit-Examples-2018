unit SplitterDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, DAV_Types, DAV_VSTModule;

type
  TSplitterDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray;
      const SampleFrames: Cardinal);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure ParameterEnvelopeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterEnvelopeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFreqLevelModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FBuffer    : array [0..1, 0..1] of Single;
    FFreq, ff  : Single;
    FFreqDisp  : Single;
    FLevel, ll : Single;
    FEnv, pp   : Single;
    FAttack    : Single;
    FRelease   : Single;
    i2l        : Single;
    i2r        : Single;
    o2l        : Single;
    o2r        : Single;
    FMode      : Integer;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math;

procedure TSplitterDataModule.ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'NORMAL';
  1 : PreDefined := 'INVERSE';
  2 : PreDefined := 'NORM/INV';
  3 : PreDefined := 'INV/NORM';
 end;
end;

procedure TSplitterDataModule.ParameterEnvelopeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(Power(10, 1 + 2 * Parameter[index]), ffGeneral, 2, 2);
end;

procedure TSplitterDataModule.ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(FFreqDisp, ffGeneral, 5, 5); 
end;

procedure TSplitterDataModule.ParameterEnvelopeChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
  FAttack  := 0.05 * (1 - Value);
  FRelease := 1 - exp(-6 - 4 * Value);             // Envelope
  if (FAttack  > 0.02)   then FAttack  := 0.02;
  if (FRelease < 0.9995) then FRelease := 0.9995;
end;

procedure TSplitterDataModule.ParameterFreqLevelModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[Index]) of
    0: PreDefined := 'BELOW';
    1: PreDefined := 'ALL';
  else PreDefined := 'ABOVE';
 end;
end;

procedure TSplitterDataModule.VSTModuleOpen(Sender: TObject);
begin
 // Initial Parameters
 Parameter[0] := 0.3;     // Mode
 Parameter[1] := 0.5;     // Freq
 Parameter[2] := 0.5;     // Freq Mode
 Parameter[3] := -20;     // Level [dB] (was 2)
 Parameter[4] := 1;       // Level Mode
 Parameter[5] := 0.5;     // Envelope
 Parameter[6] := 0;       // Gain

 // Default Preset
 with Programs[0] do
  begin
   Parameter[0] := 0.3;     // Mode
   Parameter[1] := 0.5;     // Freq
   Parameter[2] := 0.5;     // Freq Mode
   Parameter[3] := -20;     // Level [dB] (was 2)
   Parameter[4] := 1;       // Level Mode
   Parameter[5] := 0.5;     // Envelope
   Parameter[6] := 0;       // Gain
  end;

 // Preset 1
 with Programs[1] do
  begin
   Parameter[0] := 0.3;   // Mode
   Parameter[1] := 0.5;   // Freq
   Parameter[2] := 1;     // Freq Mode
   Parameter[3] := -20;   // Level [dB] (was 2)
   Parameter[4] := 0.5;   // Level Mode
   Parameter[5] := 0.5;   // Envelope
   Parameter[6] := 0;     // Gain
  end;

 // Preset 2
 with Programs[2] do
  begin
   Parameter[0] := 2;
   Parameter[1] := 0.5;   // Freq
   Parameter[2] := 1;     // Freq Mode
   Parameter[3] := -20;   // Level [dB] (was 2)
   Parameter[4] := 0.5;   // Level Mode
   Parameter[5] := 0.5;   // Envelope
   Parameter[6] := 0;     // Gain
  end;

 VSTModuleSuspend(Sender);
end;

procedure TSplitterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample       : Integer;
  a, b         : Single;
  a0, a1       : Single;
  b0, b1       : Single;
  aa, bb, ee,
  e, at, re,
  lv, lx, px   : Single;
  il, ir, fx,
  l, r         : Single;
begin
  a0 := FBuffer[0, 0];
  a1 := FBuffer[0, 1];
  b0 := FBuffer[1, 0];
  b1 := FBuffer[1, 1];
  e  := FEnv;
  at := FAttack;
  re := FRelease;
  fx := ff;
  lv := FLevel;
  lx := ll;
  px := pp;
  il := i2l;
  ir := i2r;
  l  := o2l;
  r  := o2r;

 for Sample := 0 to SampleFrames - 1 do
  begin
    a := Inputs[0, Sample];
    b := Inputs[1, Sample];

    a0 := a0 + FFreq * (a - a0 - a1);  // Frequency Split
    a1 := a1 + FFreq * a0;
    aa := a1 + fx * a;

    b0 := b0 + FFreq * (b - b0 - b1);
    b1 := b1 + FFreq * b0;
    bb := b1 + fx * b;

    ee := -(aa + bb);

    if ee > lv
     then e := e + at * (px - e);      // Level Split
    e := e * re;

    a := il * a + l * aa * (e + lx);
    b := ir * b + r * bb * (e + lx);

   Outputs[0, Sample] := a;
   Outputs[1, Sample] := b;
  end;

  FEnv := e;
  if abs(e) < 1E-10 then FEnv := 0;
  FBuffer[0, 0] := a0;
  FBuffer[0, 1] := a1;
  FBuffer[1, 0] := b0;
  FBuffer[1, 1] := b1;

  if (abs(a0) < 1E-10) then            // catch denormals
   begin
    FBuffer[0, 0] := 0;
    FBuffer[0, 1] := 0;
    FBuffer[1, 0] := 0;
    FBuffer[1, 1] := 0;
   end;
end;

procedure TSplitterDataModule.VSTModuleResume(Sender: TObject);
var
  tmp : Integer;
begin
  FFreqDisp := Power(10, 2 + 2 * Parameter[1]);   // Frequency
  FFreq     := 5.5 * FFreqDisp / SampleRate;
  if FFreq > 1 then FFreq := 1;

  ff  := -1;                                      // Above
  tmp := Round(2.9 * Parameter[2]);               // Frequency Switching
  if tmp = 0 then ff := 0.0;                      // Below
  if tmp = 1 then FFreq := 0.001;                 // All

  FLevel := Power(10, 0.05 * Parameter[3] + 0.3); // Level

  ll := 0.0;                                      // Above
  tmp := Round(2.9 * Parameter[4]);               // Level Switching
  if (tmp = 0) then ll := -1;                     // Below
  if (tmp = 1) then FLevel := 0;                  // All

  pp := -1;                                       // Phase Correction
  if (ff = ll) then pp := 1;
  if (ff = 0) and (ll = -1)
   then ll := -ll;

  i2l := dB_to_Amp(Parameter[6]);                 // Gain
  i2r := i2l;
  o2l := i2l;
  o2r := i2l;

  FMode := Round(Parameter[0]);                   // Output Routing
  case Round(FMode) of
     0: begin i2l :=   0 ;  i2r :=   0 ; end;
     1: begin o2l := -o2l;  o2r := -o2r; end;
     2: begin i2l :=   0 ;  o2r := -o2r; end;
   else begin o2l := -o2l;  i2r :=   0 ; end;
  end;
end;

procedure TSplitterDataModule.VSTModuleSuspend(Sender: TObject);
begin
 FEnv := 0;
 FBuffer[0, 0] := 0;
 FBuffer[0, 1] := 0;
 FBuffer[1, 0] := 0;
 FBuffer[1, 1] := 0;
end;

end.
