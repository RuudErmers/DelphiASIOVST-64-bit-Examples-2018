unit ShepardDM;

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
  TShepardDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModeChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FMax    : Integer;
    FBuffer : array [0..1] of PDAVSingleFixedArray;
    FOut    : Single;
    FPos    : Single;
    FRate   : Single;
    FDRate  : Single;
    FMode   : Integer;
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

procedure TShepardDataModule.ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[0]) of
  0: PreDefined := 'TONES';
  1: PreDefined := 'RING MOD';
  2: PreDefined := 'TONES + IN';
 end;
end;

procedure TShepardDataModule.ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FOut := 0.4842 * dB_to_Amp(Value - 1);
end;

procedure TShepardDataModule.ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDRate := 1 + 10 * Power(0.01 * Value - 0.5, 3) / SampleRate;
end;

procedure TShepardDataModule.ParameterModeChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMode := Round(Value);
end;

procedure TShepardDataModule.VSTModuleOpen(Sender: TObject);
var
  i, j : Integer;
  x, a : Single;
const
  Twopi = 6.2831853;
begin
 FMax := 512 * SizeOf(Single);
 GetMem(FBuffer[0], FMax);
 GetMem(FBuffer[1], FMax);
 for i := 0 to FMax - 1 do
  begin
   FPos := (2 * Pi * i / (FMax - 1)); //generate wavetables
   x    := 0;
   a    := 1;
   FBuffer[1, FMax] := sin(FPos);
   for j := 0 to 7 do
    begin
     x   := x + a * sin(f_mod(FPos, twopi));
     a   := a * 0.5;
     FPos := FPos * 2;
    end;
    FBuffer[0, FMax] := x;
  end;
 i := 511;
 FBuffer[0, i] := 0;
 FBuffer[1, i] := 0; // wrap end for interpolation
 FPos  := 0;
 FRate := 1;

 // Initial Parameters
 Parameter[0] := 0.2; // Mode
 Parameter[1] := 0.7; // Rate
 Parameter[2] := 0.5; // Level
end;

procedure TShepardDataModule.VSTModuleClose(Sender: TObject);
begin
 Dispose(FBuffer[0]);
 Dispose(FBuffer[1]);
end;

procedure TShepardDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample       : Integer;
  a, b         : Single;
  r, p, di     : Single;
  x, m, i1, i2 : Integer;
begin
 r := FRate;
 p := FPos;
 x := FMax;
 m := FMode;

 for Sample := 0 to SampleFrames - 1 do
  begin
   a := Inputs[0, Sample] + Inputs[1, Sample];

   r := r * FDRate;
   if r > 2 then
    begin
     r := r * 0.5;
     p := p * 0.5;
    end
   else if r < 1 then
    begin
     r := r * 2;
     p := p * 2;
     if (p > x)
      then p := p - x;
    end;

   p := p + r;
   if (p > x)
    then p := p - x;

   i1 := Round(p); //interpolate position
   i2 := i1 + 1;
   di := i2 - p;

   b :=          di  * (FBuffer[0, i1] + (r - 2) * FBuffer[1, i1]);
   b := b + (1 - di) * (FBuffer[0, i2] + (r - 2) * FBuffer[1, i2]);
   b := b * FOut / r;

   if (m > 0) then
    if (m = 2)
     then b := b + 0.5 * a
     else b := b * a; //ring mod or add

   Outputs[0, Sample] := b;
   Outputs[1, Sample] := b;
  end;

 FPos  := p;
 FRate := r;
end;

end.
