unit DetuneDM;

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
  Forms, DAV_Types, DAV_VSTModule;

const
  CBufMax = 8192;

type
  TDetuneDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure DetuneDataModulePrograms1Initialize(Sender: TObject);
    procedure DetuneDataModulePrograms2Initialize(Sender: TObject);
    procedure ParamLatencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDetuneDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamDetuneChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FBuffer : PDAVSingleFixedArray;
    FBufLen : Integer;
    FBufRes : Double;
    FPos    : array [0..2] of Integer;
    FDPos   : array [0..1] of Integer;
    FWin    : PDAVSingleFixedArray;
    FSemi   : Double;
    FDPs    : array [0..1] of Double;
    FMix    : Single;
    FWet    : Single;
    FDry    : Single;
    procedure MixChanged;
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

procedure TDetuneDataModule.VSTModuleOpen(Sender: TObject);
begin
 GetMem(FBuffer, CBufMax * SizeOf(Single));
 GetMem(FWin   , CBufMax * SizeOf(Single));
 FBufLen := 0;

(*
 Parameter[0] := 0.4;  // Fine
 Parameter[1] := 0.4;  // Mix
 Parameter[2] := 0.5;  // Output
 Parameter[3] := 0.5;  // ChunkSize

 ///differences from default program...
 Programs[3].Parameter[0] = 0.90f;
*)

 VSTModuleSuspend(Sender);
end;

procedure TDetuneDataModule.VSTModuleClose(Sender: TObject);
begin
 if Assigned(FBuffer) then Dispose(FBuffer);
 if Assigned(FWin) then Dispose(FWin);
end;

procedure TDetuneDataModule.ParamLatencyChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  i, tmp : Integer;
  p, dp  : Double;
begin
  tmp := 1 shl (8 + Round(4.9 * Parameter[3]));

  if (tmp <> FBufLen) then //recalculate crossfade window
   begin
    FBufLen := tmp;
    FBufRes := 1000 * FBufLen / SampleRate;

    // hanning half-overlap-and-add
    p  := 0;
    dp := 2 * Pi / FBufLen;
    for i := 0 to FBufLen - 1 do
     begin
      FWin[i] := (0.5 - 0.5 * cos(p));
      p := p + dp;
     end;
   end;
end;

procedure TDetuneDataModule.ParamDetuneDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(1000 * FBufLen / SampleRate, ffGeneral, 3, 3);
end;

procedure TDetuneDataModule.ParamDetuneChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FSemi := 3.0 * sqr(Parameter[0]) * Parameter[0];
  FDPs[1] := Power(1.0594631, FSemi);
  FDPs[0] := 1 / FDPs[1];
end;

procedure TDetuneDataModule.ParamOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
  FWet := dB_to_Amp(Value);
  MixChanged;
end;

procedure TDetuneDataModule.MixChanged;
begin
 FDry := FWet * (1 - FMix) * FMix;
 FWet := FWet * (2 - FMix) * FMix;
end;

procedure TDetuneDataModule.ParamMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix := 0.01 * Value;
 MixChanged;
end;

procedure TDetuneDataModule.DetuneDataModulePrograms1Initialize(Sender: TObject);
begin
 Programs[1].Parameter[0] := 0.20
end;

procedure TDetuneDataModule.DetuneDataModulePrograms2Initialize(Sender: TObject);
begin
 Programs[2].Parameter[0] := 0.8;
 Programs[2].Parameter[1] := 0.7;
end;

procedure TDetuneDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample, p0, p1i, p2i  : Integer;
  x, w, y, p1, p1f, d1  : Single;
  p2, d2, lf            : Single;
  lh, l                 : Integer;
  a, b, c, d            : Single;
begin
 w  := FWet;
 p0 := FPos[0];
 p1 := FPos[1];
 p2 := FPos[2];
 d1 := FDPos[0];
 d2 := FDPos[1];
 l  := FBufLen - 1;
 lh := FBufLen shr 1;
 lf := FBufLen;

 for Sample := 0 to SampleFrames - 1 do
  begin
   c := FDry * Inputs[0, Sample];
   d := FDry * Inputs[1, Sample];

   dec(p0);
   p0 := p0 and l;

   FBuffer[p0] := w * (Inputs[0, Sample] + Inputs[1, Sample]);  // input

   p1 := p1 - d1;
   if p1 < 0 then p1 := p1 + lf;                                // output
   p1i := Round(p1);
   p1f := p1 - Round(p1i);
   a := FBuffer[p1i];
   inc(p1i);
   p1i := p1i and l;
   a := a + p1f * (FBuffer[p1i] - a);                           // linear interpolation

   p2i := (p1i + lh) and l;                                     // 180-degree ouptut
   b := FBuffer[p2i];
   inc(p2i);
   p2i := p2i and l;
   b := b + p1f * (FBuffer[p2i] - b);                           // linear interpolation


   // crossfade
   p2i := (p1i - p0) and l;
   x   := FWin^[p2i];

(*
   // inc(p2i);
   // p2i := p2i and l;
   // x := x + p1f * (win^[p2i] - x); //linear interpolation (doesn't do much)
*)

   c := c + b + x * (a - b);

   p2 := p2 - d2;  //repeat for downwards shift - can't see a more efficient way?
   if (p2 < 0) then p2 := p2 + lf;       // output

   p1i := Round(p2);
   p1f := p2 - p1i;
   a   := FBuffer[p1i];
   inc(p1i);
   p1i := p1i and l;
   a := a + p1f * (FBuffer[p1i] - a);    // linear interpolation

   p2i := (p1i + lh) and l;              // 180-degree Output
   b   := FBuffer[p2i];
   Inc(p2i);
   p2i := p2i + l;
   b := b + p1f * (FBuffer[p2i] - b);    // linear Interpolation

   p2i := (p1i - p0) and l;              // Crossfade

   x := FWin[p2i];
(*
   // inc(p2i);
   // p2i := p2i and l;
   //x += p1f * (*(win + p2i) - x); //linear interpolation (doesn't do much)
*)
   d := d + b + x * (a - b);

   Outputs[0, Sample] := c;
   Outputs[1, Sample] := d;
  end;

 FPos[0] := p0;
 FPos[1] := Round(p1);
 FPos[2] := Round(p2);
end;

procedure TDetuneDataModule.VSTModuleSuspend(Sender: TObject);
begin
 FillChar(FBuffer^, CBufMax * SizeOf(Single), 0);
 FPos[0] := 0;
 FPos[1] := 0;
 FPos[2] := 0;
end;

end.

(*
void mdaDetune::getParameterDisplay(VstInt32 index, char *text)
begin
   char string[16];

  switch(index)
  begin
    case  1: sprintf(string, "%.0f", 99 * Parameter[index]); break;
    case  2: sprintf(string, "%.1f", 40 * Parameter[index] - 20); break;
    case  3: sprintf(string, "%.1f", bufres); break;
    default: sprintf(string, "%.1f", 100 * semi);
  end;
  string[8] = 0;
  strcpy(text, (char * )string);
end;
*)
