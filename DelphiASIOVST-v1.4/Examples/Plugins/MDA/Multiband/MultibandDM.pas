unit MultibandDM;

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
  TMultibandDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FDriveL    : Single;
    FTrimL     : Single;
    FAttackL   : Single;
    FReleaseL  : Single;
    FDriveM    : Single;
    FTrimM     : Single;
    FAttackM   : Single;
    FReleaseM  : Single;
    FDriveH    : Single;
    FTrimH     : Single;
    FAttackH   : Single;
    FReleaseH  : Single;
    FSLev      : Single;
    FGainL     : Single;
    FGainM     : Single;
    FGainH     : Single;
    FFeedback  : array [0..2] of Single;
    FFi        : array [0..1, 0..1] of Single;
    FMSwap     : Boolean;
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

procedure TMultibandDataModule.ParameterGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(40 * Parameter[Index] - 20, ffGeneral, 2, 2);
end;

procedure TMultibandDataModule.ParameterOutputDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Low';
  1 : PreDefined := 'Mid';
  2 : PreDefined := 'High';
  3 : PreDefined := 'Output';
 end;
end;

procedure TMultibandDataModule.VSTModuleOpen(Sender: TObject);
begin
 Parameter[ 6] := 0.5;   // L trim   (2)
 Parameter[ 7] := 0.5;   // M trim
 Parameter[ 8] := 0.5;   // H trim
{
 //inits here!
 Parameter[ 0] := 1.00;  // Listen: L/M/H/out
 Parameter[ 1] := 0.50;  // xover1
 Parameter[ 2] := 0.50;  // xover2
 Parameter[ 3] := 0.45;  // L drive  (1)
 Parameter[ 4] := 0.45;  // M drive
 Parameter[ 5] := 0.45;  // H drive
 Parameter[ 9] := 0.22;  // attack   (3)
 Parameter[10] := 0.60;  // release  (4)
 Parameter[11] := 0.50;  // width
 Parameter[12] := 0.40;  // MS swap*/
}
end;

procedure TMultibandDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 //calcs here
 FDriveL   := Power(10, (2.5 * Parameter[3]) - 1);
 FTrimL    := 0.5 + (4 - 2 * Parameter[9]) * (sqr(Parameter[3]) * Parameter[3]);
 FTrimL    := FTrimL * Power(10, 2.0 * Parameter[6] - 1);
 FAttackL  := Power(10, -0.05 -(2.5 * Parameter[9]));
 FReleaseL := Power(10, -2 - (3.5 * Parameter[10]));

 FDriveM   := Power(10, (2.5 * Parameter[4]) - 1);
 FTrimM    := 0.5 + (4 - 2 * Parameter[9]) * (sqr(Parameter[4]) * Parameter[4]);
 FTrimM    := FTrimM * Power(10, 2 * Parameter[7] - 1);
 FAttackM  := Power(10, -0.05 -(2 * Parameter[9]));
 FReleaseM := Power(10, -2 - (3 * Parameter[10]));

 FDriveH   := Power(10, (2.5 * Parameter[5]) - 1);
 FTrimH    := 0.5 + (4 - 2 * Parameter[9]) * (sqr(Parameter[5]) * Parameter[5]);
 FTrimH    := FTrimH * Power(10, 2 * Parameter[8] - 1);
 FAttackH  := Power(10, -0.05 -(1.5 * Parameter[9]));
 FReleaseH := Power(10, -2 - (2.5 * Parameter[10]));

 case Round(Parameter[0] * 10) of
     0: begin FTrimM := 0; FTrimH := 0; FSLev := 0; end;
  1, 2: begin FTrimL := 0; FTrimH := 0; FSLev := 0; end;
  3, 4: begin FTrimL := 0; FTrimM := 0; FSLev := 0; end;
   else FSLev := Parameter[11];
 end;

 FFi[0, 0] := Power(10, Parameter[1] - 1.70); FFi[1, 0] := (1 - FFi[0, 0]);
 FFi[0, 1] := Power(10, Parameter[2] - 1.05); FFi[1, 1] := (1 - FFi[0, 1]);

 FMSwap := (Parameter[12] > 0.5);
end;

procedure TMultibandDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample              : Integer;
  a, b, c, d, m, h, s : Single;
  tmp1, tmp2, tmp3    : Single;
  l, sl               : Single;
  f1i, f1o, f2i, f2o  : Single;
  b1, b2              : Single;
  g1, d1, t1, a1, r1  : Single;
  g2, d2, t2, a2, r2  : Single;
  g3, d3, t3, a3, r3  : Single;
  ms                  : Boolean;
begin
 l   := FFeedback[2];
 sl  := FSLev;
 f1i := FFi[0, 0];
 f1o := FFi[1, 0];
 f2i := FFi[0, 1];
 f2o := FFi[1, 1];
 b1  := FFeedback[0];
 b2  := FFeedback[1];
 g1  := FGainL;
 d1  := FDriveL;
 t1  := FTrimL;
 a1  := FAttackL;
 r1  := 1 - FReleaseL;
 g2  :=FGainM;
 d2 := FDriveM;
 t2 := FTrimM;
 a2 := FAttackM;
 r2 := 1 - FReleaseM;
 g3  :=FGainH;
 d3 := FDriveH;
 t3 := FTrimH;
 a3 := FAttackH;
 r3 := 1 - FReleaseH;
 ms := FMSwap;

 for Sample := 0 to SampleFrames - 1 do
  begin
   a := Inputs[0, Sample];
   b := Inputs[1, Sample]; //process from here...

   if ms
    then b := -b
    else b := b;

   s  := (a - b) * sl; //keep stereo component for later
   a  := a + b;
   b2 := (f2i * a) + (f2o * b2); //crossovers
   b1 := (f1i * b2) + (f1o * b1);
   l  := (f1i * b1) + (f1o * l);
   m  := b2 - l;
   h  := a - b2;

   tmp1 := abs(l);  //l
   if (tmp1 > g1)
    then g1 := g1 + a1 * (tmp1 - g1)
    else g1 := g1 * r1;
   tmp1 := 1 / (1 + d1 * g1);

   tmp2 := abs(m);
   if (tmp2 > g2)
    then g2 := g2 + a2 * (tmp2 - g2)
    else g2 := g2 * r2;
   tmp2 := 1 / (1 + d2 * g2);

   tmp3 := abs(h);
   if (tmp3 > g3)
    then g3 := g3 + a3 * (tmp3 - g3)
    else g3 := g3 * r3;
   tmp3 := 1 / (1 + d3 * g3);

   a := (l * tmp1 * t1) + (m * tmp2 * t2) + (h * tmp3 * t3);
   c := a + s; // output
   if ms
    then d := s - a
    else d := a - s;

   Outputs[0, Sample] := c;
   Outputs[1, Sample] := d;
  end;

  if (g1 < 1E-10) then FGainL := 0 else FGainL := g1;
  if (g2 < 1E-10) then FGainM := 0 else FGainM := g2;
  if (g3 < 1E-10) then FGainH := 0 else FGainH := g3;

  if (abs(b1) < 1E-10) then
   begin
    FFeedback[0] := 0;
    FFeedback[1] := 0;
    FFeedback[2] := 0;
   end
  else
   begin
    FFeedback[0] := b1;
    FFeedback[1] := b2;
    FFeedback[2] := l;
   end;

(*
 // do not use this code, it's just an example on how a VCA would look like
 fGain := (1 / (1 + d1 * abs(l)) ); //VCAs
 if (g1 > fGain)
  then g1 := g1 - a1 * (g1 - g)
  else g1 := g1 + r1 * (g - g1);
*)
end;

end.

(*
void mdaMultiBand::getParameterDisplay(VstInt32 index, char *text)
{
  switch(index)
  {
    case 1: long2string((long)(SampleRate * FFi[0,0] * (0.098 + 0.09 * FFi[0,0] + 0.5 * Power(FFi[0,0], 8.2))), text); break;
    case 2: long2string((long)(SampleRate * FFi[0,1] * (0.015 + 0.15 * FFi[0,1] + 0.9 * Power(FFi[0,1], 8.2))), text); break;
    case 3: long2string((long)(30.0 * Parameter[3]), text); break;
    case 4: long2string((long)(30.0 * Parameter[4]), text); break;
    case 5: long2string((long)(30.0 * Parameter[5]), text); break;
    case 6: long2string((long)(40.0 * Parameter[6] - 20.0), text); break;
    case 7: long2string((long)(40.0 * Parameter[7] - 20.0), text); break;
    case 8: long2string((long)(40.0 * Parameter[8] - 20.0), text); break;
    case 9: long2string((long)(-301030.1 / (SampleRate * log10(1.0 - FAttackM))),text); break;
    case 10: long2string((long)(-301.0301 / (SampleRate * log10(1.0 - FReleaseM))),text); break;
    case 11: long2string((long)(200.0 * Parameter[11]), text); break;
    case 12: if(FMSwap) strcpy(text, "S"); 
                  else strcpy(text, "M"); break;
  }
}

*)
