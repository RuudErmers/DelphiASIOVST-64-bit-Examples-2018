unit TrackerDM;

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
  TTrackerDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleOpen(Sender: TObject);
  private
    FMode      : Integer;
    FThreshold : Single;
    FMin, FMax : Single;
    FTrans     : Single;
    FDry, FWet : Single;
    FDyn, FPhi : Single;
    FRel, FEnv : Single;
    FDeltaPhi  : Single;
    FBuffer    : array [0..3] of Single; 
    function FilterFreq(Hz: Double): Double;
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

procedure TTrackerDataModule.ParameterModeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[Index]) of
  0: PreDefined := 'SINE';
  1: PreDefined := 'SQUARE';
  2: PreDefined := 'SAW';
  3: PreDefined := 'RING';
  4: PreDefined := 'EQ';
 end;
end;

function TTrackerDataModule.FilterFreq(Hz: Double) : Double;
var
  j, k, r : Double;
begin
 r := 0.999;
 j := r * r - 1;
 k := 2 - 2 * sqr(r) * cos(0.647 * Hz / SampleRate);
 result := (sqrt(k * k - 4 * j * j) - k) / (2 * j);
end;

procedure TTrackerDataModule.VSTModuleOpen(Sender: TObject);
begin
(*
 //inits here!
 Parameter[0] := 0; //FMode
 Parameter[1] := 1; //Dynamics
 Parameter[2] := 1; //Mix
 Parameter[3] := 0.97; //Tracking
 Parameter[4] := 0.5; //Trnspose
 Parameter[5] := 0.8; //Maximum Hz
 Parameter[6] := 0.5; //Trigger dB
 Parameter[7] := 0.5; //Output

 res1 := cos(0.01); //p
 res2 := sin(0.01); //q
*)

 FMin := SampleRate / 30; //lower limit
 FDeltaPhi := 100 / SampleRate;  // Initial Pitch
end;

procedure TTrackerDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 //calcs here
 FMode := Round(Parameter[0] * 4.9);
(*
 fo := FilterFreq(50);
 fi := sqr(1 - fo);
 ddphi := sqr(Parameter[3]);
*)
 FThreshold := Power(10, 3 * Parameter[6] - 3.8);
 FWet := Power(10, 2 * Parameter[7] - 1);
 FMax := Round(SampleRate / Power(10, 1.6 + 2.2 * Parameter[5]));
 FTrans := Power(1.0594631, Round(72 * Parameter[4] - 36));

 if (FMode < 4) then
  begin
   FDyn := FWet * 0.6 * Parameter[2] * Parameter[1];
   FDry := FWet * sqrt(1 - Parameter[2]);
   FWet := FWet * 0.3 * Parameter[2] * (1 - Parameter[1]);
  end
 else
  begin
   FDry := FWet * (1 - Parameter[2]);
   FWet := FWet * (0.02 * Parameter[2] - 0.004);
   FDyn := 0;
  end;
 FRel := Power(10, -10 / SampleRate);
end;

procedure TTrackerDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample                 : Integer;
  a, b, x, t, p, dp,
  mn, ddp, tmp, tmp2     : Single;
  o, i, b1, b2, b3, b4   : Single;
  m, we, dr, bo, r1, r2  : Single;
  sw, dsw, dy, e, re     : Single;
  n, s, mo               : Integer;
const
  TwoPi : Single = 6.2831853;
begin
 t   := FThreshold;
 p   := FPhi;
 dp  := FDeltaPhi;
(*
 ddp := ddphi;
 o   := fo;
 i   := fi;
 b1  := FBuffer[0];
 b2  := FBuffer[1]
 bo  := fBold;
 r1  := res1;
 r2  := res2;
 b3  := FBuffer[2];
 b4  := FBuffer[3];
 sw  := saw;
 dsw := dsaw;
 re  := rel;
 n   := num;
 s   := sig;
*)
 we  := FWet;
 dr  := FDry;
 mn  := FMin;
 m   := FMax;
 dy  := FDyn;
 e   := FEnv;
 mo  := FMode;

 for Sample := 0 to SampleFrames - 1 do
  begin
   a := Inputs[0, Sample];
   b := Inputs[1, Sample];
   x := a; // + b;

   if (x > 0)                                   // Dynamics Envelope
    then tmp :=  x
    else tmp := -x;
   if (tmp > e)
    then e := 0.5 * (tmp + e)
    else e := e * re;

   b1 := o * b1 + i * x;
   b2 := o * b2 + b1;                              // low-pass filter

   if b2 > t then                                  // if >thresh
    begin
     if s < 1 then                                 // and was <thresh
      begin
       if n < mn then                              // not long ago
        begin
(*
         tmp2 := b2 / (b2 - bo);                   // update period
         tmp  := FTrans * TwoPi / (n + dn - tmp2);
         dp   := dp + ddp * (tmp - dp);
         dn   := tmp2;
*)
         dsw  := 0.3183098 * dp;
         if FMode = 4 then
          begin
           r1 := cos(4 * dp); //resonator
           r2 := sin(4 * dp);
          end;
        end;
        n := 0; //restart period measurement
      end;
     s := 1;
    end else
   if n > m then s := 0; //now <thresh
   inc(n);
   bo := b2;

   p := f_mod(p + dp, 2 * Pi);
   case mo of
     0 : x := sin(p);                 // Sine
     1 : if (sin(p) > 0)              // Square
          then x :=  0.5
          else x := -0.5;

     2 : begin                        // Saw
          sw := f_mod(sw + dsw, 2);
          x  := sw - 1;
         end;
     3 : x := x * sin(p);             // Ring

       //filt
     4: begin
         x  := x + (b3 * r1) - (b4 * r2);
         b4 := 0.996 * ((b3 * r2) + (b4 * r1));
         b3 := 0.996 * x;
        end; 
    end;
    x := x * (we + dy * e);
    Outputs[0, Sample] := a;           // dr * a + x;
    Outputs[1, Sample] := dr * b + x;
  end;

  if abs(b1) < 1E-10 then
   begin
    FBuffer[0] := 0;
    FBuffer[1] := 0;
    FBuffer[2] := 0;
    FBuffer[3] := 0;
   end
  else
   begin
    FBuffer[0] := b1;
    FBuffer[1] := b2;
    FBuffer[2] := b3;
    FBuffer[3] := b4;
   end;

(*
  sig   := s;
  fBold := bo;

  if n > 100000
   then num := 100000
   else num := n;

  saw   := sw;
  dsaw  := dsw;
  res1  := r1;
  res2  := r2;
*)

 FPhi      := p;
 FDeltaPhi := dp;
 FEnv      := e;
end;

end.

(*
void mdaTracker::getParameterDisplay(VstInt32 index, char *text)
begin
 case index of
  1: long2string(Round(100 * Parameter[1]), text); break;
  2: long2string(Round(100 * Parameter[2]), text); break;
  3: long2string(Round(100 * Parameter[3]), text); break;
  4: long2string(Round( 72 * Parameter[4] - 36), text); break;
  5: long2string(Round(SampleRate / FMax), text); break;
  6: long2string(Round( 60 * Parameter[6] - 60), text); break;
  7: long2string(Round( 40 * Parameter[7] - 20), text); break;
 end;
end;
*)
