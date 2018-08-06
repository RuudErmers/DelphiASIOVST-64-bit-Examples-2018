unit TestToneDM;

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
  TTestToneDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterChannelDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FLength    : Integer;
    FPhi       : Single;
    FDeltaPhi  : Single;
    FLeft      : Single;
    FRight     : Single;
    FThru      : Single;
    FSw        : Single;
    FSwx       : Single;
    FSwd       : Single;
    FScale     : Single;
    FSwt       : Integer;
    FMode      : Integer;
    FPinkState : array [0..5] of Single;
    function Midi2String(const n : Single): string;
    function ISO2String(b: Single): string;
    procedure Update;
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

function TTestToneDataModule.Midi2String(const n: Single): string;
var
  o, s : Integer;
begin
 result := '   ';
 o      := Round(n / 12);
 s      := Round(n - (12 * o));
 o      := o - 2;

 case s of
    0: result := result + 'C';
    1: result := result + 'C#';
    2: result := result + 'D';
    3: result := result + 'Eb';
    4: result := result + 'E';
    5: result := result + 'F';
    6: result := result + 'F#';
    7: result := result + 'G';
    8: result := result + 'G#';
    9: result := result + 'A';
   10: result := result + 'Bb';
  else result := result + 'B';
 end;

 result := result + ' ';

 if (o < 0) then result := result + '-';
 result := result + char(48 + (abs(o) mod 10));
end;

procedure TTestToneDataModule.VSTModuleOpen(Sender: TObject);
begin
 // Initial Parameters
 Parameter[0] := 0.47; //FMode
 Parameter[1] := 0.71; //level dB
 Parameter[2] := 0.50; //pan dB
 Parameter[3] := 0.57; //freq1 B
 Parameter[4] := 0.50; //freq2 Hz
 Parameter[5] := 0.00; //FThru dB
 Parameter[6] := 0.30; //sweep ms
 Parameter[7] := 1.00; //cal dBFS

(*
 updateTx := updateRx;

 setParameter(6, 0);
*)
 VSTModuleSuspend(Sender);
end;

procedure TTestToneDataModule.ParameterModeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[Index]) of
  0: PreDefined := 'MIDI #';
  1: PreDefined := 'IMPULSE';
  2: PreDefined := 'WHITE';
  3: PreDefined := 'PINK';
  4: PreDefined := '---';
  5: PreDefined := 'SINE';
  6: PreDefined := 'LOG SWP.';
  7: PreDefined := 'LOG STEP';
  8: PreDefined := 'LIN SWP.';
 end;
end;

procedure TTestToneDataModule.ParameterChannelDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[2]) of
  0 : PreDefined := 'LEFT';
  1 : PreDefined := 'CENTRE';
  2 : PreDefined := 'RIGHT';
 end;
end;

function TTestToneDataModule.ISO2String(b : Single): string;
begin
 case Round(b) of
   13: result := '20 Hz';
   14: result := '25 Hz';
   15: result := '31 Hz';
   16: result := '40 Hz';
   17: result := '50 Hz';
   18: result := '63 Hz';
   19: result := '80 Hz';
   20: result := '100 Hz';
   21: result := '125 Hz';
   22: result := '160 Hz';
   23: result := '200 Hz';
   24: result := '250 Hz';
   25: result := '310 Hz';
   26: result := '400 Hz';
   27: result := '500 Hz';
   28: result := '630 Hz';
   29: result := '800 Hz';
   30: result := '1 kHz';
   31: result := '1.25 kHz';
   32: result := '1.6 kHz';
   33: result := '2.0 kHz';
   34: result := '2.5 kHz';
   35: result := '3.1 kHz';
   36: result := '4 kHz';
   37: result := '5 kHz';
   38: result := '6.3 kHz';
   39: result := '8 kHz';
   40: result := '10 kHz';
   41: result := '12.5 kHz';
   42: result := '16 kHz';
   43: result := '20 kHz';
  else result := '--';
 end;
end;

procedure TTestToneDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample, st, m  : Integer;
  a, b, x, l, r  : Single;
  z              : array [0..5] of Single;
  ph, dph, t     : Single;
  s, sx, ds, fsc : Single;
const
  TwoPi : Double = 2 * Pi;
begin
(*
 if (updateRx <> updateTx) then update;
*)

 x := 0;
 z[0] := FPinkState[0];
 z[1] := FPinkState[1];
 z[2] := FPinkState[2];
 z[3] := FPinkState[3];
 z[4] := FPinkState[4];
 z[5] := FPinkState[5];

 ph   := FPhi;
 dph  := FDeltaPhi;
 l    := FLeft;
 r    := FRight;
 t    := FThru;
 s    := FSw;
 sx   := FSwx;
 ds   := FSwd;
 fsc  := FScale;
 st   := FSwt;
 m    := FMode;

 for Sample := 0 to SampleFrames - 1 do
  begin
   a := Inputs[0, Sample];
   b := Inputs[1, Sample];

   case m of
    1: if st > 0 then //impulse
        begin
         dec(st);
         x := 0;
        end
       else
        begin
         x  := 1;
         st := round (FLength * SampleRate);
        end;
    2, 3: begin
           x := 2 * Random - 1;               // Noise
           if (m = 3) then
            begin
             z[0] := 0.997 * z[0] + 0.029591 * x; // Pink filter
             z[1] := 0.985 * z[1] + 0.032534 * x;
             z[2] := 0.950 * z[2] + 0.048056 * x;
             z[3] := 0.850 * z[3] + 0.090579 * x;
             z[4] := 0.620 * z[4] + 0.108990 * x;
             z[5] := 0.250 * z[5] + 0.255784 * x;
             x    := z[0] + z[1] + z[2] + z[3] + z[4] + z[5];
           end;
          end;

    4: x := 0;                                // Mute

    0, 5, 9: begin                            // Tones
              ph := f_mod(ph + dph, TwoPi);
              x  := sin(ph);
             end;

    6, 7: begin                               // Log sweep & step
           if (st > 0) then
            begin
             dec(st);
             ph := 0;
            end
           else
            begin
             s := s + ds;
            end;
           if (m = 7)
            then dph := fsc * Power(10, 0.1 * Round(s))
            else dph := fsc * Power(10, 0.1 * s);
           x  := sin(ph);
           ph := f_mod(ph + dph, 2 * Pi);
           if (s > sx) then
            begin
             l := 0;
             r := 0;
            end;
          end;

    8: begin                                  // Lin sweep
        if st > 0 then
         begin
          dec(st);
          ph := 0;
         end
        else
         begin
          s  := s + ds;
          x  := sin(ph);
          ph := f_mod(ph + s, 2 * Pi);
          if (s > sx) then
           begin
            l := 0;
            r := 0;
           end;
         end;
       end;
   end;

   Outputs[0, Sample] := t * a + l * x;
   Outputs[1, Sample] := t * b + r * x;
  end;

 FPinkState[0] := z[0];
 FPinkState[1] := z[1];
 FPinkState[2] := z[2];
 FPinkState[3] := z[3];
 FPinkState[4] := z[4];
 FPinkState[5] := z[5];
 FPhi := ph;
 FSw  := s;
 FSwt := st;
(*
 if (s > sx)
  then setParameter(0, Parameter[0]); //retrigger sweep
*)
end;

procedure TTestToneDataModule.VSTModuleSuspend(Sender: TObject);
begin
 FPinkState[0] := 0;
 FPinkState[1] := 0;
 FPinkState[2] := 0;
 FPinkState[3] := 0;
 FPinkState[4] := 0;
 FPinkState[5] := 0;
 FPhi := 0;
end;

procedure TTestToneDataModule.Update;
var
  f, df : Single;
const
  TwoPi : Single = 2 * Pi;  
begin
(*
 updateRx := updateTx;

 //calcs here!
 FMode := Round(8.9 * Parameter[0]);
 FLeft := 0.05 * Round(60.*Parameter[1]);
 FLeft := Power(10, FLeft - 3);
 if (FMode = 2) then FLeft := FLeft * 0.0000610; //scale white for RAND_MAX = 32767
 if (FMode = 3) then FLeft := FLeft * 0.0000243; //scale pink for RAND_MAX = 32767
 if(Parameter[2] < 0.3) then FRight := 0 else FRight := FLeft;
 if(Parameter[2] > 0.6) then FLeft  := 0;
 fLengh := 1 + 0.5 * Round(62 * Parameter[6]);
 FSwt := Round(fLengh * SampleRate);

 if (Parameter[7] > 0.8) then //output level trim
  begin
   if (Parameter[7] > 0.96)     then cal := 0;
   else if(Parameter[7] > 0.92) then cal := -0.01000001;
   else if(Parameter[7] > 0.88) then cal := -0.02000001;
   else if(Parameter[7] > 0.84) then cal := -0.1;
   else cal := -0.2;

   calx   := Power(10, 0.05 * cal);
   FLeft  := FLeft * calx;
   FRight := FRight * calx; 
   calx   := 0;
  end;
 else //output level calibrate
  begin
   cal    := Round(25 * Parameter[7] - 21.1);
   calx   := cal;
  end;
*)

 df := 0;
 if Parameter[4] > 0.6 then df := 1.25 * Parameter[4] - 0.75;
 if Parameter[4] < 0.4 then df := 1.25 * Parameter[4] - 0.50;

 case FMode of
  0: begin //MIDI note
      f := Trunc(128 * Parameter[3]);
(*
      //long2string((long), disp1); //Semi
      midi2string(f, disp1); //Semitones
      long2string((long)(100.*df), disp2); //Cents
*)
      FDeltaPhi := 51.37006 * Power(1.0594631, f + df) / SampleRate;
     end;

    1, 2, 3, 4:  begin //no frequency display
(*
                  strcpy(disp1, "--");
                  strcpy(disp2, "--"); break;
*)
                 end;
    5: begin // Sine
        f := 13 + Trunc(30 * Parameter[3]);
(*
        iso2string(, disp1); //iso band freq
        f := Power(10, 0.1 * (f + df));
        float2strng(, disp2); //Hz
*)
        FDeltaPhi := 2 * Pi * f / SampleRate;
       end;

    6, 7: begin //log sweep & step
(*
           FSw  := 13 + Trunc(30 * Parameter[3]);
           FSwx := 13 + Trunc(30 * Parameter[4]);
           iso2string(FSw, disp1); //start freq
           iso2string(FSwx, disp2); //end freq
           if FSw > FSwx then
            begin
             FSwd := FSwx;
             FSwx := FSw;
             FSw  := FSwd;
            end; //only sweep up
           if FMode = 7 then FSwx := FSwx + 1;
           FSwd := (FSwx - sw) / (fLengh * SampleRate);
           FSwt := 2 * round SampleRate;
*)
          end;

   8: begin//lin sweep
(*
       FSw  := 200 * Trunc(100 * Parameter[3]);
       FSwx := 200 * Trunc(100 * Parameter[4]);
       long2string(Round(FSw), disp1); //start freq
       long2string(Round(FSwx), disp2); //end freq
       if (FSw > FSwx) then
        begin
         FSwd := FSwx;
         FSwx := FSw;
         FSw  := FSwd;
        end; //only sweep up
       FSw  := twopi*FSw/SampleRate();
       FSwx := twopi*FSwx/SampleRate();
       FSwd := (FSwx-sw) / (fLengh*SampleRate());
       FSwt := 2 * (long)SampleRate();
*)
      end;
 end;

 FThru := Power(10, (0.05 * int(40 * Parameter[5])) - 2);
 if Parameter[5] = 0 then FThru := 0;
 FScale := 2 * Pi / SampleRate;
end;

end.

(*
void mdaTestTone::setParameter(VstInt32 index, float value)
var
  f, df : Single;
begin
 //just update display text...
 int FMode := Round(8.9 * Parameter[0]);
 df := 0.0;
 if (Parameter[4] > 0.6) then df := 1.25 * Parameter[4] - 0.75;
 if (Parameter[4] < 0.4) then df := 1.25 * Parameter[4] - 0.50;
 switch(FMode)
 begin
  case 0: //MIDI note
      f := Trunc(128.*Parameter[3]);
      //long2string((long), disp1); //Semi
      midi2string(, disp1); //Semitones
      long2string((long)(100.*df), disp2); //Cents
      break;

  case 1: //no frequency display
  case 2:
  case 3:
  case 4: strcpy(disp1, "--");
          strcpy(disp2, "--"); break;
  
  case 5: //sine
      f := 13. + Trunc(30.*Parameter[3]);
      iso2string(, disp1); //iso band freq
      f := Power(10.0, 0.1*(+df));
      float2strng(, disp2); //Hz
      break;
  
  case 6: //log sweep & step    
  case 7: FSw = 13. + Trunc(30.*Parameter[3]);
      FSwx = 13. + Trunc(30.*Parameter[4]);
      iso2string(FSw, disp1); //start freq
      iso2string(FSwx, disp2); //end freq
      break; 
  
  case 8: //lin sweep
      FSw = 200. * Trunc(100.*Parameter[3]);
      FSwx = 200. * Trunc(100.*Parameter[4]);
      long2string((long)FSw, disp1); //start freq
      long2string((long)FSwx, disp2); //end freq
      break; 
 end;

 updateTx++;
end;

void mdaTestTone::getParameterDisplay(VstInt32 index, char *text)
begin
 switch(index)
 begin
  case 1: long2string((long)(int(60. * Parameter[1]) - 60.0 - calx), text); break;
  case 3: strcpy(text, disp1); break;
  case 4: strcpy(text, disp2); break;
  case 6: if(Parameter[5]==0) strcpy(text, "OFF");
      else long2string((long)(40 * Parameter[5] - 40), text); break;
  case 5: long2string(1000 + 500*int(62*Parameter[6]), text); break;
  case 7: float2strng(cal, text); break;
 end;
end;
*)
