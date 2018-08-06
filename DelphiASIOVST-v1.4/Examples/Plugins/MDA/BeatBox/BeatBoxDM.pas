unit BeatBoxDM;

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

type
  TBeatBoxDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDynamicsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FHihatBufferLength : Integer;
    FKickBufferLength  : Integer;
    FSnareBufferLength : Integer;
    FHihatThreshold    : Single;
    FHihatDelay        : Integer;
    FSnareThreshold    : Single;
    FSnareDelay        : Integer;
    FKickThreshold     : Single;
    FKickDelay         : Integer;
    FSnareBufferPos    : Integer;
    FRec               : Integer;
    FRecx              : Integer;
    FRecPos            : Integer;
    FMix               : Single;
    FDyna              : Single;
    FDynr              : Single;
    FDyne              : Single;
    FDynm              : Single;
    FKWW, FKWWx        : Single;
    FKSF1              : Single;
    FKSF2              : Single;
    FWW, FWWx          : Single;
    FHfil              : Single;
    FHBufPos           : Integer;
    FSBufPos           : Integer;
    FSb1               : Single;
    FSb2               : Single;
    FKBufPos           : Integer;
    FKsb1              : Single;
    FKsb2              : Single;
    FSf1               : Single;
    FSf2               : Single;
    FSf3               : Single;
    FSnareFX           : Single;
    FKSFX              : Single;
    FHihatLevel        : Single;
    FKickLevel         : Single;
    FSnareLevel        : Single;
    FHihatBuffer       : PDAVSingleFixedArray;
    FKickBuffer        : PDAVSingleFixedArray;
    FSnareBuffer       : array [0..1] of PDAVSingleFixedArray;
    procedure Synth;
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

procedure TBeatBoxDataModule.VSTModuleClose(Sender: TObject);
begin
 if Assigned(FHihatBuffer)    then Dispose(FHihatBuffer);
 if Assigned(FKickBuffer)     then Dispose(FKickBuffer);
 if Assigned(FSnareBuffer[0]) then Dispose(FSnareBuffer[0]);
 if Assigned(FSnareBuffer[1]) then Dispose(FSnareBuffer[1]);
end;

procedure TBeatBoxDataModule.VSTModuleOpen(Sender: TObject);
var
  Oversampling : Integer;
begin
 Oversampling := Round(SampleRate / 49000 + 0.5);
 if Oversampling < 1 then Oversampling := 1; 

 FHihatBufferLength := Oversampling * 20000;
 FKickBufferLength  := Oversampling * 20000;
 FSnareBufferLength := Oversampling * 60000;

 GetMem(FKickBuffer,     FKickBufferLength  * SizeOf(Single));
 GetMem(FSnareBuffer[0], FSnareBufferLength * SizeOf(Single));
 GetMem(FSnareBuffer[1], FSnareBufferLength * SizeOf(Single));
 GetMem(FHihatBuffer,    FHihatBufferLength * SizeOf(Single));

 //calcs here
 FHihatThreshold := Power(10, 2 * Parameter[0] - 2);
 FHihatDelay     := Round((0.04 + 0.2 * Parameter[1]) * SampleRate);
 FSnareThreshold := 40 * Power(10, 2 * Parameter[6] - 2);
 FSnareDelay     := Round(0.12 * SampleRate);
 FKickThreshold  := 220 * Power(10, 2 * Parameter[3] - 2);
 FKickDelay      := Round(0.1 * SampleRate);

 FHihatLevel := (1E-4 + sqr(Parameter[2]) * 4);
 FKickLevel  := (1E-4 + sqr(Parameter[5]) * 4);
 FSnareLevel := (1E-4 + sqr(Parameter[8]) * 4);

 FKWW     := Power(10, -3 + 2.2 * Parameter[4]);
 FKSF1    := cos(Pi * FKWW);    // p
 FKSF2    := sin(Pi * FKWW);    // q

 FWW      := Power(10, -3 + 2.2 * Parameter[7]);
 FSf1     := cos(Pi * FWW);     // p
 FSf2     := sin(Pi * FWW);     // q
 FSf3     := 0.991;             // r
 FSnareFX := 0;
 FKSFX    := 0;

 FRec     := 0;
 FRecx    := 0;
 FRecPos  := 0;

 FDyna    := Power(10, -1E4 / SampleRate);
 FDynr    := Power(10, -6   / SampleRate);
 FDyne    := 0;

 Synth;

 Parameter[ 0] := 0.3;  // Hat Thresh
 Parameter[ 1] := 0.45; // Hat rate
 Parameter[ 2] := 0.5;  // Hat Mix
 Parameter[ 3] := 0.46; // Kick Thresh
 Parameter[ 4] := 0.15; // Kick Key
 Parameter[ 5] := 0.5;  // Kick Mix
 Parameter[ 6] := 0.5;  // Snare Thresh
 Parameter[ 7] := 0.7;  // Snare Key
 Parameter[ 8] := 0.5;  // Snare Mix
 Parameter[ 9] := 0;    // Dynamics
 Parameter[10] := 0;    // Record
 Parameter[11] := 0;    // Thru Mix
end;

procedure TBeatBoxDataModule.VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FHihatThreshold := Power(10, 2 * Parameter[0] - 2);
 FHihatDelay     := Round((0.04 + 0.2 * Parameter[1]) * SampleRate);
 FSnareThreshold := 40 * Power(10, 2 * Parameter[6] - 2);
 FKickThreshold  := 220 * Power(10, 2 * Parameter[3] - 2);

 FHihatLevel     := 1E-4 + sqr(Parameter[2]) * 4;
 FKickLevel      := 1E-4 + sqr(Parameter[5]) * 4;
 FSnareLevel     := 1E-4 + sqr(Parameter[8]) * 4;

 FWWx            := FWW;
 FWW             := Power(10, -3 + 2.2 * Parameter[7]);
 FSf1            := cos(3.1415927 * FWW);     //p
 FSf2            := sin(3.1415927 * FWW);     //q
 FSnareFX        := 0;
 FKSFX           := 0;

 FKWWx           := FKWW;
 FKWW            := Power(10, -3 + 2.2 * Parameter[4]);
 FKSF1           := cos(3.1415927 * FKWW);     //p
 FKSF2           := sin(3.1415927 * FKWW);     //q

 if (FWWx  <> FWW)  then FSnareFX := Round(2 * SampleRate);
 if (FKWWx <> FKWW) then FKSFX    := Round(2 * SampleRate);

 FRec := Round(4.9 * Parameter[10]);
 if (FRec <> FRecx) and (FRecPos > 0) then //finish sample
  begin
   case FRec of
    2: while (FRecPos < FHihatBufferLength) do begin FHihatBuffer[FRecPos] := 0; inc(FRecPos); end;
    3: while (FRecPos < FKickBufferLength)  do begin FKickBuffer[FRecPos] := 0;  inc(FRecPos); end;
    4: while (FRecPos < FSnareBufferLength) do
        begin
         FSnareBuffer[0, FRecPos] := 0;
         FSnareBuffer[1, FRecPos] := 0;
         inc(FRecPos);
        end;
   end;
 end;

 FRecPos := 0;
 FRecx   := FRec;
end;

procedure TBeatBoxDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample           : Integer;
  a, b, e, o       : Single;
  hf, ht, mx3, mx1 : Single;
  hp, hl, hd       : Integer;
  kt               : Single;
  kp, kl, kd       : Integer;
  st, s, f1, f2,
  b1, b2, b3       : Single;
  k, kf1, kf2,
  kb1, kb2, mx4    : Single;
  hlv, klv, slv    : Single;
  sp, sl, sd       : Integer;
  ya, yr, ye, ym   : Single;
begin
 ht  := FHihatThreshold;
 hl  := FHihatBufferLength - 2;
 hd  := FHihatDelay;
 kt  := FKickThreshold;
 kl  := FKickBufferLength - 2;
 kd  := FKickDelay;
 st  := FSnareThreshold;
 mx3 := 0;
 hf  := FHfil;
 mx1 := FMix;
 hp  := FHBufPos;
 kp  := FKBufPos;
 f1  := FSb1;
 f2  := FSb2;
 kf1 := FKsb1;
 kf2 := FKsb2;
 b1  := FSf1;
 b2  := FSf2;
 b3  := FSf3;
 kb1 := FKSF1;
 kb2 := FKSF2;
 hlv := FHihatLevel;
 klv := FKickLevel;
 slv := FSnareLevel;
 sp  := FSnareBufferPos;
 sl  := FSnareBufferLength - 2;
 sd  := FSnareDelay;
 ya  := FDyna;
 yr  := FDynr;
 ye  := FDyne;
 ym  := FDynm;

 if (FSnareFX > 0) then
  begin
   mx3 := 0.08;
   slv := 0;
   klv := 0;
   hlv := 0;
   mx1 := 0;
   FSnareFX := FSnareFX - sampleFrames;
  end; //key listen (snare)

 if (FKSFX > 0) then
  begin
   mx3   := 0.03;
   slv   := 0;
   klv   := 0;
   hlv   := 0;
   mx1   := 0;
   FKSFX := FKSFX - sampleFrames;
   b1    := FKSF1;
   b2    := FKSF2;
  end; //key listen (kick)

if (FRec = 0) then
 for Sample := 0 to SampleFrames - 1 do
  begin
   a    := Inputs[0, Sample];
   b    := Inputs[1, Sample];
   e    := a + b;

   if e < ye
    then ye := ye * yr
    else ye := e - ya * (e - ye);  // dynamics envelope

   hf := e - hf;                   // high filter
   if ((hp > hd) and (hf > ht))
    then hp := 0
    else
     begin
      inc(hp);
      if (hp > hl)
       then hp := hl;
     end;
   o := hlv * FHihatBuffer[hp]; //hat

   k   := e + (kf1 * kb1) - (kf2 * kb2); //low filter
   kf2 := b3 * ((kf1 * kb2) + (kf2 * kb1));
   kf1 := b3 * k;
   if ((kp > kd) and (k > kt))
    then kp := 0
    else
     begin
      inc(kp);
      if (kp > kl) then kp := kl;
     end;

   o := o + klv * FKickBuffer[kp]; //kick

   s  := hf + (0.3 * e) + (f1 * b1) - (f2 * b2); //mid filter
   f2 := b3 * ((f1 * b2) + (f2 * b1));
   f1 := b3 * s;

   if ((sp > sd) and (s > st))
    then sp := 0
    else
     begin
      inc(sp);
      if (sp > sl) then sp := sl;
     end;

   mx4 := 1 + ym * (ye + ye - 1); //dynamics

   Outputs[0, Sample] := mx1 * a + mx3 * s + mx4 * (o + slv * FSnareBuffer[0, sp]);
   Outputs[1, Sample] := mx1 * a + mx3 * s + mx4 * (o + slv * FSnareBuffer[1, sp]);

   hf := e;
  end
else //record
 for Sample := 0 to SampleFrames - 1 do
  begin
   a := Inputs[0, Sample];
   b := Inputs[1, Sample];
   e := 0.5 * (a + b);

   if ((FRecPos = 0) and (abs(e) < 0.004))
    then e := 0
    else
     case FRec of
       2: begin
           if (FRecPos < hl)
            then FHihatBuffer[FRecPos] := e
            else e := 0;
           inc(FRecPos);
          end;
       3: begin
           if (FRecPos < kl)
            then FKickBuffer[FRecPos] := e
            else e := 0;
           inc(FRecPos);
          end;
       4: if (FRecPos < sl) then
           begin
            FSnareBuffer[0, FRecPos] := a;
            FSnareBuffer[1, FRecPos] := b;
            inc(FRecPos);
           end
          else e := 0;
     end;

   Outputs[0, Sample] := e;
   Outputs[1, Sample] := e;
  end;

 FHfil    := hf;
 FHBufPos := hp;
 FSBufPos := sp;
 FSb1     := f1;
 FSb2     := f2;
 FKBufPos := kp;
 FKsb1    := kf1;
 FKsb2    := kf2;
 FDyne    := ye;
end;

function fmod(Arg1, Arg2: Single): Single;
var
  Norm : Single;
begin
 Norm := Arg1 / Arg2;
 result := (Norm - Round(Norm - 0.5)) * Arg2
end;

procedure TBeatBoxDataModule.ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix := Value;
end;

procedure TBeatBoxDataModule.ParameterDynamicsChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDynm := Value;
end;

procedure TBeatBoxDataModule.Synth;
var
  t : Integer;
  e : Single;
  de, o, o1, o2, p, dp : Single;
begin
 e   := 0.00012;
 o1  := 0;
 o2  := 0;
 p   := 0.2;

 //generate hi-hat
 FillChar(FHihatBuffer^, FHihatBufferLength * SizeOf(Single), 0);
 de := Power(10, -36 / SampleRate);
 for t := 0 to FHihatBufferLength - 1 do
  begin
   o := 1000 * (random * 2 - 1);
   FHihatBuffer[t] :=  e * (2 * o1 - o2 - o);
   e  := e * de;
   o2 := o1;
   o1 := o;
  end;

 FillChar(FKickBuffer^, FKickBufferLength * SizeOf(Single), 0); //generate kick
 de := Power(10, -3.8 / SampleRate);
 e  := 0.5;
 dp := 1588 / SampleRate;
 for t := 0 to FKickBufferLength - 1 do
  begin
   FKickBuffer[t] :=  e * sin(p);
   e := e * de;
   p := fmod(p + dp * e, 2 * Pi);
  end;

 FillChar(FSnareBuffer[0]^, FSnareBufferLength * SizeOf(Single), 0); //generate snare
 FillChar(FSnareBuffer[1]^, FSnareBufferLength * SizeOf(Single), 0); //generate snare
 de := Power(10, -15.0 / SampleRate);
 e  := 0.38;
 dp := 1103 / SampleRate;
 for t := 0 to FSnareBufferLength - 1 do
  begin
   o := (0.3 * o) + 1000 * (2 * random - 1);
   FSnareBuffer[0, t] := (e * (sin(p) + 0.0004 * o));
   FSnareBuffer[1, t] := FSnareBuffer[0, t];
   e := e * de;
   p := fmod(p + 0.025, 2 * Pi);
  end;
end;

end.

(*
void mdaBeatBox::getParameterDisplay(VstInt32 index, char *text)
begin
  switch(index)
  begin
    case 0: float2strng((40.0*Parameter[0 - 40.0),text); break;
    case 1: long2string((long)(1000. * FHihatDelay / SampleRate),text); break;
    case 2: long2string((long)(20. * log10(FHihatLevel)),text); break;
    case 3: float2strng((40.0*Parameter[3 - 40.0),text); break;
    case 4: long2string((long)(0.5 * FKWW * SampleRate), text); break;
    case 5: long2string((long)(20. * log10(FKickLevel)),text); break;
    case 6: float2strng((40.0*Parameter[6 - 40.0),text); break;
    case 7: long2string((long)(0.5 * FWW * SampleRate), text); break;
    case 8: long2string((long)(20. * log10(FSnareLevel)),text); break;
    case 9: long2string((long)(100. * Parameter[9),text); break; 
    case 11: long2string((long)(20. * log10(Parameter[11)),text); break;

    case 10: switch(FRec) 
            begin case 0: strcpy(text, "-"); break;
              case 1: strcpy(text, "MONITOR"); break;
              case 2: strcpy(text, "-> HAT"); break;
              case 3: strcpy(text, "-> KIK"); break;
              case 4: strcpy(text, "-> SNR"); break; end; break;
  end;
end;

*)
