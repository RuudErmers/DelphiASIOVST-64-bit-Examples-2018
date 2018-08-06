unit RezFilterDM;

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
  TRezFilterDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLFORateDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTriggerDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FBuffer           : array [0..2] of Single;
    FFrequency        : Single;
    FQuality, FGain   : Single;
    FFreqMax          : Single;
    FFreqEnv          : Single;
    FEnv, FEnv2       : Single;
    FAtt, FRel, FPhi  : Single;
    FLFOMode          : Integer;
    FState, FFreqLFO  : Single;
    FDeltaPhi         : Single;
    FTrigger          : Integer;
    FTriggerAttack    : Integer;
    FTriggerThreshold : Single;
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

procedure TRezFilterDataModule.ParameterGainChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FGain  := 0.5 * dB_to_Amp(Value);
end;

procedure TRezFilterDataModule.ParameterReleaseDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF((-301.0301 / (SampleRate * log10(FRel))), ffGeneral, 3, 3);
end;

procedure TRezFilterDataModule.ParameterTriggerDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if (FTriggerThreshold = 0)
  then PreDefined := 'FREE RUN'
  else PreDefined := IntToStr(Round(20 * log10(0.5 * FTriggerThreshold)));
end;

procedure TRezFilterDataModule.ParameterLFORateDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(Power(10, 4 * Parameter[Index] - 2), ffGeneral, 3, 3);
end;

procedure TRezFilterDataModule.ParameterAttackDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(-301.0301 / (SampleRate * log10(1 - FAtt)), ffGeneral, 3, 3);
end;

procedure TRezFilterDataModule.VSTModuleOpen(Sender: TObject);
begin
 // Initial Parameters
 Parameter[0] := 33;   // Frequency [%]
 Parameter[1] := 70;   // Quality [%]
 Parameter[2] := 0;    // Amplification [dB]
 Parameter[3] := 85;   // Frequency Envelope [%]
 Parameter[4] := 0.00; // Attack
 Parameter[5] := 0.50; // Release
 Parameter[6] := 70;   // LFO  [%]
 Parameter[7] := 0.40; // Rate
 Parameter[8] := 0.00; // Trigger
 Parameter[9] := 75;   // Max. Frequency

 VSTModuleSuspend(Sender);
end;

procedure TRezFilterDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FFrequency := 1.5 * sqr(0.01 * Parameter[0]) - 0.15;
 FQuality  := 0.99 * Power(0.01 * Parameter[1], 0.3); // was 0.99 *

 FFreqMax := 0.99 + 0.3 * Parameter[1];
 if FFreqMax > (0.013 * Parameter[9])
  then FFreqMax := 0.013 * Parameter[9];

 FFreqEnv := abs(2 * sqr(0.005 * Parameter[3]));
 FAtt     := Power(10, -0.01 - 4.0 * Parameter[4]);
 FRel     := 1 - Power(10, -2.00 - 4.0 * Parameter[5]);

 FLFOMode := 0;
 FFreqLFO := 2 * sqr(0.005 * Parameter[6]);
 FDeltaPhi := (6.2832 * Power(10, 3 * Parameter[7] - 1.5) / SampleRate);
 if (Parameter[6] < 0) then
  begin
   FLFOMode  := 1;
   FDeltaPhi := 0.15915 * FDeltaPhi;
   FFreqLFO  := FFreqLFO * 0.001;
  end; //S&H

 if (Parameter[8] < 0.1)
  then FTriggerThreshold := 0
  else FTriggerThreshold := 3 * sqr(Parameter[8]);
end;

procedure TRezFilterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample: Integer;
  a, f, i, ff, fe     : Single;
  q, g, e, tmp        : Single;
  b0, b1, b2          : Single;
  at, re, fm, fl      : Single;
  dph, ph, bl, th, e2 : Single;
  lm, ta, tt          : Integer;
begin
  ff  := FFrequency;
  fe  := FFreqEnv;
  q   := FQuality;
  g   := FGain;
  e   := FEnv;
  at  := FAtt;
  re  := FRel;
  fm  := FFreqMax;
  fl  := FFreqLFO;
  dph := FDeltaPhi;
  th  := FTriggerThreshold;
  e2  := FEnv2;
  lm  := FLFOMode;
  ta  := FTriggerAttack;
  tt  := FTrigger;
  b0  := FBuffer[0];
  b1  := FBuffer[1];
  b2  := FBuffer[2];
  ph  := FPhi;
  bl  := FState;

  if th = 0 then
   begin
    for Sample := 0 to SampleFrames - 1 do
     begin
      a := Inputs[0, Sample] + Inputs[1, Sample];

      i := abs(a); //envelope
      if i > e
       then e := e + at * (i - e)
       else e := e * re;

      if lm = 0 // LFO
       then bl := fl * sin(ph) else
      if (ph > 1) then
       begin
        bl := fl * (random * 2000 - 1000);
        ph := 0;
       end;
      ph := ph + dph;

      f := ff + fe * e + bl; //freq
      if (f < 0)
       then i := 0 else
      if f > fm
       then i := fm
       else i := f;

      tmp := q + q * (1 + i * (1 + 1.1 * i));

      b0 := b0 + i * (g * a - b0 + tmp * (b0 - b1));
      b1 := b1 + i * (b0 - b1);
      Outputs[0, Sample] := b1;
      Outputs[1, Sample] := b1;
    end;
   end
  else
   begin
    for Sample := 0 to SampleFrames - 1 do
     begin
      a := Inputs[0, Sample] + Inputs[1, Sample];

      i := abs(a);  // Envelope

      if i > e
       then e := i
       else e := e * re;

      if (e > th) then
       begin
        if tt = 0 then
         begin
          ta :=1;
          if lm = 1
           then ph := 2
           else tt := 1;
         end
        else tt := 0;
       end;
      if ta = 1 then
       begin
        e2 := e2 + at * (1 - e2);
        if (e2 > 0.999) then ta := 0;
       end
      else e2 := e2 * re;

      if (lm = 0) then bl := fl * sin(ph) // LFO
      else if (ph > 1) then
       begin
        bl := fl * (random * 2000 - 1000);
        ph := 0;
       end;
      ph := ph + dph;

      f := ff + fe * e + bl;              // Freq
      if (f < 0) then i := 0 else
      if f > fm then i := fm else i := f;

      tmp := q + q * (1.0 + i * (1.0 + 1.1 * i));
      b0 := b0 + i * (g * a - b0 + tmp * (b0 - b1));
      b1 := b1 + i * (b0 - b1);

      Outputs[0, Sample] := b1;
      Outputs[1, Sample] := b1;
    end;
  end;

  if (abs(b0) < 1E-10) then
   begin
    FBuffer[0] := 0;
    FBuffer[1] := 0;
    FBuffer[2] := 0;
   end
  else
   begin
    FBuffer[0] := b0;
    FBuffer[1] := b1;
    FBuffer[2] := b2;
   end;

 FState         := bl;
 FPhi           := f_mod(ph, 2 * Pi);
 FEnv           := e;
 FEnv2          := e2;
 FTriggerAttack := ta;
 FTrigger       := tt;
end;

procedure TRezFilterDataModule.VSTModuleSuspend(Sender: TObject);
begin
 FBuffer[0] := 0;
 FBuffer[1] := 0;
 FBuffer[2] := 0;
end;

end.
