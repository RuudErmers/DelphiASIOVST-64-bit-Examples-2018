unit VocoderDM;

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
  TVocoderDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterModInDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterModInChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHiBandChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOutputDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamEnvelopeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamMidFreqDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamQualityDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FSwap     : Integer;
    FGain     : Single;
    FThru     : Single;
    FHigh     : Single;
    FNumBands : Integer;
    FKOut     : Single;
    FKVal     : Integer;
    FFreq     : array [0..15, 0..12] of Single;
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

procedure TVocoderDataModule.ParameterModInDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'RIGHT'
  else PreDefined := 'LEFT';
end;

procedure TVocoderDataModule.ParameterModInChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FSwap := Integer(Parameter[0] > 0.5);
end;

procedure TVocoderDataModule.ParameterHiBandChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FHigh := sqr(Value) * Value * Power(10, 0.5 + 2 * Parameter[1]);
end;

procedure TVocoderDataModule.ParamOutputDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(40.0 * Parameter[index] - 20.0, ffGeneral, 3, 3);
end;

procedure TVocoderDataModule.ParamEnvelopeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if (Parameter[index] < 0.05)
  then Predefined := 'FREEZE'
  else Predefined := FloatToStr(Power(10.0, 1 + 3 * Parameter[index]));
end;

procedure TVocoderDataModule.ParamMidFreqDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStr(800.0 * Power(2.0, 3.0 * Parameter[index] - 2.0));
end;

procedure TVocoderDataModule.ParamQualityDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if (FNumBands = 8)
  then PreDefined := '8 BAND'
  else PreDefined := '16 BAND';
end;

procedure TVocoderDataModule.VSTModuleOpen(Sender: TObject);
begin
 // Initial Parameters
 Parameter[0] := 0.33;  // Input Select
 Parameter[1] := 0.50;  // Output dB
 Parameter[2] := 0.40;  // Hi Thru
 Parameter[3] := 0.40;  // Hi Band
 Parameter[4] := 0.16;  // Envelope
 Parameter[5] := 0.55;  // Filter Q
 Parameter[6] := 0.6667;// FFreq Range
 Parameter[7] := 0.33;  // Num Bands

 // default preset
 with programs[0] do
  begin
   Parameter[0] := 0.33;
   Parameter[1] := 0.50;
   Parameter[2] := 0.40;
   Parameter[3] := 0.40;
   Parameter[4] := 0.16;
   Parameter[5] := 0.55;
   Parameter[6] := 0.6667;
   Parameter[7] := 0.33;
  end;

 // preset 1
 with programs[1] do
  begin
   Parameter[0] := 0.33;
   Parameter[1] := 0.50;
   Parameter[2] := 0.40;
   Parameter[3] := 0.40;
   Parameter[4] := 0.16;
   Parameter[5] := 0.55;
   Parameter[6] := 0.6667;
   Parameter[7] := 0.66;
  end;

 // preset 2
 with programs[2] do
  begin
   Parameter[0] := 0.33;
   Parameter[1] := 0.50;
   Parameter[2] := 0.00;
   Parameter[3] := 0.00;
   Parameter[4] := 0.16;
   Parameter[5] := 0.55;
   Parameter[6] := 0.50;
   Parameter[7] := 0.33;
  end;

 // preset 3
 with programs[3] do
  begin
   Parameter[0] := 0.33;
   Parameter[1] := 0.50;
   Parameter[2] := 0.40;
   Parameter[3] := 0.00;
   Parameter[4] := 0.16;
   Parameter[5] := 0.70;
   Parameter[6] := 0.50;
   Parameter[7] := 0.33;
  end;

 // preset 4
 with programs[4] do
  begin
   Parameter[0] := 0.33;
   Parameter[1] := 0.50;
   Parameter[2] := 0.40;
   Parameter[3] := 0.40;
   Parameter[4] := 0.78;
   Parameter[5] := 0.55;
   Parameter[6] := 0.30;
   Parameter[7] := 0.33;
  end;

// Suspend;
end;

procedure TVocoderDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample              : Integer;
  a, b, o, aa, bb     : Single;
  oo, g, ht, hh, tmp  : Single;
  i, k, sw, nb        : Integer;
begin
 o  := 0;
 oo := FKOut;
 g  := FGain;
 ht := FThru;
 hh := FHigh;
 k  := FKVal;
 sw := FSwap;
 nb := FNumBands;

 for Sample := 0 to SampleFrames - 1 do
  begin
   a := Inputs[    sw, Sample]; //speech
   b := Inputs[1 - sw, Sample]; //synth

   tmp := a - FFreq[0][7]; //integrate modulator for HF band and filter bank pre-emphasis
   FFreq[0][7] := a;
   a := tmp;

   tmp := abs(tmp);
   FFreq[0][11] := FFreq[0][11] - FFreq[0][12] * (FFreq[0][11] - tmp); // High band envelope
   o := FFreq[0][11] * (ht * a + hh * (b - FFreq[0][3]));             // High band + High Thru

   FFreq[0][3] := b; //integrate carrier for HF band

   inc(k);
   if (k and 1) > 0 then   // this block runs at half sample rate
    begin
     oo := 0.0;
     aa := a + FFreq[0][9] - FFreq[0][8] - FFreq[0][8];  //apply zeros here instead of in each reson
     FFreq[0][9] := FFreq[0][8];
     FFreq[0][8] := a;
     bb := b + FFreq[0][5] - FFreq[0][4] - FFreq[0][4];
     FFreq[0][5] := FFreq[0][4];
     FFreq[0][4] := b;

     for i := 1 to nb - 1 do //filter bank: 4th-order band pass
      begin
       tmp := FFreq[i][0] * FFreq[i][3] + FFreq[i][1] * FFreq[i][4] + bb;
       FFreq[i][4] := FFreq[i][3];
       FFreq[i][3] := tmp;
       tmp := tmp + FFreq[i][2] * FFreq[i][5] + FFreq[i][1] * FFreq[i][6];
       FFreq[i][6] := FFreq[i][5];
       FFreq[i][5] := tmp;

       tmp := FFreq[i][0] * FFreq[i][7] + FFreq[i][1] * FFreq[i][8] + aa;
       FFreq[i][8] := FFreq[i][7];
       FFreq[i][7] := tmp;
       tmp := tmp + FFreq[i][2] * FFreq[i][9] + FFreq[i][1] * FFreq[i][10];
       FFreq[i][10] := FFreq[i][ 9];
       FFreq[i][ 9] := tmp;

       tmp := abs(tmp);
       FFreq[i][11] := FFreq[i][11] - FFreq[i][12] * (FFreq[i][11] - tmp);
       oo := oo + FFreq[i][5] * FFreq[i][11];
      end;
    end;
   o := o + oo * g; //effect of interpolating back up to Fs would be minimal (aliasing >16kHz)

   Outputs[0, Sample] := o;
   Outputs[1, Sample] := o;
  end;

  FKOut := oo;
  FKVal := k and $1;
  if abs(FFreq[0][11]) < 1E-10
   then FFreq[0][11] := 0; //catch HF envelope denormal

  for i:=1 to nb - 1 do
   if (abs(FFreq[i][3]) < 1E-10) or (abs(FFreq[i][7]) < 1E-10) then
    for k := 3 to 11 do FFreq[i][k] := 0; //catch reson & envelope denormals

(*
  if (abs(o) > 10) then suspend; //catch instability
*)
end;

procedure TVocoderDataModule.VSTModuleResume(Sender: TObject);
var
  tpofs      : Double;
  rr, th, re : Double;
  sh         : Single;
  i          : Integer;
begin
 tpofs := 2 * Pi / SampleRate;

 FGain := Power(10, 2 * Parameter[1] - 3 * Parameter[5] - 2);

 FThru := Power(10, 0.5 + 2 * Parameter[1]);
 FHigh := sqr(Parameter[3]) * Parameter[3] * FThru;
 FThru := FThru * sqr(Parameter[2]) * Parameter[2];

 if (Parameter[7] < 0.5) then
  begin
   FNumBands := 8;
   re   := 0.003;
   FFreq[1][2] := 3000.0;
   FFreq[2][2] := 2200.0;
   FFreq[3][2] := 1500.0;
   FFreq[4][2] := 1080.0;
   FFreq[5][2] := 700.0;
   FFreq[6][2] := 390.0;
   FFreq[7][2] := 190.0;
  end
 else
  begin
   FNumBands := 16;
   re   := 0.0015;
   FFreq[ 1][2] := 5000.0;  // +1000
   FFreq[ 2][2] := 4000.0;  //  +750
   FFreq[ 3][2] := 3250.0;  //  +500
   FFreq[ 4][2] := 2750.0;  //  +450
   FFreq[ 5][2] := 2300.0;  //  +300
   FFreq[ 6][2] := 2000.0;  //  +250
   FFreq[ 7][2] := 1750.0;  //  +250
   FFreq[ 8][2] := 1500.0;  //  +250
   FFreq[ 9][2] := 1250.0;  //  +250
   FFreq[10][2] := 1000.0;  //  +250
   FFreq[11][2] :=  750.0;  //  +210
   FFreq[12][2] :=  540.0;  //  +190
   FFreq[13][2] :=  350.0;  //  +155
   FFreq[14][2] :=  195.0;  //  +100
   FFreq[15][2] :=   95.0;
  end;

 if (Parameter[4] < 0.05) // freeze
  then for i := 0 to FNumBands - 1 do FFreq[i][12] := 0
  else
   begin
    FFreq[0][12] := Power(10.0, -1.7 - 2.7 * Parameter[4]); //envelope speed

    rr := 0.022 / FNumBands; //minimum proportional to frequency to stop distortion
    for i := 1 to FNumBands - 1 do
     begin
      FFreq[i][12] := (0.025 - rr * i);
      if (FFreq[0][12] < FFreq[i][12])
       then FFreq[i][12] := FFreq[0][12];
     end;
    FFreq[0][12] := 0.5 * FFreq[0][12]; //only top band is at full rate
   end;

 rr := 1 - Power(10, -1 - 1.2 * Parameter[5]);
 sh := Power(2, 3 * Parameter[6] - 1); //filter bank range shift

 for i := 1 to FNumBands - 1 do
  begin
   FFreq[i][2] := FFreq[i][2] * sh;
   th          := arccos((2.0 * rr * cos(tpofs * FFreq[i][2])) / (1.0 + rr * rr));
   FFreq[i][0] := (2.0 * rr * cos(th)); //a0
   FFreq[i][1] := (-rr * rr);           //a1
                        //was 0.98
   FFreq[i][2] := FFreq[i][2] * 0.96; //shift 2nd stage slightly to stop high resonance peaks
   th          := arccos((2 * rr * cos(tpofs * FFreq[i][2])) / (1.0 + rr * rr));
   FFreq[i][2] := (2 * rr * cos(th));
  end;
end;

procedure TVocoderDataModule.VSTModuleSuspend(Sender: TObject);
var
  i, j : Integer;
begin
 for i := 0 to FNumBands - 1 do
  for j := 3 to 11 do
   FFreq[i][j] := 0; //zero band filters and envelopes
 FKOut := 0;
 FKVal := 0;
end;

end.
