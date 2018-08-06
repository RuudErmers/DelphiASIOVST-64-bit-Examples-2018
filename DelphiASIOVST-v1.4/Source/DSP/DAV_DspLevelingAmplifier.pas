unit DAV_DspLevelingAmplifier;

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

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_Classes, DAV_DspDynamics;

type
  TCustomLevelingAmplifier = class(TDspSampleRatePersistent, IDspProcessor64)
  private
    function GetGainReductiondB: Double;
    function GetInput_dB: Double;
    function GetOutput_dB: Double;
    procedure SetKnee(const Value: Double);
    procedure SetAttack_ms(const Value: Double);
    procedure SetInput_dB(const Value: Double);
    procedure SetOutput_dB(const Value: Double);
    procedure SetRelease_ms(const Value: Double);
    procedure SetThreshold(const Value: Double);
  protected
    FOldInput        : Double;
    FInputLevel      : Double;
    FOutputLevel     : Double;
    FPeak            : Double;
    FKnee            : Double;
    FGain            : Double;
    FRatio           : Double;
    FRatioReciprocal : Double;
    FSampleRateInv   : Double;
    FRelease_ms      : Double;
    FAttack_ms       : Double;
    FReleaseFactor   : Double;
    FAttackFactor    : Double;
    FThreshold       : Double;
    FThresholdReci   : Double;
    FMakeUpGain      : array [0..2] of Double;
//    FInternal        : array [0..3] of Double;
    procedure SetRatio(const Value: Double); virtual;
    procedure CalculateAttackFactor; virtual;
    procedure CalculateReleaseFactor; virtual;
    procedure CalculateInverseSampleRate;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;

    function TranslatePeakToGain(const PeakLevel: Double): Double; virtual;
    function CharacteristicCurve(const InputLevel: Double): Double; virtual;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; virtual;

    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input : Double): Double; virtual;
    procedure Sidechain(const Input : Double); virtual;
    procedure Reset;

    property GainReductionFactor : Double read FGain;            // in dB
    property GainReduction_dB : Double read GetGainReductiondB;  // in dB
    property Input_dB: Double read GetInput_dB write SetInput_dB;
    property Output_dB: Double read GetOutput_dB write SetOutput_dB;
    property Attack_ms: Double read FAttack_ms write SetAttack_ms;
    property Release_ms: Double read FRelease_ms write SetRelease_ms;
    property Ratio: Double read FRatio write SetRatio;
    property Threshold: Double read FThreshold write SetThreshold;
    property Knee: Double read FKnee write SetKnee;
  end;

  TLevelingAmplifier = class(TCustomLevelingAmplifier)
  published
    property Input_dB;
    property Output_dB;
    property Attack_ms;
    property Release_ms;
    property Ratio;
    property SampleRate;
    property Knee;
  end;

  TLevelingAmplifierProgramDependentRelease = class(TCustomLevelingAmplifier)
  private
    FMinReleaseFactor : Double;
    FMaxReleaseFactor : Double;
    FProgramDependency: Double;
    procedure SetProgramDependency(const Value: Double);
  protected
    procedure CalculateReleaseFactor; override;
  public
    constructor Create; override;
    procedure Sidechain(const Input : Double); override;
  published
    property Input_dB;
    property Output_dB;
    property Attack_ms;
    property Release_ms;
    property ProgramDependency: Double read FProgramDependency write SetProgramDependency;
    property Ratio;
    property SampleRate;
    property Knee;
  end;

implementation

uses
  Math, DAV_Common, DAV_Approximations;

const
  Harms : array [0..3] of Single = (1.4092750123e-07, -7.5166615806e-07,
                                    1.1523939535e-06, -6.3117489523e-07);

{ TCustomLevelingAmplifier }

constructor TCustomLevelingAmplifier.Create;
begin
  inherited;
  FRatio           := 1;
  FAttack_ms       := 5;
  FRelease_ms      := 5;
  FKnee            := 0.5;
  FRatio           := 1;
  FRatioReciprocal := 1;
  FThreshold       := 1;
  FThresholdReci   := 1;
  CalculateInverseSampleRate;
  CalculateAttackFactor;
  CalculateReleaseFactor;
  Reset;
end;

procedure TCustomLevelingAmplifier.CalculateInverseSampleRate;
begin
 FSampleRateInv := 1 / SampleRate;
end;

procedure TCustomLevelingAmplifier.CalculateAttackFactor;
begin
 if FAttack_ms = 0
  then FAttackFactor := 0
  else FAttackFactor := exp( -ln2 / (FAttack_ms * 0.001 * SampleRate));
end;

procedure TCustomLevelingAmplifier.CalculateReleaseFactor;
begin
 if FRelease_ms = 0
  then FReleaseFactor := 0
  else FReleaseFactor := exp( -ln2 / (FRelease_ms * 0.001 * SampleRate));
end;

function TCustomLevelingAmplifier.CharacteristicCurve(const InputLevel: Double): Double;
begin
 Result := TranslatePeakToGain(InputLevel) * InputLevel;
end;

function TCustomLevelingAmplifier.CharacteristicCurve_dB(const InputLevel_dB: Double): Double;
begin
 Result := Amp_to_dB(1E-30 + Abs(CharacteristicCurve(dB_to_Amp(InputLevel_dB))));
end;

function TCustomLevelingAmplifier.GetGainReductiondB: Double;
begin
 Result := Amp_to_dB(FGain);
end;

function TCustomLevelingAmplifier.GetInput_dB: Double;
begin
 Result := Amp_to_dB(FInputLevel);
end;

function TCustomLevelingAmplifier.GetOutput_dB: Double;
begin
 Result := Amp_to_dB(FOutputLevel);
end;

procedure TCustomLevelingAmplifier.SetAttack_ms(const Value: Double);
begin
 if FAttack_ms <> Abs(Value) then
  begin
   FAttack_ms := Abs(Value);
   CalculateAttackFactor;
  end;
end;

procedure TCustomLevelingAmplifier.SetInput_dB(const Value: Double);
begin
 FInputLevel := dB_to_Amp(Value);
end;

procedure TCustomLevelingAmplifier.SetKnee(const Value: Double);
begin
 if FKnee <> Value then
  begin
   FKnee := Value;
  end;
end;

procedure TCustomLevelingAmplifier.SetOutput_dB(const Value: Double);
begin
 FOutputLevel := dB_to_Amp(Value);
end;

procedure TCustomLevelingAmplifier.SetRatio(const Value: Double);
begin
 if FRatio <> Value then
  begin
   FRatio := Value;
   FRatioReciprocal := 1 / FRatio;
  end;
end;

procedure TCustomLevelingAmplifier.SetRelease_ms(const Value: Double);
begin
 if FRelease_ms <> Abs(Value) then
  begin
   FRelease_ms := Abs(Value);
   CalculateReleaseFactor;
  end;
end;

procedure TCustomLevelingAmplifier.SampleRateChanged;
begin
 CalculateInverseSampleRate;
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

procedure TCustomLevelingAmplifier.SetThreshold(const Value: Double);
begin
 if FThreshold <> Value then
  begin
   FThreshold := Value;
   FThresholdReci := 1 / Value;
  end;
end;

function Diode1(x: Single): Single;
var
  a, b : Double;
begin
 x := Abs(x - 0.4) + x - 0.4;
 a := Abs(x);
 b := 10 * (1 + Sqr(a));
 Result := 0.5 * (x + (b * x) / (b * a + 1));
end;

function Diode2(x: Single): Single;
var
  a, b : Double;
begin
 x := x * 10;
 x := Abs(x - 0.4) + x - 0.4;
 a := Abs(x);
 b := 10 * (1 + Sqr(a));
 Result := 0.005 * (x + (b * x) / (b * a + 1));
end;

function SimpleDiode(const x: Single): Single;
begin
 Result := 0.5 * (Abs(x) + x);
end;

procedure TCustomLevelingAmplifier.Sidechain(const Input: Double);
var
  Value : Double;
begin
 // apply feedback gain and input level gain
 Value := Input * FInputLevel;

 // smooth input by attack
 FOldInput := Abs(Value) + (FOldInput - Abs(Value)) * FAttackFactor;

 // add fall off (released) caused by electroluminicence effect of an LED
 FPeak := FPeak * FReleaseFactor;

 // calculate difference to current peak level
 Value := SimpleDiode(FOldInput - FPeak);

 // apply release phase
 FPeak := FPeak + Value;

 FGain := TranslatePeakToGain(FPeak);
end;

function TCustomLevelingAmplifier.TranslatePeakToGain(const PeakLevel: Double): Double;
var
  d : Single;
begin
 d := SimpleDiode(PeakLevel * FThresholdReci - Sqr(1 - Knee));
 d := d - Sqr(Knee) * FastTanhOpt3Term(d);
// Result := Power(1 + d, (FRatio - 1));
 Result := FastPower2MinError3(FastLog2ContinousError5(1 + d) * (FRatio - 1));
end;

procedure TCustomLevelingAmplifier.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleCount - 1 do
    Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TCustomLevelingAmplifier.ProcessSample64(Input: Double): Double;
begin
  Result := FInputLevel * (Harms[0] + Input * (1 + Input *
    (Harms[1] + Sqr(Input) * (Harms[2] + Sqr(Input) * Harms[3]))));
  Result := FOutputLevel * FGain * Result;
end;

procedure TCustomLevelingAmplifier.Reset;
begin
  FPeak := 0;
  FOldInput := 0;
end;


{ TLevelingAmplifierProgramDependentRelease }

procedure TLevelingAmplifierProgramDependentRelease.CalculateReleaseFactor;
begin
 inherited;
 FMinReleaseFactor := Power(FReleaseFactor,     FProgramDependency);
 FMaxReleaseFactor := Power(FReleaseFactor, 1 / FProgramDependency);
end;

constructor TLevelingAmplifierProgramDependentRelease.Create;
begin
 inherited;
 FProgramDependency := 2;
end;

procedure TLevelingAmplifierProgramDependentRelease.SetProgramDependency(
  const Value: Double);
begin
 if FProgramDependency <> Value then
  begin
   FProgramDependency := Value;
   CalculateReleaseFactor;
  end;
end;

procedure TLevelingAmplifierProgramDependentRelease.Sidechain(
  const Input: Double);
var
  Value : Double;
begin
 // apply feedback gain and input level gain
 Value := Input * FInputLevel;

 // smooth input by attack
 FOldInput := Abs(Value) + (FOldInput - Abs(Value)) * FAttackFactor;

 // add fall off (released) caused by electroluminicence effect of an LED
 FPeak := FPeak * FReleaseFactor;

 // calculate difference to current peak level
 Value := SimpleDiode(FOldInput - FPeak);

 // apply release phase
 FPeak := FPeak + Value;

 FGain := TranslatePeakToGain(FPeak);
end;

initialization
  RegisterDspProcessors64([TLevelingAmplifier, TLevelingAmplifierProgramDependentRelease]);

end.
