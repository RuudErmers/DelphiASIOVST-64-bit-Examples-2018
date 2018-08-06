unit DAV_DspLightweightDynamics;

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
//  License for the specific language governing rights AND limitations under  //
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

{$IFDEF CPUx86_64}
  {$DEFINE PUREPASCAL}
{$ENDIF}

uses
  DAV_Common, DAV_Classes, DAV_DspDynamics;

type
  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TLightweightSoftKneeLimiter                                             //
  //  ---------------------------                                             //
  //                                                                          //
  //  Lightweight soft knee limiter that uses approximations to obtain a      //
  //  controllable knee [in dB] around a given threshold.                     //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TLightweightSoftKneeLimiter = class(TCustomKneeLimiter)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
    procedure CalculateLogScaledThresholdValue;
  protected
    FThrshlddB  : Single;
    FKneeFactor : Single;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; override;
  public
    constructor Create; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample64(Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

  TLightweightSoftKneeFeedbackLikeLimiter = class(TCustomKneeLimiter)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
    procedure CalculateTimeFactors;
    procedure CalculateLogScaledThresholdValue;
  protected
    FThrshlddB          : Single;
    FKneeFactor         : Single;
    FAttackSampleCycle  : Single;
    FReleaseSampleCycle : Single;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; override;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
  public
    constructor Create; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample64(Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
    procedure Reset; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

  TLightweightSoftKneeCompressor = class(TCustomKneeCompressor)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
    procedure CalculateLogScaledThresholdValue;
    procedure CalculateRatioFactor;
  protected
    FRatioFactor : Single;
    FThrshlddB   : Single;
    FKneeFactor  : Single;
    procedure RatioChanged; override;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; override;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
  public
    constructor Create; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample64(Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

  TLightweightSoftKneeUpwardCompressor = class(TCustomKneeCompressor)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
    procedure CalculateLogScaledThresholdValue;
    procedure CalculateRatioFactor;
  protected
    FRatioFactor : Single;
    FThrshlddB   : Single;
    FKneeFactor  : Single;
    procedure RatioChanged; override;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; override;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
  public
    constructor Create; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample64(Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

  TLightweightSoftKneeFeedbackCompressor = class(TCustomKneeCompressor)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
    procedure CalculateLogScaledThresholdValue;
    procedure CalculateRatioFactor;
  protected
    FRatioFactor   : Single;
    FThrshlddB     : Single;
    FKneeFactor    : Single;
    FPrevAbsSample : Double;
    procedure RatioChanged; override;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; override;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
  public
    constructor Create; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample64(Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

  TLightweightSoftKneeFeedbackLikeCompressor = class(TCustomKneeCompressor)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
    procedure CalculateTimeFactors;
    procedure CalculateLogScaledThresholdValue;
    procedure CalculateRatioFactor;
  protected
    FRatioFactor        : Single;
    FThrshlddB          : Single;
    FKneeFactor         : Single;
    FAttackSampleCycle  : Single;
    FReleaseSampleCycle : Single;
    procedure RatioChanged; override;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; override;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
  public
    constructor Create; override;
    procedure Reset; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample64(Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

implementation

uses
  SysUtils, Math, DAV_Approximations;

resourcestring
  RCStrAttackTimeInvalid = 'Attack time must be larger than zero!';
  RCStrReleaseTimeInvalid = 'Release time must be larger than zero!';

const
  CSoftKnee : array [0..7] of Single = (-8.21343513178931783E-2,
    6.49732456739820052E-1, -2.13417801862571777, 4.08642207062728868,
    -1.51984215742349793, 5.48668824216034384E-2, 2.42162975514835621E-1,
    6.93292707161004662E-1);
  CDenorm32  : Single = 1E-24;
  CHalf32    : Single = 0.5;
  CQuarter32 : Single = 0.25;

{ TLightweightSoftKneeLimiter }

constructor TLightweightSoftKneeLimiter.Create;
begin
 inherited;
 CalculateLogScaledThresholdValue;
end;

procedure TLightweightSoftKneeLimiter.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeLimiter.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
 Changed;
end;

procedure TLightweightSoftKneeLimiter.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := -FThreshold_dB * -CHalf32;
 FMakeUpGain_dB := FastSqrtBab2(Sqr(Temp) + Sqr(FKnee_dB)) - Temp + CHalf32 * FKnee_dB;
 FMakeUpGain := FastdBtoAmpMinError3(FMakeUpGain_dB);
end;

procedure TLightweightSoftKneeLimiter.CalculateKneeFactor;
begin
 FKneeFactor := Sqr(2 * CdBtoAmpExpGain32 * FKnee_dB);
end;

procedure TLightweightSoftKneeLimiter.ThresholdChanged;
begin
 inherited;
 CalculateLogScaledThresholdValue;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeLimiter.CalculateLogScaledThresholdValue;
begin
 FThrshlddB := Threshold_dB * CdBtoAmpExpGain32;
end;

function TLightweightSoftKneeLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 Result := PeakLevel;
 Result := FastLog2ContinousError5(Result) - FThrshlddB;
 Result := FastPower2MinError3(-CHalf32 * (Result + FastSqrtBab2(Sqr(Result) + FKneeFactor)));
end;

function TLightweightSoftKneeLimiter.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := -CHalf32 * (InputLevel_dB - FThreshold_dB);
 Result := Temp - FastSqrtBab2(Sqr(Temp) + Sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

function TLightweightSoftKneeLimiter.GainSample(const Input: Double): Double;
begin
 Result := FGain * FMakeUpGain * Input;
end;

procedure TLightweightSoftKneeLimiter.InputSample(const Input: Double);
{$IFDEF PUREPASCAL}
var
  Temp : Single;
begin
 Temp := CDenorm32 + abs(Input);

 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 Temp  := FThrshlddB - FastLog2ContinousError5(FPeak);
 FGain := FastPower2MinError3(CHalf32 * (Temp - FastSqrtBab2(Sqr(Temp) + FKneeFactor)));
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
    // Temp := CDenorm32 + abs(Input);
    MOV     EDX, EAX               // EDX = Self
    FLD     Input
    FABS
    FADD    CDenorm32              // Stack: temp

    FCOM    [EDX.FPeak].Double     // Stack: temp
    FSTSW   AX
    SAHF
    JBE     @Release
@Attack:
    FSUB    [EDX.FPeak]
    FMUL    [EDX.FAttackFactor]
    FADD    [EDX.FPeak]
    FST     [EDX.FPeak]
    JMP     @AmpTodB
@Release:
    FLD     [EDX.FPeak]             // Stack: FPeak, temp
    FSUB    ST(0), ST(1)            // Stack: (FPeak - temp), temp
    FMUL    [EDX.FReleaseFactor]    // Stack: (FPeak - temp) * FReleaseFactor, temp
    FADDP                           // Stack: (FPeak - temp) * FReleaseFactor + temp
    FST     [EDX.FPeak]

@AmpTodB:
    FSTP    IntCast                 // Stack: (empty)
    MOV     EAX, IntCast
    MOV     ECX, EAX                // copy EAX to ECX
    AND     EAX, $807fffff
    ADD     EAX, $3f800000
    MOV     IntCast, EAX
    FLD     CastedSingle
    FMUL    [CSoftKnee        ].Single
    FADD    [CSoftKnee + 4    ].Single
    FMUL    CastedSingle
    FADD    [CSoftKnee + 4 * 2].Single
    FMUL    CastedSingle
    FADD    [CSoftKnee + 4 * 3].Single
    FMUL    CastedSingle
    FADD    [CSoftKnee + 4 * 4].Single

    SHR     ECX, $17
    AND     ECX, $000000ff
    SUB     ECX, $00000080
    MOV     IntCast, ECX
    FILD    IntCast
    FADDP

    FSUBR   [EDX.FThrshlddB]        // Stack : Temp

    // FGain := FastPower2MinError3(CHalf32 * (FMkpdB - Temp - FastSqrtBab2(Sqr(Temp) + FKneeFactor)));
    FLD     ST(0)                   // Stack : Temp, Temp
    FMUL    ST(0), ST(0)
    FADD    [EDX.FKneeFactor]       // Stack : Temp * Temp + FKneeFactor, Temp
    FLD     ST(0)                   // Stack : Intemp, Intemp, Temp
    FST     CastedSingle            // Stack : Intemp, Intemp, Temp

    MOV     EAX, IntCast
    SUB     EAX, $00800000
    SHR     EAX, 1
    ADD     EAX, $20000000
    MOV     IntCast, EAX
    FDIV    CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp
    FADD    CastedSingle            // Stack: newResult = CastedSingle + Intemp / CastedSingle, Intemp, Temp
    FLD     ST(0)                   // Stack: newResult, newResult, Intemp, Temp
    FMUL    CQuarter32              // Stack: CQuarter32 * newResult, newResult, Intemp, Temp
    FXCH                            // Stack: newResult, CQuarter32 * newResult, Intemp, Temp
    FDIVP   ST(2), ST(0)            // Stack: Intemp / newResult, CQuarter32 * newResult, Temp
    FADDP                           // Stack: Intemp / newResult + CQuarter32 * newResult, Temp

    FSUBP                           // Stack: Temp + SqrtTemp
    FMUL    CHalf32                 // Stack: CHalf32 * (FMkpdB - (Temp + SqrtTemp))

    FLD     ST(0)                   // Stack: temp, temp
    FRNDINT                         // Stack: round(temp), temp

    FIST    IntCast                 // Stack: round(temp), temp
    FSUBP                           // Stack: newtemp = temp - round(temp)

    MOV     EAX, IntCast
    ADD     EAX, $7F
    SHL     EAX, $17
    MOV     IntCast, EAX

    FLD     ST(0)                      // Stack: newtemp, newtemp
    FMUL    [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp
    FADD    [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp
    FMUL    ST(0), ST(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
    FADD    [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
    FMULP                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
    FLD1
    FADDP                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
    FMUL    CastedSingle

    FSTP    [EDX.FGain]
end;
{$ENDIF}

function TLightweightSoftKneeLimiter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  Temp : Single;
begin
 Temp := CDenorm32 + abs(Input);

 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 Temp  := FThrshlddB - FastLog2ContinousError5(FPeak);
 FGain := FastPower2MinError3(CHalf32 * (Temp - FastSqrtBab2(Sqr(Temp) + FKneeFactor)));

 Result := FGain * FMakeUpGain * Input;
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
    // Temp := CDenorm32 + abs(Input);
    MOV     EDX, EAX               // EDX = Self
    FLD     Input
    FABS
    FADD    CDenorm32              // Stack: temp

    FCOM    [EDX.FPeak].Double     // Stack: temp
    FSTSW   AX
    SAHF
    JBE     @Release
@Attack:
    FSUB    [EDX.FPeak]
    FMUL    [EDX.FAttackFactor]
    FADD    [EDX.FPeak]
    FST     [EDX.FPeak]
    JMP     @AmpTodB
@Release:
    FLD     [EDX.FPeak]            // Stack: FPeak, temp
    FSUB    ST(0), ST(1)           // Stack: (FPeak - temp), temp
    FMUL    [EDX.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
    FADDP                          // Stack: (FPeak - temp) * FReleaseFactor + temp
    FST     [EDX.FPeak]

@AmpTodB:
    FSTP    IntCast                // Stack: (empty)
    MOV     EAX, IntCast
    MOV     ECX, EAX               // copy EAX to ECX
    AND     EAX, $807FFFFF
    ADD     EAX, $3F800000
    MOV     IntCast, EAX
    FLD     CastedSingle
    FMUL    [CSoftKnee        ].Single
    FADD    [CSoftKnee + 4    ].Single
    FMUL    CastedSingle
    FADD    [CSoftKnee + 4 * 2].Single
    FMUL    CastedSingle
    FADD    [CSoftKnee + 4 * 3].Single
    FMUL    CastedSingle
    FADD    [CSoftKnee + 4 * 4].Single

    SHR     ECX, $17
    AND     ECX, $000000FF
    SUB     ECX, $00000080
    MOV     IntCast, ECX
    FILD    IntCast
    FADDP


    FSUBR   [EDX.FThrshlddB]       // Stack : Temp

    // FGain := FastPower2MinError3(CHalf32 * (FMkpdB - Temp - FastSqrtBab2(Sqr(Temp) + FKneeFactor)));
    FLD     ST(0)                   // Stack : Temp, Temp
    FMUL    ST(0), ST(0)
    FADD    [EDX.FKneeFactor]       // Stack : Temp * Temp + FKneeFactor, Temp
    FLD     ST(0)                   // Stack : Intemp, Intemp, Temp
    FST     CastedSingle            // Stack : Intemp, Intemp, Temp

    MOV     EAX, IntCast
    SUB     EAX, $00800000
    SHR     EAX, 1
    ADD     EAX, $20000000
    MOV     IntCast, EAX
    FDIV    CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp
    FADD    CastedSingle            // Stack: newResult = CastedSingle + Intemp / CastedSingle, Intemp, Temp
    FLD     ST(0)                   // Stack: newResult, newResult, Intemp, Temp
    FMUL    CQuarter32              // Stack: CQuarter32 * newResult, newResult, Intemp, Temp
    FXCH                            // Stack: newResult, CQuarter32 * newResult, Intemp, Temp
    FDIVP   ST(2), ST(0)            // Stack: Intemp / newResult, CQuarter32 * newResult, Temp
    FADDP                           // Stack: Intemp / newResult + CQuarter32 * newResult, Temp

    FSUBP                           // Stack: Temp + SqrtTemp
    FMUL    CHalf32                 // Stack: CHalf32 * (FMkpdB - (Temp + SqrtTemp))

    FLD     ST(0)                   // Stack: temp, temp
    FRNDINT                         // Stack: round(temp), temp

    FIST    IntCast                 // Stack: round(temp), temp
    FSUBP                           // Stack: newtemp = temp - round(temp)

    MOV     EAX, IntCast
    ADD     EAX, $7F
    SHL     EAX, $17
    MOV     IntCast, EAX

    FLD     ST(0)                      // Stack: newtemp, newtemp
    FMUL    [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp
    FADD    [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp
    FMUL    ST(0), ST(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
    FADD    [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
    FMULP                              // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
    FLD1
    FADDP                              // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
    FMUL    CastedSingle

    FST     [EDX.FGain]
    FMUL    Input
    FMUL    [EDX.FMakeUpGain]
end;
{$ENDIF}


{ TLightweightSoftKneeFeedbackLikeLimiter }

constructor TLightweightSoftKneeFeedbackLikeLimiter.Create;
begin
 inherited;
 CalculateLogScaledThresholdValue;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.CalculateAttackFactor;
begin
 if FAttack <= 0
  then raise Exception.Create('Attack time must be larger than zero!')
  else
   begin
    FAttackSampleCycle := -1 / (FAttack * 0.001 * SampleRate);
    FAttackFactor := 1 - FastPower2MinError3(FAttackSampleCycle);
   end;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.CalculateReleaseFactor;
begin
 if FRelease <= 0
  then raise Exception.Create('Release time must be larger than zero!')
  else
   begin
    FReleaseSampleCycle := -1 / (FRelease * 0.001 * SampleRate);
    FReleaseFactor := FastPower2MinError3(FReleaseSampleCycle);
   end;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.Reset;
begin
 inherited;
 CalculateTimeFactors;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.CalculateTimeFactors;
begin
 FAttackFactor := 1 - FastPower2MinError3(FAttackSampleCycle);
 FReleaseFactor := FastPower2MinError3(FReleaseSampleCycle);
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain
  else //CalculateMakeUpGain;
 Changed;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := FThreshold_dB * CHalf32;
 FMakeUpGain_dB := FastSqrtBab2(Sqr(Temp) + Sqr(FKnee_dB)) - Temp;
// CalculateMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.CalculateKneeFactor;
begin
 FKneeFactor := Sqr(CdBtoAmpExpGain32 * FKnee_dB);
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.CalculateLogScaledThresholdValue;
begin
 FThrshlddB := Threshold_dB * CdBtoAmpExpGain32;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.ThresholdChanged;
begin
 inherited;
 CalculateLogScaledThresholdValue;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

function TLightweightSoftKneeFeedbackLikeLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 Result := PeakLevel;
 Result := CHalf32 * (FThrshlddB - FastLog2ContinousError5(Result));
 Result := FastPower2MinError3(Result - FastSqrtBab2(Sqr(Result) + FKneeFactor));
end;

function TLightweightSoftKneeFeedbackLikeLimiter.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := CHalf32 * (FThreshold_dB - InputLevel_dB);
 Result := Temp - FastSqrtBab2(Sqr(Temp) + Sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

function TLightweightSoftKneeFeedbackLikeLimiter.GainSample(const Input: Double): Double;
begin
 Result := FGain * FMakeUpGain * Input;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.InputSample(const Input: Double);
{$IFNDEF XPUREPASCAL}
var
  Temp  : array [0..1] of Single;
begin
 Temp[0] := CDenorm32 + abs(Input);

 if Temp[0] > FPeak
  then FPeak := FPeak + (Temp[0] - FPeak) * FAttackFactor
  else FPeak := Temp[0] + (FPeak - Temp[0]) * FReleaseFactor;

 Temp[0] := -CHalf32 * (FastLog2ContinousError5(FPeak) - FThrshlddB);  // * FRatioFactor ???, PLEASE CHECK!!!
 Temp[1] := FastSqrtBab2(Sqr(Temp[0]) + FKneeFactor);
 FGain   := FastPower2MinError3(Temp[0] - Temp[1]);
// Temp[1] := 2 * Temp[1] / (2 * Temp[1] - 1E10 * (1 / 1E10 - 1) * Temp[1] - Ratio * (1 / FRatio - 1) * Temp[0]);
 Temp[1] := 2 * Temp[1] / ((1E10 + 1) * Temp[1] + (1E10 - 1) * Temp[0]);
 FAttackFactor := 1 - FastPower2MinError3(Temp[1] * FAttackSampleCycle);
 FReleaseFactor := FastPower2MinError3(Temp[1] * FReleaseSampleCycle);
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
    // Temp := CDenorm32 + abs(Input);
    MOV     EDX, EAX               // EDX = Self
    FLD     Input
    FABS
    FADD    CDenorm32              // Stack: temp

    FCOM    [EDX.FPeak].Double     // Stack: temp
    FSTSW   AX
    SAHF
    JBE     @Release
@Attack:
    FSUB    [EDX.FPeak]
    FMUL    [EDX.FAttackFactor]
    FADD    [EDX.FPeak]
    FST     [EDX.FPeak]
    JMP     @AmpTodB
@Release:
    FLD     [EDX.FPeak]            // Stack: FPeak, temp
    FSUB    ST(0), ST(1)           // Stack: (FPeak - temp), temp
    FMUL    [EDX.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
    FADDP                          // Stack: (FPeak - temp) * FReleaseFactor + temp
    FST     [EDX.FPeak]

@AmpTodB:
    FSTP    IntCast                // Stack: (empty)
    MOV     EAX, IntCast
    MOV     ECX, EAX               // copy EAX to ECX
    AND     EAX, $807fffff
    ADD     EAX, $3f800000
    MOV     IntCast, EAX
    FLD     CastedSingle
    FMUL    [CSoftKnee        ].Single
    FADD    [CSoftKnee + 4    ].Single
    FMUL    CastedSingle
    FADD    [CSoftKnee + 4 * 2].Single
    FMUL    CastedSingle
    FADD    [CSoftKnee + 4 * 3].Single
    FMUL    CastedSingle
    FADD    [CSoftKnee + 4 * 4].Single

    SHR     ECX, $17
    AND     ECX, $000000ff
    SUB     ECX, $00000080
    MOV     IntCast, ECX
    FILD    IntCast
    FADDP


    FSUB    [EDX.FThrshlddB]       // Stack : Temp
    FMUL    [EDX.FRatioFactor]     // Stack : Temp[0]

    // Temp[1] := FastSqrtBab2(Sqr(Temp[0]) + FKneeFactor);
    FLD     ST(0)                   // Stack : Temp[0], Temp[0]
    FMUL    ST(0), ST(0)
    FADD    [EDX.FKneeFactor]       // Stack : Temp[0] * Temp[0] + FKneeFactor, Temp[0]
    FLD     ST(0)                   // Stack : Intemp, Intemp, Temp[0]
    FST     CastedSingle            // Stack : Intemp, Intemp, Temp[0]

    MOV     EAX, IntCast
    SUB     EAX, $00800000
    SHR     EAX, 1
    ADD     EAX, $20000000
    MOV     IntCast, EAX
    FDIV    CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp[0]
    FADD    CastedSingle            // Stack: newResult = CastedSingle + Intemp / CastedSingle, Intemp, Temp[0]
    FLD     ST(0)                   // Stack: newResult, newResult, Intemp, Temp[0]
    FMUL    CQuarter32              // Stack: CQuarter32 * newResult, newResult, Intemp, Temp[0]
    FXCH                            // Stack: newResult, CQuarter32 * newResult, Intemp, Temp[0]
    FDIVP   ST(2), ST(0)            // Stack: Intemp / newResult, CQuarter32 * newResult, Temp[0]
    FADDP                           // Stack: Temp[1] := Intemp / newResult + CQuarter32 * newResult, Temp[0]

    // FGain   := FastPower2MinError3(Temp[0] - Temp[1]);
    FLD     ST(0)                   // Stack: Temp[1], Temp[1], Temp[0]
    FSUBR   ST(0), ST(2)            // Stack: Temp[0] - Temp[1], Temp[1], Temp[0]

    FLD     ST(0)                   // Stack: Temp[0] - Temp[1], Temp[0] - Temp[1], Temp[1], Temp[0]
    FRNDINT                         // Stack: round(Temp[0] - Temp[1]), Temp[0] - Temp[1], Temp[1], Temp[0]

    FIST    IntCast                 // Stack: round(Temp[0] - Temp[1]), Temp[0] - Temp[1], Temp[1], Temp[0]
    FSUBP                           // Stack: newtemp = (Temp[0] - Temp[1]) - round(Temp[0] - Temp[1]), Temp[1], Temp[0]

    MOV     EAX, IntCast
    ADD     EAX, $7F
    SHL     EAX, $17
    MOV     IntCast, EAX

    FLD     ST(0)                      // Stack: newtemp, newtemp, Temp[1], Temp[0]
    FMUL    [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp, Temp[1], Temp[0]
    FADD    [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp, Temp[1], Temp[0]
    FMUL    ST(0), ST(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[1], Temp[0]
    FADD    [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[1], Temp[0]
    FMULP                              // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[1], Temp[0]
    FLD1
    FADDP                              // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[1], Temp[0]
    FMUL    CastedSingle               // NewGain, Temp[1], Temp[0]
    FSTP    [EDX.FGain]

    // Temp[1] := 2 * Temp[1] / ((FRatio + 1) * Temp[1] + (FRatio - 1) * Temp[0]);
    FLD1                               // 1, Temp[1], Temp[0]
    FADD    [EDX.FRatio]               // Ratio + 1, Temp[1], Temp[0]
    FMUL    ST(0), ST(1)               // Temp[1] * (Ratio + 1), Temp[1], Temp[0]
    FXCH    ST(2)                      // Temp[0], Temp[1], Temp[1] * (Ratio + 1)
    FLD1                               // 1, Temp[0], Temp[1], Temp[1] * (Ratio + 1)
    FSUBR   [EDX.FRatio]               // Ratio - 1, Temp[0], Temp[1], Temp[1] * (Ratio + 1)
    FMULP                              // (Ratio - 1) * Temp[0], Temp[1], Temp[1] * (Ratio + 1)
    FADDP   ST(2), ST(0)               // Temp[1], (Ratio - 1) * Temp[0] + Temp[1] * (Ratio + 1),
    FDIVRP                             // Temp[1] / (Ratio - 1) * Temp[0] + Temp[1] * (Ratio + 1)
    FADD    ST(0), ST(0)               // 2 * Temp[1] / (Ratio - 1) * Temp[0] + Temp[1] * (Ratio + 1)

    // FAttackFactor := 1 - FastPower2MinError3(Temp[1] * FAttackSampleCycle);
    FLD     ST(0)                      // Temp[1], Temp[1]
    FMUL    [EDX.FAttackSampleCycle]   // Temp[0], Temp[1]

    FLD     ST(0)                      // Stack: Temp[0], Temp[0], Temp[1]
    FRNDINT                            // Stack: round(Temp[0]), Temp[0], Temp[1]

    FIST    IntCast                    // Stack: round(Temp[0]), Temp[0], Temp[1]
    FSUBP                              // Stack: newtemp = Temp[0] - round(Temp[0], Temp[0], Temp[1]

    MOV     EAX, IntCast
    ADD     EAX, $7F
    SHL     EAX, $17
    MOV     IntCast, EAX

    FLD     ST(0)                      // Stack: newtemp, newtemp, Temp[0], Temp[1]
    FMUL    [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp, Temp[0], Temp[1]
    FADD    [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp, Temp[0], Temp[1]
    FMUL    ST(0), ST(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0], Temp[1]
    FADD    [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0], Temp[1]
    FMULP                              // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0], Temp[1]
    FLD1
    FADDP                              // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0], Temp[1]
    FMUL    CastedSingle               // NewAttackFactor, Temp[1]
    FLD1                               // 1, NewAttackFactor, Temp[1]
    FSUBRP                             // 1 - NewAttackFactor, Temp[1]
    FSTP    [EDX.FAttackFactor]

    // FReleaseFactor := FastPower2MinError3(Temp[1] * FReleaseSampleCycle);
    FMUL    [EDX.FReleaseSampleCycle]  // Temp[0]

    FLD     ST(0)                      // Stack: Temp[0], Temp[0]
    FRNDINT                            // Stack: round(Temp[0]), Temp[0]

    FIST    IntCast                    // Stack: round(Temp[0]), Temp[0]
    FSUBP                              // Stack: newtemp = Temp[0] - round(Temp[0], Temp[0]

    MOV     EAX, IntCast
    ADD     EAX, $7F
    SHL     EAX, $17
    MOV     IntCast, EAX

    FLD     ST(0)                      // Stack: newtemp, newtemp, Temp[0]
    FMUL    [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp, Temp[0]
    FADD    [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp, Temp[0]
    FMUL    ST(0), ST(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0]
    FADD    [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0]
    FMULP                              // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0]
    FLD1
    FADDP                              // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0]
    FMUL    CastedSingle               // NewReleaseFactor
    FSTP    [EDX.FReleaseFactor]
end;
{$ENDIF}

function TLightweightSoftKneeFeedbackLikeLimiter.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 Result := FGain * FMakeUpGain * Input;
end;


{ TLightweightSoftKneeCompressor }

constructor TLightweightSoftKneeCompressor.Create;
begin
 inherited;
 CalculateLogScaledThresholdValue;
 CalculateRatioFactor;
end;

procedure TLightweightSoftKneeCompressor.CalculateAttackFactor;
begin
 if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - FastPower2MinError3( -1 / (FAttack * 0.001 * SampleRate));
end;

procedure TLightweightSoftKneeCompressor.CalculateReleaseFactor;
begin
 if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := FastPower2MinError3(-1 / (FRelease * 0.001 * SampleRate));
end;

procedure TLightweightSoftKneeCompressor.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeCompressor.RatioChanged;
begin
 inherited;
 CalculateRatioFactor;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeCompressor.CalculateRatioFactor;
begin
 FRatioFactor := CHalf32 * (1 / Ratio - 1);
end;

procedure TLightweightSoftKneeCompressor.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain
  else CalculateMakeUpGain;
 Changed;
end;

procedure TLightweightSoftKneeCompressor.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := -FThreshold_dB * FRatioFactor;
 FMakeUpGain_dB := FastSqrtBab2(Sqr(Temp) + Sqr(FKnee_dB)) - Temp;
 CalculateMakeUpGain;
end;

procedure TLightweightSoftKneeCompressor.CalculateKneeFactor;
begin
 FKneeFactor := Sqr(CdBtoAmpExpGain32 * FKnee_dB);
end;

procedure TLightweightSoftKneeCompressor.ThresholdChanged;
begin
 inherited;
 CalculateLogScaledThresholdValue;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeCompressor.CalculateLogScaledThresholdValue;
begin
 FThrshlddB := Threshold_dB * CdBtoAmpExpGain32;
end;

function TLightweightSoftKneeCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 Result := PeakLevel;
 Result := FRatioFactor * (FastLog2ContinousError5(Result) - FThrshlddB);
 Result := FastPower2MinError3(Result - FastSqrtBab2(Sqr(Result) + FKneeFactor));
end;

function TLightweightSoftKneeCompressor.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := FRatioFactor * (InputLevel_dB - FThreshold_dB);
 Result := Temp - FastSqrtBab2(Sqr(Temp) + Sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

function TLightweightSoftKneeCompressor.GainSample(const Input: Double): Double;
begin
 Result := FGain * FMakeUpGain * Input;
end;

procedure TLightweightSoftKneeCompressor.InputSample(const Input: Double);
{$IFDEF PUREPASCAL}
var
  Temp : Single;
begin
 Temp := CDenorm32 + abs(Input);

 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 Temp  := FRatioFactor * (FastLog2ContinousError5(FPeak) - FThrshlddB);
 FGain := FastPower2MinError3(Temp - FastSqrtBab2(Sqr(Temp) + FKneeFactor));
 Assert(not IsNan(FGain));
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
    // Temp := CDenorm32 + abs(Input);
    MOV   EDX, EAX               // EDX = Self
    FLD   Input
    FABS
    FADD  CDenorm32              // Stack: temp

    FCOM  [EDX.FPeak].Double     // Stack: temp
    FSTSW AX
    SAHF
    JBE   @Release
@Attack:
    FSUB  [EDX.FPeak]
    FMUL  [EDX.FAttackFactor]
    FADD  [EDX.FPeak]
    FST   [EDX.FPeak]
    JMP   @AmpTodB
@Release:
    FLD   [EDX.FPeak]            // Stack: FPeak, temp
    FSUB  ST(0), ST(1)           // Stack: (FPeak - temp), temp
    FMUL  [EDX.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
    FADDP                        // Stack: (FPeak - temp) * FReleaseFactor + temp
    FST   [EDX.FPeak]

@AmpTodB:
    FSTP  IntCast                // Stack: (empty)
    MOV   EAX, IntCast
    MOV   ECX, EAX               // copy EAX to ECX
    AND   EAX, $807fffff
    ADD   EAX, $3f800000
    MOV   IntCast, EAX
    FLD   CastedSingle
    FMUL  [CSoftKnee        ].Single
    FADD  [CSoftKnee + 4    ].Single
    FMUL  CastedSingle
    FADD  [CSoftKnee + 4 * 2].Single
    FMUL  CastedSingle
    FADD  [CSoftKnee + 4 * 3].Single
    FMUL  CastedSingle
    FADD  [CSoftKnee + 4 * 4].Single

    SHR   ECX, $17
    AND   ECX, $000000ff
    SUB   ECX, $00000080
    MOV   IntCast, ECX
    FILD  IntCast
    FADDP


    FSUB  [EDX.FThrshlddB]       // Stack : Temp
    FMUL  [EDX.FRatioFactor]

    // FGain := FastPower2MinError3(CHalf32 * (FMkpdB - Temp - FastSqrtBab2(Sqr(Temp) + FKneeFactor)));
    FLD   ST(0)                   // Stack : Temp, Temp
    FMUL  ST(0), ST(0)
    FADD  [EDX.FKneeFactor]       // Stack : Temp * Temp + FKneeFactor, Temp
    FLD   ST(0)                   // Stack : Intemp, Intemp, Temp
    FST   CastedSingle            // Stack : Intemp, Intemp, Temp

    MOV   EAX, IntCast
    SUB   EAX, $00800000
    SHR   EAX, 1
    ADD   EAX, $20000000
    MOV   IntCast, EAX
    FDIV  CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp
    FADD  CastedSingle            // Stack: newResult = CastedSingle + Intemp / CastedSingle, Intemp, Temp
    FLD   ST(0)                   // Stack: newResult, newResult, Intemp, Temp
    FMUL  CQuarter32              // Stack: CQuarter32 * newResult, newResult, Intemp, Temp
    FXCH                          // Stack: newResult, CQuarter32 * newResult, Intemp, Temp
    FDIVP ST(2), ST(0)            // Stack: Intemp / newResult, CQuarter32 * newResult, Temp
    FADDP                         // Stack: Intemp / newResult + CQuarter32 * newResult, Temp

    FSUBP                         // Stack: Temp + SqrtTemp

    FLD   ST(0)                   // Stack: temp, temp
    FRNDINT                       // Stack: round(temp), temp

    FIST  IntCast                 // Stack: round(temp), temp
    FSUBP                         // Stack: newtemp = temp - round(temp)

    MOV   EAX, IntCast
    ADD   EAX, $7F
    SHL   EAX, $17
    MOV   IntCast, EAX

    FLD   ST(0)                      // Stack: newtemp, newtemp
    FMUL  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp
    FADD  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp
    FMUL  ST(0), ST(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
    FADD  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
    FMULP                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
    FLD1
    FADDP                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
    FMUL  CastedSingle

    FSTP [EDX.FGain]
end;
{$ENDIF}

function TLightweightSoftKneeCompressor.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 Result := FGain * FMakeUpGain * Input;
end;


{ TLightweightSoftKneeUpwardCompressor }

constructor TLightweightSoftKneeUpwardCompressor.Create;
begin
 inherited;
 CalculateLogScaledThresholdValue;
 CalculateRatioFactor;
end;

procedure TLightweightSoftKneeUpwardCompressor.CalculateAttackFactor;
begin
 if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - FastPower2MinError3( -1 / (FAttack * 0.001 * SampleRate));
end;

procedure TLightweightSoftKneeUpwardCompressor.CalculateReleaseFactor;
begin
 if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := FastPower2MinError3(-1 / (FRelease * 0.001 * SampleRate));
end;

procedure TLightweightSoftKneeUpwardCompressor.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeUpwardCompressor.RatioChanged;
begin
 inherited;
 CalculateRatioFactor;
end;

procedure TLightweightSoftKneeUpwardCompressor.CalculateRatioFactor;
begin
 FRatioFactor := 1 - Ratio;
end;

procedure TLightweightSoftKneeUpwardCompressor.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain
  else CalculateMakeUpGain;
end;

procedure TLightweightSoftKneeUpwardCompressor.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := -FThreshold_dB * FRatioFactor;
 FMakeUpGain_dB := FastSqrtBab2(Sqr(Temp) + Sqr(FKnee_dB)) - Temp;
 CalculateMakeUpGain;
end;

procedure TLightweightSoftKneeUpwardCompressor.CalculateKneeFactor;
begin
 FKneeFactor := Sqr(CdBtoAmpExpGain32 * FKnee_dB);
end;

procedure TLightweightSoftKneeUpwardCompressor.ThresholdChanged;
begin
 inherited;
 CalculateLogScaledThresholdValue;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeUpwardCompressor.CalculateLogScaledThresholdValue;
begin
 FThrshlddB := Threshold_dB * CdBtoAmpExpGain32;
end;

function TLightweightSoftKneeUpwardCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 Result := PeakLevel;
 Result := FRatioFactor * (FThrshlddB - FastLog2ContinousError5(Result));
 Result := FastPower2MinError3(Result + FastSqrtBab2(Sqr(Result) + FKneeFactor));
end;

function TLightweightSoftKneeUpwardCompressor.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := FRatioFactor * (FThreshold_dB - InputLevel_dB);
 Result := Temp + FastSqrtBab2(Sqr(Temp) + Sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

function TLightweightSoftKneeUpwardCompressor.GainSample(const Input: Double): Double;
begin
 Result := FGain * FMakeUpGain * Input;
end;

procedure TLightweightSoftKneeUpwardCompressor.InputSample(const Input: Double);
{$IFDEF PUREPASCAL}
var
  Temp : Single;
begin
 Temp := CDenorm32 + abs(Input);

 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 Temp  := FRatioFactor * (FThrshlddB - FastLog2ContinousError5(FPeak));
 FGain := FastPower2MinError3(Temp + FastSqrtBab2(Sqr(Temp) + FKneeFactor));
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
    // Temp := CDenorm32 + abs(Input);
    MOV   EDX, EAX               // EDX = Self
    FLD   Input
    FABS
    FADD  CDenorm32              // Stack: temp

    FCOM  [EDX.FPeak].Double     // Stack: temp
    FSTSW AX
    SAHF
    JBE   @Release
@Attack:
    FSUB  [EDX.FPeak]
    FMUL  [EDX.FAttackFactor]
    FADD  [EDX.FPeak]
    FST   [EDX.FPeak]
    JMP   @AmpTodB
@Release:
    FLD   [EDX.FPeak]            // Stack: FPeak, temp
    FSUB  ST(0), ST(1)           // Stack: (FPeak - temp), temp
    FMUL  [EDX.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
    FADDP                        // Stack: (FPeak - temp) * FReleaseFactor + temp
    FST   [EDX.FPeak]

@AmpTodB:
    FSTP  IntCast                // Stack: (empty)
    MOV   EAX, IntCast
    MOV   ECX, EAX               // copy EAX to ECX
    AND   EAX, $807fffff
    ADD   EAX, $3f800000
    MOV   IntCast, EAX
    FLD   CastedSingle
    FMUL  [CSoftKnee        ].Single
    FADD  [CSoftKnee + 4    ].Single
    FMUL  CastedSingle
    FADD  [CSoftKnee + 4 * 2].Single
    FMUL  CastedSingle
    FADD  [CSoftKnee + 4 * 3].Single
    FMUL  CastedSingle
    FADD  [CSoftKnee + 4 * 4].Single

    SHR   ECX, $17
    AND   ECX, $000000ff
    SUB   ECX, $00000080
    MOV   IntCast, ECX
    FILD  IntCast
    FADDP


    FSUBR [EDX.FThrshlddB]       // Stack : Temp
    FMUL  [EDX.FRatioFactor]

    // FGain := FastPower2MinError3(Temp + FastSqrtBab2(Sqr(Temp) + FKneeFactor)));
    FLD   ST(0)                   // Stack : Temp, Temp
    FMUL  ST(0), ST(0)
    FADD  [EDX.FKneeFactor]       // Stack : Temp * Temp + FKneeFactor, Temp
    FLD   ST(0)                   // Stack : Intemp, Intemp, Temp
    FST   CastedSingle            // Stack : Intemp, Intemp, Temp

    MOV   EAX, IntCast
    SUB   EAX, $00800000
    SHR   EAX, 1
    ADD   EAX, $20000000
    MOV   IntCast, EAX
    FDIV  CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp
    FADD  CastedSingle            // Stack: newResult = CastedSingle + Intemp / CastedSingle, Intemp, Temp
    FLD   ST(0)                   // Stack: newResult, newResult, Intemp, Temp
    FMUL  CQuarter32              // Stack: CQuarter32 * newResult, newResult, Intemp, Temp
    FXCH                          // Stack: newResult, CQuarter32 * newResult, Intemp, Temp
    FDIVP ST(2), ST(0)            // Stack: Intemp / newResult, CQuarter32 * newResult, Temp
    FADDP                         // Stack: Intemp / newResult + CQuarter32 * newResult, Temp

    FADDP                         // Stack: Temp + SqrtTemp

    FLD   ST(0)                   // Stack: temp, temp
    FRNDINT                       // Stack: round(temp), temp

    FIST  IntCast                 // Stack: round(temp), temp
    FSUBP                         // Stack: newtemp = temp - round(temp)

    MOV   EAX, IntCast
    ADD   EAX, $7F
    SHL   EAX, $17
    MOV   IntCast, EAX

    FLD   ST(0)                      // Stack: newtemp, newtemp
    FMUL  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp
    FADD  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp
    FMUL  ST(0), ST(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
    FADD  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
    FMULP                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
    FLD1
    FADDP                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
    FMUL  CastedSingle

    FSTP [EDX.FGain]
end;
{$ENDIF}

function TLightweightSoftKneeUpwardCompressor.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 Result := FGain * FMakeUpGain * Input;
end;


{ TLightweightSoftKneeFeedbackCompressor }

constructor TLightweightSoftKneeFeedbackCompressor.Create;
begin
 inherited;
 CalculateLogScaledThresholdValue;
 CalculateRatioFactor;
end;

procedure TLightweightSoftKneeFeedbackCompressor.CalculateAttackFactor;
begin
 if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - FastPower2MinError3(-1 / (FAttack * 0.001 * SampleRate * FRatio));
end;

procedure TLightweightSoftKneeFeedbackCompressor.CalculateReleaseFactor;
begin
 if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := FastPower2MinError3(-1 / (FRelease * 0.001 * SampleRate * FRatio));
end;

procedure TLightweightSoftKneeFeedbackCompressor.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackCompressor.RatioChanged;
begin
 inherited;
 CalculateAttackFactor;
 CalculateReleaseFactor;
 CalculateRatioFactor;
 CalculateKneeFactor;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackCompressor.CalculateRatioFactor;
begin
 FRatioFactor := CHalf32 * (1 - Ratio);
end;

procedure TLightweightSoftKneeFeedbackCompressor.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain
  else CalculateMakeUpGain;
 Changed;
end;

procedure TLightweightSoftKneeFeedbackCompressor.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := -FThreshold_dB * FRatioFactor;
 FMakeUpGain_dB := FastSqrtBab2(Sqr(Temp) + Sqr(FKnee_dB)) - Temp;
 CalculateMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackCompressor.CalculateKneeFactor;
begin
 FKneeFactor := Sqr(CdBtoAmpExpGain32 * FKnee_dB) * Ratio;
end;

procedure TLightweightSoftKneeFeedbackCompressor.CalculateLogScaledThresholdValue;
begin
 FThrshlddB := Threshold_dB * CdBtoAmpExpGain32;
end;

procedure TLightweightSoftKneeFeedbackCompressor.ThresholdChanged;
begin
 inherited;
 CalculateLogScaledThresholdValue;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

function TLightweightSoftKneeFeedbackCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 Result := PeakLevel;
 Result := FRatioFactor * (FastLog2ContinousError5(Result) - FThrshlddB);
 Result := FastPower2MinError3(Result - FastSqrtBab2(Sqr(Result) + FKneeFactor));
end;

function TLightweightSoftKneeFeedbackCompressor.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := FRatioFactor/Ratio * (InputLevel_dB - FThreshold_dB);
 Result := Temp - FastSqrtBab2(Sqr(Temp) + Sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

function TLightweightSoftKneeFeedbackCompressor.GainSample(const Input: Double): Double;
begin
 Result := FGain * FMakeUpGain * Input;
end;

procedure TLightweightSoftKneeFeedbackCompressor.InputSample(const Input: Double);
{$IFNDEF XPUREPASCAL}
var
  Temp : Single;
begin
 Temp := CDenorm32 + FPrevAbsSample;

(*

 if FPeak < FThreshold
  then FAttackFactor := 1 - FastPower2MinError3(-1 / (FAttack * 0.001 * SampleRate))
  else FAttackFactor := 1 - FastPower2MinError3(-1 / (FAttack * 0.001 * SampleRate * FRatio));

 if FPeak < FThreshold
  then FReleaseFactor := FastPower2MinError3(-1 / (FRelease * 0.001 * SampleRate))
  else FReleaseFactor := FastPower2MinError3(-1 / (FRelease * 0.001 * SampleRate * FRatio));
*)


 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 Temp  := FRatioFactor * (FastLog2ContinousError5(FPeak) - FThrshlddB);
 FGain := FastPower2MinError3(Temp - FastSqrtBab2(Sqr(Temp) + FKneeFactor));

 FPrevAbsSample := abs(FGain * Input);
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
    // Temp := CDenorm32 + abs(Input);
    MOV   EDX, EAX               // EDX = Self
    FLD   [EDX.FPrevAbsSample]
    FABS
    FADD  CDenorm32              // Stack: temp

    FCOM  [EDX.FPeak].Double     // Stack: temp
    FSTSW AX
    SAHF
    JBE   @Release
@Attack:
    FSUB  [EDX.FPeak]
    FMUL  [EDX.FAttackFactor]
    FADD  [EDX.FPeak]
    FST   [EDX.FPeak]
    JMP   @AmpTodB
@Release:
    FLD   [EDX.FPeak]            // Stack: FPeak, temp
    FSUB  ST(0), ST(1)           // Stack: (FPeak - temp), temp
    FMUL  [EDX.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
    FADDP                        // Stack: (FPeak - temp) * FReleaseFactor + temp
    FST   [EDX.FPeak]

@AmpTodB:
    FSTP  IntCast                // Stack: (empty)
    MOV   EAX, IntCast
    MOV   ECX, EAX               // copy EAX to ECX
    AND   EAX, $807fffff
    ADD   EAX, $3f800000
    MOV   IntCast, EAX
    FLD   CastedSingle
    FMUL  [CSoftKnee        ].Single
    FADD  [CSoftKnee + 4    ].Single
    FMUL  CastedSingle
    FADD  [CSoftKnee + 4 * 2].Single
    FMUL  CastedSingle
    FADD  [CSoftKnee + 4 * 3].Single
    FMUL  CastedSingle
    FADD  [CSoftKnee + 4 * 4].Single

    SHR   ECX, $17
    AND   ECX, $000000ff
    SUB   ECX, $00000080
    MOV   IntCast, ECX
    FILD  IntCast
    FADDP


    FSUB  [EDX.FThrshlddB]       // Stack : Temp
    FMUL  [EDX.FRatioFactor]

    // FGain := FastPower2MinError3(CHalf32 * (FMkpdB - Temp - FastSqrtBab2(Sqr(Temp) + FKneeFactor)));
    FLD   ST(0)                   // Stack : Temp, Temp
    FMUL  ST(0), ST(0)
    FADD  [EDX.FKneeFactor]       // Stack : Temp * Temp + FKneeFactor, Temp
    FLD   ST(0)                   // Stack : Intemp, Intemp, Temp
    FST   CastedSingle            // Stack : Intemp, Intemp, Temp

    MOV   EAX, IntCast
    SUB   EAX, $00800000
    SHR   EAX, 1
    ADD   EAX, $20000000
    MOV   IntCast, EAX
    FDIV  CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp
    FADD  CastedSingle            // Stack: newResult = CastedSingle + Intemp / CastedSingle, Intemp, Temp
    FLD   ST(0)                   // Stack: newResult, newResult, Intemp, Temp
    FMUL  CQuarter32              // Stack: CQuarter32 * newResult, newResult, Intemp, Temp
    FXCH                          // Stack: newResult, CQuarter32 * newResult, Intemp, Temp
    FDIVP ST(2), ST(0)            // Stack: Intemp / newResult, CQuarter32 * newResult, Temp
    FADDP                         // Stack: Intemp / newResult + CQuarter32 * newResult, Temp

    FSUBP                         // Stack: Temp + SqrtTemp

    FLD   ST(0)                   // Stack: temp, temp
    FRNDINT                       // Stack: round(temp), temp

    FIST  IntCast                 // Stack: round(temp), temp
    FSUBP                         // Stack: newtemp = temp - round(temp)

    MOV   EAX, IntCast
    ADD   EAX, $7F
    SHL   EAX, $17
    MOV   IntCast, EAX

    FLD   ST(0)                      // Stack: newtemp, newtemp
    FMUL  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp
    FADD  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp
    FMUL  ST(0), ST(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
    FADD  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
    FMULP                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
    FLD1
    FADDP                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
    FMUL  CastedSingle

    FST  [EDX.FGain]
    FMUL Input
    FSTP [EDX.FPrevAbsSample]
end;
{$ENDIF}

function TLightweightSoftKneeFeedbackCompressor.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 Result := FGain * FMakeUpGain * Input;
end;


{ TLightweightSoftKneeFeedbackLikeCompressor }

constructor TLightweightSoftKneeFeedbackLikeCompressor.Create;
begin
 inherited;
 CalculateLogScaledThresholdValue;
 CalculateRatioFactor;
 CalculateKneeFactor;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.CalculateAttackFactor;
begin
 if FAttack <= 0
  then raise Exception.Create(RCStrAttackTimeInvalid)
  else
   begin
    FAttackSampleCycle := -1 / (FAttack * 0.001 * SampleRate);
    FAttackFactor := 1 - FastPower2MinError3(FAttackSampleCycle);
   end;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.CalculateReleaseFactor;
begin
 if FRelease <= 0
  then raise Exception.Create(RCStrReleaseTimeInvalid)
  else
   begin
    FReleaseSampleCycle := -1 / (FRelease * 0.001 * SampleRate);
    FReleaseFactor := FastPower2MinError3(FReleaseSampleCycle);
   end;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.RatioChanged;
begin
 inherited;
 CalculateRatioFactor;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.CalculateRatioFactor;
begin
 FRatioFactor := CHalf32 * (1 / Ratio - 1);
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.Reset;
begin
 inherited;
 CalculateTimeFactors;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.CalculateTimeFactors;
begin
 FAttackFactor := 1 - FastPower2MinError3(FAttackSampleCycle);
 FReleaseFactor := FastPower2MinError3(FReleaseSampleCycle);
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain
  else CalculateMakeUpGain;
 Changed;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := -FThreshold_dB * FRatioFactor;
 FMakeUpGain_dB := FastSqrtBab2(Sqr(Temp) + Sqr(FKnee_dB)) - Temp;
 CalculateMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.CalculateKneeFactor;
begin
 FKneeFactor := Sqr(CdBtoAmpExpGain32 * FKnee_dB);
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.ThresholdChanged;
begin
 inherited;
 CalculateLogScaledThresholdValue;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.CalculateLogScaledThresholdValue;
begin
 FThrshlddB := Threshold_dB * CdBtoAmpExpGain32;
end;

function TLightweightSoftKneeFeedbackLikeCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 Result := PeakLevel;
 Result := FRatioFactor * (FastLog2ContinousError5(Result) - FThrshlddB);
 Result := FastPower2MinError3(Result - FastSqrtBab2(Sqr(Result) + FKneeFactor));
end;

function TLightweightSoftKneeFeedbackLikeCompressor.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := FRatioFactor * (InputLevel_dB - FThreshold_dB);
 Result := Temp - FastSqrtBab2(Sqr(Temp) + Sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

function TLightweightSoftKneeFeedbackLikeCompressor.GainSample(const Input: Double): Double;
begin
 Result := FGain * FMakeUpGain * Input;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.InputSample(const Input: Double);
{$IFDEF PUREPASCAL}
var
  Temp  : array [0..1] of Single;
begin
 Temp[0] := CDenorm32 + abs(Input);

 if Temp[0] > FPeak
  then FPeak := FPeak + (Temp[0] - FPeak) * FAttackFactor
  else FPeak := Temp[0] + (FPeak - Temp[0]) * FReleaseFactor;

 Temp[0] := FRatioFactor * (FastLog2ContinousError5(FPeak) - FThrshlddB);
 Temp[1] := FastSqrtBab2(Sqr(Temp[0]) + FKneeFactor);
 FGain   := FastPower2MinError3(Temp[0] - Temp[1]);
 Temp[1] := 2 * Temp[1] / ((FRatio + 1) * Temp[1] + (FRatio - 1) * Temp[0]);
 FAttackFactor := 1 - FastPower2MinError3(Temp[1] * FAttackSampleCycle);
 FReleaseFactor := FastPower2MinError3(Temp[1] * FReleaseSampleCycle);
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
    // Temp := CDenorm32 + abs(Input);
    MOV   EDX, EAX               // EDX = Self
    FLD   Input
    FABS
    FADD  CDenorm32              // Stack: temp

    FCOM  [EDX.FPeak].Double     // Stack: temp
    FSTSW AX
    SAHF
    JBE   @Release
@Attack:
    FSUB  [EDX.FPeak]
    FMUL  [EDX.FAttackFactor]
    FADD  [EDX.FPeak]
    FST   [EDX.FPeak]
    JMP   @AmpTodB
@Release:
    FLD   [EDX.FPeak]            // Stack: FPeak, temp
    FSUB  ST(0), ST(1)           // Stack: (FPeak - temp), temp
    FMUL  [EDX.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
    FADDP                        // Stack: (FPeak - temp) * FReleaseFactor + temp
    FST   [EDX.FPeak]

@AmpTodB:
    FSTP  IntCast                // Stack: (empty)
    MOV   EAX, IntCast
    MOV   ECX, EAX               // copy EAX to ECX
    AND   EAX, $807fffff
    ADD   EAX, $3f800000
    MOV   IntCast, EAX
    FLD   CastedSingle
    FMUL  [CSoftKnee        ].Single
    FADD  [CSoftKnee + 4    ].Single
    FMUL  CastedSingle
    FADD  [CSoftKnee + 4 * 2].Single
    FMUL  CastedSingle
    FADD  [CSoftKnee + 4 * 3].Single
    FMUL  CastedSingle
    FADD  [CSoftKnee + 4 * 4].Single

    SHR   ECX, $17
    AND   ECX, $000000ff
    SUB   ECX, $00000080
    MOV   IntCast, ECX
    FILD  IntCast
    FADDP


    FSUB  [EDX.FThrshlddB]       // Stack : Temp
    FMUL  [EDX.FRatioFactor]     // Stack : Temp[0]

    // Temp[1] := FastSqrtBab2(Sqr(Temp[0]) + FKneeFactor);
    FLD   ST(0)                   // Stack : Temp[0], Temp[0]
    FMUL  ST(0), ST(0)
    FADD  [EDX.FKneeFactor]       // Stack : Temp[0] * Temp[0] + FKneeFactor, Temp[0]
    FLD   ST(0)                   // Stack : Intemp, Intemp, Temp[0]
    FST   CastedSingle            // Stack : Intemp, Intemp, Temp[0]

    MOV   EAX, IntCast
    SUB   EAX, $00800000
    SHR   EAX, 1
    ADD   EAX, $20000000
    MOV   IntCast, EAX
    FDIV  CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp[0]
    FADD  CastedSingle            // Stack: newResult = CastedSingle + Intemp / CastedSingle, Intemp, Temp[0]
    FLD   ST(0)                   // Stack: newResult, newResult, Intemp, Temp[0]
    FMUL  CQuarter32              // Stack: CQuarter32 * newResult, newResult, Intemp, Temp[0]
    FXCH                          // Stack: newResult, CQuarter32 * newResult, Intemp, Temp[0]
    FDIVP ST(2), ST(0)            // Stack: Intemp / newResult, CQuarter32 * newResult, Temp[0]
    FADDP                         // Stack: Temp[1] := Intemp / newResult + CQuarter32 * newResult, Temp[0]

    // FGain   := FastPower2MinError3(Temp[0] - Temp[1]);
    FLD   ST(0)                   // Stack: Temp[1], Temp[1], Temp[0]
    FSUBR ST(0), ST(2)            // Stack: Temp[0] - Temp[1], Temp[1], Temp[0]

    FLD   ST(0)                   // Stack: Temp[0] - Temp[1], Temp[0] - Temp[1], Temp[1], Temp[0]
    FRNDINT                       // Stack: round(Temp[0] - Temp[1]), Temp[0] - Temp[1], Temp[1], Temp[0]

    FIST  IntCast                 // Stack: round(Temp[0] - Temp[1]), Temp[0] - Temp[1], Temp[1], Temp[0]
    FSUBP                         // Stack: newtemp = (Temp[0] - Temp[1]) - round(Temp[0] - Temp[1]), Temp[1], Temp[0]

    MOV   EAX, IntCast
    ADD   EAX, $7F
    SHL   EAX, $17
    MOV   IntCast, EAX

    FLD   ST(0)                      // Stack: newtemp, newtemp, Temp[1], Temp[0]
    FMUL  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp, Temp[1], Temp[0]
    FADD  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp, Temp[1], Temp[0]
    FMUL  ST(0), ST(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[1], Temp[0]
    FADD  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[1], Temp[0]
    FMULP                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[1], Temp[0]
    FLD1
    FADDP                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[1], Temp[0]
    FMUL  CastedSingle               // NewGain, Temp[1], Temp[0]
    FSTP  [EDX.FGain]

    // Temp[1] := 2 * Temp[1] / ((FRatio + 1) * Temp[1] + (FRatio - 1) * Temp[0]);
    FLD1                             // 1, Temp[1], Temp[0]
    FADD  [EDX.FRatio]               // Ratio + 1, Temp[1], Temp[0]
    FMUL  ST(0), ST(1)               // Temp[1] * (Ratio + 1), Temp[1], Temp[0]
    FXCH  ST(2)                      // Temp[0], Temp[1], Temp[1] * (Ratio + 1)
    FLD1                             // 1, Temp[0], Temp[1], Temp[1] * (Ratio + 1)
    FSUBR [EDX.FRatio]               // Ratio - 1, Temp[0], Temp[1], Temp[1] * (Ratio + 1)
    FMULP                            // (Ratio - 1) * Temp[0], Temp[1], Temp[1] * (Ratio + 1)
    FADDP ST(2), ST(0)               // Temp[1], (Ratio - 1) * Temp[0] + Temp[1] * (Ratio + 1),
    FDIVRP                           // Temp[1] / (Ratio - 1) * Temp[0] + Temp[1] * (Ratio + 1)
    FADD  ST(0), ST(0)               // 2 * Temp[1] / (Ratio - 1) * Temp[0] + Temp[1] * (Ratio + 1)

    // FAttackFactor := 1 - FastPower2MinError3(Temp[1] * FAttackSampleCycle);
    FLD   ST(0)                      // Temp[1], Temp[1]
    FMUL  [EDX.FAttackSampleCycle]   // Temp[0], Temp[1]

    FLD   ST(0)                      // Stack: Temp[0], Temp[0], Temp[1]
    FRNDINT                          // Stack: round(Temp[0]), Temp[0], Temp[1]

    FIST  IntCast                    // Stack: round(Temp[0]), Temp[0], Temp[1]
    FSUBP                            // Stack: newtemp = Temp[0] - round(Temp[0], Temp[0], Temp[1]

    MOV   EAX, IntCast
    ADD   EAX, $7F
    SHL   EAX, $17
    MOV   IntCast, EAX

    FLD   ST(0)                      // Stack: newtemp, newtemp, Temp[0], Temp[1]
    FMUL  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp, Temp[0], Temp[1]
    FADD  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp, Temp[0], Temp[1]
    FMUL  ST(0), ST(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0], Temp[1]
    FADD  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0], Temp[1]
    FMULP                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0], Temp[1]
    FLD1
    FADDP                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0], Temp[1]
    FMUL  CastedSingle               // NewAttackFactor, Temp[1]
    FLD1                             // 1, NewAttackFactor, Temp[1]
    FSUBRP                           // 1 - NewAttackFactor, Temp[1]
    FSTP  [EDX.FAttackFactor]

    // FReleaseFactor := FastPower2MinError3(Temp[1] * FReleaseSampleCycle);
    FMUL  [EDX.FReleaseSampleCycle]  // Temp[0]

    FLD   ST(0)                      // Stack: Temp[0], Temp[0]
    FRNDINT                          // Stack: round(Temp[0]), Temp[0]

    FIST  IntCast                    // Stack: round(Temp[0]), Temp[0]
    FSUBP                            // Stack: newtemp = Temp[0] - round(Temp[0], Temp[0]

    MOV   EAX, IntCast
    ADD   EAX, $7F
    SHL   EAX, $17
    MOV   IntCast, EAX

    FLD   ST(0)                      // Stack: newtemp, newtemp, Temp[0]
    FMUL  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp, Temp[0]
    FADD  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp, Temp[0]
    FMUL  ST(0), ST(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0]
    FADD  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0]
    FMULP                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0]
    FLD1
    FADDP                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0]
    FMUL  CastedSingle               // NewReleaseFactor
    FSTP  [EDX.FReleaseFactor]
end;
{$ENDIF}

function TLightweightSoftKneeFeedbackLikeCompressor.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 Result := FGain * FMakeUpGain * Input;
end;

initialization
  RegisterDspProcessors32([TLightweightSoftKneeLimiter,
    TLightweightSoftKneeFeedbackLikeLimiter, TLightweightSoftKneeCompressor,
    TLightweightSoftKneeUpwardCompressor,
    TLightweightSoftKneeFeedbackCompressor,
    TLightweightSoftKneeFeedbackLikeCompressor]);
  RegisterDspProcessors64([TLightweightSoftKneeLimiter,
    TLightweightSoftKneeFeedbackLikeLimiter, TLightweightSoftKneeCompressor,
    TLightweightSoftKneeUpwardCompressor,
    TLightweightSoftKneeFeedbackCompressor,
    TLightweightSoftKneeFeedbackLikeCompressor]);

end.
