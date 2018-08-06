unit DAV_DspFilterChebyshevType2;

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

{$IFDEF CPUx86_64}
  {$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Classes, DAV_Complex, DAV_DspFilterChebyshev;

type
  TCustomChebyshev2Filter = class(TCustomChebyshevFilter)
  private
    procedure SetStopband(const Value: Double);
    procedure SetFixFrequency(const Value: Boolean);
  protected
    FStopband      : Double;
    FStopbandGain  : Double;
    FFixFrequency  : Boolean;
    FRealFrequency : Double;
    FCoeffs        : array [0..127] of Double;
    FState         : array [0..63] of Double;
    FStateStack    : array of array [0..63] of Double;
    function CorrectFrequency(CurrentFrequency: Double): Double; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateW0; override;
    procedure CalculateHypFactors; override;
    procedure CalculateStopbandGain; virtual;
    procedure FixFrequencyChanged; virtual;
    procedure StopbandChanged; virtual;
    class function GetMaxOrder: Cardinal; override;
  public
    constructor Create(const Order: Integer = 0); override;
    procedure SetFilterValues(const AFrequency, AGain, AStopband : Single); virtual;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    procedure ResetStates; override;
    procedure PushStates; override;
    procedure PopStates; override;
    procedure Reset; override;

    property FixFrequency: Boolean read FFixFrequency write SetFixFrequency default False;
    property Stopband : Double read FStopband write SetStopband;
  end;

  TCustomChebyshev2LowpassFilter = class(TCustomChebyshev2Filter)
  protected
    function CorrectFrequency(CurrentFrequency: Double): Double; override;
  public
    function ProcessSample64(Input: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
    procedure CalculateCoefficients; override;
  end;

  TChebyshev2LowpassFilter = class(TCustomChebyshev2LowpassFilter)
  public
    function MagnitudeSquared(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary: Double); override;
  end;
  TChebyshev2HighCutFilter = TChebyshev2LowpassFilter;

  TChebyshev2LowpassFilterAutomatable = class(TCustomChebyshev2LowpassFilter)
  protected
    procedure CalculateW0; override;
    procedure CalculateHypFactors; override;
    procedure OrderChanged; override;
  public
    function MagnitudeSquared(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary: Double); override;
  end;
  TChebyshev2HighCutFilterAutomatable = TChebyshev2LowpassFilterAutomatable;

  TCustomChebyshev2HighpassFilter = class(TCustomChebyshev2Filter)
  protected
    function CorrectFrequency(CurrentFrequency: Double): Double; override;
  public
    function ProcessSample64(Input: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
    procedure CalculateCoefficients; override;
  end;

  TChebyshev2HighpassFilter = class(TCustomChebyshev2HighpassFilter)
  public
    function MagnitudeSquared(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary: Double); override;
  end;
  TChebyshev2LowCutFilter = TChebyshev2HighpassFilter;

  TChebyshev2HighpassFilterAutomatable = class(TCustomChebyshev2HighpassFilter)
  protected
    procedure CalculateW0; override;
    procedure CalculateHypFactors; override;
    procedure OrderChanged; override;
  public
    function MagnitudeSquared(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary: Double); override;
  end;
  TChebyshev2LowCutFilterAutomatable = TChebyshev2HighpassFilterAutomatable;

implementation

uses
  Math, SysUtils, DAV_Classes, DAV_Common, DAV_Math, DAV_Approximations;

const
  CHalf32 : Single = 0.5;
  CHalf64 : Double = 0.5;

{$IFDEF HandleDenormals}
var
  DenormRandom : Single;
const
  CDenorm32    : Single = 1E-24;
  CDenorm64    : Double = 1E-34;
{$ENDIF}

{ TCustomChebyshev2Filter }

constructor TCustomChebyshev2Filter.Create(const Order: Integer = 0);
begin
 FFilterGain := 1;
 FStopband := -10;
 CalculateStopbandGain;
 FFixFrequency := False;
 inherited Create(Order);
end;

class function TCustomChebyshev2Filter.GetMaxOrder: Cardinal;
begin
 result := 32;
end;

procedure TCustomChebyshev2Filter.Reset;
begin
 Gain := 0;
end;

procedure TCustomChebyshev2Filter.ResetStates;
begin
 FillChar(FState[0], FOrder * SizeOf(Double), 0);
end;

procedure TCustomChebyshev2Filter.StopbandChanged;
begin
 CalculateStopbandGain;
 CalculateHypFactors;
 CalculateCoefficients;
end;

procedure TCustomChebyshev2Filter.SetStopband(const Value: Double);
begin
 if Value <> FStopband then
  begin
   FStopband := Value;
   StopbandChanged;
  end;
end;

procedure TCustomChebyshev2Filter.SetFixFrequency(const Value: Boolean);
begin
 if FFixFrequency <> Value then
  begin
   FFixFrequency := Value;
   FixFrequencyChanged;
  end;
end;

procedure TCustomChebyshev2Filter.FixFrequencyChanged;
begin
 if FFixFrequency then
  begin
   CalculateW0;
   CalculateCoefficients;
   Changed;
  end;
end;

procedure TCustomChebyshev2Filter.CalculateStopbandGain;
begin
 Assert(FStopband < 0);
 FStopbandGain := Power(10, -0.1 * FStopband);
 if FFixFrequency then CalculateW0;
end;

procedure TCustomChebyshev2Filter.CalculateW0;
begin
 if FFixFrequency then
  begin
   FW0 := Pi * FSRR * CorrectFrequency(FFrequency);
   FTanW0Half := tan(FW0);
  end
 else inherited;
end;

procedure TCustomChebyshev2Filter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomChebyshev2Filter then
  with TCustomChebyshev2Filter(Dest) do
   begin
    inherited;
    FStopband      := Self.FStopband;
    FStopbandGain  := Self.FStopbandGain;
    FFixFrequency  := Self.FFixFrequency;
    FRealFrequency := Self.FRealFrequency;
    FCoeffs        := Self.FCoeffs;
    FState         := Self.FState;
    FStateStack    := Self.FStateStack;
   end
 else inherited;
end;

procedure TCustomChebyshev2Filter.CalculateHypFactors;
var
  t : Double;
begin
 t := Exp(FOrderInv * Ln(Sqrt(FStopbandGain - 1) + Sqrt(FStopbandGain)));
 FHypFactors[1] := (t - 1 / t) * 0.5;
 FHypFactors[0] := sqr(FHypFactors[1]) + 1;
 if FFixFrequency then CalculateW0; 
end;

procedure TCustomChebyshev2Filter.SetFilterValues(const AFrequency, AGain, AStopband : Single);
begin
 FFrequency  := AFrequency;
 FGain_dB    := AGain;
 FStopband   := AStopband;
 CalculateW0;
 CalculateStopbandGain;
 CalculateGainFactor;
 CalculateHypFactors;
 CalculateCoefficients;
end;

procedure TCustomChebyshev2Filter.PopStates;
begin
 if Length(FStateStack) > 0 then
  begin
   Move(FStateStack[0, 0], FState[0], Length(FStateStack[0]) * SizeOf(Double));
   if Length(FStateStack) > 1
    then Move(FStateStack[1, 0],FStateStack[0, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
   SetLength(FStateStack, Length(FStateStack) - 1);
  end;
end;

procedure TCustomChebyshev2Filter.PushStates;
begin
 SetLength(FStateStack, Length(FStateStack) + 1);
 if Length(FStateStack) > 1
  then Move(FStateStack[0, 0], FStateStack[1, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
 Move(FState[0], FStateStack[0, 0], Length(FStateStack[0]) * SizeOf(Double));
end;

function TCustomChebyshev2Filter.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := 10 * Log10(MagnitudeSquared(Frequency));
end;


{ TCustomChebyshev2LowpassFilter }

function TCustomChebyshev2LowpassFilter.CorrectFrequency(CurrentFrequency: Double): Double;
var
  t : Double;
begin
 t := sqrt(FStopbandGain - 1);
 t := Exp(FOrderInv * Ln(t + Sqrt((t - 1) / (t + 1)) * (t + 1)));
 result := FFrequency * (t + 1 / t) * 0.5;
end;

function TCustomChebyshev2LowpassFilter.Phase(const Frequency: Double): Double;
var
  Cmplx : TComplex64;
begin
 Complex(Frequency, Cmplx.Re, Cmplx.Im);
 Result := ArcTan2(Cmplx.Im, Cmplx.Re);
end;

procedure TCustomChebyshev2LowpassFilter.CalculateCoefficients;
var
  K, K2 : Double;
  t     : array [0..3] of Double;
  i     : Integer;
  Cmplx : TComplex64;
begin
 K := FTanW0Half;
 K2 := sqr(K);
 FFilterGain := FGainFactor;
 Cmplx := FExpOrdPiHalf;

 if (FOrder mod 2) = 1 then
  begin
   t[0] := FHypFactors[1];
   t[1] := 1 / (t[0] + K);
   FFilterGain := FFilterGain * K * t[1];
   FCoeffs[(3 * FOrder div 2) - 1] := (t[0] - K) * t[1];
   ComplexMultiplyInplace64(Cmplx, FExpOrdPiHalf);
  end;

 for i := 0 to (FOrder div 2) - 1 do
  begin
   t[0] := FHypFactors[0] - sqr(Cmplx.Re);
   t[1] := 2 * K * Cmplx.Re * FHypFactors[1];
   t[2] := 1 / (t[0] + t[1] + K2);
   FFilterGain := FFilterGain * (K2 + sqr(Cmplx.Im)) * t[2];
   FCoeffs[3 * i    ] := 2 * (K2 - sqr(Cmplx.Im)) / (K2 + sqr(Cmplx.Im));
   FCoeffs[3 * i + 1] := 2 * (       t[0] - K2) * t[2];
   FCoeffs[3 * i + 2] :=     (t[1] - t[0] - K2) * t[2];
   ComplexMultiply2Inplace64(Cmplx, FExpOrdPiHalf);
  end;
end;

function TCustomChebyshev2LowpassFilter.ProcessSample64(Input: Double): Double;
{-$DEFINE PUREPASCAL}
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result            := x + FState[2 * i];
   FState[2 * i    ] := x * FCoeffs[3 * i] + FCoeffs[3 * i + 1] * Result + FState[2 * i + 1];
   FState[2 * i + 1] := x                  + FCoeffs[3 * i + 2] * Result;
  end;

 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   x := Result;
   Result        := x + FState[2 * i];
   FState[2 * i] := x + FCoeffs[3 * i] * Result;
  end;
{$ELSE}
asm
 fld   Input.Double;
 {$IFDEF HandleDenormals}
 fadd  CDenorm32
 {$ENDIF}
 fmul  [eax.FFilterGain].Double

 mov   ecx, [eax.FOrder]
 test  ecx, 1
 jz    @BiquadStageCheck

@SingleStage:
 sub   ecx, 1
 imul  edx, ecx, 3
 shr   edx, 1

 fld   st(0)
 fadd  [eax.FState + ecx * 8].Double
 fld   st(0)
 fmul  [eax.FCoeffs + edx * 8].Double
 faddp st(2), st(0)
 fxch
 fstp  [eax.FState + ecx * 8].Double

@BiquadStageCheck:
 test  ecx, ecx
 jz    @End

 imul  edx, ecx, 3
 shr   edx, 1
@FilterLoop:
 sub   ecx, 2
 sub   edx, 3
 fld   st(0)
 fadd  [eax.FState + ecx * 8].Double
 fld   st(0)
 fld   st(0)
 fmul  [eax.FCoeffs + edx * 8 + 8].Double
 fadd  [eax.FState + ecx * 8 + 8].Double
 fld   st(3)
 fmul  [eax.FCoeffs + edx * 8].Double
 faddp st(1), st(0)
 fstp  [eax.FState + ecx * 8].Double
 fmul  [eax.FCoeffs + edx * 8 + 16].Double
 fxch
 fxch  st(2)
 faddp st(1), st(0)
 fstp  [eax.FState + ecx * 8 + 8].Double
 ja    @FilterLoop

 @End:
 {$ENDIF}
end;


{ TChebyshev2LowpassFilter }

procedure TChebyshev2LowpassFilter.Complex(const Frequency: Double;
  out Real, Imaginary: Double);
var
  i       : Cardinal;
  Cmplx   : TComplex64;
  A, B, R : TComplex64;
begin
 GetSinCos(2 * Pi * Frequency * FSRR, Cmplx.Im, Cmplx.Re);

 R.Re := FFilterGain;
 R.Im := 0;

 i := 0;
 while i < (FOrder div 2) do
  begin
   A.Re :=  Cmplx.Re * (2 * Cmplx.Re + FCoeffs[3 * i]);
   A.Im := -Cmplx.Im * (2 * Cmplx.Re + FCoeffs[3 * i]);
   B.Re :=  1 - FCoeffs[3 * i + 1] * Cmplx.Re - FCoeffs[3 * i + 2] * (2 * sqr(Cmplx.Re) - 1);
   B.Im :=  Cmplx.Im * (FCoeffs[3 * i + 1] + 2 * Cmplx.Re * FCoeffs[3 * i + 2]);
   R := ComplexMultiply64(R, ComplexDivide64(A, B));
   inc(i);
  end;

 if FOrder mod 2 = 1 then
  begin
   A.Re :=  Cmplx.Re + 1;
   A.Im := -Cmplx.Im;
   B.Re := -Cmplx.Re * FCoeffs[3 * i] + 1;
   B.Im :=  Cmplx.Im * FCoeffs[3 * i];
   R := ComplexMultiply64(R, ComplexDivide64(A, B));
  end;

 Real := R.Re;
 Imaginary := R.Im;
end;

function TChebyshev2LowpassFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  i  : Integer;
  cw : Double;
begin
 cw     := 2 * cos(2 * Frequency * Pi * SampleRateReciprocal);
 Result := sqr(FFilterGain);

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * (sqr(FCoeffs[3 * i]) + cw * (2 * FCoeffs[3 * i] + cw)) /
       (1 + sqr(FCoeffs[3 * i + 1]) + sqr(FCoeffs[3 * i + 2]) +
        2 * FCoeffs[3 * i + 2] + cw * (FCoeffs[3 * i + 1] *
        (FCoeffs[3 * i + 2] - 1) - cw * FCoeffs[3 * i + 2]));

 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw + 2) / (1 + sqr(FCoeffs[3 * i]) - cw * FCoeffs[3 * i]);
  end;

 Result := CDenorm64 + Abs(Result);
end;


{ TCustomChebyshev2HighpassFilter }

function TCustomChebyshev2HighpassFilter.CorrectFrequency(CurrentFrequency: Double): Double;
var
  t : Double;
begin
 t := sqrt(max(2, FStopbandGain) - 1);
 t := Exp(FOrderInv * Ln(t + Sqrt((t - 1) / (t + 1)) * (t + 1)));
 result := FFrequency / (t + 1 / t) * 2;
end;

function TCustomChebyshev2HighpassFilter.Phase(const Frequency: Double): Double;
var
  Cmplx : TComplex64;
begin
 Complex(Frequency, Cmplx.Re, Cmplx.Im);
 Result := ArcTan2(Cmplx.Im, Cmplx.Re);
end;

procedure TCustomChebyshev2HighpassFilter.CalculateCoefficients;
var
  K, K2 : Double;
  t     : array [0..3] of Double;
  i     : Integer;
  Cmplx : TComplex64;
begin
 K := FTanW0Half;
 K2 := sqr(K);
 FFilterGain := FGainFactor;
 Cmplx := FExpOrdPiHalf;

 if (FOrder mod 2) = 1 then
  begin
   t[0] := 1 / FHypFactors[1];
   t[1] := 1 / (t[0] + K);
   FFilterGain := FFilterGain * t[1] * t[0];
   FCoeffs[(3 * FOrder div 2) - 1] := (t[0] - K) * t[1];
   ComplexMultiplyInplace64(Cmplx, FExpOrdPiHalf);
  end;

 for i := 0 to (FOrder div 2) - 1 do
  begin
   t[0] := FHypFactors[0] - sqr(Cmplx.Re);
   t[1] := 2 * K * Cmplx.Re * FHypFactors[1];
   t[2] := 1 / (t[1] + 1 + t[0] * K2);
   FFilterGain := FFilterGain * t[2] * (K2 * sqr(Cmplx.Im) + 1);
   FCoeffs[3 * i    ] := 2 * (K2 * sqr(Cmplx.Im) - 1) / (K2 * sqr(Cmplx.Im) + 1);
   FCoeffs[3 * i + 1] := 2 * (       1 - t[0] * K2) * t[2];
   FCoeffs[3 * i + 2] :=     (t[1] - 1 - t[0] * K2) * t[2];
   ComplexMultiply2Inplace64(Cmplx, FExpOrdPiHalf);
  end;
end;

function TCustomChebyshev2HighpassFilter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result            := x + FState[2 * i];
   FState[2 * i    ] := x * FCoeffs[3 * i] + FCoeffs[3 * i + 1] * Result + FState[2 * i + 1];
   FState[2 * i + 1] := x                  + FCoeffs[3 * i + 2] * Result;
  end;

 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   x := Result;
   Result        :=  x + FState[2 * i];
   FState[2 * i] := -x + FCoeffs[3 * i] * Result;
  end;
{$ELSE}
asm
 fld    Input.Double;

 // eventually add denormal
 {$IFDEF HandleDenormals}
 mov  edx, DenormRandom
 imul edx, DenormRandom, $08088405
 inc  edx
 shr  edx, 23
 or   edx, $20000000
 mov  DenormRandom, edx
 fadd DenormRandom
 {$ENDIF}
 fmul   [eax.FFilterGain].Double

 mov    ecx, [eax.FOrder]
 test   ecx, 1
 jz     @BiquadStageCheck

@SingleStage:
 sub    ecx, 1
 imul   edx, ecx, 3
 shr    edx, 1

 fld    st(0)
 fadd   [eax.FState + ecx * 8].Double
 fld    st(0)
 fmul   [eax.FCoeffs + edx * 8].Double
 fsubrp st(2), st(0)
 fxch
 fstp   [eax.FState + ecx * 8].Double

@BiquadStageCheck:
 test   ecx, ecx
 jz     @End

 imul   edx, ecx, 3
 shr    edx, 1
@FilterLoop:
 sub    ecx, 2
 sub    edx, 3
 fld    st(0)
 fadd   [eax.FState + ecx * 8].Double
 fld    st(0)
 fld    st(0)
 fmul   [eax.FCoeffs + edx * 8 + 8].Double
 fadd   [eax.FState + ecx * 8 + 8].Double
 fld    st(3)
 fmul   [eax.FCoeffs + edx * 8].Double
 faddp  st(1), st(0)
 fstp   [eax.FState + ecx * 8].Double
 fmul   [eax.FCoeffs + edx * 8 + 16].Double
 fxch
 fxch   st(2)
 faddp  st(1), st(0)
 fstp   [eax.FState + ecx * 8 + 8].Double
 ja     @FilterLoop

 @End:
 {$ENDIF}
end;


{ TChebyshev2HighpassFilter }

procedure TChebyshev2HighpassFilter.Complex(const Frequency: Double; out Real,
  Imaginary: Double);
var
  i       : Cardinal;
  Cmplx   : TComplex64;
  A, B, R : TComplex64;
begin
 GetSinCos(2 * Pi * Frequency * FSRR, Cmplx.Im, Cmplx.Re);

 R.Re := FFilterGain;
 R.Im := 0;

 i := 0;
 while i < (FOrder div 2) do
  begin
   A.Re :=  Cmplx.Re * (2 * Cmplx.Re + FCoeffs[3 * i]);
   A.Im := -Cmplx.Im * (2 * Cmplx.Re + FCoeffs[3 * i]);
   B.Re :=  1 - FCoeffs[3 * i + 1] * Cmplx.Re - FCoeffs[3 * i + 2] * (2 * sqr(Cmplx.Re) - 1);
   B.Im :=  Cmplx.Im * (FCoeffs[3 * i + 1] + 2 * Cmplx.Re * FCoeffs[3 * i + 2]);
   R := ComplexMultiply64(R, ComplexDivide64(A, B));
   inc(i);
  end;

 if FOrder mod 2 = 1 then
  begin
   A.Re :=  Cmplx.Re - 1;
   A.Im := -Cmplx.Im;
   B.Re :=  Cmplx.Re * FCoeffs[3 * i] - 1;
   B.Im :=  Cmplx.Im * FCoeffs[3 * i];
   R := ComplexMultiply64(R, ComplexDivide64(A, B));
  end;

 Real := R.Re;
 Imaginary := R.Im;
end;

function TChebyshev2HighpassFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  i  : Integer;
  cw : Double;
begin
 cw     := 2 * cos(2 * Frequency * Pi * SampleRateReciprocal);
 Result := sqr(FFilterGain);

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * (sqr(FCoeffs[3 * i]) + cw * (2 * FCoeffs[3 * i] + cw)) /
       (1 + sqr(FCoeffs[3 * i + 1]) + sqr(FCoeffs[3 * i + 2]) +
        2 * FCoeffs[3 * i + 2] + cw * (FCoeffs[3 * i + 1] *
        (FCoeffs[3 * i + 2] - 1) - cw * FCoeffs[3 * i + 2]));

 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw - 2) / (1 + sqr(FCoeffs[3 * i]) - cw * FCoeffs[3 * i]);
  end;

 Result := CDenorm64 + Abs(Result);
end;

{ TChebyshev2LowpassFilterAutomatable }

procedure TChebyshev2LowpassFilterAutomatable.CalculateHypFactors;
var
  t : array [0..1] of Single;
begin
 t[0] := FastSqrtBab1(FStopbandGain - 1) + FastSqrtBab1(FStopbandGain);
 t[0] := FastLog2MinError3(t[0]) * FOrderInv;
 t[1] := FastPower2MinError3(t[0]);

 FHypFactors[1] := (t[1] - 1 / t[1]) * 0.5;
 FHypFactors[0] := sqr(FHypFactors[1]) + 1;
end;

procedure TChebyshev2LowpassFilterAutomatable.CalculateW0;
begin
 if FFixFrequency
  then FW0 := Pi * FSRR * FRealFrequency
  else FW0 := Pi * FSRR * FFrequency;

 FTanW0Half := FastTan2Term(FW0);
end;

procedure TChebyshev2LowpassFilterAutomatable.Complex(const Frequency: Double;
  out Real, Imaginary: Double);
var
  i       : Cardinal;
  Cmplx   : TComplex64;
  A, B, R : TComplex64;
begin
 GetSinCos(2 * Pi * Frequency * FSRR, Cmplx.Im, Cmplx.Re);

 R.Re := FFilterGain;
 R.Im := 0;

 i := 0;
 while i < (FOrder div 2) do
  begin
   A.Re :=  Cmplx.Re * (2 * Cmplx.Re + FCoeffs[3 * i]);
   A.Im := -Cmplx.Im * (2 * Cmplx.Re + FCoeffs[3 * i]);
   B.Re :=  1 - FCoeffs[3 * i + 1] * Cmplx.Re - FCoeffs[3 * i + 2] * (2 * sqr(Cmplx.Re) - 1);
   B.Im :=  Cmplx.Im * (FCoeffs[3 * i + 1] + 2 * Cmplx.Re * FCoeffs[3 * i + 2]);
   R := ComplexMultiply64(R, ComplexDivide64(A, B));
   inc(i);
  end;

 if FOrder mod 2 = 1 then
  begin
   A.Re :=  Cmplx.Re + 1;
   A.Im := -Cmplx.Im;
   B.Re := -Cmplx.Re * FCoeffs[3 * i] + 1;
   B.Im :=  Cmplx.Im * FCoeffs[3 * i];
   R := ComplexMultiply64(R, ComplexDivide64(A, B));
  end;

 Real := R.Re;
 Imaginary := R.Im;
end;

function TChebyshev2LowpassFilterAutomatable.MagnitudeSquared(const Frequency: Double): Double;
var
  i        : Cardinal;
  cw       : Double;
  Nom, Den : Single;
begin
 cw  := 2 * FastCosInBounds4Term(2 * Frequency * Pi * SampleRateReciprocal);
 Nom := sqr(FFilterGain);
 Den := 1;

 i := 0;
 while i < (FOrder div 2) do
  begin
   Nom := Nom * (sqr(FCoeffs[3 * i]) + cw * (2 * FCoeffs[3 * i] + cw));
   Den := Den * (1 + sqr(FCoeffs[3 * i + 1]) + sqr(FCoeffs[3 * i + 2]) +
      2 * FCoeffs[3 * i + 2] + cw * (FCoeffs[3 * i + 1] *
      (FCoeffs[3 * i + 2] - 1) - cw * FCoeffs[3 * i + 2]));
   inc(i);
  end;

 if (FOrder mod 2) = 1 then
  begin
   Nom := Nom * (cw + 2);
   Den := Den * (1 + sqr(FCoeffs[3 * i]) - cw * FCoeffs[3 * i]);
  end;

 Result := CDenorm64 + Abs(Nom / Den);
end;

procedure TChebyshev2LowpassFilterAutomatable.OrderChanged;
begin
 if FOrder > 0 then
  begin
   FOrderInv := 1 / FOrder;
   GetSinCos(Pi * CHalf64 * FOrderInv, FExpOrdPiHalf.Im, FExpOrdPiHalf.Re);
   CalculateHypFactors;
   ResetStates;
   CalculateCoefficients;
  end else FOrderInv := 1;
 Changed;  
end;

{ TChebyshev2HighpassFilterAutomatable }

procedure TChebyshev2HighpassFilterAutomatable.CalculateHypFactors;
var
  t : array [0..1] of Single;
begin
 t[0] := FastSqrtBab1(FStopbandGain - 1) + FastSqrtBab1(FStopbandGain);
 t[0] := FastLog2MinError3(t[0]) * FOrderInv;
 t[1] := FastPower2MinError3(t[0]);

 FHypFactors[1] := (t[1] - 1 / t[1]) * 0.5;
 FHypFactors[0] := sqr(FHypFactors[1]) + 1;
end;

procedure TChebyshev2HighpassFilterAutomatable.CalculateW0;
begin
 if FFixFrequency
  then FW0 := Pi * FSRR * FRealFrequency
  else FW0 := Pi * FSRR * FFrequency;

 FTanW0Half := FastTan2Term(FW0);
end;

procedure TChebyshev2HighpassFilterAutomatable.Complex(const Frequency: Double;
  out Real, Imaginary: Double);
var
  i       : Cardinal;
  Cmplx   : TComplex64;
  A, B, R : TComplex64;
begin
 GetSinCos(2 * Pi * Frequency * FSRR, Cmplx.Im, Cmplx.Re);

 R.Re := FFilterGain;
 R.Im := 0;

 i := 0;
 while i < (FOrder div 2) do
  begin
   A.Re :=  Cmplx.Re * (2 * Cmplx.Re + FCoeffs[3 * i]);
   A.Im := -Cmplx.Im * (2 * Cmplx.Re + FCoeffs[3 * i]);
   B.Re :=  1 - FCoeffs[3 * i + 1] * Cmplx.Re - FCoeffs[3 * i + 2] * (2 * sqr(Cmplx.Re) - 1);
   B.Im :=  Cmplx.Im * (FCoeffs[3 * i + 1] + 2 * Cmplx.Re * FCoeffs[3 * i + 2]);
   R := ComplexMultiply64(R, ComplexDivide64(A, B));
   inc(i);
  end;

 if FOrder mod 2 = 1 then
  begin
   A.Re :=  Cmplx.Re - 1;
   A.Im := -Cmplx.Im;
   B.Re :=  Cmplx.Re * FCoeffs[3 * i] - 1;
   B.Im :=  Cmplx.Im * FCoeffs[3 * i];
   R := ComplexMultiply64(R, ComplexDivide64(A, B));
  end;

 Real := R.Re;
 Imaginary := R.Im;
end;

function TChebyshev2HighpassFilterAutomatable.MagnitudeSquared(const Frequency: Double): Double;
var
  i        : Cardinal;
  cw       : Single;
  Nom, Den : Single;
begin
 cw  := 2 * FastCosInBounds4Term(2 * Frequency * Pi * SampleRateReciprocal);
 Nom := sqr(FFilterGain);
 Den := 1;

 i := 0;
 while i < (FOrder div 2) do
  begin
   Nom := Nom * (sqr(FCoeffs[3 * i]) + cw * (2 * FCoeffs[3 * i] + cw));
   Den := Den * (1 + sqr(FCoeffs[3 * i + 1]) + sqr(FCoeffs[3 * i + 2]) +
     2 * FCoeffs[3 * i + 2] + cw * (FCoeffs[3 * i + 1] *
     (FCoeffs[3 * i + 2] - 1) - cw * FCoeffs[3 * i + 2]));
   inc(i);
  end;

 if (FOrder mod 2) = 1 then
  begin
   Nom := Nom * (cw - 2);
   Den := Den * (1 + sqr(FCoeffs[3 * i]) - cw * FCoeffs[3 * i]);
  end;

 Result := CDenorm64 + Abs(Nom / Den);
end;

procedure TChebyshev2HighpassFilterAutomatable.OrderChanged;
begin
 if FOrder > 0 then
  begin
   FOrderInv := 1 / FOrder;
   GetSinCos(Pi * CHalf64 * FOrderInv, FExpOrdPiHalf.Im, FExpOrdPiHalf.Re);
   CalculateHypFactors;
   ResetStates;
   CalculateCoefficients;
  end else FOrderInv := 1;
 Changed;
end;

initialization
  RegisterDspProcessors32([TChebyshev2LowpassFilter,
    TChebyshev2HighpassFilter, TChebyshev2LowpassFilterAutomatable,
    TChebyshev2HighpassFilterAutomatable]);
  RegisterDspProcessors64([TChebyshev2LowpassFilter,
    TChebyshev2HighpassFilter, TChebyshev2LowpassFilterAutomatable,
    TChebyshev2HighpassFilterAutomatable]);

end.
