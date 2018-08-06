unit DAV_DspFilterButterworth;

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

{$IFDEF Darwin}
  {$DEFINE PUREPASCAL} // for OSX use pure pascal code
{$ENDIF}
{$IFDEF CPUx86_64}
  {$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Classes, DAV_Classes, DAV_Complex, DAV_DspFilter;

type
  TCustomButterworthFilterClass = class of TCustomButterworthFilter;
  TCustomButterworthFilter = class(TCustomOrderFilter, IDspProcessor32,
    IDspProcessor64)
  private
    procedure SetDownsamplePower(Value: Integer);
  protected
    FDownsamplePow  : Integer;
    FDownsampleFak  : Integer;
    FFilterGain     : Double;
    FOrderInv       : Double;
    FPiHalfOrderInv : Double;
    FExpOrdPiHalf   : TComplex64;
    FTanW0          : Double;
    FCoeffs         : array [0..63] of Double;
    FState          : array [0..63] of Double;
    FStateStack     : array of array [0.. 63] of Double;
    procedure CalculateW0; override;
    class function GetMaxOrder: Cardinal; override;
    procedure OrderChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(const Order: Integer = 0); override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    procedure SetFilterValues(const AFrequency, AGain : Single); virtual;
    procedure ResetStates; override;
    procedure Reset; override;
    procedure PushStates; override;
    procedure PopStates; override;
    procedure ResetStatesInt64; override;
    function Imaginary(const Frequency: Double): Double; override;
    function Real(const Frequency: Double): Double; override;
    property DownsampleAmount : Integer read FDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read FDownsampleFak;
  end;

  TCustomButterworthLowPassFilter = class(TCustomButterworthFilter)
  public
    constructor Create(const Order: Integer = 0); override;
    procedure CalculateCoefficients; override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real: Double; out Imaginary: Double); override;
  end;

  TCustomButterworthHighPassFilter = class(TCustomButterworthFilter)
  public
    constructor Create(const Order: Integer = 0); override;
    procedure CalculateCoefficients; override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real: Double; out Imaginary: Double); override;
  end;

  TCustomButterworthSplitBandFilter = class(TCustomButterworthFilter,
    IDspSplitter32, IDspSplitter64)
  protected
    FKs      : Double;
    FHPState : array [0..63] of Double;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(const Order: Integer = 0); override;

    procedure CalculateCoefficients; override;
    procedure ProcessSample32(Input: Single; out Lowpass, Highpass: Single); reintroduce; overload; virtual;
    procedure ProcessSample64(Input: Double; out Lowpass, Highpass: Double); reintroduce; overload; virtual;
    function ProcessSample64(Input: Double): Double; overload; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary: Double); override;
    procedure ResetStates; override;
  end;

  TButterworthHighPassFilter = class(TCustomButterworthHighPassFilter)
  published
    property Gain;
    property Order;
    property Frequency;
    property SampleRate;
  end;
  TButterworthLowCutFilter = TCustomButterworthHighPassFilter;

  TButterworthLowPassFilter = class(TCustomButterworthLowPassFilter)
  published
    property Gain;
    property Order;
    property Frequency;
    property SampleRate;
  end;
  TButterworthHighCutFilter = TCustomButterworthLowPassFilter;

  TButterworthSplitBandFilter = class(TCustomButterworthSplitBandFilter)
  published
    property Gain;
    property Order;
    property Frequency;
    property SampleRate;
  end;

  TButterworthLowPassFilterAutomatable = class(TCustomButterworthLowPassFilter)
  public
    procedure CalculateW0; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
  published
    property Gain;
    property Order;
    property Frequency;
    property SampleRate;
  end;

  TButterworthHighPassFilterAutomatable = class(TCustomButterworthHighPassFilter)
  public
    procedure CalculateW0; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
  published
    property Gain;
    property Order;
    property Frequency;
    property SampleRate;
  end;


//  TCustomButterworthLowPassFilterAutomatable = class(TCustomButterworthLowPassFilter);

implementation

uses
  Math, SysUtils, DAV_Common, DAV_Math, DAV_Approximations;

{$IFDEF HandleDenormals}
var
  DenormRandom : Single;
const
  CDenorm32    : Single = 1E-24;
  CDenorm64    : Double = 1E-34;
{$ENDIF}

constructor TCustomButterworthFilter.Create(const Order: Integer = 0);
begin
 FDownsamplePow := 0;
 FDownsampleFak := 1;
 FFilterGain    := 1;
 inherited Create(Order);
 CalculateCoefficients;
end;

class function TCustomButterworthFilter.GetMaxOrder: Cardinal;
begin
 Result := 64;
end;

procedure TCustomButterworthFilter.Reset;
begin
 Gain := 0;
end;

procedure TCustomButterworthFilter.ResetStates;
begin
 FillChar(FState[0], FOrder * SizeOf(Double), 0);
end;

procedure TCustomButterworthFilter.ResetStatesInt64;
begin
 PInt64(@FState[0])^ := 0;
 PInt64(@FState[1])^ := 0;
end;

procedure TCustomButterworthFilter.SetDownsamplePower(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FDownsamplePow <> Value then
  begin
   FDownsamplePow := Value;
   FDownsampleFak := round(IntPower(2, FDownsamplePow));
   CalculateW0;
  end;
end;

procedure TCustomButterworthFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomButterworthFilter then
  with TCustomButterworthFilter(Dest) do
   begin
    inherited;
    FDownsamplePow  := Self.FDownsamplePow;
    FDownsampleFak  := Self.FDownsampleFak;
    FFilterGain     := Self.FFilterGain;
    FOrderInv       := Self.FOrderInv;
    FPiHalfOrderInv := Self.FPiHalfOrderInv;
    FExpOrdPiHalf   := Self.FExpOrdPiHalf;
    FTanW0          := Self.FTanW0;
    FCoeffs         := Self.FCoeffs;
    FState          := Self.FState;
    FStateStack     := Self.FStateStack;
   end
 else inherited;
end;

procedure TCustomButterworthFilter.CalculateW0;
begin
 FW0 := 2 * Pi * SampleRateReciprocal * (Frequency * FDownsampleFak);
 FTanW0 := Tan(FW0 * CHalf64);
end;

procedure TCustomButterworthFilter.SetFilterValues(const AFrequency, AGain : Single);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 FFrequency := AFrequency;
 FGain_dB := AGain;
 FGainFactor := Exp(FGain_dB * ln10_0025);
 CalculateW0;
end;

function TCustomButterworthFilter.Real(const Frequency: Double): Double;
var
  Temp: Double;
begin
 Complex(Frequency, Result, Temp);
end;

function TCustomButterworthFilter.Imaginary(const Frequency: Double): Double;
var
  Temp: Double;
begin
 Complex(Frequency, Temp, Result);
end;

procedure TCustomButterworthFilter.OrderChanged;
begin
 if FOrder > 0 then
  begin
   FOrderInv := 1 / FOrder;
   FPiHalfOrderInv := PI * CHalf64 * FOrderInv;
   GetSinCos(FPiHalfOrderInv, FExpOrdPiHalf.Im, FExpOrdPiHalf.Re);
   inherited;
  end
 else
  begin
   FFilterGain := FGainFactor;
  end; 
end;

function TCustomButterworthFilter.MagnitudeLog10(const Frequency: Double): Double;
begin
 Result := 10 * Log10(MagnitudeSquared(Frequency));
end;

procedure TCustomButterworthFilter.PopStates;
begin
 if Length(FStateStack) > 0 then
  begin
   Move(FStateStack[0, 0], FState[0], Length(FStateStack[0]) * SizeOf(Double));
   if Length(FStateStack) > 1
    then Move(FStateStack[1, 0],FStateStack[0, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
   SetLength(FStateStack, Length(FStateStack) - 1);
  end;
end;

procedure TCustomButterworthFilter.PushStates;
begin
 SetLength(FStateStack, Length(FStateStack) + 1);
 if Length(FStateStack) > 1
  then Move(FStateStack[0, 0], FStateStack[1, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
 Move(FState[0], FStateStack[0, 0], Length(FStateStack[0]) * SizeOf(Double));
end;

{ TButterworthFilterLP }

constructor TCustomButterworthLowPassFilter.Create(const Order: Integer = 0);
begin
 inherited Create(Order);
end;

procedure TCustomButterworthLowPassFilter.CalculateCoefficients;
var
  i           : Integer;
  K, K2, t, a : Double;
  Cmplx       : TComplex64;
begin
 if FOrder = 0 then exit;
 FFilterGain := FGainFactorSquared;
 K := FTanW0;
 K2 := K * K;
 Cmplx := FExpOrdPiHalf;

 i := 0;
 while i < Integer(FOrder) - 1 do
  begin
   a := 2 * Cmplx.Im * K; // 2 * sin((i + 1) * FPiHalfOrderInv) * K;
   ComplexMultiply2Inplace64(Cmplx, FExpOrdPiHalf);
   t := 1 / (K2 + a + 1);
   FFilterGain := FFilterGain * t * K2;
   FCoeffs[i    ] := 2 * (1 - K2) * t;
   FCoeffs[i + 1] := (a - 1 - K2) * t;
   Inc(i, 2);
  end;
 if i < Integer(FOrder) then
  begin
   t := 1 / (1 + K);
   FFilterGain := FFilterGain * t * K;
   FCoeffs[i] := (1 - K) * t;
  end;
end;

function TCustomButterworthLowPassFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2 * cos(2 * Frequency * Pi * SampleRateReciprocal); a := Sqr(cw + 2);
 Result := Sqr(FFilterGain);
 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a / (1 + Sqr(FCoeffs[2 * i]) +
       Sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] +
       cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw + 2) / (1 + Sqr(FCoeffs[2 * i]) - cw * FCoeffs[2 * i]);
  end;
 Result := {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF} Abs(Result);
end;

function TCustomButterworthLowPassFilter.Phase(const Frequency: Double): Double;
var
  Cmplx : array [0..1] of TComplex64;
  i     : Integer;
begin
(*
  Complex(Frequency, Cmplx[1].Re, Cmplx[1].Im);
*)
 GetSinCos(2 * Frequency * Pi * SampleRateReciprocal, Cmplx[0].Im, Cmplx[0].Re);
 Cmplx[1].Im := 0; Cmplx[1].Re := 1;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   ComplexMultiplyInplace64(Cmplx[1].Re, Cmplx[1].Im,
     (Cmplx[0].Re * (1 - FCoeffs[2 * i + 1] - FCoeffs[2 * i] + Cmplx[0].Re * (1 - FCoeffs[2 * i + 1])) - FCoeffs[2 * i]),
     (Cmplx[0].Im * (1 + FCoeffs[2 * i + 1]) * (Cmplx[0].Re + 1)));
  end;
 Result := ArcTan2(Cmplx[1].Im, Cmplx[1].Re);
end;

procedure TCustomButterworthLowPassFilter.Complex(const Frequency: Double; out Real,
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
   A.Re :=  2 * Cmplx.Re * (Cmplx.Re + 1);
   A.Im := -2 * Cmplx.Im * (Cmplx.Re + 1);
   B.Re :=  1 - FCoeffs[2 * i] * Cmplx.Re - FCoeffs[2 * i + 1] * (2 * Sqr(Cmplx.Re) - 1);
   B.Im :=  Cmplx.Im * (FCoeffs[2 * i] + 2 * Cmplx.Re * FCoeffs[2 * i + 1]);
   R := ComplexMultiply64(R, ComplexDivide64(A, B));
   Inc(i);
  end;

 if FOrder mod 2 = 1 then
  begin
   A.Re :=  Cmplx.Re + 1;
   A.Im := -Cmplx.Im;
   B.Re := -Cmplx.Re * FCoeffs[2 * i] + 1;
   B.Im :=  Cmplx.Im * FCoeffs[2 * i];
   R := ComplexMultiply64(R, ComplexDivide64(A, B));
  end;

 Real := R.Re;
 Imaginary := R.Im;
end;

function TCustomButterworthLowPassFilter.ProcessSample32(Input: Single): Single;
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   Input := Result;
   Result            :=     Input + FState[2 * i];
   FState[2 * i    ] := 2 * Input + FCoeffs[2 * i] * Result + FState[2 * i + 1];
   FState[2 * i + 1] :=     Input + FCoeffs[2 * i + 1] * Result;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Input         := Result;
   Result        := Input + FState[2 * i];
   FState[2 * i] := Input + FCoeffs[2 * i] * Result;
  end;
{$ELSE}
asm
{$IFDEF CPUx86_64}
{$ELSE}
    FLD     Input.Single;
    {$IFDEF HandleDenormals}
    FADD    CDenorm32
    {$ENDIF}
    FMUL    [EAX.FFilterGain].Double
    MOV     ECX, [EAX.FOrder]
    TEST    ECX, ECX
    JZ      @End
    SHR     ECX, 1
    SHL     ECX, 2
    PUSH    ECX
    JZ      @SingleStage

@FilterLoop:
    SUB     ECX, 4
    FLD     ST(0)
    FADD    [EAX.FState + ECX * 4].Double
    FLD     ST(0)
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FADD    [EAX.FState + ECX * 4 + 8].Double
    FLD     ST(3)
    FADD    ST(0), ST(0)
    FADDP   ST(1), ST(0)
    FSTP    [EAX.FState + ECX * 4].Double
    FMUL    [EAX.FCoeffs + ECX * 4 + 8].Double
    FXCH
    FXCH    ST(2)
    FADDP   ST(1), ST(0)
    FSTP    [EAX.FState + ECX * 4 + 8].Double
    JA      @FilterLoop

@SingleStage:
    POP     ECX
    SHR     ECX, 1
    SUB     ECX, [EAX.FOrder]
    JZ      @End
    MOV     ECX, [EAX.FOrder]
    DEC     ECX
    SHL     ECX, 1
    FLD     ST(0)
    FADD    [EAX.FState + ECX * 4].Double
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FADDP   ST(2), ST(0)
    FXCH
    FSTP    [EAX.FState + ECX * 4].Double
{$ENDIF}
@End:
{$ENDIF}
end;

function TCustomButterworthLowPassFilter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   Input := Result;
   Result            :=     Input + FState[2 * i];
   FState[2 * i    ] := 2 * Input + FCoeffs[2 * i] * Result + FState[2 * i + 1];
   FState[2 * i + 1] :=     Input + FCoeffs[2 * i + 1] * Result;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Input         := Result;
   Result        := Input + FState[2 * i];
   FState[2 * i] := Input + FCoeffs[2 * i] * Result;
  end;
{$ELSE}
asm
{$IFDEF CPUx86_64}
{$ELSE}
    FLD     Input.Double;
    {$IFDEF HandleDenormals}
    FADD    CDenorm32
    {$ENDIF}
    FMUL    [EAX.FFilterGain].Double
    MOV     ECX, [EAX.FOrder]
    TEST    ECX, ECX
    JZ      @End
    SHR     ECX, 1
    SHL     ECX, 2
    PUSH    ECX
    JZ      @SingleStage

@FilterLoop:
    SUB     ECX, 4
    FLD     ST(0)
    FADD    [EAX.FState + ECX * 4].Double
    FLD     ST(0)
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FADD    [EAX.FState + ECX * 4 + 8].Double
    FLD     ST(3)
    FADD    ST(0), ST(0)
    FADDP   ST(1), ST(0)
    FSTP    [EAX.FState + ECX * 4].Double
    FMUL    [EAX.FCoeffs + ECX * 4 + 8].Double
    FXCH
    FXCH    ST(2)
    FADDP   ST(1), ST(0)
    FSTP    [EAX.FState + ECX * 4 + 8].Double
    JA      @FilterLoop

@SingleStage:
    POP     ECX
    SHR     ECX, 1
    SUB     ECX, [EAX.FOrder]
    JZ      @End
    MOV     ECX, [EAX.FOrder]
    DEC     ECX
    SHL     ECX, 1
    FLD     ST(0)
    FADD    [EAX.FState + ECX * 4].Double
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FADDP   ST(2), ST(0)
    FXCH
    FSTP    [EAX.FState + ECX * 4].Double
{$ENDIF}
@End:
{$ENDIF}
end;

{ TButterworthFilterHP }

constructor TCustomButterworthHighPassFilter.Create(const Order: Integer = 0);
begin
 inherited Create(Order);
 {$IFDEF HandleDenormals}
 DenormRandom := Random;
 {$ENDIF}
end;

procedure TCustomButterworthHighPassFilter.CalculateCoefficients;
var
  i           : Integer;
  K, K2, t, a : Double;
  Cmplx       : TComplex64;
begin
 if FOrder = 0 then exit;
 FFilterGain := Sqr(FGainFactor);
 K := FTanW0;
 K2 := K * K;

 i := 0;
 Cmplx := FExpOrdPiHalf;
 while i < Integer(FOrder) - 1 do
  begin
   a := 2 * K * Cmplx.Im;
   ComplexMultiply2Inplace64(Cmplx, FExpOrdPiHalf);

   t := 1 / (K2 + a + 1);
   FFilterGain := FFilterGain * t;
   FCoeffs[i    ] := -2 * (K2 - 1) * t;
   FCoeffs[i + 1] := (a - K2 - 1) * t;
   Inc(i, 2);
  end;
 if i < Integer(FOrder) then
  begin
   t := 1 / (K + 1);
   FFilterGain := FFilterGain * t;
   FCoeffs[i] := (1 - K) * t;
  end;
end;

function TCustomButterworthHighPassFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw     := 2 * cos(2 * Frequency * pi * fSRR);
 a      := Sqr(cw - 2);
 Result := Sqr(FFilterGain);

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a /
  (1 + Sqr(FCoeffs[2 * i]) + Sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] +
   cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));

 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw - 2) / (1 + Sqr(FCoeffs[2 * i]) - cw * FCoeffs[2 * i]);
  end;
 Result := {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF} Abs(Result);
end;

function TCustomButterworthHighPassFilter.Phase(const Frequency: Double): Double;
var
  cw, sw   : Double;
  Nom, Den : Double;
  i        : Integer;
begin
(*
 Complex(Frequency, Den, Nom);
*)
 GetSinCos(2 * Frequency * Pi * SampleRateReciprocal, sw, cw);
 Nom := 0; Den := 1;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   ComplexMultiplyInplace64(Den, Nom,
     (cw * (FCoeffs[2 * i + 1] - FCoeffs[2 * i] - 1 + cw * (1 - FCoeffs[2 * i + 1])) + FCoeffs[2 * i]),
     (sw * (FCoeffs[2 * i + 1] + 1) * (cw - 1)));
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   ComplexMultiplyInplace64(Den, Nom, (1 + FCoeffs[2 * i]) * (1 - cw),
     sw * (FCoeffs[2 * i] - 1));
  end;
 Result := ArcTan2(Nom, Den);
end;

procedure TCustomButterworthHighPassFilter.Complex(const Frequency: Double; out Real,
  Imaginary: Double);
var
  i     : Cardinal;
  Cmplx : TComplex64;
  A, R  : TComplex64;
begin
 GetSinCos(2 * Pi * Frequency * FSRR, Cmplx.Im, Cmplx.Re);

 R.Re := FFilterGain;
 R.Im := 0;

 i := 0;
 while i < (FOrder div 2) do
  begin
   A.Re :=  2 * Cmplx.Re * (Cmplx.Re - 1);
   A.Im := -2 * Cmplx.Im * (Cmplx.Re - 1);
   R := ComplexMultiply64(R, A);

   A.Re :=  1 - FCoeffs[2 * i] * Cmplx.Re - FCoeffs[2 * i + 1] * (2 * Sqr(Cmplx.Re) - 1);
   A.Im :=  Cmplx.Im * (FCoeffs[2 * i] + 2 * Cmplx.Re * FCoeffs[2 * i + 1]);
   R := ComplexDivide64(R, A);

   Inc(i);
  end;

 if FOrder mod 2 = 1 then
  begin
   A.Re :=  Cmplx.Re - 1;
   A.Im := -Cmplx.Im;
   R := ComplexMultiply64(R, A);

   A.Re := -Cmplx.Re * FCoeffs[2 * i] + 1;
   A.Im :=  Cmplx.Im * FCoeffs[2 * i];
   R := ComplexDivide64(R, A);
  end;

 Real := R.Re;
 Imaginary := R.Im;
end;

function TCustomButterworthHighPassFilter.ProcessSample32(Input: Single): Single;
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   Input := Result;
   Result            :=      Input + FState[2 * i];
   FState[2 * i    ] := -2 * Input + FCoeffs[2 * i] * Result + FState[2 * i + 1];
   FState[2 * i + 1] :=      Input + FCoeffs[2 * i + 1] * Result;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i             := ((FOrder + 1) div 2) - 1;
   Input         := Result;
   Result        :=  Input + FState[2 * i];
   FState[2 * i] := -Input + FCoeffs[2 * i] * Result;
  end;
{$ELSE}
asm
{$IFDEF CPUx86_64}
{$ELSE}
    FLD     Input.Single

    // eventually add denormal
    {$IFDEF HandleDenormals}
    MOV     EDX, DenormRandom
    IMUL    EDX, DenormRandom, $08088405
    INC     EDX
    SHR     EDX, 23
    OR      EDX, $20000000
    MOV     DenormRandom, EDX
    FADD    DenormRandom
    {$ENDIF}

    FMUL    [EAX.FFilterGain].Double
    MOV     ECX, [EAX.FOrder]
    TEST    ECX, ECX
    JZ      @End
    SHR     ECX, 1
    SHL     ECX, 2
    PUSH    ECX
    JZ      @SingleStage

@FilterLoop:
    SUB     ECX, 4
    FLD     ST(0)
    FADD    [EAX.FState + ECX * 4].Double
    FLD     ST(0)
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FADD    [EAX.FState + ECX * 4 + 8].Double
    FLD     ST(3)
    FADD    ST(0), ST(0)
    FSUBP
    FSTP    [EAX.FState + ECX * 4].Double
    FMUL    [EAX.FCoeffs + ECX * 4 + 8].Double
    FXCH
    FXCH    ST(2)
    FADDP   ST(1), ST(0)
    FSTP    [EAX.FState + ECX * 4 + 8].Double
    JA      @FilterLoop

@SingleStage:
    POP     ECX
    SHR     ECX, 1
    SUB     ECX, [EAX.FOrder]

    JZ      @End
    MOV     ECX, [EAX.FOrder]
    DEC     ECX
    SHL     ECX, 1
    FLD     ST(0)
    FADD    [EAX.FState + ECX * 4].Double
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FSUBRP  ST(2), ST(0)
    FXCH
    FSTP    [EAX.FState + ECX * 4].Double
{$ENDIF}
@End:
{$ENDIF}
end;

function TCustomButterworthHighPassFilter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result            :=      x + FState[2 * i];
   FState[2 * i    ] := -2 * x + FCoeffs[2 * i] * Result + FState[2 * i + 1];
   FState[2 * i + 1] :=      x + FCoeffs[2 * i + 1] * Result;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i             := ((FOrder + 1) div 2) - 1;
   x             := Result;
   Result        :=  x + FState[2 * i];
   FState[2 * i] := -x + FCoeffs[2 * i] * Result;
  end;
{$ELSE}
asm
{$IFDEF CPUx86_64}
{$ELSE}
    FLD     Input.Double

    // eventually add denormal
    {$IFDEF HandleDenormals}
    MOV     EDX, DenormRandom
    IMUL    EDX, DenormRandom, $08088405
    INC     EDX
    SHR     EDX, 23
    OR      EDX, $20000000
    MOV     DenormRandom, EDX
    FADD    DenormRandom
    {$ENDIF}

    FMUL    [EAX.FFilterGain].Double
    MOV     ECX, [EAX.FOrder]
    TEST    ECX, ECX
    JZ      @End
    SHR     ECX, 1
    SHL     ECX, 2
    PUSH    ECX
    JZ      @SingleStage

@FilterLoop:
    SUB     ECX, 4
    FLD     ST(0)
    FADD    [EAX.FState + ECX * 4].Double
    FLD     ST(0)
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FADD    [EAX.FState + ECX * 4 + 8].Double
    FLD     ST(3)
    FADD    ST(0), ST(0)
    FSUBP
    FSTP    [EAX.FState + ECX * 4].Double
    FMUL    [EAX.FCoeffs + ECX * 4 + 8].Double
    FXCH
    FXCH    ST(2)
    FADDP   ST(1), ST(0)
    FSTP    [EAX.FState + ECX * 4 + 8].Double
    JA      @FilterLoop

@SingleStage:
    POP     ECX
    SHR     ECX, 1
    SUB     ECX, [EAX.FOrder]
    JZ      @End
    MOV     ECX, [EAX.FOrder]
    DEC     ECX
    SHL     ECX, 1
    FLD     ST(0)
    FADD    [EAX.FState + ECX * 4].Double
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FSUBRP  ST(2), ST(0)
    FXCH
    FSTP    [EAX.FState + ECX * 4].Double
{$ENDIF}
@End:
{$ENDIF}
end;

{ TCustomButterworthSplitBandFilter }

constructor TCustomButterworthSplitBandFilter.Create(const Order: Integer = 0);
begin
 inherited Create(Order);
 Randomize;
 {$IFDEF HandleDenormals}
 DenormRandom := Random;
 {$ENDIF}
end;

procedure TCustomButterworthSplitBandFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomButterworthSplitBandFilter then
  with TCustomButterworthSplitBandFilter(Dest) do
   begin
    inherited;
    FKs      := Self.FKs;
    FHPState := Self.FHPState;
   end
 else inherited;
end;

procedure TCustomButterworthSplitBandFilter.CalculateCoefficients;
var
  i           : Integer;
  K, K2, t, a : Double;
  Cmplx       : TComplex64;
begin
 if FOrder = 0 then exit;
 FFilterGain := FGainFactorSquared;
 K := FTanW0;
 K2 := K * K;
 Cmplx := FExpOrdPiHalf;
 FKs := IntPower(K, FOrder);
 i := 0;
 while i < Integer(FOrder) - 1 do
  begin
   a := 2 * Cmplx.Im * K;
   ComplexMultiply2Inplace64(Cmplx, FExpOrdPiHalf);
   t := 1 / (K2 + a + 1);
   FFilterGain := FFilterGain * t;
   FCoeffs[i    ] := 2 * (1 - K2) * t;
   FCoeffs[i + 1] := (a - 1 - K2) * t;
   Inc(i, 2);
  end;
 if i < Integer(FOrder) then
  begin
   t := 1 / (1 + K);
   FFilterGain := FFilterGain * t;
   FCoeffs[i] := (1 - K) * t;
  end;
end;

function TCustomButterworthSplitBandFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  Cmplx : TComplex64;
begin
 Complex(Frequency, Cmplx.Re, Cmplx.Im);
 Result := Sqr(Cmplx.Re) + Sqr(Cmplx.Im);
end;

procedure TCustomButterworthSplitBandFilter.Complex(const Frequency: Double;
  out Real, Imaginary: Double);
var
  i     : Cardinal;
  Cmplx : TComplex64;
  A, R  : TComplex64;
begin
 GetSinCos(2 * Pi * Frequency * FSRR, Cmplx.Im, Cmplx.Re);

 // lowpass
 Real := FFilterGain * FKs * IntPower(2 * (Cmplx.Re + 1), FOrder div 2);
 Imaginary := 0;

 // highpass
 R.Re := FFilterGain * IntPower(2 * (Cmplx.Re - 1), FOrder div 2);
 R.Im := 0;

 i := 0;
 while i < (FOrder div 2) do
  begin
   ComplexMultiplyInplace64(Real, Imaginary, Cmplx.Re, -Cmplx.Im);
   ComplexMultiplyInplace64(R.Re, R.Im, Cmplx.Re, -Cmplx.Im);
   Inc(i);
  end;

 if FOrder mod 2 = 1 then
  begin
   ComplexMultiplyInplace64(R.Re, R.Im, Cmplx.Re - 1, -Cmplx.Im);
   ComplexMultiplyInplace64(Real, Imaginary, Cmplx.Re + 1, -Cmplx.Im);
  end;

 Real := R.Re + Real;
 Imaginary := R.Im + Imaginary;

 // calculate divider
 i := 0;
 while i < (FOrder div 2) do
  begin
   A.Re :=  1 - FCoeffs[2 * i] * Cmplx.Re - FCoeffs[2 * i + 1] * (2 * Sqr(Cmplx.Re) - 1);
   A.Im :=  Cmplx.Im * (FCoeffs[2 * i] + 2 * Cmplx.Re * FCoeffs[2 * i + 1]);
   ComplexDivideInplace64(Real, Imaginary, A.Re, A.Im);
   Inc(i);
  end;

 if FOrder mod 2 = 1 then
  begin
   A.Re := -Cmplx.Re * FCoeffs[2 * i] + 1;
   A.Im :=  Cmplx.Im * FCoeffs[2 * i];
   ComplexDivideInplace64(Real, Imaginary, A.Re, A.Im);
  end;
end;

procedure TCustomButterworthSplitBandFilter.ProcessSample32(Input: Single; out Lowpass,
  Highpass: Single);
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Highpass := FFilterGain * Input;
 Lowpass  := FFilterGain * Input * FKs;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Lowpass;
   Lowpass             :=      x + FState[2 * i];
   FState[2 * i    ]   :=  2 * x + FCoeffs[2 * i] * Lowpass + FState[2 * i + 1];
   FState[2 * i + 1]   :=      x + FCoeffs[2 * i + 1] * Lowpass;

   x := Highpass;
   Highpass            :=      x + FHPState[2 * i];
   FHPState[2 * i    ] := -2 * x + FCoeffs[2 * i] * Highpass + FHPState[2 * i + 1];
   FHPState[2 * i + 1] :=      x + FCoeffs[2 * i + 1] * Highpass;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   x               :=  Lowpass;
   Lowpass        :=   x + FState[2 * i];
   FState[2 * i]   :=  x + FCoeffs[2 * i] * Lowpass;

   x               :=  Highpass;
   Highpass        :=  x + FHPState[2 * i];
   FHPState[2 * i] := -x + FCoeffs[2 * i] * Highpass;
  end;
{$ELSE}
asm
{$IFDEF CPUx86_64}
{$ELSE}
    FLD     Input.Single               // highpass

    // eventuall add denormal
    {$IFDEF HandleDenormals}
    PUSH    EBX
    MOV     EBX, DenormRandom
    IMUL    EBX, DenormRandom, $08088405
    INC     EBX
    SHR     EBX, 23
    OR      EBX, $20000000
    MOV     DenormRandom, EBX
    FADD    DenormRandom
    POP     EBX
    {$ENDIF}

    FMUL    [EAX.FFilterGain].Double
    FLD     ST(0)                      // lowpass, highpass
    FMUL    [EAX.FKs].Double
    PUSH    ECX
    MOV     ECX, [EAX.FOrder]
    TEST    ECX, ECX
    JZ      @End
    SHR     ECX, 1
    SHL     ECX, 2
    PUSH    ECX
    JZ      @SingleStage

@FilterLoop:
    SUB     ECX, 4

    // lowpass
    FLD     ST(0)
    FADD    [EAX.FState + ECX * 4].Double
    FLD     ST(0)
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FADD    [EAX.FState + ECX * 4 + 8].Double
    FLD     ST(3)
    FADD    ST(0), ST(0)
    FADDP   ST(1), ST(0)
    FSTP    [EAX.FState + ECX * 4].Double
    FMUL    [EAX.FCoeffs + ECX * 4 + 8].Double
    FXCH
    FXCH    ST(2)
    FADDP   ST(1), ST(0)
    FSTP    [EAX.FState + ECX * 4 + 8].Double
    FXCH

    // highpass
    FLD     ST(0)
    FADD    [EAX.FHPState + ECX * 4].Double
    FLD     ST(0)
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FADD    [EAX.FHPState + ECX * 4 + 8].Double
    FLD     ST(3)
    FADD    ST(0), ST(0)
    FSUBP
    FSTP    [EAX.FHPState + ECX * 4].Double
    FMUL    [EAX.FCoeffs + ECX * 4 + 8].Double
    FXCH
    FXCH    ST(2)
    FADDP   ST(1), ST(0)
    FSTP    [EAX.FHPState + ECX * 4 + 8].Double
    FXCH
    JA      @FilterLoop

@SingleStage:
    POP     ECX
    SHR     ECX, 1
    SUB     ECX, [EAX.FOrder]
    JZ      @End
    MOV     ECX, [EAX.FOrder]
    DEC     ECX
    SHL     ECX, 1

    // lowpass
    FLD     ST(0)
    FADD    [EAX.FState + ECX * 4].Double
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FADDP   ST(2), ST(0)
    FXCH
    FSTP    [EAX.FState + ECX * 4].Double
    FXCH

    // highpass
    FLD     ST(0)
    FADD    [EAX.FHPState + ECX * 4].Double
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FSUBRP  ST(2), ST(0)
    FXCH
    FSTP    [EAX.FHPState + ECX * 4].Double
    FXCH

@End:
    FSTP    [Lowpass].Single
    POP     ECX
    FSTP    [Highpass].Single
{$ENDIF}
{$ENDIF}
end;

procedure TCustomButterworthSplitBandFilter.ProcessSample64(Input: Double; out Lowpass,
  Highpass: Double);
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Highpass := {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF} FFilterGain * Input;
 Lowpass  := {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF} FFilterGain * Input * FKs;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Lowpass;
   Lowpass             :=      x + FState[2 * i];
   FState[2 * i    ]   :=  2 * x + FCoeffs[2 * i] * Lowpass + FState[2 * i + 1];
   FState[2 * i + 1]   :=      x + FCoeffs[2 * i + 1] * Lowpass;

   x := Highpass;
   Highpass            :=      x + FHPState[2 * i];
   FHPState[2 * i    ] := -2 * x + FCoeffs[2 * i] * Highpass + FHPState[2 * i + 1];
   FHPState[2 * i + 1] :=      x + FCoeffs[2 * i + 1] * Highpass;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   x               :=  Lowpass;
   Lowpass        :=   x + FState[2 * i];
   FState[2 * i]   :=  x + FCoeffs[2 * i] * Lowpass;

   x               :=  Highpass;
   Highpass        :=  x + FHPState[2 * i];
   FHPState[2 * i] := -x + FCoeffs[2 * i] * Highpass;
  end;
{$ELSE}
asm
{$IFDEF CPUx86_64}
{$ELSE}
    FLD     Input.Double               // highpass
    FMUL    [EAX.FFilterGain].Double

    // eventually add denormal
    {$IFDEF HandleDenormals}
    PUSH    EBX
    MOV     EBX, DenormRandom
    IMUL    EBX, DenormRandom, $08088405
    INC     EBX
    SHR     EBX, 23
    OR      EBX, $20000000
    MOV     DenormRandom, EBX
    FADD    DenormRandom
    POP     EBX
    {$ENDIF}

    FLD     ST(0)                       // lowpass, highpass
    FMUL    [EAX.FKs].Double
    PUSH    ECX
    MOV     ECX, [EAX.FOrder]
    TEST    ECX, ECX
    JZ      @End
    SHR     ECX, 1
    SHL     ECX, 2
    PUSH    ECX
    JZ      @SingleStage
@FilterLoop:
    SUB     ECX, 4

    // lowpass
    FLD     ST(0)
    FADD    [EAX.FState + ECX * 4].Double
    FLD     ST(0)
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FADD    [EAX.FState + ECX * 4 + 8].Double
    FLD     ST(3)
    FADD    ST(0), ST(0)
    FADDP   ST(1), ST(0)
    FSTP    [EAX.FState + ECX * 4].Double
    FMUL    [EAX.FCoeffs + ECX * 4 + 8].Double
    FXCH
    FXCH    ST(2)
    FADDP   ST(1), ST(0)
    FSTP    [EAX.FState + ECX * 4 + 8].Double
    FXCH

    // highpass
    FLD     ST(0)
    FADD    [EAX.FHPState + ECX * 4].Double
    FLD     ST(0)
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FADD    [EAX.FHPState + ECX * 4 + 8].Double
    FLD     ST(3)
    FADD    ST(0), ST(0)
    FSUBP
    FSTP    [EAX.FHPState + ECX * 4].Double
    FMUL    [EAX.FCoeffs + ECX * 4 + 8].Double
    FXCH
    FXCH    ST(2)
    FADDP   ST(1), ST(0)
    FSTP    [EAX.FHPState + ECX * 4 + 8].Double
    FXCH
    JA      @FilterLoop

@SingleStage:
    POP     ECX
    SHR     ECX, 1
    SUB     ECX, [EAX.FOrder]
    JZ      @End
    MOV     ECX, [EAX.FOrder]
    DEC     ECX
    SHL     ECX, 1

    // lowpass
    FLD     ST(0)
    FADD    [EAX.FState + ECX * 4].Double
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FADDP   ST(2), ST(0)
    FXCH
    FSTP    [EAX.FState + ECX * 4].Double
    FXCH

    // highpass
    FLD     ST(0)
    FADD    [EAX.FHPState + ECX * 4].Double
    FLD     ST(0)
    FMUL    [EAX.FCoeffs + ECX * 4].Double
    FSUBRP  ST(2), ST(0)
    FXCH
    FSTP    [EAX.FHPState + ECX * 4].Double
    FXCH

@End:
    FSTP     [Lowpass].Double
    POP      ECX
    FSTP     [Highpass].Double
{$ENDIF}
{$ENDIF}
end;

function TCustomButterworthSplitBandFilter.ProcessSample64(
  Input: Double): Double;
begin
 raise Exception.Create('Please use the function ProcessSample!');
end;

procedure TCustomButterworthSplitBandFilter.ResetStates;
begin
 inherited;
 FillChar(FHPState[0], Length(FHPState) * SizeOf(Double), 0);
end;


{ TButterworthLowPassFilterAutomatable }

procedure TButterworthLowPassFilterAutomatable.CalculateW0;
begin
 FW0 := 2 * Pi * SampleRateReciprocal * (Frequency * FDownsampleFak);
 FTanW0 := FastTanInBounds4Term(FW0 * CHalf64)
end;

function TButterworthLowPassFilterAutomatable.MagnitudeLog10(const Frequency: Double): Double;
const
  CLogScale : Double = 3.0102999566398119521373889472449;
begin
 Result := CLogScale * FastLog2ContinousError4(MagnitudeSquared(Frequency));
end;

function TButterworthLowPassFilterAutomatable.MagnitudeSquared(
  const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2 * FastCosInBounds3Term(2 * Frequency * Pi * SampleRateReciprocal); a := Sqr(cw + 2);
 Result := Sqr(FFilterGain);
 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a / (1 + Sqr(FCoeffs[2 * i]) +
       Sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] +
       cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw + 2) / (1 + Sqr(FCoeffs[2 * i]) - cw * FCoeffs[2 * i]);
  end;
 Result := {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF} Abs(Result);
end;

{ TButterworthHighPassFilterAutomatable }

procedure TButterworthHighPassFilterAutomatable.CalculateW0;
begin
 FW0 := 2 * Pi * SampleRateReciprocal * (Frequency * FDownsampleFak);
 FTanW0 := FastTanInBounds4Term(FW0 * CHalf32)
end;

function TButterworthHighPassFilterAutomatable.MagnitudeLog10(const Frequency: Double): Double;
const
  CLogScale : Double = 3.0102999566398119521373889472449;
begin
 Result := CLogScale * FastLog2ContinousError4(MagnitudeSquared(Frequency));
end;

function TButterworthHighPassFilterAutomatable.MagnitudeSquared(
  const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2 * FastCosInBounds3Term(2 * Frequency * Pi * SampleRateReciprocal);
 a := Sqr(cw - 2);
 Result := 1;
 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a / (1 + Sqr(FCoeffs[2 * i]) + Sqr(FCoeffs[2 * i + 1]) +
       2 * FCoeffs[2 * i + 1] + cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw - 2) / (1 + Sqr(FCoeffs[2 * i]) - cw * FCoeffs[2 * i]);
  end;

 Result := {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF} Abs(Sqr(FFilterGain) * Result);
end;

initialization
  RegisterDspProcessors32([TButterworthHighPassFilter,
    TButterworthLowPassFilter, TButterworthLowPassFilterAutomatable,
    TButterworthHighPassFilterAutomatable]);

  RegisterDspProcessors64([TButterworthHighPassFilter,
    TButterworthLowPassFilter, TButterworthLowPassFilterAutomatable,
    TButterworthHighPassFilterAutomatable]);

end.