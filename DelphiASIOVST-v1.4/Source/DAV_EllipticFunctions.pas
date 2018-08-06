{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_EllipticFunctions;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types, DAV_Complex;

function DoubleFactorial(n: Integer): Integer;
function LegendreDenominator(n: Integer): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function LegendrePolynomial(n: Integer; x: Double): Double;
function IncompleteEllipticIntegral1stKind(k: Double; Phi: Double = 0.5 * Pi; Steps: Integer = 1000): Double;
function IncompleteEllipticIntegral2ndKind(k: Double; Phi: Double = 0.5 * Pi; Steps: Integer = 1000): Double;
function IncompleteEllipticIntegral3rdKind(n, k: Double; Phi: Double = 0.5 * Pi; Steps: Integer = 1000): Double;
function CompleteEllipticIntegral1stKind(k: TComplex64; Steps: Integer = 30): TComplex64; overload;
function CompleteEllipticIntegral1stKind(k: Double; Steps: Integer = 30): Double; overload;
function CompleteEllipticIntegral2ndKind(k: Double; Steps: Integer = 1000): Double;
function CompleteEllipticIntegral3rdKind(n, k: Double; Steps: Integer = 1000): Double;
function Pochhammer(x: Double; n: Integer): Double;
function PochhammerRisingSequentialProduct(x: Double; n: Integer): Double;
function PochhammerFallingSequentialProduct(x: Double; n: Integer): Double;
function NomeQk(k: Double): Double; overload;
function NomeQm(m: Double): Double; overload;
function NomeQk(k: TComplex64): TComplex64; overload;
function NomeQm(m: TComplex64): TComplex64; overload;
function Theta00(q: Double): Double; overload;
function Theta01(q: Double): Double; overload;
function Theta10(q: Double): Double; overload;
function Theta11(q: Double): Double; overload;
function Theta00(z, q: Double): Double; overload;
function Theta01(z, q: Double): Double; overload;
function Theta10(z, q: Double): Double; overload;
function Theta11(z, q: Double): Double; overload;
function Theta00(q: TComplex64): TComplex64; overload;
function Theta01(q: TComplex64): TComplex64; overload;
function Theta10(q: TComplex64): TComplex64; overload;
function Theta11(q: TComplex64): TComplex64; overload;
function Theta00(z, q: TComplex64): TComplex64; overload;
function Theta01(z, q: TComplex64): TComplex64; overload;
function Theta10(z, q: TComplex64): TComplex64; overload;
function Theta11(z, q: TComplex64): TComplex64; overload;
function Theta1(z, q: Double): Double; overload;
function Theta2(z, q: Double): Double; overload;
function Theta3(z, q: Double): Double; overload;
function Theta4(z, q: Double): Double; overload;
function Theta1(z, q: TComplex64): TComplex64; overload;
function Theta2(z, q: TComplex64): TComplex64; overload;
function Theta3(z, q: TComplex64): TComplex64; overload;
function Theta4(z, q: TComplex64): TComplex64; overload;

function Sn(z, k: Double): Double; overload;
function Cn(z, k: Double): Double; overload;
function Dn(z, k: Double): Double; overload;
procedure JacobiEllipticFunctions(z, k: Double; out Sn, Cn, Dn: Double);

function Ns(z, k: Double): Double; overload;
function Nc(z, k: Double): Double; overload;
function Nd(z, k: Double): Double; overload;
function Sd(z, k: Double): Double; overload;
function Sc(z, k: Double): Double; overload;
function Cd(z, k: Double): Double; overload;
function Cs(z, k: Double): Double; overload;
function Dc(z, k: Double): Double; overload;
function Ds(z, k: Double): Double; overload;

function Sn(z, k: TComplex64): TComplex64; overload;
function Cn(z, k: TComplex64): TComplex64; overload;
function Dn(z, k: TComplex64): TComplex64; overload;
function Ns(z, k: TComplex64): TComplex64; overload;
function Nc(z, k: TComplex64): TComplex64; overload;
function Nd(z, k: TComplex64): TComplex64; overload;
function Sd(z, k: TComplex64): TComplex64; overload;
function Sc(z, k: TComplex64): TComplex64; overload;
function Cd(z, k: TComplex64): TComplex64; overload;
function Cs(z, k: TComplex64): TComplex64; overload;
function Dc(z, k: TComplex64): TComplex64; overload;
function Ds(z, k: TComplex64): TComplex64; overload;

function HypergeometricFunction2F1(a, b, c: Double; z: Double;
  Steps: Integer = 30): Double;

function InverseCd(z, m: Double; Steps: Integer = 10): Double; overload;
function InverseCd(z, m: TComplex64; Steps: Integer = 10): TComplex64; overload;

function RationalEllipticFunction(Order: Integer;
  Selectivity, x: Double): Double;

function EvaluatePolynom01(Input: Double; Coefficients: PDAVDoubleFixedArray;
  Length: Integer): Double;
function EvaluatePolynom11(Input: Double; Coefficients: PDAVDoubleFixedArray;
  Length: Integer): Double;
function JacobianEllipticFunctions(u, m: Double; var Sn, Cn, Dn,
  ph: Double): Integer;
function IncompleteEllipticIntegral2ndKindX(Phi, m: Double): Double;
function IncompleteEllipticIntegral1stKindX(Phi, m: Double): Double;
function CompleteEllipticIntegral1stKindX(x: Double): Double;

implementation

uses
  Math, SysUtils, DAV_Math;

const
{$IFDEF UNK}
  CMachEP = 1.11022302462515654042E-16;
{$ELSE}
  CMachEP = 1.38777878078144567553E-17;
{$ENDIF}
{$IFDEF DENORMAL}
  CMaxLog: Double = 7.09782712893383996732E2; // log(MaxNum)
  CMinLog: Double = -7.451332191019412076235E2; // log(2^-1075)
{$ELSE}
  CMaxLog: Double = 7.08396418532264106224E2; // log 2^1022
  CMinLog: Double = -7.08396418532264106224E2; // log 2^-1022
{$ENDIF}
  MaxNum: Double = 1.79769313486231570815E308; // 2^1024 * (1 - CMachEP)
  PiO2: Double = 1.57079632679489661923; // Pi / 2
  PiO4: Double = 7.85398163397448309616E-1; // Pi / 4
  Sqrt2: Double = 1.41421356237309504880; // Sqrt(2)
  SqrtH: Double = 7.07106781186547524401E-1; // Sqrt(2) / 2
  Log2E: Double = 1.4426950408889634073599; // 1 / log(2)
  Sq2OPi: Double = 7.9788456080286535587989E-1; // Sqrt(2 / Pi)
  LogE2: Double = 6.93147180559945309417E-1; // log(2)
  LogSQ2: Double = 3.46573590279972654709E-1; // log(2) / 2
  ThPiO4: Double = 2.35619449019234492885; // 3 * Pi / 4
  PiHalf: Double = 6.36619772367581343075535E-1; // 2 / Pi

function DoubleFactorial(n: Integer): Integer;
begin
  if n < 0 then
    Result := 0
  else
    Result := Factorial(2 * n) div Factorial(n) div (1 shl n);
end;

function LegendreDenominator(n: Integer): Integer;
var
  Step: Integer;
  Sum: Integer;
begin
  if n < 0 then
    Result := 0
  else
  begin
    Sum := 0;
    for Step := 1 to n do
      Sum := Sum + n div (1 shl Step);
    Result := 1 shl Sum;
  end;
end;

function LegendrePolynomial(n: Integer; x: Double): Double;
begin
  case n of
    0:
      Result := 1;
    1:
      Result := x;
    2:
      Result := 0.5 * (3 * Sqr(x) - 1);
    3:
      Result := 0.5 * (5 * Sqr(x) * x - 3 * x);
    4:
      Result := 0.125 * (35 * Sqr(Sqr(x)) - 30 * Sqr(x) + 3);
    5:
      Result := 0.125 * (63 * Sqr(Sqr(x)) * x - 70 * Sqr(x) * x + 15 * x);
  else
    raise Exception.Create('Not yet defined!');
  end;
end;

function IncompleteEllipticIntegral1stKind(k: Double; Phi: Double = 0.5 * Pi;
  Steps: Integer = 1000): Double;
var
  i: Integer;
  Scale: Double;
  Cmplx: TComplex64;
  Pos: TComplex64;
begin
  Result := 0;
  Scale := 1 / Steps;
  Pos.Re := 1;
  Pos.Im := 0;
  GetSinCos(Scale * Phi, Cmplx.Im, Cmplx.Re);
  for i := 0 to Steps do
  begin
    Result := Result + Scale / Sqrt(1 - Sqr(k * Pos.Im));

    ComplexMultiplyInplace64(Pos, Cmplx);
  end;

  Result := Phi * Result;
end;

function IncompleteEllipticIntegral2ndKind(k: Double; Phi: Double = 0.5 * Pi;
  Steps: Integer = 1000): Double;
var
  i: Integer;
  Scale: Double;
  Cmplx: TComplex64;
  Pos: TComplex64;
begin
  Result := 0;
  Scale := 1 / Steps;
  Pos.Re := 1;
  Pos.Im := 0;
  GetSinCos(Scale * Phi, Cmplx.Im, Cmplx.Re);
  for i := 0 to Steps do
  begin
    Result := Result + Scale * Sqrt(1 - Sqr(k * Pos.Im));

    ComplexMultiplyInplace64(Pos, Cmplx);
  end;

  Result := Phi * Result;
end;

function IncompleteEllipticIntegral3rdKind(n, k: Double; Phi: Double = 0.5 * Pi;
  Steps: Integer = 1000): Double;
var
  i: Integer;
  Scale: Double;
  Cmplx: TComplex64;
  Pos: TComplex64;
begin
  Result := 0;
  Scale := 1 / Steps;
  Pos.Re := 1;
  Pos.Im := 0;
  GetSinCos(Scale * Phi, Cmplx.Im, Cmplx.Re);
  for i := 0 to Steps do
  begin
    Result := Result + Scale /
      ((1 - n * Sqr(Pos.Im)) * Sqrt(1 - Sqr(k * Pos.Im)));

    ComplexMultiplyInplace64(Pos, Cmplx);
  end;

  Result := Phi * Result;
end;

/// /////////////////////////////////////////////////////////////////////////////

function CompleteEllipticIntegral1stKind(k: Double;
  Steps: Integer = 30): Double;
var
  Factor: Double;
  Temp: Double;
  Step: Integer;

  function CalculateFactor(Step: Integer): Single;
  var
    n: Integer;
  begin
    Result := 1;
    for n := 1 to Step do
      Result := Result * ((n - 0.5) / n);
    Result := Sqr(Result);
  end;

begin
  // zero & first step at once
  Factor := 0.25;
  Temp := k;
  Result := 1 + Factor * Temp;

  // second step
  Temp := Temp * k;
  Factor := 0.140625;
  Result := Result + Factor * Temp;

  // n-th step
  for Step := 3 to Steps do
  begin
    Temp := Temp * k;
    Factor := CalculateFactor(Step);
    Result := Result + Factor * Temp;
  end;
  Result := 0.5 * Pi * Result;
end;

function CompleteEllipticIntegral1stKind(k: TComplex64; Steps: Integer = 30)
  : TComplex64; overload;
var
  Factor: Double;
  Temp: TComplex64;
  Step: Integer;

  function CalculateFactor(Step: Integer): Single;
  var
    n: Integer;
  begin
    Result := 1;
    for n := 1 to Step do
      Result := Result * ((n - 0.5) / n);
    Result := Sqr(Result);
  end;

begin
  // zero & first step at once
  Factor := 0.25;
  Temp := k;
  Result.Re := 1 + Factor * Temp.Re;
  Result.Im := 0 + Factor * Temp.Im;

  // second step
  ComplexMultiplyInplace64(Temp, k);
  Factor := 0.140625;
  Result.Re := Result.Re + Factor * Temp.Re;
  Result.Im := Result.Im + Factor * Temp.Im;

  // n-th step
  for Step := 3 to 1000 do
  begin
    ComplexMultiplyInplace64(Temp, k);
    Factor := CalculateFactor(Step);
    Result.Re := Result.Re + Factor * Temp.Re;
    Result.Im := Result.Im + Factor * Temp.Im;
  end;
  Result.Re := 0.5 * Pi * Result.Re;
  Result.Im := 0.5 * Pi * Result.Im;
end;

/// /////////////////////////////////////////////////////////////////////////////

function CompleteEllipticIntegral2ndKind(k: Double;
  Steps: Integer = 1000): Double;
var
  i: Integer;
  Scale: Double;
  Cmplx: TComplex64;
  Pos: TComplex64;
begin
  Result := 0;
  Scale := 1 / Steps;
  Pos.Re := 1;
  Pos.Im := 0;
  GetSinCos(Scale * 0.5 * Pi, Cmplx.Im, Cmplx.Re);
  for i := 0 to Steps do
  begin
    Result := Result + Scale * Sqrt(1 - Sqr(k * Pos.Im));

    ComplexMultiplyInplace64(Pos, Cmplx);
  end;

  Result := 0.5 * Pi * Result;
end;

function CompleteEllipticIntegral3rdKind(n, k: Double;
  Steps: Integer = 1000): Double;
var
  i: Integer;
  Scale: Double;
  Cmplx: TComplex64;
  Pos: TComplex64;
begin
  Result := 0;
  Scale := 1 / Steps;
  Pos.Re := 1;
  Pos.Im := 0;
  GetSinCos(Scale * 0.5 * Pi, Cmplx.Im, Cmplx.Re);
  for i := 0 to Steps do
  begin
    Result := Result + Scale /
      ((1 - n * Sqr(Pos.Im)) * Sqrt(1 - Sqr(k * Pos.Im)));

    ComplexMultiplyInplace64(Pos, Cmplx);
  end;

  Result := 0.5 * Pi * Result;
end;

/// /////////////////////////////////////////////////////////////////////////////

function NomeQk(k: Double): Double;
begin
  Assert(k <= 1);
  Assert(k >= 0);
  Result := exp(-Pi * CompleteEllipticIntegral1stKind(Sqrt(1 - Sqr(k))) /
    CompleteEllipticIntegral1stKind(k));
end;

function NomeQm(m: Double): Double;
begin
  Result := exp(-Pi * CompleteEllipticIntegral1stKind(1 - m) /
    CompleteEllipticIntegral1stKind(m));
end;

function NomeQk(k: TComplex64): TComplex64; overload;
begin
  raise Exception.Create('Yet todo!');
  (*
    Result := Pi * CompleteEllipticIntegral1stKind(Sqrt(1 - Sqr(k.Re))) / CompleteEllipticIntegral1stKind(k.Re));

    Result.Re := Pi * Result.Re;
    Result.Im := Pi * Result.Im;
    Result := ComplexExp64(Result);
  *)
end;

function NomeQm(m: TComplex64): TComplex64; overload;
begin
  raise Exception.Create('Yet todo!');
end;

/// /////////////////////////////////////////////////////////////////////////////

function Theta00(z, q: Double): Double;
var
  Step: Integer;
  Current: Double;
begin
  Assert(q < 1);
  Result := 0;
  Step := 1;
  repeat
    Current := IntPower(q, Sqr(Step)) * cos(2 * Step * z);

    // add current value to Result
    Result := Result + Current;

    Inc(Step);
  until Current < 1E-10;

  Result := 1 + 2 * Result;
end;

function Theta01(z, q: Double): Double;
var
  Step: Integer;
  Current: Double;
begin
  Assert(q < 1);
  Result := 0;
  Step := 1;
  repeat
    Current := IntPower(q, Sqr(Step)) * cos(2 * Step * z);
    if Step mod 2 = 1 then
      Current := -Current;

    // add current value to Result
    Result := Result + Current;

    Inc(Step);
  until Current < 1E-10;

  Result := 1 + 2 * Result;
end;

function Theta10(z, q: Double): Double;
var
  Step: Integer;
  Current: Double;
begin
  Assert(q < 1);
  Result := 0;
  Step := 0;
  repeat
    Current := IntPower(q, Step * (Step + 1)) * cos((2 * Step + 1) * z);

    // add current value to Result
    Result := Result + Current;

    Inc(Step);
  until Current < 1E-10;

  Result := 2 * Power(q, 0.25) * Result;
end;

function Theta11(z, q: Double): Double;
var
  Step: Integer;
  Current: Double;
begin
  Assert(q < 1);
  Result := 0;
  Step := 0;
  repeat
    Current := IntPower(q, Step * (Step + 1)) * sin((2 * Step + 1) * z);
    if Step mod 2 = 1 then
      Current := -Current;

    // add current value to Result
    Result := Result + Current;

    Inc(Step);
  until Current < 1E-10;

  Result := -2 * Power(q, 0.25) * Result;
end;

/// /////////////////////////////////////////////////////////////////////////////

function Theta00(q: Double): Double;
var
  Step: Integer;
  Current: Double;
begin
  Assert(q < 1);
  Result := 0;
  Step := 1;
  repeat
    Current := IntPower(q, Sqr(Step));

    // add current value to Result
    Result := Result + Current;

    Inc(Step);
  until Current < 1E-10;

  Result := 1 + 2 * Result;
end;

function Theta01(q: Double): Double;
var
  Step: Integer;
  Current: Double;
begin
  Assert(q < 1);
  Result := 0;
  Step := 1;
  repeat
    Current := IntPower(q, Sqr(Step));
    if Step mod 2 = 1 then
      Current := -Current;

    // add current value to Result
    Result := Result + Current;

    Inc(Step);
  until Current < 1E-10;

  Result := 1 + 2 * Result;
end;

function Theta10(q: Double): Double;
var
  Step: Integer;
  Current: Double;
begin
  Assert(q < 1);
  Result := 0;
  Step := 0;
  repeat
    Current := IntPower(q, Step * (Step + 1));

    // add current value to Result
    Result := Result + Current;

    Inc(Step);
  until Current < 1E-10;

  Result := 2 * Power(q, 0.25) * Result;
end;

function Theta11(q: Double): Double;
begin
  Assert(q < 1);
  Result := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

function Theta00(q: TComplex64): TComplex64;
var
  Epsilon: Double;
  Step: Integer;
  Current: TComplex64;
begin
  Assert(q.Im >= 0);
  Result.Re := 0;
  Result.Im := 0;
  Step := 1;
  repeat
    Current.Re := Pi * (Sqr(Step) * q.Im);
    Current.Im := Pi * (Sqr(Step) * q.Re);

    Current := ComplexExp64(Current);

    // evaluate epsilon
    Epsilon := max(Current.Re, Current.Im);

    // add current value to Result
    Result.Re := Result.Re + Current.Re;
    Result.Im := Result.Im + Current.Im;

    Inc(Step);
  until Epsilon < 1E-10;

  Result.Re := 1 + 2 * Result.Re;
  Result.Im := 2 * Result.Im;
end;

function Theta01(q: TComplex64): TComplex64;
var
  Temp: TComplex64;
begin
  Temp.Re := 0.5;
  Temp.Im := 0;
  Result := Theta00(Temp, q);
end;

function Theta10(q: TComplex64): TComplex64;
var
  Temp: TComplex64;
begin
  Temp.Re := 0.5 * q.Re;
  Temp.Im := 0.5 * q.Im;
  Result := Theta00(Temp, q);
  Temp.Im := Pi * (q.Im - 0.25 * Temp.Im);
  Temp.Re := Pi * (q.Re - 0.25 * Temp.Re);
  ComplexMultiplyInplace64(Result, ComplexExp64(Temp.Im, Temp.Re));
end;

function Theta11(q: TComplex64): TComplex64;
var
  Temp: TComplex64;
begin
  Temp.Re := 0.5;
  Temp.Im := 0;
  Result := Theta10(Temp, q);
end;

/// /////////////////////////////////////////////////////////////////////////////

function Theta00(z, q: TComplex64): TComplex64;
var
  Epsilon: Double;
  Step: Integer;
  Current: TComplex64;
begin
  Assert(q.Im >= 0);
  Result.Re := 0;
  Result.Im := 0;
  Step := 1;
  repeat
    Current.Re := Pi * (Sqr(Step) * q.Im + 2 * Step * z.Im);
    Current.Im := Pi * (Sqr(Step) * q.Re + 2 * Step * z.Re);

    Current := ComplexExp64(Current);

    // evaluate epsilon
    Epsilon := max(Current.Re, Current.Im);

    // add current value to Result
    Result.Re := Result.Re + Current.Re;
    Result.Im := Result.Im + Current.Im;

    Inc(Step);
  until Epsilon < 1E-10;

  Result.Re := 1 + 2 * Result.Re;
  Result.Im := 2 * Result.Im;
end;

function Theta01(z, q: TComplex64): TComplex64;
begin
  z.Re := z.Re + 0.5;
  Result := Theta00(z, q);
end;

function Theta10(z, q: TComplex64): TComplex64;
begin
  z.Re := z.Re + 0.5 * q.Re;
  z.Im := z.Im + 0.5 * q.Im;
  Result := Theta00(z, q);
  z.Im := Pi * (q.Im - 0.25 * z.Im);
  z.Re := Pi * (q.Re - 0.25 * z.Re);
  ComplexMultiplyInplace64(Result, ComplexExp64(z.Im, z.Re));
end;

function Theta11(z, q: TComplex64): TComplex64;
begin
  z.Re := z.Re + 0.5;
  Result := Theta10(z, q);
end;

/// /////////////////////////////////////////////////////////////////////////////

function Theta1(z, q: Double): Double;
var
  Step: Integer;
  Current: Double;
begin
  Assert(q < 1);
  Result := 0;
  Step := 0;
  repeat
    Current := IntPower(q, Step * (Step + 1)) * sin((2 * Step + 1) * z);
    if Step mod 2 = 1 then
      Current := -Current;

    // add current value to Result
    Result := Result + Current;

    Inc(Step);
  until Current < 1E-10;

  Result := 2 * Power(q, 0.25) * Result;
end;

function Theta2(z, q: Double): Double;
var
  Step: Integer;
  Current: Double;
begin
  Assert(q < 1);
  Result := 0;
  Step := 0;
  repeat
    Current := IntPower(q, Step * (Step + 1)) * cos((2 * Step + 1) * z);

    // add current value to Result
    Result := Result + Current;

    Inc(Step);
  until Current < 1E-10;

  Result := 2 * Power(q, 0.25) * Result;
end;

function Theta3(z, q: Double): Double;
var
  Step: Integer;
  Current: Double;
begin
  Assert(q < 1);
  Result := 0;
  Step := 1;
  repeat
    Current := IntPower(q, Sqr(Step)) * cos(2 * Step * z);

    // add current value to Result
    Result := Result + Current;

    Inc(Step);
  until Current < 1E-10;

  Result := 1 + 2 * Result;
end;

function Theta4(z, q: Double): Double;
var
  Step: Integer;
  Current: Double;
begin
  Assert(q < 0);
  Result := 0;
  Step := 1;
  repeat
    Current := IntPower(q, Sqr(Step)) * cos(2 * Step * z);
    if Step mod 2 = 1 then
      Current := -Current;

    // add current value to Result
    Result := Result + Current;

    Inc(Step);
  until Current < 1E-10;

  Result := 1 + 2 * Result;
end;

/// /////////////////////////////////////////////////////////////////////////////

function Theta1(z, q: TComplex64): TComplex64;
begin
  Result := Theta11(z, q);
  Result.Re := -Result.Re;
  Result.Im := -Result.Im;
end;

function Theta2(z, q: TComplex64): TComplex64;
begin
  Result := Theta10(z, q);
end;

function Theta3(z, q: TComplex64): TComplex64;
begin
  Result := Theta00(z, q);
end;

function Theta4(z, q: TComplex64): TComplex64;
begin
  Result := Theta01(z, q);
end;

/// /////////////////////////////////////////////////////////////////////////////

function Sn(z, k: Double): Double;
var
  Theta: Double;
begin
  Theta := NomeQk(k);
  Result := -(Theta00(Theta) * Theta11(z, Theta)) /
    (Theta10(Theta) * Theta01(z, Theta));
end;

function Cn(z, k: Double): Double;
var
  Theta: Double;
begin
  Theta := NomeQk(k);
  Result := (Theta01(Theta) * Theta10(z, Theta)) /
    (Theta10(Theta) * Theta01(z, Theta));
end;

function Dn(z, k: Double): Double;
var
  Theta: Double;
begin
  Theta := NomeQk(k);
  Result := (Theta01(Theta) * Theta00(z, Theta)) /
    (Theta00(Theta) * Theta01(z, Theta));
end;

procedure JacobiEllipticFunctions(z, k: Double; out Sn, Cn, Dn: Double);
var
  Theta: Double;
  Ts: array [0 .. 1, 0 .. 2] of Double;
begin
  Theta := NomeQk(k);

  Ts[0, 0] := Theta00(Theta);
  Ts[0, 1] := Theta01(Theta);
  Ts[0, 2] := Theta10(Theta);

  Ts[1, 0] := Theta01(z, Theta);
  Ts[1, 1] := Theta10(z, Theta);
  Ts[1, 2] := Theta11(z, Theta);

  Sn := -(Ts[0, 0] * Ts[1, 2]) / (Ts[0, 2] * Ts[1, 0]);

  Cn := (Ts[0, 1] * Ts[1, 1]) / (Ts[0, 2] * Ts[1, 0]);

  Dn := (Ts[0, 1] * Theta00(z, Theta)) / (Ts[0, 0] * Ts[1, 0]);
end;

/// /////////////////////////////////////////////////////////////////////////////

function Ns(z, k: Double): Double;
var
  Theta: Double;
begin
  Theta := NomeQk(k);
  Result := -(Theta10(Theta) * Theta01(z, Theta)) /
    (Theta00(Theta) * Theta11(z, Theta));
end;

function Nc(z, k: Double): Double;
var
  Theta: Double;
begin
  Theta := NomeQk(k);
  Result := (Theta10(Theta) * Theta01(z, Theta)) /
    (Theta01(Theta) * Theta10(z, Theta));
end;

function Nd(z, k: Double): Double;
var
  Theta: Double;
begin
  Theta := NomeQk(k);
  Result := (Theta00(Theta) * Theta01(z, Theta)) /
    (Theta01(Theta) * Theta00(z, Theta));
end;

function Sd(z, k: Double): Double;
var
  Theta: Double;
begin
  Theta := NomeQk(k);

  Result := -Sqr(Theta00(Theta)) * (Theta11(z, Theta)) /
    (Theta00(z, Theta) * Theta01(Theta) * Theta10(Theta));
end;

function Sc(z, k: Double): Double;
var
  Theta: Double;
begin
  Theta := NomeQk(k);

  Result := -(Theta00(Theta) * Theta11(z, Theta)) /
    (Theta01(Theta) * Theta10(z, Theta));
end;

function Cd(z, k: Double): Double;
var
  Theta: Double;
begin
  Theta := NomeQk(k);

  Result := (Theta00(Theta) * Theta10(z, Theta)) /
    (Theta10(Theta) * Theta00(z, Theta));
end;

function Cs(z, k: Double): Double;
var
  Theta: Double;
begin
  Theta := NomeQk(k);

  Result := -(Theta01(Theta) * Theta10(z, Theta)) /
    (Theta00(Theta) * Theta11(z, Theta));
end;

function Dc(z, k: Double): Double;
var
  Theta: Double;
begin
  Theta := NomeQk(k);

  Result := (Theta10(Theta) * Theta00(z, Theta)) /
    (Theta00(Theta) * Theta10(z, Theta));
end;

function Ds(z, k: Double): Double;
var
  Theta: Double;
begin
  Theta := NomeQk(k);

  Result := -(Theta01(Theta) * Theta10(Theta) * Theta00(z, Theta)) *
    Sqr(Theta00(Theta)) / (Theta11(z, Theta));
end;

/// /////////////////////////////////////////////////////////////////////////////

function Sn(z, k: TComplex64): TComplex64;
var
  Theta: TComplex64;
begin
  Theta := NomeQk(k);
  Result := ComplexDivide64(ComplexMultiply64(Theta00(Theta), Theta11(z, Theta)
    ), ComplexMultiply64(Theta10(Theta), Theta01(z, Theta)));
  Result.Re := -Result.Re;
  Result.Im := -Result.Im;
end;

function Cn(z, k: TComplex64): TComplex64; overload;
var
  Theta: TComplex64;
begin
  Theta := NomeQk(k);
  Result := ComplexDivide64(ComplexMultiply64(Theta01(Theta), Theta10(z, Theta)
    ), ComplexMultiply64(Theta10(Theta), Theta01(z, Theta)));
end;

function Dn(z, k: TComplex64): TComplex64; overload;
var
  Theta: TComplex64;
begin
  Theta := NomeQk(k);
  Result := ComplexDivide64(ComplexMultiply64(Theta01(Theta), Theta00(z, Theta)
    ), ComplexMultiply64(Theta00(Theta), Theta01(z, Theta)));
end;

/// /////////////////////////////////////////////////////////////////////////////

function Ns(z, k: TComplex64): TComplex64;
begin
  Result := ComplexReciprocal64(Dn(z, k));
end;

function Nc(z, k: TComplex64): TComplex64;
begin
  Result := ComplexReciprocal64(Dn(z, k));
end;

function Nd(z, k: TComplex64): TComplex64;
begin
  Result := ComplexReciprocal64(Dn(z, k));
end;

function Sd(z, k: TComplex64): TComplex64;
begin
  Result := ComplexDivide64(Sn(z, k), Dn(z, k));
end;

function Sc(z, k: TComplex64): TComplex64;
begin
  Result := ComplexDivide64(Sn(z, k), Cn(z, k));
end;

function Cd(z, k: TComplex64): TComplex64;
begin
  Result := ComplexDivide64(Cn(z, k), Dn(z, k));
end;

function Cs(z, k: TComplex64): TComplex64;
begin
  Result := ComplexDivide64(Cn(z, k), Sn(z, k));
end;

function Dc(z, k: TComplex64): TComplex64;
begin
  Result := ComplexDivide64(Dn(z, k), Cn(z, k));
end;

function Ds(z, k: TComplex64): TComplex64;
begin
  Result := ComplexDivide64(Dn(z, k), Sn(z, k));
end;

/// /////////////////////////////////////////////////////////////////////////////
// //
// Evaluate polynomial                                                       //
// -------------------                                                       //
// //
// //
// SYNOPSIS:                                                                 //
// EvaluatePolynom11(Input: Double; Coefficients: PDAVDoubleFixedArray;    //
// Length: Integer): Double;                                             //
// //
// //
// DESCRIPTION:                                                              //
// Evaluates polynomial of degree N:                                       //
// //
// 2          N                                        //
// y  =  C  + C x + C x  +Args: array of const+ C x                        //
// 0    1     2          N                                          //
// //
// Coefficients are stored in reverse order:                               //
// //
// coef[0] = C  , Args: array of const, coef[N] = C  .                     //
// N                                    0                       //
// //
// The function EvaluatePolynom01() assumes that coef[N] = 1.0 and is      //
// omitted from the array.  Its calling arguments are otherwise the same   //
// as EvaluatePolynom11().                                                 //
// //
// //
// SPEED:                                                                    //
// In the interest of speed, there are no checks for out of bounds         //
// arithmetic.  This routine is used by most of the functions in the       //
// library.  Depending on available equipment features, the user           //
// may wish to rewrite the program in microcode or assembly language.      //
// //
/// /////////////////////////////////////////////////////////////////////////////

function EvaluatePolynom11(Input: Double; Coefficients: PDAVDoubleFixedArray;
  Length: Integer): Double;
var
  i: Integer;
begin
  Result := Coefficients[0];
  i := 1;

  while i < Length do
  begin
    Result := Result * Input + Coefficients[i];
    Inc(i);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// //
// N                                //
// Evaluate polynomial when coefficient of x  is 1.0.                        //
// Otherwise same as EvaluatePolynom11.                                      //
// //
// //
/// /////////////////////////////////////////////////////////////////////////////

function EvaluatePolynom01(Input: Double; Coefficients: PDAVDoubleFixedArray;
  Length: Integer): Double;
var
  i: Integer;
begin
  Result := Input + Coefficients[0];
  i := 1;

  while i < Length do
  begin
    Result := Result * Input + Coefficients[i];
    Inc(i);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// //
// Complete Elliptic Integral of the Second Kind                             //
// ---------------------------------------------                             //
// //
// SYNOPSIS:                                                                 //
// function CompleteEllipticIntegral2ndKind(x: Double): Double;            //
// //
// DESCRIPTION:                                                              //
// Approximates the integral                                               //
// //
// //
// Pi/2                                                         //
// -                                                           //
// | |                 2                                        //
// E(m)  =    |    Sqrt( 1 - m sin t ) dt                                  //
// | |                                                            //
// -                                                             //
// 0                                                            //
// //
// Where m = 1 - m1, using the approximation                                 //
// //
// P(x)  -  x log x Q(x).                                               //
// //
// Though there are no singularities, the argument m1 is used                //
// rather than m for compatibility with CompleteEllipticIntegral1stKind.     //
// //
// E(1) := 1; E(0) = Pi/2.                                                   //
// //
// //
// ACCURACY:                                                                 //
// //
// Relative error:                                      //
// arithmetic   domain     # trials      peak         rms                    //
// DEC        0, 1       13000       3.1E-17     9.4E-18                  //
// IEEE       0, 1       10000       2.1E-16     7.3E-17                  //
// //
// //
// ERROR MESSAGES:                                                           //
// //
// message   condition   value returned    //
// CompleteEllipticIntegral2ndKind   domain     x<0, x>1         0.0         //
// //
/// /////////////////////////////////////////////////////////////////////////////

function CompleteEllipticIntegral2ndKindX(x: Double): Double;
const
  P: array [0 .. 10] of Double = (1.53552577301013293365E-4,
    2.50888492163602060990E-3, 8.68786816565889628429E-3,
    1.07350949056076193403E-2, 7.77395492516787092951E-3,
    7.58395289413514708519E-3, 1.15688436810574127319E-2,
    2.18317996015557253103E-2, 5.68051945617860553470E-2,
    4.43147180560990850618E-1, 1.00000000000000000299E0);

  q: array [0 .. 9] of Double = (3.27954898576485872656E-5,
    1.00962792679356715133E-3, 6.50609489976927491433E-3,
    1.68862163993311317300E-2, 2.61769742454493659583E-2,
    3.34833904888224918614E-2, 4.27180926518931511717E-2,
    5.85936634471101055642E-2, 9.37499997197644278445E-2,
    2.49999999999888314361E-1);
begin
  if (x <= 0) or (x > 1) then
  begin
    if x = 0 then
      Result := 1
    else
      Result := 0;
  end
  else
    Result := EvaluatePolynom11(x, @P[0], Length(P)) - log10(x) * x *
      EvaluatePolynom11(x, @q[0], Length(q));
end;

/// /////////////////////////////////////////////////////////////////////////////
// //
// Incomplete Elliptic Integral of Second Kind                               //
// -------------------------------------------                               //
// //
// SYNOPSIS:                                                                 //
// function IncompleteEllipticIntegral2ndKind(phi, m: Double): Double;     //
// //
// DESCRIPTION:                                                              //
// Approximates the integral                                               //
// //
// phi                                                     //
// -                                                      //
// | |                                                     //
// |                   2                                   //
// E(phi | m)  =   |    Sqrt( 1 - m sin t ) dt                             //
// |                                                       //
// | |                                                       //
// -                                                        //
// 0                                                        //
// //
// of amplitude phi and modulus m, using the arithmetic - geometric        //
// mean algorithm.                                                         //
// //
// //
// //
// ACCURACY:                                                                 //
// Tested at random arguments with phi in [-10, 10] and m in [0, 1].       //
// Relative error:                                       //
// arithmetic   domain     # trials      peak         rms                    //
// DEC        0,2         2000       1.9E-16     3.4E-17                  //
// IEEE     -10,10      150000       3.3E-15     1.4E-16                  //
// //
/// /////////////////////////////////////////////////////////////////////////////

function IncompleteEllipticIntegral2ndKindX(Phi, m: Double): Double;
var
  a, b, c, e: Double;
  lphi, t, Ex: Double;
  d, md: Integer;
  npio2, sign: Integer;
label
  done;
begin
  if m = 0 then
  begin
    Result := Phi;
    Exit;
  end;
  lphi := Phi;
  npio2 := Floor(lphi / PiO2);

  if (npio2 and 1) <> 0 then
    npio2 := npio2 + 1;
  lphi := lphi - npio2 * PiO2;

  if lphi < 0.0 then
  begin
    lphi := -lphi;
    sign := -1;
  end
  else
    sign := 1;

  a := 1.0 - m;
  Ex := CompleteEllipticIntegral2ndKind(a);
  if a = 0.0 then
  begin
    Result := sin(lphi);
    goto done;
  end;
  t := tan(lphi);
  b := Sqrt(a);

  (* Thanks to Brian Fitzgerald <fitzgb@mml0.meche.rpi.edu>
    for pointing out an instability near odd multiples of Pi/2. *)
  if abs(t) > 10 then
  begin
    (* Transform the amplitude *)
    e := 1 / (b * t);
    (* ... but avoid multiple recursions. *)
    if abs(e) < 10 then
    begin
      e := ArcTan(e);
      Result := Ex + m * sin(lphi) * sin(e) -
        IncompleteEllipticIntegral2ndKind(e, m);
      goto done;
    end;
  end;
  c := Sqrt(m);
  a := 1.0;
  d := 1;
  e := 0.0;
  md := 0;

  while abs(c / a) > CMachEP do
  begin
    Result := b / a;
    lphi := lphi + ArcTan(t * Result) + md * Pi;
    md := round((lphi + PiO2) / Pi);
    t := t * (1 + Result) / (1 - Result * Sqr(t));
    c := (a - b) * 0.5;
    Result := Sqrt(a * b);
    a := (a + b) * 0.5;
    b := Result;
    d := d + d;
    e := e + c * sin(lphi);
  end;

  Result := Ex / CompleteEllipticIntegral1stKind(1 - m);
  Result := Result * (ArcTan(t) + md * Pi) / (d * a);
  Result := Result + e;

done:

  if sign < 0 then
    Result := -Result;
  Result := Result + npio2 * Ex;
end;

/// /////////////////////////////////////////////////////////////////////////////
// //
// Incomplete elliptic integral of the first kind                            //
// ----------------------------------------------                            //
// //
// SYNOPSIS:                                                                 //
// function IncompleteEllipticIntegral1stKind(phi, m: Double): Double;     //
// //
// //
// DESCRIPTION:                                                              //
// Approximates the integral                                               //
// //
// phi                                                      //
// -                                                       //
// |  |                                                     //
// |           dt                                           //
// F(phi_\m)  =   |    ------------------                                  //
// |                   2                                    //
// | |    Sqrt( 1 - m sin t )                                 //
// -                                                         //
// 0                                                         //
// //
// of amplitude phi and modulus m, using the arithmetic - geometric mean     //
// algorithm.                                                                //
// //
// //
// ACCURACY:                                                                  //
// //
// Tested at random points with m in [0, 1] and phi as indicated.           //
// //
// Relative error:                                     //
// arithmetic   domain     # trials      peak         rms                   //
// IEEE     -10,10       200000      7.4E-16     1.0E-16                 //
// //
// //
/// /////////////////////////////////////////////////////////////////////////////

function IncompleteEllipticIntegral1stKindX(Phi, m: Double): Double;
var
  a, b, c, e, t, k: Double;
  d, md, sign, npio2: Integer;
label
  done;
begin
  if m = 0 then
    Result := (Phi);

  a := 1 - m;
  if a = 0 then
  begin
    if abs(Phi) >= PiO2 then
    begin
      // mtherr('IncompleteEllipticIntegral1stKind', SING);
      Result := (MaxNum);
      Exit;
    end;
    Result := log10(tan((PiO2 + Phi) * 0.5));
    Exit;
  end;
  npio2 := Floor(Phi / PiO2);

  if (npio2 and 1) <> 0 then
    npio2 := npio2 + 1;

  if npio2 <> 0 then
  begin
    k := CompleteEllipticIntegral1stKind(a);
    Phi := Phi - npio2 * PiO2;
  end
  else
    k := 0;

  if Phi < 0 then
  begin
    Phi := -Phi;
    sign := -1;
  end
  else
    sign := 0;

  b := Sqrt(a);
  t := tan(Phi);
  if abs(t) > 10 then
  begin
    (* Transform the amplitude *)
    e := 1 / (b * t);

    (* ... but avoid multiple recursions. *)
    if abs(e) < 10 then
    begin
      e := ArcTan(e);
      if (npio2 = 0) then
        k := CompleteEllipticIntegral1stKindX(a);
      Result := k - IncompleteEllipticIntegral1stKindX(e, m);
      goto done;
    end;
  end;

  a := 1.0;
  c := Sqrt(m);
  d := 1;
  md := 0;

  while abs(c / a) > CMachEP do
  begin
    Result := b / a;
    Phi := Phi + ArcTan(t * Result) + md * Pi;
    md := round((Phi + PiO2) / Pi);
    t := t * (1.0 + Result) / (1.0 - Result * Sqr(t));
    c := (a - b) * 0.5;
    Result := Sqrt(a * b);
    a := (a + b) * 0.5;
    b := Result;
    d := d + d;
  end;

  Result := (ArcTan(t) + md * Pi) / (d * a);

done:
  if (sign < 0) then
    Result := -Result;

  Result := Result + npio2 * k;
end;

/// /////////////////////////////////////////////////////////////////////////////
// //
// Complete elliptic integral of the first kind                              //
// --------------------------------------------                              //
// //
// SYNOPSIS:                                                                 //
// function CompleteEllipticIntegral1stKind(x: Double): Double;            //
// //
// DESCRIPTION:                                                              //
// Approximates the integral                                               //
// //
// Pi/2                                                         //
// -                                                           //
// | |                                                          //
// |          dt                                                //
// K(m)  =    |   ------------------                                       //
// |                  2                                         //
// | |   Sqrt( 1 - m sin t )                                      //
// -                                                             //
// 0                                                             //
// //
// where m = 1 - m1, using the approximation                               //
// //
// P(x)  -  log x Q(x).                                                  //
// //
// The argument m1 is used rather than m so that the logarithmic           //
// singularity at m := 1 will be shifted to the origin; this preserves     //
// maximum accuracy.                                                       //
// //
// K(0) = Pi/2.                                                            //
// //
// ACCURACY:                                                                 //
// //
// Relative error:                                    //
// arithmetic   domain     # trials      peak         rms                  //
// DEC        0,1        16000       3.5E-17     1.1E-17                //
// IEEE       0,1        30000       2.5E-16     6.8E-17                //
// //
// ERROR MESSAGES:                                                           //
// //
// message  condition  value returned       //
// CompleteEllipticIntegral1stKind domain   x<0, x>1        0.0             //
// //
/// /////////////////////////////////////////////////////////////////////////////

function CompleteEllipticIntegral1stKindX(x: Double): Double;
const
  P: array [0 .. 10] of Double = (1.37982864606273237150E-4,
    2.28025724005875567385E-3, 7.97404013220415179367E-3,
    9.85821379021226008714E-3, 6.87489687449949877925E-3,
    6.18901033637687613229E-3, 8.79078273952743772254E-3,
    1.49380448916805252718E-2, 3.08851465246711995998E-2,
    9.65735902811690126535E-2, 1.38629436111989062502E0);

  q: array [0 .. 10] of Double = (2.94078955048598507511E-5,
    9.14184723865917226571E-4, 5.94058303753167793257E-3,
    1.54850516649762399335E-2, 2.39089602715924892727E-2,
    3.01204715227604046988E-2, 3.73774314173823228969E-2,
    4.88280347570998239232E-2, 7.03124996963957469739E-2,
    1.24999999999870820058E-1, 4.99999999999999999821E-1);

  C1: Double = 1.3862943611198906188E0;

begin
  if (x < 0) or (x > 1) then
  begin
    // mtherr( 'CompleteEllipticIntegral1stKind', DOMAIN );
    Result := 0;
    Exit;
  end;

  if (x > CMachEP) then
    Result := EvaluatePolynom11(x, @P[0], Length(P)) - log10(x) *
      EvaluatePolynom11(x, @q[0], Length(q))
  else if x = 0.0 then
  begin
    // mtherr( 'CompleteEllipticIntegral1stKind', SING);
    Result := MaxNum;
  end
  else
    Result := C1 - 0.5 * log10(x);
end;

/// /////////////////////////////////////////////////////////////////////////////
// //
// Jacobian Elliptic Functions                                               //
// //
// //
// SYNOPSIS:                                                                 //
// //
// function JacobianEllipticFunctions(u, m: Double;                         //
// var sn, cn, dn, ph: Double): Integer;                                  //
// //
// //
// DESCRIPTION:                                                              //
// Evaluates the Jacobian elliptic functions sn(u | m), cn(u | m),         //
// and dn(u | m) of parameter m between 0 and 1, and Single                //
// argument u.                                                             //
// //
// These functions are periodic, with quarter-period on the                //
// Single axis equal to the complete elliptic integral                     //
// CompleteEllipticIntegral1stKind(1 - m).                                 //
// //
// Relation to incomplete elliptic integral:                               //
// If u = IncompleteEllipticIntegral1stKind(phi, m),                       //
// then sn(u | m) = sin(phi),                                            //
// and cn(u | m) = cos(phi).  Phi is called the amplitude of u.            //
// //
// Computation is by means of the arithmetic-geometric mean                //
// algorithm, except when m is within 1E-9 of 0 | 1.  In the               //
// latter  with m close to 1, the approximation applies                    //
// only for phi < Pi/2.                                                    //
// //
// ACCURACY:                                                                 //
// //
// Tested at random points with u between 0 and 10, m between              //
// 0 and 1.                                                                //
// //
// Absolute error ( = relative error):                          //
// arithmetic   function   # trials      peak         rms                  //
// DEC       sn           1800       4.5E-16     8.7E-17                //
// IEEE      phi         10000       9.2E-16*    1.4E-16*               //
// IEEE      sn          50000       4.1E-15     4.6E-16                //
// IEEE      cn          40000       3.6E-15     4.4E-16                //
// IEEE      dn          10000       1.3E-12     1.8E-14                //
// //
// Peak error observed in consistency check using addition                 //
// theorem for sn(u+v) was 4E-16 (absolute).  Also tested by               //
// the above relation to the incomplete elliptic integral.                 //
// Accuracy deteriorates when u is large.                                  //
// //
/// /////////////////////////////////////////////////////////////////////////////

function JacobianEllipticFunctions(u, m: Double;
  var Sn, Cn, Dn, ph: Double): Integer;
var
  ai, b, Phi: Double;
  t, twon: Double;
  i: Integer;
  a: array [0 .. 8] of Double;
  c: array [0 .. 8] of Double;
label
  done;
begin

  (* Check for special cases *)
  if (m < 0.0) or (m > 1.0) then
  begin
    // mtherr( 'JacobianEllipticFunctions', DOMAIN );
    Sn := 0.0;
    Cn := 0.0;
    ph := 0.0;
    Dn := 0.0;
    Result := -1;
    Exit;
  end;
  if (m < 1.0E-9) then
  begin
    t := sin(u);
    b := cos(u);
    ai := 0.25 * m * (u - t * b);
    Sn := t - ai * b;
    Cn := b + ai * t;
    ph := u - ai;
    Dn := 1.0 - 0.5 * m * t * t;
    Result := 0;
  end;

  if (m >= 0.9999999999) then
  begin
    ai := 0.25 * (1.0 - m);
    b := cosh(u);
    t := DAV_Math.tanh(u);
    Phi := 1.0 / b;
    twon := b * sinh(u);
    Sn := t + ai * (twon - u) / (b * b);
    ph := 2.0 * ArcTan(exp(u)) - PiO2 + ai * (twon - u) / b;
    ai := ai * t * Phi;
    Cn := Phi - ai * (twon - u);
    Dn := Phi + ai * (twon + u);
    Result := 0;
  end;

  (* A. G. M. scale *)
  a[0] := 1;
  b := Sqrt(1 - m);
  c[0] := Sqrt(m);
  twon := 1;
  i := 0;

  while abs(c[i] / a[i]) > CMachEP do
  begin
    if (i > 7) then
    begin
      // mtherr( 'JacobianEllipticFunctions', OVERFLOW );
      goto done;
    end;
    ai := a[i];
    Inc(i);
    c[i] := (ai - b) * 0.5;
    t := Sqrt(ai * b);
    a[i] := (ai + b) * 0.5;
    b := t;
    twon := twon * 2.0;
  end;

done:

  (* backward recurrence *)
  Phi := twon * a[i] * u;
  while i > 0 do
  begin
    t := c[i] * sin(Phi) / a[i];
    b := Phi;
    Phi := (arcsin(t) + Phi) * 0.5;
    dec(i);
  end;

  Sn := sin(Phi);
  t := cos(Phi);
  Cn := t;
  Dn := t / cos(Phi - b);
  ph := Phi;
  Result := 0;
end;

function PochhammerRisingSequentialProduct(x: Double; n: Integer): Double;
var
  Step: Integer;
begin
  Result := 1;
  for Step := 1 to n do
    Result := Result * ((x - 1) + Step);
end;

function PochhammerFallingSequentialProduct(x: Double; n: Integer): Double;
var
  Step: Integer;
begin
  Result := 1;
  for Step := 1 to n do
    Result := Result * (x - Step);
end;

function Pochhammer(x: Double; n: Integer): Double;
begin
  Result := PochhammerRisingSequentialProduct(x, n);
end;

function HypergeometricFunction2F1(a, b, c: Double; z: Double;
  Steps: Integer = 30): Double;
var
  n: Integer;
begin
  Assert(Steps > 0);

  Result := 1;

  for n := 1 to Steps do
  begin
    Result := Result + Pochhammer(a, n) * Pochhammer(b, n) * Power(z, n) /
      (Pochhammer(c, n) * Factorial(n));
  end;
end;

function SpecialPochhammer(k: Double; n: Integer): Double;
var
  Step: Integer;
begin
  Result := 1;
  for Step := 1 to n do
    Result := Result * (-0.5 + Step) * (-0.5 + k + Step) /
      ((0.5 + k + Step) * Step);
end;

function SpecialHypergeometricFunction(k: Double; z: Double;
  Steps: Integer = 5): Double;
var
  n: Integer;
begin
  Assert(Steps > 0);

  Result := 1;

  for n := 1 to Steps do
  begin
    Result := Result + SpecialPochhammer(k, n) * Power(z, 2 * n);
  end;
end;

function InverseCd(z, m: Double; Steps: Integer): Double;
var
  i, j: Integer;
  Scale: Extended;
  Inner: Extended;
  PochFac: array [0 .. 1] of Extended;
begin
  (*
    // constraints
    assert(z > -1);
    assert(z < +1);
  *)

  Result := 0;
  PochFac[0] := 1;
  for i := 0 to Steps - 1 do
  begin
    if i > 0 then
      PochFac[0] := PochFac[0] * (-0.5 + i) / i;

    PochFac[1] := 1;
    for j := 0 to Steps - 1 do
    begin
      if j > 0 then
        PochFac[1] := PochFac[1] * (-0.5 + j) / j;
      Result := Result + PochFac[0] * PochFac[1] * Power(m, j) *
        Power(z, 2 * i + 2 * j + 1) / (2 * i + 2 * j + 1);
    end;
  end;
  Result := CompleteEllipticIntegral1stKind(Sqrt(m)) - Result;

  (*
    assert(abs(m) < 1);
    Result := 0;
    for i := 0 to Steps - 1 do
    begin
    Inner := 0;
    for j := 1 to i
    do Inner := Inner + Factorial(j - 1) * Power(z, 2 * j) / Pochhammer(0.5, j);

    Inner := (arccos(z) + Sqrt(1 - sqr(z)) / (2 * z) * Inner) * Power(m, i);

    Result := Result + sqr(Pochhammer(0.5, i) / Factorial(i)) * Inner;
    end;
  *)

  (*
    Result := 0;
    for i := 0 to Steps - 1 do
    begin
    Result := Result + (0.5 * Pi * sqr(Pochhammer(0.5, i) / Factorial(i)) -
    (Pochhammer(0.5, i) * Power(z, 2 * i + 1)) /
    ((2 * i + 1) * Factorial(i)) *
    SpecialHypergeometricFunction(i, z)) * Power(m, i);
    end;
  *)
end;

function InverseCd(z, m: TComplex64; Steps: Integer): TComplex64;
begin
  raise Exception.Create('not yet implemented');
end;

function CalculateLn(Order: Integer; Selectivity: Double): Double;
var
  t: array [0 .. 1] of Double;
begin
  case Order of
    1:
      Result := Selectivity;
    2:
      Result := Sqr(Selectivity + Sqrt(Sqr(Selectivity) + 1));
    3:
      begin
        t[0] := Sqrt(1 - Sqr(1 / Selectivity));
        t[1] := Power(2 * t[0] * (1 - 2 * t[0]), 1 / 3);
        t[0] := 0.5 * (-(1 - 2 * t[0]) / Sqrt(1 + t[1] + Sqr(t[1])) - 1 +
          Sqrt(2 + t[1] + 2 * Sqrt(1 + t[1] + Sqr(t[1]))));
        Result := Sqr(1 + t[0]) * Selectivity *
          (Sqr(Selectivity) - ((1 + 2 * t[0]) / Sqr(1 + t[0]))) /
          ((Sqr(t[0]) - 1) * Sqr(Selectivity) + 1);
      end;
    4:
      Result := Sqr(Sqr(Sqrt(Selectivity) + Power(Sqr(Selectivity) - 1, 0.25)))
        * Sqr(Selectivity + Sqrt(Sqr(Selectivity) - 1));
    // 4 : Result := sqr(Selectivity + Sqrt(sqr(Selectivity) + 1));
    // 3 : Result :=
    // Temp := Sqrt(1 - sqr(1 / Selectivity));
  else
    raise Exception.Create('not implemented yet!');
  end;
end;

function RationalEllipticFunction(Order: Integer;
  Selectivity, x: Double): Double;
var
  Ln: Double;
begin
  Ln := CalculateLn(Order, Selectivity);
  Result := Cd(Order * CompleteEllipticIntegral1stKind(1 / Ln) /
    CompleteEllipticIntegral1stKind(1 / Selectivity) * InverseCd(x,
    1 / Selectivity), 1 / Ln);
end;

end.
