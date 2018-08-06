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

unit DAV_Complex;

interface

{$I DAV_Compiler.inc}
{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

type
  PComplex32 = ^TComplex32;

  TComplex32 = record
    Re: Single;
    Im: Single;
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
  public
{$IFNDEF FPC}
    constructor Create(const Re, Im: Single);
{$ENDIF}
    // operator overloads
    class operator Equal(const Lhs, Rhs: TComplex32): Boolean;
    class operator NotEqual(const Lhs, Rhs: TComplex32): Boolean;
    class operator Add(const Lhs, Rhs: TComplex32): TComplex32;
    class operator Subtract(const Lhs, Rhs: TComplex32): TComplex32;
    class operator Multiply(const Lhs, Rhs: TComplex32): TComplex32;
    class operator Divide(const Lhs, Rhs: TComplex32): TComplex32;
    class operator Negative(const Value: TComplex32): TComplex32;

    class function Zero: TComplex32; inline; static;

    class function Euler(const Value: Single): TComplex32; overload;
      inline; static;
    class function Euler(const Value: Double): TComplex32; overload;
      inline; static;
{$IFNDEF FPC}
    class function Euler(const Value: Extended): TComplex32; overload;
      inline; static;
{$ENDIF}
    function Magnitude: Single;

    procedure ComputeEuler(const Value: Single); overload;
    procedure ComputeEuler(const Value: Double); overload;
{$IFNDEF FPC}
    procedure ComputeEuler(const Value: Extended); overload;
{$ENDIF}
{$ENDIF}
  end;

  PComplex64 = ^TComplex64;

  TComplex64 = record
    Re: Double;
    Im: Double;
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
  public
{$IFNDEF FPC}
    constructor Create(const Re, Im: Double);
{$ENDIF}
    // operator overloads
    class operator Equal(const Lhs, Rhs: TComplex64): Boolean;
    class operator NotEqual(const Lhs, Rhs: TComplex64): Boolean;
    class operator Add(const Lhs, Rhs: TComplex64): TComplex64;
    class operator Subtract(const Lhs, Rhs: TComplex64): TComplex64;
    class operator Multiply(const Lhs, Rhs: TComplex64): TComplex64;
    class operator Divide(const Lhs, Rhs: TComplex64): TComplex64;
    class operator Negative(const Value: TComplex64): TComplex64;

    class function Zero: TComplex64; inline; static;

    class function Euler(const Value: Single): TComplex64; overload;
      inline; static;
    class function Euler(const Value: Double): TComplex64; overload;
      inline; static;
{$IFNDEF FPC}
    class function Euler(const Value: Extended): TComplex64; overload;
      inline; static;
{$ENDIF}
    function Magnitude: Double;

    procedure ComputeEuler(const Value: Single); overload;
    procedure ComputeEuler(const Value: Double); overload;
{$IFNDEF FPC}
    procedure ComputeEuler(const Value: Extended); overload;
{$ENDIF}
{$ENDIF}
  end;

  PDAVComplexSingleDynArray = ^TDAVComplexSingleDynArray;
  TDAVComplexSingleDynArray = array of TComplex32;

  PDAVComplexDoubleDynArray = ^TDAVComplexDoubleDynArray;
  TDAVComplexDoubleDynArray = array of TComplex64;

  PDAVComplexSingleFixedArray = ^TDAVComplexSingleFixedArray;
  TDAVComplexSingleFixedArray = array [0 .. 0] of TComplex32;

  PDAVComplexDoubleFixedArray = ^TDAVComplexDoubleFixedArray;
  TDAVComplexDoubleFixedArray = array [0 .. 0] of TComplex64;

  PDAV2ComplexSingleArray = ^TDAV2ComplexSingleArray;
  TDAV2ComplexSingleArray = array [0 .. 1] of TComplex32;
  PDAV3ComplexSingleArray = ^TDAV3ComplexSingleArray;
  TDAV3ComplexSingleArray = array [0 .. 2] of TComplex32;
  PDAV4ComplexSingleArray = ^TDAV4ComplexSingleArray;
  TDAV4ComplexSingleArray = array [0 .. 3] of TComplex32;

  PDAV2ComplexDoubleArray = ^TDAV2ComplexDoubleArray;
  TDAV2ComplexDoubleArray = array [0 .. 1] of TComplex64;
  PDAV3ComplexDoubleArray = ^TDAV3ComplexDoubleArray;
  TDAV3ComplexDoubleArray = array [0 .. 2] of TComplex64;
  PDAV4ComplexDoubleArray = ^TDAV4ComplexDoubleArray;
  TDAV4ComplexDoubleArray = array [0 .. 3] of TComplex64;

function Complex32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Complex64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function ComplexPolar32(const Magnitude, Angle: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function ComplexPolar64(const Magnitude, Angle: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function ComplexSign32(const Z: TComplex32): Single;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSign64(const Z: TComplex64): Double;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSign32(const Re, Im: Single): Single;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSign64(const Re, Im: Double): Double;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexConjugate32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexConjugate64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexConjugate32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexConjugate64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexInvert32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexInvert64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexInvert32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexInvert64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexMagnitude32(const Re, Im: Single): Single;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMagnitude64(const Re, Im: Double): Double;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMagnitude32(const Complex: TComplex32): Single;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMagnitude64(const Complex: TComplex64): Double;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexArgument32(const Re, Im: Single): Single;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArgument64(const Re, Im: Double): Double;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArgument32(const Complex: TComplex32): Single;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArgument64(const Complex: TComplex64): Double;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexAdd32(const A, B: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexAdd64(const A, B: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexAdd32(const ARe, AIm, BRe, BIm: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexAdd64(const ARe, AIm, BRe, BIm: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexAddInplace32(var A: TComplex32; const B: TComplex32);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexAddInplace64(var A: TComplex64; const B: TComplex64);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexAddInplace32(var ARe, AIm: Single; const BRe, BIm: Single);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexAddInplace64(var ARe, AIm: Double; const BRe, BIm: Double);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSubtract32(const A, B: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSubtract64(const A, B: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSubtract32(const ARe, AIm, BRe, BIm: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSubtract64(const ARe, AIm, BRe, BIm: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexSubtractInplace32(var A: TComplex32; const B: TComplex32);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexSubtractInplace64(var A: TComplex64; const B: TComplex64);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexSubtractInplace32(var ARe, AIm: Single;
  const BRe, BIm: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexSubtractInplace64(var ARe, AIm: Double;
  const BRe, BIm: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexMultiply32(const A, B: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMultiply64(const A, B: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMultiply32(const ARe, AIm, BRe, BIm: Single)
  : TComplex32; overload;
function ComplexMultiply64(const ARe, AIm, BRe, BIm: Double)
  : TComplex64; overload;

procedure ComplexMultiplyInplace32(var A: TComplex32;
  const B: TComplex32); overload;
procedure ComplexMultiplyInplace64(var A: TComplex64;
  const B: TComplex64); overload;
procedure ComplexMultiplyInplace32(var ARe, AIm: Single;
  const BRe, BIm: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexMultiplyInplace64(var ARe, AIm: Double;
  const BRe, BIm: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexMultiply2Inplace32(var A: TComplex32; const B: TComplex32);
procedure ComplexMultiply2Inplace64(var A: TComplex64; const B: TComplex64);

function ComplexDivide32(const A, B: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexDivide64(const A, B: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexDivide32(const ARe, AIm, BRe, BIm: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexDivide64(const ARe, AIm, BRe, BIm: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexDivideInplace32(var A: TComplex32; const B: TComplex32);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexDivideInplace64(var A: TComplex64; const B: TComplex64);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexDivideInplace32(var ARe, AIm: Single; const BRe, BIm: Single);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexDivideInplace64(var ARe, AIm: Double; const BRe, BIm: Double);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexReciprocal32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexReciprocal64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexReciprocal32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexReciprocal64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexReciprocalInplace32(var Z: TComplex32);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexReciprocalInplace64(var Z: TComplex64);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexReciprocalInplace32(var ZRe, ZIm: Single);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexReciprocalInplace64(var ZRe, ZIm: Double);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSqr32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSqr64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSqr32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSqr64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSqrt32(const Re, Im: Single): TComplex32; overload;
function ComplexSqrt64(const Re, Im: Double): TComplex64; overload;
function ComplexSqrt32(const Z: TComplex32): TComplex32; overload;
function ComplexSqrt64(const Z: TComplex64): TComplex64; overload;

function ComplexLog1032(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLog1064(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLog1032(const Complex: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLog1064(const Complex: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexExp32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexExp64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexExp32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexExp64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexLn32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLn64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLn32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLn64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSin32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSin64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSin32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSin64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexCos32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexCos64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexCos32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexCos64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexTan32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexTan64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexTan32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexTan64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSinh32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSinh64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSinh32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSinh64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexCosh32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexCosh64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexCosh32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexCosh64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexTanh32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexTanh64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexTanh32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexTanh64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexArcSin32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcSin64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcSin32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcSin64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexArcCos32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcCos64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcCos32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcCos64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexArcTan32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcTan64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcTan32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcTan64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexArcTanh32(const Re, Im: Single): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcTanh64(const Re, Im: Double): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcTanh32(const Z: TComplex32): TComplex32;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcTanh64(const Z: TComplex64): TComplex64;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

implementation

uses
  Math, DAV_Math;

{ Build Complex Record }

function Complex32(const Re, Im: Single): TComplex32;
begin
  Result.Re := Re;
  Result.Im := Im;
end;

function Complex64(const Re, Im: Double): TComplex64;
begin
  Result.Re := Re;
  Result.Im := Im;
end;

{ Build Complex Record from Magnitude & Angle }

function ComplexPolar32(const Magnitude, Angle: Single): TComplex32;
begin
  Result.Re := Magnitude * Cos(Angle);
  Result.Im := Magnitude * Sin(Angle);
end;

function ComplexPolar64(const Magnitude, Angle: Double): TComplex64;
begin
  Result.Re := Magnitude * Cos(Angle);
  Result.Im := Magnitude * Sin(Angle);
end;

{ Complex Sign Function }

function ComplexSign32(const Z: TComplex32): Single;
begin
  if (Z.Re >= 0) and (Z.Im > 0) then
    Result := 1
  else if (Z.Re <= 0) and (Z.Im < 0) then
    Result := -1
  else
    Result := Sign(Z.Re);
end;

function ComplexSign64(const Z: TComplex64): Double;
begin
  if (Z.Re >= 0) and (Z.Im > 0) then
    Result := 1
  else if (Z.Re <= 0) and (Z.Im < 0) then
    Result := -1
  else
    Result := Sign(Z.Re);
end;

function ComplexSign32(const Re, Im: Single): Single;
begin
  if (Re >= 0) and (Im > 0) then
    Result := 1
  else if (Re <= 0) and (Im < 0) then
    Result := -1
  else
    Result := Sign(Re);
end;

function ComplexSign64(const Re, Im: Double): Double;
begin
  if (Re >= 0) and (Im > 0) then
    Result := 1
  else if (Re <= 0) and (Im < 0) then
    Result := -1
  else
    Result := Sign(Re);
end;

{ ComplexConjugate }

function ComplexConjugate32(const Re, Im: Single): TComplex32;
begin
  Result.Re := Re;
  Result.Im := -Im;
end;

function ComplexConjugate64(const Re, Im: Double): TComplex64;
begin
  Result.Re := Re;
  Result.Im := -Im;
end;

function ComplexConjugate32(const Z: TComplex32): TComplex32;
begin
  Result.Re := Z.Re;
  Result.Im := -Z.Im;
end;

function ComplexConjugate64(const Z: TComplex64): TComplex64;
begin
  Result.Re := Z.Re;
  Result.Im := -Z.Im;
end;

{ ComplexInvert }

function ComplexInvert32(const Re, Im: Single): TComplex32;
begin
  Result.Re := -Re;
  Result.Im := -Im;
end;

function ComplexInvert64(const Re, Im: Double): TComplex64;
begin
  Result.Re := -Re;
  Result.Im := -Im;
end;

function ComplexInvert32(const Z: TComplex32): TComplex32;
begin
  Result.Re := -Z.Re;
  Result.Im := -Z.Im;
end;

function ComplexInvert64(const Z: TComplex64): TComplex64;
begin
  Result.Re := -Z.Re;
  Result.Im := -Z.Im;
end;

{ ComplexMagnitude }

function ComplexMagnitude32(const Re, Im: Single): Single;
begin
  Result := Hypot(Re, Im);
end;

function ComplexMagnitude64(const Re, Im: Double): Double;
begin
  Result := Hypot(Re, Im);
end;

function ComplexMagnitude32(const Complex: TComplex32): Single;
begin
  Result := Hypot(Complex.Re, Complex.Im);
end;

function ComplexMagnitude64(const Complex: TComplex64): Double;
begin
  Result := Hypot(Complex.Re, Complex.Im);
end;

{ ComplexArgument }

function ComplexArgument32(const Re, Im: Single): Single;
begin
  Result := ArcTan2(Im, Re);
end;

function ComplexArgument64(const Re, Im: Double): Double;
begin
  Result := ArcTan2(Im, Re);
end;

function ComplexArgument32(const Complex: TComplex32): Single;
begin
  Result := ArcTan2(Complex.Im, Complex.Re);
end;

function ComplexArgument64(const Complex: TComplex64): Double;
begin
  Result := ArcTan2(Complex.Im, Complex.Re);
end;

{ ComplexAdd }

function ComplexAdd32(const ARe, AIm, BRe, BIm: Single): TComplex32;
begin
  Result.Re := ARe + BRe;
  Result.Im := AIm + BIm;
end;

function ComplexAdd64(const ARe, AIm, BRe, BIm: Double): TComplex64;
begin
  Result.Re := ARe + BRe;
  Result.Im := AIm + BIm;
end;

function ComplexAdd32(const A, B: TComplex32): TComplex32;
begin
  Result.Re := A.Re + B.Re;
  Result.Im := A.Im + B.Im;
end;

function ComplexAdd64(const A, B: TComplex64): TComplex64;
begin
  Result.Re := A.Re + B.Re;
  Result.Im := A.Im + B.Im;
end;

{ ComplexAddInplace }

procedure ComplexAddInplace32(var A: TComplex32; const B: TComplex32);
begin
  A.Re := A.Re + B.Re;
  A.Im := A.Im + B.Im;
end;

procedure ComplexAddInplace64(var A: TComplex64; const B: TComplex64);
begin
  A.Re := A.Re + B.Re;
  A.Im := A.Im + B.Im;
end;

procedure ComplexAddInplace32(var ARe, AIm: Single; const BRe, BIm: Single);
begin
  ARe := ARe + BRe;
  AIm := AIm + BIm;
end;

procedure ComplexAddInplace64(var ARe, AIm: Double; const BRe, BIm: Double);
begin
  ARe := ARe + BRe;
  AIm := AIm + BIm;
end;

{ ComplexSubtract }

function ComplexSubtract32(const ARe, AIm, BRe, BIm: Single): TComplex32;
begin
  Result.Re := ARe - BRe;
  Result.Im := AIm - BIm;
end;

function ComplexSubtract64(const ARe, AIm, BRe, BIm: Double): TComplex64;
begin
  Result.Re := ARe - BRe;
  Result.Im := AIm - BIm;
end;

function ComplexSubtract32(const A, B: TComplex32): TComplex32;
begin
  Result.Re := A.Re - B.Re;
  Result.Im := A.Im - B.Im;
end;

function ComplexSubtract64(const A, B: TComplex64): TComplex64;
begin
  Result.Re := A.Re - B.Re;
  Result.Im := A.Im - B.Im;
end;

{ ComplexSubtractInplace }

procedure ComplexSubtractInplace32(var A: TComplex32; const B: TComplex32);
begin
  A.Re := A.Re - B.Re;
  A.Im := A.Im - B.Im;
end;

procedure ComplexSubtractInplace64(var A: TComplex64; const B: TComplex64);
begin
  A.Re := A.Re - B.Re;
  A.Im := A.Im - B.Im;
end;

procedure ComplexSubtractInplace32(var ARe, AIm: Single;
  const BRe, BIm: Single);
begin
  ARe := ARe - BRe;
  AIm := AIm - BIm;
end;

procedure ComplexSubtractInplace64(var ARe, AIm: Double;
  const BRe, BIm: Double);
begin
  ARe := ARe - BRe;
  AIm := AIm - BIm;
end;

{ ComplexMultiply }

function ComplexMultiply32(const ARe, AIm, BRe, BIm: Single): TComplex32;
begin
  Result.Re := ARe * BRe - AIm * BIm;
  Result.Im := AIm * BRe + ARe * BIm;
end;

function ComplexMultiply64(const ARe, AIm, BRe, BIm: Double): TComplex64;
begin
  Result.Re := ARe * BRe - AIm * BIm;
  Result.Im := AIm * BRe + ARe * BIm;
end;

function ComplexMultiply32(const A, B: TComplex32): TComplex32;
begin
  Result.Re := A.Re * B.Re - A.Im * B.Im;
  Result.Im := A.Im * B.Re + A.Re * B.Im;
end;

function ComplexMultiply64(const A, B: TComplex64): TComplex64;
begin
  Result.Re := A.Re * B.Re - A.Im * B.Im;
  Result.Im := A.Im * B.Re + A.Re * B.Im;
end;

{ ComplexMultiplyInplace }

procedure ComplexMultiplyInplace32(var A: TComplex32; const B: TComplex32);
{$IFDEF PUREPASCAL}
var
  Temp: Single;
begin
  Temp := A.Re;
  A.Re := A.Re * B.Re - A.Im * B.Im;
  A.Im := A.Im * B.Re + Temp * B.Im;
end;
{$ELSE}
asm
        FLD     A.Re.Single   // A.Re
        FLD     A.Im.Single   // A.Im, A.Re
        FLD     B.Re.Single   // B.Re, A.Im, A.Re
        FLD     B.Im.Single   // B.Im, B.Re, A.Im, A.Re
        FLD     ST(3)         // A.Re, B.Im, B.Re, A.Im, A.Re
        FMUL    ST(0), ST(2)  // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
        FLD     ST(3)         // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
        FMUL    ST(0), ST(2)  // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
        FSUBP                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
        FSTP    A.Re.Single   // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
        FXCH    ST(2)         // A.Im, B.Re, B.Im, A.Re
        FMULP                 // A.Im * B.Re, B.Im, A.Re
        FXCH    ST(2)         // B.Im, A.Re, A.Im * B.Re
        FMULP                 // B.Im * A.Re, A.Im * B.Re
        FADDP                 // A.Im * B.Re + A.Re * B.Im
        FSTP    A.Im.Single   // A.Im := A.Im * B.Re + A.Re * B.Im
end;
{$ENDIF}

procedure ComplexMultiplyInplace64(var A: TComplex64; const B: TComplex64);
{$IFDEF PUREPASCAL}
var
  Temp: Double;
begin
  Temp := A.Re;
  A.Re := A.Re * B.Re - A.Im * B.Im;
  A.Im := A.Im * B.Re + Temp * B.Im;
end;
{$ELSE}
asm
        FLD     A.Re.Double   // A.Re
        FLD     A.Im.Double   // A.Im, A.Re
        FLD     B.Re.Double   // B.Re, A.Im, A.Re
        FLD     B.Im.Double   // B.Im, B.Re, A.Im, A.Re
        FLD     ST(3)         // A.Re, B.Im, B.Re, A.Im, A.Re
        FMUL    ST(0), ST(2)  // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
        FLD     ST(3)         // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
        FMUL    ST(0), ST(2)  // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
        FSUBP                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
        FSTP    A.Re.Double   // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
        FXCH    ST(2)         // A.Im, B.Re, B.Im, A.Re
        FMULP                 // A.Im * B.Re, B.Im, A.Re
        FXCH    ST(2)         // B.Im, A.Re, A.Im * B.Re
        FMULP                 // B.Im * A.Re, A.Im * B.Re
        FADDP                 // A.Im * B.Re + A.Re * B.Im
        FSTP    A.Im.Double   // A.Im := A.Im * B.Re + A.Re * B.Im
end;
{$ENDIF}

procedure ComplexMultiplyInplace32(var ARe, AIm: Single;
  const BRe, BIm: Single);
var
  Tmp: Single;
begin
  Tmp := ARe;
  ARe := ARe * BRe - AIm * BIm;
  AIm := AIm * BRe + Tmp * BIm;
end;

procedure ComplexMultiplyInplace64(var ARe, AIm: Double;
  const BRe, BIm: Double);
var
  Tmp: Double;
begin
  Tmp := ARe;
  ARe := ARe * BRe - AIm * BIm;
  AIm := AIm * BRe + Tmp * BIm;
end;

{ ComplexMultiply2Inplace32 }

procedure ComplexMultiply2Inplace32(var A: TComplex32; const B: TComplex32);
{$IFDEF PUREPASCAL}
var
  Temp: Single;
begin
  Temp := A.Re;
  A.Re := A.Re * (Sqr(B.Re) - Sqr(B.Im)) - 2 * A.Im * B.Im * B.Re;
  A.Im := A.Im * (Sqr(B.Re) - Sqr(B.Im)) + 2 * Temp * B.Im * B.Re;
end;
{$ELSE}
asm
        FLD     B.Re          // B.Re
        FLD     B.Im          // B.Im, B.Re
        FMULP                 // B.Im * B.Re
        FADD    ST(0), ST(0)  // 2 * B.Im * B.Re = B''

        FLD     B.Re          // B.Re, B''
        FMUL    ST(0), ST(0)  // B.Re�, B''
        FLD     B.Im          // B.Im, B.Re�, B''
        FMUL    ST(0), ST(0)  // B.Im�, B.Re�, B''
        FSUBP                 // B.Im� + B.Re� = B', B''

        FLD     A.Re          // A.Re, B', B''
        FMUL    ST(0), ST(1)  // A.Re * B', B', B''
        FLD     A.Im          // A.Im, A.Re * B', B', B''
        FMUL    ST(0), ST(3)  // A.Im * B'', A.Re * B', B', B''
        FSUBP                 // A.Re * B' - A.Im * B'' := New A.Re, B', B''

        FXCH    ST(2)         // B'', B', New A.Re
        FMUL    A.Re          // A.Re * B'', B', New A.Re
        FXCH    ST(1)         // B', A.Re * B'', New A.Re
        FMUL    A.Im          // A.Im * B', A.Re * B'', New A.Re
        FADDP                 // A.Im * B' + A.Re * B'' := New A.Im, New A.Re
        FSTP    A.Im          // New A.Re
        FSTP    A.Re
end;
{$ENDIF}

procedure ComplexMultiply2Inplace64(var A: TComplex64; const B: TComplex64);
{$IFDEF PUREPASCAL}
var
  Btmp: Double;
  Temp: Double;
begin
  Btmp := (Sqr(B.Re) - Sqr(B.Im));
  Temp := A.Re;
  A.Re := A.Re * Btmp - 2 * A.Im * B.Im * B.Re;
  A.Im := A.Im * Btmp + 2 * Temp * B.Im * B.Re;
end;
{$ELSE}
asm
        FLD     B.Re          // B.Re
        FLD     B.Im          // B.Im, B.Re
        FMULP                 // B.Im * B.Re
        FADD    ST(0), ST(0)  // 2 * B.Im * B.Re = B''

        FLD     B.Re          // B.Re, B''
        FMUL    ST(0), ST(0)  // B.Re�, B''
        FLD     B.Im          // B.Im, B.Re�, B''
        FMUL    ST(0), ST(0)  // B.Im�, B.Re�, B''
        FSUBP                 // B.Im� + B.Re� = B', B''

        FLD     A.Re          // A.Re, B', B''
        FMUL    ST(0), ST(1)  // A.Re * B', B', B''
        FLD     A.Im          // A.Im, A.Re * B', B', B''
        FMUL    ST(0), ST(3)  // A.Im * B'', A.Re * B', B', B''
        FSUBP                 // A.Re * B' - A.Im * B'' := New A.Re, B', B''

        FXCH    ST(2)         // B'', B', New A.Re
        FMUL    A.Re          // A.Re * B'', B', New A.Re
        FXCH    ST(1)         // B', A.Re * B'', New A.Re
        FMUL    A.Im          // A.Im * B', A.Re * B'', New A.Re
        FADDP                 // A.Im * B' + A.Re * B'' := New A.Im, New A.Re
        FSTP    A.Im          // New A.Re
        FSTP    A.Re
end;
{$ENDIF}


{ ComplexDivide }

function ComplexDivide32(const ARe, AIm, BRe, BIm: Single): TComplex32;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(BRe) + Sqr(BIm));
  Result.Re := (ARe * BRe + AIm * BIm) * Divisor;
  Result.Im := (AIm * BRe - ARe * BIm) * Divisor;
end;

function ComplexDivide64(const ARe, AIm, BRe, BIm: Double): TComplex64;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(BRe) + Sqr(BIm));
  Result.Re := (ARe * BRe + AIm * BIm) * Divisor;
  Result.Im := (AIm * BRe - ARe * BIm) * Divisor;
end;

function ComplexDivide32(const A, B: TComplex32): TComplex32;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(B.Re) + Sqr(B.Im));
  Result.Re := (A.Re * B.Re + A.Im * B.Im) * Divisor;
  Result.Im := (A.Im * B.Re - A.Re * B.Im) * Divisor;
end;

function ComplexDivide64(const A, B: TComplex64): TComplex64;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(B.Re) + Sqr(B.Im));
  Result.Re := (A.Re * B.Re + A.Im * B.Im) * Divisor;
  Result.Im := (A.Im * B.Re - A.Re * B.Im) * Divisor;
end;

{ ComplexDivideInplace }

procedure ComplexDivideInplace32(var A: TComplex32; const B: TComplex32);
var
  Divisor, Temp: Double;
begin
  Divisor := 1 / (Sqr(B.Re) + Sqr(B.Im));
  Temp := A.Re;
  A.Re := (A.Re * B.Re + A.Im * B.Im) * Divisor;
  A.Im := (A.Im * B.Re - Temp * B.Im) * Divisor;
end;

procedure ComplexDivideInplace64(var A: TComplex64; const B: TComplex64);
var
  Divisor, Temp: Double;
begin
  Divisor := 1 / (Sqr(B.Re) + Sqr(B.Im));
  Temp := A.Re;
  A.Re := (A.Re * B.Re + A.Im * B.Im) * Divisor;
  A.Im := (A.Im * B.Re - Temp * B.Im) * Divisor;
end;

procedure ComplexDivideInplace32(var ARe, AIm: Single; const BRe, BIm: Single);
var
  Divisor, Temp: Double;
begin
  Divisor := 1 / (Sqr(BRe) + Sqr(BIm));
  Temp := ARe;
  ARe := (ARe * BRe + AIm * BIm) * Divisor;
  AIm := (AIm * BRe - Temp * BIm) * Divisor;
end;

procedure ComplexDivideInplace64(var ARe, AIm: Double; const BRe, BIm: Double);
var
  Divisor, Temp: Double;
begin
  Divisor := 1 / (Sqr(BRe) + Sqr(BIm));
  Temp := ARe;
  ARe := (ARe * BRe + AIm * BIm) * Divisor;
  AIm := (AIm * BRe - Temp * BIm) * Divisor;
end;

{ ComplexReciprocal }

function ComplexReciprocal32(const Z: TComplex32): TComplex32;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(Z.Re) + Sqr(Z.Im));
  Result.Re := Z.Re * Divisor;
  Result.Im := Z.Im * Divisor;
end;

function ComplexReciprocal64(const Z: TComplex64): TComplex64;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(Z.Re) + Sqr(Z.Im));
  Result.Re := Z.Re * Divisor;
  Result.Im := Z.Im * Divisor;
end;

function ComplexReciprocal32(const Re, Im: Single): TComplex32;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(Re) + Sqr(Im));
  Result.Re := Re * Divisor;
  Result.Im := Im * Divisor;
end;

function ComplexReciprocal64(const Re, Im: Double): TComplex64;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(Re) + Sqr(Im));
  Result.Re := Re * Divisor;
  Result.Im := Im * Divisor;
end;

{ ComplexReciprocalInplace }

procedure ComplexReciprocalInplace32(var Z: TComplex32);
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(Z.Re) + Sqr(Z.Im));
  Z.Re := Z.Re * Divisor;
  Z.Im := Z.Im * Divisor;
end;

procedure ComplexReciprocalInplace64(var Z: TComplex64);
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(Z.Re) + Sqr(Z.Im));
  Z.Re := Z.Re * Divisor;
  Z.Im := Z.Im * Divisor;
end;

procedure ComplexReciprocalInplace32(var ZRe, ZIm: Single);
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(ZRe) + Sqr(ZIm));
  ZRe := ZRe * Divisor;
  ZIm := ZIm * Divisor;
end;

procedure ComplexReciprocalInplace64(var ZRe, ZIm: Double);
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(ZRe) + Sqr(ZIm));
  ZRe := ZRe * Divisor;
  ZIm := ZIm * Divisor;
end;

{ ComplexSqr }

function ComplexSqr32(const Re, Im: Single): TComplex32;
begin
  Result.Re := Sqr(Re) - Sqr(Im);
  Result.Im := 2 * Re * Im;
end;

function ComplexSqr64(const Re, Im: Double): TComplex64;
begin
  Result.Re := Sqr(Re) - Sqr(Im);
  Result.Im := 2 * Re * Im;
end;

function ComplexSqr32(const Z: TComplex32): TComplex32;
begin
  Result.Re := Sqr(Z.Re) - Sqr(Z.Im);
  Result.Im := 2 * Z.Re * Z.Im;
end;

function ComplexSqr64(const Z: TComplex64): TComplex64;
begin
  Result.Re := Sqr(Z.Re) - Sqr(Z.Im);
  Result.Im := 2 * Z.Re * Z.Im;
end;

{ ComplexSqrt }

function ComplexSqrt32(const Re, Im: Single): TComplex32;

  function FastSqrt(x: Single): Double;
  begin
    if x > 0 then
      Result := Sqrt(x)
    else
      Result := 0;
  end;

var
  Mag: Single;
begin
  Mag := ComplexMagnitude32(Re, Im);
  Result.Re := FastSqrt(0.5 * (Mag + Re));
  Result.Im := FastSqrt(0.5 * (Mag - Re));
  if (Im < 0.0) then
    Result.Im := -Result.Im;
end;

function ComplexSqrt64(const Re, Im: Double): TComplex64;

  function FastSqrt(x: Double): Double;
  begin
    if x > 0 then
      Result := Sqrt(x)
    else
      Result := 0;
  end;

var
  Mag: Double;
begin
  Mag := ComplexMagnitude64(Re, Im);
  Result.Re := FastSqrt(0.5 * (Mag + Re));
  Result.Im := FastSqrt(0.5 * (Mag - Re));
  if (Im < 0.0) then
    Result.Im := -Result.Im;
end;

function ComplexSqrt32(const Z: TComplex32): TComplex32;

  function FastSqrt(x: Single): Double;
  begin
    if x > 0 then
      Result := Sqrt(x)
    else
      Result := 0;
  end;

var
  Mag: Single;
begin
  Mag := ComplexMagnitude32(Z);
  Result.Re := FastSqrt(0.5 * (Mag + Z.Re));
  Result.Im := FastSqrt(0.5 * (Mag - Z.Re));
  if (Z.Im < 0.0) then
    Result.Im := -Result.Im;
end;

function ComplexSqrt64(const Z: TComplex64): TComplex64;

  function FastSqrt(x: Double): Double;
  begin
    if x > 0 then
      Result := Sqrt(x)
    else
      Result := 0;
  end;

var
  Mag: Double;
begin
  Mag := ComplexMagnitude64(Z);
  Result.Re := FastSqrt(0.5 * (Mag + Z.Re));
  Result.Im := FastSqrt(0.5 * (Mag - Z.Re));
  if (Z.Im < 0.0) then
    Result.Im := -Result.Im;
end;

{ ComplexLog10 }

function ComplexLog1032(const Re, Im: Single): TComplex32;
begin
  Result.Re := 0.5 * Log10((Sqr(Re) + Sqr(Im)));
  Result.Im := ArcTan2(Im, Re);
end;

function ComplexLog1064(const Re, Im: Double): TComplex64;
begin
  Result.Re := 0.5 * Log10((Sqr(Re) + Sqr(Im)));
  Result.Im := ArcTan2(Im, Re);
end;

function ComplexLog1032(const Complex: TComplex32): TComplex32;
begin
  Result.Re := 0.5 * Log10((Sqr(Complex.Re) + Sqr(Complex.Im)));
  Result.Im := ArcTan2(Complex.Im, Complex.Re);
end;

function ComplexLog1064(const Complex: TComplex64): TComplex64;
begin
  Result.Re := 0.5 * Log10((Sqr(Complex.Re) + Sqr(Complex.Im)));
  Result.Im := ArcTan2(Complex.Im, Complex.Re);
end;

{ ComplexExp }

function ComplexExp32(const Re, Im: Single): TComplex32;
begin
  Result.Im := Exp(Re);
  Result.Re := Result.Im * Cos(Im);
  Result.Im := Result.Im * Sin(Im);
end;

function ComplexExp64(const Re, Im: Double): TComplex64;
begin
  Result.Im := Exp(Re);
  Result.Re := Result.Im * Cos(Im);
  Result.Im := Result.Im * Sin(Im);
end;

function ComplexExp32(const Z: TComplex32): TComplex32;
begin
  Result.Im := Exp(Z.Re);
  Result.Re := Result.Im * Cos(Z.Im);
  Result.Im := Result.Im * Sin(Z.Im);
end;

function ComplexExp64(const Z: TComplex64): TComplex64;
begin
  Result.Im := Exp(Z.Re);
  Result.Re := Result.Im * Cos(Z.Im);
  Result.Im := Result.Im * Sin(Z.Im);
end;

{ ComplexLn }

function ComplexLn32(const Re, Im: Single): TComplex32;
begin
  Result.Re := Ln(Hypot(Re, Im));
  Result.Im := ArcTan2(Im, Re);
end;

function ComplexLn64(const Re, Im: Double): TComplex64;
begin
  Result.Re := Ln(Hypot(Re, Im));
  Result.Im := ArcTan2(Im, Re);
end;

function ComplexLn32(const Z: TComplex32): TComplex32;
begin
  Result.Re := Ln(Hypot(Z.Re, Z.Im));
  Result.Im := ArcTan2(Z.Im, Z.Re);
end;

function ComplexLn64(const Z: TComplex64): TComplex64;
begin
  Result.Re := Ln(Hypot(Z.Re, Z.Im));
  Result.Im := ArcTan2(Z.Im, Z.Re);
end;

{ ComplexSin }

function ComplexSin32(const Re, Im: Single): TComplex32;
begin
  Result.Re := Exp(-Im);
  Result.Im := 0.5 * Cos(Re) * (1 / Result.Re - Result.Re);
  Result.Re := 0.5 * Sin(Re) * (1 / Result.Re + Result.Re);
end;

function ComplexSin64(const Re, Im: Double): TComplex64;
begin
  Result.Re := Exp(-Im);
  Result.Im := 0.5 * Cos(Re) * (1 / Result.Re - Result.Re);
  Result.Re := 0.5 * Sin(Re) * (1 / Result.Re + Result.Re);
end;

function ComplexSin32(const Z: TComplex32): TComplex32;
begin
  Result.Re := Exp(-Z.Im);
  Result.Im := 0.5 * Cos(Z.Re) * (1 / Result.Re - Result.Re);
  Result.Re := 0.5 * Sin(Z.Re) * (1 / Result.Re + Result.Re);
end;

function ComplexSin64(const Z: TComplex64): TComplex64;
begin
  Result.Re := Exp(-Z.Im);
  Result.Im := 0.5 * Cos(Z.Re) * (1 / Result.Re - Result.Re);
  Result.Re := 0.5 * Sin(Z.Re) * (1 / Result.Re + Result.Re);
end;

{ ComplexCos }

function ComplexCos32(const Re, Im: Single): TComplex32;
begin
  Result.Im := Exp(Im);
  Result.Re := 0.5 * Cos(Re) * (1 / Result.Im + Result.Im);
  Result.Im := 0.5 * Sin(Re) * (1 / Result.Im - Result.Im);
end;

function ComplexCos64(const Re, Im: Double): TComplex64;
begin
  Result.Im := Exp(Im);
  Result.Re := 0.5 * Cos(Re) * (1 / Result.Im + Result.Im);
  Result.Im := 0.5 * Sin(Re) * (1 / Result.Im - Result.Im);
end;

function ComplexCos32(const Z: TComplex32): TComplex32;
begin
  Result.Im := Exp(Z.Im);
  Result.Re := 0.5 * Cos(Z.Re) * (1 / Result.Im + Result.Im);
  Result.Im := 0.5 * Sin(Z.Re) * (1 / Result.Im - Result.Im);
end;

function ComplexCos64(const Z: TComplex64): TComplex64;
begin
  Result.Im := Exp(Z.Im);
  Result.Re := 0.5 * Cos(Z.Re) * (1 / Result.Im + Result.Im);
  Result.Im := 0.5 * Sin(Z.Re) * (1 / Result.Im - Result.Im);
end;

{ ComplexTan }

function ComplexTan32(const Re, Im: Single): TComplex32;
var
  ExpIm: Single;
  ExpRe: TComplex32;
  Divisor: Single;
begin
  ExpIm := Exp(Im);
  GetSinCos(Re, ExpRe.Im, ExpRe.Re);

  Divisor := 1 / (Sqr(ExpRe.Re * (Sqr(ExpIm) + 1)) +
    Sqr(ExpRe.Im * (Sqr(ExpIm) - 1)));
  Result.Re := ExpRe.Im * ExpRe.Re * 4 * Sqr(ExpIm) * Divisor;
  Result.Im := (Sqr(ExpRe.Re) * (Sqr(Sqr(ExpIm)) - 1) + Sqr(ExpRe.Im) *
    (Sqr(Sqr(ExpIm)) - 1)) * Divisor;
end;

function ComplexTan64(const Re, Im: Double): TComplex64;
var
  ExpIm: Double;
  ExpRe: TComplex64;
  Divisor: Double;
begin
  ExpIm := Exp(Im);
  GetSinCos(Re, ExpRe.Im, ExpRe.Re);

  Divisor := 1 / (Sqr(ExpRe.Re * (Sqr(ExpIm) + 1)) +
    Sqr(ExpRe.Im * (Sqr(ExpIm) - 1)));
  Result.Re := ExpRe.Im * ExpRe.Re * 4 * Sqr(ExpIm) * Divisor;
  Result.Im := (Sqr(ExpRe.Re) * (Sqr(Sqr(ExpIm)) - 1) + Sqr(ExpRe.Im) *
    (Sqr(Sqr(ExpIm)) - 1)) * Divisor;
end;

function ComplexTan32(const Z: TComplex32): TComplex32;
var
  Value: array [0 .. 1] of TComplex32;
begin
  Value[0] := ComplexExp32(Z.Im, -Z.Re);
  Value[1] := ComplexExp32(-Z.Im, Z.Re);
  Value[0] := ComplexDivide32(ComplexSubtract32(Value[0], Value[1]),
    ComplexAdd32(Value[0], Value[1]));

  Result.Re := -Value[0].Im;
  Result.Im := Value[0].Re;
end;

function ComplexTan64(const Z: TComplex64): TComplex64;
var
  ExpIm: Double;
  ExpRe: TComplex64;
  Divisor: Double;
begin
  ExpIm := Exp(Z.Im);
  GetSinCos(Z.Re, ExpRe.Im, ExpRe.Re);

  Divisor := 1 / (Sqr(ExpRe.Re * (Sqr(ExpIm) + 1)) +
    Sqr(ExpRe.Im * (Sqr(ExpIm) - 1)));
  Result.Re := ExpRe.Im * ExpRe.Re * 4 * Sqr(ExpIm) * Divisor;
  Result.Im := (Sqr(ExpRe.Re) * (Sqr(Sqr(ExpIm)) - 1) + Sqr(ExpRe.Im) *
    (Sqr(Sqr(ExpIm)) - 1)) * Divisor;
end;

{ ComplexSinh }

function ComplexSinh32(const Re, Im: Single): TComplex32;
begin
  Result.Im := Exp(Re);
  Result.Re := 0.5 * Cos(Im) * (1 / Result.Im - Result.Im);
  Result.Im := 0.5 * Sin(Im) * (1 / Result.Im + Result.Im);
end;

function ComplexSinh64(const Re, Im: Double): TComplex64;
begin
  Result.Im := Exp(Re);
  Result.Re := 0.5 * Cos(Im) * (1 / Result.Im - Result.Im);
  Result.Im := 0.5 * Sin(Im) * (1 / Result.Im + Result.Im);
end;

function ComplexSinh32(const Z: TComplex32): TComplex32;
begin
  Result.Im := Exp(Z.Re);
  Result.Re := 0.5 * Cos(Z.Im) * (1 / Result.Im - Result.Im);
  Result.Im := 0.5 * Sin(Z.Im) * (1 / Result.Im + Result.Im);
end;

function ComplexSinh64(const Z: TComplex64): TComplex64;
begin
  Result.Im := Exp(Z.Re);
  Result.Re := 0.5 * Cos(Z.Im) * (1 / Result.Im - Result.Im);
  Result.Im := 0.5 * Sin(Z.Im) * (1 / Result.Im + Result.Im);
end;

{ ComplexCosh }

function ComplexCosh32(const Re, Im: Single): TComplex32;
begin
  Result.Im := Exp(Re);
  Result.Re := 0.5 * Cos(Im) * (1 / Result.Im + Result.Im);
  Result.Im := 0.5 * Sin(Im) * (1 / Result.Im - Result.Im);
end;

function ComplexCosh64(const Re, Im: Double): TComplex64;
begin
  Result.Im := Exp(Re);
  Result.Re := 0.5 * Cos(Im) * (1 / Result.Im + Result.Im);
  Result.Im := 0.5 * Sin(Im) * (1 / Result.Im - Result.Im);
end;

function ComplexCosh32(const Z: TComplex32): TComplex32;
begin
  Result.Im := Exp(Z.Re);
  Result.Re := 0.5 * Cos(Z.Im) * (1 / Result.Im + Result.Im);
  Result.Im := 0.5 * Sin(Z.Im) * (1 / Result.Im - Result.Im);
end;

function ComplexCosh64(const Z: TComplex64): TComplex64;
begin
  Result.Im := Exp(Z.Re);
  Result.Re := 0.5 * Cos(Z.Im) * (1 / Result.Im + Result.Im);
  Result.Im := 0.5 * Sin(Z.Im) * (1 / Result.Im - Result.Im);
end;

{ ComplexTanh }

function ComplexTanh32(const Re, Im: Single): TComplex32;
var
  ExpRe: Single;
  ExpIm: TComplex32;
  Temp: TComplex32;
begin
  ExpRe := Exp(Re);
  GetSinCos(Im, ExpIm.Im, ExpIm.Re);

  Temp.Re := ExpIm.Re * (ExpRe - 1 / ExpRe);
  Temp.Im := ExpIm.Im * (ExpRe + 1 / ExpRe);

  Result.Re := ExpIm.Re * (ExpRe + 1 / ExpRe);
  Result.Im := ExpIm.Im * (ExpRe - 1 / ExpRe);

  Result := ComplexDivide32(Temp, Result);
end;

function ComplexTanh64(const Re, Im: Double): TComplex64;
var
  ExpRe: Double;
  ExpIm: TComplex64;
  Temp: TComplex64;
begin
  ExpRe := Exp(Re);
  GetSinCos(Im, ExpIm.Im, ExpIm.Re);

  Temp.Re := ExpIm.Re * (ExpRe - 1 / ExpRe);
  Temp.Im := ExpIm.Im * (ExpRe + 1 / ExpRe);

  Result.Re := ExpIm.Re * (ExpRe + 1 / ExpRe);
  Result.Im := ExpIm.Im * (ExpRe - 1 / ExpRe);

  Result := ComplexDivide64(Temp, Result);
end;

function ComplexTanh32(const Z: TComplex32): TComplex32;
var
  ExpRe: Single;
  ExpIm: TComplex32;
  Temp: TComplex32;
begin
  ExpRe := Exp(Z.Re);
  GetSinCos(Z.Im, ExpIm.Im, ExpIm.Re);

  Temp.Re := ExpIm.Re * (ExpRe - 1 / ExpRe);
  Temp.Im := ExpIm.Im * (ExpRe + 1 / ExpRe);

  Result.Re := ExpIm.Re * (ExpRe + 1 / ExpRe);
  Result.Im := ExpIm.Im * (ExpRe - 1 / ExpRe);

  Result := ComplexDivide32(Temp, Result);
end;

function ComplexTanh64(const Z: TComplex64): TComplex64;
var
  ExpRe: Double;
  ExpIm: TComplex64;
  Temp: TComplex64;
begin
  ExpRe := Exp(Z.Re);
  GetSinCos(Z.Im, ExpIm.Im, ExpIm.Re);

  Temp.Re := ExpIm.Re * (ExpRe - 1 / ExpRe);
  Temp.Im := ExpIm.Im * (ExpRe + 1 / ExpRe);

  Result.Re := ExpIm.Re * (ExpRe + 1 / ExpRe);
  Result.Im := ExpIm.Im * (ExpRe - 1 / ExpRe);

  Result := ComplexDivide64(Temp, Result);
end;

{ ComplexArcSin }

function ComplexArcSin32(const Re, Im: Single): TComplex32;
var
  SqrMag: Single;
begin
  SqrMag := Sqr(Sqr(Re) + Sqr(Im));
  Result.Re := 0.5 * Sign(Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im))
    - SqrMag);
  Result.Im := 0.5 * Sign(Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im))
    + SqrMag);
end;

function ComplexArcSin64(const Re, Im: Double): TComplex64;
var
  SqrMag: Double;
begin
  SqrMag := Sqr(Sqr(Re) + Sqr(Im));
  Result.Re := 0.5 * Sign(Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im))
    - SqrMag);
  Result.Im := 0.5 * Sign(Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im))
    + SqrMag);
end;

function ComplexArcSin32(const Z: TComplex32): TComplex32;
var
  SqrMag: Single;
begin
  SqrMag := Sqr(Sqr(Z.Re) + Sqr(Z.Im));
  Result.Re := 0.5 * Sign(Z.Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im))
    - SqrMag);
  Result.Im := 0.5 * Sign(Z.Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im))
    + SqrMag);
end;

function ComplexArcSin64(const Z: TComplex64): TComplex64;
var
  SqrMag: Double;
begin
  SqrMag := Sqr(Sqr(Z.Re) + Sqr(Z.Im));
  Result.Re := 0.5 * Sign(Z.Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im))
    - SqrMag);
  Result.Im := 0.5 * Sign(Z.Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im))
    + SqrMag);
end;

{ ComplexArcCos }

function ComplexArcCos32(const Re, Im: Single): TComplex32;
var
  SqrMag: Single;
begin
  SqrMag := Sqr(Sqr(Re) + Sqr(Im));
  Result.Re := 0.5 * (Pi - Sign(Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im))
    - SqrMag));
  Result.Im := -0.5 * Sign(Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im))
    + SqrMag);
end;

function ComplexArcCos64(const Re, Im: Double): TComplex64;
var
  SqrMag: Double;
begin
  SqrMag := Sqr(Sqr(Re) + Sqr(Im));
  Result.Re := 0.5 * (Pi - Sign(Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im))
    - SqrMag));
  Result.Im := -0.5 * Sign(Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im))
    + SqrMag);
end;

function ComplexArcCos32(const Z: TComplex32): TComplex32;
var
  SqrMag: Single;
begin
  SqrMag := Sqr(Sqr(Z.Re) + Sqr(Z.Im));
  Result.Re := 0.5 * (Pi - Sign(Z.Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im)
    ) - SqrMag));
  Result.Im := -0.5 * Sign(Z.Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im))
    + SqrMag);
end;

function ComplexArcCos64(const Z: TComplex64): TComplex64;
var
  SqrMag: Double;
begin
  SqrMag := Sqr(Sqr(Z.Re) + Sqr(Z.Im));
  Result.Re := 0.5 * (Pi - Sign(Z.Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im)
    ) - SqrMag));
  Result.Im := -0.5 * Sign(Z.Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im))
    + SqrMag);
end;

{ ComplexArcTan }

function ComplexArcTan32(const Re, Im: Single): TComplex32;
begin
  Result.Re := 0.5 * (ComplexArgument32(1 + Im, Re) -
    ComplexArgument32(1 - Im, Re));
  Result.Im := 0.5 * (Ln(ComplexMagnitude32(1 - Im, Re)) -
    Ln(ComplexMagnitude32(1 + Im, Re)));
end;

function ComplexArcTan64(const Re, Im: Double): TComplex64;
begin
  Result.Re := 0.5 * (ComplexArgument64(1 + Im, Re) -
    ComplexArgument64(1 - Im, Re));
  Result.Im := 0.5 * (Ln(ComplexMagnitude64(1 - Im, Re)) -
    Ln(ComplexMagnitude64(1 + Im, Re)));
end;

function ComplexArcTan32(const Z: TComplex32): TComplex32;
begin
  Result.Re := 0.5 * (ComplexArgument32(1 + Z.Im, Z.Re) -
    ComplexArgument32(1 - Z.Im, Z.Re));
  Result.Im := 0.5 * (Ln(ComplexMagnitude32(1 - Z.Im, Z.Re)) -
    Ln(ComplexMagnitude32(1 + Z.Im, Z.Re)));
end;

function ComplexArcTan64(const Z: TComplex64): TComplex64;
begin
  Result.Re := 0.5 * (ComplexArgument64(1 + Z.Im, Z.Re) -
    ComplexArgument64(1 - Z.Im, Z.Re));
  Result.Im := 0.5 * (Ln(ComplexMagnitude64(1 - Z.Im, Z.Re)) -
    Ln(ComplexMagnitude64(1 + Z.Im, Z.Re)));
end;

{ ComplexArcTanh }

function ComplexArcTanh32(const Re, Im: Single): TComplex32;
begin
  Result.Re := 0.5 * (Ln(ComplexMagnitude32(1 + Re, Im)) -
    Ln(ComplexMagnitude32(1 - Re, Im)));
  Result.Im := 0.5 * (ComplexArgument32(1 + Re, Im) -
    ComplexArgument32(1 - Re, Im));
end;

function ComplexArcTanh64(const Re, Im: Double): TComplex64;
begin
  Result.Re := 0.5 * (Ln(ComplexMagnitude64(1 + Re, Im)) -
    Ln(ComplexMagnitude64(1 - Re, Im)));
  Result.Im := 0.5 * (ComplexArgument64(1 + Re, Im) -
    ComplexArgument64(1 - Re, Im));
end;

function ComplexArcTanh32(const Z: TComplex32): TComplex32;
begin
  Result.Re := 0.5 * (Ln(ComplexMagnitude32(1 + Z.Re, Z.Im)) -
    Ln(ComplexMagnitude32(1 - Z.Re, Z.Im)));
  Result.Im := 0.5 * (ComplexArgument32(1 + Z.Re, Z.Im) -
    ComplexArgument32(1 - Z.Re, Z.Im));
end;

function ComplexArcTanh64(const Z: TComplex64): TComplex64;
begin
  Result.Re := 0.5 * (Ln(ComplexMagnitude64(1 + Z.Re, Z.Im)) -
    Ln(ComplexMagnitude64(1 - Z.Re, Z.Im)));
  Result.Im := 0.5 * (ComplexArgument64(1 + Z.Re, Z.Im) -
    ComplexArgument64(1 - Z.Re, Z.Im));
end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
{ TComplex32 }

{$IFNDEF FPC}

constructor TComplex32.Create(const Re, Im: Single);
begin
  Self.Re := Re;
  Self.Im := Im;
end;
{$ENDIF}

class operator TComplex32.Add(const Lhs, Rhs: TComplex32): TComplex32;
begin
  Result := ComplexAdd32(Lhs, Rhs);
end;

class operator TComplex32.Divide(const Lhs, Rhs: TComplex32): TComplex32;
begin
  Result := ComplexDivide32(Lhs, Rhs);
end;

class operator TComplex32.Equal(const Lhs, Rhs: TComplex32): Boolean;
begin
  Result := (Lhs.Re = Rhs.Re) and (Lhs.Im = Rhs.Im);
end;

class operator TComplex32.Multiply(const Lhs, Rhs: TComplex32): TComplex32;
begin
  Result := ComplexMultiply32(Lhs, Rhs);
end;

class operator TComplex32.Negative(const Value: TComplex32): TComplex32;
begin
  Result.Re := -Value.Re;
  Result.Im := Value.Im;
end;

class operator TComplex32.NotEqual(const Lhs, Rhs: TComplex32): Boolean;
begin
  Result := (Lhs.Re <> Rhs.Re) or (Lhs.Im <> Rhs.Im);
end;

class operator TComplex32.Subtract(const Lhs, Rhs: TComplex32): TComplex32;
begin
  Result := ComplexSubtract32(Lhs, Rhs);
end;

class function TComplex32.Zero: TComplex32;
begin
  Result.Re := 0;
  Result.Im := 0;
end;

class function TComplex32.Euler(const Value: Single): TComplex32;
begin
  GetSinCos(Value, Result.Im, Result.Re);
end;

class function TComplex32.Euler(const Value: Double): TComplex32;
begin
  GetSinCos(Value, Result.Im, Result.Re);
end;

{$IFNDEF FPC}

class function TComplex32.Euler(const Value: Extended): TComplex32;
begin
  GetSinCos(Value, Result.Im, Result.Re);
end;
{$ENDIF}

function TComplex32.Magnitude: Single;
begin
  Result := Hypot(Re, Im);
end;

procedure TComplex32.ComputeEuler(const Value: Single);
begin
  GetSinCos(Value, Self.Im, Self.Re);
end;

procedure TComplex32.ComputeEuler(const Value: Double);
begin
  GetSinCos(Value, Self.Im, Self.Re);
end;

{$IFNDEF FPC}

procedure TComplex32.ComputeEuler(const Value: Extended);
begin
  GetSinCos(Value, Self.Im, Self.Re);
end;
{$ENDIF}
{ TComplex64 }

{$IFNDEF FPC}

constructor TComplex64.Create(const Re, Im: Double);
begin
  Self.Re := 0;
  Self.Im := 0;
end;
{$ENDIF}

class operator TComplex64.Add(const Lhs, Rhs: TComplex64): TComplex64;
begin
  Result := ComplexAdd64(Lhs, Rhs);
end;

class operator TComplex64.Divide(const Lhs, Rhs: TComplex64): TComplex64;
begin
  Result := ComplexDivide64(Lhs, Rhs);
end;

class operator TComplex64.Equal(const Lhs, Rhs: TComplex64): Boolean;
begin
  Result := (Lhs.Re = Rhs.Re) and (Lhs.Im = Rhs.Im);
end;

class operator TComplex64.Multiply(const Lhs, Rhs: TComplex64): TComplex64;
begin
  Result := ComplexMultiply64(Lhs, Rhs);
end;

class operator TComplex64.Negative(const Value: TComplex64): TComplex64;
begin
  Result.Re := -Value.Re;
  Result.Im := Value.Im;
end;

class operator TComplex64.NotEqual(const Lhs, Rhs: TComplex64): Boolean;
begin
  Result := (Lhs.Re <> Rhs.Re) or (Lhs.Im <> Rhs.Im);
end;

class operator TComplex64.Subtract(const Lhs, Rhs: TComplex64): TComplex64;
begin
  Result := ComplexSubtract64(Lhs, Rhs);
end;

class function TComplex64.Zero: TComplex64;
begin
  Result.Re := 0;
  Result.Im := 0;
end;

class function TComplex64.Euler(const Value: Single): TComplex64;
begin
  GetSinCos(Value, Result.Im, Result.Re);
end;

class function TComplex64.Euler(const Value: Double): TComplex64;
begin
  GetSinCos(Value, Result.Im, Result.Re);
end;

{$IFNDEF FPC}

class function TComplex64.Euler(const Value: Extended): TComplex64;
begin
  GetSinCos(Value, Result.Im, Result.Re);
end;
{$ENDIF}

function TComplex64.Magnitude: Double;
begin
  Result := Hypot(Re, Im);
end;

procedure TComplex64.ComputeEuler(const Value: Single);
begin
  GetSinCos(Value, Self.Im, Self.Re);
end;

procedure TComplex64.ComputeEuler(const Value: Double);
begin
  GetSinCos(Value, Self.Im, Self.Re);
end;

{$IFNDEF FPC}

procedure TComplex64.ComputeEuler(const Value: Extended);
begin
  GetSinCos(Value, Self.Im, Self.Re);
end;
{$ENDIF}
{$ENDIF}

end.
