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

unit DAV_FixedPoint;

interface

{$I DAV_Compiler.inc}
{$IFDEF FPC}
{$UNDEF SUPPORTS_ENHANCED_RECORDS}
{$ENDIF}
{$IFDEF FPC}
{$UNDEF SUPPORTS_ENHANCED_RECORDS}
{$ENDIF}

uses
  Types, Math;

type
  PFixed8Dot24 = ^TFixed8Dot24;

  TFixed8Dot24 = packed record
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
  public
{$IFNDEF FPC}
    constructor Create(const Fixed: Integer); overload;
    constructor Create(const FracLow: Word; FracHigh: Byte;
      Int: SmallInt); overload;
{$ENDIF}
    // operator overloads
    class operator Equal(const Lhs, Rhs: TFixed8Dot24): Boolean;
    class operator NotEqual(const Lhs, Rhs: TFixed8Dot24): Boolean;
    class operator LessThan(const Lhs, Rhs: TFixed8Dot24): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TFixed8Dot24): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TFixed8Dot24): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TFixed8Dot24): Boolean;
    class operator Negative(const Value: TFixed8Dot24): TFixed8Dot24;
    class operator Positive(const Value: TFixed8Dot24): TFixed8Dot24;
    class operator Add(const Lhs, Rhs: TFixed8Dot24): TFixed8Dot24;
    class operator Add(const Lhs: TFixed8Dot24; Rhs: Single): TFixed8Dot24;
    class operator Add(const Lhs: TFixed8Dot24; Rhs: Integer): TFixed8Dot24;
    class operator Subtract(const Lhs, Rhs: TFixed8Dot24): TFixed8Dot24;
    class operator Subtract(const Lhs: TFixed8Dot24; Rhs: Single): TFixed8Dot24;
    class operator Subtract(const Lhs: TFixed8Dot24; Rhs: Integer)
      : TFixed8Dot24;
    class operator Multiply(const Lhs, Rhs: TFixed8Dot24): TFixed8Dot24;
    class operator Divide(const Lhs, Rhs: TFixed8Dot24): TFixed8Dot24;
    class operator Divide(const Lhs: TFixed8Dot24; Rhs: Integer): TFixed8Dot24;
{$IFNDEF FPC}
    class operator Round(const Value: TFixed8Dot24): Integer;
{$ENDIF}
    class operator LeftShift(const Value: TFixed8Dot24; Shift: Byte): TFixed8Dot24;
    class operator RightShift(const Value: TFixed8Dot24; Shift: Byte): TFixed8Dot24;

    class function Zero: TFixed8Dot24; inline; static;
    class function One: TFixed8Dot24; inline; static;
    class function Two: TFixed8Dot24; inline; static;
    class function Half: TFixed8Dot24; inline; static;

    class function ArcTan2(const A, B: TFixed8Dot24): TFixed8Dot24;
      overload; static;

    class operator Implicit(const Value: Single): TFixed8Dot24;
    class operator Implicit(const Value: Double): TFixed8Dot24;
    class operator Implicit(const Value: Integer): TFixed8Dot24;
    class operator Explicit(const Value: TFixed8Dot24): Single;
    class operator Explicit(const Value: TFixed8Dot24): Double;

    function AsSingle: Single;

    // procedure Sin; overload;
{$ENDIF}
    case Integer of
      0:
        (Fixed: Integer);
      1:
        (FracLow: Word; FracHigh: Byte; Int: ShortInt);
  end;

  PFixed8Dot24Array = ^TFixed8Dot24Array;
  TFixed8Dot24Array = array [0 .. 0] of TFixed8Dot24;
  PArrayOfFixed8Dot24 = ^TArrayOfFixed8Dot24;
  TArrayOfFixed8Dot24 = array of TFixed8Dot24;
  PArrayOfArrayOfFixed8Dot24 = ^TArrayOfArrayOfFixed8Dot24;
  TArrayOfArrayOfFixed8Dot24 = array of TArrayOfFixed8Dot24;

  PFixed16Dot16 = ^TFixed16Dot16;

  TFixed16Dot16 = packed record
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
  public
{$IFNDEF FPC}
    constructor Create(const Fixed: Integer); overload;
    constructor Create(const Frac: Word; Int: SmallInt); overload;
{$ENDIF}
    // operator overloads
    class operator Equal(const Lhs, Rhs: TFixed16Dot16): Boolean;
    class operator NotEqual(const Lhs, Rhs: TFixed16Dot16): Boolean;
    class operator LessThan(const Lhs, Rhs: TFixed16Dot16): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TFixed16Dot16): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TFixed16Dot16): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TFixed16Dot16): Boolean;
    class operator Negative(const Value: TFixed16Dot16): TFixed16Dot16;
    class operator Positive(const Value: TFixed16Dot16): TFixed16Dot16;
    class operator Add(const Lhs, Rhs: TFixed16Dot16): TFixed16Dot16;
    class operator Add(const Lhs: TFixed16Dot16; Rhs: Single): TFixed16Dot16;
    class operator Add(const Lhs: TFixed16Dot16; Rhs: Integer): TFixed16Dot16;
    class operator Subtract(const Lhs, Rhs: TFixed16Dot16): TFixed16Dot16;
    class operator Subtract(const Lhs: TFixed16Dot16; Rhs: Single): TFixed16Dot16;
    class operator Subtract(const Lhs: TFixed16Dot16; Rhs: Integer): TFixed16Dot16;
    class operator Multiply(const Lhs, Rhs: TFixed16Dot16): TFixed16Dot16;
    class operator Divide(const Lhs, Rhs: TFixed16Dot16): TFixed16Dot16;
    class operator Divide(const Lhs: TFixed16Dot16; Rhs: Integer): TFixed16Dot16;
{$IFNDEF FPC}
    class operator Round(const Value: TFixed16Dot16): Integer;
{$ENDIF}
    class operator LeftShift(const Value: TFixed16Dot16; Shift: Byte): TFixed16Dot16;
    class operator RightShift(const Value: TFixed16Dot16; Shift: Byte): TFixed16Dot16;

    class function Zero: TFixed16Dot16; inline; static;
    class function One: TFixed16Dot16; inline; static;
    class function Two: TFixed16Dot16; inline; static;
    class function Half: TFixed16Dot16; inline; static;

    class function ArcTan2(const A, B: TFixed16Dot16): TFixed16Dot16;
      overload; static;

    class operator Implicit(const Value: Single): TFixed16Dot16;
    class operator Implicit(const Value: Double): TFixed16Dot16;
    class operator Implicit(const Value: Integer): TFixed16Dot16;
    class operator Explicit(const Value: TFixed16Dot16): Single;
    class operator Explicit(const Value: TFixed16Dot16): Double;

    function AsSingle: Single;
    // procedure Sin; overload;
{$ENDIF}
    case Integer of
      0:
        (Fixed: Integer);
      1:
        (Frac: Word; Int: SmallInt);
  end;

  PFixed16Dot16Array = ^TFixed16Dot16Array;
  TFixed16Dot16Array = array [0 .. 0] of TFixed16Dot16;
  PArrayOfFixed16Dot16 = ^TArrayOfFixed16Dot16;
  TArrayOfFixed16Dot16 = array of TFixed16Dot16;
  PArrayOfArrayOfFixed16Dot16 = ^TArrayOfArrayOfFixed16Dot16;
  TArrayOfArrayOfFixed16Dot16 = array of TArrayOfFixed16Dot16;

  PFixed24Dot8 = ^TFixed24Dot8;

  TFixed24Dot8 = packed record
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
  public
{$IFNDEF FPC}
    constructor Create(const Fixed: Integer); overload;
    constructor Create(const Frac: Byte; Int: Integer); overload;
    constructor Create(const Frac: Byte; Low: Byte; High: SmallInt); overload;
{$ENDIF}
    // operator overloads
    class operator Equal(const Lhs, Rhs: TFixed24Dot8): Boolean;
    class operator NotEqual(const Lhs, Rhs: TFixed24Dot8): Boolean;
    class operator LessThan(const Lhs, Rhs: TFixed24Dot8): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TFixed24Dot8): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TFixed24Dot8): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TFixed24Dot8): Boolean;
    class operator Negative(const Value: TFixed24Dot8): TFixed24Dot8;
    class operator Positive(const Value: TFixed24Dot8): TFixed24Dot8;
    class operator Add(const Lhs, Rhs: TFixed24Dot8): TFixed24Dot8;
    class operator Add(const Lhs: TFixed24Dot8; Rhs: Single): TFixed24Dot8;
    class operator Add(const Lhs: TFixed24Dot8; Rhs: Integer): TFixed24Dot8;
    class operator Subtract(const Lhs, Rhs: TFixed24Dot8): TFixed24Dot8;
    class operator Subtract(const Lhs: TFixed24Dot8; Rhs: Single): TFixed24Dot8;
    class operator Subtract(const Lhs: TFixed24Dot8; Rhs: Integer): TFixed24Dot8;
    class operator Multiply(const Lhs, Rhs: TFixed24Dot8): TFixed24Dot8;
    class operator Divide(const Lhs, Rhs: TFixed24Dot8): TFixed24Dot8;
    class operator Divide(const Lhs: TFixed24Dot8; Rhs: Integer): TFixed24Dot8;
{$IFNDEF FPC}
    class operator Round(const Value: TFixed24Dot8): Integer;
{$ENDIF}
    class operator LeftShift(const Value: TFixed24Dot8; Shift: Byte): TFixed24Dot8;
    class operator RightShift(const Value: TFixed24Dot8; Shift: Byte): TFixed24Dot8;

    class function Zero: TFixed24Dot8; inline; static;
    class function One: TFixed24Dot8; inline; static;
    class function Two: TFixed24Dot8; inline; static;
    class function Half: TFixed24Dot8; inline; static;

    class function ArcTan2(const A, B: TFixed24Dot8): TFixed24Dot8; overload; static;

    class operator Implicit(const Value: Single): TFixed24Dot8;
    class operator Implicit(const Value: Double): TFixed24Dot8;
    class operator Implicit(const Value: Integer): TFixed24Dot8;
    class operator Explicit(const Value: TFixed24Dot8): Single;
    class operator Explicit(const Value: TFixed24Dot8): Double;

    function AsSingle: Single;
    // procedure Sin; overload;
{$ENDIF}
    case Integer of
      0:
        (Fixed: Integer);
      1:
        (Frac: Byte; Low: Byte; High: SmallInt);
  end;

  PFixed24Dot8Array = ^TFixed24Dot8Array;
  TFixed24Dot8Array = array [0 .. 0] of TFixed24Dot8;
  PArrayOfFixed24Dot8 = ^TArrayOfFixed24Dot8;
  TArrayOfFixed24Dot8 = array of TFixed24Dot8;
  PArrayOfArrayOfFixed24Dot8 = ^TArrayOfArrayOfFixed24Dot8;
  TArrayOfArrayOfFixed24Dot8 = array of TArrayOfFixed24Dot8;

function ConvertToFixed8Dot24(Value: Single): TFixed8Dot24; overload;
function ConvertToFixed8Dot24(Value: Double): TFixed8Dot24; overload;
function ConvertToFixed8Dot24(Value: Integer): TFixed8Dot24; overload;
function ConvertToFixed8Dot24(Value: TFixed16Dot16): TFixed8Dot24; overload;
function ConvertToFixed8Dot24(Value: TFixed24Dot8): TFixed8Dot24; overload;
function ConvertToFixed16Dot16(Value: Single): TFixed16Dot16; overload;
function ConvertToFixed16Dot16(Value: Double): TFixed16Dot16; overload;
function ConvertToFixed16Dot16(Value: Integer): TFixed16Dot16; overload;
function ConvertToFixed16Dot16(Value: TFixed8Dot24): TFixed16Dot16; overload;
function ConvertToFixed16Dot16(Value: TFixed24Dot8): TFixed16Dot16; overload;
function ConvertToFixed24Dot8(Value: Single): TFixed24Dot8; overload;
function ConvertToFixed24Dot8(Value: Double): TFixed24Dot8; overload;
function ConvertToFixed24Dot8(Value: Integer): TFixed24Dot8; overload;
function ConvertToFixed24Dot8(Value: TFixed8Dot24): TFixed24Dot8; overload;
function ConvertToFixed24Dot8(Value: TFixed16Dot16): TFixed24Dot8; overload;
function ConvertFromFixed8Dot24(Value: TFixed8Dot24): Single; overload;
function ConvertFromFixed16Dot16(Value: TFixed16Dot16): Single; overload;
function ConvertFromFixed24Dot8(Value: TFixed24Dot8): Single; overload;
function ConvertFromFixed8Dot24ToInteger(Value: TFixed8Dot24): Integer; overload;
function ConvertFromFixed16Dot16ToInteger(Value: TFixed16Dot16): Integer; overload;
function ConvertFromFixed24Dot8ToInteger(Value: TFixed24Dot8): Integer; overload;

procedure ConvertSingleDataToFixed8Dot24Data(Data: Pointer; Count: Integer);
procedure ConvertFixed8Dot24DataFromSingleData(Data: Pointer; Count: Integer);
procedure ConvertSingleDataToFixed16Dot16Data(Data: Pointer; Count: Integer);
procedure ConvertFixed16Dot16DataFromSingleData(Data: Pointer; Count: Integer);
procedure ConvertSingleDataToFixed24Dot8Data(Data: Pointer; Count: Integer);
procedure ConvertFixed24Dot8DataFromSingleData(Data: Pointer; Count: Integer);

function FixedAbs(Value: TFixed8Dot24): TFixed8Dot24; overload;
function FixedAbs(Value: TFixed16Dot16): TFixed16Dot16; overload;
function FixedAbs(Value: TFixed24Dot8): TFixed24Dot8; overload;
function FixedSign(Value: TFixed8Dot24): TValueSign; overload;
function FixedSign(Value: TFixed16Dot16): TValueSign; overload;
function FixedSign(Value: TFixed24Dot8): TValueSign; overload;
function FixedFloor(Value: TFixed8Dot24): Integer; overload;
function FixedFloor(Value: TFixed16Dot16): Integer; overload;
function FixedFloor(Value: TFixed24Dot8): Integer; overload;
function FixedCeil(Value: TFixed8Dot24): Integer; overload;
function FixedCeil(Value: TFixed16Dot16): Integer; overload;
function FixedCeil(Value: TFixed24Dot8): Integer; overload;
function FixedRound(Value: TFixed8Dot24): Integer; overload;
function FixedRound(Value: TFixed16Dot16): Integer; overload;
function FixedRound(Value: TFixed24Dot8): Integer; overload;
function FixedRoundHalfUp(Value: TFixed8Dot24): Integer; overload;
function FixedRoundHalfUp(Value: TFixed16Dot16): Integer; overload;
function FixedRoundHalfUp(Value: TFixed24Dot8): Integer; overload;
function FixedAdd(A, B: TFixed8Dot24): TFixed8Dot24; overload;
function FixedAdd(A, B: TFixed16Dot16): TFixed16Dot16; overload;
function FixedAdd(A, B: TFixed24Dot8): TFixed24Dot8; overload;
function FixedSub(A, B: TFixed8Dot24): TFixed8Dot24; overload;
function FixedSub(A, B: TFixed16Dot16): TFixed16Dot16; overload;
function FixedSub(A, B: TFixed24Dot8): TFixed24Dot8; overload;
function FixedMul(A, B: TFixed8Dot24): TFixed8Dot24; overload;
function FixedMul(A, B: TFixed16Dot16): TFixed16Dot16; overload;
function FixedMul(A, B: TFixed24Dot8): TFixed24Dot8; overload;
function FixedMul(A: TFixed8Dot24; B: Integer): TFixed8Dot24; overload;
function FixedMul(A: TFixed16Dot16; B: Integer): TFixed16Dot16; overload;
function FixedMul(A: TFixed24Dot8; B: Integer): TFixed24Dot8; overload;
function Fixed8Dot24Mul(A, B: Integer): Integer; overload;
function Fixed16Dot16Mul(A, B: Integer): Integer; overload;
function Fixed24Dot8Mul(A, B: Integer): Integer; overload;
function FixedDiv(A, B: TFixed8Dot24): TFixed8Dot24; overload;
function FixedDiv(A, B: TFixed16Dot16): TFixed16Dot16; overload;
function FixedDiv(A, B: TFixed24Dot8): TFixed24Dot8; overload;
function FixedDiv(A: TFixed8Dot24; B: Integer): TFixed8Dot24; overload;
function FixedDiv(A: TFixed16Dot16; B: Integer): TFixed16Dot16; overload;
function FixedDiv(A: TFixed24Dot8; B: Integer): TFixed24Dot8; overload;
function FixedDivTo8Dot24(A, B: TFixed24Dot8): TFixed8Dot24;
function FixedDivTo16Dot16(A, B: TFixed24Dot8): TFixed16Dot16;
function FixedReciprocal(Value: TFixed8Dot24): TFixed8Dot24; overload;
function FixedReciprocal(Value: TFixed16Dot16): TFixed16Dot16; overload;
function FixedReciprocal(Value: TFixed24Dot8): TFixed24Dot8; overload;
function FixedSqr(Value: TFixed8Dot24): TFixed8Dot24; overload;
function FixedSqr(Value: TFixed16Dot16): TFixed16Dot16; overload;
function FixedSqr(Value: TFixed24Dot8): TFixed24Dot8; overload;
// function FixedSqrtLowResolution(Value: TFixed8Dot24): TFixed8Dot24; overload;
// function FixedSqrtHighResolution(Value: TFixed8Dot24): TFixed8Dot24; overload;
function FixedSqrtLowResolution(Value: TFixed16Dot16): TFixed16Dot16; overload;
function FixedSqrtHighResolution(Value: TFixed16Dot16): TFixed16Dot16; overload;
function FixedSqrt(Value: TFixed24Dot8): TFixed24Dot8; overload;
function FixedMin(A, B: TFixed8Dot24): TFixed8Dot24; overload;
function FixedMin(A, B: TFixed16Dot16): TFixed16Dot16; overload;
function FixedMin(A, B: TFixed24Dot8): TFixed24Dot8; overload;
function FixedMax(A, B: TFixed8Dot24): TFixed8Dot24; overload;
function FixedMax(A, B: TFixed16Dot16): TFixed16Dot16; overload;
function FixedMax(A, B: TFixed24Dot8): TFixed24Dot8; overload;

procedure FixedSinCos(Value: TFixed8Dot24; out Sin, Cos: TFixed8Dot24);
  overload;
procedure FixedSinCos(Value: TFixed16Dot16;
  out Sin, Cos: TFixed16Dot16); overload;
procedure FixedSinCos(Value: TFixed24Dot8; out Sin, Cos: TFixed24Dot8);
  overload;
function FixedArcTan2(A, B: TFixed8Dot24): TFixed8Dot24; overload;
function FixedArcTan2(A, B: TFixed16Dot16): TFixed16Dot16; overload;
function FixedArcTan2(A, B: TFixed24Dot8): TFixed24Dot8; overload;

const
  CFixed8Dot24Two: TFixed8Dot24 = (Fixed: $2000000);
  CFixed8Dot24One: TFixed8Dot24 = (Fixed: $1000000);
  CFixed8Dot24Half: TFixed8Dot24 = (Fixed: $800000);
  CFixed8Dot24Zero: TFixed8Dot24 = (Fixed: $0);
  CFixed8Dot24ToFloat = 1 / $1000000;
  CFixed8Dot24Pi: TFixed8Dot24 = (Fixed: Round(PI * $1000000));
  CFixed8Dot24TwoPi: TFixed8Dot24 = (Fixed: Round(PI * $2000000));

  CFixed16Dot16Two: TFixed16Dot16 = (Fixed: $20000);
  CFixed16Dot16One: TFixed16Dot16 = (Fixed: $10000);
  CFixed16Dot16Half: TFixed16Dot16 = (Fixed: $8000);
  CFixed16Dot16Zero: TFixed16Dot16 = (Fixed: $0);
  CFixed16Dot16ToFloat = 1 / $10000;
  CFixed16Dot16Pi: TFixed16Dot16 = (Fixed: Round(PI * $10000));
  CFixed16Dot16TwoPi: TFixed16Dot16 = (Fixed: Round(PI * $20000));

  CFixed24Dot8Two: TFixed24Dot8 = (Fixed: $200);
  CFixed24Dot8One: TFixed24Dot8 = (Fixed: $100);
  CFixed24Dot8Half: TFixed24Dot8 = (Fixed: $80);
  CFixed24Dot8Zero: TFixed24Dot8 = (Fixed: $0);
  CFixed24Dot8ToFloat = 1 / $100;
  CFixed24Dot8Pi: TFixed24Dot8 = (Fixed: Round(PI * $100));
  CFixed24Dot8TwoPi: TFixed24Dot8 = (Fixed: Round(PI * $200));

implementation

uses
  DAV_Math;

function ConvertToFixed8Dot24(Value: Single): TFixed8Dot24; overload;
begin
  Result.Fixed := Round(Value * CFixed8Dot24One.Fixed);
end;

function ConvertToFixed8Dot24(Value: Double): TFixed8Dot24; overload;
begin
  Result.Fixed := Round(Value * CFixed8Dot24One.Fixed);
end;

function ConvertToFixed8Dot24(Value: Integer): TFixed8Dot24; overload;
begin
  Result.Fixed := Value shl 24;
end;

function ConvertToFixed16Dot16(Value: Single): TFixed16Dot16;
begin
  Result.Fixed := Round(Value * CFixed16Dot16One.Fixed);
end;

function ConvertToFixed16Dot16(Value: Double): TFixed16Dot16;
begin
  Result.Fixed := Round(Value * CFixed16Dot16One.Fixed);
end;

function ConvertToFixed16Dot16(Value: Integer): TFixed16Dot16;
begin
  Result.Fixed := Value shl 16;
end;

function ConvertToFixed24Dot8(Value: Single): TFixed24Dot8;
begin
  Result.Fixed := Round(Value * CFixed24Dot8One.Fixed);
end;

function ConvertToFixed24Dot8(Value: Double): TFixed24Dot8;
begin
  Result.Fixed := Round(Value * CFixed24Dot8One.Fixed);
end;

function ConvertToFixed24Dot8(Value: Integer): TFixed24Dot8;
begin
  Result.Fixed := Value shl 8;
end;

function ConvertToFixed8Dot24(Value: TFixed16Dot16): TFixed8Dot24;
begin
  Result.Fixed := Value.Fixed shl 8;
end;

function ConvertToFixed8Dot24(Value: TFixed24Dot8): TFixed8Dot24;
begin
  Result.Fixed := Value.Fixed shl 16;
end;

function ConvertToFixed16Dot16(Value: TFixed8Dot24): TFixed16Dot16;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := Integer(Cardinal(Cardinal(Cardinal(Value) shr 8) or
    (Cardinal(Integer(Cardinal(0 - Cardinal(Cardinal(Value) shr 31))))
    shl 24)));

  // Result.Fixed := (Value.Fixed and $80000000) or (Value.Fixed shr 8);
{$ELSE}
asm
  SAR     Value, 8
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function ConvertToFixed16Dot16(Value: TFixed24Dot8): TFixed16Dot16;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := Value.Fixed shl 8;
{$ELSE}
asm
  SHL     Value, 8
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function ConvertToFixed24Dot8(Value: TFixed8Dot24): TFixed24Dot8;
{$IFDEF PUREPASCAL}
begin
{$IFDEF FPC}
  Result.Fixed := SarLongInt(Value.Fixed, 16);
{$ELSE}
  Result.Fixed := Integer(Cardinal(Cardinal(Cardinal(Value) shr 16) or
    (Cardinal(Integer(Cardinal(0 - Cardinal(Cardinal(Value) shr 31))))
    shl 16)));
{$ENDIF}
  // Result.Fixed := (Value.Fixed and $80000000) or (Value.Fixed shr 16);
{$ELSE}
asm
  SAR     Value, 16
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function ConvertToFixed24Dot8(Value: TFixed16Dot16): TFixed24Dot8;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := Integer(Cardinal(Cardinal(Cardinal(Value.Fixed) shr 8) or
    (Cardinal(Integer(Cardinal(0 - Cardinal(Cardinal(Value.Fixed) shr 31))))
    shl 24)));

  // Result.Fixed := (Value.Fixed and $80000000) or (Value.Fixed shr 8);
{$ELSE}
asm
  SAR     Value, 8
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function ConvertFromFixed8Dot24(Value: TFixed8Dot24): Single;
begin
  Result := Value.Fixed * CFixed8Dot24ToFloat;
end;

function ConvertFromFixed16Dot16(Value: TFixed16Dot16): Single;
begin
  Result := Value.Fixed * CFixed16Dot16ToFloat;
end;

function ConvertFromFixed24Dot8(Value: TFixed24Dot8): Single;
begin
  Result := Value.Fixed * CFixed24Dot8ToFloat;
end;

function ConvertFromFixed8Dot24ToInteger(Value: TFixed8Dot24): Integer;
begin
  Result := Round(Value.Fixed * CFixed8Dot24ToFloat);
end;

function ConvertFromFixed16Dot16ToInteger(Value: TFixed16Dot16): Integer;
begin
  Result := Round(Value.Fixed * CFixed16Dot16ToFloat);
end;

function ConvertFromFixed24Dot8ToInteger(Value: TFixed24Dot8): Integer;
begin
  Result := Round(Value.Fixed * CFixed24Dot8ToFloat);
end;

function FixedAbs(Value: TFixed8Dot24): TFixed8Dot24;
begin
  Result.Fixed := Abs(Value.Fixed);
end;

function FixedAbs(Value: TFixed16Dot16): TFixed16Dot16;
begin
  Result.Fixed := Abs(Value.Fixed);
end;

function FixedAbs(Value: TFixed24Dot8): TFixed24Dot8;
begin
  Result.Fixed := Abs(Value.Fixed);
end;

function FixedSign(Value: TFixed8Dot24): TValueSign;
begin
  Result := Sign(Value.Fixed);
end;

function FixedSign(Value: TFixed16Dot16): TValueSign;
begin
  Result := Sign(Value.Fixed);
end;

function FixedSign(Value: TFixed24Dot8): TValueSign;
begin
  Result := Sign(Value.Fixed);
end;

function FixedFloor(Value: TFixed8Dot24): Integer;
{$IFDEF PUREPASCAL}
var
  Mask: Cardinal;
begin
  Result := Integer(Cardinal(Cardinal(Cardinal(Value.Fixed) shr 24) or
    (Cardinal(Integer(Cardinal(0 - Cardinal(Cardinal(Value.Fixed) shr 31)))
    ) shl 8)));
{$ELSE}
asm
  SAR     Value, 24;
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function FixedFloor(Value: TFixed16Dot16): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Integer(Cardinal(Cardinal(Cardinal(Value) shr 16) or
    (Cardinal(Integer(Cardinal(0 - Cardinal(Cardinal(Value) shr 31))))
    shl (16))));
{$ELSE}
asm
  SAR     Value, 16
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function FixedFloor(Value: TFixed24Dot8): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Integer(Cardinal(Cardinal(Cardinal(Value) shr 8) or
    (Cardinal(Integer(Cardinal(0 - Cardinal(Cardinal(Value) shr 31))))
    shl 24)));

  // Result := (Value.Fixed and $80000000) or (Value.Fixed shr 8);
{$ELSE}
asm
  SAR     Value, 8
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function FixedCeil(Value: TFixed8Dot24): Integer;
{$IFDEF PUREPASCAL}
begin
  Value.Fixed := Value.Fixed + $FFFFFF;

  Result := Integer(Cardinal(Cardinal(Value.Fixed shr 24) or
    (Cardinal(-(Value.Fixed shr 31)) shl 8)));
{$ELSE}
asm
  ADD     Value, $FFFFFF
  SAR     Value, 24
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function FixedCeil(Value: TFixed16Dot16): Integer;
{$IFDEF PUREPASCAL}
begin
  Value.Fixed := Value.Fixed + $FFFF;
  Result := Integer(Cardinal(Cardinal(Cardinal(Value.Fixed) shr 16) or
    (Cardinal(Integer(Cardinal(0 - Cardinal(Cardinal(Value.Fixed) shr 31))))
    shl 16)));

  // Result := (Value.Fixed and $80000000) or ((Value.Fixed + $FFFF) shr 16);
{$ELSE}
asm
  ADD     Value, $FFFF
  SAR     Value, 16
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function FixedCeil(Value: TFixed24Dot8): Integer;
{$IFDEF PUREPASCAL}
begin
  Value.Fixed := Value.Fixed + $FF;

  Result := Integer(Cardinal(Cardinal(Cardinal(Value) shr 8) or
    (Cardinal(Integer(Cardinal(0 - Cardinal(Cardinal(Value) shr 31))))
    shl 24)));

  // Result := (Value.Fixed and $80000000) or ((Value.Fixed + $FF) shr 8);
{$ELSE}
asm
  ADD     Value, $FF
  SAR     Value, 8
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function FixedRound(Value: TFixed8Dot24): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := FixedRoundHalfUp(Value) -
    ((((Value.Fixed + $17FFFFF) and $1FFFFFF) + 1) shr 25);
{$ELSE}
asm
  MOV     EDX, Value
  ADD     Value, $800000
  SAR     Value, 24
  ADD     EDX, $17FFFFF
  AND     EDX, $1FFFFFF
  ADD     EDX, 1
  SHR     EDX, $19
  SUB     Value, EDX
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function FixedRound(Value: TFixed16Dot16): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := FixedRoundHalfUp(Value) -
    ((((Value.Fixed + $17FFF) and $1FFFF) + 1) shr 17);
{$ELSE}
asm
  MOV     EDX, Value
  ADD     Value, $8000
  SAR     Value, 16
  ADD     EDX, $17FFF
  AND     EDX, $1FFFF
  ADD     EDX, 1
  SHR     EDX, $11
  SUB     Value, EDX
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function FixedRound(Value: TFixed24Dot8): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := FixedRoundHalfUp(Value) -
    ((((Value.Fixed + $17F) and $1FF) + 1) shr 9);
{$ELSE}
asm
  MOV     EDX, Value
  ADD     Value, $80
  SAR     Value, 8
  ADD     EDX, $17F
  AND     EDX, $1FF
  ADD     EDX, 1
  SHR     EDX, $9
  SUB     Value, EDX
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function FixedAdd(A, B: TFixed8Dot24): TFixed8Dot24;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed + B.Fixed;
{$ELSE}
asm
  ADD     A, B
  {$IFDEF CPUx86_64}
  MOV     Result, A
  {$ENDIF}
  {$ENDIF}
end;

function FixedRoundHalfUp(Value: TFixed8Dot24): Integer;
{$IFDEF PUREPASCAL}
begin
  Value.Fixed := Value.Fixed + $800000;
  Result := Integer((((Value.Fixed) shr 24) or -((Value.Fixed) shr 31) shl 8));
{$ELSE}
asm
  ADD     Value, $800000
  SAR     Value, 24
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function FixedRoundHalfUp(Value: TFixed16Dot16): Integer;
{$IFDEF PUREPASCAL}
begin
  Value.Fixed := Value.Fixed + $8000;
  Result := Integer((((Value.Fixed) shr 16) or -((Value.Fixed) shr 31) shl 16));
{$ELSE}
asm
  ADD     Value, $8000
  SAR     Value, 16
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function FixedRoundHalfUp(Value: TFixed24Dot8): Integer;
{$IFDEF PUREPASCAL}
begin
  Value.Fixed := Value.Fixed + $80;
  Result := Integer((((Value.Fixed) shr 8) or -((Value.Fixed) shr 31) shl 24));
{$ELSE}
asm
  ADD     Value, $80
  SAR     Value, 8
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
  {$ENDIF}
end;

function FixedAdd(A, B: TFixed16Dot16): TFixed16Dot16;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed + B.Fixed;
{$ELSE}
asm
  ADD     A, B
  {$IFDEF CPUx86_64}
  MOV     Result, A
  {$ENDIF}
  {$ENDIF}
end;

function FixedAdd(A, B: TFixed24Dot8): TFixed24Dot8;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed + B.Fixed;
{$ELSE}
asm
  ADD     A, B
  {$IFDEF CPUx86_64}
  MOV     Result, A
  {$ENDIF}
  {$ENDIF}
end;

function FixedSub(A, B: TFixed8Dot24): TFixed8Dot24;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed - B.Fixed;
{$ELSE}
asm
  SUB     A, B
  {$IFDEF CPUx86_64}
  MOV     Result, A
  {$ENDIF}
  {$ENDIF}
end;

function FixedSub(A, B: TFixed16Dot16): TFixed16Dot16;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed - B.Fixed;
{$ELSE}
asm
  SUB     A, B
  {$IFDEF CPUx86_64}
  MOV     Result, A
  {$ENDIF}
  {$ENDIF}
end;

function FixedSub(A, B: TFixed24Dot8): TFixed24Dot8;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed - B.Fixed;
{$ELSE}
asm
  SUB     A, B
  {$IFDEF CPUx86_64}
  MOV     Result, A
  {$ENDIF}
  {$ENDIF}
end;

function FixedMul(A, B: TFixed8Dot24): TFixed8Dot24;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed * CFixed8Dot24ToFloat * B.Fixed);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     EAX, A
  IMUL    B
  SHRD    EAX, EDX, 24
  {$ELSE}
  IMUL    B
  SHRD    A, B, 24
  {$ENDIF}
  {$ENDIF}
end;

function FixedMul(A, B: TFixed16Dot16): TFixed16Dot16;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed * CFixed16Dot16ToFloat * B.Fixed);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     EAX, A
  IMUL    B
  SHRD    EAX, EDX, 16
  {$ELSE}
  IMUL    B
  SHRD    A, B, 16
  {$ENDIF}
  {$ENDIF}
end;

function FixedMul(A, B: TFixed24Dot8): TFixed24Dot8;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed * CFixed24Dot8ToFloat * B.Fixed);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  IMUL    RDX
  SHRD    RAX, RDX, 8
  {$ELSE}
  IMUL    B
  SHRD    A, B, 8
  {$ENDIF}
  {$ENDIF}
end;

function FixedMul(A: TFixed8Dot24; B: Integer): TFixed8Dot24;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed * CFixed8Dot24ToFloat * B);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  IMUL    RDX
  SHRD    RAX, RDX, 24
  {$ELSE}
  IMUL    B
  SHRD    A, B, 24
  {$ENDIF}
  {$ENDIF}
end;

function FixedMul(A: TFixed16Dot16; B: Integer): TFixed16Dot16;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed * CFixed16Dot16ToFloat * B);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  IMUL    RDX
  SHRD    RAX, RDX, 16
  {$ELSE}
  IMUL    B
  SHRD    A, B, 16
  {$ENDIF}
  {$ENDIF}
end;

function FixedMul(A: TFixed24Dot8; B: Integer): TFixed24Dot8;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed * CFixed24Dot8ToFloat * B);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  IMUL    RDX
  SHRD    RAX, RDX, 8
  {$ELSE}
  IMUL    B
  SHRD    A, B, 8
  {$ENDIF}
  {$ENDIF}
end;

function Fixed8Dot24Mul(A, B: Integer): Integer;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A * CFixed8Dot24ToFloat * B);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  IMUL    RDX
  SHRD    RAX, RDX, 24
  {$ELSE}
  IMUL    B
  SHRD    A, B, 24
  {$ENDIF}
  {$ENDIF}
end;

function Fixed16Dot16Mul(A, B: Integer): Integer;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A * CFixed16Dot16ToFloat * B);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  IMUL    RDX
  SHRD    RAX, RDX, 16
  {$ELSE}
  IMUL    B
  SHRD    A, B, 16
  {$ENDIF}
  {$ENDIF}
end;

function Fixed24Dot8Mul(A, B: Integer): Integer;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A * CFixed24Dot8ToFloat * B);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  IMUL    RDX
  SHRD    RAX, RDX, 8
  {$ELSE}
  IMUL    B
  SHRD    A, B, 8
  {$ENDIF}
  {$ENDIF}
end;

function FixedDiv(A, B: TFixed8Dot24): TFixed8Dot24;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed / B.Fixed * CFixed8Dot24One.Fixed);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  MOV     RCX, RDX
  CDQ
  SHLD    RDX, RAX, 24
  SHL     RAX, 24
  IDIV    RDX
  {$ELSE}
  MOV     ECX, B
  CDQ
  SHLD    B, A, 24
  SHL     A, 24
  IDIV    ECX
  {$ENDIF}
  {$ENDIF}
end;

function FixedDiv(A, B: TFixed16Dot16): TFixed16Dot16;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed / B.Fixed * CFixed16Dot16One.Fixed);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  MOV     RCX, RDX
  CDQ
  SHLD    RDX, RAX, 16
  SHL     RAX, 16
  IDIV    RDX
  {$ELSE}
  MOV     ECX, B
  CDQ
  SHLD    B, A, 16
  SHL     A, 16
  IDIV    ECX
  {$ENDIF}
  {$ENDIF}
end;

function FixedDiv(A, B: TFixed24Dot8): TFixed24Dot8;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed / B.Fixed * CFixed24Dot8One.Fixed);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  MOV     RCX, RDX
  CDQ
  SHLD    RDX, RAX, 8
  SHL     RAX, 8
  IDIV    RDX
  {$ELSE}
  MOV     ECX, B
  CDQ
  SHLD    B, A, 8
  SHL     A, 8
  IDIV    ECX
  {$ENDIF}
  {$ENDIF}
end;

function FixedDiv(A: TFixed8Dot24; B: Integer): TFixed8Dot24;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed / B);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  MOV     RCX, RDX
  CDQ
  SHLD    RDX, RAX, 24
  IDIV    RDX
  {$ELSE}
  MOV     ECX, B
  CDQ
  SHLD    B, A, 24
  IDIV    ECX
  {$ENDIF}
  {$ENDIF}
end;

function FixedDiv(A: TFixed16Dot16; B: Integer): TFixed16Dot16;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed / B);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  MOV     RCX, RDX
  CDQ
  SHLD    RDX, RAX, 16
  IDIV    RDX
  {$ELSE}
  MOV     ECX, B
  CDQ
  SHLD    B, A, 16
  IDIV    ECX
  {$ENDIF}
  {$ENDIF}
end;

function FixedDiv(A: TFixed24Dot8; B: Integer): TFixed24Dot8;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed / B);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  MOV     RCX, RDX
  CDQ
  SHLD    RDX, RAX, 8
  IDIV    RDX
  {$ELSE}
  MOV     ECX, B
  CDQ
  SHLD    EDX, EAX, 8
  IDIV    ECX
  {$ENDIF}
  {$ENDIF}
end;

function FixedDivTo8Dot24(A, B: TFixed24Dot8): TFixed8Dot24;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed / B.Fixed * CFixed8Dot24One.Fixed);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  MOV     RCX, RDX
  CDQ
  SHLD    RDX, RAX, 8
  SHL     RAX, 8
  IDIV    RDX
  {$ELSE}
  MOV     ECX, B
  CDQ
  SHLD    B, A, 24
  SHL     A, 24
  IDIV    ECX
  {$ENDIF}
  {$ENDIF}
end;

function FixedDivTo16Dot16(A, B: TFixed24Dot8): TFixed16Dot16;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round(A.Fixed / B.Fixed * CFixed16Dot16One.Fixed);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  MOV     RCX, RDX
  CDQ
  SHLD    RDX, RAX, 8
  SHL     RAX, 8
  IDIV    RDX
  {$ELSE}
  MOV     ECX, B
  CDQ
  SHLD    B, A, 16
  SHL     A, 16
  IDIV    ECX
  {$ENDIF}
  {$ENDIF}
end;

function FixedReciprocal(Value: TFixed8Dot24): TFixed8Dot24;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
const
  CDividend: Single = 281474976710656;
begin
  IntResult := Round(CDividend / Value.Fixed - 0.5);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  XOR     RAX, RAX
  MOV     RDX, 1
  IDIV    RCX
  SHL     RAX, 16
  {$ELSE}
  MOV     ECX, Value
  XOR     EAX, EAX
  MOV     EDX, 1
  IDIV    ECX
  SHL     EAX, 16
  {$ENDIF}
  {$ENDIF}
end;

function FixedReciprocal(Value: TFixed16Dot16): TFixed16Dot16;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
const
  CDividend: Single = 4294967296; // CFixed16Dot16One * CFixed16Dot16One
begin
  IntResult := Round(CDividend / Value.Fixed - 0.5);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  XOR     RAX, RAX
  MOV     RDX, 1
  IDIV    RCX
  {$ELSE}
  MOV     ECX, Value
  XOR     EAX, EAX
  MOV     EDX, 1
  IDIV    ECX
  {$ENDIF}
  {$ENDIF}
end;

function FixedReciprocal(Value: TFixed24Dot8): TFixed24Dot8;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
const
  CDividend: Single = 65536; // CFixed24Dot24One * CFixed24Dot24One
begin
  IntResult := Round(CDividend / Value.Fixed - 0.5);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  XOR     RAX, RAX
  MOV     RDX, 1
  IDIV    RCX
  SHR     RAX, 16
  {$ELSE}
  MOV     ECX, Value
  XOR     EAX, EAX
  MOV     EDX, 1
  IDIV    ECX
  SHR     EAX, 16
  {$ENDIF}
  {$ENDIF}
end;

function FixedSqr(Value: TFixed8Dot24): TFixed8Dot24;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round((Value.Fixed * CFixed8Dot24ToFloat) * Value.Fixed);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  IMUL    RCX, RCX
  SHRD    RCX, RDX, 24
  MOV     RAX, RCX
  {$ELSE}
  IMUL    Value
  SHRD    EAX, EDX, 24
  {$ENDIF}
  {$ENDIF}
end;

function FixedSqr(Value: TFixed16Dot16): TFixed16Dot16;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round((Value.Fixed * CFixed16Dot16ToFloat) * Value.Fixed);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  IMUL    RCX, RCX
  SHRD    RCX, RDX, 16
  MOV     RAX, RCX
  {$ELSE}
  IMUL    Value
  SHRD    EAX, EDX, 16
  {$ENDIF}
  {$ENDIF}
end;

function FixedSqr(Value: TFixed24Dot8): TFixed24Dot8;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
begin
  IntResult := Round((Value.Fixed * CFixed24Dot8ToFloat) * Value.Fixed);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  IMUL    RCX, RCX
  SHRD    RCX, RDX, 8
  MOV     RAX, RCX
  {$ELSE}
  IMUL    Value
  SHRD    EAX, EDX, 8
  {$ENDIF}
  {$ENDIF}
end;

function FixedSqrtLowResolution(Value: TFixed16Dot16): TFixed16Dot16;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
const
  CFixed16Dot16OneAsSingle: Single = 65536;
begin
  IntResult := Round(Sqrt(Value.Fixed * CFixed16Dot16OneAsSingle) - 0.5);
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  XOR     EAX, EAX
  MOV     R8D, $40000000

@Step1:
  MOV     EDX, ECX
  SUB     EDX, R8D
  JL      @Step2
  SUB     EDX, EAX
  JL      @Step2
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, R8D
  SHR     R8D, 2
  JNZ     @Step1
  JMP     @Step3
@Step2:
  SHR     EAX, 1
  SHR     R8D, 2
  JNZ     @Step1
@Step3:
  SHL     EAX, 8
  {$ELSE}
  PUSH    EBX
  MOV     ECX, Value
  XOR     Value, Value
  MOV     EBX, $40000000
@Step1:
  MOV     EDX, ECX
  SUB     EDX, EBX
  JL      @Step2
  SUB     EDX, Value
  JL      @Step2
  MOV     ECX, EDX
  SHR     Value, 1
  OR      Value, EBX
  SHR     EBX, 2
  JNZ     @Step1
  JMP     @Step3
@Step2:
  SHR     Value, 1
  SHR     EBX, 2
  JNZ     @Step1
@Step3:
  SHL     Value, 8
  POP     EBX
  {$ENDIF}
  {$ENDIF}
end;

function FixedSqrtHighResolution(Value: TFixed16Dot16): TFixed16Dot16;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
const
  CFixed16Dot16OneAsSingle: Single = 65536;
begin
  IntResult := Round(Sqrt(Value.Fixed * CFixed16Dot16OneAsSingle));
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  XOR     EAX, EAX
  MOV     R8D, $40000000

@Step1:
  MOV     EDX, ECX
  SUB     EDX, R8D
  JB      @Step2
  SUB     EDX, EAX
  JB      @Step2
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, R8D
  SHR     R8D, 2
  JNZ     @Step1
  JZ      @Step3

@Step2:
  SHR     EAX, 1
  SHR     R8D, 2
  JNZ     @Step1

@Step3:
  MOV     R8D, $00004000
  SHL     EAX, 16
  SHL     ECX, 16

@Step4:
  MOV     EDX, ECX
  SUB     EDX, R8D
  jb      @Step5
  SUB     EDX, EAX
  jb      @Step5
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, R8D
  SHR     R8D, 2
  JNZ     @Step4
  JMP     @Step6

@Step5:
  SHR     EAX, 1
  SHR     R8D, 2
  JNZ     @Step4

@Step6:
  {$ELSE}
  PUSH    EBX
  MOV     ECX, Value
  XOR     EAX, EAX
  MOV     EBX, $40000000

@Step1:
  MOV     EDX, ECX
  SUB     EDX, EBX
  JB      @Step2
  SUB     EDX, EAX
  JB      @Step2
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, EBX
  SHR     EBX, 2
  JNZ     @Step1
  JZ      @Step3

@Step2:
  SHR     EAX, 1
  SHR     EBX, 2
  JNZ     @Step1

@Step3:
  MOV     EBX, $00004000
  SHL     EAX, 16
  SHL     ECX, 16

@Step4:
  MOV     EDX, ECX
  SUB     EDX, EBX
  jb      @Step5
  SUB     EDX, EAX
  jb      @Step5
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, EBX
  SHR     EBX, 2
  JNZ     @Step4
  JMP     @Step6

@Step5:
  SHR     EAX, 1
  SHR     EBX, 2
  JNZ     @Step4

@Step6:
  POP     EBX
  {$ENDIF}
  {$ENDIF}
end;

function FixedSqrt(Value: TFixed24Dot8): TFixed24Dot8;
{$IFDEF PUREPASCAL}
var
  IntResult: Integer absolute Result;
const
  CFixed24Dot8OneAsSingle: Single = 256;
begin
  IntResult := Round(Sqrt(Value.Fixed * CFixed24Dot8OneAsSingle));
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  XOR     EAX, EAX
  MOV     R8D, $40000000

@Step1:
  MOV     EDX, ECX
  SUB     EDX, R8D
  JB      @Step2
  SUB     EDX, EAX
  JB      @Step2
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, R8D
  SHR     R8D, 2
  JNZ     @Step1
  JZ      @Step3

@Step2:
  SHR     EAX, 1
  SHR     R8D, 2
  JNZ     @Step1

@Step3:
  MOV     R8D, $00004000
  SHL     EAX, 16
  SHL     ECX, 16

@Step4:
  MOV     EDX, ECX
  SUB     EDX, R8D
  jb      @Step5
  SUB     EDX, EAX
  JB      @Step5
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, R8D
  SHR     R8D, 2
  JNZ     @Step4
  JMP     @Step6

@Step5:
  SHR     EAX, 1
  SHR     R8D, 2
  JNZ     @Step4

@Step6:
  SHR     EAX, 4

  {$ELSE}
  PUSH    EBX
  MOV     ECX, EAX
  XOR     EAX, EAX
  MOV     EBX, $40000000

@Step1:
  MOV     EDX, ECX
  SUB     EDX, EBX
  JB      @Step2
  SUB     EDX, EAX
  JB      @Step2
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, EBX
  SHR     EBX, 2
  JNZ     @Step1
  JZ      @Step3

@Step2:
  SHR     EAX, 1
  SHR     EBX, 2
  JNZ     @Step1

@Step3:
  MOV     EBX, $00004000
  SHL     EAX, 16
  SHL     ECX, 16

@Step4:
  MOV     EDX, ECX
  SUB     EDX, EBX
  JB      @Step5
  SUB     EDX, EAX
  jb      @Step5
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, EBX
  SHR     EBX, 2
  JNZ     @Step4
  JMP     @Step6

@Step5:
  SHR     EAX, 1
  SHR     EBX, 2
  JNZ     @Step4

@Step6:
  POP     EBX
  SHR     EAX, 4
  {$ENDIF}
  {$ENDIF}
end;

function FixedMin(A, B: TFixed8Dot24): TFixed8Dot24;
begin
  if A.Fixed < B.Fixed then
    Result := A
  else
    Result := B;
end;

function FixedMin(A, B: TFixed16Dot16): TFixed16Dot16;
begin
  if A.Fixed < B.Fixed then
    Result := A
  else
    Result := B;
end;

function FixedMin(A, B: TFixed24Dot8): TFixed24Dot8;
begin
  if A.Fixed < B.Fixed then
    Result := A
  else
    Result := B;
end;

function FixedMax(A, B: TFixed8Dot24): TFixed8Dot24;
begin
  if A.Fixed > B.Fixed then
    Result := A
  else
    Result := B;
end;

function FixedMax(A, B: TFixed16Dot16): TFixed16Dot16;
begin
  if A.Fixed > B.Fixed then
    Result := A
  else
    Result := B;
end;

function FixedMax(A, B: TFixed24Dot8): TFixed24Dot8;
begin
  if A.Fixed > B.Fixed then
    Result := A
  else
    Result := B;
end;

procedure FixedSinCos(Value: TFixed8Dot24; out Sin, Cos: TFixed8Dot24);
var
  FloatSin: Single;
  FloatCos: Single;
begin
  GetSinCos(ConvertFromFixed8Dot24(Value), FloatSin, FloatCos);
  Sin := ConvertToFixed8Dot24(FloatSin);
  Cos := ConvertToFixed8Dot24(FloatCos);
end;

procedure FixedSinCos(Value: TFixed16Dot16; out Sin, Cos: TFixed16Dot16);
var
  FloatSin: Single;
  FloatCos: Single;
begin
  GetSinCos(ConvertFromFixed16Dot16(Value), FloatSin, FloatCos);
  Sin := ConvertToFixed16Dot16(FloatSin);
  Cos := ConvertToFixed16Dot16(FloatCos);
end;

procedure FixedSinCos(Value: TFixed24Dot8; out Sin, Cos: TFixed24Dot8);
var
  FloatSin: Single;
  FloatCos: Single;
begin
  GetSinCos(ConvertFromFixed24Dot8(Value), FloatSin, FloatCos);
  Sin := ConvertToFixed24Dot8(FloatSin);
  Cos := ConvertToFixed24Dot8(FloatCos);
end;

function FixedArcTan2(A, B: TFixed8Dot24): TFixed8Dot24;
begin
  Result := ConvertToFixed8Dot24(Math.ArcTan2(ConvertFromFixed8Dot24(A),
    ConvertFromFixed8Dot24(B)));
end;

function FixedArcTan2(A, B: TFixed16Dot16): TFixed16Dot16;
begin
  Result := ConvertToFixed16Dot16(Math.ArcTan2(ConvertFromFixed16Dot16(A),
    ConvertFromFixed16Dot16(B)));
end;

function FixedArcTan2(A, B: TFixed24Dot8): TFixed24Dot8;
begin
  Result := ConvertToFixed24Dot8(Math.ArcTan2(ConvertFromFixed24Dot8(A),
    ConvertFromFixed24Dot8(B)));
end;

procedure ConvertSingleDataToFixed8Dot24Data(Data: Pointer; Count: Integer);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Assert((PSingle(Data)^ < 256) and (PSingle(Data)^ > -256));
    PFixed8Dot24(Data)^ := ConvertToFixed8Dot24(PSingle(Data)^);
    Inc(PSingle(Data));
  end;
end;

procedure ConvertFixed8Dot24DataFromSingleData(Data: Pointer; Count: Integer);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    PSingle(Data)^ := ConvertFromFixed8Dot24(PFixed8Dot24(Data)^);
    Inc(PSingle(Data));
  end;
end;

procedure ConvertSingleDataToFixed16Dot16Data(Data: Pointer; Count: Integer);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    PFixed16Dot16(Data)^ := ConvertToFixed16Dot16(PSingle(Data)^);
    Inc(PSingle(Data));
  end;
end;

procedure ConvertFixed16Dot16DataFromSingleData(Data: Pointer; Count: Integer);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    PSingle(Data)^ := ConvertFromFixed16Dot16(PFixed16Dot16(Data)^);
    Inc(PSingle(Data));
  end;
end;

procedure ConvertSingleDataToFixed24Dot8Data(Data: Pointer; Count: Integer);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    PFixed24Dot8(Data)^ := ConvertToFixed24Dot8(PSingle(Data)^);
    Inc(PSingle(Data));
  end;
end;

procedure ConvertFixed24Dot8DataFromSingleData(Data: Pointer; Count: Integer);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    PFixed24Dot8(Data)^ := ConvertToFixed24Dot8(PSingle(Data)^);
    Inc(PSingle(Data));
  end;
end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
{ TFixed8Dot24 }

{$IFNDEF FPC}

constructor TFixed8Dot24.Create(const Fixed: Integer);
begin
  Self.Fixed := Fixed;
end;

constructor TFixed8Dot24.Create(const FracLow: Word; FracHigh: Byte;
  Int: SmallInt);
begin
  Self.FracLow := FracLow;
  Self.FracHigh := FracHigh;
  Self.Int := Int;
end;
{$ENDIF}

class operator TFixed8Dot24.Add(const Lhs, Rhs: TFixed8Dot24): TFixed8Dot24;
begin
  Result.Fixed := Lhs.Fixed + Rhs.Fixed;
end;

class operator TFixed8Dot24.Add(const Lhs: TFixed8Dot24; Rhs: Single)
  : TFixed8Dot24;
begin
  Result.Fixed := Lhs.Fixed + ConvertToFixed8Dot24(Rhs).Fixed;
end;

class operator TFixed8Dot24.Add(const Lhs: TFixed8Dot24; Rhs: Integer)
  : TFixed8Dot24;
begin
  Result.Fixed := Lhs.Fixed + Rhs shl 24;
end;

class function TFixed8Dot24.ArcTan2(const A, B: TFixed8Dot24): TFixed8Dot24;
begin
  Result := ConvertToFixed8Dot24(Math.ArcTan2(ConvertFromFixed8Dot24(A),
    ConvertFromFixed8Dot24(B)));
end;

class operator TFixed8Dot24.Divide(const Lhs, Rhs: TFixed8Dot24): TFixed8Dot24;
begin
  Result := FixedDiv(Lhs, Rhs);
end;

class operator TFixed8Dot24.Divide(const Lhs: TFixed8Dot24; Rhs: Integer)
  : TFixed8Dot24;
begin
  Result := FixedDiv(Lhs, Rhs);
end;

class operator TFixed8Dot24.Equal(const Lhs, Rhs: TFixed8Dot24): Boolean;
begin
  Result := Lhs.Fixed = Rhs.Fixed;
end;

class operator TFixed8Dot24.GreaterThan(const Lhs, Rhs: TFixed8Dot24): Boolean;
begin
  Result := Lhs.Fixed > Rhs.Fixed;
end;

class operator TFixed8Dot24.GreaterThanOrEqual(const Lhs,
  Rhs: TFixed8Dot24): Boolean;
begin
  Result := Lhs.Fixed >= Rhs.Fixed;
end;

class function TFixed8Dot24.Half: TFixed8Dot24;
begin
  Result := CFixed8Dot24Half;
end;

class operator TFixed8Dot24.LeftShift(const Value: TFixed8Dot24; Shift: Byte)
  : TFixed8Dot24;
begin
  Result.Fixed := Value.Fixed shl Shift;
end;

class operator TFixed8Dot24.LessThan(const Lhs, Rhs: TFixed8Dot24): Boolean;
begin
  Result := Lhs.Fixed < Rhs.Fixed;
end;

class operator TFixed8Dot24.LessThanOrEqual(const Lhs,
  Rhs: TFixed8Dot24): Boolean;
begin
  Result := Lhs.Fixed <= Rhs.Fixed;
end;

class operator TFixed8Dot24.Multiply(const Lhs, Rhs: TFixed8Dot24)
  : TFixed8Dot24;
begin
  Result := FixedMul(Lhs, Rhs);
end;

class operator TFixed8Dot24.Negative(const Value: TFixed8Dot24): TFixed8Dot24;
begin
  Result.Fixed := -Value.Fixed;
end;

class operator TFixed8Dot24.NotEqual(const Lhs, Rhs: TFixed8Dot24): Boolean;
begin
  Result := Lhs.Fixed <> Rhs.Fixed;
end;

class function TFixed8Dot24.One: TFixed8Dot24;
begin
  Result := CFixed8Dot24One;
end;

class operator TFixed8Dot24.Positive(const Value: TFixed8Dot24): TFixed8Dot24;
begin
  Result.Fixed := Abs(Value.Fixed);
end;

class operator TFixed8Dot24.RightShift(const Value: TFixed8Dot24; Shift: Byte)
  : TFixed8Dot24;
begin
  Result.Fixed := Value.Fixed shr Shift;
end;

{$IFNDEF FPC}

class operator TFixed8Dot24.Round(const Value: TFixed8Dot24): Integer;
begin
  Result := FixedRound(Value);
end;
{$ENDIF}

class operator TFixed8Dot24.Subtract(const Lhs, Rhs: TFixed8Dot24)
  : TFixed8Dot24;
begin
  Result.Fixed := Lhs.Fixed - Rhs.Fixed;
end;

class operator TFixed8Dot24.Subtract(const Lhs: TFixed8Dot24; Rhs: Single)
  : TFixed8Dot24;
begin
  Result.Fixed := Lhs.Fixed - ConvertToFixed8Dot24(Rhs).Fixed;
end;

class operator TFixed8Dot24.Subtract(const Lhs: TFixed8Dot24; Rhs: Integer)
  : TFixed8Dot24;
begin
  Result.Fixed := Lhs.Fixed - Rhs shl 24;
end;

class function TFixed8Dot24.Two: TFixed8Dot24;
begin
  Result := CFixed8Dot24Two;
end;

class function TFixed8Dot24.Zero: TFixed8Dot24;
begin
  Result.Fixed := 0;
end;

class operator TFixed8Dot24.Implicit(const Value: Single): TFixed8Dot24;
begin
  Result := ConvertToFixed8Dot24(Value);
end;

class operator TFixed8Dot24.Implicit(const Value: Double): TFixed8Dot24;
begin
  Result := ConvertToFixed8Dot24(Value);
end;

class operator TFixed8Dot24.Implicit(const Value: Integer): TFixed8Dot24;
begin
  Result.Fixed := Value shl 24;
end;

class operator TFixed8Dot24.Explicit(const Value: TFixed8Dot24): Single;
begin
  Result := ConvertFromFixed8Dot24(Value);
end;

class operator TFixed8Dot24.Explicit(const Value: TFixed8Dot24): Double;
begin
  Result := ConvertFromFixed8Dot24(Value);
end;

function TFixed8Dot24.AsSingle: Single;
begin
  Result := ConvertFromFixed8Dot24(Self);
end;

{ TFixed16Dot16 }

{$IFNDEF FPC}

constructor TFixed16Dot16.Create(const Fixed: Integer);
begin
  Self.Fixed := Fixed;
end;

constructor TFixed16Dot16.Create(const Frac: Word; Int: SmallInt);
begin
  Self.Frac := Frac;
  Self.Int := Int;
end;
{$ENDIF}

class operator TFixed16Dot16.Add(const Lhs, Rhs: TFixed16Dot16): TFixed16Dot16;
begin
  Result.Fixed := Lhs.Fixed + Rhs.Fixed;
end;

class operator TFixed16Dot16.Add(const Lhs: TFixed16Dot16; Rhs: Single)
  : TFixed16Dot16;
begin
  Result.Fixed := Lhs.Fixed + ConvertToFixed16Dot16(Rhs).Fixed;
end;

class operator TFixed16Dot16.Add(const Lhs: TFixed16Dot16; Rhs: Integer)
  : TFixed16Dot16;
begin
  Result.Fixed := Lhs.Fixed + Rhs shl 16;
end;

class function TFixed16Dot16.ArcTan2(const A, B: TFixed16Dot16): TFixed16Dot16;
begin
  Result := ConvertToFixed16Dot16(Math.ArcTan2(ConvertFromFixed16Dot16(A),
    ConvertFromFixed16Dot16(B)));
end;

class operator TFixed16Dot16.Divide(const Lhs, Rhs: TFixed16Dot16)
  : TFixed16Dot16;
begin
  Result := FixedDiv(Lhs, Rhs);
end;

class operator TFixed16Dot16.Divide(const Lhs: TFixed16Dot16; Rhs: Integer)
  : TFixed16Dot16;
begin
  Result := FixedDiv(Lhs, Rhs);
end;

class operator TFixed16Dot16.Equal(const Lhs, Rhs: TFixed16Dot16): Boolean;
begin
  Result := Lhs.Fixed = Rhs.Fixed;
end;

class operator TFixed16Dot16.GreaterThan(const Lhs, Rhs: TFixed16Dot16)
  : Boolean;
begin
  Result := Lhs.Fixed > Rhs.Fixed;
end;

class operator TFixed16Dot16.GreaterThanOrEqual(const Lhs,
  Rhs: TFixed16Dot16): Boolean;
begin
  Result := Lhs.Fixed >= Rhs.Fixed;
end;

class function TFixed16Dot16.Half: TFixed16Dot16;
begin
  Result := CFixed16Dot16Half;
end;

class operator TFixed16Dot16.LeftShift(const Value: TFixed16Dot16; Shift: Byte)
  : TFixed16Dot16;
begin
  Result.Fixed := Value.Fixed shl Shift;
end;

class operator TFixed16Dot16.LessThan(const Lhs, Rhs: TFixed16Dot16): Boolean;
begin
  Result := Lhs.Fixed < Rhs.Fixed;
end;

class operator TFixed16Dot16.LessThanOrEqual(const Lhs,
  Rhs: TFixed16Dot16): Boolean;
begin
  Result := Lhs.Fixed <= Rhs.Fixed;
end;

class operator TFixed16Dot16.Multiply(const Lhs, Rhs: TFixed16Dot16)
  : TFixed16Dot16;
begin
  Result := FixedMul(Lhs, Rhs);
end;

class operator TFixed16Dot16.Negative(const Value: TFixed16Dot16)
  : TFixed16Dot16;
begin
  Result.Fixed := -Value.Fixed;
end;

class operator TFixed16Dot16.NotEqual(const Lhs, Rhs: TFixed16Dot16): Boolean;
begin
  Result := Lhs.Fixed <> Rhs.Fixed;
end;

class function TFixed16Dot16.One: TFixed16Dot16;
begin
  Result := CFixed16Dot16One;
end;

class operator TFixed16Dot16.Positive(const Value: TFixed16Dot16)
  : TFixed16Dot16;
begin
  Result.Fixed := Abs(Value.Fixed);
end;

class operator TFixed16Dot16.RightShift(const Value: TFixed16Dot16; Shift: Byte)
  : TFixed16Dot16;
begin
  Result.Fixed := Value.Fixed shr Shift;
end;

{$IFNDEF FPC}

class operator TFixed16Dot16.Round(const Value: TFixed16Dot16): Integer;
begin
  Result := FixedRound(Value);
end;
{$ENDIF}

class operator TFixed16Dot16.Subtract(const Lhs, Rhs: TFixed16Dot16)
  : TFixed16Dot16;
begin
  Result.Fixed := Lhs.Fixed - Rhs.Fixed;
end;

class operator TFixed16Dot16.Subtract(const Lhs: TFixed16Dot16; Rhs: Single)
  : TFixed16Dot16;
begin
  Result.Fixed := Lhs.Fixed - ConvertToFixed16Dot16(Rhs).Fixed;
end;

class operator TFixed16Dot16.Subtract(const Lhs: TFixed16Dot16; Rhs: Integer)
  : TFixed16Dot16;
begin
  Result.Fixed := Lhs.Fixed - Rhs shl 16;
end;

class function TFixed16Dot16.Two: TFixed16Dot16;
begin
  Result := CFixed16Dot16Two;
end;

class function TFixed16Dot16.Zero: TFixed16Dot16;
begin
  Result.Fixed := 0;
end;

class operator TFixed16Dot16.Implicit(const Value: Single): TFixed16Dot16;
begin
  Result := ConvertToFixed16Dot16(Value);
end;

class operator TFixed16Dot16.Implicit(const Value: Double): TFixed16Dot16;
begin
  Result := ConvertToFixed16Dot16(Value);
end;

class operator TFixed16Dot16.Implicit(const Value: Integer): TFixed16Dot16;
begin
  Result.Fixed := Value shl 16;
end;

class operator TFixed16Dot16.Explicit(const Value: TFixed16Dot16): Single;
begin
  Result := ConvertFromFixed16Dot16(Value);
end;

class operator TFixed16Dot16.Explicit(const Value: TFixed16Dot16): Double;
begin
  Result := ConvertFromFixed16Dot16(Value);
end;

function TFixed16Dot16.AsSingle: Single;
begin
  Result := ConvertFromFixed16Dot16(Self);
end;

{ TFixed24Dot8 }

{$IFNDEF FPC}

constructor TFixed24Dot8.Create(const Fixed: Integer);
begin
  Self.Fixed := Fixed;
end;

constructor TFixed24Dot8.Create(const Frac: Byte; Int: Integer);
begin
  Self.Fixed := Int shl 8;
  Self.Frac := Frac;
end;

constructor TFixed24Dot8.Create(const Frac: Byte; Low: Byte; High: SmallInt);
begin
  Self.Frac := Frac;
  Self.Low := Low;
  Self.High := High;
end;
{$ENDIF}

class operator TFixed24Dot8.Add(const Lhs, Rhs: TFixed24Dot8): TFixed24Dot8;
begin
  Result.Fixed := Lhs.Fixed + Rhs.Fixed;
end;

class operator TFixed24Dot8.Add(const Lhs: TFixed24Dot8; Rhs: Single)
  : TFixed24Dot8;
begin
  Result.Fixed := Lhs.Fixed + ConvertToFixed24Dot8(Rhs).Fixed;
end;

class operator TFixed24Dot8.Add(const Lhs: TFixed24Dot8; Rhs: Integer)
  : TFixed24Dot8;
begin
  Result.Fixed := Lhs.Fixed + Rhs shl 8;
end;

class operator TFixed24Dot8.Negative(const Value: TFixed24Dot8): TFixed24Dot8;
begin
  Result.Fixed := -Value.Fixed;
end;

class operator TFixed24Dot8.Positive(const Value: TFixed24Dot8): TFixed24Dot8;
begin
  Result.Fixed := Abs(Value.Fixed);
end;

class operator TFixed24Dot8.Divide(const Lhs: TFixed24Dot8; Rhs: Integer)
  : TFixed24Dot8;
begin
  Result := FixedDiv(Lhs, Rhs);
end;

class operator TFixed24Dot8.Divide(const Lhs, Rhs: TFixed24Dot8): TFixed24Dot8;
begin
  Result := FixedDiv(Lhs, Rhs);
end;

class operator TFixed24Dot8.Equal(const Lhs, Rhs: TFixed24Dot8): Boolean;
begin
  Result := Lhs.Fixed = Rhs.Fixed;
end;

class operator TFixed24Dot8.GreaterThan(const Lhs, Rhs: TFixed24Dot8): Boolean;
begin
  Result := Lhs.Fixed > Rhs.Fixed;
end;

class operator TFixed24Dot8.GreaterThanOrEqual(const Lhs,
  Rhs: TFixed24Dot8): Boolean;
begin
  Result := Lhs.Fixed >= Rhs.Fixed;
end;

class function TFixed24Dot8.Half: TFixed24Dot8;
begin
  Result := CFixed24Dot8Half;
end;

class operator TFixed24Dot8.LeftShift(const Value: TFixed24Dot8; Shift: Byte)
  : TFixed24Dot8;
begin
  Result.Fixed := Value.Fixed shl Shift;
end;

class operator TFixed24Dot8.LessThan(const Lhs, Rhs: TFixed24Dot8): Boolean;
begin
  Result := Lhs.Fixed < Rhs.Fixed;
end;

class operator TFixed24Dot8.LessThanOrEqual(const Lhs,
  Rhs: TFixed24Dot8): Boolean;
begin
  Result := Lhs.Fixed <= Rhs.Fixed;
end;

class operator TFixed24Dot8.Multiply(const Lhs, Rhs: TFixed24Dot8)
  : TFixed24Dot8;
begin
  Result := FixedMul(Lhs, Rhs);
end;

class operator TFixed24Dot8.NotEqual(const Lhs, Rhs: TFixed24Dot8): Boolean;
begin
  Result := Lhs.Fixed <> Rhs.Fixed;
end;

class function TFixed24Dot8.One: TFixed24Dot8;
begin
  Result := CFixed24Dot8One;
end;

class operator TFixed24Dot8.RightShift(const Value: TFixed24Dot8; Shift: Byte)
  : TFixed24Dot8;
begin
  Result.Fixed := Value.Fixed shr Shift;
end;

{$IFNDEF FPC}

class operator TFixed24Dot8.Round(const Value: TFixed24Dot8): Integer;
begin
  Result := FixedRound(Value)
end;
{$ENDIF}

class operator TFixed24Dot8.Subtract(const Lhs, Rhs: TFixed24Dot8)
  : TFixed24Dot8;
begin
  Result.Fixed := Lhs.Fixed - Rhs.Fixed;
end;

class operator TFixed24Dot8.Subtract(const Lhs: TFixed24Dot8; Rhs: Single)
  : TFixed24Dot8;
begin
  Result.Fixed := Lhs.Fixed - ConvertToFixed24Dot8(Rhs).Fixed;
end;

class operator TFixed24Dot8.Subtract(const Lhs: TFixed24Dot8; Rhs: Integer)
  : TFixed24Dot8;
begin
  Result.Fixed := Lhs.Fixed - Rhs shl 8;
end;

class function TFixed24Dot8.Two: TFixed24Dot8;
begin
  Result := CFixed24Dot8Two;
end;

class function TFixed24Dot8.Zero: TFixed24Dot8;
begin
  Result.Fixed := 0;
end;

class function TFixed24Dot8.ArcTan2(const A, B: TFixed24Dot8): TFixed24Dot8;
begin
  Result := ConvertToFixed24Dot8(Math.ArcTan2(ConvertFromFixed24Dot8(A),
    ConvertFromFixed24Dot8(B)));
end;

class operator TFixed24Dot8.Implicit(const Value: Single): TFixed24Dot8;
begin
  Result := ConvertToFixed24Dot8(Value);
end;

class operator TFixed24Dot8.Implicit(const Value: Double): TFixed24Dot8;
begin
  Result := ConvertToFixed24Dot8(Value);
end;

class operator TFixed24Dot8.Implicit(const Value: Integer): TFixed24Dot8;
begin
  Result.Fixed := Value shl 8;
end;

class operator TFixed24Dot8.Explicit(const Value: TFixed24Dot8): Single;
begin
  Result := ConvertFromFixed24Dot8(Value);
end;

class operator TFixed24Dot8.Explicit(const Value: TFixed24Dot8): Double;
begin
  Result := ConvertFromFixed24Dot8(Value);
end;

function TFixed24Dot8.AsSingle: Single;
begin
  Result := ConvertFromFixed24Dot8(Self);
end;

(*
  procedure TFixed24Dot8.Sin;
  begin
  Self := ConvertToFixed24Dot8(Math.ArcTan(ConvertFromFixed24Dot8(Self)));
  end;
*)

{$ENDIF}

end.
