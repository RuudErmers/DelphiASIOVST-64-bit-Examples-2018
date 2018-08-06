unit DAV_DspInterpolation;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}
{$IFDEF CPUx86_64}{$DEFINE PUREPASCAL}{$ENDIF}
{$IFDEF FPC}{$DEFINE PUREPASCAL}{$ENDIF}

uses
  DAV_Types, DAV_Complex;

function Hermite1(const Fractional: Single; const Data: TDAV4SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function Hermite1(const Fractional: Double; const Data: TDAV4DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function Hermite2(const Fractional: Single; const Data: TDAV4SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function Hermite2(const Fractional: Double; const Data: TDAV4DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function Hermite3(const Fractional: Single; const Data: TDAV4SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function Hermite3(const Fractional: Double; const Data: TDAV4DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function Hermite4(const Fractional: Single; const Data: TDAV4SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function Hermite4(const Fractional: Double; const Data: TDAV4DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function Hermite32_asm(const Fractional: Single; Pntr: PDAV4SingleArray): Single;
function Hermite64_asm(const Fractional: Double; Pntr: PDAV4DoubleArray): Double;
function Hermite32I_asm(const Fractional: Single; Pntr: PDAVComplexSingleFixedArray): Single;
function Hermite64I_asm(const Fractional: Double; Pntr: PDAVComplexDoubleFixedArray): Double;
function LinearInterpolation(const Fractional: Single; const Data: TDAV2SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function LinearInterpolation(const Fractional: Double; const Data: TDAV2DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function LinearInterpolation(const Fractional: Single; const Data: PDAV2SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function LinearInterpolation(const Fractional: Double; const Data: PDAV2DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function CubicInterpolation(const Fractional: Single; const Data: TDAV4SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function CubicInterpolation(const Fractional: Double; const Data: TDAV4DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function CubicInterpolation(const Fractional: Single; const Data: PDAV4SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function CubicInterpolation(const Fractional: Double; const Data: PDAV4DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function BSplineInterpolation4Point3rdOrder(const Fractional: Single; const Data: TDAV4SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function BSplineInterpolation4Point3rdOrder(const Fractional: Double; const Data: TDAV4DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function BSplineInterpolation4Point3rdOrder(const Fractional: Single; Data: PDAV4SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function BSplineInterpolation4Point3rdOrder(const Fractional: Double; Data: PDAV4DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function BSplineInterpolation6Point5thOrder(const Fractional: Single; const Data: TDAV6SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function BSplineInterpolation6Point5thOrder(const Fractional: Double; const Data: TDAV6DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}

const
  CHalf32     : Single = 0.5;
  CHalf64     : Double = 0.5;
  COneSixth32 : Single = 1/6;
  COneSixth64 : Double = 1/6;

implementation

uses
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Hermite Interpolators                                                     //
//  ---------------------                                                     //
//                                                                            //
//  Parameter Explanation:                                                    //
//                                                                            //
//     Fractional :  fractional value [0.0f - 1.0f] to interpolator           //
//     Pntr       :  pointer to float array where:                            //
//     Pntr[0]    :  previous sample (idx = -1)                               //
//     Pntr[1]    :  current sample (idx = 0)                                 //
//     Pntr[2]    :  next sample (idx = +1)                                   //
//     Pntr[3]    :  after next sample (idx = +2)                             //
//                                                                            //
//     The interpolation takes place between Pntr[1] and Pntr[2].             //
//                                                                            //
//  Types:                                                                    //
//                                                                            //
//    Hermite32_asm   :  Direct Data, Single Precision                        //
//    Hermite64_asm   :  Direct Data, Double Precision                        //
//    Hermite32I_asm  :  Interleaved Data, Single Precision                   //
//    Hermite64I_asm  :  Interleaved Data, Double Precision                   //
//                                                                            //
//    The interleaved versions are interesting for performing an              //
//    interpolation of complex values.                                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function Hermite32_asm(const Fractional: Single; Pntr: PDAV4SingleArray): Single;
{$IFDEF PUREPASCAL}
var
  c : TDAV4SingleArray;
begin
  c[0] := (Pntr^[2] - Pntr^[0]) * CHalf32;
  c[1] := Pntr^[1] - Pntr^[2];
  c[2] := c[0] + c[1];
  c[3] := c[2] + c[1] + (Pntr^[3] - Pntr^[1]) * CHalf32;
  Result := ((((c[3] * Fractional) - (c[2] + c[3])) * Fractional + c[0]) *
    Fractional + Pntr^[1]);
end;
{$ELSE}
asm
    FLD     [Pntr +  8].Single  // x1
    FSUB    [Pntr     ].Single  // x1-xm1
    FLD     [Pntr +  4].Single  // x0           x1-xm1
    FSUB    [Pntr +  8].Single  // v            x1-xm1
    FLD     [Pntr + 12].Single  // x2           v            x1-xm1
    FSUB    [Pntr +  4].Single  // x2-x0        v            x1-xm1
    FXCH    ST(2)               // x1-m1        v            x2-x0
    FMUL    CHalf32             // c            v            x2-x0
    FXCH    ST(2)               // x2-x0        v            c
    FMUL    CHalf32             // 0.5*(x2-x0)  v            c
    FXCH    ST(2)               // c            v            0.5*(x2-x0)
    FST     ST(3)               // c            v            0.5*(x2-x0)  c
    FADD    ST(0), ST(1)        // w            v            0.5*(x2-x0)  c
    FXCH    ST(2)               // 0.5*(x2-x0)  v            w            c
    FADDP   ST(1), ST(0)        // v+.5(x2-x0)  w            c
    FADD    ST(0), ST(1)        // a            w            c
    FADD    ST(1), ST(0)        // a            b_neg        c
    FMUL    Fractional.Single   // a * frac     b_neg        c
    FSUBRP  ST(1), ST(0)        // a * f-b      c
    FMUL    Fractional.Single   // (a*f-b)*f    c
    FADDP   ST(1), ST(0)        // res-x0/f
    FMUL    Fractional.Single   // res-x0
    FADD    [Pntr + 4].Single   // res
end;
{$ENDIF}

function Hermite64_asm(const Fractional: Double; Pntr: PDAV4DoubleArray): Double;
{$IFDEF PUREPASCAL}
var
  c : TDAV4DoubleArray;
  b : Single;
begin
  c[0] := (Pntr[2] - Pntr[0]) * CHalf32;
  c[1] := Pntr[1] - Pntr[2];
  c[2] := c[0] + c[1];
  c[3] := c[2] + c[1] + (Pntr[3] - Pntr[1]) * CHalf32;
  b    := c[2] + c[3];
  Result := ((((c[3] * Fractional) - b) * Fractional + c[0]) * Fractional + Pntr[1]);
end;
{$ELSE}
asm
    FLD     [Pntr + 16].Double  // x1
    FSUB    [Pntr     ].Double  // x1-xm1
    FLD     [Pntr +  8].Double  // x0           x1-xm1
    FSUB    [Pntr + 16].Double  // v            x1-xm1
    FLD     [Pntr + 24].Double  // x2           v            x1-xm1
    FSUB    [Pntr +  8].Double  // x2-x0        v            x1-xm1
    FXCH    ST(2)               // x1-m1        v            x2-x0
    FMUL    CHalf64             // c            v            x2-x0
    FXCH    ST(2)               // x2-x0        v            c
    FMUL    CHalf64             // 0.5*(x2-x0)  v            c
    FXCH    ST(2)               // c            v            0.5*(x2-x0)
    FST     ST(3)               // c            v            0.5*(x2-x0)  c
    FADD    ST(0), ST(1)        // w            v            0.5*(x2-x0)  c
    FXCH    ST(2)               // 0.5*(x2-x0)  v            w            c
    FADDP   ST(1), ST(0)        // v+.5(x2-x0)  w            c
    FADD    ST(0), ST(1)        // a            w            c
    FADD    ST(1), ST(0)        // a            b_neg        c
    FMUL    Fractional.Double   // a*frac       b_neg        c
    FSUBRP  ST(1), ST(0)        // a*f-b        c
    FMUL    Fractional.Double   // (a*f-b)*f    c
    FADDP   ST(1), ST(0)        // res-x0/f
    FMUL    Fractional.Double   // res-x0
    FADD    [Pntr + 8].Double   // res
end;
{$ENDIF}

function Hermite32I_asm(const Fractional: Single; Pntr: PDAVComplexSingleFixedArray): Single;
{$IFDEF PUREPASCAL}
begin
  raise Exception.Create('not implemented yet');
end;
{$ELSE}
asm
    FLD     [Pntr + 16].Single  // x1
    FSUB    [Pntr     ].Single  // x1-xm1
    FLD     [Pntr +  8].Single  // x0           x1-xm1
    FSUB    [Pntr + 16].Single  // v            x1-xm1
    FLD     [Pntr + 24].Single  // x2           v            x1-xm1
    FSUB    [Pntr +  8].Single  // x2-x0        v            x1-xm1
    FXCH    ST(2)               // x1-m1        v            x2-x0
    FMUL    CHalf32             // c            v            x2-x0
    FXCH    ST(2)               // x2-x0        v            c
    FMUL    CHalf32             // 0.5*(x2-x0)  v            c
    FXCH    ST(2)               // c            v            0.5*(x2-x0)
    FST     ST(3)               // c            v            0.5*(x2-x0)  c
    FADD    ST(0), ST(1)        // w            v            0.5*(x2-x0)  c
    FXCH    ST(2)               // 0.5*(x2-x0)  v            w            c
    FADDP   ST(1), ST(0)        // v+.5(x2-x0)  w            c
    FADD    ST(0), ST(1)        // a            w            c
    FADD    ST(1), ST(0)        // a            b_neg        c
    FMUL    Fractional.Single   // a*frac       b_neg        c
    FSUBRP  ST(1), ST(0)        // a*f-b        c
    FMUL    Fractional.Single   // (a*f-b)*f    c
    FADDP   ST(1), ST(0)        // res-x0/f
    FMUL    Fractional.Single   // res-x0
    FADD    [Pntr + 8].Single   // res
end;
{$ENDIF}

function Hermite64I_asm(const Fractional: Double; Pntr: PDAVComplexDoubleFixedArray): Double;
{$IFDEF PUREPASCAL}
begin
  raise Exception.Create('not implemented yet');
end;
{$ELSE}
asm
    FLD     [Pntr + 32].Double  // x1
    FSUB    [Pntr     ].Double  // x1-xm1
    FLD     [Pntr + 16].Double  // x0           x1-xm1
    FSUB    [Pntr + 32].Double  // v            x1-xm1
    FLD     [Pntr + 48].Double  // x2           v            x1-xm1
    FSUB    [Pntr + 16].Double  // x2-x0        v            x1-xm1
    FXCH    ST(2)               // x1-m1        v            x2-x0
    FMUL    CHalf64             // c            v            x2-x0
    FXCH    ST(2)               // x2-x0        v            c
    FMUL    CHalf64             // 0.5*(x2-x0)  v            c
    FXCH    ST(2)               // c            v            0.5*(x2-x0)
    FST     ST(3)               // c            v            0.5*(x2-x0)  c
    FADD    ST(0), ST(1)        // w            v            0.5*(x2-x0)  c
    FXCH    ST(2)               // 0.5*(x2-x0)  v            w            c
    FADDP   ST(1), ST(0)        // v+.5(x2-x0)  w            c
    FADD    ST(0), ST(1)        // a            w            c
    FADD    ST(1), ST(0)        // a            b_neg        c
    FMUL    Fractional.Double   // a*frac       b_neg        c
    FSUBRP  ST(1), ST(0)        // a*f-b        c
    FMUL    Fractional.Double   // (a*f-b)*f    c
    FADDP   ST(1), ST(0)        // res-x0/f
    FMUL    Fractional.Double   // res-x0
    FADD   [Pntr + 16].Double   // res
end;
{$ENDIF}

function Hermite1(const Fractional: Single; const Data: TDAV4SingleArray): Single;
var
  c : TDAV4SingleArray;
begin
  // 4-point, 3rd-order Hermite (x-form)
  c[1] := CHalf32 * (Data[2] - Data[0]);
  c[2] := Data[0] - 2.5 * Data[1] + 2 * Data[2] - CHalf32 * Data[3];
  c[3] := 1.5 * (Data[1] - Data[2]) + CHalf32 * (Data[3] - Data[0]);
  Result := ((c[3] * Fractional + c[2]) * Fractional + c[1]) * Fractional + Data[1];
end;

function Hermite1(const Fractional: Double; const Data: TDAV4DoubleArray): Double;
var
  c : TDAV4DoubleArray;
begin
  // 4-point, 3rd-order Hermite (x-form)
  c[1] := CHalf32 * (Data[2] - Data[0]);
  c[2] := Data[0] - 2.5 * Data[1] + 2 * Data[2] - CHalf32 * Data[3];
  c[3] := 1.5 * (Data[1] - Data[2]) + CHalf32 * (Data[3] - Data[0]);
  Result := ((c[3] * Fractional + c[2]) * Fractional + c[1]) * Fractional + Data[1];
end;

function Hermite2(const Fractional: Single; const Data: TDAV4SingleArray): Single;
var
  c : TDAV4SingleArray;
begin
  // 4-point, 3rd-order Hermite (x-form)
  c[0] := Data[1];
  c[1] := CHalf32 * (Data[2] - Data[0]);
  c[3] := 1.5 * (Data[1] - Data[2]) + CHalf32 * (Data[3] - Data[0]);
  c[2] := Data[0] - Data[1] + c[1] - c[3];
  Result := ((c[3] * Fractional + c[2]) * Fractional + c[1]) * Fractional + c[0];
end;

function Hermite2(const Fractional: Double; const Data: TDAV4DoubleArray): Double;
var
  c : TDAV4DoubleArray;
begin
  // 4-point, 3rd-order Hermite (x-form)
  c[0] := Data[1];
  c[1] := CHalf32 * (Data[2] - Data[0]);
  c[3] := 1.5 * (Data[1] - Data[2]) + CHalf32 * (Data[3] - Data[0]);
  c[2] := Data[0] - Data[1] + c[1] - c[3];
  Result := ((c[3] * Fractional + c[2]) * Fractional + c[1]) * Fractional + c[0];
end;

function Hermite3(const Fractional: Single; const Data: TDAV4SingleArray): Single;
var
  c : TDAV4SingleArray;
begin
  // 4-point, 3rd-order Hermite (x-form)
  c[1]   := CHalf32 * (Data[2] - Data[0]);
  c[0]   := Data[0] - Data[1];
  c[3]   := (Data[1] - Data[2]) + CHalf32 * (Data[3] - c[0] - Data[2]);
  c[2]   := c[0] + c[1] - c[3];
  Result := ((c[3] * Fractional + c[2]) * Fractional + c[1]) * Fractional + Data[1];
end;

function Hermite3(const Fractional: Double; const Data: TDAV4DoubleArray): Double;
var
  c : TDAV4DoubleArray;
begin
  // 4-point, 3rd-order Hermite (x-form)
  c[1]   := CHalf32 * (Data[2] - Data[0]);
  c[0]   := Data[0] - Data[1];
  c[3]   := (Data[1] - Data[2]) + CHalf32 * (Data[3] - c[0] - Data[2]);
  c[2]   := c[0] + c[1] - c[3];
  Result := ((c[3] * Fractional + c[2]) * Fractional + c[1]) * Fractional + Data[1];
end;

function Hermite4(const Fractional: Single; const Data: TDAV4SingleArray): Single;
var
  c : TDAV4SingleArray;
  b : Single;
begin
  c[0] := (Data[2] - Data[0]) * CHalf32;
  c[1] := Data[1] - Data[2];
  c[2] := c[0] + c[1];
  c[3] := c[2] + c[1] + (Data[3] - Data[1]) * CHalf32;
  b    := c[2] + c[3];
  Result := ((((c[3] * Fractional) - b) * Fractional + c[0]) * Fractional + Data[1]);
end;

function Hermite4(const Fractional: Double; const Data: TDAV4DoubleArray): Double;
var
  c : TDAV4DoubleArray;
  b : Double;
begin
  c[0] := (Data[2] - Data[0]) * CHalf32;
  c[1] := Data[1] - Data[2];
  c[2] := c[0] + c[1];
  c[3] := c[2] + c[1] + (Data[3] - Data[1]) * CHalf32;
  b    := c[2] + c[3];
  Result := ((((c[3] * Fractional) - b) * Fractional + c[0]) * Fractional + Data[1]);
end;

function LinearInterpolation(const Fractional: Single; const Data: TDAV2SingleArray): Single;
begin
  Result := (1 - Fractional) * Data[0] + Fractional * Data[1];
end;

function LinearInterpolation(const Fractional: Double; const Data: TDAV2DoubleArray): Double;
begin
  Result := (1 - Fractional) * Data[0] + Fractional * Data[1];
end;

function LinearInterpolation(const Fractional: Single; const Data: PDAV2SingleArray): Single;
begin
  Result := (1 - Fractional) * Data^[0] + Fractional * Data^[1];
end;

function LinearInterpolation(const Fractional: Double; const Data: PDAV2DoubleArray): Double;
begin
  Result := (1 - Fractional) * Data^[0] + Fractional * Data^[1];
end;

function CubicInterpolation(const Fractional: Single; const Data: TDAV4SingleArray): Single;
begin
  Result := Data[1] + 0.5 * Fractional * (Data[2] - Data[0] + Fractional *
    (4 * Data[2] + 2 * Data[0] - 5 * Data[1] - Data[3] + Fractional *
    (3 * (Data[1] - Data[2]) - Data[0] + Data[3])));
end;

function CubicInterpolation(const Fractional: Double; const Data: TDAV4DoubleArray): Double;
begin
  Result := Data[1] + 0.5 * Fractional * (Data[2] - Data[0] + Fractional *
    (4 * Data[2] + 2 * Data[0] - 5 * Data[1] - Data[3] + Fractional *
    (3 * (Data[1] - Data[2]) - Data[0] + Data[3])));
end;

function CubicInterpolation(const Fractional: Single; const Data: PDAV4SingleArray): Single;
begin
  Result := Data^[1] + 0.5 * Fractional * (Data^[2] - Data^[0] + Fractional *
    (4 * Data^[2] + 2 * Data^[0] - 5 * Data^[1] - Data^[3] + Fractional *
    (3 * (Data^[1] - Data^[2]) - Data^[0] + Data^[3])));
end;

function CubicInterpolation(const Fractional: Double; const Data: PDAV4DoubleArray): Double;
begin
  Result := Data^[1] + 0.5 * Fractional * (Data^[2] - Data^[0] + Fractional *
    (4 * Data^[2] + 2 * Data^[0] - 5 * Data^[1] - Data^[3] + Fractional *
    (3 * (Data^[1] - Data^[2]) - Data^[0] + Data^[3])));
end;

function BSplineInterpolation4Point3rdOrder(const Fractional: Single; const Data: TDAV4SingleArray): Single;
var
  c : TDAV4SingleArray;
  b : Single;
begin
 // 4-point, 3rd-order B-spline (x-form)
 b  := Data[0] + Data[2];
 c[0] := COneSixth32 * (b + 4 * Data[1]);
 c[1] := CHalf32 * (Data[2] - Data[0]);
 c[2] := CHalf32 * b - Data[1];
 c[3] := CHalf32 * (Data[1] - Data[2]) + COneSixth32 * (Data[3] - Data[0]);
 result := ((c[3] * Fractional + c[2]) * Fractional + c[1]) * Fractional + c[0];
end;

function BSplineInterpolation4Point3rdOrder(const Fractional: Double; const Data: TDAV4DoubleArray): Double;
var
  c : TDAV4DoubleArray;
  b : Double;
begin
 // 4-point, 3rd-order B-spline (x-form)
 b  := Data[0] + Data[2];
 c[0] := COneSixth64 * (b + 4 * Data[1]);
 c[1] := CHalf32 * (Data[2] - Data[0]);
 c[2] := CHalf32 * b - Data[1];
 c[3] := CHalf32 * (Data[1] - Data[2]) + COneSixth64 * (Data[3] - Data[0]);
 result := ((c[3] * Fractional + c[2]) * Fractional + c[1]) * Fractional + c[0];
end;

function BSplineInterpolation4Point3rdOrder(const Fractional: Single; Data: PDAV4SingleArray): Single;
begin
 Result := BSplineInterpolation4Point3rdOrder(Fractional, Data^);
end;

function BSplineInterpolation4Point3rdOrder(const Fractional: Double; Data: PDAV4DoubleArray): Double;
begin
 Result := BSplineInterpolation4Point3rdOrder(Fractional, Data^);
end;

function BSplineInterpolation6Point5thOrder(const Fractional: Single; const Data: TDAV6SingleArray): Single;
var
  c : TDAV8SingleArray;
begin
 // 6-point, 5th-order B-spline (x-form)
 c[0] := Data[0] + Data[4];
 c[1] := COneSixth32 * (Data[1] + Data[3]);
 c[2] := Data[4] - Data[0];
 c[3] := Data[3] - Data[1];
 c[4] := 1/120 * c[0] + 13/10 * c[1] + 11/20 * Data[2];
 c[5] := 1/24 * c[2] + 5/12 * c[3];
 c[6] := 1/12 * c[0] + c[1] - 1/2 * Data[2];
 c[7] := COneSixth32 * (CHalf32 * c[2] - c[3]);
 result := (((((1/120 * (Data[5] - Data[0]) + 1/24 * (Data[1] - Data[3]) + 1 / 12.0 * (Data[3] - Data[2])) * Fractional + (1/24 * c[0] - c[1] + 1/4 * Data[2])) * Fractional + c[7]) * Fractional + c[6]) * Fractional + c[5]) * Fractional + c[4];
end;

function BSplineInterpolation6Point5thOrder(const Fractional: Double; const Data: TDAV6DoubleArray): Double;
var
  c : TDAV4DoubleArray;
  b : Double;
begin
 // 4-point, 3rd-order B-spline (x-form)
 b  := Data[0] + Data[2];
 c[0] := COneSixth64 * (b + 4 * Data[1]);
 c[1] := CHalf32 * (Data[2] - Data[0]);
 c[2] := CHalf32 * b - Data[1];
 c[3] := CHalf32 * (Data[1] - Data[2]) + COneSixth64 * (Data[3] - Data[0]);
 result := ((c[3] * Fractional + c[2]) * Fractional + c[1]) * Fractional + c[0];
end;

end.
