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

unit DAV_Approximations;

interface

{$I DAV_Compiler.inc}
{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}
{$IFDEF CPUx86_64}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  DAV_Common, {$IFDEF FPC} LCLIntf; {$ELSE}
  Windows {$IFDEF UseNativeTypes}, Types{$ENDIF}; {$ENDIF}

function FastInvSqrt(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastInvSqrt(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSqrt(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSqrt(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSqrtBab0(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSqrtBab1(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSqrtBab2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSqrtBab0(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSqrtBab1(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSqrtBab2(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastRoot(I: Single; N: Integer): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastIntPower(I: Single; N: Integer): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower(Base, Exponent: Double): Double;
function FastLog2(Value: Single): Single; overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower2(Value: Single): Single; overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastExp(Value: Single): Single; overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastFloorLn2(Value: Single): Integer;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastArctanLike(Value: Single): Single; overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastArctanLike(Value: Double): Double; overload;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastRandomGauss: Single;
  overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function FastSinLike(Value: Single): Single; overload;
function FastSinLike(Value: Double): Double; overload;
function FastCosLike(Value: Single): Single; overload;
function FastCosLike(Value: Double): Double; overload;


{ Trigonomic Approximations }

// 3-Term: Accurate to about 3.2 decimal digits over the range [0, pi/2].
function FastCosPart3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCosPart3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCosInBounds3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCosInBounds3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCos3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCos3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSinInBounds3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSinInBounds3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSin3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSin3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSec3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSec3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCsc3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCsc3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

// 4-Term: Accurate to about 5.2 decimal digits over the range [0, pi/2].
function FastCosPart4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCosPart4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCosInBounds4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCosInBounds4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCos4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCos4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSinPart4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSinPart4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSinInBounds4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSinInBounds4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSin4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSin4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSec4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSec4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCsc4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCsc4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

// 5-Term: Accurate to about 7.3 decimal digits over the range [0, pi/2].
function FastCosPart5Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCosPart5Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCosInBounds5Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCosInBounds5Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCos5Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCos5Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSinInBounds5Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSinInBounds5Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSin5Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSin5Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSec5Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSec5Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCsc5Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCsc5Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

// 6-Term: Accurate to about ?.? decimal digits over the range [0, pi/2].
function FastCosPart6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCosPart6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCosInBounds6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCosInBounds6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCos6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCos6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSinInBounds6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSinInBounds6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSin6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSin6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSec6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSec6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCsc6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCsc6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

// FPU assembler versions
{$IFNDEF Purepascal}
function FastCosPart3TermFPU(Value: Single): Single; overload;
function FastCosPart3TermFPU(Value: Double): Double; overload;
function FastCosPart4TermFPU(Value: Single): Single; overload;
function FastCosPart4TermFPU(Value: Double): Double; overload;
function FastCosPart5TermFPU(Value: Single): Single; overload;
function FastCosPart5TermFPU(Value: Double): Double; overload;
function FastCosPart6TermFPU(Value: Single): Single; overload;
function FastCosPart6TermFPU(Value: Double): Double; overload;
{$ENDIF}

// 7-Term: Accurate to about 12.1 decimal digits over the range [0, pi/2].
function FastCosPart7Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCosPart7Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCos7Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCos7Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSin7Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSin7Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSec7Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastSec7Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCsc7Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCsc7Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

// 2-Term: Accurate to about 3.2 decimal digits over the range [0, pi/4].
function FastTanPart2Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanPart2Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanPInv2Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanPInv2Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanInBounds2Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanInBounds2Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTanInBounds2Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTanInBounds2Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTan2Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTan2Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTan2Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTan2Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

// 3-Term: Accurate to about 5.6 decimal digits over the range [0, pi/4].
function FastTanPart3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanPart3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanPInv3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanPInv3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanInBounds3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanInBounds3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTanInBounds3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTanInBounds3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTan3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTan3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTan3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTan3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

// 4-Term: Accurate to about 8.2 decimal digits over the range [0, pi/4].
function FastTanPart4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanPart4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanPInv4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanPInv4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanInBounds4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanInBounds4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTanInBounds4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTanInBounds4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTan4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTan4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTan4Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTan4Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

// 6-Term: Accurate to about 14 decimal digits over the range [0, pi/4].
function FastTanPart6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanPart6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanPInv6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanPInv6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanInBounds6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanInBounds6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTanInBounds6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTanInBounds6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTan6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTan6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTan6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastCoTan6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

// 3-Term: Accurate to about 6.6 decimal digits over the range [0, pi/12].
function FastArcTanPart3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastArcTanPart3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastArcTan3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastArcTan3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastArcCotan3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastArcCotan3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

// 6-Term: Accurate to about 13.7 decimal digits over the range [0, pi/12].
function FastArcTanPart6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastArcTanPart6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastArcTan6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastArcTan6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastArcCotan6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastArcCotan6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

// 3-Term: Accurate to about 6.6 decimal digits over the range [0, pi/12].
function FastArcCos3Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastArcCos3Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

// 6-Term: Accurate to about 13.7 decimal digits over the range [0, pi/12].
function FastArcCos6Term(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastArcCos6Term(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;


{ 2^x Approximations }

function FastPower2MinError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower2ContinousError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower2MinError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower2ContinousError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower2MinError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower2ContinousError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower2MinError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower2ContinousError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}


{ Log2 Approximations }

function FastLog2ContinousError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog2Laurent2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog2MinError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog2ContinousError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog2Laurent3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog2MinError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog2ContinousError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog2Laurent4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog2MinError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog2ContinousError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog2Laurent5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog2MinError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}


{ 10^X Approximations }

function FastPower10MinError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower10ContinousError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower10MinError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower10ContinousError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower10MinError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower10ContinousError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower10MinError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastPower10ContinousError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}


{ Exp Approximations }

function FastExpMinError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastExpContinousError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastExpMinError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastExpContinousError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastExpMinError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastExpContinousError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastExpMinError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastExpContinousError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}


{ Log10 Approximations }

function FastLog10ContinousError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog10Laurent2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog10MinError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog10ContinousError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog10Laurent3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog10MinError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog10ContinousError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog10Laurent4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog10MinError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog10ContinousError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog10Laurent5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLog10MinError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}


{ Ln Approximations }

function FastLnContinousError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLnLaurent2(Value: Single): Single; {$IFDEF SUPPORTS_INLINE} inline;
  {$ENDIF}
function FastLnMinError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLnContinousError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLnLaurent3(Value: Single): Single; {$IFDEF SUPPORTS_INLINE} inline;
  {$ENDIF}
function FastLnMinError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLnContinousError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLnLaurent4(Value: Single): Single; {$IFDEF SUPPORTS_INLINE} inline;
  {$ENDIF}
function FastLnMinError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLnContinousError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastLnLaurent5(Value: Single): Single; {$IFDEF SUPPORTS_INLINE} inline;
  {$ENDIF}
function FastLnMinError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}


{ Sqrt Approximations }

// function FastSqrtMinError2(Value: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}


{ TanH Approximations }

function FastTanhOpt3Term(Input: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanhOpt4Term(Input: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanhOpt5Term(Input: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanhOpt6Term(Input: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanhOpt7Term(Input: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanhOpt3Term(Input: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanhOpt4Term(Input: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanhOpt5Term(Input: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanhOpt6Term(Input: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastTanhOpt7Term(Input: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

{$IFNDEF PUREPASCAL}
function FastTanhOpt3TermFPU(Input: Single): Single; assembler; overload;
function FastTanhOpt4TermFPU(Input: Single): Single; assembler; overload;
function FastTanhOpt5TermFPU(Input: Single): Single; assembler; overload;
function FastTanhOpt6TermFPU(Input: Single): Single; assembler; overload;
function FastTanhOpt7TermFPU(Input: Single): Single; assembler; overload;
function FastTanhOpt3TermFPU(Input: Double): Double; assembler; overload;
function FastTanhOpt4TermFPU(Input: Double): Double; assembler; overload;
function FastTanhOpt5TermFPU(Input: Double): Double; assembler; overload;
function FastTanhOpt6TermFPU(Input: Double): Double; assembler; overload;
function FastTanhOpt7TermFPU(Input: Double): Double; assembler; overload;
{$ENDIF}
function FastTanh2Like4Term(Input: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastTanh2Like3Term(Input: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastTanh2Like2Term(Input: Single): Single;
function FastTanh2Like1Term(Input: Single): Single;

function FastTanhMinError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastTanhContinousError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastTanhMinError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastTanhContinousError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastTanhMinError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastTanhContinousError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastTanhMinError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastTanhContinousError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastdBtoAmpMinError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpMinError2(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpContinousError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpContinousError2(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpMinError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpMinError3(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpContinousError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpContinousError3(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpMinError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpMinError4(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpContinousError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpContinousError4(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpMinError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpMinError5(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpContinousError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastdBtoAmpContinousError5(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function FastAmptodBMinError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBMinError2(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBContinousError2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBContinousError2(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBLaurent2(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBLaurent2(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBMinError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBMinError3(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBContinousError3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBContinousError3(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBLaurent3(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBLaurent3(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBMinError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBMinError4(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBContinousError4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBContinousError4(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBLaurent4(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBLaurent4(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBMinError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBMinError5(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBContinousError5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBContinousError5(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBLaurent5(Value: Single): Single;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function FastAmptodBLaurent5(Value: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

var
  Ln10, Ln2, Ln2Half, Ln2Rez: Double;
  TanSixthPi32, TanTwelfthPi32: Single;
  TanSixthPi64, TanTwelfthPi64: Double;

const
  CMinusOneThird: Double = -1 / 3;
  CMinusTwoThird: Double = -2 / 3;
  CTwo32: Single = 2;
  CTwo64: Double = 2;
  CTwoDivPi32: Single = 2.0 / Pi;
  CTwoDivPi64: Double = 2.0 / Pi;
  CPiHalf32: Single = Pi * 0.5;
  CPiHalf64: Double = Pi * 0.5;
  CThreeHalfPi32: Single = 1.5 * Pi; // pi times 3/2, used in tan routines
  CThreeHalfPi64: Double = 1.5 * Pi; // pi times 3/2, used in tan routines
  CFourDivPi32: Single = 4.0 / Pi; // 4 / pi, used in tan routines
  CFourDivPi64: Double = 4.0 / Pi; // 4 / pi, used in tan routines
  CFourthPi32: Single = Pi * 0.25; // pi / 4.0, used in tan routines
  CFourthPi64: Double = Pi * 0.25; // pi / 4.0, used in tan routines
  CSixthPi32: Single = Pi / 6.0; // pi/6.0, used in atan routines
  CSixthPi64: Double = Pi / 6.0; // pi/6.0, used in atan routines
  CTwelfthPi32: Single = Pi / 12.0; // pi/12.0, used in atan routines
  CTwelfthPi64: Double = Pi / 12.0; // pi/12.0, used in atan routines
  CdBtoAmpExpGain32: Single = 1.6609640474436811739351597147447E-1;
  // 1.5051499783199059760686944736225E-2;
  CdBtoAmpExpGain64: Double = 1.6609640474436811739351597147447E-1;
  CFactor2IndB32: Single = 6.0205999132796239042747778944899;
  CFactor2IndB64: Double = 6.0205999132796239042747778944899;
  CExp32: Single = 1.4426950408889634073599246810019;
  C2Exp32: Single = 2 * 1.442695040888963407359924681;
  C1032: Single = 3.3219280948873623478703194294894;
  C21032: Single = 2 * 3.321928094887362347870319429;
  CLog2of10Inv32: Single = 0.3010299956639811952137388947245;
  CLog2ofEInv32: Single = 0.6931471805599453094172321214582;

const
  CArcTanLike: array [0 .. 4] of Single = (0.0208351, -0.085133, 0.180141,
    -0.3302995, 0.999866);
  CCos3Term: array [0 .. 2] of Single = (9.99410127468481235E-1,
    -4.95614819206541213E-1, 3.68101531561626713E-2);
  CCos4Term: array [0 .. 3] of Single = (9.99993413205793602E-1,
    -4.99913469025972335E-1, 4.14891801631523741E-2, -1.27168190108224634E-3);
  CCos5Term: array [0 .. 4] of Double = (9.99999954384273471E-1,
    -4.99999068400003466E-1, 4.16636211009559776E-2, -1.38539804095743203E-3,
    2.31603397215404142E-5);
  CCos6Term: array [0 .. 5] of Double = (9.99999999785851190E-1,
    -4.99999993711054302E-1, 4.16666367367911419E-2, -1.38883676220687841E-3,
    2.47604862741507171E-5, -2.60573608883968794E-7);
  CCos7Term: array [0 .. 6] of Double = (9.9999999999925182E-1,
    -4.9999999997024012E-1, 4.1666666473384543E-2, -1.388888418000423E-3,
    2.48010406484558E-5, -2.752469638432E-7, 1.9907856854E-9);
  CTan2Term: array [0 .. 1] of Single = (-3.6112171, -4.6133253);
  CTan3Term: array [0 .. 2] of Single = (-3.16783027, 0.134516124,
    -4.033321984);
  CTan4Term: array [0 .. 3] of Double = (211.849369664121, -12.5288887278448,
    269.7350131214121, -71.4145309347748);
  CTan5Term: array [0 .. 4] of Double = (8.38820505317477848E+22,
    -3.70665302360166393E+21, -5.32928201354683187E+19, 1.06801947663613863E+23,
    -2.66798401001070002E+22);
  CTan6Term: array [0 .. 5] of Double = (-34287.4662577359568109624,
    2566.7175462315050423295, -26.5366371951731325438,
    -43656.1579281292375769579, 12244.4839556747426927793,
    -336.611376245464339493);
  CArcTan3Term: array [0 .. 2] of Single = (1.6867629106, 0.4378497304,
    1.6867633134);
  CArcTan6Term: array [0 .. 5] of Double = (48.70107004404898384,
    49.5326263772254345, 9.40604244231624, 48.70107004404996166,
    65.7663163908956299, 21.587934067020262);
  CP2MinError2: array [0 .. 1] of Single = (7.02679339377207945E-1,
    2.39338555345344262E-1);
  CP2ContError2: array [0 .. 1] of Single = (7.07990673676189286E-1,
    2.47944580878438597E-1);
  CP2MinError3: array [0 .. 2] of Single = (6.93292707161004662E-1,
    2.42162975514835621E-1, 5.48668824216034384E-2);
  CP2ContError3: array [0 .. 2] of Single = (6.93282526441610814E-1,
    2.42201488582370950E-1, 5.50043626970249666E-2);
  CP2MinError4: array [0 .. 3] of Single = (6.93125327471890484E-1,
    2.40243955446532431E-1, 5.58964584033713671E-2, 9.56014434382608004E-3);
  CP2ContError4: array [0 .. 3] of Single = (6.93118326815805763E-1,
    2.40239617833581720E-1, 5.59538423222786241E-2, 9.60540553453761992E-3);
  CP2MinError5: array [0 .. 4] of Single = (6.93146707655684646E-1,
    2.40222758408825315E-1, 5.55115371076677494E-2, 9.66918194816469324E-3,
    1.31179782896480692E-3);
  CP2ContError5: array [0 .. 4] of Single = (6.93147084150589565E-1,
    2.40221342016562145E-1, 5.55055126727891784E-2, 9.67692162036261003E-3,
    1.33355654772594713E-3);
  CL2MinError2: array [0 .. 1] of Single = (1.00055782634514956,
    4.23544952666627533E-2);
  CL2Laurent2: array [0 .. 1] of Single = (1.00000009294157932,
    -8.24519535454190642E-8);
  CL2Continous2: array [0 .. 1] of Single = (1.00011486779516678,
    3.26835576187857176E-4);
  CL2MinError3: array [0 .. 2] of Single = (-3.45237616924014556E-1,
    2.02572339392057543, -6.75567209748426434E-1);
  CL2Continous3: array [0 .. 2] of Single = (-3.33191603749037668E-1,
    1.99957454862186501, -6.64176001948231232E-1);
  CL2Laurent3: array [0 .. 2] of Single = (-3.46544729609133795E-1,
    2.03963383654773933, -6.93089382045648295E-1);
  CL2MinError4: array [0 .. 3] of Single = (1.58518682965714114E-1,
    -1.05301320934821097, 3.04944518136610121, -1.15431731153602279);
  CL2Continous4: array [0 .. 3] of Single = (1.63659707946391092E-1,
    -1.09661081181213116, 3.14421441381158484, -1.21126297836588193);
  CL2Laurent4: array [0 .. 3] of Single = (1.59220899692695511E-1,
    -1.05974853905456978, 3.06469939326067076, -1.16417164373658544);
  CL2MinError5: array [0 .. 4] of Single = (-8.18038640187952054E-2,
    6.46216635143615381E-1, -2.12293700635511007, 4.07217052527789480,
    -1.51355930430330177);
  CL2Continous5: array [0 .. 4] of Single = (-8.21343513178931783E-2,
    6.49732456739820052E-1, -2.13417801862571777, 4.08642207062728868,
    -1.51984215742349793);
  CL2Laurent5: array [0 .. 4] of Single = (-8.00848677328682978E-2,
    6.38108601387251673E-1, -2.11019449052551389, 4.06509622185509922,
    -1.51292537160088569);
  CGaussScaledLog2ofEInv32: Single = -4.3321698784996581838577007591125E-2;

implementation

uses
  Math, SysUtils, DAV_Math;

{$WARNINGS OFF}

{ Trigonomic Approximations }

// 3-Term: Accurate to about 3.2 decimal digits over the range [0, pi/2].

function FastCosPart3Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := CCos3Term[0] + Result * (CCos3Term[1] + CCos3Term[2] * Result);
end;

function FastCosPart3Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := CCos3Term[0] + Result * (CCos3Term[1] + CCos3Term[2] * Result);
end;

{$IFNDEF Purepascal}

function FastCosPart3TermFPU(Value: Single): Single;
asm
  FLD     Value
  FMUL    ST(0), ST(0)                // Value²
  FLD     [CCos3Term + 4 * 2].Single  // CCos3Term[2], Value²
  FMUL    ST(0), ST(1)                // Value² * CCos3Term[2], Value²
  FADD    [CCos3Term + 4 * 1].Single  // ...
  FMULP
  FADD    [CCos3Term].Single
end;

function FastCosPart3TermFPU(Value: Double): Double;
asm
  FLD     Value
  FMUL    ST(0), ST(0)                // Value²
  FLD     [CCos3Term + 4 * 2].Single  // CCos3Term[2], Value²
  FMUL    ST(0), ST(1)                // Value² * CCos3Term[2], Value²
  FADD    [CCos3Term + 4 * 1].Single  // ...
  FMULP
  FADD    [CCos3Term].Single
end;
{$ENDIF}

function FastCosInBounds3Term(Value: Single): Single;
begin
  case Round(Value * CTwoDivPi32 - CHalf32) of
    0:
      Result := FastCosPart3Term(Value);
    1:
      Result := -FastCosPart3Term(Pi - Value);
    2:
      Result := -FastCosPart3Term(Value - Pi);
    3:
      Result := FastCosPart3Term(CTwoPI32 - Value);
  else
    Result := FastCosPart3Term(Value);
  end;
end;

function FastCosInBounds3Term(Value: Double): Double;
begin
  case Round(Value * CTwoDivPi64 - CHalf64) of
    0:
      Result := FastCosPart3Term(Value);
    1:
      Result := -FastCosPart3Term(Pi - Value);
    2:
      Result := -FastCosPart3Term(Value - Pi);
    3:
      Result := FastCosPart3Term(CTwoPI64 - Value);
  else
    Result := FastCosPart3Term(Value);
  end;
end;

function FastCos3Term(Value: Single): Single;
begin
  // Get rid of values > 2 * pi
  Result := Abs(FastMod(Value, CTwoPi32));
  Result := FastCosInBounds3Term(Result);
end;

function FastCos3Term(Value: Double): Double;
begin
  // Get rid of values > 2 * pi
  Result := FastCosInBounds3Term(Abs(FastMod(Value, CTwoPi64)));
end;

function FastSinInBounds3Term(Value: Single): Single;
begin
  Result := FastCosInBounds3Term(CPiHalf32 - Value);
end;

function FastSinInBounds3Term(Value: Double): Double;
begin
  Result := FastCosInBounds3Term(CPiHalf64 - Value);
end;

function FastSin3Term(Value: Single): Single;
begin
  Result := FastCos3Term(CPiHalf32 - Value);
end;

function FastSin3Term(Value: Double): Double;
begin
  Result := FastCos3Term(CPiHalf64 - Value);
end;

function FastSec3Term(Value: Single): Single;
begin
  Result := 1 / FastCos3Term(Value);
end;

function FastSec3Term(Value: Double): Double;
begin
  Result := 1 / FastCos3Term(Value);
end;

function FastCsc3Term(Value: Single): Single;
begin
  Result := 1 / FastCos3Term(CPiHalf32 - Value);
end;

function FastCsc3Term(Value: Double): Double;
begin
  Result := 1 / FastCos3Term(CPiHalf64 - Value);
end;


// 4-Term: Accurate to about 5.2 decimal digits over the range [0, pi/2].

function FastCosPart4Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := CCos4Term[0] + Result *
    (CCos4Term[1] + Result * (CCos4Term[2] + CCos4Term[3] * Result));
end;

function FastCosPart4Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := CCos4Term[0] + Result *
    (CCos4Term[1] + Result * (CCos4Term[2] + CCos4Term[3] * Result));
end;

{$IFNDEF Purepascal}

function FastCosPart4TermFPU(Value: Single): Single;
asm
  FLD     Value
  FMUL    ST(0), ST(0)                // Value²
  FLD     [CCos4Term + 4 * 3].Single  // CCos4Term[3], Value²
  FMUL    ST(0), ST(1)                // Value² * CCos4Term[3], Value²
  FADD    [CCos4Term + 4 * 2].Single  // ...
  FMUL    ST(0), ST(1)
  FADD    [CCos4Term + 4 * 1].Single
  FMULP
  FADD    [CCos4Term].Single
end;

function FastCosPart4TermFPU(Value: Double): Double;
asm
  FLD     Value
  FMUL    ST(0), ST(0)                // Value²
  FLD     [CCos4Term + 4 * 3].Single  // CCos4Term[3], Value²
  FMUL    ST(0), ST(1)                // Value² * CCos4Term[3], Value²
  FADD    [CCos4Term + 4 * 2].Single  // ...
  FMUL    ST(0), ST(1)
  FADD    [CCos4Term + 4 * 1].Single
  FMULP
  FADD    [CCos4Term].Single
end;
{$ENDIF}

function FastCosInBounds4Term(Value: Single): Single;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
begin
  case Round(Value * CTwoDivPi32 - CHalf32) of
    0:
      Result := FastCosPart4Term(Value);
    1:
      Result := -FastCosPart4Term(Pi - Value);
    2:
      Result := -FastCosPart4Term(Value - Pi);
    3:
      Result := FastCosPart4Term(CTwoPI32 - Value);
  else
    Result := FastCosPart4Term(Value);
  end;
end;

function FastCosInBounds4Term(Value: Double): Double;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
begin
  case Round(Value * CTwoDivPi64 - CHalf64) of
    0:
      Result := FastCosPart4Term(Value);
    1:
      Result := -FastCosPart4Term(Pi - Value);
    2:
      Result := -FastCosPart4Term(Value - Pi);
    3:
      Result := FastCosPart4Term(CTwoPI64 - Value);
  else
    Result := FastCosPart4Term(Value);
  end;
end;

function FastCos4Term(Value: Single): Single;
begin
  // Get rid of values > 2 * pi
  Result := Abs(FastMod(Value, CTwoPi32));
  Result := FastCosInBounds4Term(Result);
end;

function FastCos4Term(Value: Double): Double;
begin
  // Get rid of values > 2 * pi
  Result := FastCosInBounds4Term(Abs(FastMod(Value, CTwoPi64)));
end;

function FastSinPart4Term(Value: Single): Single;
begin
  Result := FastCosPart4Term(CPiHalf32 - Value);
end;

function FastSinPart4Term(Value: Double): Double;
begin
  Result := FastCosPart4Term(CPiHalf64 - Value);
end;

function FastSinInBounds4Term(Value: Single): Single;
begin
  Result := FastCosInBounds4Term(CPiHalf32 - Value);
end;

function FastSinInBounds4Term(Value: Double): Double;
begin
  Result := FastCosInBounds4Term(CPiHalf64 - Value);
end;

function FastSin4Term(Value: Single): Single;
begin
  Result := FastCos4Term(CPiHalf32 - Value);
end;

function FastSin4Term(Value: Double): Double;
begin
  Result := FastCos4Term(CPiHalf64 - Value);
end;

function FastSec4Term(Value: Single): Single;
begin
  Result := 1 / FastCos4Term(Value);
end;

function FastSec4Term(Value: Double): Double;
begin
  Result := 1 / FastCos4Term(Value);
end;

function FastCsc4Term(Value: Single): Single;
begin
  Result := 1 / FastCos4Term(CPiHalf32 - Value);
end;

function FastCsc4Term(Value: Double): Double;
begin
  Result := 1 / FastCos4Term(CPiHalf64 - Value);
end;


// 5-Term: Accurate to about 7.3 decimal digits over the range [0, pi/2].

function FastCosPart5Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := CCos5Term[0] + Result *
    (CCos5Term[1] + Result * (CCos5Term[2] + Result *
    (CCos5Term[3] + CCos5Term[4] * Result)));
end;

function FastCosPart5Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := CCos5Term[0] + Result *
    (CCos5Term[1] + Result * (CCos5Term[2] + Result *
    (CCos5Term[3] + CCos5Term[4] * Result)));
end;

{$IFNDEF Purepascal}

function FastCosPart5TermFPU(Value: Single): Single;
asm
  FLD     Value
  FMUL    ST(0), ST(0)                // Value²
  FLD     [CCos5Term + 4 * 4].Single  // CCos5Term[4], Value²
  FMUL    ST(0), ST(1)                // Value² * CCos5Term[4], Value²
  FADD    [CCos5Term + 4 * 3].Single  // ...
  FMUL    ST(0), ST(1)
  FADD    [CCos5Term + 4 * 2].Single
  FMUL    ST(0), ST(1)
  FADD    [CCos5Term + 4 * 1].Single
  FMULP
  FADD    [CCos5Term].Single
end;

function FastCosPart5TermFPU(Value: Double): Double;
asm
  FLD     Value
  FMUL    ST(0), ST(0)                // Value²
  FLD     [CCos5Term + 4 * 4].Single  // CCos5Term[4], Value²
  FMUL    ST(0), ST(1)                // Value² * CCos5Term[4], Value²
  FADD    [CCos5Term + 4 * 3].Single  // ...
  FMUL    ST(0), ST(1)
  FADD    [CCos5Term + 4 * 2].Single
  FMUL    ST(0), ST(1)
  FADD    [CCos5Term + 4 * 1].Single
  FMULP
  FADD    [CCos5Term].Single
end;
{$ENDIF}

function FastCosInBounds5Term(Value: Single): Single;
begin
  case Round(Value * CTwoDivPi32 - CHalf32) of
    0:
      Result := FastCosPart5Term(Value);
    1:
      Result := -FastCosPart5Term(Pi - Value);
    2:
      Result := -FastCosPart5Term(Value - Pi);
    3:
      Result := FastCosPart5Term(CTwoPI32 - Value);
    4:
      Result := FastCosPart5Term(Value);
  else
    Result := 0;
  end;
end;

function FastCosInBounds5Term(Value: Double): Double;
begin
  case Round(Value * CTwoDivPi64 - CHalf64) of
    0:
      Result := FastCosPart5Term(Value);
    1:
      Result := -FastCosPart5Term(Pi - Value);
    2:
      Result := -FastCosPart5Term(Value - Pi);
    3:
      Result := FastCosPart5Term(CTwoPI64 - Value);
    4:
      Result := FastCosPart5Term(Value);
  else
    Result := 0;
  end;
end;

function FastCos5Term(Value: Single): Single;
begin
  // Get rid of values > 2 * pi
  Result := Abs(FastMod(Value, CTwoPi32));
  Result := FastCosInBounds5Term(Result);
end;

function FastCos5Term(Value: Double): Double;
begin
  // Get rid of values > 2 * pi
  Result := FastCosInBounds5Term(Abs(FastMod(Value, CTwoPi64)));
end;

function FastSinInBounds5Term(Value: Single): Single;
begin
  Result := FastCosInBounds5Term(CPiHalf32 - Value);
end;

function FastSinInBounds5Term(Value: Double): Double;
begin
  Result := FastCosInBounds5Term(CPiHalf64 - Value);
end;

function FastSin5Term(Value: Single): Single;
begin
  Result := FastCos5Term(CPiHalf32 - Value);
end;

function FastSin5Term(Value: Double): Double;
begin
  Result := FastCos5Term(CPiHalf64 - Value);
end;

function FastSec5Term(Value: Single): Single;
begin
  Result := 1 / FastCos5Term(Value);
end;

function FastSec5Term(Value: Double): Double;
begin
  Result := 1 / FastCos5Term(Value);
end;

function FastCsc5Term(Value: Single): Single;
begin
  Result := 1 / FastCos5Term(CPiHalf32 - Value);
end;

function FastCsc5Term(Value: Double): Double;
begin
  Result := 1 / FastCos5Term(CPiHalf64 - Value);
end;


// 6-Term: Accurate to about 7.3 decimal digits over the range [0, pi/2].

function FastCosPart6Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := CCos6Term[0] + Result *
    (CCos6Term[1] + Result * (CCos6Term[2] + Result * (CCos6Term[3] + Result *
    (CCos6Term[4] + CCos6Term[5] * Result))));
end;

function FastCosPart6Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := CCos6Term[0] + Result *
    (CCos6Term[1] + Result * (CCos6Term[2] + Result * (CCos6Term[3] + Result *
    (CCos6Term[4] + CCos6Term[5] * Result))));
end;

{$IFNDEF Purepascal}

function FastCosPart6TermFPU(Value: Single): Single;
asm
  FLD     Value
  FMUL    ST(0), ST(0)                // Value²
  FLD     [CCos6Term + 8 * 6].Double  // CCos6Term[5], Value²
  FMUL    ST(0), ST(1)                // Value² * CCos6Term[5], Value²
  FADD    [CCos6Term + 8 * 5].Double  // ...
  FMUL    ST(0), ST(1)
  FADD    [CCos6Term + 8 * 4].Double
  FMUL    ST(0), ST(1)
  FADD    [CCos6Term + 8 * 3].Double
  FMUL    ST(0), ST(1)
  FADD    [CCos6Term + 8 * 2].Double
  FMUL    ST(0), ST(1)
  FADD    [CCos6Term + 8 * 1].Double
  FMULP
  FADD    [CCos6Term + 8].Double
end;

function FastCosPart6TermFPU(Value: Double): Double;
asm
  FLD     Value
  FMUL    ST(0), ST(0)                // Value²
  FLD     [CCos6Term + 8 * 6].Double  // CCos6Term[5], Value²
  FMUL    ST(0), ST(1)                // Value² * CCos6Term[5], Value²
  FADD    [CCos6Term + 8 * 5].Double  // ...
  FMUL    ST(0), ST(1)
  FADD    [CCos6Term + 8 * 4].Double
  FMUL    ST(0), ST(1)
  FADD    [CCos6Term + 8 * 3].Double
  FMUL    ST(0), ST(1)
  FADD    [CCos6Term + 8 * 2].Double
  FMUL    ST(0), ST(1)
  FADD    [CCos6Term + 8 * 1].Double
  FMULP
  FADD   [CCos6Term + 8].Double
end;
{$ENDIF}

function FastCosInBounds6Term(Value: Single): Single;
begin
  case Round(Value * CTwoDivPi32 - CHalf32) of
    0:
      Result := FastCosPart6Term(Value);
    1:
      Result := -FastCosPart6Term(Pi - Value);
    2:
      Result := -FastCosPart6Term(Value - Pi);
    3:
      Result := FastCosPart6Term(CTwoPI32 - Value);
    4:
      Result := FastCosPart6Term(Value);
  else
    Result := 0;
  end;
end;

function FastCosInBounds6Term(Value: Double): Double;
begin
  case Round(Value * CTwoDivPi64 - CHalf64) of
    0:
      Result := FastCosPart6Term(Value);
    1:
      Result := -FastCosPart6Term(Pi - Value);
    2:
      Result := -FastCosPart6Term(Value - Pi);
    3:
      Result := FastCosPart6Term(CTwoPI64 - Value);
    4:
      Result := FastCosPart6Term(Value);
  else
    Result := 0;
  end;
end;

function FastCos6Term(Value: Single): Single;
begin
  Result := Abs(FastMod(Value, CTwoPi32)); // Get rid of values > 2 * pi
  Result := FastCosInBounds6Term(Result);
end;

function FastCos6Term(Value: Double): Double;
begin
  Result := Abs(FastMod(Value, CTwoPi64)); // Get rid of values > 2 * pi
  Result := FastCosInBounds6Term(Result);
end;

function FastSinInBounds6Term(Value: Single): Single;
begin
  Result := FastCosInBounds6Term(CPiHalf32 - Value);
end;

function FastSinInBounds6Term(Value: Double): Double;
begin
  Result := FastCosInBounds6Term(CPiHalf64 - Value);
end;

function FastSin6Term(Value: Single): Single;
begin
  Result := FastCos6Term(CPiHalf32 - Value);
end;

function FastSin6Term(Value: Double): Double;
begin
  Result := FastCos6Term(CPiHalf64 - Value);
end;

function FastSec6Term(Value: Single): Single;
begin
  Result := 1 / FastCos6Term(Value);
end;

function FastSec6Term(Value: Double): Double;
begin
  Result := 1 / FastCos6Term(Value);
end;

function FastCsc6Term(Value: Single): Single;
begin
  Result := 1 / FastCos6Term(CPiHalf32 - Value);
end;

function FastCsc6Term(Value: Double): Double;
begin
  Result := 1 / FastCos6Term(CPiHalf64 - Value);
end;


// 7-Term: Accurate to about 12.1 decimal digits over the range [0, pi/2].

function FastCosPart7Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := CCos7Term[0] + Result *
    (CCos7Term[1] + Result * (CCos7Term[2] + Result * (CCos7Term[3] + Result *
    (CCos7Term[4] + Result * (CCos7Term[5] + CCos7Term[6] * Result)))));
end;

function FastCosPart7Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := CCos7Term[0] + Result *
    (CCos7Term[1] + Result * (CCos7Term[2] + Result * (CCos7Term[3] + Result *
    (CCos7Term[4] + Result * (CCos7Term[5] + CCos7Term[6] * Result)))));
end;

function FastCos7Term(Value: Single): Single;
begin
  Result := Abs(FastMod(Value, CTwoPi32)); // Get rid of values > 2 * pi
  case Round(Result * CTwoDivPi32 - CHalf32) of
    0:
      Result := FastCosPart7Term(Result);
    1:
      Result := -FastCosPart7Term(Pi - Result);
    2:
      Result := -FastCosPart7Term(Result - Pi);
    3:
      Result := FastCosPart7Term(CTwoPI32 - Result);
    4:
      Result := FastCosPart7Term(Result);
  end;
end;

function FastCos7Term(Value: Double): Double;
begin
  Result := Abs(FastMod(Value, CTwoPi64)); // Get rid of values > 2 * pi
  case Round(Result * CTwoDivPi64 - CHalf64) of
    0:
      Result := FastCosPart7Term(Result);
    1:
      Result := -FastCosPart7Term(Pi - Result);
    2:
      Result := -FastCosPart7Term(Result - Pi);
    3:
      Result := FastCosPart7Term(CTwoPI64 - Result);
    4:
      Result := FastCosPart7Term(Result);
  end;
end;

function FastSin7Term(Value: Single): Single;
begin
  Result := FastCos7Term(CPiHalf32 - Value);
end;

function FastSin7Term(Value: Double): Double;
begin
  Result := FastCos7Term(CPiHalf64 - Value);
end;

function FastSec7Term(Value: Single): Single;
begin
  Result := 1 / FastCos7Term(Value);
end;

function FastSec7Term(Value: Double): Double;
begin
  Result := 1 / FastCos7Term(Value);
end;

function FastCsc7Term(Value: Single): Single;
begin
  Result := 1 / FastCos7Term(CPiHalf32 - Value);
end;

function FastCsc7Term(Value: Double): Double;
begin
  Result := 1 / FastCos7Term(CPiHalf64 - Value);
end;


// 2-Term: Accurate to about 3.2 decimal digits over the range [0, pi/4].

function FastTanPart2Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := Value * CTan2Term[0] / (CTan2Term[1] + Result);
end;

function FastTanPart2Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := Value * CTan2Term[0] / (CTan2Term[1] + Result);
end;

function FastTanPInv2Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := (CTan2Term[1] + Result) / (Value * CTan2Term[0]);
end;

function FastTanPInv2Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := (CTan2Term[1] + Result) / (Value * CTan2Term[0]);
end;

function FastTanInBounds2Term(Value: Single): Single;
begin
  case Round(Value * CFourDivPi32) of
    0:
      Result := FastTanPart2Term(Value * CFourDivPi32);
    1:
      Result := FastTanPInv2Term((CPiHalf32 - Value) * CFourDivPi32);
    2:
      Result := -FastTanPInv2Term((Value - CPiHalf32) * CFourDivPi32);
    3:
      Result := -FastTanPart2Term((Pi - Value) * CFourDivPi32);
    4:
      Result := FastTanPart2Term((Value - Pi) * CFourDivPi32);
    5:
      Result := FastTanPInv2Term((CThreeHalfPi32 - Value) * CFourDivPi32);
    6:
      Result := -FastTanPInv2Term((Value - CThreeHalfPi32) * CFourDivPi32);
    7:
      Result := -FastTanPart2Term((CTwo32 - Value) * CFourDivPi32);
  else
    Result := FastTanPart2Term(Value * CFourDivPi32);
  end;
end;

function FastTanInBounds2Term(Value: Double): Double;
begin
  case Round(Value * CFourDivPi64) of
    0:
      Result := FastTanPart2Term(Value * CFourDivPi64);
    1:
      Result := FastTanPInv2Term((CPiHalf64 - Value) * CFourDivPi64);
    2:
      Result := -FastTanPInv2Term((Value - CPiHalf64) * CFourDivPi64);
    3:
      Result := -FastTanPart2Term((Pi - Value) * CFourDivPi64);
    4:
      Result := FastTanPart2Term((Value - Pi) * CFourDivPi64);
    5:
      Result := FastTanPInv2Term((CThreeHalfPi64 - Value) * CFourDivPi64);
    6:
      Result := -FastTanPInv2Term((Value - CThreeHalfPi64) * CFourDivPi64);
    7:
      Result := -FastTanPart2Term((CTwo64 - Value) * CFourDivPi64);
  else
    Result := FastTanPart2Term(Value * CFourDivPi64);
  end;
end;

function FastCotanInBounds2Term(Value: Single): Single;
begin
  Result := -FastTanInBounds2Term(CPiHalf32 - Value);
end;

function FastCotanInBounds2Term(Value: Double): Double;
begin
  Result := -FastTanInBounds2Term(CPiHalf64 - Value);
end;

// WARNING: We do not test for the tangent approaching infinity,
// which it will at x = pi / 2 and x = 3 * pi / 2. If this is a problem
// in your application, take appropriate action.

function FastTan2Term(Value: Single): Single;
begin
  Result := Abs(FastMod(Value, CTwoPi32)); // Get rid of values > 2 * pi
  Result := FastTanInBounds2Term(Result);
end;

function FastTan2Term(Value: Double): Double;
begin
  Result := FastTanInBounds2Term(Abs(FastMod(Value, CTwoPi64)));
end;

function FastCotan2Term(Value: Single): Single;
begin
  Result := -FastTan2Term(CPiHalf32 - Value);
end;

function FastCotan2Term(Value: Double): Double;
begin
  Result := -FastTan2Term(CPiHalf64 - Value);
end;


// 3-Term: Accurate to about 5.6 decimal digits over the range [0, pi/4].

function FastTanPart3Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := Value * (CTan3Term[0] + CTan3Term[1] * Result) /
    (CTan3Term[2] + Result);
end;

function FastTanPart3Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := Value * (CTan3Term[0] + CTan3Term[1] * Result) /
    (CTan3Term[2] + Result);
end;

function FastTanPInv3Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := (CTan3Term[2] + Result) /
    (Value * (CTan3Term[0] + CTan3Term[1] * Result));
end;

function FastTanPInv3Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := (CTan3Term[2] + Result) /
    (Value * (CTan3Term[0] + CTan3Term[1] * Result));
end;

function FastTanInBounds3Term(Value: Single): Single;
begin
  case Round(Value * CFourDivPi32) of
    0:
      Result := FastTanPart3Term(Value * CFourDivPi32);
    1:
      Result := FastTanPInv3Term((CPiHalf32 - Value) * CFourDivPi32);
    2:
      Result := -FastTanPInv3Term((Value - CPiHalf32) * CFourDivPi32);
    3:
      Result := -FastTanPart3Term((Pi - Value) * CFourDivPi32);
    4:
      Result := FastTanPart3Term((Value - Pi) * CFourDivPi32);
    5:
      Result := FastTanPInv3Term((CThreeHalfPi32 - Value) * CFourDivPi32);
    6:
      Result := -FastTanPInv3Term((Value - CThreeHalfPi32) * CFourDivPi32);
    7:
      Result := -FastTanPart3Term((CTwo32 - Value) * CFourDivPi32);
  else
    Result := FastTanPart3Term(Value * CFourDivPi32);
  end;
end;

function FastTanInBounds3Term(Value: Double): Double;
begin
  Result := Abs(FastMod(Value, CTwoPi64)); // Get rid of values > 2 * pi
  case Round(Result * CFourDivPi64) of
    0:
      Result := FastTanPart3Term(Value * CFourDivPi64);
    1:
      Result := FastTanPInv3Term((CPiHalf64 - Value) * CFourDivPi64);
    2:
      Result := -FastTanPInv3Term((Value - CPiHalf64) * CFourDivPi64);
    3:
      Result := -FastTanPart3Term((Pi - Value) * CFourDivPi64);
    4:
      Result := FastTanPart3Term((Value - Pi) * CFourDivPi64);
    5:
      Result := FastTanPInv3Term((CThreeHalfPi64 - Value) * CFourDivPi64);
    6:
      Result := -FastTanPInv3Term((Value - CThreeHalfPi64) * CFourDivPi64);
    7:
      Result := -FastTanPart3Term((CTwo64 - Value) * CFourDivPi64);
  else
    Result := FastTanPart3Term(Value * CFourDivPi64);
  end;
end;

function FastCotanInBounds3Term(Value: Single): Single;
begin
  Result := -FastTanInBounds3Term(CPiHalf32 - Value);
end;

function FastCotanInBounds3Term(Value: Double): Double;
begin
  Result := -FastTanInBounds3Term(CPiHalf64 - Value);
end;

// WARNING: We do not test for the tangent approaching infinity,
// which it will at x=pi/2 and x=3*pi/2. If this is a problem
// in your application, take appropriate action.

function FastTan3Term(Value: Single): Single;
begin
  // Get rid of values > 2 * pi
  Result := Abs(FastMod(Value, CTwoPi32));
  Result := FastTanInBounds3Term(Result);
end;

function FastTan3Term(Value: Double): Double;
begin
  // Get rid of values > 2 * pi
  Result := FastTanInBounds3Term(Abs(FastMod(Value, CTwoPi64)));
end;

function FastCotan3Term(Value: Single): Single;
begin
  Result := -FastTan3Term(CPiHalf32 - Value);
end;

function FastCotan3Term(Value: Double): Double;
begin
  Result := -FastTan3Term(CPiHalf64 - Value);
end;


// 4-Term: Accurate to about 8.2 decimal digits over the range [0, pi/4].

function FastTanPart4Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := Value * (CTan4Term[0] + CTan4Term[1] * Result) /
    (CTan4Term[2] + Result * (CTan4Term[3] + Result));
end;

function FastTanPart4Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := Value * (CTan4Term[0] + CTan4Term[1] * Result) /
    (CTan4Term[2] + Result * (CTan4Term[3] + Result));
end;

function FastTanPInv4Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := (CTan4Term[2] + Result * (CTan4Term[3] + Result)) /
    (Value * (CTan4Term[0] + CTan4Term[1] * Result));
end;

function FastTanPInv4Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := (CTan4Term[2] + Result * (CTan4Term[3] + Result)) /
    (Value * (CTan4Term[0] + CTan4Term[1] * Result));
end;

function FastTanInBounds4Term(Value: Single): Single;
begin
  case Round(Value * CFourDivPi32) of
    0:
      Result := FastTanPart4Term(Value * CFourDivPi32);
    1:
      Result := FastTanPInv4Term((CPiHalf32 - Value) * CFourDivPi32);
    2:
      Result := -FastTanPInv4Term((Value - CPiHalf32) * CFourDivPi32);
    3:
      Result := -FastTanPart4Term((Pi - Value) * CFourDivPi32);
    4:
      Result := FastTanPart4Term((Value - Pi) * CFourDivPi32);
    5:
      Result := FastTanPInv4Term((CThreeHalfPi32 - Value) * CFourDivPi32);
    6:
      Result := -FastTanPInv4Term((Value - CThreeHalfPi32) * CFourDivPi32);
    7:
      Result := -FastTanPart4Term((CTwo32 - Value) * CFourDivPi32);
  else
    Result := FastTanPart4Term(Value * CFourDivPi32);
  end;
end;

function FastTanInBounds4Term(Value: Double): Double;
begin
  case Round(Value * CFourDivPi64) of
    0:
      Result := FastTanPart4Term(Value * CFourDivPi64);
    1:
      Result := FastTanPInv4Term((CPiHalf64 - Value) * CFourDivPi64);
    2:
      Result := -FastTanPInv4Term((Value - CPiHalf64) * CFourDivPi64);
    3:
      Result := -FastTanPart4Term((Pi - Value) * CFourDivPi64);
    4:
      Result := FastTanPart4Term((Value - Pi) * CFourDivPi64);
    5:
      Result := FastTanPInv4Term((CThreeHalfPi64 - Value) * CFourDivPi64);
    6:
      Result := -FastTanPInv4Term((Value - CThreeHalfPi64) * CFourDivPi64);
    7:
      Result := -FastTanPart4Term((CTwo64 - Value) * CFourDivPi64);
  else
    Result := FastTanPart4Term(Value * CFourDivPi64);
  end;
end;

function FastCotanInBounds4Term(Value: Single): Single;
begin
  Result := -FastTanInBounds4Term(CPiHalf32 - Value);
end;

function FastCotanInBounds4Term(Value: Double): Double;
begin
  Result := -FastTanInBounds4Term(CPiHalf64 - Value);
end;

// WARNING: We do not test for the tangent approaching infinity,
// which it will at x=pi/2 and x=3*pi/2. If this is a problem
// in your application, take appropriate action.

function FastTan4Term(Value: Single): Single;
begin
  Result := Abs(FastMod(Value, CTwoPi32)); // Get rid of values > 2 * pi
  Result := FastTanInBounds4Term(Result);
end;

function FastTan4Term(Value: Double): Double;
begin
  Result := FastTanInBounds4Term(Abs(FastMod(Value, CTwoPi64)));
  // Get rid of values > 2 * pi
end;

function FastCotan4Term(Value: Single): Single;
begin
  Result := -FastTan4Term(CPiHalf32 - Value);
end;

function FastCotan4Term(Value: Double): Double;
begin
  Result := -FastTan4Term(CPiHalf64 - Value);
end;


// 6-Term: Accurate to about 14 decimal digits over the range [0, pi/4].

function FastTanPart6Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := Value * (CTan6Term[0] + Result * (CTan6Term[1] + Result *
    CTan6Term[2])) / (CTan6Term[3] + Result * (CTan6Term[4] + Result *
    (CTan6Term[5] + Result)));
end;

function FastTanPart6Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := Value * (CTan6Term[0] + Result * (CTan6Term[1] + Result *
    CTan6Term[2])) / (CTan6Term[3] + Result * (CTan6Term[4] + Result *
    (CTan6Term[5] + Result)));
end;

function FastTanPInv6Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := (CTan6Term[3] + Result * (CTan6Term[4] + Result *
    (CTan6Term[5] + Result))) /
    (Value * (CTan6Term[0] + Result * (CTan6Term[1] + Result * CTan6Term[2])));
end;

function FastTanPInv6Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := (CTan6Term[3] + Result * (CTan6Term[4] + Result *
    (CTan6Term[5] + Result))) /
    (Value * (CTan6Term[0] + Result * (CTan6Term[1] + Result * CTan6Term[2])));
end;

function FastTanInBounds6Term(Value: Single): Single;
begin
  case Round(Value * CFourDivPi32) of
    0:
      Result := FastTanPart6Term(Value * CFourDivPi32);
    1:
      Result := FastTanPInv6Term((CPiHalf32 - Value) * CFourDivPi32);
    2:
      Result := -FastTanPInv6Term((Value - CPiHalf32) * CFourDivPi32);
    3:
      Result := -FastTanPart6Term((Pi - Value) * CFourDivPi32);
    4:
      Result := FastTanPart6Term((Value - Pi) * CFourDivPi32);
    5:
      Result := FastTanPInv6Term((CThreeHalfPi32 - Value) * CFourDivPi32);
    6:
      Result := -FastTanPInv6Term((Value - CThreeHalfPi32) * CFourDivPi32);
    7:
      Result := -FastTanPart6Term((CTwo32 - Value) * CFourDivPi32);
  else
    Result := FastTanPart6Term(Value * CFourDivPi32);
  end;
end;

function FastTanInBounds6Term(Value: Double): Double;
begin
  case Round(Value * CFourDivPi64) of
    0:
      Result := FastTanPart6Term(Value * CFourDivPi64);
    1:
      Result := FastTanPInv6Term((CPiHalf64 - Value) * CFourDivPi64);
    2:
      Result := -FastTanPInv6Term((Value - CPiHalf64) * CFourDivPi64);
    3:
      Result := -FastTanPart6Term((Pi - Value) * CFourDivPi64);
    4:
      Result := FastTanPart6Term((Value - Pi) * CFourDivPi64);
    5:
      Result := FastTanPInv6Term((CThreeHalfPi64 - Value) * CFourDivPi64);
    6:
      Result := -FastTanPInv6Term((Value - CThreeHalfPi64) * CFourDivPi64);
    7:
      Result := -FastTanPart6Term((CTwo64 - Value) * CFourDivPi64);
  else
    Result := FastTanPart6Term(Value * CFourDivPi64);
  end;
end;

function FastCotanInBounds6Term(Value: Single): Single;
begin
  Result := -FastTanInBounds6Term(CPiHalf32 - Value);
end;

function FastCotanInBounds6Term(Value: Double): Double;
begin
  Result := -FastTanInBounds6Term(CPiHalf64 - Value);
end;

// WARNING: We do not test for the tangent approaching infinity,
// which it will at x=pi/2 and x=3*pi/2. If this is a problem
// in your application, take appropriate action.

function FastTan6Term(Value: Single): Single;
begin
  Result := Abs(FastMod(Value, CTwoPi32)); // Get rid of values > 2 * pi
  Result := FastTanInBounds6Term(Result);
end;

function FastTan6Term(Value: Double): Double;
begin
  Result := FastTanInBounds6Term(Abs(FastMod(Value, CTwoPi64)));
end;

function FastCotan6Term(Value: Single): Single;
begin
  Result := -FastTan6Term(CPiHalf32 - Value);
end;

function FastCotan6Term(Value: Double): Double;
begin
  Result := -FastTan6Term(CPiHalf64 - Value);
end;


// 3-Term: Accurate to about 6.6 decimal digits over the range [0, pi/12].

function FastArcTanPart3Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := Value * (CArcTan3Term[0] + CArcTan3Term[1] * Result) /
    (CArcTan3Term[2] + Result);
end;

function FastArcTanPart3Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := Value * (CArcTan3Term[0] + CArcTan3Term[1] * Result) /
    (CArcTan3Term[2] + Result);
end;

// WARNING: We do not test for the tangent approaching infinity,
// which it will at x=pi/2 and x=3*pi/2. If this is a problem
// in your application, take appropriate action.

function FastArcTan3Term(Value: Single): Single;
var
  Complement: Boolean; // true if arg was >1
  Region: Boolean; // true depending on region arg is in
  Sign: Boolean; // true if arg was < 0
begin
  Complement := False;
  Region := False;
  Sign := False;
  Result := Value;

  if Result < 0 then
  begin
    Result := -Result;
    Sign := True; // arctan(-x) = -arctan(x)
  end;
  if Result > 1.0 then
  begin
    Result := 1.0 / Result; // keep arg between 0 and 1
    Complement := True;
  end;
  if Result > TanTwelfthPi32 then
  begin
    Result := (Result - TanSixthPi32) / (1 + TanSixthPi32 * Result);
    // reduce arg to under tan(pi/12)
    Region := True;
  end;

  Result := FastArcTanPart3Term(Result); // run the approximation
  if Region then
    Result := Result + CSixthPi32; // correct for region we're in
  if Complement then
    Result := CHalf32 - Result; // correct for 1/x if we did that
  if Sign then
    Result := -Result; // correct for negative arg
end;

function FastArcTan3Term(Value: Double): Double;
var
  Complement: Boolean; // true if arg was >1
  Region: Boolean; // true depending on region arg is in
  Sign: Boolean; // true if arg was < 0
begin
  Complement := False;
  Region := False;
  Sign := False;
  Result := Value;

  if Result < 0 then
  begin
    Result := -Result;
    Sign := True; // arctan(-x) = -arctan(x)
  end;
  if Result > 1.0 then
  begin
    Result := 1.0 / Result; // keep arg between 0 and 1
    Complement := True;
  end;
  if Result > TanTwelfthPi64 then
  begin
    Result := (Result - TanSixthPi64) / (1 + TanSixthPi64 * Result);
    // reduce arg to under tan(pi/12)
    Region := True;
  end;

  Result := FastArcTanPart3Term(Result); // run the approximation
  if Region then
    Result := Result + CSixthPi64; // correct for region we're in
  if Complement then
    Result := CHalf64 - Result; // correct for 1/x if we did that
  if Sign then
    Result := -Result; // correct for negative arg
end;

function FastArcCotan3Term(Value: Single): Single;
begin
  Result := -FastArcTan3Term(CPiHalf32 - Value);
end;

function FastArcCotan3Term(Value: Double): Double;
begin
  Result := -FastArcTan3Term(CPiHalf64 - Value);
end;


// 6-Term: Accurate to about 13.7 decimal digits over the range [0, pi/12].

function FastArcTanPart6Term(Value: Single): Single;
begin
  Result := Sqr(Value);
  Result := Value * (CArcTan6Term[0] + Result * (CArcTan6Term[1] + Result *
    CArcTan6Term[2])) / (CArcTan6Term[3] + Result * (CArcTan6Term[4] + Result *
    (CArcTan6Term[5] + Result)));
end;

function FastArcTanPart6Term(Value: Double): Double;
begin
  Result := Sqr(Value);
  Result := Value * (CArcTan6Term[0] + Result * (CArcTan6Term[1] + Result *
    CArcTan6Term[2])) / (CArcTan6Term[3] + Result * (CArcTan6Term[4] + Result *
    (CArcTan6Term[5] + Result)));
end;

// WARNING: We do not test for the tangent approaching infinity,
// which it will at x = pi / 2 and x = 3 * pi / 2. If this is a problem
// in your application, take appropriate action.

function FastArcTan6Term(Value: Single): Single;
var
  Complement: Boolean; // true if arg was >1
  Region: Boolean; // true depending on region arg is in
  Sign: Boolean; // true if arg was < 0
begin
  Complement := False;
  Region := False;
  Sign := False;
  Result := Value;

  if Result < 0 then
  begin
    Result := -Result;
    Sign := True; // arctan(-x) = -arctan(x)
  end;
  if Result > 1.0 then
  begin
    Result := 1.0 / Result; // keep arg between 0 and 1
    Complement := True;
  end;
  if Result > TanTwelfthPi32 then
  begin
    Result := (Result - TanSixthPi32) / (1 + TanSixthPi32 * Result);
    // reduce arg to under tan(pi/12)
    Region := True;
  end;

  Result := FastArcTanPart6Term(Result); // run the approximation
  if Region then
    Result := Result + CSixthPi32; // correct for region we're in
  if Complement then
    Result := CHalf32 - Result; // correct for 1/x if we did that
  if Sign then
    Result := -Result; // correct for negative arg
end;

function FastArcTan6Term(Value: Double): Double;
var
  Complement: Boolean; // true if arg was >1
  Region: Boolean; // true depending on region arg is in
  Sign: Boolean; // true if arg was < 0
begin
  Complement := False;
  Region := False;
  Sign := False;
  Result := Value;

  if Result < 0 then
  begin
    Result := -Result;
    Sign := True; // arctan(-x) = -arctan(x)
  end;
  if Result > 1.0 then
  begin
    Result := 1.0 / Result; // keep arg between 0 and 1
    Complement := True;
  end;
  if Result > TanTwelfthPi64 then
  begin
    Result := (Result - TanSixthPi64) / (1 + TanSixthPi64 * Result);
    // reduce arg to under tan(pi/12)
    Region := True;
  end;

  Result := FastArcTanPart6Term(Result); // run the approximation
  if Region then
    Result := Result + CSixthPi64; // correct for region we're in
  if Complement then
    Result := CHalf64 - Result; // correct for 1/x if we did that
  if Sign then
    Result := -Result; // correct for negative arg
end;

function FastArcCotan6Term(Value: Single): Single;
begin
  Result := -FastArcTan6Term(CPiHalf32 - Value);
end;

function FastArcCotan6Term(Value: Double): Double;
begin
  Result := -FastArcTan6Term(CPiHalf64 - Value);
end;

// 3-Term: Accurate to about 6.6 decimal digits over the range [0, pi/12].
function FastArcCos3Term(Value: Single): Single;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
begin
  Result := FastArcTan3Term(Value * FastInvSqrt(1 - Sqr(Value)));
end;

function FastArcCos3Term(Value: Double): Double;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
begin
  Result := FastArcTan3Term(Value * FastInvSqrt(1 - Sqr(Value)));
end;

// 6-Term: Accurate to about 13.7 decimal digits over the range [0, pi/12].
function FastArcCos6Term(Value: Single): Single;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
begin
  Result := FastArcTan6Term(Value * FastInvSqrt(1 - Sqr(Value)));
end;

function FastArcCos6Term(Value: Double): Double;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
begin
  Result := FastArcTan6Term(Value * FastInvSqrt(1 - Sqr(Value)));
end;

function FastSinLike(Value: Single): Single; overload;
const
  C1: Single = 7.61E-03;
  C2: Single = -1.6605E-01;
{$IFDEF PUREPASCAL}
var
  Asqr: Double;
begin
  Asqr := Sqr(Value);
  Result := (((Asqr * C1) * Asqr + C2 * Asqr) + 1) * Value;
{$ELSE}
  asm
    FLD     Value
    FMUL    Value
    FLD     C1
    FMUL    ST(0),  ST(1)
    FLD     C2
    FADDP   ST(1), ST(0)
    FMULP   ST(1), ST(0)
    FLD1
    FADDP
    FMUL    Value
    {$ENDIF}
end;

function FastSinLike(Value: Double): Double; overload;
const
  C1: Double = 7.61E-03;
  C2: Double = -1.6605E-01;
{$IFDEF PUREPASCAL}
var
  Asqr: Double;
begin
  Asqr := Sqr(Value);
  Result := (((Asqr * C1) * Asqr + C2 * Asqr) + 1) * Value;
{$ELSE}
  asm
    FLD     Value
    FMUL    Value
    FLD     C1
    FMUL    ST(0), ST(1)
    FLD     C2
    FADDP   ST(1), ST(0)
    FMULP   ST(1), ST(0)
    FLD1
    FADDP
    FMUL    Value
    {$ENDIF}
end;

function FastCosLike(Value: Single): Single; overload;
const
  C1: Single = 3.705E-02;
  C2: Single = -4.967E-01;
{$IFDEF PUREPASCAL}
var
  Asqr: Single;
begin
  Asqr := Sqr(Value);
  Result := (((Asqr * C1) * Asqr + C2 * Asqr) + 1) * Value;
{$ELSE}
  asm
    FLD     Value
    FMUL    Value
    FLD     C1
    FMUL    ST(0), ST(1)
    FLD     C2
    FADDP   ST(1), ST(0)
    FMULP   ST(1), ST(0)
    FLD1
    FADDP
    FMUL    Value
    {$ENDIF}
end;

function FastCosLike(Value: Double): Double; overload;
const
  C1: Double = 3.705E-02;
  C2: Double = -4.967E-01;
{$IFDEF PUREPASCAL}
var
  Asqr: Double;
begin
  Asqr := Sqr(Value);
  Result := (((Asqr * C1) * Asqr + C2 * Asqr) + 1) * Value;
{$ELSE}
  asm
    FLD     Value
    FMUL    Value
    FLD     C1
    FMUL    ST(0), ST(1)
    FLD     C2
    FADDP   ST(1), ST(0)
    FMULP   ST(1), ST(0)
    FLD1
    FADDP
    FMUL    Value
    {$ENDIF}
end;

function FastArctanLike(Value: Single): Single; overload;
var
  VSqr: Single;
begin
  VSqr := Sqr(Value);
  Result := ((((CArcTanLike[0] * VSqr + CArcTanLike[1]) * VSqr + CArcTanLike[2])
    * VSqr + CArcTanLike[3]) * VSqr + CArcTanLike[4]) * Value;
end;

function FastArctanLike(Value: Double): Double; overload;
var
  VSqr: Single;
begin
  VSqr := Sqr(Value);
  Result := ((((CArcTanLike[0] * VSqr + CArcTanLike[1]) * VSqr + CArcTanLike[2])
    * VSqr + CArcTanLike[3]) * VSqr + CArcTanLike[4]) * Value;
end;

function FastFloorLn2(Value: Single): Integer;
begin
  Result := (((Integer((@Value)^) and $7F800000) shr 23) - $7F);
end;

function FastLog2(Value: Single): Single;
var
  IntCast: Integer absolute Value;
begin
  Result := (((IntCast and $7F800000) shr 23) - $7F) + (IntCast and $007FFFFF)
    / $800000;
end;

function FastRandomGauss: Single;
var
  U1, S2: Single;
  Val: Integer absolute S2;
  Res: Integer absolute Result;
begin
  repeat
    U1 := FastRandom;
    S2 := Sqr(U1) + Sqr(FastRandom);
  until S2 < 1;

  // fast log
  Res := Val and (not($FF shl 23)) + $7F shl 23;
  Result := (((Val shr 23) and $FF) - $80) +
    ((CL2Continous4[0] * Result + CL2Continous4[1]) * Result + CL2Continous4[2])
    * Result + CL2Continous4[3];

  // fast sqrt
  S2 := CGaussScaledLog2ofEInv32 * Result / S2;
  Result := S2;
  Res := ((Res - (1 shl 23)) shr 1) + (1 shl 29);
  Result := CHalf32 * (Result + S2 / Result) * U1;
end;

/// ////////////////////////////
// //
// Fast 2^x approximations  //
// //
/// ////////////////////////////

function FastPower2MinError2(Value: Single): Single;
var
  IntCast: Integer absolute Result;
begin
  IntCast := Round(Value);
  Value := Value - IntCast;
  IntCast := ($7F + Intcast) shl 23;
  Result := Result *
    (1 + Value * (CP2MinError2[0] + Value * (CP2MinError2[1])));
end;

function FastPower2ContinousError2(Value: Single): Single;
var
  IntCast: Integer absolute Result;
begin
  IntCast := Round(Value);
  Value := Value - IntCast;
  IntCast := ($7F + Intcast) shl 23;
  Result := Result * (1 + Value * (CP2ContError2[0] + Value *
    (CP2ContError2[1])));
end;

function FastPower2MinError3(Value: Single): Single;
var
  IntCast: Integer absolute Result;
begin
  IntCast := Round(Value);
  Value := Value - IntCast;
  IntCast := ($7F + Intcast) shl 23;
  Result := Result * (1.0 + Value * (CP2MinError3[0] + Value *
    (CP2MinError3[1] + Value * CP2MinError3[2])));
end;

function FastPower2ContinousError3(Value: Single): Single;
var
  IntCast: Integer absolute Result;
begin
  IntCast := Round(Value);
  Value := Value - IntCast;
  IntCast := ($7F + Intcast) shl 23;
  Result := Result * (1 + Value * (CP2ContError3[0] + Value *
    (CP2ContError3[1] + Value * CP2ContError3[2])));
end;

function FastPower2MinError4(Value: Single): Single;
var
  IntCast: Integer absolute Result;
begin
  IntCast := Round(Value);
  Value := Value - IntCast;
  IntCast := ($7F + Intcast) shl 23;
  Result := Result *
    (1 + Value * (CP2MinError4[0] + Value * (CP2MinError4[1] + Value *
    (CP2MinError4[2] + Value * CP2MinError4[3]))));
end;

function FastPower2ContinousError4(Value: Single): Single;
var
  IntCast: Integer absolute Result;
begin
  IntCast := Round(Value);
  Value := Value - IntCast;
  IntCast := ($7F + Intcast) shl 23;
  Result := Result * (1 + Value * (CP2ContError4[0] + Value *
    (CP2ContError4[1] + Value * (CP2ContError4[2] + Value *
    (CP2ContError4[3])))));
end;

function FastPower2MinError5(Value: Single): Single;
var
  IntCast: Integer absolute Result;
begin
  IntCast := Round(Value);
  Value := Value - IntCast;
  IntCast := ($7F + Intcast) shl 23;
  Result := Result *
    (1 + Value * (CP2MinError5[0] + Value * (CP2MinError5[1] + Value *
    (CP2MinError5[2] + Value * (CP2MinError5[3] + Value * CP2MinError5[4])))));
end;

function FastPower2ContinousError5(Value: Single): Single;
var
  IntCast: Integer absolute Result;
begin
  IntCast := Round(Value);
  Value := Value - IntCast;
  IntCast := ($7F + Intcast) shl 23;
  Result := Result * (1 + Value * (CP2ContError5[0] + Value *
    (CP2ContError5[1] + Value * (CP2ContError5[2] + Value * (CP2ContError5[3] +
    Value * (CP2ContError5[4]))))));
end;

{ Log2 Approximations }

function FastLog2Laurent(Value: Single): Single;
var
  Log2: Integer;
  X: Integer absolute Value;
begin
  Log2 := ((X shr 23) and $FF) - $80;
  X := X and (not($FF shl 23)) + $7F shl 23;
  Value := ((CMinusOneThird * Value) + CTwo32) * Value + CMinusTwoThird;
  Result := Value + Log2;
end;

function FastLog2MinError2(Value: Single): Single;
var
  Val: Integer absolute Value;
  Res: Integer absolute Result;
begin
  Res := Val and (not($FF shl 23)) + $7F shl 23;
  Result := (((Val shr 23) and $FF) - $80) +
    (CL2MinError2[0] * Result + CL2MinError2[1]);
end;

function FastLog2ContinousError2(Value: Single): Single;
var
  Val: Integer absolute Value;
  Res: Integer absolute Result;
begin
  Res := Val and (not($FF shl 23)) + $7F shl 23;
  Result := (((Val shr 23) and $FF) - $80) +
    (CL2Continous2[0] * Result + CL2Continous2[1]);
end;

function FastLog2Laurent2(Value: Single): Single;
var
  Val: Integer absolute Value;
  Res: Integer absolute Result;
begin
  Res := Val and (not($FF shl 23)) + $7F shl 23;
  Result := (((Val shr 23) and $FF) - $80) +
    (CL2Laurent2[0] * Result + CL2Laurent2[1]);
end;

function FastLog2MinError3(Value: Single): Single;
var
  Val: Integer absolute Value;
  Res: Integer absolute Result;
begin
  Res := Val and (not($FF shl 23)) + $7F shl 23;
  Result := (((Val shr 23) and $FF) - $80) +
    ((CL2MinError3[0] * Result + CL2MinError3[1]) * Result + CL2MinError3[2]);
end;

function FastLog2ContinousError3(Value: Single): Single;
var
  Val: Integer absolute Value;
  Res: Integer absolute Result;
begin
  Res := Val and (not($FF shl 23)) + $7F shl 23;
  Result := (((Val shr 23) and $FF) - $80) +
    ((CL2Continous3[0] * Result + CL2Continous3[1]) * Result +
    CL2Continous3[2]);
end;

function FastLog2Laurent3(Value: Single): Single;
var
  Val: Integer absolute Value;
  Res: Integer absolute Result;
begin
  Res := Val and (not($FF shl 23)) + $7F shl 23;
  Result := (((Val shr 23) and $FF) - $80) +
    (CL2Laurent3[0] * Result + CL2Laurent3[1]) * Result + CL2Laurent3[2];
end;

function FastLog2MinError4(Value: Single): Single;
var
  Val: Integer absolute Value;
  Res: Integer absolute Result;
begin
  Res := Val and (not($FF shl 23)) + $7F shl 23;
  Result := (((Val shr 23) and $FF) - $80) +
    ((CL2MinError4[0] * Result + CL2MinError4[1]) * Result + CL2MinError4[2]) *
    Result + CL2MinError4[3];
end;

function FastLog2ContinousError4(Value: Single): Single;
var
  Val: Integer absolute Value;
  Res: Integer absolute Result;
begin
  Res := Val and (not($FF shl 23)) + $7F shl 23;
  Result := (((Val shr 23) and $FF) - $80) +
    ((CL2Continous4[0] * Result + CL2Continous4[1]) * Result + CL2Continous4[2])
    * Result + CL2Continous4[3];
end;

function FastLog2Laurent4(Value: Single): Single;
var
  Val: Integer absolute Value;
  Res: Integer absolute Result;
begin
  Res := Val and (not($FF shl 23)) + $7F shl 23;
  Result := (((Val shr 23) and $FF) - $80) +
    ((CL2Laurent4[0] * Result + CL2Laurent4[1]) * Result + CL2Laurent4[2]) *
    Result + CL2Laurent4[3];
end;

function FastLog2MinError5(Value: Single): Single;
var
  Val: Integer absolute Value;
  Res: Integer absolute Result;
begin
  Res := Val and (not($FF shl 23)) + $7F shl 23;
  Result := (((Val shr 23) and $FF) - $80) +
    (((CL2MinError5[0] * Result + CL2MinError5[1]) * Result + CL2MinError5[2]) *
    Result + CL2MinError5[3]) * Result + CL2MinError5[4];
end;

function FastLog2ContinousError5(Value: Single): Single;
var
  Val: Integer absolute Value;
  Res: Integer absolute Result;
begin
  Res := Val and (not($FF shl 23)) + $7F shl 23;
  Result := (((Val shr 23) and $FF) - $80) +
    (((CL2Continous5[0] * Result + CL2Continous5[1]) * Result + CL2Continous5[2]
    ) * Result + CL2Continous5[3]) * Result + CL2Continous5[4];
end;

function FastLog2Laurent5(Value: Single): Single;
var
  Val: Integer absolute Value;
  Res: Integer absolute Result;
begin
  Res := Val and (not($FF shl 23)) + $7F shl 23;
  Result := (((Val shr 23) and $FF) - $80) +
    (((CL2Laurent5[0] * Result + CL2Laurent5[1]) * Result + CL2Laurent5[2]) *
    Result + CL2Laurent5[3]) * Result + CL2Laurent5[4];
end;

function FastExpMinError2(Value: Single): Single;
begin
  Result := FastPower10MinError2(CExp32 * Value)
end;

function FastExpContinousError2(Value: Single): Single;
begin
  Result := FastPower10ContinousError2(CExp32 * Value)
end;

function FastExpMinError3(Value: Single): Single;
begin
  Result := FastPower10MinError2(CExp32 * Value)
end;

function FastExpContinousError3(Value: Single): Single;
begin
  Result := FastPower10ContinousError3(CExp32 * Value)
end;

function FastExpMinError4(Value: Single): Single;
begin
  Result := FastPower10MinError4(CExp32 * Value)
end;

function FastExpContinousError4(Value: Single): Single;
begin
  Result := FastPower10ContinousError4(CExp32 * Value)
end;

function FastExpMinError5(Value: Single): Single;
begin
  Result := FastPower10MinError5(CExp32 * Value)
end;

function FastExpContinousError5(Value: Single): Single;
begin
  Result := FastPower10ContinousError5(CExp32 * Value)
end;

function FastPower10MinError2(Value: Single): Single;
begin
  Result := FastPower2MinError2(C1032 * Value)
end;

function FastPower10ContinousError2(Value: Single): Single;
begin
  Result := FastPower2ContinousError2(C1032 * Value)
end;

function FastPower10MinError3(Value: Single): Single;
begin
  Result := FastPower2MinError2(C1032 * Value)
end;

function FastPower10ContinousError3(Value: Single): Single;
begin
  Result := FastPower2ContinousError3(C1032 * Value)
end;

function FastPower10MinError4(Value: Single): Single;
begin
  Result := FastPower2MinError4(C1032 * Value)
end;

function FastPower10ContinousError4(Value: Single): Single;
begin
  Result := FastPower2ContinousError4(C1032 * Value)
end;

function FastPower10MinError5(Value: Single): Single;
begin
  Result := FastPower2MinError5(C1032 * Value)
end;

function FastPower10ContinousError5(Value: Single): Single;
begin
  Result := FastPower2ContinousError5(C1032 * Value)
end;

{ Log10 Approximations }

function FastLog10MinError2(Value: Single): Single;
begin
  Result := FastLog2MinError2(Value) * CLog2of10Inv32;
end;

function FastLog10ContinousError2(Value: Single): Single;
begin
  Result := FastLog2ContinousError2(Value) * CLog2of10Inv32;
end;

function FastLog10Laurent2(Value: Single): Single;
begin
  Result := FastLog2Laurent2(Value) * CLog2of10Inv32;
end;

function FastLog10MinError3(Value: Single): Single;
begin
  Result := FastLog2MinError3(Value) * CLog2of10Inv32;
end;

function FastLog10ContinousError3(Value: Single): Single;
begin
  Result := FastLog2ContinousError3(Value) * CLog2of10Inv32;
end;

function FastLog10Laurent3(Value: Single): Single;
begin
  Result := FastLog2Laurent3(Value) * CLog2of10Inv32;
end;

function FastLog10MinError4(Value: Single): Single;
begin
  Result := FastLog2MinError4(Value) * CLog2of10Inv32;
end;

function FastLog10ContinousError4(Value: Single): Single;
begin
  Result := FastLog2ContinousError4(Value) * CLog2of10Inv32;
end;

function FastLog10Laurent4(Value: Single): Single;
begin
  Result := FastLog2Laurent4(Value) * CLog2of10Inv32;
end;

function FastLog10MinError5(Value: Single): Single;
begin
  Result := FastLog2MinError5(Value) * CLog2of10Inv32;
end;

function FastLog10ContinousError5(Value: Single): Single;
begin
  Result := FastLog2ContinousError5(Value) * CLog2of10Inv32;
end;

function FastLog10Laurent5(Value: Single): Single;
begin
  Result := FastLog2Laurent5(Value) * CLog2of10Inv32;
end;

{ Ln Approximations }

function FastLnMinError2(Value: Single): Single;
begin
  Result := FastLog2MinError2(Value) * CLog2ofEInv32;
end;

function FastLnContinousError2(Value: Single): Single;
begin
  Result := FastLog2ContinousError2(Value) * CLog2ofEInv32;
end;

function FastLnLaurent2(Value: Single): Single;
begin
  Result := FastLog2Laurent2(Value) * CLog2ofEInv32;
end;

function FastLnMinError3(Value: Single): Single;
begin
  Result := FastLog2MinError3(Value) * CLog2ofEInv32;
end;

function FastLnContinousError3(Value: Single): Single;
begin
  Result := FastLog2ContinousError3(Value) * CLog2ofEInv32;
end;

function FastLnLaurent3(Value: Single): Single;
begin
  Result := FastLog2Laurent3(Value) * CLog2ofEInv32;
end;

function FastLnMinError4(Value: Single): Single;
begin
  Result := FastLog2MinError4(Value) * CLog2ofEInv32;
end;

function FastLnContinousError4(Value: Single): Single;
begin
  Result := FastLog2ContinousError4(Value) * CLog2ofEInv32;
end;

function FastLnLaurent4(Value: Single): Single;
begin
  Result := FastLog2Laurent4(Value) * CLog2ofEInv32;
end;

function FastLnMinError5(Value: Single): Single;
begin
  Result := FastLog2MinError5(Value) * CLog2ofEInv32;
end;

function FastLnContinousError5(Value: Single): Single;
begin
  Result := FastLog2ContinousError5(Value) * CLog2ofEInv32;
end;

function FastLnLaurent5(Value: Single): Single;
begin
  Result := FastLog2Laurent5(Value) * CLog2ofEInv32;
end;

(*
  function FastSqrtMinError2(Value: Single): Single;
  var
  Val : Integer absolute Value;
  Res : Integer absolute Result;
  Apr : Single;
  Ica : Integer absolute Apr;
  begin
  Res := (((((Val shr 23) and $FF) - $7F) div 2) + $7F) shl 23;
  // Res := (((((Val shr 24) shl 1 and $FF) - $7F) div 2 + $7F) shl 23);

  Ica := (Val and ($7FFFFFF)) or $3F000000 or (Val and $800000);

  Apr := 1 + Apr;


  Result := Apr * Result;

  // Result := Value + Result;
  // Res := (Val and (not ($FF shl 23))) or ($7F shl 23);
  // Result := Apr;
  // Res := {(((((Val shr 23) and $FF) - $80) div 2) shl 23 + $80)} (((((Val shr 23) and $FF) + $7F) shr 1 - $7F) shl 23) or (Val and (not ($FF shl 23)));

  end;
*)

// Convert a value in dB's to a linear amplitude

function FastdBtoAmpMinError2(Value: Single): Single;
var
  IntCast: Integer absolute Value;
begin
  if (IntCast and $FF800000) shr 23 > $3F then
    Result := FastPower2MinError2(Value * CdBtoAmpExpGain32)
  else
    Result := 0;
end;

function FastdBtoAmpMinError2(Value: Double): Double;
begin
  if (Value > -1000.0) then
    Result := FastPower2MinError2(Value * CdBtoAmpExpGain64)
  else
    Result := 0;
end;

function FastdBtoAmpContinousError2(Value: Single): Single;
var
  IntCast: Integer absolute Value;
begin
  if (IntCast and $FF800000) shr 23 > $3F then
    Result := FastPower2ContinousError2(Value * CdBtoAmpExpGain32)
  else
    Result := 0;
end;

function FastdBtoAmpContinousError2(Value: Double): Double;
begin
  if (Value > -1000.0) then
    Result := FastPower2ContinousError2(Value * CdBtoAmpExpGain64)
  else
    Result := 0;
end;

function FastdBtoAmpMinError3(Value: Single): Single;
var
  IntCast: Integer absolute Value;
begin
  if (IntCast and $FF800000) shr 23 > $3F then
    Result := FastPower2MinError3(Value * CdBtoAmpExpGain32)
  else
    Result := 0;
end;

function FastdBtoAmpMinError3(Value: Double): Double;
begin
  if (Value > -1000.0) then
    Result := FastPower2MinError3(Value * CdBtoAmpExpGain64)
  else
    Result := 0;
end;

function FastdBtoAmpContinousError3(Value: Single): Single;
var
  IntCast: Integer absolute Value;
begin
  if (IntCast and $FF800000) shr 23 > $3F then
    Result := FastPower2ContinousError3(Value * CdBtoAmpExpGain32)
  else
    Result := 0;
end;

function FastdBtoAmpContinousError3(Value: Double): Double;
begin
  if (Value > -1000.0) then
    Result := FastPower2ContinousError3(Value * CdBtoAmpExpGain64)
  else
    Result := 0;
end;

function FastdBtoAmpMinError4(Value: Single): Single;
var
  IntCast: Integer absolute Value;
begin
  if (IntCast and $FF800000) shr 23 > $3F then
    Result := FastPower2MinError4(Value * CdBtoAmpExpGain32)
  else
    Result := 0;
end;

function FastdBtoAmpMinError4(Value: Double): Double;
begin
  if (Value > -1000.0) then
    Result := FastPower2MinError4(Value * CdBtoAmpExpGain64)
  else
    Result := 0;
end;

function FastdBtoAmpContinousError4(Value: Single): Single;
var
  IntCast: Integer absolute Value;
begin
  if (IntCast and $FF800000) shr 23 > $3F then
    Result := FastPower2ContinousError4(Value * CdBtoAmpExpGain32)
  else
    Result := 0;
end;

function FastdBtoAmpContinousError4(Value: Double): Double;
begin
  if (Value > -1000.0) then
    Result := FastPower2ContinousError4(Value * CdBtoAmpExpGain64)
  else
    Result := 0;
end;

function FastdBtoAmpMinError5(Value: Single): Single;
var
  IntCast: Integer absolute Value;
begin
  if (IntCast and $FF800000) shr 23 > $3F then
    Result := FastPower2MinError5(Value * CdBtoAmpExpGain32)
  else
    Result := 0;
end;

function FastdBtoAmpMinError5(Value: Double): Double;
begin
  if (Value > -1000.0) then
    Result := FastPower2MinError5(Value * CdBtoAmpExpGain64)
  else
    Result := 0;
end;

function FastdBtoAmpContinousError5(Value: Single): Single;
var
  IntCast: Integer absolute Value;
begin
  if (IntCast and $FF800000) shr 23 > $3F then
    Result := FastPower2ContinousError5(Value * CdBtoAmpExpGain32)
  else
    Result := 0;
end;

function FastdBtoAmpContinousError5(Value: Double): Double;
begin
  if (Value > -1000.0) then
    Result := FastPower2ContinousError5(Value * CdBtoAmpExpGain64)
  else
    Result := 0;
end;


// Convert a value in dB's to a linear amplitude

function FastAmptodBMinError2(Value: Single): Single;
begin
  Result := CFactor2IndB32 * FastLog2MinError2(Value);
end;

function FastAmptodBMinError2(Value: Double): Double;
begin
  Result := CFactor2IndB64 * FastLog2MinError2(Value);
end;

function FastAmptodBContinousError2(Value: Single): Single;
begin
  Result := CFactor2IndB32 * FastLog2ContinousError2(Value);
end;

function FastAmptodBContinousError2(Value: Double): Double;
begin
  Result := CFactor2IndB64 * FastLog2ContinousError2(Value);
end;

function FastAmptodBLaurent2(Value: Single): Single;
begin
  Result := CFactor2IndB32 * FastLog2Laurent2(Value);
end;

function FastAmptodBLaurent2(Value: Double): Double;
begin
  Result := CFactor2IndB64 * FastLog2Laurent2(Value);
end;

function FastAmptodBMinError3(Value: Single): Single;
begin
  Result := CFactor2IndB32 * FastLog2MinError3(Value);
end;

function FastAmptodBMinError3(Value: Double): Double;
begin
  Result := CFactor2IndB64 * FastLog2MinError3(Value);
end;

function FastAmptodBContinousError3(Value: Single): Single;
begin
  Result := CFactor2IndB32 * FastLog2ContinousError3(Value);
end;

function FastAmptodBContinousError3(Value: Double): Double;
begin
  Result := CFactor2IndB64 * FastLog2ContinousError3(Value);
end;

function FastAmptodBLaurent3(Value: Single): Single;
begin
  Result := CFactor2IndB32 * FastLog2Laurent3(Value);
end;

function FastAmptodBLaurent3(Value: Double): Double;
begin
  Result := CFactor2IndB64 * FastLog2Laurent3(Value);
end;

function FastAmptodBMinError4(Value: Single): Single;
begin
  Result := CFactor2IndB32 * FastLog2MinError4(Value);
end;

function FastAmptodBMinError4(Value: Double): Double;
begin
  Result := CFactor2IndB64 * FastLog2MinError4(Value);
end;

function FastAmptodBContinousError4(Value: Single): Single;
begin
  Result := CFactor2IndB32 * FastLog2ContinousError4(Value);
end;

function FastAmptodBContinousError4(Value: Double): Double;
begin
  Result := CFactor2IndB64 * FastLog2ContinousError4(Value);
end;

function FastAmptodBLaurent4(Value: Single): Single;
begin
  Result := CFactor2IndB32 * FastLog2Laurent4(Value);
end;

function FastAmptodBLaurent4(Value: Double): Double;
begin
  Result := CFactor2IndB64 * FastLog2Laurent4(Value);
end;

function FastAmptodBMinError5(Value: Single): Single;
begin
  Result := CFactor2IndB32 * FastLog2MinError5(Value);
end;

function FastAmptodBMinError5(Value: Double): Double;
begin
  Result := CFactor2IndB64 * FastLog2MinError5(Value);
end;

function FastAmptodBContinousError5(Value: Single): Single;
begin
  Result := CFactor2IndB32 * FastLog2ContinousError5(Value);
end;

function FastAmptodBContinousError5(Value: Double): Double;
begin
  Result := CFactor2IndB64 * FastLog2ContinousError5(Value);
end;

function FastAmptodBLaurent5(Value: Single): Single;
begin
  Result := CFactor2IndB32 * FastLog2Laurent5(Value);
end;

function FastAmptodBLaurent5(Value: Double): Double;
begin
  Result := CFactor2IndB64 * FastLog2Laurent5(Value);
end;

function FastIntPower(I: Single; N: Integer): Single;
var
  L: Integer absolute I;
begin
  Result := (L - $3F800000) shr (N - 1) + $3F800000;
end;

function FastPower(Base, Exponent: Double): Double;
// The Original Code is Fastcode (Contributor(s): John O'Harrow)
{$IFDEF PUREPASCAL}
begin
  Result := Power(Base, Exponent);
end;
{$ELSE}

const
  Max: Double = MaxInt;
var
  IntExp: Integer;
  asm
    FLD     Exponent
    FLD     ST             // copy to ST(1)
    FABS                   // Abs(exp)
    FLD     Max
    FCOMPP                 // leave exp in ST(0)
    FSTSW   AX
    SAHF
    JB      @RealPower     // exp > MaxInt
    FLD     ST             // exp in ST(0) and ST(1)
    FRNDINT                // Round(exp)
    FCOMP                  // compare exp and Round(exp)
    FSTSW   AX
    SAHF
    JNE     @RealPower
    FISTP   IntExp
    MOV     EAX, IntExp    // EAX = Trunc(Exponent)
    MOV     ECX, EAX
    CDQ
    FLD1                   // Result = 1
    XOR     EAX, EDX
    SUB     EAX, EDX       // Abs(exp)
    JZ      @Exit
    FLD     Base
    JMP     @Entry
  @Loop:
    FMUL    ST, ST         // Base * Base
  @Entry:
    SHR     EAX, 1
    JNC     @Loop
    FMUL    ST(1), ST      // Result * X
    JNZ     @Loop
    FSTP    ST
    CMP     ECX, 0
    JGE     @Exit
    FLD1
    FDIVRP  ST(1), ST(0)   // 1 / Result
    JMP     @Exit
  @RealPower:
    FLD     Base
    FTST
    FSTSW   AX
    SAHF
    JZ      @Done
    FLDLN2
    FXCH
    FYL2X
    FXCH
    FMULP   ST(1), ST
    FLDL2E
    FMULP   ST(1), ST
    FLD     ST(0)
    FRNDINT
    FSUB    ST(1), ST
    FXCH    ST(1)
    F2XM1
    FLD1
    FADDP   ST(1), ST
    FSCALE
  @Done:
    FSTP    ST(1)
  @Exit:
end;
{$ENDIF}

function FastPower2(Value: Single): Single;
var
  IntCast: Integer absolute Result;
begin
  IntCast := ((($7F + Round(Value)) shl 23) and $FF800000);
end;

function FastExp(Value: Single): Single;
begin
  Result := FastPower2(CExp32 * Value);
end;

function FastRoot(I: Single; N: Integer): Single;
var
  L: Integer absolute I;
begin
  Result := (L - $3F800000) shr (N - 1) + $3F800000;
end;

function FastInvSqrt(Value: Single): Single; overload;
var
  IntCst: Cardinal absolute Result;
begin
  Result := Value;
  IntCst := ($BE6EB50C - IntCst) shr 1;
  Result := CHalf32 * Result * (3 - Value * Sqr(Result));
end;

function FastSqrt(Value: Single): Single; overload;
begin
  Result := Value * FastInvSqrt(Value);
end;

function FastInvSqrt(Value: Double): Double; overload;
var
  IntCst: Int64 absolute Result;
begin
  Result := Value;
  IntCst := ($BFCDD90BCFBC61B4 - IntCst) shr 1;
  Result := CHalf64 * Result * (3 - Value * Sqr(Result));
end;

function FastSqrt(Value: Double): Double; overload;
begin
  Result := Value * FastInvSqrt(Value);
end;

function FastSqrtBab0(Value: Single): Single; overload;
var
  IntCst: Integer absolute Result;
begin
  Result := Value;
  IntCst := ((IntCst - (1 shl 23)) shr 1) + (1 shl 29);
end;

function FastSqrtBab1(Value: Single): Single; overload;
var
  IntCst: Integer absolute Result;
begin
  Result := Value;
  IntCst := ((IntCst - (1 shl 23)) shr 1) + (1 shl 29);
  Result := CHalf32 * (Result + Value / Result);
end;

function FastSqrtBab2(Value: Single): Single; overload;
var
  IntCst: Integer absolute Result;
begin
  Result := Value;
  IntCst := ((IntCst - (1 shl 23)) shr 1) + (1 shl 29);
  Result := Result + Value / Result;
  Result := CQuarter32 * Result + Value / Result;
end;

function FastSqrtBab0(Value: Double): Double; overload;
var
  IntCst: Int64 absolute Result;
begin
  Result := Value;
  IntCst := (Int64(1) shl 61) + ((IntCst - (Int64(1) shl 52)) shr 1);
end;

function FastSqrtBab1(Value: Double): Double; overload;
var
  IntCst: Int64 absolute Result;
begin
  Result := Value;
  IntCst := (Int64(1) shl 61) + ((IntCst - (Int64(1) shl 52)) shr 1);
  Result := CHalf32 * (Result + Value / Result);
end;

function FastSqrtBab2(Value: Double): Double; overload;
var
  IntCst: Int64 absolute Result;
begin
  Result := Value;
  IntCst := (Int64(1) shl 61) + ((IntCst - (Int64(1) shl 52)) shr 1);
  Result := Result + Value / Result;
  Result := CQuarter64 * Result + Value / Result;
end;

function FastTanhOpt3Term(Input: Single): Single;
var
  A, B: Double;
begin
  A := Abs(Input);
  B := 1.26175667589988239 + A *
    (-0.54699348440059470 + A * (2.66559097474027817));
  Result := (B * Input) / (B * A + 1);
end;

function FastTanhOpt4Term(Input: Single): Single;
var
  A, B: Double;
begin
  A := Abs(Input);
  B := 0.89690305801668457 + A *
    (1.89047619399687661 + A * (-1.35205169119085666 + A *
    1.74656303770202670));
  Result := (B * Input) / (B * A + 1);
end;

function FastTanhOpt5Term(Input: Single): Single;
var
  A, B: Double;
begin
  A := Abs(Input);
  B := 1.03971379878158321 + A *
    (0.54953758170495126 + A * (2.13184139104070569 + A * (-1.46060069227128242
    + A * (0.91996358346770157))));
  Result := (B * Input) / (B * A + 1);
end;

function FastTanhOpt6Term(Input: Single): Single;
var
  A, B: Double;
begin
  A := Abs(Input);
  B := 0.98516470896867081 + A *
    (1.21020234045009012 + A * (-0.22720155259481389 + A * (1.89719615102030725
    + A * (-1.07161642656874956 + A * (0.40487405571569546)))));
  Result := (B * Input) / (B * A + 1);
end;

function FastTanhOpt7Term(Input: Single): Single;
var
  A, B: Double;
begin
  A := Abs(Input);
  B := 1.00518193411912860 + A *
    (0.91005085146116016 + A * (1.14542500876429276 + A * (-0.76509890972158046
    + A * (1.34808969964882519 + A * (-0.60147655894944263 + A *
    (0.15264109378548973))))));
  Result := (B * Input) / (B * A + 1);
end;

function FastTanhOpt3Term(Input: Double): Double;
var
  A, B: Double;
begin
  A := Abs(Input);
  B := 1.26175667589988239 + A *
    (-0.54699348440059470 + A * (2.66559097474027817));
  Result := (B * Input) / (B * A + 1);
end;

function FastTanhOpt4Term(Input: Double): Double;
var
  A, B: Double;
begin
  A := Abs(Input);
  B := 0.89690305801668457 + A *
    (1.89047619399687661 + A * (-1.35205169119085666 + A *
    1.74656303770202670));
  Result := (B * Input) / (B * A + 1);
end;

function FastTanhOpt5Term(Input: Double): Double;
var
  A, B: Double;
begin
  A := Abs(Input);
  B := 1.03971379878158321 + A *
    (0.54953758170495126 + A * (2.13184139104070569 + A * (-1.46060069227128242
    + A * (0.91996358346770157))));
  Result := (B * Input) / (B * A + 1);
end;

function FastTanhOpt6Term(Input: Double): Double;
var
  A, B: Double;
begin
  A := Abs(Input);
  B := 0.98516470896867081 + A *
    (1.21020234045009012 + A * (-0.22720155259481389 + A * (1.89719615102030725
    + A * (-1.07161642656874956 + A * (0.40487405571569546)))));
  Result := (B * Input) / (B * A + 1);
end;

function FastTanhOpt7Term(Input: Double): Double;
var
  A, B: Double;
begin
  A := Abs(Input);
  B := 1.00518193411912860 + A *
    (0.91005085146116016 + A * (1.14542500876429276 + A * (-0.76509890972158046
    + A * (1.34808969964882519 + A * (-0.60147655894944263 + A *
    (0.15264109378548973))))));
  Result := (B * Input) / (B * A + 1);
end;

{$IFNDEF Purepascal}

function FastTanhOpt3TermFPU(Input: Single): Single; assembler;
const
  C0: Double = 2.66559097474027817;
  C1: Double = -0.54699348440059470;
  C2: Double = 1.26175667589988239;
  asm
    FLD     Input.Single // Load Input
    FLD     ST(0)        // Copy Input
    FABS                 // Stack: Abs(Input), Input
    FLD     c0           // Load c0 as working value, Abs(Input) => a
    FMUL    ST(0), ST(1) // Stack: a * c0, a, Input
    FADD    c1           // Stack: c1 + a * c0, a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c1 + a * c0), a, Input
    FADD    c2           // Stack: b := c2 + a * (c1 + a * c0), a, Input
    FXCH    ST(2)        // exchange b and Input, Stack: Input, a, b
    FMUL    ST(0), ST(2) // Stack: b * Input, a, b
    FXCH    ST(2)        // exchange b * Input and Input, Stack: b, a, b * Input
    FMULP                // Stack: b * a, b * Input
    FLD1                 // Stack: 1, b * a, b * Input
    FADDP                // Stack: 1 + b * a, b * Input
    FDIVP                // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt4TermFPU(Input: Single): Single; assembler;
const
  C0: Double = 1.74656303770202670;
  C1: Double = -1.35205169119085666;
  C2: Double = 1.89047619399687661;
  C3: Double = 0.89690305801668457;
  asm
    FLD     Input.Single // Load Input
    FLD     ST(0)        // Copy Input
    FABS                 // Stack: Abs(Input), Input
    FLD     c0           // Load c0 as working value, Abs(Input) => a
    FMUL    ST(0), ST(1) // Stack: a * c0, a, Input
    FADD    c1           // Stack: c1 + a * c0, a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c1 + a * c0), a, Input
    FADD    c2           // Stack: c2 + a * (c1 + a * c0), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
    FADD    c3           // Stack: b := c3 + a * (c2 + a * (c1 + a * c0)), a, Input
    FXCH    ST(2)        // exchange b and Input, Stack: Input, a, b
    FMUL    ST(0), ST(2) // Stack: b * Input, a, b
    FXCH    ST(2)        // exchange b * Input and Input, Stack: b, a, b * Input
    FMULP                // Stack: b * a, b * Input
    FLD1                 // Stack: 1, b * a, b * Input
    FADDP                // Stack: 1 + b * a, b * Input
    FDIVP                // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt5TermFPU(Input: Single): Single; assembler;
const
  C0: Double = 0.91996358346770157;
  C1: Double = -1.46060069227128242;
  C2: Double = 2.13184139104070569;
  C3: Double = 0.54953758170495126;
  C4: Double = 1.03971379878158321;
  asm
    FLD     Input.Single // Load Input
    FLD     ST(0)        // Copy Input
    FABS                 // Stack: Abs(Input), Input
    FLD     c0           // Load c0 as working value, Abs(Input) => a
    FMUL    ST(0), ST(1) // Stack: a * c0, a, Input
    FADD    c1           // Stack: c1 + a * c0, a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c1 + a * c0), a, Input
    FADD    c2           // Stack: c2 + a * (c1 + a * c0), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
    FADD    c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
    FADD    c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
    FXCH    ST(2)        // exchange b and Input, Stack: Input, a, b
    FMUL    ST(0), ST(2) // Stack: b * Input, a, b
    FXCH    ST(2)        // exchange b * Input and Input, Stack: b, a, b * Input
    FMULP                // Stack: b * a, b * Input
    FLD1                 // Stack: 1, b * a, b * Input
    FADDP                // Stack: 1 + b * a, b * Input
    FDIVP                // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt6TermFPU(Input: Single): Single; assembler;
const
  C0: Double = 0.40487405571569546;
  C1: Double = -1.07161642656874956;
  C2: Double = 1.89719615102030725;
  C3: Double = -0.22720155259481389;
  C4: Double = 1.21020234045009012;
  C5: Double = 0.98516470896867081;
  asm
    FLD     Input.Single // Load Input
    FLD     ST(0)        // Copy Input
    FABS                 // Stack: Abs(Input), Input
    FLD     c0           // Load c0 as working value, Abs(Input) => a
    FMUL    ST(0), ST(1) // Stack: a * c0, a, Input
    FADD    c1           // Stack: c1 + a * c0, a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c1 + a * c0), a, Input
    FADD    c2           // Stack: c2 + a * (c1 + a * c0), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
    FADD    c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
    FADD    c4           // Stack: c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
    FADD    c5           // Stack: b := c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
    FXCH    ST(2)        // exchange b and Input, Stack: Input, a, b
    FMUL    ST(0), ST(2) // Stack: b * Input, a, b
    FXCH    ST(2)        // exchange b * Input and Input, Stack: b, a, b * Input
    FMULP                // Stack: b * a, b * Input
    FLD1                 // Stack: 1, b * a, b * Input
    FADDP                // Stack: 1 + b * a, b * Input
    FDIVP                // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt7TermFPU(Input: Single): Single; assembler;
const
  C0: Double = 0.152641093785489734;
  C1: Double = -0.60147655894944263;
  C2: Double = 1.34808969964882519;
  C3: Double = -0.765098909721580456;
  C4: Double = 1.14542500876429276;
  C5: Double = 0.91005085146116016;
  C6: Double = 1.00518193411912860;
  asm
    FLD     Input.Single // Load Input
    FLD     ST(0)        // Copy Input
    FABS                 // Stack: Abs(Input), Input
    FLD     c0           // Load c0 as working value, Abs(Input) => a
    FMUL    ST(0), ST(1) // Stack: a * c0, a, Input
    FADD    c1           // Stack: c1 + a * c0, a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c1 + a * c0), a, Input
    FADD    c2           // Stack: c2 + a * (c1 + a * c0), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
    FADD    c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
    FADD    c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
    FADD    c5           // Stack: c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, Input
    FADD    c6           // Stack: b := c6 + a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, Input
    FXCH    ST(2)        // exchange b and x, Stack: x, a, b
    FMUL    ST(0), ST(2) // Stack: b * Input, a, b
    FXCH    ST(2)        // exchange b * Input and Input, Stack: b, a, b * Input
    FMULP                // Stack: b * a, b * Input
    FLD1                 // Stack: 1, b * a, b * Input
    FADDP                // Stack: 1 + b * a, b * Input
    FDIVP                // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt3TermFPU(Input: Double): Double; assembler;
const
  C0: Double = 2.66559097474027817;
  C1: Double = -0.54699348440059470;
  C2: Double = 1.26175667589988239;
  asm
    FLD     Input.Double // Load Input
    FLD     ST(0)        // Copy Input
    FABS                 // Stack: Abs(Input), Input
    FLD     c0           // Load c0 as working value, Abs(Input) => a
    FMUL    ST(0), ST(1) // Stack: a * c0, a, Input
    FADD    c1           // Stack: c1 + a * c0, a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c1 + a * c0), a, Input
    FADD    c2           // Stack: b := c2 + a * (c1 + a * c0), a, Input
    FXCH    ST(2)        // exchange b and Input, Stack: Input, a, b
    FMUL    ST(0), ST(2) // Stack: b * Input, a, b
    FXCH    ST(2)        // exchange b * Input and Input, Stack: b, a, b * Input
    FMULP                // Stack: b * a, b * Input
    FLD1                 // Stack: 1, b * a, b * Input
    FADDP                // Stack: 1 + b * a, b * Input
    FDIVP                // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt4TermFPU(Input: Double): Double; assembler;
const
  C0: Double = 1.74656303770202670;
  C1: Double = -1.35205169119085666;
  C2: Double = 1.89047619399687661;
  C3: Double = 0.89690305801668457;
  asm
    FLD     Input.Double // Load Input
    FLD     ST(0)        // Copy Input
    FABS                 // Stack: Abs(Input), Input
    FLD     c0           // Load c0 as working value, Abs(Input) => a
    FMUL    ST(0), ST(1) // Stack: a * c0, a, Input
    FADD    c1           // Stack: c1 + a * c0, a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c1 + a * c0), a, Input
    FADD    c2           // Stack: c2 + a * (c1 + a * c0), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
    FADD    c3           // Stack: b := c3 + a * (c2 + a * (c1 + a * c0)), a, Input
    FXCH    ST(2)        // exchange b and Input, Stack: Input, a, b
    FMUL    ST(0), ST(2) // Stack: b * Input, a, b
    FXCH    ST(2)        // exchange b * Input and Input, Stack: b, a, b * Input
    FMULP                // Stack: b * a, b * Input
    FLD1                 // Stack: 1, b * a, b * Input
    FADDP                // Stack: 1 + b * a, b * Input
    FDIVP                // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt5TermFPU(Input: Double): Double; assembler;
const
  C4: Double = 0.91996358346770157;
  C3: Double = -1.46060069227128242;
  C2: Double = 2.13184139104070569;
  C1: Double = 0.54953758170495126;
  C0: Double = 1.03971379878158321;
  asm
    FLD     Input.Double // Load Input
    FLD     ST(0)        // Copy Input
    FABS                 // Stack: Abs(Input), Input
    FLD     c0           // Load c0 as working value, Abs(Input) => a
    FMUL    ST(0), ST(1) // Stack: a * c0, a, Input
    FADD    c1           // Stack: c1 + a * c0, a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c1 + a * c0), a, Input
    FADD    c2           // Stack: c2 + a * (c1 + a * c0), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
    FADD    c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
    FADD    c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
    FXCH    ST(2)        // exchange b and Input, Stack: Input, a, b
    FMUL    ST(0), ST(2) // Stack: b * Input, a, b
    FXCH    ST(2)        // exchange b * Input and Input, Stack: b, a, b * Input
    FMULP                // Stack: b * a, b * Input
    FLD1                 // Stack: 1, b * a, b * Input
    FADDP                // Stack: 1 + b * a, b * Input
    FDIVP                // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt6TermFPU(Input: Double): Double; assembler;
const
  C0: Double = 0.40487405571569546;
  C1: Double = -1.07161642656874956;
  C2: Double = 1.89719615102030725;
  C3: Double = -0.22720155259481389;
  C4: Double = 1.21020234045009012;
  C5: Double = 0.98516470896867081;
  asm
    FLD     Input.Double // Load Input
    FLD     ST(0)        // Copy Input
    FABS                 // Stack: Abs(Input), Input
    FLD     c0           // Load c0 as working value, Abs(Input) => a
    FMUL    ST(0), ST(1) // Stack: a * c0, a, Input
    FADD    c1           // Stack: c1 + a * c0, a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c1 + a * c0), a, Input
    FADD    c2           // Stack: c2 + a * (c1 + a * c0), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
    FADD    c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
    FADD    c4           // Stack: c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
    FADD    c5           // Stack: b := c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
    FXCH    ST(2)        // exchange b and Input, Stack: Input, a, b
    FMUL    ST(0), ST(2) // Stack: b * Input, a, b
    FXCH    ST(2)        // exchange b * Input and Input, Stack: b, a, b * Input
    FMULP                // Stack: b * a, b * Input
    FLD1                 // Stack: 1, b * a, b * Input
    FADDP                // Stack: 1 + b * a, b * Input
    FDIVP                // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt7TermFPU(Input: Double): Double; assembler;
const
  C0: Double = 0.152641093785489734;
  C1: Double = -0.60147655894944263;
  C2: Double = 1.34808969964882519;
  C3: Double = -0.765098909721580456;
  C4: Double = 1.14542500876429276;
  C5: Double = 0.91005085146116016;
  C6: Double = 1.00518193411912860;
  asm
    FLD     Input.Double // Load Input
    FLD     ST(0)        // Copy Input
    FABS                 // Stack: Abs(Input), Input
    FLD     c0           // Load c0 as working value, Abs(Input) => a
    FMUL    ST(0), ST(1) // Stack: a * c0, a, Input
    FADD    c1           // Stack: c1 + a * c0, a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c1 + a * c0), a, Input
    FADD    c2           // Stack: c2 + a * (c1 + a * c0), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
    FADD    c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
    FADD    c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
    FADD    c5           // Stack: c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
    FMUL    ST(0), ST(1) // Stack: a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, Input
    FADD    c6           // Stack: b := c6 + a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, Input
    FXCH    ST(2)        // exchange b and Input, Stack: Input, a, b
    FMUL    ST(0), ST(2) // Stack: b * Input, a, b
    FXCH    ST(2)        // exchange b * Input and Input, Stack: b, a, b * Input
    FMULP                // Stack: b * a, b * Input
    FLD1                 // Stack: 1, b * a, b * Input
    FADDP                // Stack: 1 + b * a, b * Input
    FDIVP                // Stack: (b * Input) / (1 + b * a)
end;
{$ENDIF}

function FastTanh2Like4Term(Input: Single): Single;
var
  A, B: Single;
begin
  A := Abs(Input);
  B := 12 + A * (6 + A * (3 + A));
  Result := (Input * B) / (A * B + 24);
end;

function FastTanh2Like3Term(Input: Single): Single;
var
  A, B: Single;
begin
  A := Abs(Input);
  B := (6 + A * (3 + A));
  Result := (Input * B) / (A * B + 12);
end;

function FastTanh2Like2Term(Input: Single): Single;
{$IFDEF PUREPASCAL}
var
  A, B: Single;
begin
  A := Abs(Input);
  B := 3 + A;
  Result := (Input * B) / (A * B + 6);
{$ELSE}
const
  C3: Single = 3;
  C6: Single = 6;
  asm
    FLD     Input.Single;
    FABS
    FLD     c3
    FADD    ST(0), ST(1)
    FLD     ST(0)
    FMUL    Input.Single
    FXCH    ST(2)
    FMULP
    FADD    c6.Single
    FDIV
    {$ENDIF}
end;

function FastTanh2Like1Term(Input: Single): Single;
{$IFDEF PUREPASCAL}
begin
  Result := Input / (Abs(Input) + 3);
{$ELSE}
const
  C3: Single = 3;
  asm
    FLD     Input.Single;
    FLD     Input.Single;
    FABS
    FADD    c3
    FDIV
    {$ENDIF}
end;

function FastTanhMinError2(Value: Single): Single;
var
  Temp: Single;
begin
  Temp := FastPower2MinError2(C2Exp32 * Value);
  Result := (Temp - 1) / (Temp + 1);
end;

function FastTanhContinousError2(Value: Single): Single;
var
  Temp: Single;
begin
  Temp := FastPower2ContinousError2(C2Exp32 * Value);
  Result := (Temp - 1) / (Temp + 1);
end;

function FastTanhMinError3(Value: Single): Single;
var
  Temp: Single;
begin
  Temp := FastPower2MinError3(C2Exp32 * Value);
  Result := (Temp - 1) / (Temp + 1);
end;

function FastTanhContinousError3(Value: Single): Single;
var
  Temp: Single;
begin
  Temp := FastPower2ContinousError3(C2Exp32 * Value);
  Result := (Temp - 1) / (Temp + 1);
end;

function FastTanhMinError4(Value: Single): Single;
var
  Temp: Single;
begin
  Temp := FastPower2MinError4(C2Exp32 * Value);
  Result := (Temp - 1) / (Temp + 1);
end;

function FastTanhContinousError4(Value: Single): Single;
var
  Temp: Single;
begin
  Temp := FastPower2ContinousError4(C2Exp32 * Value);
  Result := (Temp - 1) / (Temp + 1);
end;

function FastTanhMinError5(Value: Single): Single;
var
  Temp: Single;
begin
  Temp := FastPower2MinError5(C2Exp32 * Value);
  Result := (Temp - 1) / (Temp + 1);
end;

function FastTanhContinousError5(Value: Single): Single;
var
  Temp: Single;
begin
  Temp := FastPower2ContinousError5(C2Exp32 * Value);
  Result := (Temp - 1) / (Temp + 1);
end;

procedure InitConstants;
{$IFDEF PUREPASCAL}
begin
  Ln2 := Ln(2);
  Ln2Half := Ln2 * CHalf32;
  Ln2Rez := 1 / Ln2;
  Ln10 := Ln(10);
  TanSixthPi64 := Tan(CSixthPi64);
  TanTwelfthPi64 := Tan(CTwelfthPi64);
  TanSixthPi32 := TanSixthPi64;
  TanTwelfthPi32 := TanTwelfthPi64;
end;
{$ELSE}

begin
  Ln2 := Ln(2); // ToDo: use ASM here!
  Ln2Half := Ln2 * CHalf32;
  Ln2Rez := 1 / Ln2;
  Ln10 := Ln(10);
  TanSixthPi64 := Tan(CSixthPi64);
  TanTwelfthPi64 := Tan(CTwelfthPi64);
  TanSixthPi32 := TanSixthPi64;
  TanTwelfthPi32 := TanTwelfthPi64;
end;
{$ENDIF}

initialization

InitConstants;

end.
