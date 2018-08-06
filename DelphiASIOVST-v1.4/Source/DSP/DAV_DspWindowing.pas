unit DAV_DspWindowing;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

{$IFDEF CPUx86_64}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  DAV_Types, DAV_Classes, DAV_Complex;

const
  CHamming : array [0..1] of Double = (0.54, -0.46);
  CHanning : Double = -0.5;
  CBlackman : array [0..2] of Double = (0.34, -0.5, 0.16);

type
  TParameterRecord = record
    ComplexPosition          : TComplex64;
    ComplexAngle             : TComplex64;
    SpectrumCorrectionFactor : Double;
    SpuaredCorrectionFactor  : Double;
    CoefficientPointer       : PDAVDoubleFixedArray;
  end;

procedure ApplyTriangleWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyHanningWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyHammingWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyBlackmanWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyBlackmanHarrisWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyGaussianWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyKaiserBesselWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer; const Alpha: Single); overload;

procedure ApplyTriangleWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyHanningWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyHammingWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyBlackmanWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyBlackmanHarrisWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyGaussianWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyKaiserBesselWindow(var Data: TDAVSingleDynArray; const Alpha: Single); overload;

procedure DoWinLoopCos2T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos2T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos2T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos2T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos3T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos3T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos3T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos3T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos4T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos4T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos4T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos4T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos5T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos5T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos5T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos5T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos6T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos6T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos6T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos6T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos7T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos7T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos7T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos7T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos8T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos8T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos8T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos8T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos9T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos9T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos9T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos9T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos10T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos10T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos10T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos10T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos11T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos11T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos11T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos11T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopTriangle32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopTriangle64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopTriangle32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopTriangle64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCosine32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCosine64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCosine32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCosine64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopLanczos32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopLanczos64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopLanczos32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopLanczos64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopHanning32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopHanning64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopHanning32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopHanning64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);

implementation

uses
  Math, DAV_Math;

// Generate window function (Triangle)
procedure ApplyTriangleWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to (SampleFrames div 2) - 1
  do Data^[i] := i / (SampleFrames div 2) * Data^[i];
 for i := (SampleFrames div 2) to SampleFrames - 1
  do Data^[i] := (SampleFrames - i) / (SampleFrames div 2) * Data^[i];
end;

procedure ApplyTriangleWindow(var Data: TDAVSingleDynArray);
begin
 ApplyHanningWindow(@Data[0], Length(Data));
end;


// Generate window function (Hanning)
procedure ApplyHanningWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
  k    : Double;
begin
 j := SampleFrames - 1;
 k := 1 / j;
 for i := 0 to j
  do Data^[i] := Data^[i] * (0.5 * (1.0 - cos(2 * PI * i * k)));
end;

procedure ApplyHanningWindow(var Data: TDAVSingleDynArray);
begin
 ApplyHanningWindow(@Data[0], Length(Data));
end;


// Generate window function (Hamming)
procedure ApplyHammingWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
  k    : Double;
begin
 j := SampleFrames - 1;
 k := 1 / j;
 for i := 0 to j
  do Data^[i] := Data^[i] * (0.54 - (0.46 * cos(2 * PI * i * k)));
end;

procedure ApplyHammingWindow(var Data: TDAVSingleDynArray);
begin
 ApplyHammingWindow(@Data[0], Length(Data));
end;


// Generate window function (Gaussian)
procedure ApplyGaussianWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
begin
 j := SampleFrames - 1;
 for i := 0 to j
  do Data^[i] := Data^[i] * (exp(-5.0 / (sqr(j)) * (2 * i - j) * (2 * i - j)));
end;

procedure ApplyGaussianWindow(var Data: TDAVSingleDynArray);
begin
 ApplyGaussianWindow(@Data[0], Length(Data));
end;


// Generate window function (Blackman)
procedure ApplyBlackmanWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Phase  : TComplex64;
  Value  : TComplex64;
const
  CBlackman : array [0..2] of Double = (0.34, -0.5, 0.16);
begin
 Value.Re := 1;
 Value.Im := 0;
 GetSinCos(2 * PI / (SampleFrames - 1), Phase.Im, Phase.Re);
 for Sample := 0 to SampleFrames - 1 do
  begin
   // using the chebyshev polynom identity to get rid of the cos(2*x)
   Data^[Sample] := Data^[Sample] * (CBlackman[0] + Value.Re * (CBlackman[1] + CBlackman[2] * Value.Re));
   ComplexMultiplyInplace64(Value, Phase);
  end;
end;

procedure ApplyBlackmanWindow(var Data: TDAVSingleDynArray);
begin
 ApplyBlackmanWindow(@Data[0], Length(Data));
end;


// Generate window function (Blackman-Harris)
procedure ApplyBlackmanHarrisWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
  k    : Double;
begin
 j := SampleFrames - 1;
 k := 1 / j;
 for i := 0 to j
  do Data^[i] := Data^[i] * (0.35875 - 0.48829 * cos(2 * PI * (i + 0.5) * k)
                           + 0.14128 * cos(4 * PI * (i + 0.5) * k)
                           - 0.01168 * cos(6 * PI * (i + 0.5) * k));
end;

procedure ApplyBlackmanHarrisWindow(var Data: TDAVSingleDynArray);
begin
 ApplyBlackmanHarrisWindow(@Data[0], Length(Data));
end;

function Io(const Value: Double): Double;
var
  y, de : Double;
  i     : Integer;
  sde   : Double;
const
  CEpsilon: Double = 1E-08;
begin
 y := 0.5 * Value;
 de := 1.0;
 Result := 1;
 for i := 1 to 25 do
  begin
   de := de * y / i;
   sde := sqr(de);
   Result := Result + sde;
   if (Result * CEpsilon - sde) > 0
    then break;
  end;
end;

// Generate window function (Kaiser-Bessel)
procedure ApplyKaiserBesselWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer; const Alpha: Single); overload;
var
  i    : Integer;
  bes  : Double;
  odd  : Integer;
  xi   : Double;
  xind : Double;
begin
 bes := 1.0 / Io(Alpha);
 odd := SampleFrames mod 2;
 xind := sqr(SampleFrames - 1);
 for i := 0 to SampleFrames - 1 do
  begin
   if (odd = 1)
    then xi := i + 0.5
    else xi := i;
   xi  := 4 * sqr(xi);
   Data^[i] := Io(Alpha * sqrt(1 - xi/xind)) * bes;
  end;
end;

procedure ApplyKaiserBesselWindow(var Data: TDAVSingleDynArray; const Alpha: Single);
begin
 ApplyKaiserBesselWindow(@Data[0], Length(Data), Alpha);
end;

procedure DoWinLoopCos2T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFDEF PUREPASCAL}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (CoefficientPointer[0] - PDAV2DoubleArray(CoefficientPointer)^[1] * ComplexPosition.Im);
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end
{$ELSE}
asm
    OR      ECX, ECX
    JNG     @Exit
    PUSH    EBX
    MOV     EBX, [EDX + 48]
    FLD     [EDX + 40].Double   // SpuaredCorrectionFactor
    FLD     [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
    FLD     [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
    FLD     [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
    FLD     [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
    FLD     [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

@CosLoop:
    FLD     ST(1)
    FMUL    [EBX + 8].Double
    FADD    [EBX].Double

    FLD     [EAX].Single
    FMUL    ST(0), ST(1)
    FSTP    [EAX].Single

    FADD    ST(5), ST(0)
    FMUL    ST(0), ST(0)
    FADDP   ST(6), ST(0)

    FLD     ST(3)         // Im, SampleIndex, i, Re, Im
    FMUL    ST(0), ST(2)  // Im * i, SampleIndex, i, Re, Im
    FLD     ST(3)         // Re, Im * i, SampleIndex, i, Re, Im
    FMUL    ST(0), ST(2)  // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
    FSUBRP                // newRe, SampleIndex, i, Re, Im

    FLD     ST(4)         // Im, newRe, SampleIndex, i, Re, Im
    FMULP   ST(2), ST(0)  // newRe, Im * SampleIndex, i, Re, Im
    FLD     ST(3)         // Re, newRe, Im * SampleIndex, i, Re, Im
    FMULP   ST(3), ST(0)  // newRe, Im * SampleIndex, Re * i, Re, Im
    FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
    FADDP   ST(2), ST(0)  // newRe, newIm, Re, Im

    ADD     EAX, 4

    LOOP    @CosLoop

    FSTP    [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
    FSTP    [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
    FSTP    ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
    FSTP    ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

    FSTP    [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
    FSTP    [EDX + 40].Double  // SpuaredCorrectionFactor
    POP     EBX
@Exit:
{$ENDIF}
end;

procedure DoWinLoopCos2T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFDEF PUREPASCAL}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (CoefficientPointer[0] - PDAV2DoubleArray(CoefficientPointer)^[1] * ComplexPosition.Im);
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end
{$ELSE}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(1)
   FMUL [EBX + 8].Double
   FADD [EBX].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
{$ENDIF}
end;

procedure DoWinLoopCos2T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(1)
   FMUL [EBX + 8].Double
   FADD [EBX].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4
   SUB EDI, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := CoefficientPointer[0] - PDAV2DoubleArray(CoefficientPointer)^[1] * ComplexPosition.Im;
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor  := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCos2T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(1)
   FMUL [EBX + 8].Double
   FADD [EBX].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8
   SUB EDI, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
@exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := CoefficientPointer[0] - PDAV2DoubleArray(CoefficientPointer)^[1] * ComplexPosition.Im;
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor  := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCos3T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := PDAV4DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im  *
      (PDAV4DoubleArray(CoefficientPointer)^[1] + ComplexPosition.Im  *
       PDAV4DoubleArray(CoefficientPointer)^[2]);
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCos3T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im  *
          (PDAV4DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im  *
           PDAV4DoubleArray(CoefficientPointer)[2]));
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos3T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4
   SUB EDI, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im  *
          (PDAV4DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im  *
           PDAV4DoubleArray(CoefficientPointer)[2]));
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    Assert(not IsNaN(CurrentValue));
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos3T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8
   SUB EDI, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im  *
          (PDAV4DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im  *
           PDAV4DoubleArray(CoefficientPointer)[2]));
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos4T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im  *
      (PDAV4DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im  *
      (PDAV4DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im  *
       PDAV4DoubleArray(CoefficientPointer)[3])));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos4T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
          (PDAV4DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
          (PDAV4DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
           PDAV4DoubleArray(CoefficientPointer)[3])));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos4T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4
   SUB EDI, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV4DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV4DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
       PDAV4DoubleArray(CoefficientPointer)[3])));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos4T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8
   SUB EDI, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV4DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV4DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV4DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
       PDAV4DoubleArray(CoefficientPointer)[3])));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos5T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
       PDAV6DoubleArray(CoefficientPointer)[4]))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos5T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
       PDAV6DoubleArray(CoefficientPointer)[4]))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos5T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4
   SUB EDI, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
       PDAV6DoubleArray(CoefficientPointer)[4]))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos5T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8
   SUB EDI, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
       PDAV6DoubleArray(CoefficientPointer)[4]))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos6T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
       PDAV6DoubleArray(CoefficientPointer)[5])))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos6T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
       PDAV6DoubleArray(CoefficientPointer)[5])))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos6T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4
   SUB EDI, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
       PDAV6DoubleArray(CoefficientPointer)[5])))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos6T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8
   SUB EDI, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV6DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV6DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
       PDAV6DoubleArray(CoefficientPointer)[5])))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos7T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
       PDAV8DoubleArray(CoefficientPointer)[6]))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos7T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
       PDAV8DoubleArray(CoefficientPointer)[6]))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos7T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4
   SUB EDI, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
       PDAV8DoubleArray(CoefficientPointer)[6]))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos7T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8
   SUB EDI, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im * 
      (PDAV8DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
       PDAV8DoubleArray(CoefficientPointer)[6]))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos8T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
       PDAV8DoubleArray(CoefficientPointer)[7])))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos8T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
       PDAV8DoubleArray(CoefficientPointer)[7])))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos8T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4
   SUB EDI, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV8DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
       PDAV8DoubleArray(CoefficientPointer)[7])))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos8T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8
   SUB EDI, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV8DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
        (PDAV8DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
        (PDAV8DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
        (PDAV8DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
        (PDAV8DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
        (PDAV8DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
        (PDAV8DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
         PDAV8DoubleArray(CoefficientPointer)[7])))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos9T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 64].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im *
       PDAV16DoubleArray(CoefficientPointer)[8]))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos9T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 64].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im *
       PDAV16DoubleArray(CoefficientPointer)[8]))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos9T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 64].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4
   SUB EDI, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im *
       PDAV16DoubleArray(CoefficientPointer)[8]))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos9T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 64].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8
   SUB EDI, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
      (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im *
       PDAV16DoubleArray(CoefficientPointer)[8]))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos10T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 72].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 64].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[8] + ComplexPosition.Im *
         PDAV16DoubleArray(CoefficientPointer)[9])))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos10T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 72].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 64].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[8] + ComplexPosition.Im * 
         PDAV16DoubleArray(CoefficientPointer)[9])))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos10T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 72].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 64].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4
   SUB EDI, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[8] + ComplexPosition.Im *
         PDAV16DoubleArray(CoefficientPointer)[9])))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos10T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 72].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 64].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8
   SUB EDI, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[8] + ComplexPosition.Im * 
         PDAV16DoubleArray(CoefficientPointer)[9])))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos11T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 80].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 72].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 64].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[8] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[9] + ComplexPosition.Im *
         PDAV16DoubleArray(CoefficientPointer)[10]))))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos11T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 80].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 72].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 64].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[8] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[9] + ComplexPosition.Im * 
         PDAV16DoubleArray(CoefficientPointer)[10]))))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos11T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 80].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 72].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 64].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4
   SUB EDI, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[8] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[9] + ComplexPosition.Im *
         PDAV16DoubleArray(CoefficientPointer)[10]))))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopCos11T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD [EBX + 80].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 72].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 64].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 56].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 48].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 40].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 32].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 24].Double
   FMUL ST(0), ST(2)
   FADD [EBX + 16].Double
   FMUL ST(0), ST(2)
   FADD [EBX +  8].Double
   FMUL ST(0), ST(2)
   FADD [EBX  ].Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8
   SUB EDI, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := (PDAV16DoubleArray(CoefficientPointer)[0] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[1] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[2] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[3] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[4] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[5] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[6] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[7] + ComplexPosition.Im * 
        (PDAV16DoubleArray(CoefficientPointer)[8] + ComplexPosition.Im *
        (PDAV16DoubleArray(CoefficientPointer)[9] + ComplexPosition.Im *
         PDAV16DoubleArray(CoefficientPointer)[10]))))))))));

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
   end;
end;
{$ENDIF}

procedure DoWinLoopTriangle32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 FLD [EDX     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq

 @triloop:
   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FADD ST(2), ST(0)
   FLD ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(4), ST(0)

   FADD ST(0), ST(1)
   ADD EAX, 4
 LOOP @triloop

 FSTP [EDX     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq
 FSTP [EDX + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 FSTP [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double   // SpuaredCorrectionFactor

 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + ComplexPosition.Re;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + Sqr(ComplexPosition.Re);
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * ComplexPosition.Re;
    ComplexPosition.Re := ComplexPosition.Re + ComplexAngle.Re;
   end;
end;
{$ENDIF}

procedure DoWinLoopTriangle32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 FLD [EDX     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq

 @CosLoop:
   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single
   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(2), ST(0)
   FADD ST(2), ST(0)
   FLD ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(4), ST(0)

   FADD ST(0), ST(1)                 // Cnt + Ofs, Ofs, fSpkCorFak, fSpkCorFakSq
   ADD EAX, 4
   SUB EDI, 4
 LOOP @CosLoop

 FSTP [EDX     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq
 FSTP [EDX + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 FSTP [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double   // SpuaredCorrectionFactor

 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    SpectrumCorrectionFactor   := SpectrumCorrectionFactor + 2 * ComplexPosition.Re;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + 2 * Sqr(ComplexPosition.Re);
    StartAdr[SampleIndex]  := StartAdr[SampleIndex] * ComplexPosition.Re;
    EndAdr[-SampleIndex]   := EndAdr[-SampleIndex] * ComplexPosition.Re;
    ComplexPosition.Re := ComplexPosition.Re + ComplexAngle.Re;
   end;
end;
{$ENDIF}

procedure DoWinLoopTriangle64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 FLD [EDX     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq

 @triloop:
   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FADD ST(2), ST(0)
   FLD ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(4), ST(0)

   FADD ST(0), ST(1)
   ADD EAX, 8
 LOOP @triloop

 FSTP [EDX     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq
 FSTP [EDX + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 FSTP [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double   // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    SpectrumCorrectionFactor := SpectrumCorrectionFactor + ComplexPosition.Re;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + Sqr(ComplexPosition.Re);
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * ComplexPosition.Re;
    ComplexPosition.Re := ComplexPosition.Re + ComplexAngle.Re;
   end
end;
{$ENDIF}

procedure DoWinLoopTriangle64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 FLD [EDX     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq

 @CosLoop:
   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double
   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(2), ST(0)
   FADD ST(2), ST(0)
   FLD ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(4), ST(0)

   FADD ST(0), ST(1)                 // Cnt + Ofs, Ofs, fSpkCorFak, fSpkCorFakSq
   ADD EAX, 8
   SUB EDI, 8
 LOOP @CosLoop

 FSTP [EDX     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq
 FSTP [EDX + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 FSTP [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double   // SpuaredCorrectionFactor

 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    SpectrumCorrectionFactor   := SpectrumCorrectionFactor + 2 * ComplexPosition.Re;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + 2 * Sqr(ComplexPosition.Re);
    StartAdr[SampleIndex]  := StartAdr[SampleIndex] * ComplexPosition.Re;
    EndAdr[-SampleIndex]   := EndAdr[-SampleIndex] * ComplexPosition.Re;
    ComplexPosition.Re := ComplexPosition.Re + ComplexAngle.Re;
   end;
end;
{$ENDIF}

procedure DoWinLoopCosine32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(0)
   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im
   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im
   ADD EAX, 4
 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCosine32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(0)
   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4
   SUB EDI, 4
 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor

 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCosine64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(0)
   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im
   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im
   ADD EAX, 8
 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopCosine64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(0)
   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im
   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im
   ADD EAX, 8
   SUB EDI, 8
 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopLanczos32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(0)
   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im
   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im
   ADD EAX, 4
 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopLanczos32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(0)
   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4
   SUB EDI, 4
 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor

 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopLanczos64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(0)
   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im
   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im
   ADD EAX, 8
 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopLanczos64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(0)
   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double
   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im
   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im
   ADD EAX, 8
   SUB EDI, 8
 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re-ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopHanning32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 MOV EBX, [EDX + 48]
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(1)
   FMUL CHanning.Double
   FSUB CHanning.Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopHanning32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(1)
   FMUL CHanning.Double
   FSUB CHanning.Double

   FLD [EAX].Single
   FMUL ST(0), ST(1)
   FSTP [EAX].Single

   FLD [EDI].Single
   FMUL ST(0), ST(1)
   FSTP [EDI].Single

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 4
   SUB EDI, 4

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopHanning64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
  OR      ECX, ECX
  JNG     @exit
  PUSH    EBX
  MOV     EBX, [EDX + 48]
  FLD     [EDX + 40].Double   // SpuaredCorrectionFactor
  FLD     [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
  FLD     [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
  FLD     [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
  FLD     [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
  FLD     [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

@CosLoop:
  FLD     ST(1)
  FMUL    CHanning.Double
  FSUB    CHanning.Double

  FLD     [EAX].Double
  FMUL    ST(0), ST(1)
  FSTP    [EAX].Double

  FADD    ST(5), ST(0)
  FMUL    ST(0), ST(0)
  FADDP   ST(6), ST(0)

  FLD     ST(3)         // Im, SampleIndex, i, Re, Im
  FMUL    ST(0), ST(2)  // Im * i, SampleIndex, i, Re, Im
  FLD     ST(3)         // Re, Im * i, SampleIndex, i, Re, Im
  FMUL    ST(0), ST(2)  // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
  FSUBRP                // newRe, SampleIndex, i, Re, Im

  FLD     ST(4)         // Im, newRe, SampleIndex, i, Re, Im
  FMULP   ST(2), ST(0)  // newRe, Im * SampleIndex, i, Re, Im
  FLD     ST(3)         // Re, newRe, Im * SampleIndex, i, Re, Im
  FMULP   ST(3), ST(0)  // newRe, Im * SampleIndex, Re * i, Re, Im
  FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
  FADDP   ST(2), ST(0)  // newRe, newIm, Re, Im

  ADD     EAX, 8

  LOOP    @CosLoop

  FSTP    [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
  FSTP    [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
  FSTP    ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
  FSTP    ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

  FSTP    [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
  FSTP    [EDX + 40].Double  // SpuaredCorrectionFactor
  POP     EBX
@Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

procedure DoWinLoopHanning64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 OR ECX, ECX
 JNG @exit
 PUSH EBX
 PUSH EDI
 MOV EBX, [EDX + 48]
 MOV EDI, EndAdr
 FLD [EDX + 40].Double   // SpuaredCorrectionFactor
 FLD [EDX + 32].Double   // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 24].Double   // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX + 16].Double   // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX +  8].Double   // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FLD [EDX     ].Double   // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 @CosLoop:
   FLD ST(1)
   FMUL CHanning.Double
   FSUB CHanning.Double

   FLD [EAX].Double
   FMUL ST(0), ST(1)
   FSTP [EAX].Double

   FLD [EDI].Double
   FMUL ST(0), ST(1)
   FSTP [EDI].Double

   FADD ST(5), ST(0)
   FADD ST(5), ST(0)
   FMUL ST(0), ST(0)
   FADDP ST(6), ST(0)

   FLD ST(3)             // Im, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Im * i, SampleIndex, i, Re, Im
   FLD ST(3)             // Re, Im * i, SampleIndex, i, Re, Im
   FMUL ST(0), ST(2)     // Re * SampleIndex, Im * i, SampleIndex, i, Re, Im
   FSUBRP                // newRe, SampleIndex, i, Re, Im

   FLD ST(4)             // Im, newRe, SampleIndex, i, Re, Im
   FMULP ST(2), ST(0)    // newRe, Im * SampleIndex, i, Re, Im
   FLD ST(3)             // Re, newRe, Im * SampleIndex, i, Re, Im
   FMULP ST(3), ST(0)    // newRe, Im * SampleIndex, Re * i, Re, Im
   FXCH                  // Im * SampleIndex, newRe, Re * i, Re, Im
   FADDP ST(2), ST(0)    // newRe, newIm, Re, Im

   ADD EAX, 8
   SUB EDI, 8

 LOOP @CosLoop

 FSTP [EDX     ].Double  // ComplexPosition.Re, ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX +  8].Double  // ComplexPosition.Im, ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Re, ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP ST(0)              // ComplexAngle.Im, SpectrumCorrectionFactor, SpuaredCorrectionFactor

 FSTP [EDX + 32].Double  // SpectrumCorrectionFactor, SpuaredCorrectionFactor
 FSTP [EDX + 40].Double  // SpuaredCorrectionFactor
 POP EDI
 POP EBX
 @Exit:
end;
{$ELSE}
var
  SampleIndex  : Integer;
  CurrentValue : Double;
begin
 for SampleIndex := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    CurrentValue := ComplexPosition.Re * ComplexAngle.Re - ComplexPosition.Im * ComplexAngle.Im;
    ComplexPosition.Im := ComplexPosition.Im * ComplexAngle.Re + ComplexPosition.Re * ComplexAngle.Im;
    ComplexPosition.Re := CurrentValue;
    CurrentValue := (0.5 -  0.5 * ComplexPosition.Im);

    SpectrumCorrectionFactor := SpectrumCorrectionFactor + CurrentValue;
    SpuaredCorrectionFactor  := SpuaredCorrectionFactor + CurrentValue * CurrentValue;
    StartAdr[SampleIndex] := StartAdr[SampleIndex] * CurrentValue;
    EndAdr[SampleCount - 1 - SampleIndex] := EndAdr[SampleCount - 1 - SampleIndex] * CurrentValue;
   end
end;
{$ENDIF}

end.
