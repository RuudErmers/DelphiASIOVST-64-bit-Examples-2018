unit DAV_DspFilterTransform;

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
  DAV_Types, DAV_Complex;

procedure MakeLinearPhase32(const FilterKernel: PDAVSingleFixedArray; const SampleFrames: Integer);
procedure MakeLinearPhase64(const FilterKernel: PDAVDoubleFixedArray; const SampleFrames: Integer);
procedure MakeMinimumPhase32(const FilterKernel: PDAVSingleFixedArray; const SampleFrames: Integer);
procedure MakeMinimumPhase64(const FilterKernel: PDAVDoubleFixedArray; const SampleFrames: Integer);

procedure ConvertReImToSquaredMagnitude32(const FrequencyDomain: PDAVComplexSingleFixedArray; const SqMagDomain: PDAVSingleFixedArray; BinCount: Integer);
procedure ConvertReImToSquaredMagnitude64(const FrequencyDomain: PDAVComplexDoubleFixedArray; const SqMagDomain: PDAVDoubleFixedArray; BinCount: Integer);

implementation

uses
  Math, DAV_Math, DAV_DspFftReal2Complex
  {$IFDEF USE_IPPS}, DAV_DspFftReal2ComplexIpps{$ENDIF};

procedure MakeLinearPhase32(const FilterKernel: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  {$IFDEF USE_IPPS}
  FFT         : TFftReal2ComplexIPPSFloat32;
  {$ELSE}
  FFT         : TFftReal2ComplexNativeFloat32;
  {$ENDIF}
  TimeDomain  : PDAVSingleFixedArray;
  FreqDomain  : PDAVComplexSingleFixedArray;
  i           : Integer;
begin
 i   := ExtendToPowerOf2(SampleFrames);
 i   := Round(Log2(i));

  {$IFDEF USE_IPPS}
 FFT := TFftReal2ComplexIPPSFloat32.Create(i);
 FFT.DataOrder := doComplex;
 {$ELSE}
 FFT := TFftReal2ComplexNativeFloat32.Create(i);
 FFT.DataOrder := doPackedComplex;
 {$ENDIF}
 with FFT do
  try
   AutoScaleType := astDivideBySqrtN;

   GetMem(TimeDomain, FFTSize * SizeOf(Single));
   GetMem(FreqDomain, BinCount * SizeOf(TComplex32));
   Move(FilterKernel[0], TimeDomain[0], SampleFrames * SizeOf(Single));

   // fill extended part with zeroes
   if (FFTSize - SampleFrames) > 0
    then FillChar(TimeDomain[SampleFrames], (FFTSize - SampleFrames) * SizeOf(Single), 0);

   // transform time signal to frequency domain
   PerformFFT(@FreqDomain[0], @TimeDomain[0]);

   // make linear phase and copy magnitude to real part
   for i := 1 to BinCount - 2 do
    begin
     FreqDomain[i].Re := Sqrt(sqr(FreqDomain[i].Re) + sqr(FreqDomain[i].Im));
     FreqDomain[i].Im := 0;
    end;

   PerformiFFT(@FreqDomain[0], @TimeDomain[0]);

   // fit impulse response to old length
   if SampleFrames < FFTSize then
    begin
     Move(TimeDomain[0], FilterKernel[0], (SampleFrames div 2) * SizeOf(Single));
     Move(TimeDomain[FFTSize - (SampleFrames div 2) - 1], FilterKernel[(SampleFrames div 2)], (SampleFrames div 2) * SizeOf(Single));
    end
   else Move(TimeDomain[0], FilterKernel[0], SampleFrames * SizeOf(Single));

   Dispose(TimeDomain);
   Dispose(FreqDomain);
  finally
   FFT.Free;
  end;
end;

procedure MakeLinearPhase64(const FilterKernel: PDAVDoubleFixedArray; const SampleFrames: Integer);
var
  {$IFDEF USE_IPPS}
  FFT         : TFftReal2ComplexIPPSFloat64;
  {$ELSE}
  FFT         : TFftReal2ComplexNativeFloat64;
  {$ENDIF}
  TimeDomain  : PDAVDoubleFixedArray;
  FreqDomain  : PDAVComplexDoubleFixedArray;
  i           : Integer;
begin
 i   := ExtendToPowerOf2(SampleFrames);
 i   := Round(Log2(i));
  {$IFDEF USE_IPPS}
 FFT := TFftReal2ComplexIPPSFloat64.Create(i);
 FFT.DataOrder := doComplex;
 {$ELSE}
 FFT := TFftReal2ComplexNativeFloat64.Create(i);
 FFT.DataOrder := doPackedComplex;
 {$ENDIF}
 with FFT do
  try
   AutoScaleType := astDivideBySqrtN;

   GetMem(TimeDomain, FFTSize * SizeOf(Double));
   GetMem(FreqDomain, BinCount * SizeOf(TComplex64));
   Move(FilterKernel[0], TimeDomain[0], SampleFrames * SizeOf(Double));

   // fill extended part with zeroes
   if (FFTSize - SampleFrames) > 0
    then FillChar(TimeDomain[SampleFrames], (FFTSize - SampleFrames) * SizeOf(Double), 0);

   // transform time signal to frequency domain
   PerformFFT(@FreqDomain[0], @TimeDomain[0]);

   // make linear phase and copy magnitude to real part
   for i := 1 to BinCount - 2 do
    begin
     FreqDomain[i].Re := Sqrt(sqr(FreqDomain[i].Re) + sqr(FreqDomain[i].Im));
     FreqDomain[i].Im := 0;
    end;

   PerformiFFT(@FreqDomain[0], @TimeDomain[0]);

   // fit impulse response to old length
   if SampleFrames < FFTSize then
    begin
     Move(TimeDomain[0], FilterKernel[0], (SampleFrames div 2) * SizeOf(Double));
     Move(TimeDomain[FFTSize - (SampleFrames div 2) - 1], FilterKernel[(SampleFrames div 2)], (SampleFrames div 2) * SizeOf(Double));
    end
   else Move(TimeDomain[0], FilterKernel[0], SampleFrames * SizeOf(Double));

   Dispose(TimeDomain);
   Dispose(FreqDomain);
  finally
   Free;
  end;
end;

procedure MakeMinimumPhase32(const FilterKernel : PDAVSingleFixedArray; const SampleFrames: Integer);
var
  {$IFDEF USE_IPPS}
  FFT         : TFftReal2ComplexIPPSFloat32;
  {$ELSE}
  FFT         : TFftReal2ComplexNativeFloat32;
  {$ENDIF}
  TimeDomain  : PDAVSingleFixedArray;
  FreqDomain  : PDAVComplexSingleFixedArray;
  Magnitude   : PDAVSingleFixedArray;
  i, h        : Integer;
begin
 // Choose an FFT size larger than needed to achieve more accuracy
 i   := ExtendToPowerOf2(3 * SampleFrames);
 i   := round(Log2(i));
  {$IFDEF USE_IPPS}
 FFT := TFftReal2ComplexIPPSFloat32.Create(i);
 FFT.DataOrder := doComplex;
 {$ELSE}
 FFT := TFftReal2ComplexNativeFloat32.Create(i);
 FFT.DataOrder := doPackedComplex;
 {$ENDIF}
 with FFT do
  try
   h := FFTSize div 2;

   // reserve memory
   GetMem(TimeDomain, FFTSize * SizeOf(Single));
   GetMem(FreqDomain, FFTSize * SizeOf(TComplex32));
   GetMem(Magnitude,  FFTSize * SizeOf(Single));
   try

     // copy signal to internal time domain buffer
     Move(FilterKernel[0], TimeDomain[0], SampleFrames * SizeOf(Single));

     // clear buffers
     FillChar(TimeDomain[SampleFrames], (FFTSize - SampleFrames) * SizeOf(Single), 0);
     FillChar(FreqDomain[0], FFTSize * SizeOf(Single), 0);
     FillChar(Magnitude[0],  FFTSize * SizeOf(Single), 0);

     AutoScaleType := astDivideNoDivByAny;
     PerformFFT(@FreqDomain[0], @TimeDomain[0]);

     // calculate magnitude
     for i := 0 to BinCount - 1
      do Magnitude[i] := Sqrt(sqr(FreqDomain[i].Re) + sqr(FreqDomain[i].Im));

     // logarithmize magnitude -> real
     for i := 0 to BinCount - 1 do
      begin
       FreqDomain[i].Re := ln(Magnitude[i]);
       FreqDomain[i].Im := 0;
      end;

     // calculate cepstrum
     PerformiFFT(@FreqDomain[0], @TimeDomain[0]);

     // clear first half
     TimeDomain[0] := 0.5 * TimeDomain[0];
     TimeDomain[h] := 0.5 * TimeDomain[h];
     for i := 1 to h - 1 do TimeDomain[i] := 0;

     PerformFFT(@FreqDomain[0], @TimeDomain[0]);

     // restore complex from magnitude and the new phase
     for i := 0 to BinCount - 1 do
      begin
       FreqDomain[i].Re :=  cos(FreqDomain[i].Im / h) * Magnitude[i];
       FreqDomain[i].Im := -sin(FreqDomain[i].Im / h) * Magnitude[i];
      end;
     AutoScaleType := astDivideInvByN;
     PerformiFFT(@FreqDomain[0], @TimeDomain[0]);

     // copy signal to internal time domain buffer
     Move(TimeDomain[0], FilterKernel[0], SampleFrames * SizeOf(Single));
   finally
    // dispose memory
    Dispose(TimeDomain);
    Dispose(FreqDomain);
    Dispose(Magnitude);
   end;
  finally
   Free;
  end;
end;

procedure MakeMinimumPhase64(const FilterKernel : PDAVDoubleFixedArray; const SampleFrames: Integer);
var
  {$IFDEF USE_IPPS}
  FFT         : TFftReal2ComplexIPPSFloat64;
  {$ELSE}
  FFT         : TFftReal2ComplexNativeFloat64;
  {$ENDIF}
  TimeDomain  : PDAVDoubleFixedArray;
  FreqDomain  : PDAVComplexDoubleFixedArray;
  Magnitude   : PDAVDoubleFixedArray;
  i, h        : Integer;
begin
 // Choose an FFT size larger than needed to achieve more accuracy
 i   := ExtendToPowerOf2(3 * SampleFrames);
 i   := round(Log2(i));
  {$IFDEF USE_IPPS}
 FFT := TFftReal2ComplexIPPSFloat64.Create(i);
 FFT.DataOrder := doComplex;
 {$ELSE}
 FFT := TFftReal2ComplexNativeFloat64.Create(i);
 FFT.DataOrder := doPackedComplex;
 {$ENDIF}
 with FFT do
  try
   h := FFTSize div 2;

   // reserve memory
   GetMem(TimeDomain, FFTSize * SizeOf(Double));
   GetMem(FreqDomain, FFTSize * SizeOf(TComplex64));
   GetMem(Magnitude,  FFTSize * SizeOf(Double));
   try

     // copy signal to internal time domain buffer
     Move(FilterKernel[0], TimeDomain[0], SampleFrames * SizeOf(Double));

     // clear buffers
     FillChar(TimeDomain[SampleFrames], (FFTSize - SampleFrames) * SizeOf(Double), 0);
     FillChar(FreqDomain[0], FFTSize * SizeOf(Double), 0);
     FillChar(Magnitude[0],  FFTSize * SizeOf(Double), 0);

     AutoScaleType := astDivideNoDivByAny;
     PerformFFT(@FreqDomain[0], @TimeDomain[0]);

     // calculate magnitude
     for i := 0 to BinCount - 1
      do Magnitude[i] := Sqrt(sqr(FreqDomain[i].Re) + sqr(FreqDomain[i].Im));

     // logarithmize magnitude -> real
     for i := 0 to BinCount - 1 do
      begin
       FreqDomain[i].Re := ln(Magnitude[i]);
       FreqDomain[i].Im := 0;
      end;

     // calculate cepstrum
     PerformiFFT(@FreqDomain[0], @TimeDomain[0]);

     // clear first half
     TimeDomain[0] := 0.5 * TimeDomain[0];
     TimeDomain[h] := 0.5 * TimeDomain[h];
     for i := 1 to h - 1 do TimeDomain[i] := 0;

     PerformFFT(@FreqDomain[0], @TimeDomain[0]);

     // restore complex from magnitude and the new phase
     for i := 0 to BinCount - 1 do
      begin
       FreqDomain[i].Re :=  cos(FreqDomain[i].Im / h) * Magnitude[i];
       FreqDomain[i].Im := -sin(FreqDomain[i].Im / h) * Magnitude[i];
      end;
     AutoScaleType := astDivideInvByN;
     PerformiFFT(@FreqDomain[0], @TimeDomain[0]);

     // copy signal to internal time domain buffer
     Move(TimeDomain[0], FilterKernel[0], SampleFrames * SizeOf(Double));
   finally
    // dispose memory
    Dispose(TimeDomain);
    Dispose(FreqDomain);
    Dispose(Magnitude);
   end;
  finally
   Free;
  end;
end;

procedure ConvertReImToSquaredMagnitude32(const FrequencyDomain: PDAVComplexSingleFixedArray; const SqMagDomain: PDAVSingleFixedArray; BinCount: Integer);
var
  i : Integer;
begin
 for i := 0 to BinCount - 1
  do SqMagDomain[i] := sqr(FrequencyDomain[i].Re) + sqr(FrequencyDomain[i].Im);
end;

procedure ConvertReImToSquaredMagnitude64(const FrequencyDomain: PDAVComplexDoubleFixedArray; const SqMagDomain: PDAVDoubleFixedArray; BinCount: Integer);
var
  i : Integer;
begin
 for i := 0 to BinCount - 1
  do SqMagDomain[i] := sqr(FrequencyDomain[i].Re) + sqr(FrequencyDomain[i].Im);
end;

end.
