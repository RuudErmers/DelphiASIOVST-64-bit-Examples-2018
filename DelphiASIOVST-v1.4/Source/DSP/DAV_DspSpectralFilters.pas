unit DAV_DspSpectralFilters;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_Complex, DAV_DspSpectralEffects,
  DAV_DspWindowFunctions, DAV_DspFftReal2Complex;

// TODO: check and implement all assignto functions!!!

type
  TCustomSpectralFilter32 = class(TCustomSpectralEffect32)
  private
    procedure SetWindowClass(const Value: TWindowFunctionClass);
  protected
    FFilterIR       : PDAVSingleFixedArray;
    FAddTimeBuffer  : PDAVSingleFixedArray;
    FAddSpecBuffer  : PDAVComplexSingleFixedArray;
    FFilter         : PDAVComplexSingleFixedArray;
    FWindowFunction : TCustomWindowFunction;
    FWindowClass    : TWindowFunctionClass;
    procedure BuildFilter(Spectrum: PDAVComplexSingleFixedArray); virtual; abstract;
    procedure WindowFilterIR; virtual;
    procedure FFTOrderChanged; override;
    procedure WindowFunctionClassChanged; virtual;
    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVSingleFixedArray); overload; override;
    procedure PerformSpectralEffect(Spectrum: PDAVComplexSingleFixedArray); overload; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property WindowFunctionClass : TWindowFunctionClass read FWindowClass write SetWindowClass;
  end;

  TCustomTimeConstantFilter32 = class(TCustomSpectralFilter32)
  private
    FDecay : Single;
    procedure SetDecay(const Value: Single);
  protected
    FDecayFactor : Single;
    procedure CalculateDecayFactor; virtual;
    procedure DecayChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    property Decay: Single read FDecay write SetDecay;
  end;

  TAveragedSelfFiltering32 = class(TCustomTimeConstantFilter32, IDspProcessor32)
  protected
    procedure BuildFilter(Spectrum: PDAVComplexSingleFixedArray); override;
  published
    property Decay;
    property FFTOrder;
    property FFTSize;
    property WindowFunctionClass;
  end;

  TPeakSelfFiltering32 = class(TCustomTimeConstantFilter32, IDspProcessor32)
  protected
    procedure BuildFilter(Spectrum: PDAVComplexSingleFixedArray); override;
  published
    property Decay;
    property FFTOrder;
    property FFTSize;
    property WindowFunctionClass;
  end;

  TSpectralSmoothing32 = class(TCustomSpectralFilter32, IDspProcessor32)
  protected
    FMagnitudeBuffer : PDAVSingleFixedArray;
    procedure BuildFilter(Spectrum: PDAVComplexSingleFixedArray); override;
    procedure FFTOrderChanged; override;
  published
    property FFTOrder;
    property FFTSize;
    property WindowFunctionClass;
  end;

  TAdjustPhase32 = class(TCustomSpectralFilter32, IDspProcessor32)
  protected
    procedure BuildFilter(Spectrum: PDAVComplexSingleFixedArray); override;
  published
    property FFTOrder;
    property FFTSize;
    property WindowFunctionClass;
  end;

  // 64 bit

  TCustomSpectralFilter64 = class(TCustomSpectralEffect64)
  private
    procedure SetWindowClass(const Value: TWindowFunctionClass);
  protected
    FFilterIR       : PDAVDoubleFixedArray;
    FAddTimeBuffer  : PDAVDoubleFixedArray;
    FAddSpecBuffer  : PDAVComplexDoubleFixedArray;
    FFilter         : PDAVComplexDoubleFixedArray;
    FWindowFunction : TCustomWindowFunction;
    FWindowClass    : TWindowFunctionClass;
    procedure BuildFilter(Spectrum: PDAVComplexDoubleFixedArray); virtual; abstract;
    procedure FFTOrderChanged; override;
    procedure WindowFunctionClassChanged; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    property WindowFunctionClass : TWindowFunctionClass read FWindowClass write SetWindowClass;
  end;

  TSpectralFilter64 = class(TCustomSpectralFilter64, IDspProcessor64)
  private
    FThresholdFactor : Double;
    FThreshold       : Double;
    procedure SetThreshold(const Value: Double);
  protected
    procedure BuildFilter(Spectrum: PDAVComplexDoubleFixedArray); override;
    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVDoubleFixedArray); overload; override;
    procedure PerformSpectralEffect(Spectrum: PDAVComplexDoubleFixedArray); overload; override;

    procedure ThresholdChanged; virtual;
  public
    constructor Create; override;
  published
    property Threshold: Double read FThreshold write SetThreshold;

    property FFTOrder;
    property FFTSize;
    property WindowFunctionClass;
  end;


implementation

uses
  SysUtils, DAV_Common, DAV_BlockProcessing, DAV_Approximations;

{ TCustomSpectralFilter32 }

constructor TCustomSpectralFilter32.Create;
begin
 FFilterIR      := nil;
 FAddTimeBuffer := nil;
 FAddSpecBuffer := nil;
 FFilter        := nil;

 FWindowClass   := TWindowFunctionBlackman;
 WindowFunctionClassChanged;

 inherited;
end;

destructor TCustomSpectralFilter32.Destroy;
begin
 Dispose(FFilterIR);
 Dispose(FAddTimeBuffer);
 Dispose(FAddSpecBuffer);
 Dispose(FFilter);

 FreeAndNil(FWindowFunction);

 inherited;
end;

procedure TCustomSpectralFilter32.FFTOrderChanged;
begin
 inherited;
 ReallocMem(FAddTimeBuffer, FFFTSize * SizeOf(Single));
 ReallocMem(FAddSpecBuffer, Fft.BinCount * SizeOf(TComplex32));
 ReallocMem(FFilter, Fft.BinCount * SizeOf(TComplex32));
 ReallocMem(FFilterIR, FFFTSize * SizeOf(Single));
 FillChar(FAddTimeBuffer^, FFFTSize * SizeOf(Single), 0);
 FillChar(FAddSpecBuffer^, Fft.BinCount * SizeOf(TComplex32), 0);
 FillChar(FFilter^, Fft.BinCount * SizeOf(TComplex32), 0);
 FillChar(FFilterIR^, FFFTSize * SizeOf(Single), 0);

 if Assigned(FWindowFunction)
  then FWindowFunction.Length := FFTSize;
end;

procedure TCustomSpectralFilter32.SetWindowClass(
  const Value: TWindowFunctionClass);
begin
 if FWindowClass <> Value then
  begin
   FWindowClass := Value;
   WindowFunctionClassChanged;
  end;
end;

procedure TCustomSpectralFilter32.WindowFilterIR;
begin
 // transform filter to time domain
 FFft.PerformIFFT(FFilter, FFilterIR);

 // shift filter impulse response and apply window function
 Move(FFilterIR^[0], FFilterIR^[FFFTSizeHalf div 2], FFFTSizeHalf div 2 * SizeOf(Single));
 Move(FFilterIR^[FFFTSize - FFFTSizeHalf div 2], FFilterIR^[0], FFFTSizeHalf div 2 * SizeOf(Single));
 FWindowFunction.ProcessBlock32(@FFilterIR^[0], FFFTSizeHalf);
 FillChar(FFilterIR^[FFFTSizeHalf], FFFTSizeHalf * SizeOf(Single), 0);

 // transform impulse response back to frequency domain
 FFft.PerformFFT(FFilter, FFilterIR);
end;

procedure TCustomSpectralFilter32.WindowFunctionClassChanged;
var
  OldWindow : TCustomWindowFunction;
begin
 OldWindow := FWindowFunction;

 FWindowFunction := FWindowClass.Create;

 with FWindowFunction do
  begin
   Start := 0;
   Slope := wsSymmetric;
  end;

 if assigned(OldWindow) then
  begin
   FWindowFunction.Length := OldWindow.Length;
   FreeAndNil(OldWindow);
  end;
end;

procedure TCustomSpectralFilter32.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVSingleFixedArray);
var
  Sample : Integer;
  Reci   : Double;
  Scale  : Double;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);

 Move(FSignalFreq^, FAddSpecBuffer^, Fft.BinCount * SizeOf(TComplex32));
 PerformSpectralEffect(FAddSpecBuffer);
 BuildFilter(FSignalFreq);
 PerformSpectralEffect(FSignalFreq);
 FFft.PerformIFFT(FSignalFreq, SignalOut);
 FFft.PerformIFFT(FAddSpecBuffer, FAddTimeBuffer);

 Reci := 1 / FFFTSizeHalf;
 for Sample := 0 to FFFTSizeHalf - 1 do
  begin
   Scale := Sample * Reci;
   SignalOut^[Sample] := Scale * SignalOut^[FFFTSizeHalf + Sample] +
     (1 - Scale) * FAddTimeBuffer^[FFFTSizeHalf + Sample];
  end;
end;

procedure TCustomSpectralFilter32.PerformSpectralEffect(Spectrum: PDAVComplexSingleFixedArray);
begin
 ComplexMultiplyBlock32(Spectrum, FFilter, FFFTSizeHalf);
end;


{ TCustomTimeConstantFilter32 }

constructor TCustomTimeConstantFilter32.Create;
begin
 inherited;
 FDecay := 20;
 FDecayFactor := 0.5;
end;

procedure TCustomTimeConstantFilter32.SampleRateChanged;
begin
 CalculateDecayFactor;
 inherited;
end;

procedure TCustomTimeConstantFilter32.SetDecay(const Value: Single);
begin
 if FDecay <> Value then
  begin
   FDecay := Value;
   DecayChanged;
  end;
end;

procedure TCustomTimeConstantFilter32.DecayChanged;
begin
 CalculateDecayFactor;
end;

procedure TCustomTimeConstantFilter32.CalculateDecayFactor;
begin
 FDecayFactor := Exp( -ln2 / (FDecay * 0.001 * SampleRate));
end;


{ TAveragedSelfFiltering32 }

procedure TAveragedSelfFiltering32.BuildFilter(Spectrum: PDAVComplexSingleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
const
  COffset : Single = 1E-10;
begin
 Half := FFFTSizeHalf;

 // DC bin
 FFilter^[0].Re := FDecayFactor * FFilter^[0].Re +
   (1 - FDecayFactor) * Abs(Spectrum^[0].Re);
 FFilter^[0].Im := 0;

 // other bins
 for Bin := 1 to Half - 1 do
  begin
   FFilter^[Bin].Re := FDecayFactor * FFilter^[Bin].Re +
     (1 - FDecayFactor) * FastSqrtBab2(Sqr(Spectrum^[Bin].Re) + Sqr(Spectrum^[Bin].Im));
   FFilter^[Bin].Im := 0;
  end;

 // Nyquist bin
 FFilter^[Half].Re := FDecayFactor * FFilter^[Half].Re +
   (1 - FDecayFactor) * Abs(Spectrum^[Half].Re);
 FFilter^[Half].Im := 0;

 WindowFilterIR;
end;


{ TPeakSelfFiltering32 }

procedure TPeakSelfFiltering32.BuildFilter(Spectrum: PDAVComplexSingleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
  Temp : Double;
begin
 Half := FFFTSizeHalf;

 // DC bin
 if Abs(Spectrum^[0].Re) > FFilter^[0].Re
  then FFilter^[0].Re := Abs(Spectrum^[0].Re);
 FFilter^[0].Re := FDecayFactor * FFilter^[0].Re;
 FFilter^[0].Im := 0;

 // other bins
 for Bin := 1 to Half - 1 do
  begin
   Temp := FastSqrtBab2(Sqr(Spectrum^[Bin].Re) + Sqr(Spectrum^[Bin].Im));
   if Temp > FFilter^[Bin].Re
    then FFilter^[Bin].Re := Temp;

   FFilter^[Bin].Re := FDecayFactor * FFilter^[Bin].Re;
   FFilter^[Bin].Im := 0;
  end;

 // Nyquist bin
 if Abs(Spectrum^[Half].Re) > FFilter^[Half].Re
  then FFilter^[Half].Re := Abs(Spectrum^[Half].Re);
 FFilter^[Half].Re := FDecayFactor * FFilter^[Half].Re;
 FFilter^[Half].Im := 0;

 WindowFilterIR;
end;


{ TSpectralSmoothing32 }

procedure TSpectralSmoothing32.BuildFilter(
  Spectrum: PDAVComplexSingleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
begin
 // eventually logarithmize magnitude!

 Half := FFFTSizeHalf;

 // DC bin
 FMagnitudeBuffer^[0] := Abs(FFilter^[0].Re);

 // other bins
 for Bin := 1 to Half - 1
  do FMagnitudeBuffer^[Bin] := FastSqrtBab2(Sqr(Spectrum^[Bin].Re) + Sqr(Spectrum^[Bin].Im));

 FMagnitudeBuffer^[Half] := Abs(FFilter^[Half].Re);

 // DC bin
 Bin := 1;
 FFilter^[0].Re := 0.5 * (FMagnitudeBuffer^[0] + 0.5 * FMagnitudeBuffer^[Bin]) - FMagnitudeBuffer^[0];
 FFilter^[0].Im := 0;

 // other bins
 for Bin := 1 to Half - 1 do
  begin
   FFilter^[Bin].Re :=  0.5 * (FMagnitudeBuffer^[Bin] +
     0.5 * (FMagnitudeBuffer^[Bin + 1] + FMagnitudeBuffer^[Bin - 1])) - FMagnitudeBuffer^[Bin];
   FFilter^[Bin].Im := 0;
  end;

 // Nyquist bin
 FFilter^[Half].Re := 0.5 * (FMagnitudeBuffer^[Half] + 0.5 * FMagnitudeBuffer^[Half - 1]) - FMagnitudeBuffer^[Half];
 FFilter^[Half].Im := 0;

 WindowFilterIR;
end;

procedure TSpectralSmoothing32.FFTOrderChanged;
begin
 inherited;
 ReallocMem(FMagnitudeBuffer, Fft.BinCount * SizeOf(Single));
 FillChar(FMagnitudeBuffer^, Fft.BinCount * SizeOf(Single), 0);
end;


{ TAdjustPhase32 }

procedure TAdjustPhase32.BuildFilter(Spectrum: PDAVComplexSingleFixedArray);
var
  BinIndex  : Integer;
  Half      : Integer;
  Phase     : Single;
  Magnitude : Single;
begin
 Half := FFFTSizeHalf;

 // DC BinIndex
 // leave untouched

 // other bins
 for BinIndex := 1 to Half - 1 do
  begin
   Phase := ComplexArgument32(FFilter^[BinIndex]);
   Magnitude := 1;
   FFilter^[BinIndex] := ComplexPolar32(Magnitude, Phase);
  end;

 // Nyquist BinIndex
 // leave untouched

 WindowFilterIR;
end;


{ TCustomSpectralFilter64 }

constructor TCustomSpectralFilter64.Create;
begin
 FFilterIR      := nil;
 FAddTimeBuffer := nil;
 FAddSpecBuffer := nil;
 FFilter        := nil;

 FWindowClass   := TWindowFunctionBlackman;
 WindowFunctionClassChanged;

 inherited;
end;

destructor TCustomSpectralFilter64.Destroy;
begin
 Dispose(FFilterIR);
 Dispose(FAddTimeBuffer);
 Dispose(FAddSpecBuffer);
 Dispose(FFilter);

 FreeAndNil(FWindowFunction);

 inherited;
end;

procedure TCustomSpectralFilter64.FFTOrderChanged;
begin
 inherited;
 ReallocMem(FAddTimeBuffer, FFFTSize * SizeOf(Double));
 ReallocMem(FAddSpecBuffer, Fft.BinCount * SizeOf(TComplex64));
 ReallocMem(FFilter, Fft.BinCount * SizeOf(TComplex64));
 ReallocMem(FFilterIR, FFFTSize * SizeOf(Double));
 FillChar(FAddTimeBuffer^, FFFTSize * SizeOf(Double), 0);
 FillChar(FAddSpecBuffer^, Fft.BinCount * SizeOf(TComplex64), 0);
 FillChar(FFilter^, Fft.BinCount * SizeOf(TComplex64), 0);
 FillChar(FFilterIR^, FFFTSize * SizeOf(Double), 0);

 if Assigned(FWindowFunction)
  then FWindowFunction.Length := FFTSize;
end;

procedure TCustomSpectralFilter64.SetWindowClass(
  const Value: TWindowFunctionClass);
begin
 if FWindowClass <> Value then
  begin
   FWindowClass := Value;
   WindowFunctionClassChanged;
  end;
end;

procedure TCustomSpectralFilter64.WindowFunctionClassChanged;
var
  OldWindow : TCustomWindowFunction;
begin
 OldWindow := FWindowFunction;

 FWindowFunction := FWindowClass.Create;

 with FWindowFunction do
  begin
   Start := 0;
   Slope := wsSymmetric;
  end;

 if assigned(OldWindow) then
  begin
   FWindowFunction.Length := OldWindow.Length;
   FreeAndNil(OldWindow);
  end;
end;


{ TSpectralFilter64 }

constructor TSpectralFilter64.Create;
begin
 inherited;
 FThresholdFactor := 1E-3;
end;

procedure TSpectralFilter64.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVDoubleFixedArray);
var
  Sample : Integer;
  Reci   : Double;
  Scale  : Double;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);

 Move(FSignalFreq^, FAddSpecBuffer^, Fft.BinCount * SizeOf(TComplex64));
 PerformSpectralEffect(FAddSpecBuffer);
 BuildFilter(FSignalFreq);
 PerformSpectralEffect(FSignalFreq);
 FFft.PerformIFFT(FSignalFreq, SignalOut);
 FFft.PerformIFFT(FAddSpecBuffer, FAddTimeBuffer);

 Reci := 1 / FFFTSizeHalf;
 for Sample := 0 to FFFTSizeHalf - 1 do
  begin
   Scale := Sample * Reci;
   SignalOut^[Sample] := Scale * SignalOut^[FFFTSizeHalf + Sample] +
     (1 - Scale) * FAddTimeBuffer^[FFFTSizeHalf + Sample];
  end;
end;

procedure TSpectralFilter64.BuildFilter(Spectrum: PDAVComplexDoubleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
const
  COffset : Double = 1E-10;
begin
 Half := FFFTSizeHalf;

 // DC bin
 if Spectrum^[0].Re > FThreshold
  then FFilter^[0].Re := CHalf64 * (1 + FFilter^[0].Re)
  else FFilter^[0].Re := CHalf64 * FFilter^[0].Re;
 FFilter^[0].Im := 0;

 // other bins
 for Bin := 1 to Half - 1 do
  begin
   if FastSqrtBab2(Sqr(Spectrum^[Bin].Re) + Sqr(Spectrum^[Bin].Im)) > FThreshold
    then FFilter^[Bin].Re := CHalf64 * (1 + FFilter^[Bin].Re)
    else FFilter^[Bin].Re := CHalf64 * FFilter^[Bin].Re;
   FFilter^[Bin].Im := 0;
  end;

 // Nyquist bin
 if Spectrum^[Half].Re > FThreshold
  then FFilter^[Half].Re := CHalf64 * (1 + FFilter^[Half].Re)
  else FFilter^[Half].Re := CHalf64 * FFilter^[Half].Re;
 FFilter^[Half].Im := 0;

 // transform filter to time domain
 FFft.PerformIFFT(FFilter, FFilterIR);

 // shift filter impulse response and apply window function
 Move(FFilterIR^[0], FFilterIR^[FFFTSizeHalf div 2], FFFTSizeHalf div 2 * SizeOf(Double));
 Move(FFilterIR^[FFFTSize - FFFTSizeHalf div 2], FFilterIR^[0], FFFTSizeHalf div 2 * SizeOf(Double));
 FWindowFunction.ProcessBlock64(@FFilterIR^[0], FFFTSizeHalf);
 FillChar(FFilterIR^[FFFTSizeHalf], FFFTSizeHalf * SizeOf(Double), 0);

 // transform impulse response back to frequency domain
 FFft.PerformFFT(FFilter, FFilterIR);
end;

procedure TSpectralFilter64.PerformSpectralEffect(Spectrum: PDAVComplexDoubleFixedArray);
begin
 ComplexMultiplyBlock64(Spectrum, FFilter, FFFTSizeHalf);
end;

procedure TSpectralFilter64.SetThreshold(const Value: Double);
begin
 if FThreshold <> Value then
  begin
   FThreshold := Value;
   ThresholdChanged;
  end;
end;

procedure TSpectralFilter64.ThresholdChanged;
begin
 FThresholdFactor := dB_to_Amp(FThreshold);
end;


initialization
  RegisterDspProcessors32([TAveragedSelfFiltering32]);
  RegisterDspProcessors64([TSpectralFilter64]);

end.
