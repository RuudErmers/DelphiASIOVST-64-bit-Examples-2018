unit DAV_DspSpectralNoiseReduction;

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

{$IFDEF Use_IPPS}
{$DEFINE ComplexDataOrder}
{$ELSE}
{$DEFINE PackedComplexDataOrder}
{$ENDIF}

uses
  Classes, DAV_Types, DAV_Classes, DAV_Complex, DAV_DspSpectralEffects,
  DAV_DspDynamics, DAV_DspLightweightDynamics, DAV_DspWindowFunctions,
  DAV_DspFftReal2Complex;

// TODO: check and implement all assignto functions!!!

type
  TCustomNoiseReduction32 = class(TCustomSpectralEffect32)
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
    procedure FFTOrderChanged; override;
    procedure WindowFunctionClassChanged; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;

    property WindowFunctionClass : TWindowFunctionClass read FWindowClass write SetWindowClass;
  end;

  TSpectralNoiseCut32 = class(TCustomNoiseReduction32, IDspProcessor32)
  private
    FThresholdFactor : Single;
    FThreshold       : Double;
    procedure SetThreshold(const Value: Double);
  protected
    procedure BuildFilter(Spectrum: PDAVComplexSingleFixedArray); override;
    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVSingleFixedArray); overload; override;
    procedure PerformSpectralEffect(Spectrum: PDAVComplexSingleFixedArray); overload; override;

    procedure ThresholdChanged; virtual;
  public
    constructor Create; override;
  published
    property Threshold: Double read FThreshold write SetThreshold;

    property FFTOrder;
    property FFTSize;
    property WindowFunctionClass;
  end;

  TSpectralNoiseGate32 = class(TCustomNoiseReduction32, IDspProcessor32)
  private
    FGates         : array of TLightweightSoftKneeCompressor;
    FThreshold     : Double;
    FRelease       : Double;
    FAttack        : Double;
    FRatio         : Double;
    FKnee          : Double;
    procedure SetThreshold(const Value: Double);
    procedure SetAttack(const Value: Double);
    procedure SetRelease(const Value: Double);
    procedure SetRatio(const Value: Double);
    procedure SetKnee(const Value: Double);

    procedure UpdateGates;
  protected
    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVSingleFixedArray); overload; override;
    procedure PerformSpectralEffect(Spectrum: PDAVComplexSingleFixedArray); overload; override;
    procedure BuildFilter(Spectrum: PDAVComplexSingleFixedArray); override;

    procedure AttackChanged; virtual;
    procedure FFTOrderChanged; override;
    procedure KneeChanged; virtual;
    procedure RatioChanged; virtual;
    procedure ReleaseChanged; virtual;
    procedure ThresholdChanged; virtual;
  public
    constructor Create; override;
  published
    property Attack: Double read FAttack write SetAttack;
    property Release: Double read FRelease write SetRelease;
    property Ratio: Double read FRatio write SetRatio;
    property Knee: Double read FKnee write SetKnee;
    property Threshold: Double read FThreshold write SetThreshold;

    property FFTOrder;
    property FFTSize;
    property WindowFunctionClass;
  end;

  TNoiseReduction32 = class(TCustomNoiseReduction32, IDspProcessor32)
  private
    FGates      : array of TLightweightSoftKneeCompressor;
    FRelease    : Double;
    FAttack     : Double;
    FRatio      : Double;
    FKnee       : Double;
    FAvrgCount  : Integer;
    FMatch      : Boolean;
    FOffset     : Double;
    FThresholds : PDAVSingleFixedArray;
    procedure SetAttack(const Value: Double);
    procedure SetRelease(const Value: Double);
    procedure SetRatio(const Value: Double);
    procedure SetKnee(const Value: Double);
    procedure SetMatch(const Value: Boolean);
    procedure SetOffset(const Value: Double);
  protected
    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVSingleFixedArray); overload; override;
    procedure PerformSpectralEffect(Spectrum: PDAVComplexSingleFixedArray); overload; override;
    procedure BuildFilter(Spectrum: PDAVComplexSingleFixedArray); override;
    procedure MatchThreshold(Spectrum: PDAVComplexSingleFixedArray); virtual;

    procedure AttackChanged; virtual;
    procedure FFTOrderChanged; override;
    procedure KneeChanged; virtual;
    procedure MatchChanged; virtual;
    procedure RatioChanged; virtual;
    procedure ReleaseChanged; virtual;
    procedure ThresholdOffsetChanged; virtual;
    procedure UpdateThresholds; virtual;
  public
    constructor Create; override;
  published
    property Attack: Double read FAttack write SetAttack;
    property Release: Double read FRelease write SetRelease;
    property Ratio: Double read FRatio write SetRatio;
    property Knee: Double read FKnee write SetKnee;
    property Match: Boolean read FMatch write SetMatch;
    property ThresholdOffset: Double read FOffset write SetOffset;

    property FFTOrder;
    property FFTSize;
    property WindowFunctionClass;
  end;


  TCustomNoiseReduction64 = class(TCustomSpectralEffect64)
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

    procedure Clear; override;

    property WindowFunctionClass : TWindowFunctionClass read FWindowClass write SetWindowClass;
  end;

  TSpectralNoiseCut64 = class(TCustomNoiseReduction64, IDspProcessor64)
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

  TSpectralNoiseGate64 = class(TCustomNoiseReduction64, IDspProcessor64)
  private
    FGates         : array of TLightweightSoftKneeCompressor;
    FThreshold     : Double;
    FRelease       : Double;
    FAttack        : Double;
    FRatio         : Double;
    FKnee          : Double;
    procedure SetThreshold(const Value: Double);
    procedure SetAttack(const Value: Double);
    procedure SetRelease(const Value: Double);
    procedure SetRatio(const Value: Double);
    procedure SetKnee(const Value: Double);

    procedure UpdateGates;
  protected
    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVDoubleFixedArray); overload; override;
    procedure PerformSpectralEffect(Spectrum: PDAVComplexDoubleFixedArray); overload; override;
    procedure BuildFilter(Spectrum: PDAVComplexDoubleFixedArray); override;

    procedure AttackChanged; virtual;
    procedure FFTOrderChanged; override;
    procedure KneeChanged; virtual;
    procedure RatioChanged; virtual;
    procedure ReleaseChanged; virtual;
    procedure ThresholdChanged; virtual;
  public
    constructor Create; override;
  published
    property Attack: Double read FAttack write SetAttack;
    property Release: Double read FRelease write SetRelease;
    property Ratio: Double read FRatio write SetRatio;
    property Knee: Double read FKnee write SetKnee;
    property Threshold: Double read FThreshold write SetThreshold;

    property FFTOrder;
    property FFTSize;
    property WindowFunctionClass;
  end;


implementation

uses
  Math, SysUtils, DAV_Common, DAV_BlockProcessing, DAV_Approximations;

{ TCustomNoiseReduction32 }

constructor TCustomNoiseReduction32.Create;
begin
 FFilterIR      := nil;
 FAddTimeBuffer := nil;
 FAddSpecBuffer := nil;
 FFilter        := nil;

 FWindowClass   := TWindowFunctionBlackman;
 WindowFunctionClassChanged;

 inherited;
end;

destructor TCustomNoiseReduction32.Destroy;
begin
 FreeMem(FFilterIR);
 FreeMem(FAddTimeBuffer);
 FreeMem(FAddSpecBuffer);
 FreeMem(FFilter);

 FreeAndNil(FWindowFunction);

 inherited;
end;

procedure TCustomNoiseReduction32.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FAddTimeBuffer, FFFTSize * SizeOf(Single));
 {$IFDEF ComplexDataOrder}
 ReallocMem(FAddSpecBuffer, (Fft.BinCount + 1) * SizeOf(TComplex32));
 ReallocMem(FFilter, (Fft.BinCount + 1) * SizeOf(TComplex32));
 {$ELSE}
 ReallocMem(FAddSpecBuffer, Fft.BinCount * SizeOf(TComplex32));
 ReallocMem(FFilter, Fft.BinCount * SizeOf(TComplex32));
 {$ENDIF}
 ReallocMem(FFilterIR, FFFTSize * SizeOf(Single));

 FillChar(FAddTimeBuffer^, FFFTSize * SizeOf(Single), 0);
 {$IFDEF ComplexDataOrder}
 FillChar(FAddSpecBuffer^, (Fft.BinCount + 1) * SizeOf(TComplex32), 0);
 FillChar(FFilter^, (Fft.BinCount + 1) * SizeOf(TComplex32), 0);
 {$ELSE}
 FillChar(FAddSpecBuffer^, Fft.BinCount * SizeOf(TComplex32), 0);
 FillChar(FFilter^, Fft.BinCount * SizeOf(TComplex32), 0);
 {$ENDIF}
 FillChar(FFilterIR^, FFFTSize * SizeOf(Single), 0);

 if Assigned(FWindowFunction)
  then FWindowFunction.Length := FFTSize;
end;

procedure TCustomNoiseReduction32.Clear;
begin
 inherited;

 FillChar(FAddTimeBuffer^, FFFTSize * SizeOf(Single), 0);
 {$IFDEF ComplexDataOrder}
 FillChar(FAddSpecBuffer^, (Fft.BinCount + 1) * SizeOf(TComplex32), 0);
 FillChar(FFilter^, (Fft.BinCount + 1) * SizeOf(TComplex32), 0);
 {$ELSE}
 FillChar(FAddSpecBuffer^, Fft.BinCount * SizeOf(TComplex32), 0);
 FillChar(FFilter^, Fft.BinCount * SizeOf(TComplex32), 0);
 {$ENDIF}
 FillChar(FFilterIR^, FFFTSize * SizeOf(Single), 0);
end;

procedure TCustomNoiseReduction32.SetWindowClass(
  const Value: TWindowFunctionClass);
begin
 Assert(Value <> nil);
 if FWindowClass <> Value then
  begin
   FWindowClass := Value;
   WindowFunctionClassChanged;
  end;
end;

procedure TCustomNoiseReduction32.WindowFunctionClassChanged;
var
  OldWindow : TCustomWindowFunction;
begin
 OldWindow := FWindowFunction;

 FWindowFunction := FWindowClass.Create;

 if Assigned(FWindowFunction) then
  with FWindowFunction do
   begin
    Start := 0;
    Slope := wsSymmetric;
   end;

 if Assigned(OldWindow) then
  begin
   FWindowFunction.Length := OldWindow.Length;
   FreeAndNil(OldWindow);
  end;
end;


{ TSpectralNoiseCut32 }

constructor TSpectralNoiseCut32.Create;
begin
 inherited;
 FThresholdFactor := 1E-3;
end;

procedure TSpectralNoiseCut32.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVSingleFixedArray);
var
  Sample : Integer;
  Reci   : Double;
  Scale  : Double;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);

 {$IFDEF ComplexDataOrder}
 Move(FSignalFreq^, FAddSpecBuffer^, (Fft.BinCount + 1) * SizeOf(TComplex32));
 {$ELSE}
 Move(FSignalFreq^, FAddSpecBuffer^, Fft.BinCount * SizeOf(TComplex32));
 {$ENDIF}
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

procedure TSpectralNoiseCut32.BuildFilter(Spectrum: PDAVComplexSingleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
const
  COffset : Single = 1E-10;
begin
 Half := FFFTSizeHalf;

 // DC bin
 if Abs(Spectrum^[0].Re) > FThreshold
  then FFilter^[0].Re := CHalf32 * (1 + FFilter^[0].Re)
  else FFilter^[0].Re := CHalf32 * FFilter^[0].Re;
 {$IFDEF ComplexDataOrder}
 FFilter^[Half].Re := 0;
 {$ENDIF}

 // other bins
 for Bin := 1 to Half - 1 do
  begin
   if FastSqrtBab2(Sqr(Spectrum^[Bin].Re) + Sqr(Spectrum^[Bin].Im)) > FThreshold
    then FFilter^[Bin].Re := CHalf32 * (1 + FFilter^[Bin].Re)
    else FFilter^[Bin].Re := CHalf32 * FFilter^[Bin].Re;
   FFilter^[Bin].Im := 0;
  end;

 // Nyquist bin
 {$IFDEF ComplexDataOrder}
 if Abs(Spectrum^[Half].Re) > FThreshold
  then FFilter^[Half].Re := CHalf32 * (1 + FFilter^[Half].Re)
  else FFilter^[Half].Re := CHalf32 * FFilter^[Half].Re;
 FFilter^[Half].Im := 0;
 {$ELSE}
 if Abs(Spectrum^[0].Im) > FThreshold
  then FFilter^[0].Im := CHalf32 * (1 + FFilter^[0].Im)
  else FFilter^[0].Im := CHalf32 * FFilter^[0].Im;
 {$ENDIF}

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

procedure TSpectralNoiseCut32.PerformSpectralEffect(Spectrum: PDAVComplexSingleFixedArray);
begin
 {$IFDEF ComplexDataOrder}
 ComplexMultiplyBlock32(Spectrum, FFilter, FFFTSizeHalf + 1);
 {$ELSE}
 ComplexMultiplyBlock32(Spectrum, FFilter, FFFTSizeHalf);
 {$ENDIF}
end;

procedure TSpectralNoiseCut32.SetThreshold(const Value: Double);
begin
 if FThreshold <> Value then
  begin
   FThreshold := Value;
   ThresholdChanged;
  end;
end;

procedure TSpectralNoiseCut32.ThresholdChanged;
begin
 FThresholdFactor := dB_to_Amp(FThreshold);
end;


{ TSpectralNoiseGate32 }

constructor TSpectralNoiseGate32.Create;
begin
 inherited;

 FFFT.AutoScaleType := astDivideInvByN;
end;

procedure TSpectralNoiseGate32.FFTOrderChanged;
begin
 inherited;
 UpdateGates;
end;

procedure TSpectralNoiseGate32.UpdateGates;
var
  Bin : Integer;
begin
 // dispose unused Gates
 for Bin := Fft.BinCount to Length(FGates) - 1 do
  if Assigned(FGates[Bin]) then FreeAndNil(FGates[Bin]);

 SetLength(FGates, Fft.BinCount);

 for Bin := 0 to Length(FGates) - 1 do
  if not Assigned(FGates[Bin]) then
   begin
    FGates[Bin] := TLightweightSoftKneeCompressor.Create;
    with FGates[Bin] do
     begin
      SampleRate := Self.SampleRate * 2 / FFft.FFTSize;
      Ratio := 0.1;
      Knee_dB := 2;
      Attack := 1;
      Release := 100;
     end;
   end;
end;

procedure TSpectralNoiseGate32.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVSingleFixedArray);
var
  Sample : Integer;
  Reci   : Double;
  Scale  : Double;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);

 {$IFDEF ComplexDataOrder}
 Move(FSignalFreq^, FAddSpecBuffer^, (Fft.BinCount + 1) * SizeOf(TComplex32));
 {$ELSE}
 Move(FSignalFreq^, FAddSpecBuffer^, Fft.BinCount * SizeOf(TComplex32));
 {$ENDIF}
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

// Move(SignalOut^[FFFTSizeHalf], SignalOut^[0], FFFTSizeHalf * SizeOf(Single));
end;

procedure TSpectralNoiseGate32.BuildFilter(
  Spectrum: PDAVComplexSingleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
const
  COffset : Single = 1E-10;
begin
 Half := FFFTSizeHalf;

 // DC bin
 FGates[0].InputSample(COffset + Sqr(Spectrum^[0].Re));
 FFilter^[0].Re := FGates[0].GainSample(1);
 if Abs(FFilter^[0].Re) > 1 then FFilter^[0].Re := 0;
 {$IFDEF ComplexDataOrder}
 FFilter^[0].Im := 0;
 {$ENDIF}

 // other bins
 for Bin := 1 to Half - 1 do
  begin
   FGates[Bin].InputSample(COffset + Sqr(Spectrum^[Bin].Re) + Sqr(Spectrum^[Bin].Im));
   FFilter^[Bin].Re := FGates[Bin].GainSample(1);
   if Abs(FFilter^[Bin].Re) > 1 then FFilter^[Bin].Re := 0;
   FFilter^[Bin].Im := 0;
  end;

 // Nyquist bin
 {$IFDEF ComplexDataOrder}
 FGates[Half].InputSample(COffset + Sqr(Spectrum^[Half].Re));
 FFilter^[Half].Re := FGates[Half].GainSample(1);
 if Abs(FFilter^[Half].Re) > 1 then FFilter^[Half].Re := 0;
 FFilter^[Half].Im := 0;
 {$ELSE}
 FGates[Half].InputSample(COffset + Sqr(Spectrum^[0].Im));
 FFilter^[0].Im := FGates[Half].GainSample(1);
 if Abs(FFilter^[0].Im) > 1 then FFilter^[0].Im := 0;
 {$ENDIF}

 FFft.PerformIFFT(FFilter, FFilterIR);

 Move(FFilterIR^[0], FFilterIR^[FFFTSizeHalf div 2], FFFTSizeHalf div 2 * SizeOf(Single));
 Move(FFilterIR^[FFFTSize - FFFTSizeHalf div 2], FFilterIR^[0], FFFTSizeHalf div 2 * SizeOf(Single));
 FWindowFunction.ProcessBlock32(@FFilterIR^[0], FFFTSizeHalf);
 FillChar(FFilterIR^[FFFTSizeHalf], FFFTSizeHalf * SizeOf(Single), 0);
 FFft.PerformFFT(FFilter, FFilterIR);
end;

procedure TSpectralNoiseGate32.PerformSpectralEffect(Spectrum: PDAVComplexSingleFixedArray);
begin
 {$IFDEF ComplexDataOrder}
 ComplexMultiplyBlock32(Spectrum, FFilter, FFFTSizeHalf + 1);
 {$ELSE}
 ComplexMultiplyBlock32(Spectrum, FFilter, FFFTSizeHalf);
 {$ENDIF}
end;

procedure TSpectralNoiseGate32.SetAttack(const Value: Double);
begin
 if FAttack <> Value then
  begin
   FAttack := Value;
   AttackChanged;
  end;
end;

procedure TSpectralNoiseGate32.SetKnee(const Value: Double);
begin
 if FKnee <> Value then
  begin
   FKnee := Value;
   KneeChanged;
  end;
end;

procedure TSpectralNoiseGate32.SetRatio(const Value: Double);
begin
 if FRatio <> Value then
  begin
   FRatio := Value;
   RatioChanged;
  end;
end;

procedure TSpectralNoiseGate32.SetRelease(const Value: Double);
begin
 if FRelease <> Value then
  begin
   FRelease := Value;
   ReleaseChanged;
  end;
end;

procedure TSpectralNoiseGate32.SetThreshold(const Value: Double);
begin
 if FThreshold <> Value then
  begin
   FThreshold := Value;
   ThresholdChanged;
  end;
end;

procedure TSpectralNoiseGate32.RatioChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Ratio := Ratio;
end;

procedure TSpectralNoiseGate32.ThresholdChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Threshold_dB := 2 * FThreshold;
end;

procedure TSpectralNoiseGate32.KneeChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Knee_dB := FKnee;
end;

procedure TSpectralNoiseGate32.AttackChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Attack := FAttack;
end;

procedure TSpectralNoiseGate32.ReleaseChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Release := FRelease;
end;


{ TNoiseReduction32 }

constructor TNoiseReduction32.Create;
begin
 inherited;
 FOffset := 8;

 FFFT.AutoScaleType := astDivideInvByN;

 {$IFDEF ComplexDataOrder}
 FFFT.DataOrder := doComplex;
 {$ENDIF}

 {$IFDEF PackedComplexDataOrder}
 FFFT.DataOrder := doPackedComplex;
 {$ENDIF}
end;

procedure TNoiseReduction32.FFTOrderChanged;
var
  Bin : Integer;
begin
 inherited;

 ReallocMem(FThresholds, Fft.BinCount * SizeOf(Single));
 FillChar(FThresholds^, Fft.BinCount * SizeOf(Single), 0);

  // dispose unused Gates
 for Bin := Fft.BinCount to Length(FGates) - 1 do
  if Assigned(FGates[Bin]) then FreeAndNil(FGates[Bin]);

 SetLength(FGates, Fft.BinCount);

 for Bin := 0 to Length(FGates) - 1 do
  if not Assigned(FGates[Bin]) then
   begin
    FGates[Bin] := TLightweightSoftKneeCompressor.Create;
    with FGates[Bin] do
     begin
      SampleRate := Self.SampleRate * 2 / FFft.FFTSize;
      Ratio := 0.1;
      Knee_dB := 2;
      Attack := 1;
      Release := 100;
      Threshold_dB := -400;
     end;
  end;
end;

procedure TNoiseReduction32.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVSingleFixedArray);
var
  Sample : Integer;
  Reci   : Double;
  Scale  : Double;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);

 // filter with the currest filter coefficients
 {$IFDEF ComplexDataOrder}
 Move(FSignalFreq^, FAddSpecBuffer^, (Fft.BinCount + 1) * SizeOf(TComplex32));
 {$ELSE}
 Move(FSignalFreq^, FAddSpecBuffer^, Fft.BinCount * SizeOf(TComplex32));
 {$ENDIF}
 PerformSpectralEffect(FAddSpecBuffer);

 // eventually match threshold
 if FMatch then MatchThreshold(FSignalFreq);

 // build new filter and filter spectrum
 BuildFilter(FSignalFreq);
 PerformSpectralEffect(FSignalFreq);

 // transform back to time domain
 FFft.PerformIFFT(FSignalFreq, SignalOut);
 FFft.PerformIFFT(FAddSpecBuffer, FAddTimeBuffer);

 // crossfade time domain
 Reci := 1 / FFFTSizeHalf;
 for Sample := 0 to FFFTSizeHalf - 1 do
  begin
   Scale := Sample * Reci;
   SignalOut^[Sample] := Scale * SignalOut^[FFFTSizeHalf + Sample] +
     (1 - Scale) * FAddTimeBuffer^[FFFTSizeHalf + Sample];
  end;
 end;

procedure TNoiseReduction32.BuildFilter(Spectrum: PDAVComplexSingleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
const
  COffset : Single = 1E-10;
begin
 Half := FFFTSizeHalf;

 {$IFDEF ComplexDataOrder}
 Assert(FFFT.DataOrder = doComplex);
 {$ENDIF}
 {$IFDEF PackedComplexDataOrder}
 Assert(FFFT.DataOrder = doPackedComplex);
 {$ENDIF}

 // DC bin
 FGates[0].InputSample(COffset + Sqr(Spectrum^[0].Re));
 FFilter^[0].Re := FGates[0].GainSample(1);
 Assert(not (IsNan(FFilter^[0].Re)));
 if Abs(FFilter^[0].Re) > 1 then FFilter^[0].Re := 0;
 {$IFDEF ComplexDataOrder}
 FFilter^[0].Im := 0;
 {$ENDIF}

 // other bins
 for Bin := 1 to Half - 1 do
  begin
   FGates[Bin].InputSample(COffset + Sqr(Spectrum^[Bin].Re) + Sqr(Spectrum^[Bin].Im));
   FFilter^[Bin].Re := FGates[Bin].GainSample(1);
   Assert(not (IsNan(FFilter^[Bin].Re)));
   if Abs(FFilter^[Bin].Re) > 1 then FFilter^[Bin].Re := 0;
   FFilter^[Bin].Im := 0;
  end;

 // Nyquist bin
 {$IFDEF ComplexDataOrder}
 FGates[Half].InputSample(COffset + Sqr(Spectrum^[Half].Re));
 FFilter^[Half].Re := FGates[Half].GainSample(1);
 Assert(not (IsNan(FFilter^[Half].Re)));
 if Abs(FFilter^[Half].Re) > 1 then FFilter^[Half].Re := 0;
 FFilter^[Half].Im := 0;
 {$ELSE}
 FGates[Half].InputSample(COffset + Sqr(Spectrum^[Half].Re));
 FFilter^[0].Im := FGates[Half].GainSample(1);
 Assert(not (IsNan(FFilter^[0].Im)));
 if Abs(FFilter^[0].Im) > 1 then FFilter^[0].Im := 0;
 {$ENDIF}

 FFft.PerformIFFT(FFilter, FFilterIR);

 Move(FFilterIR^[0], FFilterIR^[FFFTSizeHalf div 2], FFFTSizeHalf div 2 * SizeOf(Single));
 Move(FFilterIR^[FFFTSize - FFFTSizeHalf div 2], FFilterIR^[0], FFFTSizeHalf div 2 * SizeOf(Single));
 FWindowFunction.ProcessBlock32(@FFilterIR^[0], FFFTSizeHalf);
 FillChar(FFilterIR^[FFFTSizeHalf], FFFTSizeHalf * SizeOf(Single), 0);
 FFft.PerformFFT(FFilter, FFilterIR);
end;

procedure TNoiseReduction32.MatchThreshold(Spectrum: PDAVComplexSingleFixedArray);
var
  Bin    : Integer;
  Half   : Integer;
  Weight : array [0..1] of Single;
const
  COffset : Single = 1E-10;
begin
 Half := FFFTSizeHalf;

 Inc(FAvrgCount);
 Weight[1] := 1 / FAvrgCount;
 Weight[0] := 1 - Weight[1];

 // DC bin
 FThresholds[0] := Weight[0] * FThresholds[0] + Weight[1] * Sqr(FFilter^[0].Re);

 // other bins
 for Bin := 1 to Half - 1
  do FThresholds[Bin] := Weight[0] * FThresholds[Bin] +
    Weight[1] * (Sqr(Spectrum^[Bin].Re) + Sqr(Spectrum^[Bin].Im));

 // Nyquist bin
 FThresholds[Half] := Weight[0] * FThresholds[Half] +
   Weight[1] * Sqr(Spectrum^[Half].Re);

 UpdateThresholds;
end;

procedure TNoiseReduction32.UpdateThresholds;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Threshold_dB := Amp_to_dB(CDenorm32 + FThresholds[Bin]) + FOffset;
end;

procedure TNoiseReduction32.PerformSpectralEffect(Spectrum: PDAVComplexSingleFixedArray);
begin
 {$IFDEF ComplexDataOrder}
 ComplexMultiplyBlock32(Spectrum, FFilter, FFFTSizeHalf + 1);
 {$ELSE}
 ComplexMultiplyBlock32(Spectrum, FFilter, FFFTSizeHalf);
 {$ENDIF}
end;

procedure TNoiseReduction32.SetAttack(const Value: Double);
begin
 if FAttack <> Value then
  begin
   FAttack := Value;
   AttackChanged;
  end;
end;

procedure TNoiseReduction32.SetKnee(const Value: Double);
begin
 if FKnee <> Value then
  begin
   FKnee := Value;
   KneeChanged;
  end;
end;

procedure TNoiseReduction32.SetMatch(const Value: Boolean);
begin
 if FMatch <> Value then
  begin
   FMatch := Value;
   MatchChanged;
  end;
end;

procedure TNoiseReduction32.SetOffset(const Value: Double);
begin
 if FOffset <> Value then
  begin
   FOffset := Value;
   ThresholdOffsetChanged;
  end;
end;

procedure TNoiseReduction32.SetRatio(const Value: Double);
begin
 if FRatio <> Value then
  begin
   FRatio := Value;
   RatioChanged;
  end;
end;

procedure TNoiseReduction32.SetRelease(const Value: Double);
begin
 if FRelease <> Value then
  begin
   FRelease := Value;
   ReleaseChanged;
  end;
end;

procedure TNoiseReduction32.RatioChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Ratio := Ratio;
end;

procedure TNoiseReduction32.MatchChanged;
begin
 if FMatch
  then FAvrgCount := 0;
end;

procedure TNoiseReduction32.KneeChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Knee_dB := FKnee;
end;

procedure TNoiseReduction32.AttackChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Attack := FAttack;
end;

procedure TNoiseReduction32.ReleaseChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Release := FRelease;
end;

procedure TNoiseReduction32.ThresholdOffsetChanged;
begin
 UpdateThresholds;
end;


{ TCustomNoiseReduction64 }

constructor TCustomNoiseReduction64.Create;
begin
 FFilterIR      := nil;
 FAddTimeBuffer := nil;
 FAddSpecBuffer := nil;
 FFilter        := nil;

 FWindowClass   := TWindowFunctionBlackman;
 WindowFunctionClassChanged;

 inherited;
end;

destructor TCustomNoiseReduction64.Destroy;
begin
 Dispose(FFilterIR);
 Dispose(FAddTimeBuffer);
 Dispose(FAddSpecBuffer);
 Dispose(FFilter);

 FreeAndNil(FWindowFunction);

 inherited;
end;

procedure TCustomNoiseReduction64.FFTOrderChanged;
begin
 inherited;
 ReallocMem(FAddTimeBuffer, FFFTSize * SizeOf(Double));
 {$IFDEF ComplexDataOrder}
 ReallocMem(FAddSpecBuffer, (Fft.BinCount + 1) * SizeOf(TComplex64));
 ReallocMem(FFilter, (Fft.BinCount + 1) * SizeOf(TComplex64));
 {$ELSE}
 ReallocMem(FAddSpecBuffer, Fft.BinCount * SizeOf(TComplex64));
 ReallocMem(FFilter, Fft.BinCount * SizeOf(TComplex64));
 {$ENDIF}
 ReallocMem(FFilterIR, FFFTSize * SizeOf(Double));

 FillChar(FAddTimeBuffer^, FFFTSize * SizeOf(Double), 0);
 {$IFDEF ComplexDataOrder}
 FillChar(FAddSpecBuffer^, (Fft.BinCount + 1) * SizeOf(TComplex64), 0);
 FillChar(FFilter^, (Fft.BinCount + 1) * SizeOf(TComplex64), 0);
 {$ELSE}
 FillChar(FAddSpecBuffer^, Fft.BinCount * SizeOf(TComplex64), 0);
 FillChar(FFilter^, Fft.BinCount * SizeOf(TComplex64), 0);
 {$ENDIF}
 FillChar(FFilterIR^, FFFTSize * SizeOf(Double), 0);

 if Assigned(FWindowFunction)
  then FWindowFunction.Length := FFTSize;
end;

procedure TCustomNoiseReduction64.Clear;
begin
 inherited;

 FillChar(FAddTimeBuffer^, FFFTSize * SizeOf(Double), 0);
 {$IFDEF ComplexDataOrder}
 FillChar(FAddSpecBuffer^, (Fft.BinCount + 1) * SizeOf(TComplex64), 0);
 FillChar(FFilter^, (Fft.BinCount + 1) * SizeOf(TComplex64), 0);
 {$ELSE}
 FillChar(FAddSpecBuffer^, Fft.BinCount * SizeOf(TComplex64), 0);
 FillChar(FFilter^, Fft.BinCount * SizeOf(TComplex64), 0);
 {$ENDIF}
 FillChar(FFilterIR^, FFFTSize * SizeOf(Double), 0);
end;

procedure TCustomNoiseReduction64.SetWindowClass(
  const Value: TWindowFunctionClass);
begin
 if FWindowClass <> Value then
  begin
   FWindowClass := Value;
   WindowFunctionClassChanged;
  end;
end;

procedure TCustomNoiseReduction64.WindowFunctionClassChanged;
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

 if Assigned(OldWindow) then
  begin
   FWindowFunction.Length := OldWindow.Length;
   FreeAndNil(OldWindow);
  end;
end;


{ TSpectralNoiseCut64 }

constructor TSpectralNoiseCut64.Create;
begin
 inherited;
 FThresholdFactor := 1E-3;
end;

procedure TSpectralNoiseCut64.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVDoubleFixedArray);
var
  Sample : Integer;
  Reci   : Double;
  Scale  : Double;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);

 {$IFDEF ComplexDataOrder}
 Move(FSignalFreq^, FAddSpecBuffer^, (Fft.BinCount + 1) * SizeOf(TComplex64));
 {$ELSE}
 Move(FSignalFreq^, FAddSpecBuffer^, Fft.BinCount * SizeOf(TComplex64));
 {$ENDIF}
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

procedure TSpectralNoiseCut64.BuildFilter(Spectrum: PDAVComplexDoubleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
const
  COffset : Double = 1E-10;
begin
 Half := FFFTSizeHalf;

 // DC bin
 if Abs(Spectrum^[0].Re) > FThreshold
  then FFilter^[0].Re := CHalf64 * (1 + FFilter^[0].Re)
  else FFilter^[0].Re := CHalf64 * FFilter^[0].Re;
 {$IFDEF ComplexDataOrder}
 FFilter^[0].Im := 0;
 {$ENDIF}

 // other bins
 for Bin := 1 to Half - 1 do
  begin
   if FastSqrtBab2(Sqr(Spectrum^[Bin].Re) + Sqr(Spectrum^[Bin].Im)) > FThreshold
    then FFilter^[Bin].Re := CHalf64 * (1 + FFilter^[Bin].Re)
    else FFilter^[Bin].Re := CHalf64 * FFilter^[Bin].Re;
   FFilter^[Bin].Im := 0;
  end;

 // Nyquist bin
 {$IFDEF ComplexDataOrder}
 if Abs(Spectrum^[Half].Re) > FThreshold
  then FFilter^[Half].Re := CHalf64 * (1 + FFilter^[Half].Re)
  else FFilter^[Half].Re := CHalf64 * FFilter^[Half].Re;
 FFilter^[Half].Im := 0;
 {$ELSE}
 if Abs(Spectrum^[0].Im) > FThreshold
  then FFilter^[0].Im := CHalf64 * (1 + FFilter^[0].Im)
  else FFilter^[0].Im := CHalf64 * FFilter^[0].Im;
 {$ENDIF}

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

procedure TSpectralNoiseCut64.PerformSpectralEffect(Spectrum: PDAVComplexDoubleFixedArray);
begin
 {$IFDEF ComplexDataOrder}
 ComplexMultiplyBlock64(Spectrum, FFilter, FFFTSizeHalf + 1);
 {$ELSE}
 ComplexMultiplyBlock64(Spectrum, FFilter, FFFTSizeHalf);
 {$ENDIF}
end;

procedure TSpectralNoiseCut64.SetThreshold(const Value: Double);
begin
 if FThreshold <> Value then
  begin
   FThreshold := Value;
   ThresholdChanged;
  end;
end;

procedure TSpectralNoiseCut64.ThresholdChanged;
begin
 FThresholdFactor := dB_to_Amp(FThreshold);
end;


{ TSpectralNoiseGate64 }

constructor TSpectralNoiseGate64.Create;
begin
 inherited;

 FFFT.AutoScaleType := astDivideInvByN;
end;

procedure TSpectralNoiseGate64.FFTOrderChanged;
begin
 inherited;
 UpdateGates;
end;

procedure TSpectralNoiseGate64.UpdateGates;
var
  Bin : Integer;
begin
 // dispose unused Gates
 for Bin := Fft.BinCount to Length(FGates) - 1 do
  if Assigned(FGates[Bin]) then FreeAndNil(FGates[Bin]);

 SetLength(FGates, Fft.BinCount);

 for Bin := 0 to Length(FGates) - 1 do
  if not Assigned(FGates[Bin]) then
   begin
    FGates[Bin] := TLightweightSoftKneeCompressor.Create;
    with FGates[Bin] do
     begin
      SampleRate := Self.SampleRate * 2 / FFft.FFTSize;
      Ratio := 0.1;
      Knee_dB := 2;
      Attack := 1;
      Release := 100;
     end;
   end;
end;

procedure TSpectralNoiseGate64.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVDoubleFixedArray);
var
  Sample : Integer;
  Reci   : Double;
  Scale  : Double;
begin
 FFft.PerformFFT(FSignalFreq, SignalIn);

 {$IFDEF ComplexDataOrder}
 Move(FSignalFreq^, FAddSpecBuffer^, (Fft.BinCount + 1) * SizeOf(TComplex64));
 {$ELSE}
 Move(FSignalFreq^, FAddSpecBuffer^, Fft.BinCount * SizeOf(TComplex64));
 {$ENDIF}
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

// Move(SignalOut^[FFFTSizeHalf], SignalOut^[0], FFFTSizeHalf * SizeOf(Double));
end;

procedure TSpectralNoiseGate64.BuildFilter(
  Spectrum: PDAVComplexDoubleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
const
  COffset : Double = 1E-10;
begin
 Half := FFFTSizeHalf;

 // DC bin
 FGates[0].InputSample(COffset + Sqr(Spectrum^[0].Re));
 FFilter^[0].Re := FGates[0].GainSample(1);
 if Abs(FFilter^[0].Re) > 1 then FFilter^[0].Re := 0;
 {$IFDEF ComplexDataOrder}
 FFilter^[0].Im := 0;
 {$ENDIF}

 // other bins
 for Bin := 1 to Half - 1 do
  begin
   FGates[Bin].InputSample(COffset + Sqr(Spectrum^[Bin].Re) + Sqr(Spectrum^[Bin].Im));
   FFilter^[Bin].Re := FGates[Bin].GainSample(1);
   if Abs(FFilter^[Bin].Re) > 1 then FFilter^[Bin].Re := 0;
   FFilter^[Bin].Im := 0;
  end;

 // Nyquist bin
 {$IFDEF ComplexDataOrder}
 FGates[Half].InputSample(COffset + Sqr(Spectrum^[Half].Re));
 FFilter^[0].Re := FGates[Half].GainSample(1);
 if Abs(FFilter^[0].Re) > 1 then FFilter^[0].Re := 0;
 FFilter^[Half].Im := 0;
 {$ELSE}
 FGates[Half].InputSample(COffset + Sqr(Spectrum^[0].Im));
 FFilter^[0].Im := FGates[Half].GainSample(1);
 if Abs(FFilter^[0].Im) > 1 then FFilter^[0].Im := 0;
 {$ENDIF}

 FFft.PerformIFFT(FFilter, FFilterIR);

 Move(FFilterIR^[0], FFilterIR^[FFFTSizeHalf div 2], FFFTSizeHalf div 2 * SizeOf(Double));
 Move(FFilterIR^[FFFTSize - FFFTSizeHalf div 2], FFilterIR^[0], FFFTSizeHalf div 2 * SizeOf(Double));
 FWindowFunction.ProcessBlock64(@FFilterIR^[0], FFFTSizeHalf);
 FillChar(FFilterIR^[FFFTSizeHalf], FFFTSizeHalf * SizeOf(Double), 0);
 FFft.PerformFFT(FFilter, FFilterIR);
end;

procedure TSpectralNoiseGate64.PerformSpectralEffect(Spectrum: PDAVComplexDoubleFixedArray);
begin
 {$IFDEF ComplexDataOrder}
 ComplexMultiplyBlock64(Spectrum, FFilter, FFFTSizeHalf + 1);
 {$ELSE}
 ComplexMultiplyBlock64(Spectrum, FFilter, FFFTSizeHalf);
 {$ENDIF}
end;

procedure TSpectralNoiseGate64.SetAttack(const Value: Double);
begin
 if FAttack <> Value then
  begin
   FAttack := Value;
   AttackChanged;
  end;
end;

procedure TSpectralNoiseGate64.SetKnee(const Value: Double);
begin
 if FKnee <> Value then
  begin
   FKnee := Value;
   KneeChanged;
  end;
end;

procedure TSpectralNoiseGate64.SetRatio(const Value: Double);
begin
 if FRatio <> Value then
  begin
   FRatio := Value;
   RatioChanged;
  end;
end;

procedure TSpectralNoiseGate64.SetRelease(const Value: Double);
begin
 if FRelease <> Value then
  begin
   FRelease := Value;
   ReleaseChanged;
  end;
end;

procedure TSpectralNoiseGate64.SetThreshold(const Value: Double);
begin
 if FThreshold <> Value then
  begin
   FThreshold := Value;
   ThresholdChanged;
  end;
end;

procedure TSpectralNoiseGate64.RatioChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Ratio := Ratio;
end;

procedure TSpectralNoiseGate64.ThresholdChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Threshold_dB := 2 * FThreshold;
end;

procedure TSpectralNoiseGate64.KneeChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Knee_dB := FKnee;
end;

procedure TSpectralNoiseGate64.AttackChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Attack := FAttack;
end;

procedure TSpectralNoiseGate64.ReleaseChanged;
var
  Bin : Integer;
begin
 for Bin := 0 to Length(FGates) - 1
  do FGates[Bin].Release := FRelease;
end;

initialization
  RegisterDspProcessors32([TSpectralNoiseCut32, TSpectralNoiseGate32,
    TNoiseReduction32]);
  RegisterDspProcessors64([TSpectralNoiseCut64, TSpectralNoiseGate64]);

end.
