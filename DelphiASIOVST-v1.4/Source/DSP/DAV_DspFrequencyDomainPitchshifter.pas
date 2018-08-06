unit DAV_DspFrequencyDomainPitchshifter;

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
//  The initial developer of this code is Stephan M. Bernsee, translated and  //
//  adapted for the Delphi ASIO & VST Project by Christian-W. Budde           //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_ClassesFft, DAV_Complex,
  {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF}
  DAV_DspFftReal2Complex, DAV_DspWindowFunctions;

type
  TCustomFrequencyDomainPitchShifter = class(TDspSampleRateFftPersistent)
  private
    procedure SetPitchFactor(const Value: Single);
  protected
    FBlockPosition  : Integer;
    FPitchFactor    : Single;
    FWindowFunction : TCustomWindowFunction;
    procedure PitchFactorChanged; virtual;
    procedure SampleRateChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property PitchFactor: Single read FPitchFactor write SetPitchFactor;
  end;

  TCustomFrequencyDomainPitchShifter32 = class(TCustomFrequencyDomainPitchShifter)
  private
    {$IFDEF Use_IPPS}
    function GetFft : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    function GetFft : TFftReal2ComplexCUDA32;
    {$ELSE}
    function GetFft : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
  protected
    FSignalFreq    : PDAVComplexSingleFixedArray;
    FInputBuffer   : PDAVSingleFixedArray;
    FOutputBuffer  : PDAVSingleFixedArray;

    FFFTworksp     : PDAVSingleFixedArray; // [2 * CMaxFrameLength]
    FLastPhase     : PDAVSingleFixedArray; // [CMaxFrameLength div 2 + 1]
    FSumPhase      : PDAVSingleFixedArray; // [CMaxFrameLength div 2 + 1]
    FOutputAccum   : PDAVSingleFixedArray; // [2 * CMaxFrameLength]
    FAnaFreq       : PDAVSingleFixedArray; // [CMaxFrameLength]
    FAnaMagn       : PDAVSingleFixedArray; // [CMaxFrameLength]
    FSynFreq       : PDAVSingleFixedArray; // [CMaxFrameLength]
    FSynMagn       : PDAVSingleFixedArray; // [CMaxFrameLength]
    FInit          : Boolean;

    FOSFactor      : Integer;

    procedure AssignTo(Dest: TPersistent); override;
    procedure FFTOrderChanged; override;
    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVSingleFixedArray); overload; virtual;
//    procedure PerformSpectralEffect(Spectum: PDAVComplexSingleFixedArray); overload; virtual;

    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat32 read GetFft;
    {$ELSE} {$IFDEF Use_CUDA}
    property Fft : TFftReal2ComplexCUDA32 read GetFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat32  read GetFft;
    {$ENDIF}{$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, Output: PDAVSingleFixedArray; const SampleFrames: Integer); virtual;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleFrames: Integer); virtual;
    function ProcessSample32(Input: Single): Single; virtual;
  end;

  TFrequencyDomainPitchShifter32 = class(TCustomFrequencyDomainPitchShifter32)
  published
    property PitchFactor;
  end;

implementation

uses
  SysUtils, Math;

function smbAtan2(x, y: Double): Double;
var
  signx : Double;
begin
  if (x > 0)
   then signx :=  1
   else signx := -1;

  if (x = 0) then Result := 0 else
  if (y = 0)
   then Result := signx * Pi * 0.5
   else Result := ArcTan2(y, x);
end;

{ TCustomFrequencyDomainPitchShifter }

constructor TCustomFrequencyDomainPitchShifter.Create;
begin
 inherited;
 FPitchFactor := 1;
 FBlockPosition := 0;
 FWindowFunction := TWindowFunctionHanning.Create;
end;

destructor TCustomFrequencyDomainPitchShifter.Destroy;
begin
 FreeAndNil(FWindowFunction);
 inherited;
end;

procedure TCustomFrequencyDomainPitchShifter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomFrequencyDomainPitchShifter then
  with TCustomFrequencyDomainPitchShifter(Dest) do
   begin
    inherited;
    FBlockPosition := Self.FBlockPosition;
    FPitchFactor := Self.FPitchFactor;
    FWindowFunction.Assign(Self.FWindowFunction);
   end
 else inherited;
end;

procedure TCustomFrequencyDomainPitchShifter.PitchFactorChanged;
begin
 //
end;

procedure TCustomFrequencyDomainPitchShifter.SampleRateChanged;
begin
 //
end;

procedure TCustomFrequencyDomainPitchShifter.SetPitchFactor(const Value: Single);
begin
 if FPitchFactor <> Value then
  begin
   FPitchFactor := Value;
   PitchFactorChanged;
  end;
end;


{ TCustomFrequencyDomainPitchShifter32 }

constructor TCustomFrequencyDomainPitchShifter32.Create;
begin
 inherited;

 FSignalFreq := nil;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(6);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(6);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(6);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}{$ENDIF}
 FFTOrderChanged;
end;

destructor TCustomFrequencyDomainPitchShifter32.Destroy;
begin
 Dispose(FSignalFreq);
 Dispose(FInputBuffer);
 Dispose(FOutputBuffer);
 FreeAndNil(FFft);
 inherited;
end;

procedure TCustomFrequencyDomainPitchShifter32.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomFrequencyDomainPitchShifter32 then
  with TCustomFrequencyDomainPitchShifter32(Dest) do
   begin
    inherited;
    FBlockPosition := Self.FBlockPosition;
   end
 else inherited;
end;

procedure TCustomFrequencyDomainPitchShifter32.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FInputBuffer, FFFTSize * SizeOf(Single));
 ReallocMem(FOutputBuffer, FFFTSize * SizeOf(Single));
 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex32));
 ReallocMem(FLastPhase, (FFFTSizeHalf + 1) * SizeOf(Single));
 ReallocMem(FSumPhase, (FFFTSizeHalf + 1) * SizeOf(Single));
 ReallocMem(FOutputAccum, FFFTSize * SizeOf(Single));
 ReallocMem(FAnaFreq, (FFFTSizeHalf + 1) * SizeOf(Single));
 ReallocMem(FAnaMagn, (FFFTSizeHalf + 1) * SizeOf(Single));
 ReallocMem(FSynMagn, Fft.FFTSize * SizeOf(Single));
 ReallocMem(FSynFreq, Fft.FFTSize * SizeOf(Single));

 FillChar(FInputBuffer^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FOutputBuffer^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex32), 0);
 FillChar(FLastPhase^[0], (FFFTSizeHalf + 1) * SizeOf(Single), 0);
 FillChar(FSumPhase^[0], (FFFTSizeHalf + 1) * SizeOf(Single), 0);
 FillChar(FOutputAccum^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FAnaFreq^[0], (FFFTSizeHalf + 1) * SizeOf(Single), 0);
 FillChar(FAnaMagn^[0], (FFFTSizeHalf + 1) * SizeOf(Single), 0);
 FillChar(FSynMagn^[0], Fft.FFTSize * SizeOf(Single), 0);
 FillChar(FSynFreq^[0], Fft.FFTSize * SizeOf(Single), 0);
end;

{$IFDEF Use_IPPS}
function TCustomFrequencyDomainPitchShifter32.GetFft : TFftReal2ComplexIPPSFloat32;
begin
 Result := TFftReal2ComplexIPPSFloat32(FFft);
end;

{$ELSE} {$IFDEF Use_CUDA}

function TCustomFrequencyDomainPitchShifter32.GetFft : TFftReal2ComplexCUDA32;
begin
 Result := TFftReal2ComplexCUDA32(FFft);
end;

{$ELSE}

function TCustomFrequencyDomainPitchShifter32.GetFft : TFftReal2ComplexNativeFloat32;
begin
 Result := TFftReal2ComplexNativeFloat32(FFft);
end;
{$ENDIF}{$ENDIF}

procedure TCustomFrequencyDomainPitchShifter32.ProcessBlock(const Input,
  Output: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  CurrentPosition : Integer;
begin
 CurrentPosition := 0;

 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FFFTSizeHalf then
   begin
    // copy to ring buffer only
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));

    // increase block position and Break
    Inc(FBlockPosition, SampleFrames - CurrentPosition);
    Exit;
   end
  else
   begin
    Move(Input^[CurrentPosition], FInputBuffer^[FFFTSizeHalf + FBlockPosition], (FFFTSizeHalf - FBlockPosition) * SizeOf(Single));
    Move(FOutputBuffer^[FBlockPosition], Output^[CurrentPosition], (FFFTSizeHalf - FBlockPosition) * SizeOf(Single));

    PerformSpectralEffect(FInputBuffer, FOutputBuffer);

    // discard already used input buffer part to make space for new data
    Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Single));

    // increase current position and reset block position
    Inc(CurrentPosition, (FFFTSizeHalf - FBlockPosition));
    FBlockPosition := 0;
   end;
  until CurrentPosition >= SampleFrames;
end;

procedure TCustomFrequencyDomainPitchShifter32.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleFrames: Integer);
begin
 ProcessBlock(Data, Data, SampleFrames);
end;

function TCustomFrequencyDomainPitchShifter32.ProcessSample32(Input: Single): Single;
begin
 // copy to ring buffer only
 FInputBuffer^[FFFTSizeHalf + FBlockPosition] := Input;
 Result := FOutputBuffer^[FBlockPosition];

 // increase block position and Break
 Inc(FBlockPosition, 1);
 if FBlockPosition >= FFFTSizeHalf then
  begin
   PerformSpectralEffect(FInputBuffer, FOutputBuffer);

   // discard already used input buffer part to make space for new data
   Move(FInputBuffer[FFFTSizeHalf], FInputBuffer[0], FFFTSizeHalf * SizeOf(Single));

   // increase current position and reset block position
   FBlockPosition := 0;
  end;
end;


procedure TCustomFrequencyDomainPitchShifter32.PerformSpectralEffect(SignalIn,
  SignalOut: PDAVSingleFixedArray);
var
  magn, phase   : Double;
  tmp, window   : Double;
  real, imag    : Double;
  freqPerBin    : Double;
  expct         : Double;
  i, k          : Integer;
  qpd, index    : Integer;
  inFifoLatency : Integer;
  stepSize      : Integer;
  fftFrameSize2 : Integer;
begin
  FInit := False;

  (* set up some handy variables *)
  fftFrameSize2 := Fft.FFTSize div 2;
  stepSize := Fft.FFTSize div FOSFactor;
  freqPerBin := SampleRate / Fft.FFTSize;
  expct := 2 * Pi * stepSize / Fft.FFTSize;
  inFifoLatency := Fft.FFTSize - stepSize;


  //////////////
  // ANALYSIS //
  //////////////

  Move(SignalIn^[0], SignalOut^[0], Fft.FFTSize);

  // apply window
  FWindowFunction.ProcessBlock32(SignalOut, Fft.FFTSize);

  // perform FFT
  FFFT.PerformFFT(FSignalFreq, FOutputBuffer);

  for k := 0 to fftFrameSize2 - 1 do
   begin
    // de-interlace FFT buffer
    real := FSignalFreq[k].Re;
    imag := FSignalFreq[k].Im;

    // compute magnitude and phase
    magn := 2 * Sqrt(real * real + imag * imag);
    phase := arctan2(imag, real);

    // compute phase difference
    tmp := Phase - FLastPhase[k];
    FLastPhase[k] := Phase;

    // subtract expected phase difference
    tmp := tmp - k * expct;

    // map delta phase into +/- Pi interval
    qpd := Round(tmp / Pi);
    if (qpd >= 0)
     then qpd := (qpd + qpd) and 1
     else qpd := (qpd - qpd) and 1;

    tmp := Pi * qpd;

    // get deviation from bin frequency from the +/- Pi interval
    tmp := FOSFactor * tmp / (2 * Pi);

    // compute the k-th partials' true frequency
    tmp := k * freqPerBin + tmp * freqPerBin;

    // store magnitude and true frequency in analysis arrays *)
    FAnaMagn[k] := magn;
    FAnaFreq[k] := tmp;
   end;


  ////////////////
  // PROCESSING //
  ////////////////

  FillChar(FSynMagn^[0], Fft.FFTSize * SizeOf(Single), 0);
  FillChar(FSynFreq^[0], Fft.FFTSize * SizeOf(Single), 0);
  for k := 0 to fftFrameSize2 - 1 do
   begin
    index := Round(k * FPitchFactor);
    if (index <= fftFrameSize2) then
     begin
      FSynMagn[index] := FSynMagn[index] + FAnaMagn[k];
      FSynFreq[index] := FSynFreq[index] + FAnaFreq[k] * FPitchFactor;
     end;
   end;

  ///////////////
  // SYNTHESIS //
  ///////////////

  for k := 0 to fftFrameSize2 - 1 do
   begin
    // get magnitude and true frequency from synthesis arrays
    magn := FSynMagn[k];
    tmp := FSynFreq[k];

    // subtract bin mid frequency
    tmp := tmp - k * freqPerBin;

    // get bin deviation from freq deviation
    tmp := tmp / freqPerBin;

    // take oversampling factor into account
    tmp := 2 * Pi * tmp / FOSFactor;

    // add the overlap phase advance back in
    tmp := tmp + k * expct;

    // accumulate delta phase to get bin phase
    FSumPhase[k] := FSumPhase[k] + tmp;
    phase := FSumPhase[k];

    // get real and imag part and re-interleave
    FSignalFreq[k].Re := magn * cos(phase);
    FSignalFreq[k].Im := magn * sin(phase);
   end;

  (* zero negative frequencies *)
  for k := Fft.FFTSize + 2 to 2 * Fft.FFTSize - 1 do
   begin
    FSignalFreq[k].Re := 0;
    FSignalFreq[k].Im := 0;
   end;

  // do inverse transform
  Fft.PerformIFFT(FSignalFreq, SignalOut);

  // do windowing and add to output accumulator
  for k := 0 to Fft.FFTSize - 1 do
   begin
    window := -0.5 * cos(2 * Pi * k / Fft.FFTSize) + 0.5;
    FOutputAccum[k] := FOutputAccum[k] + 2 * window * SignalOut[k] / (fftFrameSize2 * FOSFactor);
   end;

(*
  for k := 0 to stepSize - 1
   do FOutputBuffer[k] = FOutputAccum[k];

  // shift accumulator
  Move(FOutputAccum, FOutputAccum + stepSize, Fft.FFTSize * SizeOf(Single));

  // move input FIFO
  for (k = 0; k < inFifoLatency; k++) FInputBuffer[k] = FInputBuffer[k + stepSize];
*)
end;

end.

// -----------------------------------------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------------------------------------

{

procedure smbFft(fftBuffer: PSingle; fftFrameSize: Integer; sign: Integer);
(*
  FFT routine, (C)1996 S.M.Bernsee. Sign = -1 is FFT, 1 is iFFT (inverse)
  Fills fftBuffer[0...2*fftFrameSize-1] with the Fourier transform of the
  time domain data in fftBuffer[0...2*fftFrameSize-1]. The FFT array takes
  and returns the cosine and sine parts in an interleaved manner, ie.
  fftBuffer[0] = cosPart[0], fftBuffer[1] = sinPart[0], asf. fftFrameSize
  must be a power of 2. It expects a complex input signal (see footnote 2),
  ie. when working with 'common' audio signals our input signal has to be
  passed as (in[0],0.,in[1],0.,in[2],0.,...) asf. In that case, the transform
  of the frequencies of interest is in fftBuffer[0...fftFrameSize].
*)
var
  wr, wi, arg, temp      : Single;
  tr, ti, ur, ui         : Single;
  p1, p2                 : PSingle;
  p1r, p1i, p2r, p2i     : PSingle;
  i, bitm, j, le, le2, k : Integer;
begin
  for i := 1 to fftFrameSize - 2 do
   begin
    for (bitm = 2, j = 0; bitm < 2 * fftFrameSize; bitm <<= 1)
     begin
      if (2 * i & bitm) j++;
      j <<= 1;
     end;
    if (2 * i < j) then
     begin
      p1 = fftBuffer + 2 * i;
      p2 = fftBuffer + j;
      temp = *p1;
      *(p1++) = *p2;
      *(p2++) = temp;
      temp = *p1;
      *p1 = *p2;
      *p2 = temp;
     end;
   end;

  for (k = 0, le = 2; k < (long)(log(fftFrameSize)/log(2.)+.5); k++) begin
    le <<= 1;
    le2 = le>>1;
    ur = 1.0;
    ui = 0.0;
    arg = Pi / (le2>>1);
    wr = cos(arg);
    wi = sign*sin(arg);
    for (j = 0; j < le2; j += 2) begin
      p1r = fftBuffer+j; p1i = p1r+1;
      p2r = p1r+le2; p2i = p2r+1;
      for (i = j; i < 2*fftFrameSize; i += le) begin
        tr = *p2r * ur - *p2i * ui;
        ti = *p2r * ui + *p2i * ur;
        *p2r = *p1r - tr; *p2i = *p1i - ti;
        *p1r += tr; *p1i += ti;
        p1r += le; p1i += le;
        p2r += le; p2i += le;
      end;
      tr = ur*wr - ui*wi;
      ui = ur*wi + ui*wr;
      ur = tr;
    end;
  end;
end;
}

