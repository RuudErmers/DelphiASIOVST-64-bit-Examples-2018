unit DAV_DspMinBlep;

// MinBLEP Generation Code
// By Daniel Werner
// This Code Is Public Domain

// translated to pascal by Christian-W. Budde

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_DspWindowing;

procedure RealCepstrum(Signal, RealCepstrum: TDAVSingleDynArray);
procedure MinimumPhase(RealCepstrum, MinimumPhase: TDAVSingleDynArray);
function GenerateMinBLEP(ZeroCrossings, OverSampling: Integer): TDAVSingleDynArray;

implementation

uses
  Math, DAV_Math, DAV_Complex, DAV_DspDFT;

// Complex Exponential
procedure ComplexExponential(Re, Im: Double; var zx, zy: Single);
var
  expx: Double;
begin
  expx := exp(Re);
  zx := expx * cos(Im);
  zy := expx * sin(Im);
end;

// Compute Real Cepstrum Of Signal
procedure RealCepstrum(Signal, RealCepstrum: TDAVSingleDynArray);
var
  realTime, imagTime,
  realFreq, imagFreq  : TDAVSingleDynArray;
  i, sz               : Integer;
begin
  sz := Length(Signal);
  Assert(Length(RealCepstrum) = sz);

  SetLength(realTime, sz);
  SetLength(imagTime, sz);
  SetLength(realFreq, sz);
  SetLength(imagFreq, sz);

  // Compose Complex FFT Input
  for i := 0 to sz - 1 do
   begin
    realTime[i] := Signal[i];
    imagTime[i] := 0;
   end;

  // Perform DFT
  DFT(realTime, imagTime, realFreq, imagFreq);

  // Calculate Log Of Absolute Value
  for i := 0 to sz - 1 do
   begin
    realFreq[i] := Log10(ComplexMagnitude32(realFreq[i], imagFreq[i]));
    imagFreq[i] := 0;
   end;

  // Perform Inverse FFT
  InverseDFT(realTime, imagTime, realFreq, imagFreq);

  // Output Real Part Of FFT
  for i := 0 to sz - 1 do
    RealCepstrum[i] := realTime[i];
end;

// Compute Minimum Phase Reconstruction Of Signal
procedure MinimumPhase(RealCepstrum, MinimumPhase: TDAVSingleDynArray);
var
  realTime, imagTime,
  realFreq, imagFreq  : TDAVSingleDynArray;
  n, i, nd2           : Integer;
begin
  n := Length(RealCepstrum);
  Assert(Length(MinimumPhase) = n);

  nd2 := n div 2;
  SetLength(realTime, n);
  SetLength(imagTime, n);
  SetLength(realFreq, n);
  SetLength(imagFreq, n);

  if ((n mod 2) = 1) then
   begin
    realTime[0] := RealCepstrum[0];
    for i := 1 to nd2 - 1 do
      realTime[i] := 2.0 * RealCepstrum[i];
    for i := nd2 to n - 1 do
      realTime[i] := 0.0;
   end
  else
   begin
    realTime[0] := RealCepstrum[0];
    for i := 1 to nd2 - 1 do
      realTime[i] := 2.0 * RealCepstrum[i];
    realTime[nd2] := RealCepstrum[nd2];
    for i := nd2 + 1 to n - 1 do
      realTime[i] := 0.0;
   end;

  for i := 1 to n - 1 do
    imagTime[i] := 0;
  DFT(realTime, imagTime, realFreq, imagFreq);

  for i := 0 to n - 1 do
    ComplexExponential(realFreq[i], imagFreq[i], realFreq[i], imagFreq[i]);
  InverseDFT(realTime, imagTime, realFreq, imagFreq);

  for i := 0 to n - 1 do
    MinimumPhase[i] := realTime[i];
end;

// Generate MinBLEP And Return It In An Array Of Floating Point Values
function GenerateMinBLEP(ZeroCrossings, OverSampling: Integer): TDAVSingleDynArray;
var
  i, n     : Integer;
  r, a, b  : Double;
  buffer   : array [0..1] of TDAVSingleDynArray;
begin
  n := (2 * ZeroCrossings * OverSampling) + 1;
  SetLength(buffer[0], n);
  SetLength(buffer[1], n);

  // Generate Sinc
  a := -ZeroCrossings;
  b :=  ZeroCrossings;
  for i := 0 to n - 1 do
   begin
    r := i / (n - 1);
    buffer[0][i] := Sinc(a + (r * (b - a)));
   end;

  // Window Sinc
  ApplyBlackmanWindow(buffer[0]);

  // Minimum Phase Reconstruction
  RealCepstrum(buffer[0], buffer[1]);
  MinimumPhase(buffer[1], buffer[0]);

  // Integrate Into MinBLEP
  SetLength(Result, n);
  a := 0;
  for i := 0 to n - 1 do
   begin
    a := a + buffer[0][i];
    Result[i] := a;
   end;

  // Normalize
  a := Result[n - 1];
  a := 1 / a;
  for i := 0 to n - 1 do
    Result[i] := Result[i] * a;
end;

end.
