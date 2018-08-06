unit DAV_DspNoiseShapingFilterDesigner;

// based on code found at http://shibatch.sourceforge.net/
// see: 'feedback filter for noise shaper'
// check the copyright before you use this code

{$I ..\DAV_Compiler.inc}

interface

uses
  Classes, DAV_Types, DAV_Complex,
  {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF}
  DAV_DspFftReal2Complex;

type
  TCoefficientUpdate = procedure(Sender: TObject; Coefficients: PDAVDoubleFixedArray; Best: Double) of object;
   
  TCustomNoiseShapingFilterDesigner = class(TObject)
  private
    FSampleRate          : Single;
    FLoopCount           : Integer;
    FLoopCountReciprocal : Single;
    FSampleFrames        : Integer;
    FOnCoefficientUpdate : TCoefficientUpdate;
    {$IFDEF Use_IPPS}
    FFFT                 : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    FFFT                 : TFftReal2ComplexCUDA32;
    {$ELSE}
    FFFT                 : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
    function GetFFTSize: Integer;
    function GetFFTSizeHalf: Integer;
    procedure SetSampleRate(const Value: Single);
    procedure SetSampleFrames(const Value: Integer);
    procedure SetLoopCount(const Value: Integer);
    procedure LoopCountChanged;
  protected
    procedure SampleRateChanged; virtual;
    procedure SampleFramesChanged; virtual;
    property FFTSizeHalf: Integer read GetFFTSizeHalf;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Calculate; virtual;

    property SampleRate: Single read FSampleRate write SetSampleRate;
    property LoopCount: Integer read FLoopCount write SetLoopCount default 10000;
    property SampleFrames: Integer read FSampleFrames write SetSampleFrames default 20;
    property OnCoefficientUpdate: TCoefficientUpdate read FOnCoefficientUpdate write FOnCoefficientUpdate;
    property FFTSize: Integer read GetFFTSize;
  end;

  TNoiseShapingFilterDesigner = class(TCustomNoiseShapingFilterDesigner)
  published
    property SampleRate;
    property LoopCount;
    property SampleFrames;
    property OnCoefficientUpdate;
    property FFTSize;
  end;

implementation

uses
  SysUtils, Math;

resourcestring
  RCStrInvalidSampleframes = 'Sampleframes must be above 2!';
  RCStrInvalidLoopcount = 'Loopcount must be above 2!';

{ TCustomNoiseShapingFilterDesigner }

constructor TCustomNoiseShapingFilterDesigner.Create;
begin
 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(6);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(6);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(6);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}{$ENDIF}

 FLoopCount := 10000;
 FSampleFrames := 20;
 LoopCountChanged;
 SampleFramesChanged;
end;

destructor TCustomNoiseShapingFilterDesigner.Destroy;
begin
 FreeAndNil(FFft);
 inherited;
end;

procedure TCustomNoiseShapingFilterDesigner.SetLoopCount(const Value: Integer);
begin
 if Value <= 2
  then raise Exception.Create(RCStrInvalidLoopcount);

 if FLoopCount <> Value then
  begin
   FLoopCount := Value;
   LoopCountChanged;
  end;
end;

procedure TCustomNoiseShapingFilterDesigner.SetSampleFrames(const Value: Integer);
begin
 if Value <= 2
  then raise Exception.Create(RCStrInvalidSampleframes);

 if FSampleFrames <> Value then
  begin
   FSampleFrames := Value;
   SampleFramesChanged;
  end;
end;

procedure TCustomNoiseShapingFilterDesigner.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

function TCustomNoiseShapingFilterDesigner.GetFFTSize: Integer;
begin
 result := FFft.FFTSize;
end;

function TCustomNoiseShapingFilterDesigner.GetFFTSizeHalf: Integer;
begin
 result := FFft.FFTSize div 2;
end;

procedure TCustomNoiseShapingFilterDesigner.LoopCountChanged;
begin
 FLoopCountReciprocal := 1 / FLoopCount;
end;

procedure TCustomNoiseShapingFilterDesigner.SampleRateChanged;
begin
 // not implemented yet
end;

procedure TCustomNoiseShapingFilterDesigner.SampleFramesChanged;
begin
 // not implemented yet
end;

function ATHformula(Frequency: Double): Double;
begin
  Frequency := Max(0.01, Frequency * 0.001); // convert to khz and define a minimum
  //Frequency  = Min(18.0, Frequency);

  // from Painter & Spanias, 1997 modified by Gabriel Bouvigne to better fit to the reality
  result :=   3.640 * Power(Frequency, -0.8)
            - 6.800 * Exp(-0.6  * sqr(Frequency - 3.4))
            + 6.000 * Exp(-0.15 * sqr(Frequency - 8.7))
            + 0.6 * 0.001 * IntPower(Frequency, 4);
end;

// see for example "Zwicker: Psychoakustik, 1982; ISBN 3-540-11401-7
function FrequencyToCriticalBandwidth(Frequency: Double): Double;
begin
 // input: freq in hz  output: critical band width
 Frequency := Frequency * 0.001;
 result := 25 + 75 * Power(1 + 1.4 * sqr(Frequency), 0.69);
end;

procedure TCustomNoiseShapingFilterDesigner.Calculate;
var
  i, j, TotalLoop, Num     : Integer;
  iath, ath, logath        : PDAVDoubleFixedArray;
  Noise, TimeDomain        : PDAVSingleFixedArray;
  FrequencyDomain          : PDAVComplexSingleFixedArray;
  BestMax, BestMax2        : Double;
  LogathAvg, LastBestMax   : Double;
  OldBestMax               : Double;
  Average                  : Double;
  m, t                     : Single;
  ntry                     : Integer;
  xtry, Best, Best2        : PDAVDoubleFixedArray;
  Freq                     : Double;

const
  NTRYMAX = 20;

begin
 Randomize;
 OldBestMax := 10000;
 Num := (3 * FLoopCount div 2);

 // allocate memory
 GetMem(iath, FFTSize * SizeOf(Single));
 GetMem(ath, FFTSize * SizeOf(Single));
 GetMem(logath, FFTSize * SizeOf(Single));
 GetMem(Noise, FFTSize * SizeOf(Double));
 GetMem(TimeDomain, FFTSize * SizeOf(Single));
 GetMem(FrequencyDomain, FFTSizeHalf * SizeOf(TComplex32));
 GetMem(xtry, Num * SizeOf(Single));
 GetMem(Best, Num * SizeOf(Single));
 GetMem(Best2, Num * SizeOf(Single));

 // initialize noise array
 for i := 0 to FFTSize - 1 do Noise[i] := random;

 // initialize other arrays
 for i := 0 to FFTSizeHalf - 1 do
  begin
   Freq := FSampleRate * i / FFTSize;
   ath[i] := Power(10, ATHformula(Freq) * 0.1) / FrequencyToCriticalBandwidth(Freq);
   iath[i] := 1 / ath[i];
   logath[i] := log10(ath[i]);
  end;

 // initialize LogathAvg
 LogathAvg := logath[0];
 for i := 1 to FFTSizeHalf - 1 do LogathAvg := LogathAvg + logath[i];
 LogathAvg := LogathAvg * 2 / FFTSize;

 TotalLoop := 0;
 LastBestMax := 1000000;
 BestMax2 := sqr(OldBestMax);

 // initialize best array
 for i := 0 to Num - 1 do Best^[i] := 0;

 // loop
 while True do
  begin
   ntry := 0;

   BestMax := 1E+300;

   while (LastBestMax < BestMax) and (ntry < NTRYMAX) do
    begin
//      printf('n = %d', SampleFrames);

     for i := 0 to FLoopCount - 1 do
      begin
       Inc(TotalLoop);

       for j := 0 to SampleFrames - 1 do
        begin
         xtry^[j] := Best^[j];
         if random(2) = 0
          then xtry^[j] := xtry^[j] + ((random - 0.5) * (FLoopCount - i) * FLoopCountReciprocal);
        end;

       TimeDomain[0] := 1;
       j := 0;
       while j < SampleFrames do
        begin
         TimeDomain[j + 1] := xtry^[j];
         inc(j);
        end;
       while j < FFTSize do
        begin
         TimeDomain[j] := 0;
         inc(j);
        end;

       // perform FFT
       FFFT.PerformFFT(FrequencyDomain, TimeDomain);

       // calculate magnitude
       FrequencyDomain[0].Re := sqr(FrequencyDomain[0].Re);
       FrequencyDomain[0].Im := sqr(FrequencyDomain[0].Im);
       Average := FrequencyDomain[0].Re; // 'Average' not used, what for?!?
       j := 1;
       while j < FFTSizeHalf do
        begin
         FrequencyDomain[j].Re := sqr(FrequencyDomain[j].Re) + sqr(FrequencyDomain[j].Im);
         Average := Average + FrequencyDomain[j].Re;
         inc(j);
        end;
       FrequencyDomain[j].Re := sqr(FrequencyDomain[j].Re);
       Average := 2 * Average / FFTSize;

       m := -100000;

       // evaluate magnitude
       for j := 0 to FFTSizeHalf - 1 do
        begin
         t := FrequencyDomain[j].Re * iath[j];
         if (t > m) then  m := t;
        end;

       if (BestMax > m) then
        begin
         BestMax := m;
         Move(xtry^[0], Best^[0], Num * SizeOf(Double));
        end;
      end;

//     printf(' %d( %d) : bestmax :=  %g,  %g', TotalLoop, ntry, log(BestMax), log(BestMax2));
     inc(ntry);
    end;

   if (BestMax2 > BestMax) then
    begin
     m := 1;

     for i := 0 to Num - 1 do Best2^[i] := Best^[i];
     BestMax2 := BestMax;

     if assigned(FOnCoefficientUpdate)
      then FOnCoefficientUpdate(Self, Best2, Log10(Bestmax2));

     for j := 0 to SampleFrames - 1 do
      begin
       if (Best2^[j] >  m) then  m :=  Best2^[j];
       if (Best2^[j] < -m) then  m := -Best2^[j];
      end;
    end;

   if (ntry = NTRYMAX) then
    begin
     LastBestMax := 1000000;
     Continue;
    end;

   LastBestMax := BestMax;
  end;
end;

end.
