unit DAV_StkJCReverb;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  John Chowning's reverberator class.

   This class is derived from the CLM TStkJCRev function, which is based on the
   use of networks of simple allpass and comb delay filters.
   This class implements three series allpass units, followed by four parallel
   comb filters, and two decorrelation delay lines in parallel at the output.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkReverb, DAV_StkDelay, Math;

type
  TStkJCReverb = class(TStkReverb)
  private
    procedure SetT60(const Value: Single);
    procedure T60Changed;
  protected
    FAllpassDelays      : array[0..2] of TStkDelay;
    FCombDelays         : array[0..3] of TStkDelay;
    FCombCoefficient    : array[0..3] of Single;
    FOutLeftDelay       : TStkDelay;
    FOutRightDelay      : TStkDelay;
    FAllpassCoefficient : Single;
    FT60                : Single;
    FInternalLengths    : array[0..8] of Integer;
    procedure SampleRateChanged; override;
    procedure CalculateInternalLengths; virtual;
  public
    constructor Create(const SampleRate: Single = 44100); overload; override;
    constructor Create(const SampleRate, T60: Single); reintroduce; overload; virtual;
    destructor Destroy; override;
    procedure Clear; override;
    function Tick(const Input: Single): Single; override;

    property T60: Single read FT60 write SetT60;  
  end;

implementation

uses
  SysUtils, DAV_Common, DAV_Math, DAV_StkFilter;

const
  CLength : array[0..8] of Integer = (1777, 1847, 1993, 2137, 389, 127, 43,
    211, 179);

constructor TStkJCReverb.Create(const SampleRate, T60: Single);
begin
  inherited Create(SampleRate);
  FT60 := T60;
  FAllpassCoefficient := 0.7;
  FEffectMix := 0.3;
  SampleRateChanged;
  Clear;
end;

constructor TStkJCReverb.Create(const SampleRate: Single = 44100);
begin
 Create(SampleRate, 0.5);
end;

destructor TStkJCReverb.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FAllpassDelays[0]);
  FreeAndNil(FAllpassDelays[1]);
  FreeAndNil(FAllpassDelays[2]);
  FreeAndNil(FCombDelays[0]);
  FreeAndNil(FCombDelays[1]);
  FreeAndNil(FCombDelays[2]);
  FreeAndNil(FCombDelays[3]);
  FreeAndNil(FOutLeftDelay);
  FreeAndNil(FOutRightDelay);
end;

procedure TStkJCReverb.SampleRateChanged;
var
  i : Integer;
begin
 inherited;
 CalculateInternalLengths;

 for i := 0 to Length(FAllpassDelays) - 1 do
  begin
   if assigned(FAllpassDelays[i]) then
    if FInternalLengths[i + 4] < FAllpassDelays[i].Length
     then FAllpassDelays[i].Delay := FInternalLengths[i + 4]
     else FreeAndNil(FAllpassDelays[i]);

   // create new allpass delay if necessary
   if not assigned(FAllpassDelays[i])
    then FAllpassDelays[i] := TStkDelay.Create(SampleRate, FInternalLengths[i + 4], ExtendToPowerOf2(FInternalLengths[i + 4]) - 1);
  end;

 for i := 0 to Length(FCombDelays) - 1 do
  begin
   if assigned(FCombDelays[i]) then
    if FInternalLengths[i] < FCombDelays[i].Length
     then FCombDelays[i].Delay := FInternalLengths[i]
     else FreeAndNil(FCombDelays[i]);

   // create new comb delay if necessary
   if not assigned(FCombDelays[i])
    then FCombDelays[i] := TStkDelay.Create(SampleRate, FInternalLengths[i], ExtendToPowerOf2(FInternalLengths[i]) - 1);
   FCombCoefficient[i] := Power(10, (-3 * FInternalLengths[i] / (T60 * SampleRate)));
  end;

 if assigned(FOutLeftDelay) then
  if FInternalLengths[7] < FOutLeftDelay.Length
   then FOutLeftDelay.Delay := FInternalLengths[7]
   else FreeAndNil(FOutLeftDelay);

 // create new comb delay if necessary
 if not assigned(FOutLeftDelay)
  then FOutLeftDelay := TStkDelay.Create(SampleRate, FInternalLengths[7], ExtendToPowerOf2(FInternalLengths[7]) - 1);

 if assigned(FOutRightDelay) then
  if FInternalLengths[8] < FOutRightDelay.Length
   then FOutRightDelay.Delay := FInternalLengths[8]
   else FreeAndNil(FOutRightDelay);

 // create new comb delay if necessary
 if not assigned(FOutRightDelay)
  then FOutRightDelay := TStkDelay.Create(SampleRate, FInternalLengths[8], ExtendToPowerOf2(FInternalLengths[8]) - 1);
end;

procedure TStkJCReverb.CalculateInternalLengths;
var
  Scaler   : Double;
  Delay, i : Integer;
begin
 // delay lengths for 44100 Hz sample rate.
 Scaler := SampleRate / 44100.0;

 for i := 0 to 8 do
  begin
   Delay := round(Scaler * CLength[i] + CHalf32) - 1;
   if ((Delay and 1) = 0)
    then Delay := Delay + 1;
   while (not isPrime(Delay))
    do Delay := Delay + 2;
   FInternalLengths[i] := Delay;
  end;
end;

procedure TStkJCReverb.SetT60(const Value: Single);
begin
 if FT60 <> Value then
  begin
   FT60 := Value;
   T60Changed;
  end;
end;

procedure TStkJCReverb.T60Changed;
var
  i: Integer;
begin
 for i := 0 to Length(FCombDelays) - 1
  do FCombCoefficient[i] := Power(10, (-3 * FInternalLengths[i] / (T60 * SampleRate)));
end;

procedure TStkJCReverb.Clear;
begin
  FAllpassDelays[0].Clear;
  FAllpassDelays[1].Clear;
  FAllpassDelays[2].Clear;
  FCombDelays[0].Clear;
  FCombDelays[1].Clear;
  FCombDelays[2].Clear;
  FCombDelays[3].Clear;
  FOutRightDelay.Clear;
  FOutLeftDelay.Clear;
  FLastOutput[0] := 0.0;
  FLastOutput[1] := 0.0;
end;

function TStkJCReverb.Tick(const Input: Single): Single;
var
  Temp    : Single;
  FiltOut : Single;
  Tmp     : array [0..3] of Single;
begin
  Temp := FAllpassDelays[0].LastOutput;
  Tmp[1] := FAllpassCoefficient * Temp;
  Tmp[1] := Tmp[1] + Input;
  FAllpassDelays[0].Tick(Tmp[1]);
  Tmp[1] := -(FAllpassCoefficient * Tmp[1]) + Temp;

  Temp := FAllpassDelays[1].LastOutput;
  Tmp[2] := FAllpassCoefficient * Temp;
  Tmp[2] := Tmp[2] + Tmp[1];
  FAllpassDelays[1].Tick(Tmp[2]);
  Tmp[2] := -(FAllpassCoefficient * Tmp[2]) + Temp;

  Temp := FAllpassDelays[2].LastOutput;
  Tmp[3] := FAllpassCoefficient * Temp;
  Tmp[3] := Tmp[3] + Tmp[2];
  FAllpassDelays[2].Tick(Tmp[3]);
  Tmp[3] := -(FAllpassCoefficient * Tmp[3]) + Temp;

  Tmp[0] := Tmp[3] + (FCombCoefficient[0] * FCombDelays[0].LastOutput);
  Tmp[1] := Tmp[3] + (FCombCoefficient[1] * FCombDelays[1].LastOutput);
  Tmp[2] := Tmp[3] + (FCombCoefficient[2] * FCombDelays[2].LastOutput);
  Tmp[3] := Tmp[3] + (FCombCoefficient[3] * FCombDelays[3].LastOutput);

  FCombDelays[0].Tick(Tmp[0]);
  FCombDelays[1].Tick(Tmp[1]);
  FCombDelays[2].Tick(Tmp[2]);
  FCombDelays[3].Tick(Tmp[3]);

  FiltOut := Tmp[0] + Tmp[1] + Tmp[2] + Tmp[3];

  FLastOutput[0] := FEffectMix * (FOutLeftDelay.Tick(FiltOut));
  FLastOutput[1] := FEffectMix * (FOutRightDelay.Tick(FiltOut));
  Temp := (1.0 - FEffectMix) * Input;
  FLastOutput[0] := FLastOutput[0] + Temp;
  FLastOutput[1] := FLastOutput[1] + Temp;

  Result := (FLastOutput[0] + FLastOutput[1]) * 0.5;
end;

end.
