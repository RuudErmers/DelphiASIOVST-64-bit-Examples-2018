unit DAV_StkNReverb;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ CCRMA's TNRev reverberator class.

  This class is derived from the CLM TNRev function, which is based on the use
  of networks of simple allpass and comb delay filters. This particular
  arrangement consists of 6 comb filters in parallel, followed by 3 allpass
  filters, a lowpass filter, and another allpass in series, followed by two
  allpass filters in parallel with corresponding right and left outputs.

}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkReverb, DAV_StkDelay, Math;

type
  TStkNReverb = class(TStkReverb)
  private
    procedure SetT60(const Value: Single);
    procedure T60Changed;
    procedure CalculateInternalLengths;
  protected
    FAllpassDelays      : array [0..7] of TStkDelay;
    FCombDelays         : array [0..5] of TStkDelay;
    FLowpassState       : Single;
    FAllpassCoefficient : Single;
    FT60                : Single;
    FInternalLengths    : array [0..14] of Integer;
    FCombCoefficient    : array [0..5] of Single;
    procedure SampleRateChanged; override;
  public
    constructor Create(const SampleRate: Single = 44100); overload; override;
    constructor Create(const SampleRate, T60: Single); reintroduce; overload; virtual;
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear; override;

    // Compute one output sample.
    function Tick(const Input: Single): Single; override;

    property T60: Single read FT60 write SetT60;
  end;

implementation

uses
  SysUtils, DAV_Common, DAV_Math, DAV_StkFilter;

constructor TStkNReverb.Create(const SampleRate, T60: Single);
begin
  FT60 := T60;
  FAllpassCoefficient := 0.7;
  FEffectMix := 0.3;

  inherited Create(SampleRate);

  SampleRateChanged;
  Clear;
end;

constructor TStkNReverb.Create(const SampleRate: Single = 44100);
begin
 Create(SampleRate, 0.5);
end;

destructor TStkNReverb.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  for i := 0 to Length(FCombDelays) - 1 do FreeAndNil(FCombDelays[i]);
  for i := 0 to Length(FAllpassDelays) - 1 do FreeAndNil(FAllpassDelays[i]);
end;

procedure TStkNReverb.CalculateInternalLengths;
const
  CLengths: array[0..14] of Integer = (
    1433, 1601, 1867, 2053, 2251, 2399, 347, 113, 37, 59, 53, 43, 37, 29, 19);
var
  ScaleFactor: Double;
  Delay, i : Integer;
begin
 ScaleFactor := SampleRate / 25641.0;
 for i := 0 to Length(CLengths) - 1 do
  begin
   Delay := round(ScaleFactor * CLengths[i] - 0.5);
   if (Delay and 1) = 0
    then Delay := Delay + 1;
   while (not IsPrime(Delay)) do inc(Delay, 2);
   FInternalLengths[i] := Delay;
  end;
end;

procedure TStkNReverb.SampleRateChanged;
const
  CLengths: array[0..14] of Integer = (
    1433, 1601, 1867, 2053, 2251, 2399, 347, 113, 37, 59, 53, 43, 37, 29, 19);
var
  i : Integer;
begin
 inherited;

 CalculateInternalLengths;

 for i := 0 to Length(FAllpassDelays) - 1 do
  begin
   if Assigned(FAllpassDelays[i]) then
    if FInternalLengths[i + 6] < FAllpassDelays[i].Length
     then FAllpassDelays[i].Delay := FInternalLengths[i + 6]
     else FreeAndNil(FAllpassDelays[i]);

   // create new allpass delay if necessary
   if not Assigned(FAllpassDelays[i])
    then FAllpassDelays[i] := TStkDelay.Create(SampleRate, FInternalLengths[i + 6], ExtendToPowerOf2(FInternalLengths[i + 6]) - 1);
  end;

 for i := 0 to Length(FCombDelays) - 1 do
  begin
   if Assigned(FCombDelays[i]) then
    if FInternalLengths[i] < FCombDelays[i].Length
     then FCombDelays[i].Delay := FInternalLengths[i]
     else FreeAndNil(FCombDelays[i]);

   // create new comb delay if necessary
   if not Assigned(FCombDelays[i])
    then FCombDelays[i] := TStkDelay.Create(SampleRate, FInternalLengths[i], ExtendToPowerOf2(FInternalLengths[i]) - 1);
   FCombCoefficient[i] := Power(10, (-3 * FInternalLengths[i] / (T60 * SampleRate)));
  end;
end;

procedure TStkNReverb.SetT60(const Value: Single);
begin
 if T60 <> Value then
  begin
   FT60 := Value;
   T60Changed;
  end;
end;

procedure TStkNReverb.T60Changed;
var
  i: Integer;
begin
 for i := 0 to Length(FCombDelays) - 1
  do FCombCoefficient[i] := Power(10, (-3 * FInternalLengths[i] / (T60 * SampleRate)));
end;

procedure TStkNReverb.Clear;
var
  i: Integer;
begin
  for i := 0 to Length(FCombDelays) - 1 do FCombDelays[i].Clear;
  for i := 0 to Length(FAllpassDelays) - 1 do FAllpassDelays[i].Clear;
  FLastOutput[0] := 0.0;
  FLastOutput[1] := 0.0;
  FLowpassState := 0.0;
end;

function TStkNReverb.Tick(const Input: Single): Single;
var
  temp : Single;
  tmp  : Array [0..3] of Single;
  i    : Integer;
begin
  tmp[0] := 0.0;
  for i := 0 to Length(FCombDelays) - 1 do
   begin
    temp := Input + (FCombCoefficient[i] * FCombDelays[i].LastOutput);
    tmp[0] := tmp[0] + FCombDelays[i].Tick(temp);
   end;
  for i := 0 to 2 do
   begin
    temp := FAllpassDelays[i].LastOutput;
    tmp[1] := FAllpassCoefficient * temp;
    tmp[1] := tmp[1] + tmp[0];
    FAllpassDelays[i].Tick(tmp[1]);
    tmp[0] := -(FAllpassCoefficient * tmp[1]) + temp;
   end;

  // One-pole lowpass filter.
  FLowpassState := 0.7 * FLowpassState + 0.3 * tmp[0];
  temp := FAllpassDelays[3].LastOutput;
  tmp[1] := FAllpassCoefficient * temp;
  tmp[1] := tmp[1] + FLowpassState;
  FAllpassDelays[3].Tick(tmp[1]);
  tmp[1] := -(FAllpassCoefficient * tmp[1]) + temp;

  temp := FAllpassDelays[4].LastOutput;
  tmp[2] := FAllpassCoefficient * temp;
  tmp[2] := tmp[2] + tmp[1];
  FAllpassDelays[4].Tick(tmp[2]);
  FLastOutput[0] := FEffectMix * (-(FAllpassCoefficient * tmp[2]) + temp);

  temp := FAllpassDelays[5].LastOutput;
  tmp[3] := FAllpassCoefficient * temp;
  tmp[3] := tmp[3] + tmp[1];
  FAllpassDelays[5].Tick(tmp[3]);
  FLastOutput[1] := FEffectMix * (-(FAllpassCoefficient * tmp[3]) + temp);

  temp := (1.0 - FEffectMix) * Input;
  FLastOutput[0] := FLastOutput[0] + temp;
  FLastOutput[1] := FLastOutput[1] + temp;

  Result := (FLastOutput[0] + FLastOutput[1]) * 0.5;
end;

end.
