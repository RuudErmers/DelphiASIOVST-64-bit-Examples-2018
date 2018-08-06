unit DAV_StkPerryCookReverb;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ Perry's simple reverberator class.

  This class is based on some of the famous Stanford/CCRMA reverbs
  (NRev, KipRev), which were based on the Chowning/Moorer/Schroeder
  reverberators using networks of simple allpass and comb delay filters.
  This class implements two series allpass units and two parallel comb filters.

}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkReverb, DAV_StkDelay, Math;

type
  TStkPerryCookReverb = class(TStkReverb)
  private
    FT60: SIngle;
    procedure SetT60(const Value: SIngle);
    procedure UpdateDelayTimes;
  protected
    FAllpassDelays      : array [0..1] of TStkDelay;
    FCombDelays         : array [0..1] of TStkDelay;
    FAllpassCoefficient : Single;
    FCombCoefficient    : array [0..1] of Single;
    FInternalLengths    : array [0..3] of Integer;

    procedure CalculateInternalLengths; virtual;
    procedure T60Changed; virtual;
    procedure SampleRateChanged; override;
  public
    // class constructor taking a T60 decay time argument.
    constructor Create(const SampleRate: Single = 44100); override;

    // class destructor.
    destructor Destroy; override;

    // reset and clear all internal state.
    procedure Clear; override;

    // compute one output sample.
    function Tick(const Input: Single): Single; override;

    property T60: SIngle read FT60 write SetT60;
  end;

implementation

uses
  SysUtils, DAV_StkFilter;

constructor TStkPerryCookReverb.Create(const SampleRate: Single = 44100);
begin
 FT60 := 1;
 FAllpassCoefficient := 0.7;
 FEffectMix := 0.5;
 FAllpassDelays[0] := nil;
 FAllpassDelays[1] := nil;
 FCombDelays[0] := nil;
 FCombDelays[1] := nil;
 inherited Create(SampleRate);
 Clear;
end;

destructor TStkPerryCookReverb.Destroy;
begin
 FreeAndNil(FAllpassDelays[0]);
 FreeAndNil(FAllpassDelays[1]);
 FreeAndNil(FCombDelays[0]);
 FreeAndNil(FCombDelays[1]);
 inherited Destroy;
end;

procedure TStkPerryCookReverb.SampleRateChanged;
begin
 inherited;
 UpdateDelayTimes;
end;

procedure TStkPerryCookReverb.SetT60(const Value: SIngle);
begin
 if FT60 <> Value then
  begin
   FT60 := Value;
   T60Changed;
  end;
end;

procedure TStkPerryCookReverb.T60Changed;
begin
 UpdateDelayTimes;
end;

procedure TStkPerryCookReverb.UpdateDelayTimes;
var
  i : Integer;
begin
 CalculateInternalLengths;
 for i := 0 to 1 do
  begin
   if Assigned(FAllpassDelays[i]) then FreeAndNil(FAllpassDelays[i]);
   FAllpassDelays[i] := TStkDelay.Create(SampleRate, FInternalLengths[i], FInternalLengths[i]);

   if Assigned(FCombDelays[i]) then FreeAndNil(FCombDelays[i]);
   FCombDelays[i] := TStkDelay.Create(SampleRate, FInternalLengths[i + 2], FInternalLengths[i + 2]);
   FCombCoefficient[i] := Power(10, (-3 * FInternalLengths[i + 2] / (FT60 * SampleRate)));
  end;
end;

procedure TStkPerryCookReverb.CalculateInternalLengths;
const
  CFInternalLengths: array[0..3] of Integer = (353, 1097, 1777, 2137);
var
  ScaleFactor: Double;
  Delay, i : Integer;
begin
 // Delay FInternalLengths for 44100 Hz sample rate.
 ScaleFactor := SampleRate / 44100.0;

 // Scale the delay FInternalLengths if necessary.
 if (ScaleFactor <> 1.0) then
  for i := 0 to 3 do
   begin
    Delay := Round(Floor(ScaleFactor * CFInternalLengths[i]));
    if ((Delay and 1) = 0)
     then Delay := Delay + 1;
    while (not isPrime(Delay)) do Delay := Delay + 2;
    FInternalLengths[i] := Delay;
   end
 else Move(CFInternalLengths[0], FInternalLengths[0], Length(FInternalLengths) * SizeOf(Integer));
end;

procedure TStkPerryCookReverb.Clear;
begin
 inherited;
 FAllpassDelays[0].Clear;
 FAllpassDelays[1].Clear;
 FCombDelays[0].Clear;
 FCombDelays[1].Clear;
 FLastOutput[0] := 0.0;
 FLastOutput[1] := 0.0;
end;

function TStkPerryCookReverb.Tick(const Input: Single): Single;
var
  temp, temp0, temp1, temp2, temp3: Single;
begin
  temp := FAllpassDelays[0].LastOutput;
  temp0 := FAllpassCoefficient * temp;
  temp0 := temp0 + Input;
  FAllpassDelays[0].Tick(temp0);
  temp0 := -(FAllpassCoefficient * temp0) + temp;

  temp := FAllpassDelays[1].LastOutput;
  temp1 := FAllpassCoefficient * temp;
  temp1 := temp1 + temp0;
  FAllpassDelays[1].Tick(temp1);
  temp1 := -(FAllpassCoefficient * temp1) + temp;

  temp2 := temp1 + (FCombCoefficient[0] * FCombDelays[0].LastOutput);
  temp3 := temp1 + (FCombCoefficient[1] * FCombDelays[1].LastOutput);

  FLastOutput[0] := FEffectMix * (FCombDelays[0].Tick(temp2));
  FLastOutput[1] := FEffectMix * (FCombDelays[1].Tick(temp3));
  temp := (1.0 - effectMix) * Input;
  FLastOutput[0] := FLastOutput[0] + temp;
  FLastOutput[1] := FLastOutput[1] + temp;
  Result := (FLastOutput[0] + FLastOutput[1]) * 0.5;
end;

end.
