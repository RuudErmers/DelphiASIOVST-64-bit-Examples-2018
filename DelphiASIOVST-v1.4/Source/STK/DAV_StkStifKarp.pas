unit DAV_StkStifKarp;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK plucked stiff string instrument.

  This class implements a simple plucked string algorithm (Karplus Strong) with
  enhancements (Jaffe-Smith, Smith, and others), including string stiffness and
  Pluck position controls. The stiffness is modeled with allpass filters.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.

  Control Change Numbers:
    - Pickup Position = 4
    - String Sustain = 11
    - String Stretch = 1
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkDelayl, DAV_StkDelaya,
  DAV_StkOneZero, DAV_StkNoise, DAV_StkBiquad;

type
  TStkStifKarp = class(TStkControlableInstrument)
  private
    // Set the Stretch "factor" of the string (0.0 - 1.0).
    procedure SetStretch(const Value: Single);

    // Set the Pluck or "excitation" position along the string (0.0 - 1.0).
    procedure SetPickupPosition(const Value: Single);

    // Set the base loop gain.
  {
    The actual loop gain is set according to the Frequency.
    Because of high-Frequency loop FFilter roll-off, higher
    Frequency settings have greater loop gains.
  }
    procedure SetBaseLoopGain(const aGain: Single);
  protected
    FDelayLine      : TStkDelayA;
    FCombDelay      : TStkDelayl;
    FFilter         : TStkOneZero;
    FNoise          : TStkNoise;
    FBiQuad         : array[0..3] of TStkBiquad;
    FLength         : Integer;
    FLoopGain       : Single;
    FBaseLoopGain   : Single;
    FLastFrequency  : Single;
    FLastLength     : Single;
    FStretching     : Single;
    FPluckAmplitude : Single;
    FPickupPosition : Single;

    // Set instrument parameters for a particular Frequency.
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;

    procedure FrequencyChanged; virtual;
    procedure StretchChanged; virtual;
  public
    // Class constructor, taking the lowest desired playing Frequency.
    constructor Create(const SampleRate, LowestFrequency: Single); reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Pluck the string with the given amplitude using the current Frequency.
    procedure Pluck(const amplitude: Single);

    // Start a note with the given Frequency and amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by \e Number and \e value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single); override;
  end;

implementation

uses
  SysUtils;

constructor TStkStifKarp.Create;
begin
  inherited Create(SampleRate);
  FLength := round(SampleRate / LowestFrequency + 1);
  FDelayLine := TStkDelayA.Create(SampleRate, 0.5 * FLength, FLength);
  FCombDelay := TStkDelayl.Create(SampleRate, 0.2 * FLength, FLength);

  FFilter := TStkOneZero.Create(SampleRate);
  FNoise := TStkNoise.Create(SampleRate);
  FBiQuad[0] := TStkBiquad.Create(SampleRate);
  FBiQuad[1] := TStkBiquad.Create(SampleRate);
  FBiQuad[2] := TStkBiquad.Create(SampleRate);
  FBiQuad[3] := TStkBiquad.Create(SampleRate);

  FPluckAmplitude := 0.3;
  FPickupPosition := 0.4;
  FLastFrequency := LowestFrequency * 2.0;
  FLastLength := FLength * 0.5;
  FStretching := 0.9999;
  FBaseLoopGain := 0.995;
  FLoopGain := 0.999;

  Clear;
end;

destructor TStkStifKarp.Destroy;
begin
 FreeAndNil(FDelayLine);
 FreeAndNil(FCombDelay);
 FreeAndNil(FFilter);
 FreeAndNil(FNoise);
 FreeAndNil(FBiQuad[0]);
 FreeAndNil(FBiQuad[1]);
 FreeAndNil(FBiQuad[2]);
 FreeAndNil(FBiQuad[3]);
 inherited Destroy;
end;

procedure TStkStifKarp.Clear;
begin
  FDelayLine.Clear;
  FCombDelay.Clear;
  FFilter.Clear;
end;

procedure TStkStifKarp.SetFrequency(const Value: Single);
begin
 if FLastFrequency <> Frequency then
  begin
   if (Frequency <= 0.0)
    then FLastFrequency := 220.0
    else FLastFrequency := Value;
   FrequencyChanged;
  end;
end;


procedure TStkStifKarp.FrequencyChanged;
var
  Delay: Single;
begin
 FLastLength := SampleRate / FLastFrequency;
 Delay := FLastLength - 0.5;
 if (Delay <= 0.0) then Delay := 0.3
 else if (Delay > FLength) then Delay := FLength;
 FDelayLine.Delay := Delay;

 FLoopGain := FBaseLoopGain + (Frequency * 0.000005);
 if (FLoopGain >= 1.0) then FLoopGain := 0.99999;

 StretchChanged;

 FCombDelay.Delay := 0.5 * FPickupPosition * FLastLength;
end;

function TStkStifKarp.GetFrequency: Single;
begin
 result := FLastFrequency;
end;

procedure TStkStifKarp.SetStretch(const Value: Single);
begin
 if FStretching <> Value then
  begin
   FStretching := Value;
   StretchChanged;
  end;
end;

procedure TStkStifKarp.StretchChanged;
var
  temp        : Single;
  dfreq       : Single;
  freq        : Single;
  coefficient : Single;
  i           : Integer;
begin
 freq := FLastFrequency * 2.0;
 dFreq := ((0.5 * SampleRate) - freq) * 0.25;
 temp := 0.5 + (FStretching * 0.5);
 if (temp > 0.9999) then temp := 0.9999;
 for i := 0 to 3 do
  begin
   coefficient := temp * temp;
   FBiQuad[i].setA2(coefficient);
   FBiQuad[i].setB0(coefficient);
   FBiQuad[i].setB2(1.0);

   coefficient := -2.0 * temp * cos(2 * Pi * freq * FSampleRateInv);
   FBiQuad[i].setA1(coefficient);
   FBiQuad[i].setB1(coefficient);

   freq := freq + dFreq;
  end;
end;

procedure TStkStifKarp.SetPickupPosition;
begin
  FPickupPosition := Limit(Value, 0, 1);

  // Set the pick position, which puts zeroes at position * FLength.
  FCombDelay.Delay := 0.5 * FPickupPosition * FLastLength;
end;

procedure TStkStifKarp.SetBaseLoopGain;
begin
  FBaseLoopGain := aGain;
  FLoopGain := FBaseLoopGain + (FLastFrequency * 0.000005);
  if (FLoopGain > 0.99999) then
    FLoopGain := 0.99999;
end;

procedure TStkStifKarp.Pluck;
var
  gain: Single;
  i: Integer;
begin
  gain := amplitude;
  if (gain > 1.0) then
    gain := 1.0
  else if (gain < 0.0) then
    gain := 0.0;

  FPluckAmplitude := gain;
  for i := 0 to FLength - 1 do
    FDelayLine.Tick((FDelayLine.LastOutput * 0.6) + 0.4 * FNoise.Tick * FPluckAmplitude)// Fill delay with FNoise additively with current contents.
//FDelayLine->Tick( FCombDelay->Tick((FDelayLine->lastOut() * 0.6) + 0.4 * FNoise->Tick() * FPluckAmplitude));
  ;
end;

procedure TStkStifKarp.NoteOn(const Frequency, Amplitude: Single);
begin
  SetFrequency(Frequency);
  Pluck(amplitude);
end;

procedure TStkStifKarp.NoteOff(const Amplitude: Single);
begin
  FLoopGain := (1.0 - Limit(Amplitude, 0, 1)) * 0.5;
end;

function TStkStifKarp.Tick: Single;
var
  temp: Single;
  i: integer;
begin
  temp := FDelayLine.LastOutput * FLoopGain;

  // Calculate allpass FStretching.
  for i := 0 to 3
   do temp := FBiQuad[i].Tick(temp);

  // Moving average FFilter.
  temp := FFilter.Tick(temp);

  FLastOutput := FDelayLine.Tick(temp);
  FLastOutput := FLastOutput - FCombDelay.Tick(FLastOutput);
  Result := FLastOutput;
end;

procedure TStkStifKarp.ControlChange(const Number: Integer; const Value: Single);
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (Number = CMidiPickPosition) then // 4
    SetPickupPosition(norm)
  else if (Number = CMidiStringDamping) then // 11
    SetBaseLoopGain(0.97 + (norm * 0.03))
  else if (Number = CMidiStringDetune) then // 1
    SetStretch(0.9 + (0.1 * (1.0 - norm)));
end;

end.

