unit DAV_StkPlucked;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK TStkPlucked string model class.

  This class implements a simple TStkPlucked string physical model based on the
  Karplus-Strong algorithm.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others. There exist at least two
  patents, assigned to Stanford, bearing the names of Karplus and/or Strong.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkDelaya, DAV_StkOneZero,
  DAV_StkOnePole, DAV_StkNoise;

type
  TStkPlucked = class(TStkInstrument)
  private
  protected
    FDelayLine     : TStkDelayA;
    FLoopFilter    : TStkOneZero;
    FPpickFilter   : TStkOnePole;
    FNoise         : TStkNoise;
    FLength        : Integer;
    FLoopGain      : Single;
    FBaseFrequency : Single;

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;

    procedure FrequencyChanged; virtual;
  public
    // Class constructor, taking the lowest desired playing frequency.
    constructor Create(const SampleRate, LowestFrequency: Single); reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Pluck the string with the given Amplitude using the current frequency.
    procedure Pluck(const Amplitude: Single);

    // Start a note with the given frequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;
  end;

implementation

uses
  SysUtils;

constructor TStkPlucked.Create;
begin
  inherited Create(SampleRate);
  FLength := round(SampleRate / LowestFrequency + 1);
  FLoopGain := 0.999;
  FDelayLine := TStkDelayA.Create(SampleRate, (FLength / 2.0), FLength);
  FLoopFilter := TStkOneZero.Create(SampleRate);
  FPpickFilter := TStkOnePole.Create(SampleRate);
  FNoise := TStkNoise.Create(SampleRate);
  Clear;
end;

destructor TStkPlucked.Destroy;
begin
 FreeAndNil(FDelayLine);
 FreeAndNil(FLoopFilter);
 FreeAndNil(FPpickFilter);
 FreeAndNil(FNoise);
 inherited Destroy;
end;

procedure TStkPlucked.Clear;
begin
  FDelayLine.Clear;
  FLoopFilter.Clear;
  FPpickFilter.Clear;
end;

procedure TStkPlucked.SetFrequency(const Value: Single);
begin
 if FBaseFrequency <> Value then
  begin
   if Value <= 0.0
    then FBaseFrequency := 220.0
    else FBaseFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TStkPlucked.FrequencyChanged;
var
  Delay: Single;
begin
 // Delay := FLength - approximate filter Delay.
 Delay := (SampleRate / FBaseFrequency) - 0.5;
 if (Delay <= 0.0) then Delay := 0.3
 else if (Delay > FLength) then Delay := FLength;
 FDelayLine.Delay := Delay;
 FLoopGain := 0.995 + (FBaseFrequency * 0.000005);
 if (FLoopGain >= 1.0) then FLoopGain := 0.99999;
end;

function TStkPlucked.GetFrequency: Single;
begin
 result := FBaseFrequency;
end;

procedure TStkPlucked.Pluck(const Amplitude: Single);
var
  Gain: Single;
  i: Integer;
begin
  Gain := Limit(Amplitude, 0, 1);

  FPpickFilter.SetPole(0.999 - (gain * 0.15));
  FPpickFilter.Gain := Gain * 0.5;

  // Fill delay with noise additively with current contents.
  for i := 0 to FLength - 1
   do FDelayLine.Tick(0.6 * FDelayLine.LastOutput + FPpickFilter.Tick(FNoise.Tick));
end;

procedure TStkPlucked.NoteOn(const Frequency, Amplitude: Single);
begin
  SetFrequency(Frequency);
  Pluck(Amplitude);
end;

procedure TStkPlucked.NoteOff(const Amplitude: Single);
begin
  FLoopGain := Limit(1.0 - Amplitude, 0, 0.99999);
end;

function TStkPlucked.Tick: Single;
begin
  // Here's the whole inner loop of the instrument!!
  FLastOutput := FDelayLine.Tick(FLoopFilter.Tick(FDelayLine.LastOutput * FLoopGain)) * 3;
  Result := FLastOutput;
end;

end.
