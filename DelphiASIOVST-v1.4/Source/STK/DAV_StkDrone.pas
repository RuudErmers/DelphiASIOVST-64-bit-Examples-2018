unit DAV_StkDrone;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK "drone" plucked string model.

   This class implements a simple plucked string physical model based on the
   Karplus-Strong algorithm.

   This is a digital waveguide model, making its use possibly subject to
   patents held by Stanford University, Yamaha, and others.
   There exist at least two patents, assigned to Stanford, bearing the names
   of Karplus and/or Strong.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkDelayA, DAV_StkOneZero,
  DAV_StkAdsr, DAV_StkNoise;

type
  TStkDrone = class(TStkInstrument)
  private
    procedure FrequencyChanged;
  protected
    FDelayLine     : TStkDelayA;
    FLoopFilter    : TStkOneZero;
    FEnvelope      : TStkAdsr;
    FNoise         : TStkNoise;
    FLength        : Integer;
    FLoopGain      : Single;
    FBaseFrequency : Single;

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;
  public
    // Class constructor, taking the lowest desired playing frequency.
    constructor Create(const SampleRate, LowestFrequency: Single); reintroduce; virtual;

    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Pluck the string with the given amplitude using the current frequency.
    procedure Pluck(const Amplitude: Single);

    // Start a note with the given frequency and amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;
  end;

implementation

uses
  SysUtils;

constructor TStkDrone.Create(const SampleRate, LowestFrequency: Single);
begin
  inherited Create(SampleRate);
  FLength := round(FSampleRate / LowestFrequency + 1);
  FLoopGain := 0.999;
  FDelayLine := TStkDelayA.Create(FSampleRate, (FLength / 2.0), FLength);
  FLoopFilter := TStkOneZero.Create(FSampleRate);
  FNoise := TStkNoise.Create(FSampleRate);
  FEnvelope := TStkAdsr.Create(FSampleRate);
  FEnvelope.setAllTimes(2.0, 0.5, 0.0, 0.5);
  Clear;
end;

destructor TStkDrone.Destroy;
begin
 FreeAndNil(FDelayLine);
 FreeAndNil(FLoopFilter);
 FreeAndNil(FEnvelope);
 FreeAndNil(FNoise);
 inherited;
end;

procedure TStkDrone.Clear;
begin
  FDelayLine.Clear;
  FLoopFilter.Clear;
end;

procedure TStkDrone.SetFrequency(const Value: Single);
begin
 if FBaseFrequency <> Value then
  begin
   if (Value <= 0.0)
    then FBaseFrequency := 220.0
    else FBaseFrequency := Value;

   FrequencyChanged;
  end;
end;

procedure TStkDrone.FrequencyChanged;
var
  Delay: Single;
begin
 // Delay = FLength - approximate filter Delay.
  Delay := (FSampleRate / FBaseFrequency) - 0.5;
  if (Delay <= 0.0) then Delay := 0.3
  else if (Delay > FLength) then Delay := FLength;
  FDelayLine.Delay := Delay;
  FLoopGain := 0.997 + (FBaseFrequency * 0.000002);
  if (FLoopGain >= 1.0) then FLoopGain := 0.99999;
end;

function TStkDrone.GetFrequency: Single;
begin
 result := FBaseFrequency;
end;

procedure TStkDrone.Pluck(const Amplitude: Single);
begin
 FEnvelope.KeyOn;
end;

procedure TStkDrone.NoteOn(const Frequency, Amplitude: Single);
begin
  SetFrequency(Frequency);
  Pluck(Amplitude);
end;

procedure TStkDrone.NoteOff(const Amplitude: Single);
begin
  FLoopGain := Limit(1.0 - Amplitude, 0, 0.99999);
end;

function TStkDrone.Tick: Single;
begin
 // Here's the whole inner loop of the instrument!!
  FLastOutput := FDelayLine.Tick(FLoopFilter.Tick(FDelayLine.LastOutput * FLoopGain) +
    (0.005 * FEnvelope.Tick * FNoise.Tick));
  Result := FLastOutput;
end;

end.
