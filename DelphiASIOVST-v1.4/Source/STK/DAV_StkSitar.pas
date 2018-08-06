unit DAV_StkSitar;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK TStkSitar string model class.

  This class implements a TStkSitar plucked string physical model based on the
  Karplus-Strong algorithm.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others. There exist at least two
  patents, assigned to Stanford, bearing the names of Karplus and/or Strong.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkDelaya, DAV_StkOneZero,
  DAV_StkNoise, DAV_StkAdsr;

type
  TStkSitar = class(TStkInstrument)
  protected
    FDelayLine     : TStkDelayA;
    FLoopFilter    : TStkOneZero;
    FNoise         : TStkNoise;
    FLength        : Longint;
    FAmGain        : Single;
    FDelay         : Single;
    FTargetDelay   : Single;
    FLoopGain      : Single;
    FBaseFrequency : Single;

    // Set instrument parameters for a particular Frequency.
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;

    procedure FrequencyChanged; virtual;
  public
    FEnvelope     : TStkADSR;

    // Class constructor, taking the lowest desired playing Frequency.
    constructor Create(const SampleRate, lowestFrequency: Single); reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Pluck the string with the given amplitude using the current frequency.
    procedure Pluck(Amplitude: Single);

    // Start a note with the given frequency and amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

  end;

implementation

uses
  SysUtils;

constructor TStkSitar.Create;
begin
  inherited Create(SampleRate);
  FLength := round(SampleRate / lowestFrequency + 1);
  FLoopGain := 0.999;
  FDelayLine := TStkDelayA.Create(SampleRate, (FLength / 2.0), FLength);
  FDelay := FLength / 2.0;
  FTargetDelay := FDelay;

  FLoopFilter := TStkOneZero.Create(SampleRate);
  FLoopFilter.setZero(0.01);

  FEnvelope := TStkADSR.Create(SampleRate);
  FEnvelope.setAllTimes(0.001, 0.04, 0.0, 0.5);
  FNoise := TStkNoise.Create(SampleRate);
  Clear;
end;

destructor TStkSitar.Destroy;
begin
 FreeAndNil(FDelayLine);
 FreeAndNil(FLoopFilter);
 FreeAndNil(FEnvelope);
 FreeAndNil(FNoise);
 inherited Destroy;
end;

function TStkSitar.GetFrequency: Single;
begin
 result := FBaseFrequency;
end;

procedure TStkSitar.Clear;
begin
  FDelayLine.Clear;
  FLoopFilter.Clear;
end;

procedure TStkSitar.SetFrequency(const Value: Single);
begin
 if FBaseFrequency <> Frequency then
  begin
   if (Value <= 0.0)
    then FBaseFrequency := 220.0
    else FBaseFrequency := Value;
   FrequencyChanged; 
  end;
end;

procedure TStkSitar.FrequencyChanged;
begin
 FTargetDelay := (SampleRate / FBaseFrequency);
 FDelay := FTargetDelay * (1.0 + (0.05 * FNoise.Tick));
 FDelayLine.Delay := FDelay;
 FLoopGain := 0.995 + (FBaseFrequency * 0.0000005);
 if (FLoopGain > 0.9995) then FLoopGain := 0.9995;
end;

procedure TStkSitar.Pluck;
begin
  FEnvelope.KeyOn;
end;

procedure TStkSitar.NoteOn(const Frequency, Amplitude: Single);
begin
  SetFrequency(Frequency);
  pluck(Amplitude);
  FAmGain := 0.1 * Amplitude;
end;

procedure TStkSitar.NoteOff(const Amplitude: Single);
begin
  FLoopGain := 1.0 - Amplitude;
  if (FLoopGain < 0.0) then
    FLoopGain := 0.0
  else if (FLoopGain > 1.0) then
    FLoopGain := 0.99999;
end;

function TStkSitar.Tick: Single;
begin
  if (abs(FTargetDelay - FDelay) > 0.001) then
   begin
    if (FTargetDelay < FDelay)
     then FDelay := FDelay * 0.99999
     else FDelay := FDelay * 1.00001;
    FDelayLine.Delay := FDelay;
   end;
  FLastOutput := FDelayLine.Tick(FLoopFilter.Tick(FDelayLine.LastOutput *
    FLoopGain) + (FAmGain * FEnvelope.Tick * FNoise.Tick));
  Result := FLastOutput;
end;

end.
