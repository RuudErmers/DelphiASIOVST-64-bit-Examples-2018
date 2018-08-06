unit DAV_StkVoiceFormant;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  Four formant synthesis instrument.

   This instrument contains an excitation singing wavetable (looping wave
   with random and periodic vibrato, smoothing on frequency, etc.),
   excitation noise, and four sweepable complex resonances.

  Measured formant data is included, and enough data is there to support
  either parallel or cascade synthesis.  In the floating point case cascade
  synthesis is the most natural so that's what you'll find here.

  Control Change Numbers:
    - Voiced/Unvoiced Mix = 2
    - Vowel/Phoneme Selection = 4
    - Vibrato Frequency = 11
    - Vibrato Gain = 1
    - Loudness (Spectral Tilt) = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkEnvelope, DAV_StkNoise,
  DAV_StkSingWave, DAV_StkFormantSweep, DAV_StkOnePole, DAV_StkOneZero,
  DAV_StkPhonemes;

type
  TStkVoiceFormant = class(TStkInstrument)
  private
  protected
    FVoiced        : TStkSingWave;
    FPhonemes      : TStkPhonemes;
    FNoise         : TStkNoise;
    FNoiseEnv      : TStkEnvelope;
    FFilters       : array[0..3] of TStkFormantSweep;
    FOnePole       : TStkOnepole;
    FOneZero       : TStkOnezero;
    FBaseFrequency : Single;

    // Set instrument parameters for a particular Frequency.
    procedure SetFrequency(const Value: Single); override;

    procedure FrequencyChanged; virtual;
  public
    // Class constructor, taking the lowest desired playing Frequency.
    constructor Create(const SampleRate: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Set instrument parameters for the given Phoneme.  Returns FALSE if Phoneme not found.
    function SetPhoneme(const Phoneme: string): Boolean;

    // Set the FVoiced component gain.
    procedure SetVoiced(const Value: Single);

    // Set the unvoiced component gain.
    procedure SetUnVoiced(const Value: Single);

    // Set the sweep Rate for a particular formant filter (0-3).
    procedure SetFilterSweepRate(const Index: Integer; const Rate: Single);

    // Set FVoiced component pitch sweep Rate.
    procedure SetPitchSweepRate(const Value: Single);

    // Start the voice.
    procedure Speak;

    // Stop the voice.
    procedure Quiet;

    // Start a note with the given Frequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by \e Number and \e value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single);
  end;

implementation

uses
  Math, SysUtils;

constructor TStkVoiceFormant.Create;
var
  i: Integer;
begin
  inherited Create(SampleRate);
  FVoiced := TStkSingWave.Create(SampleRate, 'impuls20.wav');
  FVoiced.GainRate := 0.001;
  FVoiced.GainTarget := 0.0;
  FNoise := TStkNoise.Create(SampleRate);
  for i := 0 to 3 do
   begin
    FFilters[i] := TStkFormantSweep.Create(SampleRate);
    FFilters[i].SweepRate := 0.001;
   end;

  FPhonemes := TStkPhonemes.Create(SampleRate);
  FOneZero := TStkOnezero.Create(SampleRate);
  FOneZero.setZero(-0.9);
  FOnePole := TStkOnepole.Create(SampleRate);
  FOnePole.setPole(0.9);

  FNoiseEnv := TStkEnvelope.Create(SampleRate);
  FNoiseEnv.Rate := 0.001;
  FNoiseEnv.Target := 0.0;

  SetPhoneme('eee');
  Clear;
end;

destructor TStkVoiceFormant.Destroy;
var
  i: Integer;
begin
 FreeAndNil(FVoiced);
 FreeAndNil(FNoise);
 FreeAndNil(FOneZero);
 FreeAndNil(FOnePole);
 FreeAndNil(FNoiseEnv);
 FreeAndNil(FPhonemes);
 for i := 0 to 3 do FreeAndNil(FFilters[i]);
 inherited Destroy;
end;

procedure TStkVoiceFormant.Clear;
var
  i: Integer;
begin
  FOneZero.Clear;
  FOnePole.Clear;
  for i := 0 to 3 do
    FFilters[i].Clear;
end;

procedure TStkVoiceFormant.SetFrequency(const Value: Single);
begin
 if FBaseFrequency <> Frequency then
  begin
   if Value <= 0.0
    then FBaseFrequency := 220.0
    else FBaseFrequency := Frequency;
   FrequencyChanged;
  end;
end;

procedure TStkVoiceFormant.FrequencyChanged;
begin
 FVoiced.Frequency := FBaseFrequency;
end;

function TStkVoiceFormant.SetPhoneme(const Phoneme: string): Boolean;
var
  found : Boolean;
  i     : Integer;
begin
  found := False;
  i := 0;
  while ((i < 32) and (not found)) do
   begin
    if (FPhonemes.Name(i) = Phoneme) then
     begin
      found := True;
      FFilters[0].setTargets(FPhonemes.formantFrequency(i, 0), FPhonemes.formantRadius(i, 0),
        power(10.0, FPhonemes.formantGain(i, 0) / 20.0));
      FFilters[1].setTargets(FPhonemes.formantFrequency(i, 1), FPhonemes.formantRadius(i, 1),
        power(10.0, FPhonemes.formantGain(i, 1) / 20.0));
      FFilters[2].setTargets(FPhonemes.formantFrequency(i, 2), FPhonemes.formantRadius(i, 2),
        power(10.0, FPhonemes.formantGain(i, 2) / 20.0));
      FFilters[3].setTargets(FPhonemes.formantFrequency(i, 3), FPhonemes.formantRadius(i, 3),
        power(10.0, FPhonemes.formantGain(i, 3) / 20.0));
      SetVoiced(FPhonemes.voiceGain(i));
      SetUnVoiced(FPhonemes.noiseGain(i));
     end;
    i := i + 1;
   end;
  Result := found;
end;

procedure TStkVoiceFormant.SetVoiced(const Value: Single);
begin
  FVoiced.GainTarget := Value;
end;

procedure TStkVoiceFormant.SetUnVoiced(const Value: Single);
begin
  FNoiseEnv.Target := Value;
end;

procedure TStkVoiceFormant.SetFilterSweepRate(const Index: Integer; const Rate: Single);
begin
  if Index in [0..3]
   then FFilters[Index].SweepRate := Rate
   else raise Exception.CreateFmt('Sweeprate index out of bounds (%d)', [Index]);
end;

procedure TStkVoiceFormant.SetPitchSweepRate(const Value: Single);
begin
  FVoiced.SweepRate := Value;
end;

procedure TStkVoiceFormant.Speak;
begin
  FVoiced.NoteOn;
end;

procedure TStkVoiceFormant.Quiet;
begin
  FVoiced.NoteOff;
  FNoiseEnv.Target := 0.0;
end;

procedure TStkVoiceFormant.NoteOn;
begin
  SetFrequency(Frequency);
  FVoiced.GainTarget := Amplitude;
  FOnePole.SetPole(0.97 - (Amplitude * 0.2));
end;

procedure TStkVoiceFormant.NoteOff;
begin
  Quiet;
end;

function TStkVoiceFormant.Tick: Single;
var
  temp: Single;
begin
  temp := FOnePole.Tick(FOneZero.Tick(FVoiced.Tick)) + FNoiseEnv.Tick * FNoise.Tick;
  FLastOutput := FFilters[0].Tick(temp) + FFilters[1].Tick(temp) +
    FFilters[2].Tick(temp) + FFilters[3].Tick(temp);
  Result := FLastOutput;
end;

procedure TStkVoiceFormant.ControlChange;
var
  temp, norm : Single;
  i          : Integer;
begin
  norm := Limit(Value, 0, 1);

  if (Number = CMidiBreath) then
   begin // 2
    SetVoiced(1.0 - norm);
    SetUnVoiced(0.01 * norm);
   end
  else if (Number = CMidiFootControl) then
   begin // 4
    temp := 0.0;
    i := round(norm * 128);
    if (i < 32) then
      temp := 0.9
    else if (i < 64) then
     begin
      i := i - 32;
      temp := 1.0;
     end
    else if (i < 96) then
     begin
      i := i - 64;
      temp := 1.1;
     end
    else if (i < 128) then
     begin
      i := i - 96;
      temp := 1.2;
     end
    else if (i = 128) then
     begin
      i := 0;
      temp := 1.4;
     end;
    FFilters[0].setTargets(temp * FPhonemes.formantFrequency(i, 0),
      FPhonemes.formantRadius(i, 0), power(10.0, FPhonemes.formantGain(i, 0) / 20.0));
    FFilters[1].setTargets(temp * FPhonemes.formantFrequency(i, 1),
      FPhonemes.formantRadius(i, 1), power(10.0, FPhonemes.formantGain(i, 1) / 20.0));
    FFilters[2].setTargets(temp * FPhonemes.formantFrequency(i, 2),
      FPhonemes.formantRadius(i, 2), power(10.0, FPhonemes.formantGain(i, 2) / 20.0));
    FFilters[3].setTargets(temp * FPhonemes.formantFrequency(i, 3),
      FPhonemes.formantRadius(i, 3), power(10.0, FPhonemes.formantGain(i, 3) / 20.0));
    SetVoiced(FPhonemes.voiceGain(i));
    SetUnVoiced(FPhonemes.noiseGain(i));
   end
  else if (Number = CMidiModFrequency) then // 11
    FVoiced.VibratoRate := norm * 12  // 0 to 12 Hz
  else if (Number = CMidiModWheel) then // 1
    FVoiced.VibratoGain := norm * 0.2
  else if (Number = CMidiAfterTouchCont) then
   begin // 128
    SetVoiced(norm);
    FOnePole.setPole(0.97 - (norm * 0.2));
   end;
end;

end.
