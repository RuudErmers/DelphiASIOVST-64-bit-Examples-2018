unit DAV_StkMoog;

{  STK moog-like swept filter sampling synthesis class.

   This instrument uses one attack wave, one looped wave, and an ADSR envelope
   (inherited from the Sampler class) and adds two sweepable formant
   (FormantSweep) filters.

    Control Change Numbers:
       - Filter Q = 2
       - Filter Sweep Rate = 4
       - Vibrato Frequency = 11
       - Vibrato Gain = 1
       - Gain = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkWavePlayer, DAV_StkSampler,
  DAV_StkFormantSweep;

type
  TStkMoog = class(TStkSampler)
  private
    // Set the modulation (vibrato) depth.
    procedure SetModulationDepth(const Value: Single);

    // Set the modulation (vibrato) speed in Hz.
    procedure SetModulationSpeed(const Value: Single);
  protected
    FFilters    : array[0..1] of TStkFormantSweep;
    FModDepth   : Single;
    FFilterQ    : Single;
    FFilterRate : Single;

    procedure FrequencyChanged; override;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Start a note with the given frequency and amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by number and value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single); override;
  end;

implementation

uses
  SysUtils;

constructor TStkMoog.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  FAttacks[0] := TStkWavePlayer.Create(SampleRate, 'mandpluk.wav');
  FLoops[0] := TStkWavePlayer.Create(SampleRate, 'impuls20.wav');
  FLoops[1] := TStkWavePlayer.Create(SampleRate, 'sinewave.wav');
  FLoops[0].OneShot := False;
  FLoops[1].OneShot := False;
  FLoops[1].Frequency := 6.122;
  FFilters[0] := TStkFormantSweep.Create(SampleRate);
  FFilters[0].setTargets(0.0, 0.7);
  FFilters[1] := TStkFormantSweep.Create(SampleRate);
  FFilters[1].setTargets(0.0, 0.7);
  FAdsr.setAllTimes(0.001, 1.5, 0.6, 0.250);
  FFilterQ := 0.85;
  FFilterRate := 0.0001;
  FModDepth := 0.0;
end;

destructor TStkMoog.Destroy;
begin
 FreeAndNil(FAttacks[0]);
 FreeAndNil(FLoops[0]);
 FreeAndNil(FLoops[1]);
 FreeAndNil(FFilters[0]);
 FreeAndNil(FFilters[1]);
 inherited Destroy;
end;

procedure TStkMoog.FrequencyChanged;
begin
//  rate := FAttacks[0].Size * 0.01 * FBaseFrequency * FSampleRateInv;
//  FAttacks[0].Rate := rate;
  FAttacks[0].Frequency := FBaseFrequency;
  FLoops[0].Frequency := FBaseFrequency;
end;

procedure TStkMoog.NoteOn(const Frequency, Amplitude: Single);
var
  temp: Single;
begin
  SetFrequency(Frequency);
  keyOn;
  FAttackGain := Amplitude * 0.5;
  FLoopGain := Amplitude;

  temp := FFilterQ + 0.05;
  FFilters[0].SetStates(2000.0, temp);
  FFilters[1].setStates(2000.0, temp);

  temp := FFilterQ + 0.099;
  FFilters[0].setTargets(Frequency, temp);
  FFilters[1].setTargets(Frequency, temp);

  FFilters[0].SweepRate := FFilterRate * 22050.0 * FSampleRateInv;
  FFilters[1].SweepRate := FFilterRate * 22050.0 * FSampleRateInv;
end;

procedure TStkMoog.SetModulationSpeed(const Value: Single);
begin
  FLoops[1].Frequency := Value;
end;

procedure TStkMoog.SetModulationDepth;
begin
  FModDepth := Value * 0.5;
end;

function TStkMoog.Tick: Single;
var
  temp: Single;
begin
  if (FModDepth <> 0.0) then
   begin
    temp := FLoops[1].Tick * FModDepth;
    FLoops[0].Frequency := FBaseFrequency * (1.0 + temp);
   end;

  temp := inherited Tick;
  temp := FFilters[0].Tick(temp);
  FLastOutput := FFilters[1].Tick(temp);
  Result := lastOutput * 3.0;
end;

procedure TStkMoog.ControlChange(const Number: Integer; const Value: Single);
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (Number = CMidiFilterQ) then // 2
    FFilterQ := 0.80 + (0.1 * norm)
  else if (Number = CMidiFilterSweepRate) then // 4
    FFilterRate := norm * 0.0002
  else if (Number = CMidiModFrequency) then // 11
    SetModulationSpeed(norm * 12.0)
  else if (Number = CMidiModWheel) then // 1
    SetModulationDepth(norm)
  else if (Number = CMidiAfterTouchCont) then // 128
    FAdsr.Target := norm;
end;

end.
