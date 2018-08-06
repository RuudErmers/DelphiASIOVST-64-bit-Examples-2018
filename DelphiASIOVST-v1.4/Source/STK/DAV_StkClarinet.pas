unit DAV_StkClarinet;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.


{ STK TStkClarinet physical model class.
  -----------------------------------

  This class implements a simple clarinet physical model, as discussed by
  Smith (1986), McIntyre, Schumacher, Woodhouse (1983), and others.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.

  Control Change Numbers:
    - Reed Stiffness = 2
    - Noise Gain = 4
    - Vibrato Frequency = 11
    - Vibrato Gain = 1
    - Breath Pressure = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkDelayl, DAV_StkReedTable,
  DAV_StkOnezero, DAV_StkEnvelope, DAV_StkNoise, DAV_StkLFO;

type
  TStkClarinet = class(TStkControlableInstrument)
  private
  protected
    FDelayLine     : TStkDelayL;
    FReedTable     : TStkReedTable;
    FFilter        : TStkOneZero;
    FEnvelope      : TStkEnvelope;
    FNoise         : TStkNoise;
    FVibrato       : TStkLfo;
    FLength        : Integer;
    FOutputGain    : Single;
    FNoiseGain     : Single;
    FVibratoGain   : Single;
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

    // Apply breath pressure to instrument with given amplitude and rate of increase.
    procedure StartBlowing(const Amplitude, Rate: Single);

    // Decrease breath pressure with given rate of decrease.
    procedure StopBlowing(const Rate: Single);

    // Start a note with the given Frequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by \e Number and \e value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single); override;
  end;

implementation

uses
  SysUtils;

constructor TStkClarinet.Create;
begin
  inherited Create(SampleRate);
  FLength := round(SampleRate / lowestFrequency + 1);
  FDelayLine := TStkDelayL.Create(SampleRate, (FLength / 2.0), FLength);
  FReedTable := TStkReedTable.Create(SampleRate);
  FReedTable.Offset := 0.7;
  FReedTable.Slope := -0.3;
  FFilter := TStkOneZero.Create(SampleRate);
  FEnvelope := TStkEnvelope.Create(SampleRate);
  FNoise := TStkNoise.Create(SampleRate);
  FVibrato := TStkLfo.Create(SampleRate);
  FVibrato.Frequency := 5.735;
  FOutputGain := 1.0;
  FNoiseGain := 0.2;
  FVibratoGain := 0.1;
end;

destructor TStkClarinet.Destroy;
begin
  FreeAndNil(FDelayLine);
  FreeAndNil(FReedTable);
  FreeAndNil(FFilter);
  FreeAndNil(FEnvelope);
  FreeAndNil(FNoise);
  FreeAndNil(FVibrato);
  inherited Destroy;
end;

procedure TStkClarinet.Clear;
begin
  FDelayLine.Clear;
  FFilter.Tick(0.0);
end;

procedure TStkClarinet.SetFrequency(const Value: Single);
begin
 if FBaseFrequency <> Value then
  begin
   if (Value <= 0.0)
    then FBaseFrequency := 220.0
    else FBaseFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TStkClarinet.FrequencyChanged;
var
  Delay: Single;
begin
 // Delay := FLength - approximate FFilter delay.
 Delay := (SampleRate / FBaseFrequency) * 0.5 - 1.5;
 if (Delay <= 0.0) then Delay := 0.3
 else if (Delay > FLength) then Delay := FLength;
 FDelayLine.Delay := Delay;
end;

function TStkClarinet.GetFrequency: Single;
begin
 result := FBaseFrequency;
end;

procedure TStkClarinet.StartBlowing(const Amplitude, Rate: Single);
begin
  FEnvelope.Rate := Rate;
  FEnvelope.Target := Amplitude;
end;

procedure TStkClarinet.StopBlowing(const Rate: Single);
begin
  FEnvelope.Rate := Rate;
  FEnvelope.Target := 0.0;
end;

procedure TStkClarinet.NoteOn(const Frequency, Amplitude: Single);
begin
  SetFrequency(Frequency);
  StartBlowing(0.55 + (Amplitude * 0.30), Amplitude * 0.005);
  FOutputGain := Amplitude + 0.001;
end;

procedure TStkClarinet.NoteOff(const Amplitude: Single);
begin
  StopBlowing(Amplitude * 0.01);
end;

function TStkClarinet.Tick: Single;
var
  breathPressure, pressureDiff: Single;
begin
  // Calculate the breath pressure (FEnvelope + FNoise + FVibrato)
  breathPressure := FEnvelope.Tick;
  breathPressure := breathPressure + breathPressure * FNoiseGain * FNoise.Tick;
  breathPressure := breathPressure + breathPressure * FVibratoGain * FVibrato.Tick;

  // Perform commuted loss filtering.
  pressureDiff := -0.95 * FFilter.Tick(FDelayLine.LastOutput);

  // Calculate pressure difference of reflected and mouthpiece pressures.
  pressureDiff := pressureDiff - breathPressure;

  // Perform non-linear scattering using pressure difference in reed function.
  FLastOutput := FDelayLine.Tick(breathPressure + pressureDiff * FReedTable.Tick(pressureDiff));

  // Apply output gain.
  FLastOutput := FLastOutput * FOutputGain;

  Result := FLastOutput;
end;

procedure TStkClarinet.ControlChange(const Number: Integer; const Value: Single);
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (Number = CMidiReedStiffness) then // 2
    FReedTable.Slope := -0.44 + (0.26 * norm)
  else if (Number = CMidiNoiseLevel) then // 4
    FNoiseGain := (norm * 0.4)
  else if (Number = CMidiModFrequency) then // 11
    FVibrato.Frequency := norm * 12
  else if (Number = CMidiModWheel) then // 1
    FVibratoGain := (norm * 0.5)
  else if (Number = CMidiAfterTouchCont) then // 128
    FEnvelope.CurrentValue := norm;
end;

end.
