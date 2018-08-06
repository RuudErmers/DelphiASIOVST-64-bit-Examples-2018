unit DAV_StkSaxofony;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK faux conical bore reed instrument class.

  This class implements a "hybrid" digital waveguide instrument that can
  generate a variety of wind-like sounds. It has also been referred to as the
  "blowed string" model. The waveguide section is essentially that of a string,
  with one rigid and one lossy termination. The non-linear function is a reed
  table. The string can be "blown" at any point between the terminations,
  though just as with strings, it is impossible to excite the system at either
  end. If the excitation is placed at the string mid-point, the sound is that
  of a clarinet.  At points closer to the "bridge", the sound is closer to that
  of a saxophone. See Scavone (2002) for more details.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.

  Control Change Numbers:
    - Reed Stiffness = 2
    - Reed Aperture = 26
    - Noise Gain = 4
    - Blow Position = 11
    - Vibrato Frequency = 29
    - Vibrato Gain = 1
    - Breath Pressure = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkDelayl, DAV_StkReedTable,
  DAV_StkOneZero, DAV_StkEnvelope, DAV_StkNoise, DAV_StkLfo;

type
  TStkSaxofony = class(TStkControlableInstrument)
  private
    // Set the "blowing" FPosition between the air column terminations (0.0 - 1.0).
    procedure SetBlowPosition(const Position: Single);
    procedure BlowPositionChanged;
    procedure FrequencyChanged;

  protected
    FDelays        : array[0..1] of TStkDelayL;
    FReedTable     : TStkReedTable;
    FFilter        : TStkOnezero;
    FEnvelope      : TStkEnvelope;
    FNoise         : TStkNoise;
    FVibrato       : TStkLfo;
    FLength        : Integer;
    FOutputGain    : Single;
    FNoiseGain     : Single;
    FVibratoGain   : Single;
    FPosition      : Single;
    FBaseFrequency : Single;

    // Set instrument parameters for a particular AFrequency.
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;
  public
    // Class constructor, taking the lowest desired playing AFrequency.
    constructor Create(const SampleRate, LowestFrequency: Single); reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Apply breath pressure to instrument with given Amplitude and Rate of increase.
    procedure StartBlowing(const Amplitude, Rate: Single);

    // Decrease breath pressure with given Rate of decrease.
    procedure StopBlowing(const Rate: Single);

    // Start a note with the given AFrequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single); override;
  end;

implementation

uses
  SysUtils;

constructor TStkSaxofony.Create;
begin
  inherited Create(SampleRate);
  FLength := round(SampleRate / LowestFrequency + 1);
  // Initialize blowing FPosition to 0.2 of FLength / 2.
  FPosition := 0.2;
  FDelays[0] := TStkDelayL.Create(SampleRate, (1.0 - FPosition) * (FLength shr 1), FLength);
  FDelays[1] := TStkDelayL.Create(SampleRate, FPosition * (FLength shr 1), FLength);

  FReedTable := TStkReedTable.Create(SampleRate);
  FReedTable.Offset := 0.7;
  FReedTable.Slope := 0.3;
  FFilter := TStkOnezero.Create(SampleRate);
  FEnvelope := TStkEnvelope.Create(SampleRate);
  FNoise := TStkNoise.Create(SampleRate);

  FVibrato := TStkLfo.Create(SampleRate);
  FVibrato.Frequency := 5.735;

  FOutputGain := 0.3;
  FNoiseGain := 0.2;
  FVibratoGain := 0.1;
end;

destructor TStkSaxofony.Destroy;
begin
 FreeAndNil(FDelays[0]);
 FreeAndNil(FDelays[1]);
 FreeAndNil(FReedTable);
 FreeAndNil(FFilter);
 FreeAndNil(FEnvelope);
 FreeAndNil(FNoise);
 FreeAndNil(FVibrato);
 inherited Destroy;
end;

procedure TStkSaxofony.Clear;
begin
  FDelays[0].Clear;
  FDelays[1].Clear;
  FFilter.Tick(0.0);
end;

procedure TStkSaxofony.SetFrequency(const Value: Single);
begin
 if FBaseFrequency <> Value then
  begin
   if (Value <= 0.0)
    then FBaseFrequency := 220.0
    else FBaseFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TStkSaxofony.FrequencyChanged;
var
  Delay: Single;
begin
 Delay := (SampleRate / FBaseFrequency) - 3.0;
 if (Delay <= 0.0) then Delay := 0.3
 else if (Delay > FLength) then Delay := FLength;

 FDelays[0].Delay := (1.0 - FPosition) * Delay;
 FDelays[1].Delay := FPosition * Delay;
end;

function TStkSaxofony.GetFrequency: Single;
begin
 result := FBaseFrequency;
end;

procedure TStkSaxofony.SetBlowPosition(const Position: Single);
begin
  if FPosition <> Limit(Position, 0, 1) then
   begin
    FPosition := Limit(Position, 0, 1);
    BlowPositionChanged;
   end;
end;

procedure TStkSaxofony.BlowPositionChanged;
var
  TotalDelay: Single;
begin
 TotalDelay := FDelays[0].Delay;
 TotalDelay := TotalDelay + FDelays[1].Delay;

 FDelays[0].Delay := (1.0 - FPosition) * TotalDelay;
 FDelays[1].Delay := FPosition * TotalDelay;
end;

procedure TStkSaxofony.StartBlowing(const Amplitude, Rate: Single);
begin
  FEnvelope.Rate := Rate;
  FEnvelope.Target := Amplitude;
end;

procedure TStkSaxofony.StopBlowing(const Rate: Single);
begin
  FEnvelope.Rate  := Rate;
  FEnvelope.Target := 0.0;
end;

procedure TStkSaxofony.NoteOn(const Frequency, Amplitude: Single);
begin
  SetFrequency(Frequency);
  StartBlowing(0.55 + (Amplitude * 0.30), Amplitude * 0.005);
  FOutputGain := Amplitude + 0.001;
end;

procedure TStkSaxofony.NoteOff(const Amplitude: Single);
begin
  StopBlowing(Amplitude * 0.01);
end;

function TStkSaxofony.Tick: Single;
var
  pressureDiff, breathPressure, temp: Single;
begin
  // Calculate the breath pressure (FEnvelope + FNoise + FVibrato)
  breathPressure := FEnvelope.Tick;
  breathPressure := breathPressure + breathPressure * FNoiseGain * FNoise.Tick;
  breathPressure := breathPressure + breathPressure * FVibratoGain *
    FVibrato.Tick;

  temp := -0.95 * FFilter.Tick(FDelays[0].LastOutput);
  FLastOutput := temp - FDelays[1].LastOutput;
  pressureDiff := breathPressure - FLastOutput;
  FDelays[1].Tick(temp);
  FDelays[0].Tick(breathPressure - (pressureDiff *
    FReedTable.Tick(pressureDiff)) - temp);

  FLastOutput := FLastOutput * FOutputGain;
  Result := FLastOutput;
end;

procedure TStkSaxofony.ControlChange(const Number: Integer; const Value: Single);
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (number = CMidiReedStiffness) then // 2
    FReedTable.Slope := 0.1 + (0.4 * norm)
  else if (number = CMidiNoiseLevel) then // 4
    FNoiseGain := (norm * 0.4)
  else if (number = 29) then // 29
    FVibrato.Frequency := norm * 12.0
  else if (number = CMidiModWheel) then // 1
    FVibratoGain := (norm * 0.5)
  else if (number = CMidiAfterTouchCont) then // 128
    FEnvelope.CurrentValue := norm
  else if (number = 11) then // 11
    SetBlowPosition(norm)
  else if (number = 26) then // reed table offset
    FReedTable.Offset := 0.4 + (norm * 0.6);
end;

end.
