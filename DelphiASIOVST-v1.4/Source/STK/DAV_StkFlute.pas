unit DAV_StkFlute;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK TStkFlute physical model class.

   This class implements a simple TStkFlute physical model, as discussed by
   Karjalainen, Smith, Waryznyk, etc.  The jet model uses a polynomial, a la
   Cook.

   This is a digital waveguide model, making its use possibly subject to
   patents held by Stanford University, Yamaha, and others.

   Control Change Numbers:
     - Jet Delay = 2
     - Noise Gain = 4
     - Vibrato Frequency = 11
     - Vibrato Gain = 1
     - Breath Pressure = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkLfo, DAV_StkInstrument, DAV_StkJetTable,
  DAV_StkDelayL, DAV_StkOnePole, DAV_StkPoleZero, DAV_StkNoise, DAV_StkAdsr;

type
  TStkFlute = class(TStkControlableInstrument)
  private
    // Set the reflection ACoefficient for the jet delay (-1.0 - 1.0).
    procedure SetJetReflection(const Coefficient: Single);

    // Set the reflection ACoefficient for the air column delay (-1.0 - 1.0).
    procedure SetEndReflection(const Coefficient: Single);

    // Set the FLength of the jet delay in terms of a ratio of jet delay to air column delay lengths.
    procedure SetJetDelay(const Value: Single);
  protected
    FJetDelay      : TStkDelayL;
    FBoreDelay     : TStkDelayL;
    FJetTable      : TStkJetTable;
    FFilter        : TStkOnePole;
    FDCBlock       : TStkPoleZero;
    FNoise         : TStkNoise;
    FAdsr          : TStkADSR;
    FVibrato       : TStkLFO;
    FLength        : Integer;
    FLastFrequency : Single;
    FMaxPressure   : Single;
    FJetReflection : Single;
    FEndReflection : Single;
    FNoiseGain     : Single;
    FVibratoGain   : Single;
    FOutputGain    : Single;
    FJetRatio      : Single;

    procedure JetRatioChanged; virtual;
    procedure FrequencyChanged; virtual;

    // Set instrument parameters for a particular AFrequency.
    procedure SetFrequency(const Value: Single); override;
  public
    // Class constructor, taking the lowest desired playing AFrequency.
    constructor Create(const SampleRate, LowestFrequency: Single); reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Apply breath velocity to instrument with given amplitude and rate of increase.
    procedure StartBlowing(const Amplitude, Rate: Single);

    // Decrease breath velocity with given rate of decrease.
    procedure StopBlowing(const Rate: Single);

    // Start a note with the given AFrequency and amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single); override;

    property JetDelay: Single read FJetRatio write SetJetDelay;
    property JetReflection: Single read FJetReflection write SetJetReflection;
    property EndReflection: Single read FEndReflection write SetEndReflection;
  end;

implementation

uses
  SysUtils;

constructor TStkFlute.Create;
begin
  inherited Create(SampleRate);
  FLength := round(SampleRate / LowestFrequency + 1);
  FBoreDelay := TStkDelayL.Create(SampleRate, 100.0, FLength);
  FLength := FLength shr 1;
  FJetDelay := TStkDelayL.Create(SampleRate, 49.0, FLength);
  FJetTable := TStkJetTable.Create(SampleRate);
  FFilter := TStkOnePole.Create(SampleRate);
  FDCBlock := TStkPoleZero.Create(SampleRate);
  FDCBlock.setBlockZero(0.99);
  FNoise := TStkNoise.Create(SampleRate);
  FAdsr := TStkADSR.Create(SampleRate);

  FVibrato := TStkLFO.Create(SampleRate);
  FVibrato.Frequency := 5.925;

  Clear;

  FFilter.SetPole(0.7 - (0.1 * 22050.0 * FSampleRateInv));
  FFilter.Gain := -1.0;
  FAdsr.SetAllTimes(0.005, 0.01, 0.8, 0.010);
  FEndReflection := 0.5;
  FJetReflection := 0.5;
  FNoiseGain := 0.15;             // Breath pressure random component.
  FVibratoGain := 0.05; // Breath periodic FVibrato component.
  FJetRatio := 0.32;

  FMaxPressure := 0.0;
  FLastFrequency := 220.0;
end;

destructor TStkFlute.Destroy;
begin
  FreeAndNil(FJetDelay);
  FreeAndNil(FBoreDelay);
  FreeAndNil(FJetTable);
  FreeAndNil(FFilter);
  FreeAndNil(FDCBlock);
  FreeAndNil(FNoise);
  FreeAndNil(FAdsr);
  FreeAndNil(FVibrato);
  inherited Destroy;
end;

procedure TStkFlute.Clear;
begin
  FJetDelay.Clear;
  FBoreDelay.Clear;
  FFilter.Clear;
  FDCBlock.Clear;
end;

procedure TStkFlute.SetFrequency(const Value: Single);
begin
 if FLastFrequency <> Value then
  begin
   if (Value <= 0.0)
    then FLastFrequency := 220.0
    else FLastFrequency := Value;

   FrequencyChanged;
  end;
end;

procedure TStkFlute.FrequencyChanged;
var
  Delay: Single;
begin
 // Delay := FLength - approximate FFilter delay.
 Delay := SampleRate / (0.66666 * FLastFrequency - 2.0);
 if (Delay <= 0.0) then Delay := 0.3
 else if (Delay > FLength) then Delay := FLength;

 FBoreDelay.Delay := Delay;
 FJetDelay.Delay := Delay * FJetRatio;
end;

procedure TStkFlute.StartBlowing(const Amplitude, Rate: Single);
begin
  FAdsr.AttackRate := Rate;
  FMaxPressure := Amplitude / 0.8;
  FAdsr.keyOn;
end;

procedure TStkFlute.StopBlowing(const Rate: Single);
begin
  FAdsr.ReleaseRate := Rate;
  FAdsr.KeyOff;
end;

procedure TStkFlute.NoteOn(const Frequency, Amplitude: Single);
begin
  SetFrequency(Frequency);
  StartBlowing(1.1 + (Amplitude * 0.20), Amplitude * 0.02);
  FOutputGain := Amplitude + 0.001;
end;

procedure TStkFlute.NoteOff(const Amplitude: Single);
begin
 StopBlowing(Amplitude * 0.02);
end;

procedure TStkFlute.SetJetReflection(const Coefficient: Single);
begin
  FJetReflection := Coefficient;
end;

procedure TStkFlute.SetEndReflection(const Coefficient: Single);
begin
  FEndReflection := Coefficient;
end;

procedure TStkFlute.SetJetDelay(const Value: Single);
begin
 if FJetRatio <> Value then
  begin
   FJetRatio := Value;
   JetRatioChanged;
  end;
end;

procedure TStkFlute.JetRatioChanged;
var
  Delay: Single;
begin
 // Delay := FLength - approximate FFilter delay.
 Delay := SampleRate / (0.66666 * FLastFrequency - 2.0);
 FJetDelay.Delay := Delay * FJetRatio; // Scaled by ratio.
end;

function TStkFlute.Tick: Single;
var
  temp, pressureDiff, breathPressure: Single;
begin
  // Calculate the breath pressure (envelope + FNoise + FVibrato)
  breathPressure := FMaxPressure * FAdsr.Tick;
  breathPressure := breathpressure + breathPressure * FNoiseGain * FNoise.Tick;
  breathPressure := breathpressure + breathPressure * FVibratoGain *
    FVibrato.Tick;

  temp := FFilter.Tick(FBoreDelay.LastOutput);
  temp := FDCBlock.Tick(temp); // Block DC on reflection.

  pressureDiff := breathPressure - (FJetReflection * temp);
  pressureDiff := FJetDelay.Tick(pressureDiff);
  pressureDiff := FJetTable.Tick(pressureDiff) + (FEndReflection * temp);
  FLastOutput := 0.3 * FBoreDelay.Tick(pressureDiff) * FOutputGain;

  Result := FLastOutput;
end;

procedure TStkFlute.ControlChange(const Number: Integer; const Value: Single);
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (number = CMidiJetDelay) then // 2
    SetJetDelay((0.08 + (0.48 * norm)))
  else if (number = CMidiNoiseLevel) then // 4
    FNoiseGain := (norm * 0.4)
  else if (number = CMidiModFrequency) then // 11
    FVibrato.Frequency := norm * 12
  else if (number = CMidiModWheel) then // 1
    FVibratoGain := (norm * 0.4)
  else if (number = CMidiAfterTouchCont) then // 128
    FAdsr.Target := norm;
end;

end.
