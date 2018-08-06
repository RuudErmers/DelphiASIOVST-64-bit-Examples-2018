unit DAV_StkBlowHole;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK clarinet physical model with one register hole and one tonehole
  -------------------------------------------------------------------

  This class is based on the clarinet model, with the addition of a two-port
  register hole and a three-port dynamic tonehole implementation, as discussed
  by Scavone and Cook (1998).

  In this implementation, the distances between the reed/register hole and
  tonehole/bell are fixed.  As a result, both the tonehole and register hole
  will have variable influence on the playing frequency, which is dependent on
  the length of the air column.  In addition, the highest playing freqeuency is
  limited by these fixed lengths.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.

  Control Change Numbers:
    - Reed Stiffness = 2
    - Noise Gain = 4
    - Tonehole State = 11
    - Register State = 1
    - Breath Pressure = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkDelayl, DAV_StkReedTable,
  DAV_StkLfo, DAV_StkOneZero, DAV_StkPoleZero, DAV_StkEnvelope, DAV_StkNoise;

type
  TStkBlowHole = class(TStkControlableInstrument)
  protected
    FDelays        : array[0..2] of TStkDelayL;
    FReedTable     : TStkReedTable;
    FFilter        : TStkOneZero;
    FTonehole      : TStkPoleZero;
    FVent          : TStkPoleZero;
    FEnvelope      : TStkEnvelope;
    FNoise         : TStkNoise;
    FVibrato       : TStkLfo;
    FLength        : Integer;
    FScatter       : Single;
    FThCoeff       : Single;
    FRth           : Single;
    FRhCoeff       : Single;
    FRhGain        : Single;
    FOutputGain    : Single;
    FNoiseGain     : Single;
    FVibratoGain   : Single;
    FBaseFrequency : Single;

    //! Set instrument parameters for a particular frequency.
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;

    procedure FrequencyChanged; virtual;

    //! Set the tonehole state (0.0 := closed, 1.0 := fully open).
    procedure SetTonehole(const Value: Single);

    //! Set the register hole state (0.0 := closed, 1.0 := fully open).
    procedure SetVent(const Value: Single);

  public
    //! Class constructor.
    constructor Create(const SampleRate, LowestFrequency: Single); reintroduce; virtual;

    //! Class destructor.
    destructor Destroy; override;

    //! Reset and clear all internal state.
    procedure Clear;

    //! Apply breath pressure to instrument with given amplitude and rate of increase.
    procedure StartBlowing(const Amplitude, Rate: Single);

    //! Decrease breath pressure with given rate of decrease.
    procedure StopBlowing(const Rate: Single);

    //! Start a note with the given frequency and amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    //! Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    //! Compute one output sample.
    function Tick: Single; override;

    //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single); override;
  end;

implementation

uses
  SysUtils, Math, DAV_StkFilter;

constructor TStkBlowHole.Create;
var
  xi, zeta, psi, r_rh, r_b: double;
  te: Single;
begin
  inherited Create(SampleRate);
  FLength := round(SampleRate / LowestFrequency + 1);
  // FDelays[0] is the delay line between the reed and the register FVent.
  FDelays[0] := TStkDelayL.Create(SampleRate, 5.0 * SampleRate / 22050.0, 100);
  // FDelays[1] is the delay line between the register FVent and the FTonehole.
  FDelays[1] := TStkDelayL.Create(SampleRate, FLength shr 1, FLength);
  // FDelays[2] is the delay line between the FTonehole and the end of the bore.
  FDelays[2] := TStkDelayL.Create(SampleRate, 4.0 * SampleRate / 22050.0, 100);
  FReedTable := TStkReedTable.Create(SampleRate);
  FReedTable.Offset := 0.7;
  FReedTable.Slope  := -0.3;
  FFilter := TStkOneZero.Create(SampleRate);
  FEnvelope := TStkEnvelope.Create(SampleRate);
  FNoise := TStkNoise.Create(SampleRate);

  // Calculate the initial FTonehole three-port scattering coefficient
  r_b := 0.0075;    // main bore radius
  FRth := 0.003;          // FTonehole radius
  FScatter := -power(FRth, 2) / (power(FRth, 2) + 2 * power(r_b, 2));

  // Calculate FTonehole coefficients
  te := 1.4 * FRth;    // effective FLength of the open hole
  FThCoeff := (te * 2 * SampleRate - 347.23) / (te * 2 * SampleRate + 347.23);
  FTonehole := TStkPoleZero.Create(SampleRate);
  // Start with FTonehole open
  FTonehole.setA1(-FThCoeff);
  FTonehole.setB0(FThCoeff);
  FTonehole.setB1(-1.0);

  // Calculate register hole FFilter coefficients
  r_rh := 0.0015;    // register FVent radius
  te := 1.4 * r_rh;       // effective FLength of the open hole
  xi := 0.0;         // series resistance term
  zeta := 347.23 + 2 * PI * power(r_b, 2) * xi / 1.1769;
  psi := 2 * PI * power(r_b, 2) * te / (PI * power(r_rh, 2));
  FRhCoeff := (zeta - 2 * SampleRate * psi) / (zeta + 2 * SampleRate * psi);
  FRhGain := -347.23 / (zeta + 2 * SampleRate * psi);
  FVent := TStkPoleZero.Create(SampleRate);
  FVent.setA1(FRhCoeff);
  FVent.setB0(1.0);
  FVent.setB1(1.0);
  // Start with register FVent closed
  FVent.Gain := 0.0;
  FVibrato := TStkLfo.Create(SampleRate);
  FVibrato.Frequency := 5.735;
  FOutputGain := 1.0;
  FNoiseGain := 0.2;
  FVibratoGain := 0.01;
end;

destructor TStkBlowHole.Destroy;
begin
  FreeAndNil(FDelays[0]);
  FreeAndNil(FDelays[1]);
  FreeAndNil(FDelays[2]);
  FreeAndNil(FReedTable);
  FreeAndNil(FFilter);
  FreeAndNil(FTonehole);
  FreeAndNil(FVent);
  FreeAndNil(FEnvelope);
  FreeAndNil(FNoise);
  FreeAndNil(FVibrato);
  inherited Destroy;
end;

procedure TStkBlowHole.Clear;
begin
  FDelays[0].Clear;
  FDelays[1].Clear;
  FDelays[2].Clear;
  FFilter.Tick(0.0);
  FTonehole.Tick(0.0);
  FVent.Tick(0.0);
end;

procedure TStkBlowHole.SetFrequency(const Value: Single);
begin
 if FBaseFrequency <> Value then
  begin
   if (Value <= 0.0)
    then FBaseFrequency := 220.0
    else FBaseFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TStkBlowHole.FrequencyChanged;
var
  Delay: Single;
begin
 // Delay := FLength - approximate FFilter delay.
 Delay := (SampleRate / FBaseFrequency) * 0.5 - 3.5;
 Delay := Delay - (FDelays[0].Delay + FDelays[2].Delay);

 if (Delay <= 0.0) then Delay := 0.3
 else if (Delay > FLength) then Delay := FLength;
 FDelays[1].Delay := Delay;
end;

function TStkBlowHole.GetFrequency: Single;
begin
 result := FBaseFrequency;
end;

procedure TStkBlowHole.SetVent(const Value: Single);
var
  Gain: Single;
begin
  // This method allows setting of the register FVent "open-ness" at
  // any point between "Open" (newValue := 1) and "Closed"
  // (newValue := 0).

  if (Value <= 0.0) then Gain := 0.0
   else if (Value >= 1.0) then Gain := FRhGain
   else Gain := Value * FRhGain;
  FVent.Gain := Gain;
end;

procedure TStkBlowHole.SetTonehole(const Value: Single);
var
  NewCoeff: Single;
begin
 // This method allows setting of the FTonehole "open-ness" at
 // any point between "Open" (newValue := 1) and "Closed"
 // (newValue := 0).
 if (Value <= 0.0) then NewCoeff := 0.9995
  else if (Value >= 1.0) then NewCoeff := FThCoeff
  else NewCoeff := (Value * (FThCoeff - 0.9995)) + 0.9995;
 FTonehole.SetA1(-NewCoeff);
 FTonehole.SetB0(NewCoeff);
end;

procedure TStkBlowHole.StartBlowing(const Amplitude, Rate: Single);
begin
 FEnvelope.Rate := Rate;
 FEnvelope.Target := Amplitude;
end;

procedure TStkBlowHole.StopBlowing(const Rate: Single);
begin
  FEnvelope.Rate := Rate;
  FEnvelope.Target := 0.0;
end;

procedure TStkBlowHole.NoteOn(const Frequency, Amplitude: Single);
begin
  SetFrequency(frequency);
  StartBlowing(0.55 + (Amplitude * 0.30), Amplitude * 0.005);
  FOutputGain := Amplitude + 0.001;
end;

procedure TStkBlowHole.NoteOff(const Amplitude: Single);
begin
 StopBlowing(Amplitude * 0.01);
end;

function TStkBlowHole.Tick: Single;
var
  pth, pa, pb, pressureDiff, breathPressure, temp: Single;
begin
 // Calculate the breath pressure (FEnvelope + FNoise + FVibrato)
 breathPressure := FEnvelope.Tick;
 breathPressure := breathpressure + breathPressure * FNoiseGain * FNoise.Tick;
 breathPressure := breathpressure + breathPressure * FVibratoGain *
   FVibrato.Tick;

 // Calculate the differential pressure := reflected - mouthpiece pressures
 pressureDiff := FDelays[0].LastOutput - breathPressure;

 // Do two-port junction scattering for register FVent
 pa := breathPressure + pressureDiff * FReedTable.Tick(pressureDiff);
 pb := FDelays[1].LastOutput;
 FVent.Tick(pa + pb);

 FLastOutput := FOutputGain * FDelays[0].Tick(FVent.LastOutput + pb);

 // Do three-port junction scattering (under FTonehole)
 pa := pa + FVent.LastOutput;
 pb := FDelays[2].LastOutput;
 pth := FTonehole.LastOutput;
 temp := FScatter * (pa + pb - 2 * pth);

 FDelays[2].Tick(FFilter.Tick(pa + temp) * -0.95);
 FDelays[1].Tick(pb + temp);
 FTonehole.Tick(pa + pb - pth + temp);

 Result := lastOutput;
end;

procedure TStkBlowHole.ControlChange(const Number: Integer; const Value: Single);
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

 if (number = CMidiReedStiffness) then // 2
   FReedTable.Slope := -0.44 + (0.26 * norm)
 else if (number = CMidiNoiseLevel) then // 4
   FNoiseGain := (norm * 0.4)
 else if (number = CMidiModFrequency) then // 11
   setTonehole(norm)
 else if (number = CMidiModWheel) then // 1
   setVent(norm)
 else if (number = CMidiAfterTouchCont) then // 128
   FEnvelope.CurrentValue := norm;
end;

end.
