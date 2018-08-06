unit DAV_StkBowed;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK TStkBowed string instrument class.

  This class implements a TStkBowed string model, a la Smith (1986), after
  McIntyre, Schumacher, Woodhouse (1983).

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.

  Control Change Numbers:
    - Bow Pressure = 2
    - Bow Position = 4
    - FVibrato Frequency = 11
    - FVibrato Gain = 1
    - Volume = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkDelayl, DAV_StkBowTable,
  DAV_StkOnepole, DAV_StkBiquad, DAV_StkLfo, DAV_StkAdsr;

type
  TStkBowed = class(TStkControlableInstrument)
  private
    // Set vibrato gain.
    procedure SetVibrato(const Value: Single);
  protected
    FNeckDelay    : TStkDelayl;
    FBridgeDelay  : TStkDelayl;
    FBowTable     : TStkBowTable;
    FStringFilter : TStkOnePole;
    FBodyFilter   : TStkBiquad;
    FVibrato      : TStkLfo;
    FAdsr         : TStkAdsr;
    FMaxVelocity  : Single;
    FBaseDelay    : Single;
    FFrequency    : Single;
    FVibratoGain  : Single;
    FBetaRatio    : Single;
  public
    constructor Create(const SampleRate, lowestFrequency: Single); reintroduce; virtual;
    destructor Destroy; override;

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(const Value: Single); override;

    // Reset and clear all internal state.
    procedure Clear;

    // Apply breath pressure to instrument with given amplitude and rate of increase.
    procedure StartBowing(const Amplitude, Rate: Single);

    // Decrease breath pressure with given rate of decrease.
    procedure StopBowing(const Rate: Single);

    // Start a note with the given frequency and amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single); override;

    property Vibrato: Single read FVibratoGain write SetVibrato;
    property Frequency: Single read FFrequency write SetFrequency;
  end;

implementation

uses
  SysUtils, DAV_StkFilter;

constructor TStkBowed.Create;
var
  Length: longint;
begin
  inherited Create(SampleRate);
  Length := round(SampleRate / lowestFrequency + 1);
  FNeckDelay := TStkDelayl.Create(SampleRate, 100.0, Length);
  Length := Length shr 1;
  FBridgeDelay := TStkDelayl.Create(SampleRate, 29.0, Length);
  FBowTable := TStkBowTable.Create(SampleRate);
  FBowTable.Slope := 3.0;
  FVibrato := TStkLfo.Create(SampleRate);
  FVibrato.Frequency := 6.12723;
  FVibratoGain := 0.0;

  FStringFilter := TStkOnePole.Create(SampleRate);
  FStringFilter.setPole((0.6 - (0.1 * 22050.0 * FSampleRateInv)));
  FStringFilter.Gain := 0.95;

  FBodyFilter := TStkBiquad.Create(SampleRate);
  FBodyFilter.setResonance(500.0, 0.85, True);
  FBodyFilter.Gain := 0.2;

  FAdsr := TStkAdsr.Create(SampleRate);
  FAdsr.setAllTimes(0.02, 0.005, 0.9, 0.01);

  FBetaRatio := 0.127236;

 // Necessary to initialize internal variables.
  setFrequency(220.0);
end;

destructor TStkBowed.Destroy;
begin
  FreeAndNil(FNeckDelay);
  FreeAndNil(FBridgeDelay);
  FreeAndNil(FBowTable);
  FreeAndNil(FStringFilter);
  FreeAndNil(FBodyFilter);
  FreeAndNil(FVibrato);
  FreeAndNil(FAdsr);
  inherited Destroy;
end;

procedure TStkBowed.Clear;
begin
  FNeckDelay.Clear;
  FBridgeDelay.Clear;
end;

procedure TStkBowed.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   if (Value <= 0.0) then FFrequency := 220.0;

   // Delay := length - approximate filter delay.
   FBaseDelay := SampleRate / FFrequency - 4.0;
   if (FBaseDelay <= 0.0) then FBaseDelay := 0.3;
   FBridgeDelay.Delay := FBaseDelay * FBetaRatio;
                   // bow to bridge length
   FNeckDelay.Delay := FBaseDelay * (1.0 - FBetaRatio);
   // bow to nut (finger) length
  end;
end;

procedure TStkBowed.StartBowing(const Amplitude, Rate: Single);
begin
  FAdsr.Rate := Rate;
  FAdsr.KeyOn;
  FMaxVelocity := 0.03 + (0.2 * Amplitude);
end;

procedure TStkBowed.stopBowing(const Rate: Single);
begin
  FAdsr.Rate := Rate;
  FAdsr.KeyOff;
end;

procedure TStkBowed.NoteOn(const Frequency, Amplitude: Single);
begin
 StartBowing(Amplitude, Amplitude * 0.001);
 SetFrequency(Frequency);
end;

procedure TStkBowed.NoteOff(const Amplitude: Single);
begin
 StopBowing((1.0 - Amplitude) * 0.005);
end;

procedure TStkBowed.SetVibrato(const Value: Single);
begin
 FVibratoGain := Value;
end;

function TStkBowed.Tick: Single;
var
  bowVelocity, bridgeRefl, nutRefl, newVel, velDiff, stringVel: Single;
begin
  bowVelocity := FMaxVelocity * FAdsr.Tick;
  bridgeRefl := -FStringFilter.Tick(FBridgeDelay.LastOutput);
  nutRefl := -FNeckDelay.LastOutput;
  stringVel := bridgeRefl + nutRefl;               // Sum is String Velocity
  velDiff := bowVelocity - stringVel;              // Differential Velocity
  newVel := velDiff * FBowTable.Tick(velDiff);   // Non-Linear Bow Function
  FNeckDelay.Tick(bridgeRefl + newVel);           // Do string propagations
  FBridgeDelay.Tick(nutRefl + newVel);

  if (FVibratoGain > 0.0) then
    FNeckDelay.Delay := (FBaseDelay * (1.0 - FBetaRatio)) +
      (FBaseDelay * FVibratoGain * FVibrato.Tick);
  FLastOutput := FBodyFilter.Tick(FBridgeDelay.LastOutput);
  Result := LastOutput;
end;

procedure TStkBowed.controlChange(const Number: Integer; const Value: Single);
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (number = CMIDIBowPressure) then // 2
    FBowTable.Slope := (5.0 - (4.0 * norm))
  else if (number = CMIDIBowPosition) then
   begin // 4
    FBetaRatio := 0.027236 + (0.2 * norm);
    FBridgeDelay.Delay := (FBaseDelay * FBetaRatio);
    FNeckDelay.Delay := (FBaseDelay * (1.0 - FBetaRatio));
   end
  else if (number = CMIDIModFrequency) then // 11
    FVibrato.Frequency := (norm * 12.0)
  else if (number = CMIDIModWheel) then // 1
    FVibratoGain := (norm * 0.4)
  else if (number = CMIDIAfterTouchCont) then // 128
    FAdsr.Target := norm;
end;

end.
