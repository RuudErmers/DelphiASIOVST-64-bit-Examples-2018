unit DAV_StkFormantSweep;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK sweepable formant filter class.

   This public BiQuad filter subclass implements a formant (resonance) which
   can be "swept" over time from one frequency setting to another.
   It provides methods for controlling the sweep rate and target frequency.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkBiquad;

type
  TStkFormantSweep = class(TStkBiQuad)
  private
    // Set the sweep rate (between 0.0 - 1.0).
  {
    The formant parameters are varied in increments of the
    sweep rate between their current and target values.
    A sweep rate of 1.0 will produce an immediate change in
    resonance parameters from their current values to the
    target values.  A sweep rate of 0.0 will produce no
    change in resonance parameters.
  }
    procedure SetSweepRate(const Value: Single);

    // Set the sweep rate in terms of a time value in seconds.
  {
    This method adjusts the sweep rate based on a
    given time for the formant parameters to reach
    their target values.
 }
    procedure SetSweepTime(const Value: Single);
    function GetSweepTime: Single;
  protected
    FDirty             : Boolean;
    FFrequency         : Single;
    FRadius            : Single;
    FStartFrequency    : Single;
    FStartRadius       : Single;
    FStartGain         : Single;
    FTargetFrequency   : Single;
    FTargetRadius      : Single;
    FTargetGain        : Single;
    FDeltaFrequency    : Single;
    FDeltaRadius       : Single;
    FDeltaGain         : Single;
    FSweepState        : Single;
    FSweepRate         : Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Sets the filter coefficients for a resonance at \e frequency (in Hz).
  {
    This method determines the filter coefficients corresponding to
    two complex-conjugate poles with the given \e frequency (in Hz)
    and \e radius from the z-plane origin.  The filter zeros are
    placed at z := 1, z := -1, and the coefficients are then normalized to
    produce a constant unity gain (independent of the filter \e gain
    parameter).  The resulting filter frequency response has a
    resonance at the given \e frequency.  The closer the poles are to
    the unit-circle (\e radius close to one), the narrower the
    resulting resonance width.
  }
    procedure SetResonance(const Frequency, Radius: Single);

    // Set both the current and target resonance parameters.
    procedure SetStates(const Frequency, Radius: Single; const Gain: Single = 1.0);

    // Set target resonance parameters.
    procedure SetTargets(const Frequency, Radius: Single; const Gain: Single = 1.0);

    // Input one sample to the filter and return one output.
    function Tick(const Sample: Single): Single; overload; override;

    // Input \e VectorSize samples to the filter and return an equal number of outputs in \e vector.
    function Tick(vector: PSingle; VectorSize: Integer): PSingle; overload;

    property SweepRate: Single read FSweepRate write SetSweepRate;
    property SweepTime: Single read GetSweepTime write SetSweepTime;
  end;

implementation

constructor TStkFormantSweep.Create(const SampleRate: Single); 
begin
  inherited Create(SampleRate);
  FFrequency       := 0;
  FRadius          := 0;
  FTargetGain      := 1;
  FTargetFrequency := 0;
  FTargetRadius    := 0;
  FDeltaGain       := 0;
  FDeltaFrequency  := 0;
  FDeltaRadius     := 0;
  FSweepState      := 0;
  FSweepRate       := 0.002;
  FDirty           := False;
  Clear;
end;

destructor TStkFormantSweep.Destroy;
begin
  inherited Destroy;
end;

procedure TStkFormantSweep.SetResonance(const Frequency, Radius: Single);
begin
  FDirty := False;
  FRadius := Radius;
  FFrequency := Frequency;
  inherited setResonance(FFrequency, FRadius, True);
end;

procedure TStkFormantSweep.SetStates(const Frequency, Radius: Single; const Gain: Single = 1.0);
begin
  FDirty := False;

  if (FFrequency <> Frequency) or (FRadius <> Radius)
   then inherited SetResonance(Frequency, Radius, True);

  FFrequency := Frequency;
  FRadius := Radius;
  FGain := Gain;
  FTargetFrequency := Frequency;
  FTargetRadius := Radius;
  FTargetGain := Gain;
end;

procedure TStkFormantSweep.SetTargets(const Frequency, Radius: Single; const Gain: Single = 1.0);
begin
  FDirty := True;
  FStartFrequency := FFrequency;
  FStartRadius := FRadius;
  FStartGain := FGain;
  FTargetFrequency := Frequency;
  FTargetRadius := Radius;
  FTargetGain := Gain;
  FDeltaFrequency := Frequency - FFrequency;
  FDeltaRadius := Radius - FRadius;
  FDeltaGain := Gain - FGain;
  FSweepState := 0;
end;

procedure TStkFormantSweep.SetSweepRate;
begin
 FSweepRate := Limit(Value, 0, 1);
end;

procedure TStkFormantSweep.SetSweepTime(const Value: Single);
begin
  FSweepRate := Limit(1.0 / (Value * SampleRate), 0, 1);
end;

function TStkFormantSweep.GetSweepTime: Single;
begin
  result := 1.0 / (FSweepRate * SampleRate);
end;

function TStkFormantSweep.Tick(const Sample: Single): Single;
begin
  if (FDirty) then
   begin
    FSweepState := FSweepState + FSweepRate;
    if (FSweepState >= 1.0) then
     begin
      FSweepState := 1.0;
      FDirty := False;
      FRadius := FTargetRadius;
      FFrequency := FTargetFrequency;
      gain := FTargetGain;
     end
    else
     begin
      FRadius := FStartRadius + (FDeltaRadius * FSweepState);
      FFrequency := FStartFrequency + (FDeltaFrequency * FSweepState);
      gain := FStartGain + (FDeltaGain * FSweepState);
     end;
    inherited setResonance(FFrequency, FRadius, True);
   end;
  Result := inherited Tick(sample);
end;

function TStkFormantSweep.Tick(vector: PSingle; VectorSize: Integer): PSingle;
var
  i: integer;
  p: PSingle;
begin
  p := vector;
  for i := 0 to VectorSize - 1 do
   begin
    p^ := Tick(p^);
    Inc(p);
   end;
  Result := vector;
end;

end.
