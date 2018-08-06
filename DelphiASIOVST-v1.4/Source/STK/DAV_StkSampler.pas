unit DAV_StkSampler;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK sampling synthesis abstract base class.

  This instrument contains up to 5 attack waves, 5 looped waves, and an ADSR
  envelope.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkWavePlayer, DAV_StkInstrument, DAV_StkAdsr,
  DAV_StkOnePole;

type
  TStkSampler = class(TStkControlableInstrument)
  protected
    FADSR          : TStkAdsr;
    FAttacks       : array[0..4] of TStkWavePlayer;
    FLoops         : array[0..4] of TStkWavePlayer;
    FFilter        : TStkOnepole;
    FAttackGain    : Single;
    FLoopGain      : Single;
    FBaseFrequency : Single;
    FLoopratios    : Single;
    FAttackRatios  : array[0..4] of Single;
    FWhichOne      : Integer;

    // Set instrument parameters for a particular AFrequency.
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;

    procedure FrequencyChanged; virtual; abstract;
  public
    // Default constructor.
    constructor Create(const SampleRate: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear; virtual;

    // Initiate the envelopes with a key-on event and reset the attack waves.
    procedure KeyOn; virtual;

    // Signal a key-off event to the envelopes.
    procedure KeyOff; virtual;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;
  end;

implementation

uses
  SysUtils;

constructor TStkSampler.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  // We don't make the waves here yet, because
  // we don't know what they will be.
  FADSR := TStkAdsr.Create(SampleRate);
  FBaseFrequency := 440.0;
  FFilter := TStkOnepole.Create(SampleRate);
  FAttackGain := 0.25;
  FLoopGain := 0.25;
  FWhichOne := 0;
end;

destructor TStkSampler.Destroy;
begin
 FreeAndNil(FADSR);
 FreeAndNil(FFilter);
 inherited Destroy;
end;

function TStkSampler.GetFrequency: Single;
begin
 result := FBaseFrequency;
end;

procedure TStkSampler.KeyOn;
begin
  FADSR.KeyOn;
  FAttacks[0].reset;
end;

procedure TStkSampler.KeyOff;
begin
  FADSR.KeyOff;
end;

procedure TStkSampler.NoteOff(const Amplitude: Single);
begin
  KeyOff;
end;

procedure TStkSampler.SetFrequency(const Value: Single);
begin
 if FBaseFrequency <> Value then
  begin
   if (Value <= 0.0)
    then FBaseFrequency := 220.0
    else FBaseFrequency := Value;
   FrequencyChanged;
  end;
 inherited;
end;

function TStkSampler.Tick: Single;
begin
  FLastOutput := FAttackGain * FAttacks[FWhichOne].Tick;
  FLastOutput := FLastOutput + FLoopGain * FLoops[FWhichOne].Tick;
  FLastOutput := FFilter.Tick(FLastOutput) * FADSR.Tick;
  Result := FLastOutput;
end;

procedure TStkSampler.Clear;
begin
 // nothing in here yet
end;

end.
