unit DAV_StkSingWave;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK "singing" looped soundfile class.

  This class contains all that is needed to make a pitched musical sound, like
  a simple voice or violin.  In general, it will not be used alone because of
  munchkinification effects from pitch shifting.  It will be used as an
  excitation source for other instruments.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkWavePlayer, DAV_StkModulate,
  DAV_StkEnvelope;

type
  TStkSingWave = class(TStk)
  private
    // Set the vibrato frequency in Hz.
    procedure SetVibratoRate(const Value: Single);

    // Set the vibrato gain.
    procedure SetVibratoGain(const Value: Single);

    // Set the random-ness amount.
    procedure SetRandomGain(const Value: Single);

    // Set the sweep rate.
    procedure SetSweepRate(const Value: Single);

    // Set the gain rate.
    procedure SetGainRate(const Value: Single);

    // Set the gain target value.
    procedure SetGainTarget(const Value: Single);
    function GetGainRate: Single;
    function GetGainTarget: Single;
    function GetRandomGain: Single;
    function GetVibratoGain: Single;
    function GetVibratoRate: Single;

  protected
    FWave          : TStkWavePlayer;
    FModulator     : TStkModulate;
    FEnvelope      : TStkEnvelope;
    FPitchEnvelope : TStkEnvelope;
    FRate          : Single;
    FSweepRate     : Single;
    FLastOutput    : Single;

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(Frequency: Single);

  public
    // Class constructor taking filename argument.
  {
    An StkError will be thrown if the file is not found, its format is
    unknown, or a read error occurs.
  }
    constructor Create(const SampleRate: Single; const FileName: string); reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset file to beginning.
    procedure Reset;

    // Start a note.
    procedure NoteOn;

    // Stop a note.
    procedure NoteOff;

    // Return the last output value.
    function LastOut: Single;

    // Compute one output sample.
    function Tick: Single;

    property VibratoRate: Single read GetVibratoRate write SetVibratoRate;
    property VibratoGain: Single read GetVibratoGain write SetVibratoGain;
    property RandomGain: Single read GetRandomGain write SetRandomGain;
    property SweepRate: Single read FSweepRate write SetSweepRate;
    property GainRate: Single read GetGainRate write SetGainRate;
    property GainTarget: Single read GetGainTarget write SetGainTarget;
  end;

implementation

uses
  SysUtils;

constructor TStkSingWave.Create;
begin
  inherited Create(SampleRate);
  FWave := TStkWavePlayer.Create(SampleRate, FileName);
  FWave.OneShot := False;
  FRate := 1.0;
  FSweepRate := 0.001;
  FModulator := TStkModulate.Create(SampleRate);
  FModulator.VibratoRate := 6.0;
  FModulator.VibratoGain := 0.04;
  FModulator.RandomGain := 0.005;
  FEnvelope := TStkEnvelope.Create(SampleRate);
  FPitchEnvelope := TStkEnvelope.Create(SampleRate);
  setFrequency(75.0);
  FPitchEnvelope.Rate := 1.0;
  Tick;
  Tick;
  FPitchEnvelope.Rate := FSweepRate * FRate;
end;

destructor TStkSingWave.Destroy;
begin
 FreeAndNil(FWave);
 FreeAndNil(FModulator);
 FreeAndNil(FEnvelope);
 FreeAndNil(FPitchEnvelope);
 inherited Destroy;
end;

procedure TStkSingWave.Reset;
begin
  FWave.Reset;
  FLastOutput := 0.0;
end;

procedure TStkSingWave.setFrequency;
var
  temp: Single;
begin
  temp := FRate;
  FRate := frequency * FSampleRateInv;
  temp := temp - FRate;
  if (temp < 0) then
    temp := -temp;
  FPitchEnvelope.Target := FRate;
  FPitchEnvelope.Rate := FSweepRate * temp;
end;

procedure TStkSingWave.SetVibratoRate(const Value: Single);
begin
  FModulator.VibratoRate := Value;
end;

function TStkSingWave.GetVibratoRate: Single;
begin
 result := FModulator.VibratoRate;
end;

procedure TStkSingWave.SetVibratoGain(const Value: Single);
begin
 FModulator.VibratoGain := Value;
end;

function TStkSingWave.GetVibratoGain: Single;
begin
 result := FModulator.VibratoGain
end;

procedure TStkSingWave.SetRandomGain(const Value: Single);
begin
  FModulator.RandomGain := Value;
end;

function TStkSingWave.GetRandomGain: Single;
begin
 result := FModulator.RandomGain;
end;

procedure TStkSingWave.setSweepRate(const Value: Single);
begin
  FSweepRate := Value;
end;

procedure TStkSingWave.SetGainRate(const Value: Single);
begin
  FEnvelope.Rate := Value;
end;

function TStkSingWave.GetGainRate: Single;
begin
 result := FEnvelope.Rate;
end;

procedure TStkSingWave.SetGainTarget(const Value: Single);
begin
  FEnvelope.Target := Value;
end;

function TStkSingWave.GetGainTarget: Single;
begin
 result := FEnvelope.Target;
end;

procedure TStkSingWave.NoteOn;
begin
  FEnvelope.KeyOn;
end;

procedure TStkSingWave.NoteOff;
begin
  FEnvelope.keyOff;
end;

function TStkSingWave.Tick: Single;
var
  newrate: Single;
begin
  // Set the FWave FRate.
  newRate := FPitchEnvelope.Tick;
  newRate := newrate + newRate * FModulator.Tick;
  FWave.Frequency := newRate;
  FLastOutput := FWave.Tick;
  FLastOutput := FLastOutput * FEnvelope.Tick;
  Result := FLastOutput;
end;

function TStkSingWave.LastOut: Single;
begin
  Result := FLastOutput;
end;

end.
