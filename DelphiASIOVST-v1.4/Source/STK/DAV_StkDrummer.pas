unit DAV_StkDrummer;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK drum sample player class.

  This class implements a drum sampling synthesizer using WvIn objects and
  one-pole filters.  The drum rawwave files are sampled at 22050 Hz, but will
  be appropriately interpolated for other sample rates. You can specify the
  maximum polyphony (maximum number of simultaneous voices)
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkInstrument, DAV_StkWaveplayer, DAV_StkOnePole;

const
  CDrumWaveCount = 11;
  CDrumPolyphony = 8;

type
  TStkDrummer = class(TStkInstrument)
  protected
    FWaves     : array[0..CDrumPolyphony - 1] of TStkWavePlayer;
    FFilters   : array[0..CDrumPolyphony - 1] of TStkOnePole;
    FSounding  : array[0..CDrumPolyphony - 1] of Integer;
    FNSounding : Integer;
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Start a note with the given drum type and amplitude.
  {*!
    Use general MIDI drum instrument numbers, converted to
    frequency values as if MIDI note numbers, to select a
    particular instrument.
  *}
    procedure NoteOn(const Instrument, Amplitude: Single); override;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;
  end;

implementation

uses
  SysUtils;

// Not really General MIDI yet.  Coming soon.
const
  genMIDIMap: array[0..127] of Byte =
    (0, 0, 0, 0, 0, 0, 0, 0,    // 0-7
     0, 0, 0, 0, 0, 0, 0, 0,    // 8-15
     0, 0, 0, 0, 0, 0, 0, 0,    // 16-23
     0, 0, 0, 0, 0, 0, 0, 0,    // 24-31
     0, 0, 0, 0, 1, 0, 2, 0,    // 32-39
     2, 3, 6, 3, 6, 4, 7, 4,    // 40-47
     5, 8, 5, 0, 0, 0, 10, 0,   // 48-55
     9, 0, 0, 0, 0, 0, 0, 0,    // 56-63
     0, 0, 0, 0, 0, 0, 0, 0,    // 64-71
     0, 0, 0, 0, 0, 0, 0, 0,    // 72-79
     0, 0, 0, 0, 0, 0, 0, 0,    // 80-87
     0, 0, 0, 0, 0, 0, 0, 0,    // 88-95
     0, 0, 0, 0, 0, 0, 0, 0,    // 96-103
     0, 0, 0, 0, 0, 0, 0, 0,    // 104-111
     0, 0, 0, 0, 0, 0, 0, 0,    // 112-119
     0, 0, 0, 0, 0, 0, 0, 0     // 120-127
    );

const
  waveNames: array[0..CDrumWaveCount - 1] of TFileName =
    (
    'dope.wav',
    'bassdrum.wav',
    'snardrum.wav',
    'tomlowdr.wav',
    'tommiddr.wav',
    'tomhidrm.wav',
    'hihatcym.wav',
    'ridecymb.wav',
    'crashcym.wav',
    'cowbell1.wav',
    'tambourn.wav'
    );

constructor TStkDrummer.Create;
var
  i: Integer;
begin
 inherited Create(SampleRate);
 for i := 0 to CDrumPolyphony - 1 do
  begin
   FFilters[i] := TStkOnePole.Create(SampleRate);
   FSounding[i] := -1;
  end;
 // This counts the number of sounding voices.
 FNSounding := 0;
end;

destructor TStkDrummer.Destroy;
var
  i: Integer;
begin
  for i := 0 to FNSounding - 2 do FreeAndNil(FWaves[i]);
  for i := 0 to CDrumPolyphony - 1 do FreeAndNil(FFilters[i]);
  inherited Destroy;
end;

function TStkDrummer.GetFrequency: Single;
begin
 result := 0;
end;

procedure TStkDrummer.NoteOn(const Instrument, Amplitude: Single);
var
  gain: Single;
  i, waveindex, notenum: Integer;
  tempWv: TStkWavePlayer;
  tempFilt: TStkOnePole;
begin
  gain := amplitude;
  if (amplitude > 1.0) then
    gain := 1.0
  else if (amplitude < 0.0) then
    gain := 0;

  // Yes, this is tres kludgey.
  // int noteNum:=(int) ((12 * log(instrument / 220.0) / log(2.0)) + 57.01);
  notenum := round(instrument);

  // Check first to see if there's already one like this sounding.
  waveIndex := -1;
  for i := 0 to CDrumPolyphony - 1 do
    if (FSounding[i] = noteNum) then
      waveIndex := i;

  if (waveIndex >= 0) then
   begin
    // Reset this sound.
    FWaves[waveIndex].Reset;
    FFilters[waveIndex].SetPole(0.999 - (gain * 0.6));
    FFilters[waveIndex].Gain := gain;
   end else
   begin
    if (FNSounding = CDrumPolyphony) then
     begin
      // If we're already at maximum polyphony, then preempt the oldest voice.
      FreeAndNil(FWaves[0]);
      FFilters[0].Clear;
      tempWv := FWaves[0];
      tempFilt := FFilters[0];
      // Re-order the list.
      for i := 0 to CDrumPolyphony - 2 do
       begin
        FWaves[i] := FWaves[i + 1];
        FFilters[i] := FFilters[i + 1];
       end;
      FWaves[CDrumPolyphony - 1] := tempWv;
      FFilters[CDrumPolyphony - 1] := tempFilt;
     end
    else
      FNSounding := FNSounding + 1;

    FSounding[FNSounding - 1] := noteNum;

    FWaves[FNSounding - 1] := TStkWavePlayer.Create(
      SampleRate, wavenames[genmidimap[notenum]]);
    FWaves[FNSounding - 1].OneShot := True;
    FWaves[FNSounding - 1].Reset;
//    inputbox(FloatToStr(FWaves[FNSounding-1].length),'','');
{    if (SampleRate <> 22050.0) then
      FWaves[FNSounding-1].Rate := 22050.0 * FSampleRateInv;}
    FWaves[FNSounding - 1].Frequency := 1 / FWaves[FNSounding - 1].Length;
    FFilters[FNSounding - 1].SetPole(0.999 - (gain * 0.6));
    FFilters[FNSounding - 1].Gain := gain;
   end;
end;

procedure TStkDrummer.SetFrequency(const Value: Single);
begin
 inherited;
 // nothing here yet
end;

procedure TStkDrummer.NoteOff(const Amplitude: Single);
var
  i: Integer;
begin
  // Set all sounding wave filter gains low.
  i := 0;
  while (i < FNSounding) do
   begin
    FFilters[i].Gain:= Amplitude * 0.01;
    i := i + 1;
   end;
end;

function TStkDrummer.Tick: Single;
var
  output: Single;
  tempfilt: TStkOnePole;
  i, j: Integer;
begin
  output := 0.0;
  i := 0;
  while (i < FNSounding) do
   begin
    if (FWaves[i].isFinished) then
     begin
      FreeAndNil(FWaves[i]);
      tempFilt := FFilters[i];
      // Re-order the list.
      for j := i to FNSounding - 2 do
       begin
        FSounding[j] := FSounding[j + 1];
        FWaves[j] := FWaves[j + 1];
        FFilters[j] := FFilters[j + 1];
       end;
      FFilters[FNSounding - 2] := tempFilt;
      FFilters[FNSounding - 2].Clear;
      FSounding[FNSounding - 2] := -1;
      FNSounding := FNSounding - 1;
      i := i - 1;
     end
    else
      output := output + FFilters[i].Tick(FWaves[i].Tick);
    i := i + 1;
   end;
  Result := output;
end;

end.
