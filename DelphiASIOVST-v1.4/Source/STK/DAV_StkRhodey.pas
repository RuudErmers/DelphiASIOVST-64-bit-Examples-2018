unit DAV_StkRhodey;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK Fender Rhodes electric piano FM synthesis instrument.

  This class implements two simple FM Pairs summed together, also referred to
  as algorithm 5 of the TX81Z.

  Algorithm 5 is :  4.3--\
                          + -. Out
                    2.1--/

  Control Change Numbers:
    - Modulator Index One = 2
    - Crossfade of Outputs = 4
    - LFO Speed = 11
    - LFO Depth = 1
    - ADSR 2 & 4 Target = 128

  The basic Chowning/Stanford FM patent expired in 1995, but there exist
  follow-on patents, mostly assigned to Yamaha. If you are of the type who
  should worry about this (making money) worry away.

}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkFm, DAV_StkWavePlayer;

type
  TStkRhodey = class(TStkFM)
  protected
    procedure FrequencyChanged; override;
  public
    // Class constructor.
    constructor Create(const SampleRate: Single = 44100); override;

    // Class destructor.
    destructor Destroy; override;

    // Start a note with the given Frequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;
  end;

implementation

constructor TStkRhodey.Create(const SampleRate: Single = 44100);
begin
  inherited Create(SampleRate);
  FWaves[0] := TStkWavePlayer.Create(Samplerate, 'sinewave.wav');
  FWaves[1] := TStkWavePlayer.Create(Samplerate, 'sinewave.wav');
  FWaves[2] := TStkWavePlayer.Create(Samplerate, 'sinewave.wav');
  FWaves[3] := TStkWavePlayer.Create(Samplerate, 'fwavblnk.wav');
  FWaves[0].OneShot := False;
  FWaves[1].OneShot := False;
  FWaves[2].OneShot := False;
  FWaves[3].OneShot := False;

  Ratio[0] := 1.0;
  Ratio[1] := 0.5;
  Ratio[2] := 1.0;
  Ratio[3] := 15.0;

  FGains[0] := FFmGains[99];
  FGains[1] := FFmGains[90];
  FGains[2] := FFmGains[99];
  FGains[3] := FFmGains[67];

  FAdsr[0].setAllTimes(0.001, 1.50, 0.0, 0.04);
  FAdsr[1].setAllTimes(0.001, 1.50, 0.0, 0.04);
  FAdsr[2].setAllTimes(0.001, 1.00, 0.0, 0.04);
  FAdsr[3].setAllTimes(0.001, 0.25, 0.0, 0.04);

  FTwoZero.Gain := 1.0;
end;

destructor TStkRhodey.Destroy;
begin
 inherited Destroy;
end;

procedure TStkRhodey.FrequencyChanged;
var
  i: Integer;
begin
 for i := 0 to FNOperators - 1
  do FWaves[i].Frequency := 2 * FBaseFrequency * FRatios[i];
end;


procedure TStkRhodey.NoteOn(const Frequency, Amplitude: Single);
begin
 FGains[0] := Amplitude * FFmGains[99];
 FGains[1] := Amplitude * FFmGains[90];
 FGains[2] := Amplitude * FFmGains[99];
 FGains[3] := Amplitude * FFmGains[67];
 Self.Frequency := Frequency;
 KeyOn;
end;

function TStkRhodey.Tick: Single;
var
  temp, temp2: Single;
begin
  temp := FGains[1] * FAdsr[1].Tick * FWaves[1].Tick;
  temp := temp * 2 * FControlA;
  FWaves[0].addPhaseOffset(temp);
  FWaves[3].addPhaseOffset(FTwoZero.LastOutput);
  temp := FGains[3] * FAdsr[3].Tick * FWaves[3].Tick;
  FTwoZero.Tick(temp);
  FWaves[2].addPhaseOffset(temp);
  temp := (1.0 - FControlB) * FGains[0] * FAdsr[0].Tick * FWaves[0].Tick;
  temp := temp + FControlB * FGains[2] * FAdsr[2].Tick * FWaves[2].Tick;
  // Calculate Amplitude modulation and apply it to output.
  temp2 := FVibrato.Tick * FModDepth;
  temp := temp * (1.0 + temp2);
  FLastOutput := temp * 0.5;
  Result := lastOutput;
end;

end.
