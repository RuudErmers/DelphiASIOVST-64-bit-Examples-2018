unit DAV_StkBeeThree;

{ STK Hammond-oid organ FM synthesis instrument.

  This class implements a simple 4 operator topology, also referred to as
  algorithm 8 of the TX81Z.

  Algorithm 8 is :
                    1 --.
                    2 -\|
                        +. Out
                    3 -/|
                    4 --

  Control Change Numbers:
    - Operator 4 (feedback) Gain = 2
    - Operator 3 Gain = 4
    - LFO Speed = 11
    - LFO Depth = 1
    - ADSR 2 & 4 Target = 128

  The basic Chowning/Stanford FM patent expired in 1995, but there exist
  follow-on patents, mostly assigned to Yamaha.
  If you are of the type who should worry about this (making money) worry away.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkFm, DAV_StkWaveplayer;

type
  TStkBeeThree = class(TStkFM)
  public
    constructor Create(const SampleRate: Single = 44100); override;
    destructor Destroy; override;

    // Start a note with the given Frequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;
  end;

implementation

{ TStkBeeThree }

constructor TStkBeeThree.Create(const SampleRate: Single = 44100);
begin
  inherited Create(SampleRate);

  FWaves[0] := TStkWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[1] := TStkWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[2] := TStkWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[3] := TStkWavePlayer.Create(SampleRate, 'fwavblnk.wav');
  FWaves[0].OneShot := False;
  FWaves[1].OneShot := False;
  FWaves[2].OneShot := False;
  FWaves[3].OneShot := False;

  Ratio[0] := 0.999;
  Ratio[1] := 1.997;
  Ratio[2] := 3.006;
  Ratio[3] := 6.009;

  FGains[0] := FFmGains[95];
  FGains[1] := FFmGains[95];
  FGains[2] := FFmGains[99];
  FGains[3] := FFmGains[95];

  FAdsr[0].setAllTimes(0.005, 0.003, 1.0, 0.01);
  FAdsr[1].setAllTimes(0.005, 0.003, 1.0, 0.01);
  FAdsr[2].setAllTimes(0.005, 0.003, 1.0, 0.01);
  FAdsr[3].setAllTimes(0.005, 0.001, 0.4, 0.03);

  FTwoZero.Gain := 0.1;
end;

destructor TStkBeeThree.Destroy;
begin
  inherited Destroy;
end;

procedure TStkBeeThree.noteOn(const Frequency, Amplitude: Single);
begin
  FGains[0] := Amplitude * FFmGains[95];
  FGains[1] := Amplitude * FFmGains[95];
  FGains[2] := Amplitude * FFmGains[99];
  FGains[3] := Amplitude * FFmGains[95];
  Self.Frequency := Frequency;
  keyOn;
end;

function TStkBeeThree.Tick: Single;
var
  temp: Single;
begin
  if (FModDepth > 0.0) then
   begin
    temp := 1.0 + (FModDepth * FVibrato.Tick * 0.1);
    FWaves[0].Frequency := FBaseFrequency * temp * FRatios[0];
    FWaves[1].Frequency := FBaseFrequency * temp * FRatios[1];
    FWaves[2].Frequency := FBaseFrequency * temp * FRatios[2];
    FWaves[3].Frequency := FBaseFrequency * temp * FRatios[3];
   end;

  FWaves[3].addPhaseOffset(FTwoZero.LastOutput);
  temp := FControlA * 4.0 * FGains[3] * FAdsr[3].Tick * FWaves[3].Tick;
  FTwoZero.Tick(temp);

  temp := temp + FControlB * 4.0 * FGains[2] * FAdsr[2].Tick * FWaves[2].Tick;
  temp := temp + FGains[1] * FAdsr[1].Tick * FWaves[1].Tick;
  temp := temp + FGains[0] * FAdsr[0].Tick * FWaves[0].Tick;
  FLastOutput := temp * 0.125;
  Result := lastOutput;
end;

end.
