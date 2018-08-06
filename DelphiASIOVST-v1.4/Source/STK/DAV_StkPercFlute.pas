unit DAV_StkPercFlute;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK percussive flute FM synthesis instrument.

  This class implements algorithm 4 of the TX81Z.

  Algorithm 4 is :   4.3--\
                       2-- + -.1-.Out

  Control Change Numbers:
    - Total Modulator Index = 2
    - Modulator Crossfade = 4
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
  DAV_Common, DAV_StkCommon, DAV_StkFm, DAV_StkWavePlayer;

type
  TStkPercFlute = class(TStkFM)
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

uses
  SysUtils;

constructor TStkPercFlute.Create(const SampleRate: Single = 44100);
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

  Ratio[0] :=  1.50 * 1.000;
  Ratio[1] :=  3.00 * 0.995;
  Ratio[2] :=  2.99 * 1.005;
  Ratio[3] :=  6.00 * 0.997;
  FGains[0] := FFmGains[99];
  FGains[1] := FFmGains[71];
  FGains[2] := FFmGains[93];
  FGains[3] := FFmGains[85];
  FAdsr[0].SetAllTimes(0.05, 0.05, FFmSusLevels[14], 0.05);
  FAdsr[1].SetAllTimes(0.02, 0.50, FFmSusLevels[13], 0.5);
  FAdsr[2].SetAllTimes(0.02, 0.30, FFmSusLevels[11], 0.05);
  FAdsr[3].SetAllTimes(0.02, 0.05, FFmSusLevels[13], 0.01);
  FTwozero.Gain := 0.0;
  FModDepth := 0.005;
end;

destructor TStkPercFlute.Destroy;
begin
  inherited Destroy;
end;

procedure TStkPercFlute.NoteOn(const Frequency, Amplitude: Single);
begin
  FGains[0] := Amplitude * FFmGains[99] * 0.5;
  FGains[1] := Amplitude * FFmGains[71] * 0.5;
  FGains[2] := Amplitude * FFmGains[93] * 0.5;
  FGains[3] := Amplitude * FFmGains[85] * 0.5;
  SetFrequency(Frequency);
  keyOn;
end;

function TStkPercFlute.Tick: Single;
var
  temp: Single;
begin
  temp := FVibrato.Tick * FModDepth * 0.2;
  FWaves[0].Frequency := FBaseFrequency * (1.0 + temp) * FRatios[0];
  FWaves[1].Frequency := FBaseFrequency * (1.0 + temp) * FRatios[1];
  FWaves[2].Frequency := FBaseFrequency * (1.0 + temp) * FRatios[2];
  FWaves[3].Frequency := FBaseFrequency * (1.0 + temp) * FRatios[3];
  FWaves[3].addPhaseOffset(FTwozero.LastOutput);
  temp := FGains[3] * FAdsr[3].Tick * FWaves[3].Tick;
  FTwozero.Tick(temp);
  FWaves[2].addPhaseOffset(temp);
  temp := (1.0 - FControlB) * FGains[2] * FAdsr[2].Tick * FWaves[2].Tick;
  temp := temp + FControlB * FGains[1] * FAdsr[1].Tick * FWaves[1].Tick;
  temp := temp * FControlA;
  FWaves[0].addPhaseOffset(temp);
  temp := FGains[0] * FAdsr[0].Tick * FWaves[0].Tick;
  FLastOutput := temp * 0.5;
  Result := FLastOutput;
end;

end.
