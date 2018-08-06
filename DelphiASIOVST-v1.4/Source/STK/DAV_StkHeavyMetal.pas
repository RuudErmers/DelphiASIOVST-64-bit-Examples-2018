unit DAV_StkHeavyMetal;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK heavy metal Fm synthesis instrument.

   This class implements 3 cascade operators with feedback modulation, also
   referred to as algorithm 3 of the TX81Z.

   Algorithm 3 is :    4-\
                    3->2--+->1->Out

   Control Change Numbers:
     - Total Modulator Index = 2
     - Modulator Crossfade = 4
     - LFO Speed = 11
     - LFO Depth = 1
     - ADSR 2 & 4 Target = 128

   The basic Chowning/Stanford StkFm patent expired in 1995, but there exist
   follow-on patents, mostly assigned to Yamaha. If you are of the type who
   should worry about this (making money) worry away.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} DAV_StkCommon, DAV_StkFm, DAV_StkWavePlayer;

type
  TStkHeavyMetal = class(TStkFM)
  public
    constructor Create(const SampleRate: Single = 44100); override;
    destructor Destroy; override;

    procedure NoteOn(const Frequency, Amplitude: Single); override;
    function Tick: Single; override;
  end;

implementation

uses
  SysUtils;

constructor TStkHeavyMetal.Create(const SampleRate: Single = 44100);
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
  Ratio[0] := 1.0 * 1.000;
  Ratio[1] := 4.0 * 0.999;
  Ratio[2] := 3.0 * 1.001;
  Ratio[3] := 0.5 * 1.002;

  FGains[0] := FFmGains[92];
  FGains[1] := FFmGains[76];
  FGains[2] := FFmGains[91];
  FGains[3] := FFmGains[68];

  FAdsr[0].setAllTimes(0.001, 0.001, 1.0, 0.01);
  FAdsr[1].setAllTimes(0.001, 0.010, 1.0, 0.50);
  FAdsr[2].setAllTimes(0.010, 0.005, 1.0, 0.20);
  FAdsr[3].setAllTimes(0.030, 0.010, 0.2, 0.20);

  FTwoZero.Gain := 2.0;
  FVibrato.Frequency := 5.5;
  FModDepth := 0.0;
end;

destructor TStkHeavyMetal.Destroy;
begin
  inherited Destroy;
end;

procedure TStkHeavyMetal.NoteOn(const Frequency, Amplitude: Single);
begin
 FGains[0] := Amplitude * FFmGains[92];
 FGains[1] := Amplitude * FFmGains[76];
 FGains[2] := Amplitude * FFmGains[91];
 FGains[3] := Amplitude * FFmGains[68];
 SetFrequency(Frequency);
 KeyOn;
end;

function TStkHeavyMetal.Tick: Single;
var
  temp: Single;
begin
  temp := FVibrato.Tick * FModDepth * 0.2;
  FWaves[0].Frequency := FBaseFrequency * (1.0 + temp) * FRatios[0];
  FWaves[1].Frequency := FBaseFrequency * (1.0 + temp) * FRatios[1];
  FWaves[2].Frequency := FBaseFrequency * (1.0 + temp) * FRatios[2];
  FWaves[3].Frequency := FBaseFrequency * (1.0 + temp) * FRatios[3];

  temp := FGains[2] * FAdsr[2].Tick * FWaves[2].Tick;
  FWaves[1].addPhaseOffset(temp);

  FWaves[3].addPhaseOffset(FTwozero.LastOutput);
  temp := (1.0 - ControlB) * FGains[3] * FAdsr[3].Tick * FWaves[3].Tick;
  FTwoZero.Tick(temp);

  temp := temp + (ControlB * FGains[1] * FAdsr[1].Tick * FWaves[1].Tick);
  temp := temp * 2 * FControlA;

  FWaves[0].addPhaseOffset(temp);
  temp := FGains[0] * FAdsr[0].Tick * FWaves[0].Tick;

  FLastOutput := temp * 0.5;
  Result := FLastOutput;
end;

end.
