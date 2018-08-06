unit DAV_StkFMVoices;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK singing FM synthesis instrument.

   This class implements 3 carriers and a common modulator, also referred to as
   algorithm 6 of the TX81Z.

   Algorithm 6 is :
                       /- 1 -\
                    4-+-- 2 --+- Out
                       \- 3 -/

   Control Change Numbers:
      - Vowel = 2
      - Spectral FTilt = 4
      - LFO Speed = 11
      - LFO Depth = 1
      - Adsr 2 & 4 Target = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkFm, DAV_StkWavePlayer, DAV_StkPhoneMes;

type
  TStkFMVoices = class(TStkFM)
  protected
    FCurrentVowel : Integer;
    FPhonems      : TStkPhonemes;
    FMods, FTilt  : array[0..2] of Single;
    procedure SetFrequency(const Value: Single); override;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;
    procedure NoteOn(const Frequency, Amplitude: Single); override;
    function Tick: Single; override;
    procedure ControlChange(const Number: Integer; const Value: Single); override;
  end;

implementation

uses
  SysUtils;

constructor TStkFMVoices.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  FPhonems := TStkPhonemes.Create(SampleRate);
  FWaves[0] := TStkWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[1] := TStkWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[2] := TStkWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[3] := TStkWavePlayer.Create(SampleRate, 'fwavblnk.wav');
  FWaves[0].OneShot := False;
  FWaves[1].OneShot := False;
  FWaves[2].OneShot := False;
  FWaves[3].OneShot := False;

  Ratio[0] := 2.00;
  Ratio[1] := 4.00;
  Ratio[2] := 12.0;
  Ratio[3] := 1.00;

  FGains[3] := FFMGains[80];

  FAdsr[0].SetAllTimes(0.05, 0.05, FFMSusLevels[15], 0.05);
  FAdsr[1].SetAllTimes(0.05, 0.05, FFMSusLevels[15], 0.05);
  FAdsr[2].SetAllTimes(0.05, 0.05, FFMSusLevels[15], 0.05);
  FAdsr[3].SetAllTimes(0.01, 0.01, FFMSusLevels[15], 0.5);

  FTwoZero.Gain := 0.0;
  FModDepth := 0.005;
  FCurrentVowel := 0;
  FTilt[0] := 1.0;
  FTilt[1] := 0.5;
  FTilt[2] := 0.2;
  FMods[0] := 1.0;
  FMods[1] := 1.1;
  FMods[2] := 1.1;
  FBaseFrequency := 110.0;
  SetFrequency(110.0);
end;

destructor TStkFMVoices.Destroy;
begin
 FreeAndNil(FPhonems);
 inherited Destroy;
end;

procedure TStkFMVoices.SetFrequency(const Value: Single);
var
  temp, temp2: Single;
  i, tempi: Integer;
begin
  temp2 := 0.0;
  i := 0;

  if (FCurrentVowel < 32) then
   begin
    i := FCurrentVowel;
    temp2 := 0.9;
   end
  else if (FCurrentVowel < 64) then
   begin
    i := FCurrentVowel - 32;
    temp2 := 1.0;
   end
  else if (FCurrentVowel < 96) then
   begin
    i := FCurrentVowel - 64;
    temp2 := 1.1;
   end
  else if (FCurrentVowel <= 128) then
   begin
    i := FCurrentVowel - 96;
    temp2 := 1.2;
   end;

  FBaseFrequency := Value;
  temp := (temp2 * FPhonems.FormantFrequency(i, 0) / FBaseFrequency) + 0.5;
  tempi := round(temp);
  Ratio[0] := tempi;
  temp := (temp2 * FPhonems.FormantFrequency(i, 1) / FBaseFrequency) + 0.5;
  tempi := round(temp);
  Ratio[1] := tempi;
  temp := (temp2 * FPhonems.FormantFrequency(i, 2) / FBaseFrequency) + 0.5;
  tempi := round(temp);
  Ratio[2] := tempi;
  FGains[0] := 1.0;
  FGains[1] := 1.0;
  FGains[2] := 1.0;
end;

procedure TStkFMVoices.NoteOn(const Frequency, Amplitude: Single);
begin
  SetFrequency(Frequency);
  FTilt[0] := Amplitude;
  FTilt[1] := sqr(Amplitude);
  FTilt[2] := FTilt[1] * Amplitude;
  keyOn;
end;

function TStkFMVoices.Tick: Single;
var
  temp, temp2: Single;
begin
  temp := FGains[3] * FAdsr[3].Tick * FWaves[3].Tick;
  temp2 := FVibrato.Tick * FModDepth * 0.1;

  FWaves[0].Frequency := FBaseFrequency * (1.0 + temp2) * FRatios[0];
  FWaves[1].Frequency := FBaseFrequency * (1.0 + temp2) * FRatios[1];
  FWaves[2].Frequency := FBaseFrequency * (1.0 + temp2) * FRatios[2];
  FWaves[3].Frequency := FBaseFrequency * (1.0 + temp2) * FRatios[3];

  FWaves[0].addPhaseOffset(temp * FMods[0]);
  FWaves[1].addPhaseOffset(temp * FMods[1]);
  FWaves[2].addPhaseOffset(temp * FMods[2]);
  FWaves[3].addPhaseOffset(FTwoZero.LastOutput);
  FTwoZero.Tick(temp);
  temp := FGains[0] * FTilt[0] * FAdsr[0].Tick * FWaves[0].Tick;
  temp := temp + FGains[1] * FTilt[1] * FAdsr[1].Tick * FWaves[1].Tick;
  temp := temp + FGains[2] * FTilt[2] * FAdsr[2].Tick * FWaves[2].Tick;

  Result := temp * 0.33;
end;

procedure TStkFMVoices.ControlChange(const Number: Integer; const Value: Single);
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (Number = CMidiBreath) then // 2
    FGains[3] := FFmGains[round(norm * 99.9)]
  else if (Number = CMidiFootControl) then
   begin // 4
    FCurrentVowel := round(norm * 128.0);
    SetFrequency(FBaseFrequency);
   end
  else if (Number = CMidiModFrequency) then // 11
    ModulationSpeed := norm * 12.0
  else if (Number = CMidiModWheel) then // 1
    ModulationDepth := norm
  else if (Number = CMidiAfterTouchCont) then
   begin // 128
    FTilt[0] := norm;
    FTilt[1] := sqr(norm);
    FTilt[2] := FTilt[1] * norm;
   end;
end;

end.
