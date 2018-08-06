unit UWaveformOscillator;

interface

uses Generics.Collections;

const pitchToFrequency:array[0..127] of single = (
	   8.176,     8.662,     9.177,     9.723,    10.301,    10.913,    11.562,    12.250,    12.978,    13.750,    14.568,    15.434,
	  16.352,    17.324,    18.354,    19.445,    20.602,    21.827,    23.125,    24.500,    25.957,    27.500,    29.135,    30.868,
	  32.703,    34.648,    36.708,    38.891,    41.203,    43.654,    46.249,    48.999,    51.913,    55.000,    58.270,    61.735,
	  65.406,    69.296,    73.416,    77.782,    82.407,    87.307,    92.499,    97.999,   103.826,   110.000,   116.541,   123.471,
	 130.813,   138.591,   146.832,   155.563,   164.814,   174.614,   184.997,   195.998,   207.652,   220.000,   233.082,   246.942,
	 261.626,   277.183,   293.665,   311.127,   329.628,   349.228,   369.994,   391.995,   415.305,   440.000,   466.164,   493.883,
	 523.251,   554.365,   587.330,   622.254,   659.255,   698.456,   739.989,   783.991,   830.609,   880.000,   932.328,   987.767,
	1046.502,  1108.731,  1174.659,  1244.508,  1318.510,  1396.913,  1479.978,  1567.982,  1661.219,  1760.000,  1864.655,  1975.533,
	2093.005,  2217.461,  2349.318,  2489.016,  2637.020,  2793.826,  2959.955,  3135.963,  3322.438,  3520.000,  3729.310,  3951.066,
	4186.009,  4434.922,  4698.636,  4978.032,  5274.041,  5587.652,  5919.911,  6271.927,  6644.875,  7040.000,  7458.620,  7902.133,
	8372.018,  8869.844,  9397.273,  9956.063, 10548.082, 11175.303, 11839.822, 12543.854 );


type EWaveSound = (wvNone,wvMeloFlute,wvMeloStrings,wvMeloChoir,wvSynthPolyTrombone,wvSynthPolyTrumpet,wvSynthPolyPiano,wvSynthPolyViola,
                   wvSynthMonoTuba,wvSynthMonoTrombone,wvSynthMonoTrumpet,wvSynthMonoBaritoneSax,wvSynthMonoAltoSax,wvSynthMonoBassFlute,wvSynthMonoFlute,wvSynthMonoPiccolo,    wvSynthMonoViolin);
const SWaveSound: array[EWaveSound] of string  = ('wvNone','wvMeloFlute','wvMeloStrings','wvMeloChoir','wvSynthPolyTrombone','wvSynthPolyTrumpet','wvSynthPolyPiano','wvSynthPolyViola',
                   'wvSynthMonoTuba','wvSynthMonoTrombone','wvSynthMonoTrumpet','wvSynthMonoBaritoneSax','wvSynthMonoAltoSax','wvSynthMonoBassFlute','wvSynthMonoFlute','wvSynthMonoPiccolo','wvSynthMonoViolin');

type TWaveFormInfo = record
         Position: single;
         Region:integer;
         procedure Reset;
      end;
type IWaveFormSoundData = interface
  function SampleAt(VAR info:TWaveFormInfo;frequency:single):single;
end;

type TWaveformOscillator = class
private
  FSampleRate : Single;
  FSampleReci : Single;
  FFrequency:single;
  FInfo:TWaveFormInfo;
  FWaveSound:EWaveSound;
  FWaveData: IWaveFormSoundData;
  procedure SetFrequency(const Value: Single);
  procedure SetSampleRate(const Value: Single);
  procedure SetWaveSound(wavesound:EWaveSound);
public
  property  WaveSound: EWaveSound read FWaveSound write SetWaveSound;
  procedure Reset;
  function Process:single;
  constructor Create(const SampleRate: Single);
  property Frequency: Single read FFrequency write SetFrequency;
  property SampleRate: Single read FSampleRate write SetSampleRate;
end;

type TSoundsFactory = class
   function LoadWaveData(wavesound:EWaveSound):IWaveFormSoundData;virtual;abstract;
end;

type TWaveSoundFactory = class
   FSounds:TList<TSoundsFactory>;
   function LoadWaveData(wavesound:EWaveSound):IWaveFormSoundData;
   procedure AddSoundFactory(factory:TSoundsFactory);
   constructor Create;
end;

function IWaveSoundFactory:TWaveSoundFactory;


implementation

{ TWaveFormOscillator }

VAR __WaveSoundFactory:TWaveSoundFactory;
function IWaveSoundFactory:TWaveSoundFactory;
begin
  if __WaveSoundFactory=NIL then __WaveSoundFactory:=TWaveSoundFactory.Create;
  result:=__WaveSoundFactory;
end;

procedure TWaveSoundFactory.AddSoundFactory(factory: TSoundsFactory);
begin
  FSounds.Add(factory);
end;

constructor TWaveSoundFactory.Create;
begin
  FSounds:=TList<TSoundsFactory>.Create;
end;

function TWaveSoundFactory.LoadWaveData(wavesound: EWaveSound): IWaveFormSoundData;
VAR i:integer;
begin
  for i:=0 to FSounds.Count-1 do
  begin
    result:=FSounds[i].LoadWaveData(waveSound);
    if result<>NIL then exit;
  end;
end;

constructor TWaveformOscillator.Create(const SampleRate: Single);
begin
  FFrequency  := 1000;
  Self.SampleRate := SampleRate;
end;

function TWaveformOscillator.Process: single;
begin
  if FWaveData=NIL then result:=0
  else result:=FWaveData.SampleAt(FInfo,FFrequency)
end;

procedure TWaveformOscillator.Reset;
begin
  FInfo.Reset;
end;

procedure TWaveformOscillator.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
   FFrequency := Value;
end;

procedure TWaveformOscillator.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
//   SampleRateChanged;
  end;
end;

procedure TWaveformOscillator.SetWaveSound(wavesound: EWaveSound);
begin
  if FWaveSound=wavesound then exit;
  FWaveSound:=wavesound;
  FWaveData:=IWaveSoundFactory.LoadWaveData(wavesound);
end;

procedure TWaveFormInfo.Reset;
begin
  Position:=0;
  Region:=-1;
end;


end.


