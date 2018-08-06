unit USoundDataSamplesMellotron;

interface

uses UWaveFormSoundData,UWaveformOscillator, SysUtils;

implementation

type TWaveSoundDataMellotron = class(TWaveSoundData)
   procedure LoadSamples(wave: EWaveSound);override;
end;

type TMellotronSounds = class(TSoundsFactory)
   wavesounddata: array [wvMeloFlute..wvMeloChoir] of TWaveSoundData;
   function LoadWaveData(wavesound:EWaveSound):IWaveFormSoundData;override;
   constructor create;
end;

{ TMellotronSounds }

procedure TWaveSoundDataMellotron.LoadSamples(wave: EWaveSound);
    procedure LoadMellotron(dir,after:string);
    const fkeys:array[0..11] of string = ('C','C#','D','D#','E','F','F#','G','G#','A','A#','B');
    VAR region:TWaveSoundRegion;
        i:integer;
        filename:string;
    begin
      for i:=43 to 75 do
      begin
        region:=TWaveSoundRegion.Create;
        filename:='C:\Midi\Data\Mellotron\'+dir+'\'+fkeys[i MOD 12]+inttostr(i DIV 12-1)+after+'.wav';
        region.SetSound(filename,pitchToFrequency[i]);
        FWaveSoundRegion.Add(region);
      end;
    end;
begin
  case wave of
    wvMeloFlute:   LoadMellotron('MK2 Flute','-3');
    wvMeloStrings: LoadMellotron('String Section','-4');
    wvMeloChoir:   LoadMellotron('Choir','');
  end;
end;

constructor TMellotronSounds.create;
VAR wavesound: EWaveSound;
begin
  for wavesound:=low(wavesounddata) to high(wavesounddata) do
    LoadWaveData(wavesound);
end;

function TMellotronSounds.LoadWaveData(wavesound: EWaveSound): IWaveFormSoundData;
begin
  if (wavesound>=low(wavesounddata)) and (wavesound<=high(wavesounddata)) then
  begin
    if wavesounddata[wavesound] = NIL then
      wavesounddata[wavesound]:=TWaveSoundDataMellotron.Create(wavesound);
    result:=wavesounddata[wavesound];
  end
  else
    result:=NIL;
end;

begin
  IWaveSoundFactory.AddSoundFactory(TMellotronSounds.Create);
end.
