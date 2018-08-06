unit USoundDataSamplesSyntorchestra;

interface

uses UWaveFormSoundData,UWaveformOscillator, SysUtils;

implementation

type TWaveSoundDataSyntorchestra = class(TWaveSoundData)
   procedure LoadSamples(wave: EWaveSound);override;
end;

type TSyntorchestraSounds = class(TSoundsFactory)
   wavesounddata: array [wvSynthPolyTrombone..wvSynthMonoViolin] of TWaveSoundData;
   function LoadWaveData(wavesound:EWaveSound):IWaveFormSoundData;override;
   constructor create;
end;

procedure TWaveSoundDataSyntorchestra.LoadSamples(wave: EWaveSound);
    procedure LoadSyntorchestra(fid,basename:string);
    VAR region:TWaveSoundRegion;
        i:integer;
        filename:string;
    begin
      for i:=1 to 13 do
      begin
        region:=TWaveSoundRegion.Create;
        filename:='C:\Midi\Data\Syntorchestra\Samples\'+FId+'-'+basename+'.'+inttostr(i)+'.wav';
        region.SetSound(filename,pitchToFrequency[48+3*(i-1)]);
        FWaveSoundRegion.Add(region);
      end;
    end;

begin
  case wave of
    wvSynthPolyTrombone:   LoadSyntorchestra('FSO01','Trombone Poly');
    wvSynthPolyViola:      LoadSyntorchestra('FSO21',' Viola Poly');  // vanaf FS020 staat er een extra spatie, duh...
    wvSynthPolyTrumpet:    LoadSyntorchestra('FSO20','Trumpet Poly');
    wvSynthPolyPiano:      LoadSyntorchestra('FSO40',' Piano Poly');  // vanaf FS020 staat er een extra spatie, duh...
    wvSynthMonoTuba:       LoadSyntorchestra('FSO31',' Tuba Solo');
    wvSynthMonoTrombone:   LoadSyntorchestra('FSO32',' Trombone Solo');
    wvSynthMonoTrumpet:    LoadSyntorchestra('FSO33',' Trumpet Solo');
    wvSynthMonoBaritoneSax:LoadSyntorchestra('FSO34',' Bariton Sax Solo');
    wvSynthMonoAltoSax:    LoadSyntorchestra('FSO35',' Alto Sax Solo');
    wvSynthMonoBassFlute:  LoadSyntorchestra('FSO36',' Bass Flute Solo');
    wvSynthMonoFlute:      LoadSyntorchestra('FSO37',' Flute Solo');
    wvSynthMonoPiccolo:    LoadSyntorchestra('FSO38',' Piccolo Solo');
    wvSynthMonoViolin:     LoadSyntorchestra('FSO39',' Violin Solo');
  end;
end;

constructor TSyntorchestraSounds.create;
VAR wavesound: EWaveSound;
begin
// preload...
  for wavesound:=low(wavesounddata) to high(wavesounddata) do
    LoadWaveData(wavesound);
end;

function TSyntorchestraSounds.LoadWaveData(wavesound: EWaveSound): IWaveFormSoundData;
begin
  if (wavesound>=low(wavesounddata)) and (wavesound<=high(wavesounddata)) then
  begin
    if wavesounddata[wavesound] = NIL then
      wavesounddata[wavesound]:=TWaveSoundDataSyntorchestra.Create(wavesound);
    result:=wavesounddata[wavesound];
  end
  else
    result:=NIL;
end;

begin
//  IWaveSoundFactory.AddSoundFactory(TSyntorchestraSounds.Create);
end.

