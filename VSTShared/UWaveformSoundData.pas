unit UWaveformSoundData;

interface

uses Generics.Collections,Classes,UWaveFormOscillator,UWavePlayer;

type TWaveSoundRegion = class
private
  SampleBuffer:TArrayOfSingle;
  FChannels:integer;
  FBaseFrequency: single;
//  FLoopRegionStart,
  FLoopStartPoint,FLoopEndPoint:integer;

  procedure SaveToStream(str:TMemoryStream);
  procedure LoadFromStream(str:TMemoryStream);
  
public
  function SampleAt(VAR info:TWaveFormInfo;frequency:single):single;
  procedure SetSound(filename:string;frequency:single);
  destructor Destroy;
end;

  TWaveSoundData = class(TComponent,IWaveFormSoundData)
private
  FModifying:boolean;
  FWaveSound:EWaveSound;
    procedure SaveSound(filename: string);
    procedure LoadSound(filename: string);
    procedure SetSound(wave: EWaveSound);
protected
    FWaveSoundRegion:TList<TWaveSoundRegion>;
    procedure LoadSamples(wave: EWaveSound);virtual;abstract;
public
  function SampleAt(VAR info:TWaveFormInfo;frequency:single):single;
  constructor Create(wave:EWaveSound);
end;

implementation

uses SysUtils;

destructor TWaveSoundRegion.Destroy;
begin
  SetLength(SampleBuffer,0);
end;

function TWaveSoundRegion.SampleAt(VAR info:TWaveFormInfo;frequency:single): single;
  function _SampleAt(position:single):single;
  begin
    if FChannels = 2 then
    begin
      result:=SampleBuffer[round(2*Position)]+SampleBuffer[round(2*Position)+1];
      result:=result / 2;
    end
    else
      result:=SampleBuffer[round(Position)];
  end;
VAR t:single;
begin
  if info.position>=FLoopEndPoint then
    info.position:=info.position + FloopStartPoint-FLoopEndPoint;
  result:=_SampleAt(info.Position);
  info.Position:=info.Position+frequency/FBaseFrequency;
end;

const MAGIC = 345283;
procedure TWaveSoundRegion.SaveToStream(str: TMemoryStream);
VAR l:integer;
begin
  l:=MAGIC;
  str.Write(l,sizeof(integer));
  str.Write(FChannels,sizeof(integer));
  str.Write(FBaseFrequency,sizeof(single));
  str.Write(FLoopStartPoint,sizeof(single));
  str.Write(FLoopEndPoint,sizeof(single));
  l:=length(SampleBuffer);
  str.Write(l,sizeof(integer));
  str.Write(SampleBuffer[0],l*sizeof(single));
end;

procedure TWaveSoundRegion.LoadFromStream(str: TMemoryStream);
VAR l:integer;
begin
  str.Read(l,sizeof(integer));
  if l<>MAGIC then exit;
  str.Read(FChannels,sizeof(integer));
  str.Read(FBaseFrequency,sizeof(single));
  str.Read(FLoopStartPoint,sizeof(single));
  str.Read(FLoopEndPoint,sizeof(single));
  str.Read(l,sizeof(integer));
  SetLength(SampleBuffer,l);
  str.Read(SampleBuffer[0],l*sizeof(single));
end;

procedure TWaveSoundRegion.SetSound(filename: string;frequency:single);
VAR wavePlayer:TWavePlayer;
begin
  FBaseFrequency:=frequency;
  wavePlayer:=TWavePlayer.Create;
  wavePlayer.getSamples(filename,SampleBuffer,FChannels,20*44100,FloopstartPoint,FloopendPoint);
  wavePlayer.free;
end;

{ TWaveSoundData }

constructor TWaveSoundData.Create(wave:EWaveSound);
begin
  FWaveSound:=wvNone;
  FWaveSoundRegion:=TList<TWaveSoundRegion>.Create;
  SetSound(wave);
end;

function TWaveSoundData.SampleAt(var info: TWaveFormInfo;  frequency: single): single;
VAR i,index:integer;
begin
  if FModifying or (FWaveSound=wvNone) or (FWaveSoundRegion.Count=0) then
  begin
    result:=0;
    exit;
  end;
  if info.region =-1 then
  begin
    index:=FWaveSoundRegion.Count-1;
    for i:=1 to FWaveSoundRegion.Count-1 do
    if (frequency<FWaveSoundRegion[i].FBaseFrequency) then
    begin
      index:=i-1;
      break;
    end;
    info.region:=index;
  end;
  result:=FWaveSoundRegion[info.region].SampleAt(info,frequency);
end;

procedure TWaveSoundData.LoadSound(filename:string);
VAR str:TMemoryStream;
    i,l:integer;
    regCount:integer;
VAR region:TWaveSoundRegion;

begin
  str:=TMemoryStream.Create;
  str.LoadFromFile('c:\midi\data\waves\'+filename+'.sound');
  str.Read(regCount,sizeof(integer));
  while FWaveSoundRegion.Count>0 do
  begin
    FWaveSoundRegion[FWaveSoundRegion.Count-1].Free;
    FWaveSoundRegion.Delete(FWaveSoundRegion.Count-1);
  end;
  for i:=0 to regCount-1 do
  begin
    region:=TWaveSoundRegion.Create;
    region.LoadFromStream(str);
    FWaveSoundRegion.Add(region);
  end;
  str.free;
end;

procedure TWaveSoundData.SaveSound(filename:string);
VAR str:TMemoryStream;
    i,l:integer;
begin
  str:=TMemoryStream.Create;
  str.Write(FWaveSoundRegion.Count,sizeof(integer));
  for i:=0 to FWaveSoundRegion.Count-1 do
    FWaveSoundRegion[i].SaveToStream(str);
  str.SaveToFile('c:\midi\data\waves\'+filename+'.sound');
  str.free;
  LoadSound(filename);
end;

{$DEFINE LOADFROMSOUNDS}
{$DEFINE SAVESOUNDS}
procedure TWaveSoundData.SetSound(wave: EWaveSound);
begin
  if (FWaveSound=wave) then exit;
  FModifying:=true;
  while FWaveSoundRegion.Count>0 do
  begin
    FWaveSoundRegion[FWaveSoundRegion.Count-1].Free;
    FWaveSoundRegion.Delete(FWaveSoundRegion.Count-1);
  end;
  FWaveSound:=wave;
{$ifdef LOADFROMSOUNDS }
  LoadSound(SWaveSound[FWaveSound]);
{$else}
  LoadSamples(wave);
{$ifdef SAVESOUNDS}
  SaveSound(SWaveSound[FWaveSound]);
{$endif}
{$endif}
  FModifying:=false;
end;



end.
