unit UWavePlayer;

interface

uses
{$define USEBASS }
{$Ifdef USEBASS}
 BassMidi,
{$else}
 DAV_DspBufferedMp3Player, DAV_DspBufferedAudioFilePlayer, DAV_AudioFileWAV, DAV_ChannelDataCoder,
{$endif}
      DAV_Types,Classes, ExtCtrls, Messages;

type TPlayMode = (pm3play,pm3pause,pm3stop);
     TArrayOfSingle = array of single;
     PArrayOfSingle = ^TArrayOfSingle;
{$Ifdef USEBASS}
type TWavePlayer = class
private
  handle:HStream;
  FStreamName:string;
  PlayMode:TPlayMode;
  SampleBuffer:array of single;
  TotSamples:integer;
  procedure SetStreamName(value:string);
  procedure Close;
  procedure Open;
public
  {property } Length:integer;
  property StreamName:string read FStreamName write SetStreamName;
  constructor Create;
  procedure Process(const Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
  procedure Stop;
  procedure Play;
  procedure TogglePause;
  procedure SetPause(pause:boolean);
  function Position:integer;
  function getSamples(filename:string;VAR SampleBuffer:TArrayOfSingle; var FChannels:integer; maxSamples:integer;var LoopStart,LoopEnd:integer):boolean;
end;
{$else}
type EWaveType =  (wtNone,wtMP3,wtWAV);

type TWavePlayer = class
private
  FStreamName:string;
  FSamplesProcessed:integer;
  PlayMode:TPlayMode;
  FMP3Player: TBufferedMP3FilePlayer;
  FWAVPlayer: TBufferedAudioFilePlayer;
  FWaveType :EWaveType;
  FDecodeBuffer:PArrayOfSingle;
  procedure SetStreamName(value:string);
  procedure Close;
  procedure Open;
    procedure MP3EndOfFile(Sender: TObject);
    procedure WavDecode(Sender: TObject; const Coder: TCustomChannelDataCoder;
      var Position: Cardinal);
public
  {property } Length:integer;
  function Position:integer;
  procedure Stop;
  procedure TogglePause;
  procedure SetPause(pause:boolean);
  property StreamName:string read FStreamName write SetStreamName;
  constructor Create;
  procedure Play;
  procedure Process(const Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
  function getSamples(filename:string;VAR SampleBuffer:TArrayOfSingle; var FChannels:integer; maxSamples:integer; var LoopStart,LoopEnd:integer):boolean;

end;
{$endif}


implementation

uses  SysUtils, Windows, Math,DAV_AudioFile, DAV_ChunkWaveBasic;

{$Ifdef USEBASS}

constructor TWavePlayer.Create;
VAR i:integer;
begin
	BASS_Init(0, 44100, 0, 0, nil);
end;

function TWavePlayer.getSamples(filename:string;var SampleBuffer: TArrayOfSingle; var FChannels:integer; maxSamples:integer;var LoopStart,LoopEnd:integer): boolean;
VAR p:pchar;
    FsampleLength:integer;
    info:BASS_ChannelInfo;
    handle:HStream;
begin
  p:=PChar(FileName);
  handle:=BASS_StreamCreateFile(False, p , 0, 0, BASS_UNICODE or BASS_SAMPLE_FLOAT or BASS_STREAM_DECODE);
  if handle<>0 then
  begin
    FsampleLength:=trunc(BASS_ChannelBytes2Seconds(handle,BASS_ChannelGetLength(handle,BASS_POS_BYTE))*44100);
    BASS_ChannelGetInfo(handle,info);
    FChannels:=info.chans;
    if FsampleLength>maxSamples then FsampleLength:=maxSamples;
    SetLength(SampleBuffer,FsampleLength*FChannels);
    BASS_ChannelGetData(handle,SampleBuffer,(FsampleLength*FChannels*sizeof(single)) or BASS_DATA_FLOAT);  //crashed bij 47000 samples
    BASS_StreamFree(handle);
  end
  else
    handle:=0 ; // for debug
  result:=handle<>0;
end;

procedure TWavePlayer.SetPause(pause: boolean);
begin
  if pause then begin if PlayMode = pm3Play then PlayMode:=pm3Pause; end
           else begin if PlayMode = pm3Pause then PlayMode:=pm3Play; end

end;

procedure TWavePlayer.SetStreamName(value:string);
begin
  FStreamName:=value;
  Open;
end;

procedure TWavePlayer.Close;
VAR i:integer;
begin
  PlayMode:=pm3Stop;
  if handle<>0 then
      BASS_StreamFree(handle);
  handle:=0;
end;

procedure TWavePlayer.Stop;
begin
  PlayMode:=pm3Stop;
end;

procedure TWavePlayer.TogglePause;
begin
  if handle=0 then exit;
  case PlayMode of
    pm3Play: PlayMode:=pm3Pause;
    pm3Pause: PlayMode:=pm3Play;
  end;
end;

procedure TWavePlayer.Open;
VAR p:pchar;
begin
  Close;
  BASS_SetDevice(0);
  p:=PChar(StreamName);
  handle:=BASS_StreamCreateFile(False, p , 0, 0, BASS_UNICODE or BASS_SAMPLE_FLOAT or BASS_STREAM_DECODE);
  if handle<>0 then
      length:=trunc(BASS_ChannelBytes2Seconds(handle,BASS_ChannelGetLength(handle,BASS_POS_BYTE)));
  TotSamples:=0;
  Play;
end;

procedure TWavePlayer.Play;
begin
  if handle<>0 then
    PlayMode:=pm3Play;
end;

function TWavePlayer.Position: integer;
begin
  result:=TotSamples DIV 44100;
end;

procedure TWavePlayer.Process(const Outputs: TDAVArrayOfSingleFixedArray;  const SampleFrames: Cardinal);
VAR i,l:integer;
    ok:boolean;
begin
  ok:=(PlayMode=pm3Play) and (handle<>0);

  if ok then
  begin
    inc(TotSamples,SampleFrames);
    SetLength(SampleBuffer,2*SampleFrames*4);
    l:=BASS_ChannelGetData(handle,SampleBuffer,8*SampleFrames or BASS_DATA_FLOAT); // Moet niet @ zijn ??
    if l<>8*SampleFrames then // EOF, Restart
    begin
      ok:=false;
      Open;
    end;
  end;
  for i := 0 to SampleFrames - 1 do
    begin
      if ok then  Outputs[0][i]:=SampleBuffer[2*i] else Outputs[0][i]:=0;
      if ok then  Outputs[1][i]:=SampleBuffer[2*i+1] else Outputs[1][i]:=0;
    end;
end;

{$else}

{ TWavePlayer }

procedure TWavePlayer.Close;
begin
  PlayMode:=pm3Stop;
  Sleep(50);
  FWaveType:=wtNONE;
  Sleep(50);
  FreeAndNIL(FMP3Player);
  FreeAndNIL(FWAVPlayer);
end;

constructor TWavePlayer.Create;
begin
  RegisterFileFormat(TAudioFileWAV);
end;

function TWavePlayer.getSamples(filename: string;  var SampleBuffer: TArrayOfSingle; var FChannels: integer;  maxSamples: integer; var LoopStart,LoopEnd:integer): boolean;
VAR
  FWAVPlayer: TAudioFileWAV;
  loopItem:TLoopItem;
  k:integer;
begin
  result:=false;
  try
  FWAVPlayer:=TAudioFileWAV.Create(filename);
  except
    exit;
  end;
  FChannels:=FWAVPlayer.ChannelCount;
  length:=min(maxSamples,FWAVPlayer.SampleFrames);
  SetLength(SampleBuffer,length*FChannels);
  FDecodeBuffer:=@SAmpleBuffer;
  FWAVPlayer.OnDecode:=WavDecode;
  FWAVPlayer.Decode(0,length);
  if (FWAVPlayer.SamplerChunk<>NIL) and (FWAVPlayer.SamplerChunk.LoopCollection.Count>0) then
  begin
    loopItem:=TLoopItem(FWAVPlayer.SamplerChunk.LoopCollection.Items[0]);
    loopStart:=loopItem.LoopStart;
    loopend:=loopItem.LoopEnd;
  end
  else
  begin
    loopStart:=length DIV 2;
    loopEnd:=trunc(length*0.95);
  end;
  FWAVPlayer.Free;
  result:=true;
end;

procedure TWavePlayer.WavDecode(Sender: TObject;   const Coder: TCustomChannelDataCoder; var Position: Cardinal);
VAR Channel: Cardinal;
  Coder32: TCustomChannel32DataCoder absolute Coder;
  i:integer;
  v:single;
begin
  Assert(Coder is TCustomChannel32DataCoder);
  for Channel := 0 to Coder.ChannelCount - 1 do
    for i:=0 to Coder32.SampleFrames-1 do
    begin
      v:=Coder32.ChannelPointer[Channel]^[i];
      FDecodeBuffer^[Coder.ChannelCount*i+Channel+Position]:=v;
    end;
end;

procedure TWavePlayer.MP3EndOfFile(Sender:TObject);
begin
  Stop;
end;

procedure TWavePlayer.Open;
begin
  Close;
  if UpperCase(ExtractFileExt(FStreamName))='.MP3' then
  begin
    FMP3Player:=TBufferedMP3FilePlayer.Create;
    FMP3Player.Interpolation:=DAV_DspBufferedMp3Player.biBSpline6Point5thOrder;
    FMP3Player.BufferSize := 65536;
    FMP3Player.BlockSize  := 4096;
    FMP3Player.Filename :=FStreamName;
    FMP3Player.MpegAudio.OnEndOfFile:=MP3EndOfFile;
    length:=trunc(FMP3Player.MpegAudio.EstimatedLength);
    FWaveType:=wtMP3;
  end
  else if UpperCase(ExtractFileExt(FStreamName))='.WAV' then
  begin
    FWAVPlayer:=TBufferedAudioFilePlayer.Create;
    FWAVPlayer.BufferSize := 65536;
    FWAVPlayer.BlockSize  := 4096;
    FWAVPlayer.Filename :=FStreamName;
// TODO     FWAVPlayer.SampleRate:=1;
// TODO: End Of File ...     FWAVPlayer.OnChange:=NIL
    length:=trunc(FWAVPlayer.AudioFile.TotalTime);    // use SampleFrames to retreive ..
    FWaveType:=wtWAV;
  end
  else
  begin
    FWaveType:=wtNONE;
    exit;
  end;
  FSamplesProcessed:=0;
end;

procedure TWavePlayer.Play;
begin
  if FWaveType<>wtNone then
    PlayMode:=pm3Play;
end;

function TWavePlayer.Position: integer;
begin
  if FWaveType= wtNone then result:=0
                       else result:=FSamplesProcessed DIV 44100;
end;

procedure TWavePlayer.Process(const Outputs: TDAVArrayOfSingleFixedArray;  const SampleFrames: Cardinal);
VAR ok:boolean;
    i,j:integer;
begin
  inc(FSamplesProcessed,SampleFrames);
  ok:=(PlayMode=pm3Play) and (FWaveType<>wtNone);
  if ok then
    if FWaveType = wtMP3 then
      FMP3Player.GetSamples(Outputs[0],Outputs[1],SampleFrames)
    else
      FWavPlayer.GetSamples(Outputs, SampleFrames)
  else for j:=0 to 1 do for i:=0 to SampleFrames-1 do Outputs[j][i]:=0;
end;

procedure TWavePlayer.SetPause(pause: boolean);
begin
  if pause then begin if PlayMode = pm3Play then PlayMode:=pm3Pause; end
           else begin if PlayMode = pm3Pause then PlayMode:=pm3Play; end
end;

procedure TWavePlayer.SetStreamName(value: string);
begin
  Stop;
  FStreamName:=value;
  Open;
  Play;
end;

procedure TWavePlayer.Stop;
begin
  PlayMode:=pm3Stop;
end;

procedure TWavePlayer.TogglePause;
begin
  if FWaveType=wtNone then exit;
  case PlayMode of
    pm3Play: PlayMode:=pm3Pause;
    pm3Pause: PlayMode:=pm3Play;
  end;

end;

{$endif}
end.




