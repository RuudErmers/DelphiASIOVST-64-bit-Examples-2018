unit UBASSPlayer;

interface

uses Bass,Classes, ExtCtrls, Messages,DAV_Types;

type TPlayMode = (pm3play,pm3pause,pm3stop);
type TBassPlayer = class
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
  procedure Process(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
  procedure Stop;
  procedure Play;
  procedure SetPause(Pause:boolean);
  function Position:integer;
end;


implementation

uses  SysUtils, Windows,Dialogs, Forms;

constructor TBassPlayer.Create;
VAR i:integer;
begin
	BASS_Init(0, 44100, 0, 0, nil);
end;

procedure TBassPlayer.SetStreamName(value:string);
begin
  FStreamName:=value;
  Open;
end;

procedure TBassPlayer.Close;
VAR i:integer;
begin
  PlayMode:=pm3Stop;
  if handle<>0 then
      BASS_StreamFree(handle);
  handle:=0;
end;

procedure TBassPlayer.Stop;
begin
  PlayMode:=pm3Stop;
end;

procedure TBassPlayer.SetPause(Pause:boolean);
VAR i:integer;
begin
  if handle=0 then exit;
  case PlayMode of
  pm3Play: PlayMode:=pm3Pause;
  pm3Pause: PlayMode:=pm3Play;
end;
end;

procedure TBassPlayer.Open;
VAR p:pchar;
begin
  Close;
  BASS_SetDevice(0);
  p:=PChar(StreamName);
  handle:=BASS_StreamCreateFile(False, p , 0, 0, BASS_UNICODE or BASS_SAMPLE_FLOAT or BASS_STREAM_DECODE);
  if handle<>0 then
      length:=trunc(BASS_ChannelBytes2Seconds(handle,BASS_ChannelGetLength(handle,BASS_POS_BYTE))*1000);
  TotSamples:=0;
  Play;
end;

procedure TBassPlayer.Play;
VAR i:integer;
    vol:real;
begin
  if handle<>0 then
    PlayMode:=pm3Play;
end;

function TBassPlayer.Position: integer;
begin
  result:=TotSamples DIV 44100;
end;

procedure TBassPlayer.Process(const Inputs, Outputs: TDAVArrayOfSingleFixedArray;  const SampleFrames: Cardinal);
VAR i,l:integer;
    ok:boolean;
begin
  ok:=(PlayMode=pm3Play) and (handle<>0);

  if ok then
  begin
    inc(TotSamples,SampleFrames);
    SetLength(SampleBuffer,2*SampleFrames*4);
    l:=BASS_ChannelGetData(handle,@SampleBuffer,8*SampleFrames or BASS_DATA_FLOAT);
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

end.




