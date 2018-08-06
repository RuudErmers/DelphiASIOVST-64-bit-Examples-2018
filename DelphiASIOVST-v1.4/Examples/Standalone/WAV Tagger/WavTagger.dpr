program WavTagger;

{$I DAV_Compiler.inc}
{$APPTYPE CONSOLE}

uses
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF}
  SysUtils, Classes, Math, DAV_AudioFile, DAV_AudioFileWAV, DAV_ChunkWaveBasic;

type
  TTaggedAudioFileWAV = class(TAudioFileWAV);

procedure TagFile(FileName: TFileName);
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmOpenReadWrite);
 try
  if TTaggedAudioFileWAV.CanLoad(FileStream) then
   with TTaggedAudioFileWAV.Create do
    try
     // read wav file
     LoadFromStream(FileStream);

     if not Assigned(CartChunk)
      then AddSubChunk(TCartChunk.Create);

     with CartChunk do
      begin
       Artist := 'Your name';
      end;

     // reset file position
     FileStream.Seek(soFromBeginning, 0);

     // finally save wave file
     SaveToStream(FileStream);
    finally
     Free;
    end;
 finally
  FreeAndNil(FileStream);
 end;
end;

var
  SR : TSearchRec;

begin
 try
  if ParamStr(1) = ''
   then WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' Filename') else
  if FileExists(ParamStr(1))
   then TagFile(ParamStr(1))
   else
    if FindFirst(ParamStr(1), faAnyFile, SR) = 0 then
     try
      repeat
       TagFile(SR.Name);
       Writeln('---------------------------------');
      until FindNext(SR) <> 0;
     finally
      // Must free up resources used by these successful finds
      FindClose(SR);
     end;

  Readln;
 except
   on E: Exception do
     Writeln(E.ClassName, ': ', E.Message);
 end;
end.
