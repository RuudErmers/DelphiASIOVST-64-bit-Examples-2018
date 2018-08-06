program LoudnessR128;

{$I DAV_Compiler.inc}
{$APPTYPE CONSOLE}

uses
  {$IFDEF FPC}
  Interfaces,
  {$ELSE}
  {$IFNDEF COMPILER16_UP}
  FastMove,
  {$ENDIF}
  {$ENDIF}
  SysUtils, Math, DAV_AudioFile, DAV_AudioFileWAV,
  DAV_AudioFileAIFF, DAV_AudioFileAU, DAV_MpegAudio, DAV_DspR128,
  DAV_AudioData;


procedure CalculateLoudness(FileName: TFileName);
var
  R128        : TStereoR128;
  ADC         : TAudioDataCollection32;
  SampleIndex : Integer;
begin
 R128 := TStereoR128.Create;
 R128.Time := ltIntegrated;
 R128.ResetPeak;
 R128.StartIntegration;
 try
  ADC := TAudioDataCollection32.Create(nil);
  try
   Writeln('Loading File: ' + FileName);
   ADC.LoadFromFile(FileName);
   R128.SampleRate := ADC.SampleRate;

   Writeln('Calculating...');

   // check channel count
   case ADC.ChannelCount of
    1 : for SampleIndex := 0 to ADC.SampleFrames - 1
         do R128.ProcessMono(ADC.ChannelDataPointer[0]^[SampleIndex]);
    2 : for SampleIndex := 0 to ADC.SampleFrames - 1 do R128.ProcessStereo(
         ADC.ChannelDataPointer[0]^[SampleIndex],
         ADC.ChannelDataPointer[1]^[SampleIndex]);
    else raise Exception.Create('Unsupported number of channels');
   end;

  finally
   FreeAndNil(ADC);
  end;
  R128.StopIntegration;
  Writeln('Integrated Loudness: ' + FloatToStrF(RoundTo(R128.LoudnessIntegration, -1), ffGeneral, 4, 4) + ' LUFS');
  Writeln('Peak Momentary Loudness: ' + FloatToStrF(RoundTo(R128.LoudnessPeak, -1), ffGeneral, 4, 4) + ' LUFS');
 finally
  FreeAndNil(R128);
 end;
end;

var
  SR : TSearchRec;

begin
 try
  if ParamStr(1) = ''
   then WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' Filename') else
  if FileExists(ParamStr(1))
   then CalculateLoudness(ParamStr(1))
   else
    if FindFirst(ParamStr(1), faAnyFile, SR) = 0 then
     try
      repeat
       CalculateLoudness(SR.Name);
       Writeln('---------------------------------');
      until FindNext(SR) <> 0;
     finally
      // Must free up resources used by these successful finds
      FindClose(SR);
     end;

//  Readln;
 except
   on E: Exception do
     Writeln(E.ClassName, ': ', E.Message);
 end;
end.
