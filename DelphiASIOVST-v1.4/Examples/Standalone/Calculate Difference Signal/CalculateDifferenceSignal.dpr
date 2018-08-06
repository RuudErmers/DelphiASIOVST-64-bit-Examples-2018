program CalculateDifferenceSignal;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils,
  Math,
  DAV_Types,
  DAV_DspSimpleOscillator,
  DAV_AudioData,
  DAV_AudioFile,
  DAV_AudioFileWAV,
  DAV_AudioFileAIFF;

resourcestring
  RCStrFileMustBeStereo = 'The file must contain stereo data!';

type
  TMyAudioFileWAV = class(TAudioFileWAV)
  public
    property AudioDataPosition: Cardinal read FAudioDataPosition;
    property Stream: TStream read FStream;
  end;

procedure Subtract(FileName: TFileName);
var
  Input       : TAudioDataCollection32;
  Output      : TAudioDataCollection32;
  InData      : array [0..1] of PDAVSingleFixedArray;
  OutData     : PDAVSingleFixedArray;
  ByteSize    : Integer;
  Left        : array [0..3] of Byte;
  Right       : array [0..3] of Byte;
  LeftInt     : Integer absolute Left;
  RightInt    : Integer absolute Right;
  Difference  : Double;
  MaxDiffer   : Double;
  DiffSum     : Double;
  SampleIndex : Integer;
  IntDiff     : Integer;
  AvgDiff     : Integer;
  MaxDiff     : Integer;
  FileStream  : TFileStream;
  MemStream   : TMemoryStream;
begin
  if TAudioFileWAV.CanLoad(FileName) then
    with TMyAudioFileWAV.Create do
    try
      MemStream := TMemoryStream.Create;
      FileStream := TFileStream.Create(FileName, fmOpenRead);
      try
        MemStream.CopyFrom(FileStream, FileStream.Size);
        MemStream.Position := 0;
        LoadFromStream(MemStream);
        MemStream.Position := AudioDataPosition + 8;
        LeftInt := 0;
        RightInt := 0;
        MaxDiff := 0;
        AvgDiff := 0;
        ByteSize := (BitsPerSample + 7) div 8;
        if ByteSize = 3 then
        begin
          Writeln('Calculating Difference in Bits (unoptimized, may take a while!)...');
          for SampleIndex := 0 to SampleFrames - 1 do
          begin
            case ByteSize of
              3 :
                begin
                  MemStream.Read(Left[2], 1);
                  MemStream.Read(Left[3], 1);
                  MemStream.Read(Left[1], 1);
                  MemStream.Read(Right[2], 1);
                  MemStream.Read(Right[3], 1);
                  MemStream.Read(Right[1], 1);
                end;
            end;
            IntDiff := (LeftInt - RightInt) div 256;
            AvgDiff := AvgDiff + IntDiff;
            if Abs(IntDiff) > MaxDiff then
              MaxDiff := Abs(IntDiff);
          end;
        end;
        if MaxDiff > 0 then
        begin
          Writeln('Maximum Difference: ' + IntToStr(MaxDiff) + ' (Bits: ' +
            FloatToStr(RoundTo(Log2(1 + MaxDiff), -2)) + ')');
          Writeln('Average Difference: ' + FloatToStr(AvgDiff / SampleFrames));
          Readln;
        end;
      finally
        FreeAndNil(MemStream);
        FreeAndNil(FileStream);
      end;
    finally
      Free;
    end;

  Input := TAudioDataCollection32.Create(nil);
  with Input do
  try
    LoadFromFile(FileName);
    if Input.ChannelCount <> 2 then
      raise Exception.Create(RCStrFileMustBeStereo);

    Output := TAudioDataCollection32.Create(nil);
    with Output do
    try
      SampleFrames := Input.SampleFrames;
      SampleRate := Input.SampleRate;
      ChannelCount := 1;
      InData[0] := Input.ChannelDataPointer[0];
      InData[1] := Input.ChannelDataPointer[1];
      OutData := Output.ChannelDataPointer[0];
      DiffSum := 0;
      MaxDiffer := 0;
      for SampleIndex := 0 to Input.SampleFrames - 1 do
      begin
        Difference := InData[0]^[SampleIndex] - InData[1]^[SampleIndex];
        if Abs(Difference) > MaxDiffer then
          MaxDiffer := Abs(Difference);
        OutData^[SampleIndex] := Difference;
        DiffSum := DiffSum + Abs(Difference);
      end;

      if DiffSum > 0 then
      begin
        WriteLn('Max. Difference: ' + FloatToStr(MaxDiffer * (1 shl 23 - 1)));
        ReadLn;
        SaveToFile(ChangeFileExt(FileName, '.diff.wav'))
      end
      else
        WriteLn('No difference');
    finally
      FreeAndNil(Output);
    end;
  finally
    FreeAndNil(Input);
  end;
end;

begin
  try
    if (ParamCount >= 1) and FileExists(ParamStr(1)) then
      Subtract(ParamStr(1))
    else
      raise Exception.CreateFmt('File %s does not exist', [ParamStr(1)]);

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

