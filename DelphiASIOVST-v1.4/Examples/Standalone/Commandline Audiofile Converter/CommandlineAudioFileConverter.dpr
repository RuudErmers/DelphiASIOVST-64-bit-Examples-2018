program CommandlineAudioFileConverter;

{$APPTYPE CONSOLE}

uses
  DAV_Types,
  DAV_Common,
  DAV_Math,
  DAV_AudioFile,
  DAV_AudioFileWav,
  DAV_AudioFileAiff,
  DAV_AudioFileAu,
  DAV_ChannelDataCoder,
  SysUtils,
  Math;

resourcestring
  RCStrUnknownFileFormat = 'Unknown file format!';

type
  TAudioConverter = class(TObject)
  private
    FTempData     : TDAVArrayOfSingleDynArray;
    FMaxLength    : Cardinal;
    FOffset       : Cardinal;
    FMakePowerOf2 : Boolean;
  public
    AudioFile  : array [0..1] of TCustomAudioFile;
    constructor Create; virtual;
    procedure DecodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
    procedure EncodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
  published
    property MaxLength: Cardinal read FMaxLength;
    property Offset: Cardinal read FOffset;
    property MakePowerOf2: Boolean read FMakePowerOf2;
  end;

constructor TAudioConverter.Create;
begin
 inherited;
 FMaxLength    := 0;
 FOffset       := 0;
 FMakePowerOf2 := False; //True;
end;

procedure TAudioConverter.DecodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel : Integer;
begin
 if Coder is TCustomChannel32DataCoder then
  begin
   SetLength(FTempData, Coder.ChannelCount);
   for Channel := 0 to Coder.ChannelCount - 1 do
    begin
     SetLength(FTempData[Channel], Coder.SampleFrames);
     Move(TCustomChannel32DataCoder(Coder).ChannelPointer[Channel]^[0],
       FTempData[Channel, 0], Coder.SampleFrames * SizeOf(Single));
    end;
  end;
end;

procedure TAudioConverter.EncodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel : Integer;
begin
 if Coder is TCustomChannel32DataCoder then
  begin
   assert(Length(FTempData) = Coder.ChannelCount);
   for Channel := 0 to Coder.ChannelCount - 1 do
    begin
     assert(Length(FTempData[Channel]) = Coder.SampleFrames);
     Move(FTempData[Channel, 0], TCustomChannel32DataCoder(Coder).ChannelPointer[Channel]^[0], Coder.SampleFrames * SizeOf(Single));
    end;
  end;
end;

procedure ConvertFile(InputFile, OutputFile: TFileName);
var
  FileFormat   : TAudioFileClass;
  Converter    : TAudioConverter;
  Sample       : Integer;
  SampleFrames : Cardinal;
const
  CBlockSize : Integer = 2048;
begin
 Converter := TAudioConverter.Create;
 with Converter do
  try
   FileFormat := FileNameToFormat(InputFile);
   if FileFormat = nil
    then raise Exception.Create(RCStrUnknownFileFormat);
   AudioFile[0] := FileFormat.Create(InputFile);
   try
    if OutputFile <> '' then
     begin
      FileFormat := ExtensionToFileFormat(ExtractFileExt(OutputFile));
      if FileFormat = nil
       then raise Exception.Create(RCStrUnknownFileFormat);
      AudioFile[1] := FileFormat.Create(OutputFile);
      try
       AudioFile[1].SampleRate := AudioFile[0].SampleRate;
       AudioFile[0].OnDecode := DecodeHandler;
       AudioFile[1].OnEncode := EncodeHandler;

       Sample := FOffset;
       SampleFrames := AudioFile[0].SampleFrames - FOffset;
       if FMaxLength > 0
        then SampleFrames := Min(SampleFrames, FMaxLength);
       if FMakePowerOf2
        then SampleFrames := TruncToPowerOf2(SampleFrames);

       while Sample < SampleFrames do
        begin
         AudioFile[0].Decode(Sample, CBlockSize);
         AudioFile[1].Encode(Sample, CBlockSize);
         Inc(Sample, CBlockSize);
        end;
       if SampleFrames - Sample > 0 then
        begin
         AudioFile[0].Decode(Sample, SampleFrames - Sample);
         AudioFile[1].Encode(Sample, SampleFrames - Sample);
        end;
      finally
       FreeAndNil(AudioFile[1]);
      end;
     end;
   finally
    FreeAndNil(AudioFile[0]);
   end;
  finally
   FreeAndNil(Converter);
  end;
end;

var
  SR : TSearchRec;
begin
 if ParamStr(1) = '' then
  begin
   Writeln('Commandline Audiofile Converter');
   Writeln('-------------------------------');
   Writeln('');
   Writeln('Usage: ' + ExtractFileName(ParamStr(0)) + ' InputFile [OutputFile]' );
   Exit;
  end;

 if Pos('*', ParamStr(1)) > 0 then
  if FindFirst(ParamStr(1), faAnyFile, SR) = 0 then
   try
    repeat
     ConvertFile(SR.Name, ChangeFileExt(SR.Name, 'X.wav'));
    until FindNext(SR) <> 0;
   finally
    FindClose(SR);
   end else
  else ConvertFile(ParamStr(1), ParamStr(2));
end.
