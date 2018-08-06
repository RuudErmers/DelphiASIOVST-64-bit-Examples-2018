unit WaveIOX;

{
Unit WaveIOX;
This unit reads WAV files of almost any format
and converts them into 32-bit floating point buffers.

The basic structure is from the WaveIO unit by
Carlos Barbosa (delphi@carlosb.com) with various
extensions like the reader/converter from me.
You can get the original I/O routines from his
site(http://www.carlosb.com).

Have fun!

Tobybear
www.tobybear.de
tobybear@web.de

History:
1.0: initial release
1.1: fixed some conversion errors,
     thanks to Christian Knufinke for finding them!
}

interface

{$I DAV_Compiler.inc}

uses
  Windows, Classes, MMSystem, SysUtils, MSACMX;

const
  cMPEGLayer3 = 85;
  cBufferSize = 12000;

  //High-Level functions
function LoadWAVFileMono(FileName: TFileName; var SampleRate, Channels, WavSize: LongInt): Pointer;
function LoadWAVFile(FileName: TFileName; var SampleRate, Channels, WavSize: LongInt): Pointer;
procedure SaveWAVFile(fn: string; fdata: pointer; sr, ch, bits, size: LongInt);
procedure SaveWAVFileSeparateStereo(fn: string; fdata1, fdata2: pointer; sr, ch, bits, size: LongInt);
procedure GetWAVFileInfo(FileName: TFileName; var SampleRate, Channels, WavSize: LongInt);

type
  EWaveIOError = Exception;

  TWaveStream = class(TObject)
  private
    FFormat            : PWaveFormatEx;
    FDataOffset, FSize : LongInt;
    FFullsize, FFlSize : LongInt;
    FMM                : hmmIO;
    FPck, FPckRIFF     : TMMCKINFO;

    function GetPosition: LongInt;
    procedure SetPosition(Pos: LongInt);
    procedure CheckMMIOWave;

  public
    constructor Create(FMMIO: hmmIO; WriteFormat: PWaveFormatEx);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; virtual;
    function Seek(Offset: LongInt; Origin: Word): LongInt; virtual;
    function Write(var Buffer; Count: LongInt): LongInt; virtual;

    property Format: PWaveFormatEx read FFormat;
    property Position: LongInt read GetPosition write SetPosition;
    property Size: LongInt read FSize;
    property FloatSize: LongInt read FFlSize;
  end;

  TMemoryWaveStream = class(TWaveStream)
  private
    FMMIO: hmmIO;
  public
    constructor Create(Memory: Pointer; Size: LongInt;
      WriteFormat: PWaveFormatEx); virtual;
    destructor Destroy; override;
  end;

  TFilewaveStream = class(TWaveStream)
  private
    FMMIO: hmmIO;
  public
    constructor Create(FileName: String; WriteFormat: PWaveFormatEx); virtual;
    destructor Destroy; override;
  end;

  TPCMWaveReader = class(TObject)
  private
    FPosition              : LongInt;
    FStream                : TWaveStream;
    FSize                  : LongInt;
    FBufferLength          : LongInt;
    FACMStream             : Integer;
    FACMStreamHeader       : TACMStreamHeader;
    FDstWFx                : PWaveFormatEx;
    FPFullSize, FPFlSize   : LongInt;

    FRawBuffer             : Pointer;
    FRawBufferByteSize     : LongInt;
    FRawBufferSampleSize   : LongInt;

    FPCMBuffer             : Pointer;
    FPCMBufferByteSize     : LongInt;   // size of total buffer in Bytes
    FPCMBufferSampleSize   : LongInt;   // size of converted samples in buffer

    FPCMSamplesPerSample   : LongInt;
    FPCMBufferSamplePos    : LongInt;

    procedure AllocBuffers;
    procedure DestroyBuffers;
    procedure ReadSamples(Index: LongInt);
    procedure SetBufferLength(Value: LongInt);
    procedure SetPosition(Value: LongInt);

  public
    constructor Create(Stream: TWaveStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt;

    property Format: PWaveFormatEx read FDstWFx;
    property BufferLength: LongInt read FBufferLength write SetBufferLength;
    property Position: LongInt read FPosition write SetPosition;
    property Size: LongInt read FSize;
    property Stream: TWaveStream read FStream;
    function BufferToFloat(makemono: boolean): pointer;
  end;

  TWavWriter = class(TObject)
  private
    FFormat: TWaveFormatEx;
    FStream: TFilewaveStream;
  public
    constructor Create(fn: string; sr, ch, bits: LongInt);
    destructor Destroy; override;
    procedure WriteFloatData(p: pointer; size: LongInt);
    procedure WriteFloatDataSeparateStereo(p1, p2: pointer; size: LongInt);

    property Format: TWaveFormatEx read FFormat;
    property Stream: TFilewaveStream read FStream;
  end;

implementation


function mmioFourCC(Chr1: Char; Chr2: Char; Chr3: Char; Chr4: Char): DWord;
begin
  Result := Integer(Chr1) +
            Integer(Chr2) shl 8 +
            Integer(Chr3) shl 16 +
            Integer(Chr4) shl 24;
end;

constructor TWaveStream.Create(FMMIO: hmmIO; WriteFormat: PWaveFormatEx);
begin
  inherited Create;
  filemode := 0;
  FMM := FMMIO;

  if (WriteFormat <> nil) then
   begin
    // Create the output file RIFF chunk of form type 'WAVE'.
    FPckRIFF.fccType := mmioFOURCC('W', 'A', 'V', 'E');
    FPckRIFF.cksize := 0;
    if (mmioCreateChunk(FMM, @FPckRIFF, MMIO_CREATERIFF) <> 0) then
      raise EWaveIOError.Create('Error 01: Cannot create chunk.');

    // We are now descended into the 'RIFF' chunk we just created.
    // * Now create the 'fmt ' chunk. Since we know the size of this chunk,
    // specify it in the MMCKINFO structure so FMMIO doesn't have to seek
    // back and set the chunk size after ascending from the chunk.

    FPck.ckid := mmioFOURCC('f', 'm', 't', ' ');
    FPck.cksize := SizeOf(WriteFormat^) + WriteFormat^.cbSize;
    if (mmioCreateChunk(FMM, @FPck, 0) <> 0) then
      raise EWaveIOError.Create('Error 02: Cannot create chunk.');

    // Write the variable length size.
    if (mmioWrite(FMM, Pointer(WriteFormat), SizeOf(WriteFormat^) +
      WriteFormat^.cbSize) <> SizeOf(WriteFormat^) + WriteFormat^.cbSize) then
      raise EWaveIOError.Create('Error 03: Cannot write wave format.');

    GetMem(FFormat, SizeOf(FFormat^));
    CopyMemory(FFormat, WriteFormat, SizeOf(FFormat^));

    // Ascend out of the 'fmt ' chunk,back into the 'RIFF' chunk.
    if (mmioAscend(FMM, @FPck, 0) <> 0) then
      raise EWaveIOError.Create('Error 04: Cannot ascend chunk.');

    // We are now descended into the 'RIFF' chunk we just created.
    // * Now create the 'data' chunk.
    FPck.ckid := mmioFOURCC('d', 'a', 't', 'a');
    FPck.cksize := 0;
    if (mmioCreateChunk(FMM, @FPck, 0) <> 0) then
      raise EWaveIOError.Create('Error 05: Cannot create chunk.');
   end
  else
   begin
    CheckMMIOWave;

    // put in the beggining of the file
    mmioSeek(FMM, FDataOffset, SEEK_SET);
   end;
end;


destructor TWaveStream.Destroy;
begin
  if (FFormat <> nil) then FreeMem(FFormat);

  mmioSeek(FMM, 0, SEEK_END);

  if (mmioAscend(FMM, @FPck, 0) <> 0)
   then raise EWaveIOError.Create('Error 06: Cannot ascend chunk');

  if (mmioAscend(FMM, @FPckRIFF, 0) <> 0)
   then raise EWaveIOError.Create('Error 07: Cannot ascend chunk');

  if (mmioFlush(FMM, 0) <> 0)
   then raise EWaveIOError.Create('Error 08: Cannot flush');
   
  if (mmioClose(FMM, 0) <> 0)
   then raise EWaveIOError.Create('Error 09: Cannot close');

  inherited Destroy;
end;


function TWaveStream.GetPosition: LongInt;
begin
  Result := Seek(0, SEEK_CUR);
end;

procedure TWaveStream.SetPosition(Pos: LongInt);
begin
  Seek(Pos, SEEK_SET);
end;

function TWaveStream.Read(var Buffer; Count: LongInt): LongInt;
var
  p: Pointer;
begin
  p := @Buffer;

  Result := mmioRead(FMM, p, Count * FFormat^.nBlockAlign);
  if (Result = -1)
   then raise EWaveIOError.Create('Error 09: Cannot read from file.')
   else Result := Result div FFormat^.nBlockAlign;
end;

function TWaveStream.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
 if (Origin = SEEK_SET)
  then Result := (mmioSeek(FMM, Offset * FFormat^.nBlockAlign + FDataOffset, Origin) - FDataOffset) div FFormat^.nBlockAlign
  else Result := (mmioSeek(FMM, Offset * FFormat^.nBlockAlign, Origin) - FDataOffset) div FFormat^.nBlockAlign;

 if (Result < 0)
  then raise EWaveIOError.Create('Error 10: Cannot seek in file.');
end;

function TWaveStream.Write(var Buffer; Count: LongInt): LongInt;
begin
 Result := mmioWrite(FMM, @Buffer, Count * FFormat^.nBlockAlign);
 if (Result = -1)
  then raise EWaveIOError.Create('Error 11: Cannot write to file.')
  else Result := Result div FFormat^.nBlockAlign;
end;

procedure TWaveStream.CheckMMIOWave;
var
  mmIOInfo   : TMMIOInfo;
  FormatTmp  : TWaveFormatEx;
  ExtraAlloc : Word;
begin
  if (mmioDescend(FMM, @FPckRIFF, nil, 0) <> 0) then
    raise EWaveIOError.Create('Error 12: Invalid multimedia file!');

  if (FPckRIFF.ckid <> FOURCC_RIFF) or
    (FPckRIFF.fccType <> mmioFOURCC('W', 'A', 'V', 'E')) then
    raise EWaveIOError.Create('Error 13: Not a wave file!');

  // Search the input file for for the 'fmt ' chunk.     */
  FPck.ckid := mmioFOURCC('f', 'm', 't', ' ');
  if (mmioDescend(FMM, @FPck, @FPckRIFF, MMIO_FINDCHUNK) <> 0) then
    raise EWaveIOError.Create('Error 14: Cannot find ''fmt'' chunk!');

  // Expect the 'fmt' chunk to be at least as large as <PCMWAVEFORMAT>;
  // if there are extra parameters at the end,we'll ignore them */
  if (FPck.cksize < 16) then
    raise EWaveIOError.Create('Error 15: Abnormal ''fmt'' size!');

  // Read the 'fmt ' chunk into <pcmWaveFormat>.*/
  if (mmioRead(FMM, PAnsiChar(@FormatTmp), 16) <> 16) then
    raise EWaveIOError.Create('Error 16: Cannot read ''fmt'' chunk!');

  // Ok, allocate the waveformatex, but if its not PCM
  // format, read the next word and thats how many extra
  // Bytes to allocate.
  if (FormatTmp.wFormatTag = WAVE_FORMAT_PCM) or (FormatTmp.wFormatTag = 3)
   then ExtraAlloc := 0 else

  // Read in length of extra Bytes.
  if (mmioRead(FMM, @ExtraAlloc, SizeOf(ExtraAlloc)) <> SizeOf(ExtraAlloc))
   then raise EWaveIOError.Create('Error 17: Cannot read ''waveformatex'' length!');

  GetMem(FFormat, SizeOf(FFormat^) + ExtraAlloc);
  CopyMemory(FFormat, @FormatTmp, SizeOf(FFormat^));
  FFormat^.cbSize := ExtraAlloc;
  if (ExtraAlloc <> 0) then
   if (mmioRead(FMM, PAnsiChar(FFormat) + SizeOf(FFormat^), ExtraAlloc) <> ExtraAlloc)
    then raise EWaveIOError.Create('Error 18: Cannot read ''waveformatex''!');

  if (FFormat^.wFormatTag = cMPEGLayer3)
   then raise EWaveIOError.Create('Error 19: MPEG Layer-3 compression is not supported.');

  // Ascend the input file out of the 'fmt ' chunk. */
  if (mmioAscend(FMM, @FPck, 0) <> 0)
   then raise EWaveIOError.Create('Error 20: Cannot ascend from ''fmt'' chunk!');

  // Do a nice little seek...
  if (mmioSeek(FMM, FPckRIFF.dwDataOffset + SizeOf(FOURCC), SEEK_SET) = -1)
   then raise EWaveIOError.Create('Error 21: Cannot seek to data!');

  // Search the input file for the 'data' chunk.
  FPck.ckid := mmioFOURCC('d', 'a', 't', 'a');
  mmioDescend(FMM, @FPck, @FPckRIFF, MMIO_FINDCHUNK);

  if (mmioGetInfo(FMM, @mmioInfo, 0) <> 0) then
    raise EWaveIOError.Create('Error 22: Cannot get info!');

  FDataOffset := FPck.dwDataOffset;
  FSize := FPck.cksize div FFormat^.nBlockAlign;
  FFlSize := FSize;
end;


//***************************************************************
//        TMemoryWaveStream
//***************************************************************

constructor TMemoryWaveStream.Create(Memory: Pointer; Size: LongInt;
  WriteFormat: PWaveFormatEx);
var
  info: TMMIOINFO;
begin
  ZeroMemory(@info, SizeOf(info));
  with info do
   begin
    pchBuffer := Memory;
    fccIOProc := FOURCC_MEM;
    cchBuffer := Size;
   end;

  // Initialization...
  FMMIO := mmioOpen(nil, @info, MMIO_READ);
  if (FMMIO = 0) then
    raise EWaveIOError.Create('Error 23: Cannot open memory stream.');

  inherited Create(FMMIO, WriteFormat);
end;

destructor TMemoryWaveStream.Destroy;
begin
  inherited Destroy;

  if (FMMIO <> 0) then mmioClose(FMMIO, 0);
end;


//***************************************************************
//        TFilewaveStream
//***************************************************************

constructor TFilewaveStream.Create(FileName: String;
  WriteFormat: PWaveFormatEx);
var
  f: file of Byte;
begin
  // Initialization...
  if (WriteFormat = nil) then
   begin
    assignfile(f, filename);
   {$I-} reset(f);{$I+}
    if ioresult <> 0 then
      raise EWaveIOError.Create('Error 24: Cannot open file.')
    else
     begin
      FFullsize := filesize(f);
      CloseFile(f);
     end;
    FMMIO := mmioOpen(Pointer(FileName), nil, MMIO_READ or MMIO_ALLOCBUF)
   end
  else FMMIO := mmioOpen(Pointer(FileName), nil, MMIO_CREATE or MMIO_READWRITE or MMIO_ALLOCBUF);

  if (FMMIO = 0) then raise EWaveIOError.Create('Error 25: Cannot open file stream.');

  inherited Create(FMMIO, WriteFormat);
end;

destructor TFilewaveStream.Destroy;
begin
  inherited Destroy;
  if (FMMIO <> 0) then mmioClose(FMMIO, 0);
end;


//***************************************************************
//        TPCMWaveReader
//***************************************************************

constructor TPCMWaveReader.Create(Stream: TWaveStream);
begin
  inherited Create;

  FStream := Stream;
  GetMem(FDstWFx, SizeOf(FDstWFx^));

//  FBufferLength := 4096;
  FBufferLength := 20000;

  if (FStream.Format^.wFormatTag <> WAVE_FORMAT_PCM) and (FStream.Format^.wFormatTag <> 3) then
   begin
    // prepare acm stream converter
    ZeroMemory(FDstWFx, SizeOf(FDstWFx^));
    FDstWFx^.wFormatTag := WAVE_FORMAT_PCM;
    if (acmFormatSuggest(0, FStream.FFormat, FDstWFx, SizeOf(FDstWFx^),
      ACM_FORMATSUGGESTF_WFORMATTAG) <> 0) then
      raise EWaveIOError.Create('Error 26: Cannot suggest pcm format.');

    if (acmStreamOpen(PHACMSTREAM(@FACMStream), 0, FStream.Format^, FDstWFx^,
      nil, 0, 0, 0) <> 0) then
      raise EWaveIOError.Create('Error 27: Cannot open acm stream.');

    AllocBuffers;

    // prepare buffers
    ZeroMemory(@FACMStreamHeader, SizeOf(FACMStreamHeader));
    with FACMStreamHeader do
     begin
      cbStruct := SizeOf(FACMStreamHeader);
      pbSrc := Pointer(FRawBuffer);
      cbSrcLength := FRawBufferByteSize;
      dwSrcUser := cbSrcLength;
      pbDst := Pointer(FPCMBuffer);
      cbDstLength := FPCMBufferByteSize;
      dwDstUser := cbDstLength;
     end;

    if (acmStreamPrepareHeader(FACMStream, FACMStreamHeader, 0) <> 0) then
      raise EWaveIOError.Create('Error 28: Cannot prepare headers.');
   end
  else FDstWFx^ := FStream.Format^;

  if (FStream.Format^.wFormatTag <> WAVE_FORMAT_PCM) and
    (FStream.Format^.wFormatTag <> 3) then
    FPCMSamplesPerSample :=
      (FPCMBufferByteSize * FStream.Format^.nBlockAlign) div
      (FRawBufferByteSize * FDstWFx^.nBlockAlign)
  else FPCMSamplesPerSample := 1;
  FSize := FPCMSamplesPerSample * FStream.Size;
  FPCMBufferSampleSize := 0;
end;


destructor TPCMWaveReader.Destroy;
begin
  if (FACMStream <> 0) then
   begin
    FACMStreamHeader.cbSrcLength := FRawBufferByteSize;
    acmStreamUnprepareHeader(FACMStream, FACMStreamHeader, 0);
    acmStreamClose(FACMStream, 0);
   end;

  if (FDstWFx <> nil) then FreeMem(FDstWFx);

  DestroyBuffers;

  inherited Destroy;
end;


procedure TPCMWaveReader.AllocBuffers;
var
  ss, dd : Cardinal;
begin
  DestroyBuffers;

  // calc space needed for holding the decompression of one sample
  if (acmStreamSize(FACMStream, FStream.Format^.nBlockAlign, dd, ACM_STREAMSIZEF_SOURCE) <> 0)
   then raise EWaveIOError.Create('Error 29: Cannot recommend an acm stream size.');

  // calc minimum size block of the source
  if (acmStreamSize(FACMStream, dd, ss, ACM_STREAMSIZEF_DESTINATION) <> 0)
   then raise EWaveIOError.Create('Error 30: Cannot recommend an acm stream size.');

  // alloc source buffer(raw)
  FRawBufferSampleSize := FBufferLength div Integer(dd);
  if (FRawBufferSampleSize = 0)
   then FRawBufferSampleSize := 1;
  FRawBufferByteSize := FRawBufferSampleSize * Integer(ss);
  GetMem(FRawBuffer, FRawBufferByteSize);

  // Alloc destination buffer(decompressed)
  if (acmStreamSize(FACMStream, DWord(FRawBufferByteSize), DWord(FPCMBufferByteSize), ACM_STREAMSIZEF_SOURCE) <> 0)
   then raise EWaveIOError.Create('Error 31: Cannot recommend an acm stream size.');
  GetMem(FPCMBuffer, FPCMBufferByteSize);
end;


procedure TPCMWaveReader.DestroyBuffers;
begin
  FPCMBufferSampleSize := 0;

  if (FPCMBuffer <> nil) then FreeMem(FPCMBuffer);
  FPCMBuffer := nil;

  if (FRawBuffer <> nil) then FreeMem(FRawBuffer);
  FRawBuffer := nil;
end;

procedure TPCMWaveReader.SetBufferLength(Value: LongInt);
begin
  if (Value <> FBufferLength) and (Value >= 1024) then
   begin
    FBufferLength := Value;
    if (FPCMBuffer <> nil) then AllocBuffers;
   end;
end;

procedure TPCMWaveReader.SetPosition(Value: LongInt);
begin
  if (Value <> FPosition) then
   if (Value >= 0) and (Value < FSize)
    then FPosition := Value
    else raise EWaveIOError.Create('Error 32: Position out of bounds.');
end;

function TPCMWaveReader.Read(var Buffer; Count: LongInt): LongInt;
var
  pos, len: LongInt;
  posi, posf: LongInt;
begin
  if Count < 1 then
   begin
    Result := 0;
    Exit;
   end;

  if (FStream.Format^.wFormatTag = WAVE_FORMAT_PCM) or
    (FStream.Format^.wFormatTag = 3) then
   begin
    FStream.Position := FPosition;
    Result := FStream.Read(Buffer, Count);
    FPosition := FPosition + Result;
   end
  else
   begin
    if (Count + FPosition >= FSize) then
      Count := FSize - FPosition;                                 // limit to wave size
    if (FPosition >= FPCMBufferSamplePos) and
      (FPosition < FPCMBufferSamplePos + FPCMBufferSampleSize)
 // use current buffer data if possible
    then
     begin
      len := FPCMBufferSamplePos + FPCMBufferSampleSize - FPosition;
      if (len > Count) then
        len := Count;
      CopyMemory(PChar(@Buffer), PChar(FPCMBuffer) +
        (FPosition - FPCMBufferSamplePos) * FDstWFx^.nBlockAlign, len * FDstWFx^.nBlockAlign);
      pos := len;
     end
    else
      pos := 0;
    while (pos < Count) do // put next data
     begin
      ReadSamples((FPosition + pos) div FPCMSamplesPerSample);
 // calc. range of current pcm buffer that can be used to fill request
      posi := FPosition + pos;
      if (FPosition + pos < FPCMBufferSamplePos) then
        raise EWaveIOError.Create(
          'Error 33: Position smaller than PCMBufferSamplePos');
      if (FPosition + Count > FPCMBufferSamplePos + FPCMBufferSampleSize) then
        posf := FPCMBufferSamplePos + FPCMBufferSampleSize
      else
        posf := FPosition + Count;
      len := posf - posi;
      // put pcm buffer data into target
      CopyMemory(PChar(@Buffer) + (posi - FPosition) * FDstWFx^.nBlockAlign,
        PChar(FPCMBuffer) + (posi - FPCMBufferSamplePos) * FDstWFx^.nBlockAlign,
        len * FDstWFx^.nBlockAlign);
      pos := pos + len;
     end;
    FPosition := FPosition + Count;
    Result := Count;
   end;
end;

procedure TPCMWaveReader.ReadSamples(Index: LongInt);
begin
  FStream.Position := Index;
  FACMStreamHeader.cbSrcLength := FStream.Read(FRawBuffer^, FRawBufferSampleSize) * FStream.Format^.nBlockAlign;
  if (acmStreamConvert(FACMStream, FACMStreamHeader, ACM_STREAMCONVERTF_BLOCKALIGN) <> 0)
   then raise EWaveIOError.Create('Error 34: Unable to convert sample.');
  FPCMBufferSampleSize := FACMStreamHeader.cbDstLengthUsed div FDstWFx^.nBlockAlign;
  FPCMBufferSamplePos := Index * FPCMSamplesPerSample;
end;

function TPCMWaveReader.BufferToFloat(makemono: boolean): pointer;
var
  p              : ^shortint;
  p2             : ^Byte;
  ps, pf         : ^Single;
  fbuffer,
  buffer         : pointer;
  l              : LongInt;

  procedure Convert(n: LongInt);
  var
    cnt, cc, v : LongInt;
    v2         : smallint;
    s          : Single;
    y, y2, y3  : Byte;
    by         : Byte;
  begin
    cnt := 0;
    while (cnt <= format.nChannels * (n - 1)) do
      if (format.wBitsPerSample = 8) then
       begin
        for cc := 1 to format.nChannels do
         begin
          by := p2^;
          Inc(p2);
          s := ((2 * by / (1 shl 8 - 1)) - 1);
          if (not makemono) then
           begin
            pf^ := s;
            Inc(pf);
           end else
           begin
            pf^ := pf^ + s;
            if cc = format.nChannels then
             begin
              pf^ := pf^ / format.nChannels;
              Inc(pf);
             end;
           end;
          Inc(cnt);
         end;
       end else
      if (format.wBitsPerSample = 16) then
       begin
        for cc := 1 to format.nChannels do
         begin
          y := p^;
          Inc(p);
          y2 := p^;
          Inc(p);
          v2 := y + y2 * (1 shl 8);
          s := v2;
          if s > 0 then
            s := s / (1 shl 15 - 1)
          else
            s := s / (1 shl 15);
          if (not makemono) then
           begin
            pf^ := s;
            Inc(pf);
           end else
           begin
            pf^ := pf^ + s;
            if cc = format.nChannels then
             begin
              pf^ := pf^ / format.nChannels;
              Inc(pf);
             end;
           end;
          Inc(cnt);
         end;
       end else
      if (format.wBitsPerSample = 20) then
       begin
        for cc := 1 to format.nChannels do
         begin
          y := p^;
          Inc(p);
          y2 := p^;
          Inc(p);
          y3 := p^;
          Inc(p);
          if not (format.nBlockAlign mod 3 = 0) then
            Inc(p);
          v := (y + y2 * (1 shl 8) + y3 * (1 shl 16)) shr 4;
          if v >= 1 shl 19 then
            v := v - 1 shl 20;
          s := v;
          if s > 0 then
            s := s / (1 shl 19 - 1)
          else
            s := s / (1 shl 19);
          if (not makemono) then
           begin
            pf^ := s;
            Inc(pf);
           end else
           begin
            pf^ := pf^ + s;
            if cc = format.nChannels then
             begin
              pf^ := pf^ / format.nChannels;
              Inc(pf);
             end;
           end;
          Inc(cnt);
         end;
       end else
      if (format.wBitsPerSample = 24) then
       begin
        for cc := 1 to format.nChannels do
         begin
          y := p^;
          Inc(p);
          y2 := p^;
          Inc(p);
          y3 := p^;
          Inc(p);
          if not (format.nBlockAlign mod 3 = 0) then
            Inc(p);
          v := y + y2 * (1 shl 8) + y3 * (1 shl 16);
          if v >= (1 shl 23) then
            v := v - (1 shl 24);
          s := v;
          if s > 0 then
            s := s / (1 shl 23 - 1)
          else
            s := s / (1 shl 23);
          if (not makemono) then
           begin
            pf^ := s;
            Inc(pf);
           end else
           begin
            pf^ := pf^ + s;
            if cc = format.nChannels then
             begin
              pf^ := pf^ / format.nChannels;
              Inc(pf);
             end;
           end;
          Inc(cnt);
         end;
       end else
      if (format.wBitsPerSample = 32) then
        for cc := 1 to format.nChannels do
         begin
          s := ps^;
          Inc(ps);
          if (not makemono) then
           begin
            pf^ := s;
            Inc(pf);
           end else
           begin
            pf^ := pf^ + s;
            if cc = format.nChannels then
             begin
              pf^ := pf^ / format.nChannels;
              Inc(pf);
             end;
           end;
          Inc(cnt);
         end;
  end;

begin
  if FPFullSize > FPFlSize then FPFlSize := FPFullSize;
  l := format.nChannels * FPFlSize * SizeOf(Single);
  if makemono then l := l div format.nChannels;
  GetMem(FBuffer, l);
  GetMem(buffer, cBufferSize * SizeOf(smallint) * 4);
  fillchar(fbuffer^, l, 0);
  p := buffer;
  p2 := buffer;
  pf := fbuffer;
  l := Read(buffer^, cBufferSize);
  while (l > 0) do
   begin
    p := buffer;
    p2 := buffer;
    ps := buffer;
    convert(l);
    l := Read(buffer^, cBufferSize);
   end;
  if makemono
   then FPFlSize := FSize
   else FPFlSize := format.nChannels * FSize;
  FreeMem(buffer);
  Result := fbuffer;
end;

{ TWavWriter }

constructor TWavWriter.Create(fn: string; sr, ch, bits: Integer);
var
  p: PWaveFormatEx;
begin
  FFormat.nChannels := ch;
  FFormat.nSamplesPerSec := sr;
  if bits > 32 then bits := 32;
  FFormat.wBitsPerSample := bits;
  case bits of
   20, 24 :
    begin
     FFormat.nBlockAlign := 3;
     FFormat.wFormatTag := WAVE_FORMAT_PCM
    end;
   32 :
    begin
     FFormat.nBlockAlign := 4;
     FFormat.wFormatTag := 3
    end;
   else
    begin
     FFormat.nBlockAlign := (bits + 7) div 8;
     FFormat.wFormatTag := WAVE_FORMAT_PCM;
    end;
  end;
  FFormat.nAvgBytesPerSec := sr * ch * FFormat.nBlockAlign;
  p := @format;
  FStream := TFileWaveStream.Create(fn, p);
end;

destructor TWavWriter.Destroy;
begin
 FreeAndNil(FStream);
 Sleep(10);
 inherited Destroy;
end;

procedure TWavWriter.WriteFloatData(p: pointer; size: Integer);
var
  ps    : PSingle;
  l, li : LongInt;
  x     : Single;
begin
  ps := p;
  for l := 0 to size - 1 do
   begin
    if Format.wBitsPerSample = 32 then
     begin
      x := ps^;
      li := LongInt((@x)^);
     end
    else
      li := round(ps^ * (1 shl (Format.wBitsPerSample - 1)));
    stream.Write(li, 1);
    Inc(ps);
   end;
end;


procedure TWavWriter.WriteFloatDataSeparateStereo(p1, p2: Pointer; Size: Integer);
var
  ps, ps2 : PSingle;
  l, li   : LongInt;
  x       : Single;
begin
  ps := p1;
  ps2 := p2;
  for l := 0 to size - 1 do
   begin
    if Format.wBitsPerSample = 32 then
     begin
      x := ps^;
      li := LongInt((@x)^);
     end
    else
      li := round(ps^ * (1 shl (Format.wBitsPerSample - 1)));
    stream.Write(li, 1);
    Inc(ps);
    if Format.wBitsPerSample = 32 then
     begin
      x := ps2^;
      li := LongInt((@x)^);
     end
    else
      li := round(ps2^ * (1 shl (Format.wBitsPerSample - 1)));
    stream.Write(li, 1);
    Inc(ps2);
   end;
end;

{ High-Level functions }

function LoadWAVFile(FileName: TFileName; var SampleRate, Channels, WavSize: LongInt): Pointer;
var
  wave: TFilewaveStream;
begin
  wave := TFilewaveStream.Create(FileName, nil);
  try
   with TPCMWaveReader.Create(wave) do
    try
     SampleRate := wave.format.nSamplesPerSec;
     Channels   := wave.format.nChannels;
     FPFullSize := wave.FFullsize;
     Result     := BufferToFloat(False);
     WavSize    := FPFlSize;
    finally
     Free;
    end;
  finally
   FreeAndNil(wave);
  end;
end;

function LoadWAVFileMono(FileName: TFileName; var SampleRate, Channels, WavSize: LongInt): Pointer;
var
  Wave: TFilewaveStream;
begin
  Wave := TFilewaveStream.Create(FileName, nil);
  try
   with TPCMWaveReader.Create(Wave) do
    try
     SampleRate := Wave.format.nSamplesPerSec;
     FPFullSize := Wave.FFullsize;
     Channels   := 1;
     Result     := BufferToFloat(True);
     WavSize    := FPFlSize;
    finally
     Free;
    end;
  finally
   FreeAndNil(Wave);
  end;
end;

procedure GetWAVFileInfo(FileName: TFileName; var SampleRate, Channels, WavSize: LongInt);
begin
 with TFilewaveStream.Create(FileName, nil) do
  try
   SampleRate := format.nSamplesPerSec;
   Channels   := format.nChannels;
   WavSize    := Size;
  finally
   Free;
  end;
end;

procedure SaveWAVFile(fn: string; fdata: pointer; sr, ch, bits, size: LongInt);
var
  w     : TFilewaveStream;
  ps    : PSingle;
  l, li : LongInt;
  t     : TWaveFormatEx;
  p     : PWaveFormatEx;
  x     : Single;
begin
  t.nChannels := ch;
  t.nSamplesPerSec := sr;
  if bits > 32 then bits := 32;
  t.wBitsPerSample := bits;
  case bits of
    20, 24 :
     begin
      t.nBlockAlign := 3;
      t.wFormatTag := WAVE_FORMAT_PCM
     end;
    32 :
     begin
      t.nBlockAlign := 4;
      t.wFormatTag := 3
     end;
  else
   begin
    t.nBlockAlign := (bits + 7) div 8;
    t.wFormatTag := WAVE_FORMAT_PCM;
   end;
   end;
  t.nAvgBytesPerSec := sr * ch * t.nBlockAlign;
  p := @t;

  w := TFilewaveStream.Create(fn, p);
  ps := fdata;
  for l := 0 to size - 1 do
   begin
    if t.wBitsPerSample = 32 then
     begin
      x := ps^;
      li := LongInt((@x)^);
     end
    else
      li := round(ps^ * (1 shl (bits - 1)));
    w.Write(li, 1);
    Inc(ps);
   end;
  w.Free;
end;

procedure SaveWAVFileSeparateStereo(fn: string; fdata1, fdata2: pointer;
  sr, ch, bits, size: LongInt);
var
  w       : TFilewaveStream;
  ps, ps2 : PSingle;
  l, li   : LongInt;
  t       : TWaveFormatEx;
  p       : PWaveFormatEx;
  x       : Single;
begin
  t.nChannels := ch;
  t.nSamplesPerSec := sr;
  if bits > 32 then
    bits := 32;
  t.wBitsPerSample := bits;
  case bits of
    20, 24 :
     begin
      t.nBlockAlign := 3;
      t.wFormatTag := WAVE_FORMAT_PCM
     end;
    32 :
     begin
      t.nBlockAlign := 4;
      t.wFormatTag := 3
     end;
  else
   begin
    if bits <= 8 then t.nBlockAlign := 1
    else if bits <= 16 then t.nBlockAlign := 2
    else if bits <= 24 then t.nBlockAlign := 3
    else if bits <  32 then t.nBlockAlign := 4;
    t.wFormatTag := WAVE_FORMAT_PCM;
   end;
   end;
  t.nAvgBytesPerSec := sr * ch * t.nBlockAlign;
  p := @t;

  w := TFilewaveStream.Create(fn, p);
  ps := fdata1;
  ps2 := fdata2;
  for l := 0 to size - 1 do
   begin
    if t.wBitsPerSample = 32 then
     begin
      x := ps^;
      li := LongInt((@x)^);
     end
    else li := round(ps^ * (1 shl (bits - 1)));
    w.Write(li, 1);
    Inc(ps);
    if t.wBitsPerSample = 32 then
     begin
      x := ps2^;
      li := LongInt((@x)^);
     end
    else li := round(ps2^ * (1 shl (bits - 1)));
    w.Write(li, 1);
    Inc(ps2);
   end;
  FreeAndNil(w);
end;

end.
