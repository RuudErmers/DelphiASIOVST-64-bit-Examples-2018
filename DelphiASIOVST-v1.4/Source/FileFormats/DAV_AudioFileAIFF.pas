unit DAV_AudioFileAIFF;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Types, DAV_AudioFile, DAV_ChunkClasses,
  DAV_ChunkAiffBasic, DAV_ChannelDataCoder;

type
  TAiffChunkScan = (acsName, acsAuthor, acsCopyright, acsMarker,
    acsAudioRecording, acsComment, acsInstrument);
  TAiffChunkScans = set of TAiffChunkScan;

  TCustomAudioFileAIFF = class(TCustomAudioFile, IAudioFileBitsPerSample,
    IAudioFileEncoding)
  private
    FIsCompressed        : Boolean;
    FChunkSize           : Cardinal;
    FCommonChunk         : TAIFFCommonChunk;
    FCommentChunk        : TAIFFCommentChunk;
    FNameChunk           : TAIFFNameChunk;
    FAuthorChunk         : TAIFFAuthorChunk;
    FCopyrightChunk      : TAIFFCopyrightChunk;
    FAudioRecordingChunk : TAIFFAudioRecordingChunk;
    FMarkerChunk         : TAIFFMarkerChunk;
    FInstrumentChunk     : TAIFFInstrumentChunk;
    FVersionChunk        : TAIFFFormatVersionChunk;
    FAiffChunkScans      : TAiffChunkScans;
    FAudioDataPosition   : Cardinal;
    function GetAESChannelStatusData: AnsiString;
    function GetAIFFName: AnsiString;
    function GetAuthor: AnsiString;
    function GetCopyright: AnsiString;
    procedure SetAESChannelStatusData(const Value: AnsiString);
    procedure SetAIFFName(const Value: AnsiString);
    procedure SetAuthor(const Value: AnsiString);
    procedure SetCopyright(const Value: AnsiString);
    procedure WriteSSNDChunk(const Stream: TStream);
    function GetDataSize: Cardinal;
    function GetEmptyData: Boolean;
  protected
    function GetBitsPerSample: Byte; virtual;
    function GetEncoding: TAudioEncoding; virtual;
    function GetChannels: Cardinal; override;
    function GetSampleRate: Double; override;
    function GetSampleFrames: Cardinal; override;
    function CreateDataCoder: TCustomChannelDataCoder;

    procedure ReadAESDChunk(const Stream: TStream); virtual;
    procedure ReadANNOChunk(const Stream: TStream); virtual;
    procedure ReadALCHChunk(const Stream: TStream); virtual;
    procedure ReadAPPLChunk(const Stream: TStream); virtual;
    procedure ReadAUTHChunk(const Stream: TStream); virtual;
    procedure ReadCOMMChunk(const Stream: TStream); virtual;
    procedure ReadCOMTChunk(const Stream: TStream); virtual;
    procedure ReadCOPYChunk(const Stream: TStream); virtual;
    procedure ReadFVERChunk(const Stream: TStream); virtual;
    procedure ReadINSTChunk(const Stream: TStream); virtual;
    procedure ReadMARKChunk(const Stream: TStream); virtual;
    procedure ReadNAMEChunk(const Stream: TStream); virtual;
    procedure ReadSSNDChunk(const Stream: TStream); virtual;
    procedure ReadUnknownChunk(const Stream: TStream); virtual;

    procedure SetBitsPerSample(const Value: Byte); virtual;
    procedure SetEncoding(const Value: TAudioEncoding); virtual;
    procedure SetChannels(const Value: Cardinal); override;
    procedure SetSampleRate(const Value: Double); override;
    procedure SetSampleFrames(const Value: Cardinal); override;
    procedure ReadAndSkipSize(const Stream: TStream);
    procedure CheckHeader(const Stream: TStream); override;
    procedure ParseStream(const Stream: TStream); override;
    procedure ReadAudioDataFromStream(const Stream: TStream);
    procedure WriteAudioDataToStream(const Stream: TStream);
    procedure WriteTotalSampleFrames(const Stream: TStream);
    property EmptyData: Boolean read GetEmptyData;
    procedure SampleFramesChanged; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    // load/save stream
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    // decode/encode
    procedure Decode(SamplePosition: Cardinal; SampleFrames: Cardinal); override;
    procedure Encode(SamplePosition: Cardinal; SampleFrames: Cardinal); override;

    // file format identifier
    class function DefaultExtension: string; override;
    class function Description: string; override;
    class function FileFormatFilter: string; override;
    class function CanLoad(const Stream: TStream): Boolean; override;

    property BitsPerSample: Byte read GetBitsPerSample write SetBitsPerSample;
    property Encoding: TAudioEncoding read GetEncoding write SetEncoding;
    property DataSize: Cardinal read GetDataSize;
    property AiffChunkScans: TAiffChunkScans read FAiffChunkScans write
      FAiffChunkScans default [acsName, acsAuthor, acsCopyright, acsMarker,
      acsComment, acsInstrument];

    property Name: AnsiString read GetAIFFName write SetAIFFName;
    property Author: AnsiString read GetAuthor write SetAuthor;
    property Copyright: AnsiString read GetCopyright write SetCopyright;
    property AESChannelStatusData: AnsiString read GetAESChannelStatusData write SetAESChannelStatusData;
  end;

  TAudioFileAIFF  = class(TCustomAudioFileAIFF)
  published
    property SampleRate;
    property ChannelCount;
    property SampleFrames;
    property TotalTime;
    property BitsPerSample;
    property Encoding;
    property AiffChunkScans;

    property OnEncode;
    property OnDecode;
  end;

  EAIFFError = class(Exception);

implementation

uses
  DAV_Common;

resourcestring
  RCStrFORMChunkNotFound      = 'This is not a AIFF file!';
  RCStrFORMSizeMismatch       = 'Filesize mismatch';
  RCStrAIFFChunkNotFound      = 'This is not a WAVE file!';
  RCStrFMTChunkDublicate      = 'One format chunk has already been found!';
  RCStrFACTChunkDublicate     = 'One fact chunk has already been found!';
  RCStrDATAChunkDublicate     = 'Only one data chunk supported!';
  RCStrIntegerEncodingOnly    = 'Audio encoding for AIFF is aeInteger only';
  RCStrOneVersionChunkOnly    = 'Only one version chunk allowed';
  RCStrOneCommentChunkOnly    = 'Only one comment chunk allowed';
  RCStrOneMarkerChunkOnly     = 'Only one marker chunk allowed';
  RCStrOneInstrumentChunkOnly = 'Only one instrument chunk allowed';
  RCStrOneCopyrightChunkOnly  = 'Only one copyright chunk allowed';
  RCStrOneNameChunkOnly       = 'Only one name chunk allowed';
  RCStrOneAuthorChunkOnly     = 'Only one author chunk allowed';
  RCStrOneAESChunkOnly        = 'Only one audio recording chunk allowed';
  RCStrNoSoundData = 'No sound data information found!';

{ TCustomAudioFileAIFF }

constructor TCustomAudioFileAIFF.Create;
begin
 inherited;
 FCommonChunk := TAIFFCommonChunk.Create;
 FAudioDataPosition := 0;
 FAiffChunkScans := [acsName, acsAuthor, acsCopyright, acsMarker, acsComment,
   acsInstrument];
end;

destructor TCustomAudioFileAIFF.Destroy;
begin
 FreeAndNil(FCommonChunk);
 if Assigned(FCommentChunk)             then FreeAndNil(FCommentChunk);
 if Assigned(FMarkerChunk)              then FreeAndNil(FMarkerChunk);
 if Assigned(FInstrumentChunk)          then FreeAndNil(FInstrumentChunk);
 if Assigned(FVersionChunk)             then FreeAndNil(FVersionChunk);
 if Assigned(FNameChunk)                then FreeAndNil(FNameChunk);
 if Assigned(FAuthorChunk)              then FreeAndNil(FAuthorChunk);
 if Assigned(FCopyrightChunk)           then FreeAndNil(FCopyrightChunk);
 if Assigned(FAudioRecordingChunk)      then FreeAndNil(FAudioRecordingChunk);
 inherited;
end;

class function TCustomAudioFileAIFF.DefaultExtension: string;
begin
 Result := '.aiff';
end;

class function TCustomAudioFileAIFF.Description: string;
begin
 Result := 'Audio Interchange File Format';
end;

class function TCustomAudioFileAIFF.FileFormatFilter: string;
begin
 Result := Description + ' (*.' + DefaultExtension + ')|*.aif*'
end;

class function TCustomAudioFileAIFF.CanLoad(const Stream: TStream): Boolean;
var
  ChunkName   : TChunkName;
  ChunkSize   : Cardinal;
  OldPosition : Cardinal;
begin
 Result := False;

 // store old position
 OldPosition := Stream.Position;

 with Stream do
  try
   // check minimum file size
   if Size < 12 then exit;

   // check whether file is a resource interchange file format ('RIFF')
   Read(ChunkName, 4);
   if ChunkName <> 'FORM' then Exit;

   // check whether the real file size match the filesize stored inside the RIFF chunk
   Read(ChunkSize, 4);
   Flip32(ChunkSize);
   if (ChunkSize > ((Size + 1) shr 1) shl 1 - Position) and not (ChunkSize = $FFFFFFFF)
    then Exit;

   // now specify the RIFF file to be a WAVE file
   Read(ChunkName, 4);
   if (ChunkName <> 'AIFF') and (ChunkName <> 'AIFC') then Exit;

   Result := True;
  finally
   // restore old position
   Position := OldPosition;
  end;
end;

function TCustomAudioFileAIFF.GetAuthor: AnsiString;
begin
 if Assigned(FAuthorChunk)
  then Result := FAuthorChunk.Author
  else Result := '';
end;

function TCustomAudioFileAIFF.GetEmptyData: Boolean;
begin
 Result := FAudioDataPosition = 0;
end;

function TCustomAudioFileAIFF.GetBitsPerSample: Byte;
begin
 Result := FCommonChunk.SampleSize;
end;

function TCustomAudioFileAIFF.GetChannels: Cardinal;
begin
 Result := FCommonChunk.Channels;
end;

function TCustomAudioFileAIFF.GetCopyright: AnsiString;
begin
 if Assigned(FCopyrightChunk)
  then Result := FCopyrightChunk.Copyright
  else Result := '';
end;

function TCustomAudioFileAIFF.GetDataSize: Cardinal;
begin
 Result := FCommonChunk.SampleFrames * Cardinal(FCommonChunk.Channels) *
   Cardinal((FCommonChunk.SampleSize + 7) div 8);
end;

function TCustomAudioFileAIFF.GetEncoding: TAudioEncoding;
begin
 Result := aeInteger;
end;

function TCustomAudioFileAIFF.GetAESChannelStatusData: AnsiString;
begin
 if Assigned(FAudioRecordingChunk)
  then Result := FAudioRecordingChunk.AESChannelStatusData
  else Result := '';
end;

function TCustomAudioFileAIFF.GetAIFFName: AnsiString;
begin
 if Assigned(FNameChunk)
  then Result := FNameChunk.Name
  else Result := '';
end;

function TCustomAudioFileAIFF.GetSampleFrames: Cardinal;
begin
 Result := FCommonChunk.SampleFrames;
end;

function TCustomAudioFileAIFF.GetSampleRate: Double;
begin
 Result := FCommonChunk.SampleRate;
end;

procedure TCustomAudioFileAIFF.SetAuthor(const Value: AnsiString);
begin
 if not Assigned(FAuthorChunk)
  then FAuthorChunk := TAIFFAuthorChunk.Create;
 FAuthorChunk.Author := Value; 
end;

procedure TCustomAudioFileAIFF.SetBitsPerSample(const Value: Byte);
begin
 with FCommonChunk do
  if SampleSize <> Value then
   begin
    SampleSize := Value;
   end;
end;

procedure TCustomAudioFileAIFF.SetChannels(const Value: Cardinal);
begin
 with FCommonChunk do
  if Channels <> SmallInt(Value) then
   begin
    inherited;
    Channels := SmallInt(Value);
   end;
end;

procedure TCustomAudioFileAIFF.SetCopyright(const Value: AnsiString);
begin
 if not Assigned(FCopyrightChunk)
  then FCopyrightChunk := TAIFFCopyrightChunk.Create;
 FCopyrightChunk.Copyright := Value;
end;

procedure TCustomAudioFileAIFF.SetEncoding(const Value: TAudioEncoding);
begin
 case Value of
  aeInteger : begin
               FIsCompressed := False;
               FCommonChunk.Compression := ctNotAvailable;
              end;
    aeFloat : begin
               FCommonChunk.Compression := ctFL32;
               FCommonChunk.SampleSize := 32;
               FIsCompressed := True;
              end;
    aeALaw : begin
               FCommonChunk.Compression := ctALAW;
               FCommonChunk.SampleSize := 8;
               FIsCompressed := True;
              end;
   aeMuLaw : begin
               FCommonChunk.Compression := ctULAW;
               FCommonChunk.SampleSize := 8;
               FIsCompressed := True;
              end;
  else raise EAIFFError.Create(RCStrIntegerEncodingOnly);
 end;
end;

procedure TCustomAudioFileAIFF.SetAESChannelStatusData(const Value: AnsiString);
begin
 if not Assigned(FAudioRecordingChunk)
  then FAudioRecordingChunk := TAIFFAudioRecordingChunk.Create;
 FAudioRecordingChunk.AESChannelStatusData := Value;
end;

procedure TCustomAudioFileAIFF.SetAIFFName(const Value: AnsiString);
begin
 if not Assigned(FNameChunk)
  then FNameChunk := TAIFFNameChunk.Create;
 FNameChunk.Name := Value;
end;

procedure TCustomAudioFileAIFF.SetSampleFrames(const Value: Cardinal);
begin
 with FCommonChunk do
  if SampleFrames <> Value then
   begin
    inherited;
    SampleFrames := Value;
    SampleFramesChanged;
   end;
end;

procedure TCustomAudioFileAIFF.SampleFramesChanged;
begin
 if Assigned(FStream)
  then WriteTotalSampleFrames(FStream);
end;

procedure TCustomAudioFileAIFF.SetSampleRate(const Value: Double);
begin
 with FCommonChunk do
  if SampleRate <> Value then
   begin
    inherited;
    SampleRate := Value;
   end;
end;

procedure TCustomAudioFileAIFF.CheckHeader(const Stream: TStream);
var
  ChunkName : TChunkName;
begin
 with Stream do
  begin
   // check whether file is a resource interchange file format ('RIFF')
   Read(ChunkName, 4);
   if ChunkName <> 'FORM'
    then raise EAIFFError.Create(RCStrFORMChunkNotFound);

   // check whether the real file size match the filesize stored inside the RIFF chunk
   Read(FChunkSize, 4);
   Flip32(FChunkSize);
   if (FChunkSize > ((Size + 1) shr 1) shl 1 - Position) and not (FChunkSize = $FFFFFFFF)
    then raise EAIFFError.Create(RCStrFORMSizeMismatch);

   // now specify the RIFF file to be a WAVE file
   Read(ChunkName, 4);
   FIsCompressed := ChunkName = 'AIFC';
   if (ChunkName <> 'AIFF') and (ChunkName <> 'AIFC')
    then raise EAIFFError.Create(RCStrAIFFChunkNotFound);
  end;
end;

procedure TCustomAudioFileAIFF.ParseStream(const Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkEnd     : Cardinal;
begin
 with Stream do
  begin
   // Remove existing optional chunk
   if Assigned(FCommentChunk)             then FreeAndNil(FCommentChunk);
   if Assigned(FMarkerChunk)              then FreeAndNil(FMarkerChunk);
   if Assigned(FInstrumentChunk)          then FreeAndNil(FInstrumentChunk);
   if Assigned(FVersionChunk)             then FreeAndNil(FVersionChunk);
   if Assigned(FNameChunk)                then FreeAndNil(FNameChunk);
   if Assigned(FAuthorChunk)              then FreeAndNil(FAuthorChunk);
   if Assigned(FCopyrightChunk)           then FreeAndNil(FCopyrightChunk);
   if Assigned(FAudioRecordingChunk)      then FreeAndNil(FAudioRecordingChunk);

   // reset current data positions
   FAudioDataPosition := 0;

   assert(Position = 12);
   ChunkEnd := Position + FChunkSize - 4;

   // start parsing here
   while Stream.Position < ChunkEnd do
    begin
     // read chunk name
     Read(ChunkName, 4);

     // set position to chunk start
     Position := Position - 4;

     if ChunkName = 'FVER' then ReadFVERChunk(Stream) else
     if ChunkName = 'COMM' then ReadCOMMChunk(Stream) else
     if ChunkName = 'SSND' then ReadSSNDChunk(Stream) else
     if ChunkName = 'MARK' then ReadMARKChunk(Stream) else
     if ChunkName = 'COMT' then ReadCOMTChunk(Stream) else
     if ChunkName = 'INST' then ReadINSTChunk(Stream) else
     if ChunkName = 'AESD' then ReadAESDChunk(Stream) else
     if ChunkName = 'APPL' then ReadAPPLChunk(Stream) else
     if ChunkName = 'NAME' then ReadNAMEChunk(Stream) else
     if ChunkName = 'AUTH' then ReadAUTHChunk(Stream) else
     if ChunkName = '(c) ' then ReadCOPYChunk(Stream) else
     if ChunkName = 'ANNO' then ReadANNOChunk(Stream)
      else ReadUnknownChunk(Stream);
    end;

   Assert(Position = ChunkEnd);

   if (FCommonChunk.SampleFrames > 0) then
    if (FAudioDataPosition = 0)
     then raise EAIFFError.Create(RCStrNoSoundData)
     else ReadAudioDataFromStream(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ReadFVERChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if Assigned(FVersionChunk)
    then raise EAIFFError.Create(RCStrOneVersionChunkOnly);

   FVersionChunk := TAIFFFormatVersionChunk.Create;
   FVersionChunk.LoadFromStream(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ReadCOMMChunk(const Stream: TStream);
begin
 with Stream do
  begin

   // load common chunk
   FCommonChunk.ForceReadCompression := FIsCompressed;
   FCommonChunk.LoadFromStream(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ReadSSNDChunk(const Stream: TStream);
var
  DataSize : Cardinal;
begin
 with Stream do
  begin
   // store SSND chunk position
   FAudioDataPosition := Position;

   // skip chunk name
   Position := Position + 4;

   // read data size
   Read(DataSize, 4);
   Flip32(DataSize);

   // apply padding
   DataSize := ((DataSize + 1) shr 1) shl 1;

   Position := Position + DataSize;
  end;
end;

procedure TCustomAudioFileAIFF.ReadCOMTChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if Assigned(FCommentChunk)
    then raise EAIFFError.Create(RCStrOneCommentChunkOnly);

   if acsComment in FAiffChunkScans then
    begin
     // load comment chunk
     FCommentChunk := TAIFFCommentChunk.Create;
     FCommentChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ReadMARKChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if Assigned(FMarkerChunk)
    then raise EAIFFError.Create(RCStrOneMarkerChunkOnly);

   if acsMarker in FAiffChunkScans then
    begin
     // load marker chunk
     FMarkerChunk := TAIFFMArkerChunk.Create;
     FMarkerChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ReadINSTChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if Assigned(FInstrumentChunk)
    then raise EAIFFError.Create(RCStrOneInstrumentChunkOnly);

   if acsInstrument in FAiffChunkScans then
    begin
     // load instrument chunk
     FInstrumentChunk := TAIFFInstrumentChunk.Create;
     FInstrumentChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ReadAPPLChunk(const Stream: TStream);
begin
 with Stream, TAIFFUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileAIFF.ReadAESDChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if Assigned(FAudioRecordingChunk)
    then raise EAIFFError.Create(RCStrOneAESChunkOnly);

   if acsAudioRecording in FAiffChunkScans then
    begin
     // load name chunk
     FAudioRecordingChunk := TAIFFAudioRecordingChunk.Create;
     FAudioRecordingChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ReadALCHChunk(const Stream: TStream);
begin
 with Stream, TAIFFUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileAIFF.ReadNAMEChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if Assigned(FNameChunk)
    then raise EAIFFError.Create(RCStrOneNameChunkOnly);

   if acsName in FAiffChunkScans then
    begin
     // load name chunk
     FNameChunk := TAIFFNameChunk.Create;
     FNameChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ReadAUTHChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if Assigned(FAuthorChunk)
    then raise EAIFFError.Create(RCStrOneAuthorChunkOnly);

   if acsAuthor in FAiffChunkScans then
    begin
     // load author chunk
     FAuthorChunk := TAIFFAuthorChunk.Create;
     FAuthorChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ReadCOPYChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if Assigned(FCopyrightChunk)
    then raise EAIFFError.Create(RCStrOneCopyrightChunkOnly);

   if acsCopyright in FAiffChunkScans then
    begin
     // load comment chunk
     FCopyrightChunk := TAIFFCopyrightChunk.Create;
     FCopyrightChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ReadANNOChunk(const Stream: TStream);
begin
 with Stream, TAIFFUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileAIFF.ReadUnknownChunk(const Stream: TStream);
begin
 with Stream, TAIFFUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileAIFF.ReadAndSkipSize(const Stream: TStream);
var
  ChunkSize : Cardinal;
begin
 with Stream do
  begin
   Read(ChunkSize, SizeOf(Cardinal));
   Position := Position + ChunkSize;
  end;
end;

procedure TCustomAudioFileAIFF.LoadFromStream(Stream: TStream);
begin
 inherited;
 CheckHeader(Stream);
 ParseStream(Stream);
end;

procedure TCustomAudioFileAIFF.SaveToStream(Stream: TStream);
var
  ChunkName  : TChunkName;
  ChunkStart : Cardinal;
  ChunkSize  : Cardinal;
  TempSize   : Cardinal;
begin
 inherited;
 with Stream do
  begin
   // Store chunk start position, just in case the stream position is not 0;
   ChunkStart := Position;

   // first write 'RIFF' (resource interchange file format)
   ChunkName := 'FORM';
   Write(ChunkName, 4);

   // write dummy filesize yet, since final size is still unknown
   ChunkSize := $FFFFFFFF;
   Write(ChunkSize, 4);

   // now specify the RIFF file to be a WAVE file
   ChunkName := 'AIFF';
   Write(ChunkName, 4);

   // write format chunk
   FCommonChunk.SaveToStream(Stream);

   if Assigned(FNameChunk) then FNameChunk.SaveToStream(Stream);
   if Assigned(FAuthorChunk) then FAuthorChunk.SaveToStream(Stream);
   if Assigned(FCopyrightChunk) then FCopyrightChunk.SaveToStream(Stream);
   if Assigned(FAudioRecordingChunk) then FAudioRecordingChunk.SaveToStream(Stream);

   WriteAudioDataToStream(Stream);

   // finally write filesize
   ChunkSize := Position - (ChunkStart + 8);
   Position  := ChunkStart + 4;
   TempSize  := ChunkSize;
   Flip32(TempSize);
   Write(TempSize, 4);

   // Reset Position to end of Stream;
   Position := ChunkStart + ChunkSize;
  end;
end;

function TCustomAudioFileAIFF.CreateDataCoder: TCustomChannelDataCoder;
begin
 case FCommonChunk.Compression of
  ctNotAvailable, ctNone:
   begin
    Result := TChannel32DataCoderFixedPoint.Create;
    with TChannel32DataCoderFixedPoint(Result), FCommonChunk
     do SetBitsAndSampleSize(SampleSize, (SampleSize + 7) div 8);
   end;
   ctFL32 : Result := TChannel32DataCoderFloat32.Create;
   ctFL64 : Result := TChannel32DataCoderFloat64.Create;
   ctALAW : Result := TChannel32DataCoderALaw.Create;
   ctULAW : Result := TChannel32DataCoderMuLaw.Create;
  else Result := nil;
 end;

 if Assigned(Result) then
  with Result do
   begin
    BlockSize := Self.FBlockSize;
    ChannelCount := FCommonChunk.Channels;
   end;
end;

procedure TCustomAudioFileAIFF.Decode(SamplePosition, SampleFrames: Cardinal);
var
  DataDecoder : TCustomChannelDataCoder;
  Samples     : Cardinal;
begin
 inherited;

 // check for no samples to load
 if SampleFrames = 0 then Exit;

 with FStream do
  begin
   assert(FAudioDataPosition > 0);
   Position := FAudioDataPosition + 8 + SamplePosition;

   DataDecoder := CreateDataCoder;
   if not Assigned(DataDecoder) then exit;

   if Assigned(FOnBeginRead)
    then FOnBeginRead(Self);

   try
    Samples := 0;
    while Samples + DataDecoder.SampleFrames < SampleFrames do
     begin
      DataDecoder.LoadFromStream(FStream);
      if Assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);

      Samples := Samples + DataDecoder.SampleFrames;
     end;

     DataDecoder.SampleFrames := SampleFrames - Samples;
     DataDecoder.LoadFromStream(FStream);
     if Assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);
   finally
    FreeAndNil(DataDecoder);
   end;
  end;
end;

procedure TCustomAudioFileAIFF.Encode(SamplePosition, SampleFrames: Cardinal);
var
  DataEncoder : TCustomChannelDataCoder;
  Samples     : Cardinal;
begin
 inherited;

 // check for no samples to load
 if SampleFrames = 0 then Exit;

 with FStream do
  begin
   DataEncoder := CreateDataCoder;
   if not Assigned(DataEncoder) then exit;

   if EmptyData then
    begin
     FStream.Seek(0, soFromEnd);
     FAudioDataPosition := FStream.Position;
     WriteSSNDChunk(FStream);
    end;

   Position := FAudioDataPosition + 8 + DataEncoder.SampleToByte(SamplePosition);

   if Assigned(FOnBeginWrite)
    then FOnBeginWrite(Self);

   try
    Samples := 0;
    while Samples + DataEncoder.SampleFrames < SampleFrames do
     begin
      if Assigned(FOnEncode) then FOnEncode(Self, DataEncoder, Samples);
      DataEncoder.SaveToStream(FStream);

      Samples := Samples + DataEncoder.SampleFrames;
     end;

     DataEncoder.SampleFrames := SampleFrames - Samples;
     if Assigned(FOnEncode) then FOnEncode(Self, DataEncoder, Samples);
     DataEncoder.SaveToStream(FStream);
   finally
    FreeAndNil(DataEncoder);
   end;
  end;
end;

procedure TCustomAudioFileAIFF.ReadAudioDataFromStream(const Stream: TStream);
var
  Offset      : Integer;
  BlockAlign  : Integer;
  DataDecoder : TCustomChannelDataCoder;
  Samples     : Cardinal;
begin
 with Stream do
  begin
   Position := FAudioDataPosition;

   // read offset
   Read(Offset, 4);
   Flip32(Offset);

   // read block align (even if it is not used here)
   Read(BlockAlign, 4);
   Flip32(BlockAlign);

   // advance offset
   Position := Position + Offset;

   if Assigned(FOnBeginRead)
    then FOnBeginRead(Self);

   DataDecoder := CreateDataCoder;
   if not Assigned(DataDecoder) then exit;

   with DataDecoder do
    try
     Samples := 0;
     while Samples + SampleFrames <= FCommonChunk.SampleFrames do
      begin
       LoadFromStream(Stream);
       if Assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);
       Samples := Samples + SampleFrames;
      end;

      SampleFrames := FCommonChunk.SampleFrames - Samples;
      LoadFromStream(Stream);
      if Assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);
    finally
     FreeAndNil(DataDecoder);
    end;
  end;
end;

/////////////
//  write  //
/////////////

procedure TCustomAudioFileAIFF.WriteTotalSampleFrames(const Stream: TStream);
var
  ChunkSize   : Cardinal;
  OldPosition : Cardinal;
const
  CZero : Integer = 0;  
begin
 with Stream do
  begin
   OldPosition := Position;
   if not EmptyData then
    begin
     Position := FAudioDataPosition + 4;
     ChunkSize := 8 + DataSize;

     Flip32(ChunkSize);
     Write(ChunkSize, 4);

     Write(CZero, 4); // offset
     Write(CZero, 4); // block align
    end;

   // finally write filesize
   ChunkSize := Size - 8;
   Flip32(ChunkSize);
   Seek(4, soFromBeginning);
   Write(ChunkSize, 4);
   Position := OldPosition;
  end;
end;

procedure TCustomAudioFileAIFF.WriteSSNDChunk(const Stream: TStream);
var
  ChunkName   : TChunkName;
  ChunkSize   : Cardinal;
const
  CZero: Cardinal = 0;
begin
 with Stream do
  begin
   // write 'data' chunk name
   ChunkName := 'SSND';
   Write(ChunkName, 4);

   // write chunk size
   ChunkSize := 8 + DataSize;
   Flip32(ChunkSize);
   Write(ChunkSize, 4);

   Write(CZero, 4); // offset
   Write(CZero, 4); // block align
  end;
end;

procedure TCustomAudioFileAIFF.WriteAudioDataToStream(const Stream: TStream);
var
  ChunkEnd    : Cardinal;
  DataEncoder : TCustomChannelDataCoder;
  Samples     : Cardinal;
begin
 // check if samples are available
 if SampleFrames <= 0 then Exit;

 with Stream do
  begin
   FAudioDataPosition := Position;

   // write data chunk
   WriteSSNDChunk(Stream);

   // calculate chunk end (to ensure the above value is correct)
   ChunkEnd := Stream.Position + DataSize;

   DataEncoder := CreateDataCoder;
   if not Assigned(DataEncoder) then exit;

   if Assigned(FOnBeginWrite)
    then FOnBeginWrite(Self);

   with DataEncoder do
    try
     Samples   := 0;
     while Samples + SampleFrames < Self.SampleFrames do
      begin
       if Assigned(FOnEncode) then FOnEncode(Self, DataEncoder, Samples);
       SaveToStream(Stream);

       Samples := Samples + SampleFrames;
      end;

      SampleFrames := Self.SampleFrames - Samples;
      if Assigned(FOnEncode) then FOnEncode(Self, DataEncoder, Samples);
      SaveToStream(Stream);
     finally
     FreeAndNil(DataEncoder);
    end;

   Assert(Position = ChunkEnd);
   Position := ChunkEnd; 
  end;
end;

initialization
  RegisterFileFormat(TAudioFileAIFF);

end.
