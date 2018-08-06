unit DAV_AudioFileWAV;

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
  Classes, Contnrs, SysUtils, DAV_Types, DAV_AudioFile, DAV_WaveFileTypes,
  DAV_ChunkClasses, DAV_ChunkWaveBasic, DAV_ChunkWaveCustom,
  DAV_ChannelDataCoder;

type
  EWavError = class(Exception);

  TWaveChunkType = (ctFormat, ctFact, ctData);
  TWaveChunkTypes = set of TWaveChunkType;

  TCustomAudioFileWAV = class(TCustomAudioFile, IAudioFileBitsPerSample,
    IAudioFileEncoding)
  private
    FChunkSize         : Cardinal;
    FTotalSampleFrames : Cardinal;
    FFormatChunk       : TFormatChunk;
    FFactChunk         : TFactChunk;
    FBextChunk         : TBextChunk;
    FCartChunk         : TCartChunk;
    FChunkList         : TChunkList;
    FBytesPerSample    : Integer;
    FFormatChunkFound  : Boolean;
    function GetTitle: string;
    function GetArtist: string;
    function GetCategory: string;
    function GetClassification: string;
    function GetClientID: string;
    function GetCutID: string;
    function GetdbLevelReference: Integer;
    function GetEndDate: string;
    function GetEndTime: string;
    function GetOutCue: string;
    function GetProducerAppID: string;
    function GetProducerAppVersion: string;
    function GetStartDate: string;
    function GetStartTime: string;
    function GetUserDef: string;
    function GetCartVersion: Integer;
    function GetBextVersion: Integer;
    function GetBextDescription: string;
    function GetOriginationDate: string;
    function GetOriginationTime: string;
    function GetOriginator: string;
    function GetOriginatorRef: string;
    function GetTimeRefHigh: Integer;
    function GetTimeRefLow: Integer;
    procedure ReadAudioDataFromStream(const Stream: TStream);
    procedure WriteAudioDataToStream(const Stream: TStream);
    procedure SetTitle(const Value: string);
    procedure SetArtist(const Value: string);
    procedure SetCategory(const Value: string);
    procedure SetClassification(const Value: string);
    procedure SetClientID(const Value: string);
    procedure SetCutID(const Value: string);
    procedure SetdbLevelReference(const Value: Integer);
    procedure SetEndDate(const Value: string);
    procedure SetEndTime(const Value: string);
    procedure SetOutCue(const Value: string);
    procedure SetProducerAppID(const Value: string);
    procedure SetProducerAppVersion(const Value: string);
    procedure SetStartDate(const Value: string);
    procedure SetStartTime(const Value: string);
    procedure SetUserDef(const Value: string);
    procedure SetBextVersion(const Value: Integer);
    procedure SetCartVersion(const Value: Integer);
    procedure SetBextDescription(const Value: string);
    procedure SetOriginationDate(const Value: string);
    procedure SetOriginationTime(const Value: string);
    procedure SetOriginator(const Value: string);
    procedure SetOriginatorRef(const Value: string);
    procedure SetTimeRefHigh(const Value: Integer);
    procedure SetTimeRefLow(const Value: Integer);
    function GetEmptyData: Boolean;
    function GetSubChunk(Index: Integer): TCustomChunk;
    function GetSubChunkCount: Cardinal;
    function GetTypicalAudioDataPosition: Cardinal;
  protected
    FAudioDataPosition : Cardinal;
    function GetBitsPerSample: Byte; virtual;
    function GetEncoding: TAudioEncoding; virtual;
    function GetChannels: Cardinal; override;
    function GetSampleRate: Double; override;
    function GetSampleFrames: Cardinal; override;
    function GetDataSize: Cardinal;

    procedure SetBitsPerSample(const Value: Byte); virtual;
    procedure SetEncoding(const Value: TAudioEncoding); virtual;
    procedure SetChannels(const Value: Cardinal); override;
    procedure SetSampleRate(const Value: Double); override;
    procedure SetSampleFrames(const Value: Cardinal); override;

    procedure CheckCartChunkEmpty; virtual;
    procedure CheckBextChunkEmpty; virtual;
    procedure CheckCreateBextChunk; virtual;
    procedure CheckCreateCartChunk; virtual;
    procedure SampleFramesChanged; virtual;
    procedure CheckHeader(const Stream: TStream); override;
    procedure ParseStream(const Stream: TStream); override;

    function CreateDataCoder: TCustomChannelDataCoder;
    procedure ReadFactChunk(const Stream: TStream);
    procedure ReadFormatChunk(const Stream: TStream);
    procedure ReadDataChunk(const Stream: TStream);
    procedure ReadBextChunk(const Stream: TStream);
    procedure ReadCartChunk(const Stream: TStream);
    procedure ReadUnknownChunk(const Stream: TStream; const ChunkName: TChunkName);

    procedure WriteBasicChunks(const Stream: TStream);
    procedure WriteAdditionalChunks(const Stream: TStream);
    procedure WriteDataChunk(const Stream: TStream);
    procedure WriteFormatChunk(const Stream: TStream);
    procedure WriteTotalSampleFrames(const Stream: TStream);

    property EmptyData: Boolean read GetEmptyData;
    property TypicalAudioDataPosition: Cardinal read GetTypicalAudioDataPosition;
    property BextChunk: TBextChunk read FBextChunk;
    property CartChunk: TCartChunk read FCartChunk;
  public
    constructor Create; override;
    destructor Destroy; override;

    // load/save stream
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    // decode/encode
    procedure Decode(SamplePosition: Cardinal; SampleFrames: Cardinal); override;
    procedure Encode(SamplePosition: Cardinal; SampleFrames: Cardinal); override;

    // sub chunks
    procedure AddSubChunk(SubChunk: TCustomChunk); virtual;
    procedure DeleteSubChunk(SubChunk: TCustomChunk); overload; virtual;
    procedure DeleteSubChunk(const Index: Integer); overload; virtual;

    // file format identifier
    class function DefaultExtension: string; override;
    class function Description: string; override;
    class function FileFormatFilter: string; override;
    class function CanLoad(const Stream: TStream): Boolean; override;

    property BitsPerSample: Byte read GetBitsPerSample write SetBitsPerSample;
    property BytesPerSample: Integer read FBytesPerSample;
    property Encoding: TAudioEncoding read GetEncoding write SetEncoding;
    property DataSize: Cardinal read GetDataSize;

    // sub chunks
    property SubChunkCount: Cardinal read GetSubChunkCount;
    property SubChunk[Index: Integer]: TCustomChunk read GetSubChunk;

    // from CART chunk
    property CartVersion: Integer read GetCartVersion write SetCartVersion;
    property Title: string read GetTitle write SetTitle;
    property Artist: string read GetArtist write SetArtist;
    property CutID: string read GetCutID write SetCutID;
    property ClientID: string read GetClientID write SetClientID;
    property Category: string read GetCategory write SetCategory;
    property Classification: string read GetClassification write SetClassification;
    property OutCue: string read GetOutCue write SetOutCue;
    property StartDate: string read GetStartDate write SetStartDate;
    property StartTime: string read GetStartTime write SetStartTime;
    property EndDate: string read GetEndDate write SetEndDate;
    property EndTime: string read GetEndTime write SetEndTime;
    property ProducerAppID: string read GetProducerAppID write SetProducerAppID;
    property ProducerAppVersion: string read GetProducerAppVersion write SetProducerAppVersion;
    property UserDef: string read GetUserDef write SetUserDef;
    property dbLevelReference: Integer read GetdbLevelReference write SetdbLevelReference;

    // from BEXT chunk
    property BextVersion: Integer read GetBextVersion write SetBextVersion;
    property BextDescription: string read GetBextDescription write SetBextDescription;
    property Originator: string read GetOriginator write SetOriginator;
    property OriginatorRef: string read GetOriginatorRef write SetOriginatorRef;
    property OriginationDate: string read GetOriginationDate write SetOriginationDate;
    property OriginationTime: string read GetOriginationTime write SetOriginationTime;
    property TimeRefLow: Integer read GetTimeRefLow write SetTimeRefLow;
    property TimeRefHigh: Integer read GetTimeRefHigh write SetTimeRefHigh;
  end;

  TAudioFileWAV  = class(TCustomAudioFileWAV)
  published
    property SampleRate;
    property ChannelCount;
    property SampleFrames;
    property TotalTime;
    property BitsPerSample;
    property BytesPerSample;
    property Encoding;

    property Title;
  end;

implementation

uses
  DAV_Common;

resourcestring
  RCRIFFChunkNotFound  = 'This is not a RIFF file!';
  RCRIFFSizeMismatch   = 'Filesize mismatch';
  RCWAVEChunkNotFound  = 'This is not a WAVE file!';
  RCFMTChunkDublicate  = 'More than one format chunk found!';
  RCFACTChunkDublicate = 'More than one fact chunk found!';
  RCDATAChunkDublicate = 'Only one data chunk supported!';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrCantChangeTheFormat = 'Can''t change the format!';
  RCStrNoDataChunkFound = 'No data chunk found!';

{ TCustomAudioFileWAV }

constructor TCustomAudioFileWAV.Create;
begin
 inherited;
 FChunkList := TChunkList.Create;
 FAudioDataPosition := 0;
 FTotalSampleFrames := 0;
 FBytesPerSample := 3; // 24 bit
 FFormatChunk := TFormatChunk.Create;
end;

destructor TCustomAudioFileWAV.Destroy;
begin
 // make sure a data chunk is written and the file is valid
 if False and Assigned(FStream) then
  begin
   if EmptyData then
    begin
     FAudioDataPosition := TypicalAudioDataPosition;
     FStream.Position := FAudioDataPosition;
     WriteDataChunk(FStream);
     WriteAdditionalChunks(FStream);
    end;
   WriteTotalSampleFrames(FStream);
  end;

 if Assigned(FFactChunk)
  then FreeAndNil(FFactChunk);
 if Assigned(FBextChunk)
  then FreeAndNil(FBextChunk);
 if Assigned(FCartChunk)
  then FreeAndNil(FCartChunk);
 FreeAndNil(FFormatChunk);
 FreeAndNil(FChunkList);

 inherited;
end;

class function TCustomAudioFileWAV.DefaultExtension: string;
begin
 Result := '.wav';
end;

procedure TCustomAudioFileWAV.DeleteSubChunk(SubChunk: TCustomChunk);
var
  i : Integer;
begin
 i := 0;
 while i < FChunkList.Count do
  if FChunkList[i] = SubChunk
   then FChunkList.Delete(i)
   else Inc(i);
end;

procedure TCustomAudioFileWAV.DeleteSubChunk(const Index: Integer);
begin
 if (Index >= 0) and (Index < FChunkList.Count)
  then FChunkList.Delete(Index)
  else raise EWavError.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

class function TCustomAudioFileWAV.Description: string;
begin
 Result := 'Microsoft RIFF WAVE';
end;

class function TCustomAudioFileWAV.FileFormatFilter: string;
begin
 Result := Description + ' (*.' + DefaultExtension + ')|*.wav*'
end;

procedure TCustomAudioFileWAV.AddSubChunk(SubChunk: TCustomChunk);
begin
 FChunkList.Add(SubChunk);
end;

class function TCustomAudioFileWAV.CanLoad(const Stream: TStream): Boolean;
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
   // check whether file is a resource interchange file format ('RIFF')
   Read(ChunkName, 4);
   if ChunkName <> 'RIFF' then exit;

   // check whether the real file size match the filesize stored inside the RIFF chunk
   Read(ChunkSize, 4);
   if (ChunkSize > Size - Position) and not (ChunkSize = $FFFFFFFF) then exit;

   // now specify the RIFF file to be a WAVE file
   Read(ChunkName, 4);
   if ChunkName <> 'WAVE' then exit;

   Result := True;
  finally
   // restore old position
   Position := OldPosition;
  end;
end;

function TCustomAudioFileWAV.GetBextDescription: string;
begin
 if Assigned(FBextChunk)
  then Result := FBextChunk.Description
  else Result := '';
end;

function TCustomAudioFileWAV.GetCategory: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.Category
  else Result := '';
end;

function TCustomAudioFileWAV.GetChannels: Cardinal;
begin
 Result := FFormatChunk.Channels;
end;

function TCustomAudioFileWAV.GetClassification: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.Classification
  else Result := '';
end;

function TCustomAudioFileWAV.GetClientID: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.ClientID
  else Result := '';
end;

function TCustomAudioFileWAV.GetCutID: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.CutID
  else Result := '';
end;

function TCustomAudioFileWAV.GetdbLevelReference: Integer;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.dbLevelReference
  else Result := 0;
end;

function TCustomAudioFileWAV.GetSampleFrames: Cardinal;
begin
 Result := FTotalSampleFrames;
end;

function TCustomAudioFileWAV.GetSampleRate: Double;
begin
 Result := FFormatChunk.SampleRate;
end;

function TCustomAudioFileWAV.GetStartDate: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.StartDate
  else Result := '';
end;

function TCustomAudioFileWAV.GetStartTime: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.StartTime
  else Result := '';
end;

function TCustomAudioFileWAV.GetSubChunk(Index: Integer): TCustomChunk;
begin
 if (Index >= 0) and (Index < FChunkList.Count)
  then Result := FChunkList[Index]
  else raise EWavError.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TCustomAudioFileWAV.GetSubChunkCount: Cardinal;
begin
 Result := FChunkList.Count;
end;

function TCustomAudioFileWAV.GetTimeRefHigh: Integer;
begin
 if Assigned(FBextChunk)
  then Result := FBextChunk.TimeRefHigh
  else Result := 0;
end;

function TCustomAudioFileWAV.GetTimeRefLow: Integer;
begin
 if Assigned(FBextChunk)
  then Result := FBextChunk.TimeRefLow
  else Result := 0;
end;

function TCustomAudioFileWAV.GetTitle: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.Title
  else Result := '';
end;

function TCustomAudioFileWAV.GetTypicalAudioDataPosition: Cardinal;
begin
 Result := 12 + SizeOf(TChunkName) + SizeOf(Integer) + FFormatChunk.ChunkSize;
 if Assigned(FFactChunk)
  then Result := Result + SizeOf(TChunkName) + SizeOf(Integer) + FFactChunk.ChunkSize;
end;

function TCustomAudioFileWAV.GetArtist: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.Artist
  else Result := '';
end;

function TCustomAudioFileWAV.GetUserDef: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.UserDef
  else Result := '';
end;

function TCustomAudioFileWAV.GetBextVersion: Integer;
begin
 if Assigned(FBextChunk)
  then Result := FBextChunk.Version
  else Result := 0;
end;

function TCustomAudioFileWAV.GetCartVersion: Integer;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.Version
  else Result := 0;
end;

function TCustomAudioFileWAV.GetBitsPerSample: Byte;
begin
 Result := FFormatChunk.BitsPerSample;
end;

function TCustomAudioFileWAV.GetDataSize: Cardinal;
begin
 Result := FFormatChunk.BlockAlign * SampleFrames;
end;

function TCustomAudioFileWAV.GetEmptyData: Boolean;
begin
 Result := FAudioDataPosition = 0;
end;

function TCustomAudioFileWAV.GetEncoding: TAudioEncoding;
begin
 case FFormatChunk.FormatTag of
             etPCM : Result := aeInteger;
        etPCMFLOAT : Result := aeFloat;
         etMSADPCM : Result := aeMSADPCM;
        etDVIADPCM : Result := aeDVIADPCM;
//  etACM, etACMMPEG : Result := aeACM;
              else   Result := aeOther;
 end;
end;

function TCustomAudioFileWAV.GetEndDate: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.EndDate
  else Result := '';
end;

function TCustomAudioFileWAV.GetEndTime: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.EndTime
  else Result := '';
end;

function TCustomAudioFileWAV.GetOriginationDate: string;
begin
 if Assigned(FBextChunk)
  then Result := FBextChunk.OriginationDate
  else Result := '';
end;

function TCustomAudioFileWAV.GetOriginationTime: string;
begin
 if Assigned(FBextChunk)
  then Result := FBextChunk.OriginationTime
  else Result := '';
end;

function TCustomAudioFileWAV.GetOriginator: string;
begin
 if Assigned(FBextChunk)
  then Result := FBextChunk.Originator
  else Result := '';
end;

function TCustomAudioFileWAV.GetOriginatorRef: string;
begin
 if Assigned(FBextChunk)
  then Result := FBextChunk.OriginatorRef
  else Result := '';
end;

function TCustomAudioFileWAV.GetOutCue: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.OutCue
  else Result := '';
end;

function TCustomAudioFileWAV.GetProducerAppID: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.ProducerAppID
  else Result := '';
end;

function TCustomAudioFileWAV.GetProducerAppVersion: string;
begin
 if Assigned(FCartChunk)
  then Result := FCartChunk.ProducerAppVersion
  else Result := '';
end;

procedure TCustomAudioFileWAV.SetArtist(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.Artist := Value;
  end
 else
  begin
   FCartChunk.Artist := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetBitsPerSample(const Value: Byte);
begin
 // Assert stream is empty
 if Assigned(FStream) and not EmptyData
  then raise Exception.Create(RCStrCantChangeTheFormat);

 with FFormatChunk do
  if BitsPerSample <> Value then
   begin
    BitsPerSample   := Value;
    FBytesPerSample := (BitsPerSample + 7) div 8;
    BlockAlign      := Channels * FBytesPerSample;
    BytesPerSecond  := BlockAlign * SampleRate;
//    BitsPerSampleChanged;
   end;

 // if empty stream is assigned update format chunk
 if Assigned(FStream) and EmptyData then
  begin
   FStream.Position := 12;
   WriteFormatChunk(FStream);
  end;
end;

procedure TCustomAudioFileWAV.SetCategory(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.Category := Value;
  end
 else
  begin
   FCartChunk.Category := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetChannels(const Value: Cardinal);
var
  WordValue : Word;
begin
 WordValue := Word(IntLimit(Value, 0, 65536));

 // Assert stream is empty
 if Assigned(FStream) and not EmptyData
  then raise Exception.Create(RCStrCantChangeTheFormat);

 inherited;

 with FFormatChunk do
  if Channels <> Value then
   begin
    Channels       := WordValue;
    BlockAlign     := Word(FBytesPerSample * WordValue);
    BytesPerSecond := BlockAlign * SampleRate;
   end;

 // if empty stream is assigned update format chunk
 if Assigned(FStream) and EmptyData then
  begin
   FStream.Position := 12;
   WriteFormatChunk(FStream);
  end;
end;

procedure TCustomAudioFileWAV.SetClassification(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.Classification := Value;
  end
 else
  begin
   FCartChunk.Classification := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetClientID(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.ClientID := Value;
  end
 else
  begin
   FCartChunk.ClientID := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetCutID(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.CutID := Value;
  end
 else
  begin
   FCartChunk.CutID := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetdbLevelReference(const Value: Integer);
begin
 if Value <> 0 then
  begin
   CheckCreateCartChunk;
   FCartChunk.dbLevelReference := Value;
  end
 else
  begin
   FCartChunk.dbLevelReference := 0;
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetBextDescription(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FBextChunk.Description := Value;
  end
 else
  begin
   FBextChunk.Description := '';
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetEncoding(const Value: TAudioEncoding);
begin
 // Assert stream is empty
 if Assigned(FStream) and not EmptyData
  then raise Exception.Create(RCStrCantChangeTheFormat);

 case Value of
   aeInteger : FFormatChunk.FormatTag := etPCM;
     aeFloat : begin
                FFormatChunk.FormatTag := etPCMFLOAT;
                BitsPerSample := 32;
               end;
   aeMSADPCM : FFormatChunk.FormatTag := etMSADPCM;
  aeDVIADPCM : FFormatChunk.FormatTag := etDVIADPCM;
      aeALaw : begin
                FFormatChunk.FormatTag := etALaw;
                BitsPerSample := 8;
               end;
     aeMuLaw : begin
                FFormatChunk.FormatTag := etMuLaw;
                BitsPerSample := 8;
               end;
  else raise Exception.Create('Not yet implemented');
 end;

 // if empty stream is assigned update format chunk
 if Assigned(FStream) and EmptyData then
  begin
   FStream.Position := 12;
   WriteFormatChunk(FStream);
  end;
end;

procedure TCustomAudioFileWAV.SetEndDate(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.EndTime := Value;
  end
 else
  begin
   FCartChunk.EndTime := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetEndTime(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.EndTime := Value;
  end
 else
  begin
   FCartChunk.EndTime := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetOriginationDate(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FBextChunk.OriginationDate := Value;
  end
 else
  begin
   FBextChunk.OriginationDate := '';
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetOriginationTime(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FBextChunk.OriginationTime := Value;
  end
 else
  begin
   FBextChunk.OriginationTime := '';
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetOriginator(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FBextChunk.Originator := Value;
  end
 else
  begin
   FBextChunk.Originator := '';
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetOriginatorRef(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FBextChunk.OriginatorRef := Value;
  end
 else
  begin
   FBextChunk.OriginatorRef := '';
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetOutCue(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.OutCue := Value;
  end
 else
  begin
   FCartChunk.OutCue := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetProducerAppID(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.ProducerAppID := Value;
  end
 else
  begin
   FCartChunk.ProducerAppID := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetProducerAppVersion(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.ProducerAppVersion := Value;
  end
 else
  begin
   FCartChunk.ProducerAppVersion := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetSampleFrames(const Value: Cardinal);
begin
 if FTotalSampleFrames <> Value then
  begin
   inherited;
   FTotalSampleFrames := Value;
   SampleFramesChanged;
  end;
end;

procedure TCustomAudioFileWAV.SampleFramesChanged;
begin
 if Assigned(FFactChunk)
  then FFactChunk.SampleCount := FTotalSampleFrames;
 if Assigned(FStream)
  then WriteTotalSampleFrames(FStream);
end;

procedure TCustomAudioFileWAV.SetSampleRate(const Value: Double);
begin
 // Assert stream is empty
 if Assigned(FStream) and not EmptyData
  then raise Exception.Create(RCStrCantChangeTheFormat);

 inherited;
 with FFormatChunk do
  if SampleRate <> Value then
   begin
    SampleRate := Round(Value);
    BytesPerSecond := BlockAlign * SampleRate;
   end;

 // if empty stream is assigned update format chunk
 if Assigned(FStream) and EmptyData then
  begin
   FStream.Position := 12;
   WriteFormatChunk(FStream);
  end;
end;

procedure TCustomAudioFileWAV.SetStartDate(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.StartDate := Value;
  end
 else
  begin
   FCartChunk.StartDate := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetStartTime(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.StartTime := Value;
  end
 else
  begin
   FCartChunk.StartTime := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetTimeRefHigh(const Value: Integer);
begin
 if Value <> 0 then
  begin
   CheckCreateCartChunk;
   FBextChunk.TimeRefHigh := Value;
  end
 else
  begin
   FBextChunk.TimeRefHigh := 0;
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetTimeRefLow(const Value: Integer);
begin
 if Value <> 0 then
  begin
   CheckCreateCartChunk;
   FBextChunk.TimeRefLow := Value;
  end
 else
  begin
   FBextChunk.TimeRefLow := 0;
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetTitle(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.Title := Value;
  end
 else
  begin
   FCartChunk.Title := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetUserDef(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.UserDef := Value;
  end
 else
  begin
   FCartChunk.UserDef := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetBextVersion(const Value: Integer);
begin
 if Value <> 0 then
  begin
   CheckCreateBextChunk;
   FBextChunk.Version := Value;
  end
 else
  begin
   FBextChunk.Version := 0;
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetCartVersion(const Value: Integer);
begin
 if Value <> 0 then
  begin
   CheckCreateCartChunk;
   FCartChunk.Version := Value;
  end
 else
  begin
   FCartChunk.Version := 0;
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.CheckCartChunkEmpty;
begin
 // todo: not yet implemented!
end;

procedure TCustomAudioFileWAV.CheckBextChunkEmpty;
begin
 with FBextChunk do
  if (Description = '') and (Originator = '') and (OriginatorRef = '') and
    (OriginationDate = '') and (OriginationTime = '') and (TimeRefLow = 0) and
    (TimeRefHigh = 0) and (Version = 0) then
   begin
    FreeAndNil(FBextChunk);
   end;
end;

procedure TCustomAudioFileWAV.CheckCreateCartChunk;
begin
 // eventually create cart chunk
 if not Assigned(FCartChunk)
  then FCartChunk := TCartChunk.Create;
end;

procedure TCustomAudioFileWAV.CheckCreateBextChunk;
begin
 // eventually create bext chunk
 if not Assigned(FBextChunk)
  then FBextChunk := TBextChunk.Create;
end;

procedure TCustomAudioFileWAV.CheckHeader(const Stream: TStream);
var
  ChunkName : TChunkName;
begin
 with Stream do
  begin
   // check whether file is a resource interchange file format ('RIFF')
   Read(ChunkName, 4);
   if ChunkName <> 'RIFF'
    then raise EWavError.Create(rcRIFFChunkNotFound);

   // check whether the real file size match the filesize stored inside the RIFF chunk
   Read(FChunkSize, 4);
   if (FChunkSize > Size - Position) and not (FChunkSize = $FFFFFFFF)
    then raise EWavError.Create(rcRIFFSizeMismatch);

   // now specify the RIFF file to be a WAVE file
   Read(ChunkName, 4);
   if ChunkName <> 'WAVE'
    then raise EWavError.Create(rcWAVEChunkNotFound);
  end;
end;

procedure TCustomAudioFileWAV.ParseStream(const Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkEnd     : Cardinal;
begin
 with Stream do
  begin
   // clear all chunks
   if Assigned(FFactChunk) then FreeAndNil(FFactChunk);
   if Assigned(FCartChunk) then FreeAndNil(FCartChunk);
   if Assigned(FBextChunk) then FreeAndNil(FBextChunk);
   FChunkList.Clear;

   // reset current data positions
   FAudioDataPosition := 0;

   // reset FormatChunkFound
   FFormatChunkFound := False;

   // start parsing here
   ChunkEnd := Position + FChunkSize - 4;
   while Position < ChunkEnd do
    begin
     // read chunk name
     Read(ChunkName, 4);

     // read chunk position
     Position := Position - 4;

     if ChunkName = 'fmt ' then ReadFormatChunk(Stream) else
     if ChunkName = 'fact' then ReadFactChunk(Stream) else
     if ChunkName = 'cart' then ReadCartChunk(Stream) else
     if ChunkName = 'bext' then ReadBextChunk(Stream) else
     if ChunkName = 'data' then ReadDataChunk(Stream)
      else ReadUnknownChunk(Stream, ChunkName);
    end;

   Assert(Position = ChunkEnd);

   if (FAudioDataPosition = 0)
    then raise EWavError.Create(RCStrNoDataChunkFound)
  end;
end;

procedure TCustomAudioFileWAV.ReadFormatChunk(const Stream: TStream);
begin
 with Stream do
  begin
   // check whether format chunk has already been created
   if FFormatChunkFound
    then raise Exception.Create(RCFACTChunkDublicate);

   // load format chunk
   FFormatChunk.LoadFromStream(Stream);
   FFormatChunkFound := True;
  end;
end;

procedure TCustomAudioFileWAV.ReadFactChunk(const Stream: TStream);
begin
 with Stream do
  begin
   // check whether fact chunk has already been created
   if Assigned(FFactChunk)
    then raise Exception.Create(RCFACTChunkDublicate);

   FFactChunk := TFactChunk.Create;
   with FFactChunk do
    begin
     // now load fact chunk
     LoadFromStream(Stream);

     // now only use the sample count information
     if SampleCount < Stream.Size
      then FTotalSampleFrames := SampleCount;
    end;
  end;
end;

procedure TCustomAudioFileWAV.ReadDataChunk(const Stream: TStream);
var
  DataSize      : Cardinal;
  ChunksReaded  : TWaveChunkTypes;
begin
 with Stream do
  if ctData in ChunksReaded
   then raise EWavError.Create(rcDATAChunkDublicate)
   else
    begin
     // store data chunk position
     FAudioDataPosition := Position;

     // skip chunk name
     Position := Position + 4;

     // read data size
     Read(DataSize, 4);

     // eventually set total number of samples
     if not Assigned(FFactChunk)
      then FTotalSampleFrames := DataSize div FFormatChunk.BlockAlign
      else
     if FFormatChunk.FormatTag <> etPcm
      then FTotalSampleFrames := FFactChunk.SampleCount;

     Position := Position + DataSize;

     // make all chunks word aligned!
     // Quote: "The sample data must end on an even byte boundary"
     Position := Position + ((Position - FAudioDataPosition) and $1);
    end
end;

procedure TCustomAudioFileWAV.ReadBextChunk(const Stream: TStream);
begin
 with Stream do
  begin
   CheckCreateBextChunk;
   FBextChunk.LoadFromStream(Stream);
  end;
end;

procedure TCustomAudioFileWAV.ReadCartChunk(const Stream: TStream);
begin
 with Stream do
  begin
   CheckCreateCartChunk;
   FCartChunk.LoadFromStream(Stream);
  end;
end;

procedure TCustomAudioFileWAV.ReadUnknownChunk(const Stream: TStream;
  const ChunkName: TChunkName);
var
  ChunkClass   : TDefinedChunkClass;
  DefinedChunk : TDefinedChunk;
begin
 ChunkClass := WaveChunkClassByChunkName(ChunkName);
 if Assigned(ChunkClass) then
  begin
   DefinedChunk := ChunkClass.Create;
   DefinedChunk.LoadFromStream(Stream);
   FChunkList.Add(DefinedChunk);
  end
 else
  with TWavUnknownChunk.Create do
   try
    LoadFromStream(Stream);
   finally
    Free;
   end
end;


(*
procedure TCustomAudioFileWAV.ReadSDA8Chunk(const Stream: TStream);
begin
 with Stream, TWavSDA8Chunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;
*)

procedure TCustomAudioFileWAV.WriteFormatChunk(const Stream: TStream);
begin
 FFormatChunk.SaveToStream(Stream);
end;

procedure TCustomAudioFileWAV.WriteDataChunk(const Stream: TStream);
var
  ChunkName   : TChunkName;
  ChunkSize   : Cardinal;
begin
 with Stream do
  begin
   // write 'data' chunk name
   ChunkName := 'data';
   Write(ChunkName, 4);

   // write chunk size
   ChunkSize := DataSize;
   Write(ChunkSize, 4);
  end;
end;


// Load/Save

procedure TCustomAudioFileWAV.LoadFromStream(Stream: TStream);
begin
 inherited;
 CheckHeader(Stream);
 ParseStream(Stream);

 ReadAudioDataFromStream(Stream);
end;

procedure TCustomAudioFileWAV.SaveToStream(Stream: TStream);
var
  ChunkName  : TChunkName;
  ChunkStart : Cardinal;
  ChunkSize  : Cardinal;
begin
 inherited;
 with Stream do
  begin
   // Store chunk start position, just in case the stream position is not 0;
   ChunkStart := Position;

   // first write 'RIFF' (resource interchange file format)
   ChunkName := 'RIFF';
   Write(ChunkName, 4);

   // write dummy filesize yet, since final size is still unknown
   ChunkSize := $FFFFFFFF;
   Write(ChunkSize, 4);

   // now specify the RIFF file to be a WAVE file
   ChunkName := 'WAVE';
   Write(ChunkName, 4);

   // write basic chunks
   WriteBasicChunks(Stream);

   // write additional chunks
   WriteAdditionalChunks(Stream);

   // finally write filesize
   ChunkSize := Position - (ChunkStart + 8);
   Position  := ChunkStart + 4;
   Write(ChunkSize, 4);

   // Reset Position to end of Stream;
   Position := ChunkStart + ChunkSize;
  end;
end;

procedure TCustomAudioFileWAV.WriteBasicChunks(const Stream: TStream);
begin
 with Stream do
  begin
   // write format chunk
   WriteFormatChunk(Stream);

   // check whether a fact chunk is necessary
   if (FFormatChunk.FormatTag <> etPCM) then
    begin
     // if no fact chunk has been found, create it
     if not Assigned(FFactChunk)
      then FFactChunk := TFactChunk.Create;

     // store total number of samples to fact
     FFactChunk.SampleCount := FTotalSampleFrames;
     FFactChunk.SaveToStream(Stream);
    end;

   // ToDo: write data here!
   WriteAudioDataToStream(Stream);
  end;
end;

procedure TCustomAudioFileWAV.WriteAdditionalChunks(const Stream: TStream);
var
  SubChunk : Integer;
begin
 with Stream do
  begin
   // write cart chunk if available
   if Assigned(FCartChunk)
    then FCartChunk.SaveToStream(Stream);

   // write bext chunk if available
   if Assigned(FBextChunk)
    then FBextChunk.SaveToStream(Stream);

   // write subchunks
   if Assigned(FChunkList) and (FChunkList.Count > 0) then
    for SubChunk := 0 to FChunkList.Count - 1
     do FChunkList[SubChunk].SaveToStream(Stream);
  end;
end;

procedure TCustomAudioFileWAV.WriteTotalSampleFrames(const Stream: TStream);
var
  ChunkSize   : Cardinal;
  OldPosition : Cardinal;
begin
 with Stream do
  begin
   OldPosition := Position;
   if not EmptyData then
    begin
     Position := FAudioDataPosition + 4;
     ChunkSize := DataSize;
     Write(ChunkSize, 4);
    end;

   // finally write filesize
   ChunkSize := Size - 8;
   Seek(4, soFromBeginning);
   Write(ChunkSize, 4);
   Position := OldPosition;
  end;
end;

function TCustomAudioFileWAV.CreateDataCoder: TCustomChannelDataCoder;
begin
 case FFormatChunk.FormatTag of
  etPcm:
   begin
    Result := TChannel32DataCoderFixedPoint.Create;
    with TChannel32DataCoderFixedPoint(Result), FFormatChunk
     do SetBitsAndSampleSize(ValidBitsPerSample, BlockAlign div Channels);
   end;
  etPcmFloat:
    case FFormatChunk.BlockAlign div FFormatChunk.Channels of
      2 : Result := TChannel32DataCoderFloat16.Create;
      4 : Result := TChannel32DataCoderFloat32.Create;
      8 : Result := TChannel32DataCoderFloat64.Create;
     else Result := nil
    end;
  etALaw: Result := TChannel32DataCoderALaw.Create;
  etMuLaw: Result := TChannel32DataCoderMuLaw.Create;
  etExtended:
    // assuming these encodings is plain wrong here!!!
    case FFormatChunk.BlockAlign div FFormatChunk.Channels of
      1, 2, 3 :
        begin
          Result := TChannel32DataCoderFixedPoint.Create;
          with TChannel32DataCoderFixedPoint(Result), FFormatChunk
            do SetBitsAndSampleSize(ValidBitsPerSample, BlockAlign div Channels);
        end;
      4 : Result := TChannel32DataCoderFloat32.Create;
      8 : Result := TChannel32DataCoderFloat64.Create;
     else Result := nil
    end;
  else Result := nil;
 end;

 // set blocksize
 if Assigned(Result) then
  with Result do
   begin
    if Self.FBlockSize > 0
     then BlockSize := Self.FBlockSize;
    ChannelCount := FFormatChunk.Channels;
   end;
end;

procedure TCustomAudioFileWAV.Decode(SamplePosition, SampleFrames: Cardinal);
var
  DataDecoder : TCustomChannelDataCoder;
  Samples     : Cardinal;
begin
 inherited;

 // check for no samples to load
 if SampleFrames = 0 then Exit;

 with FStream do
  begin
   DataDecoder := CreateDataCoder;
   if not Assigned(DataDecoder) then exit;
   if FBlockSize <= 0
    then DataDecoder.SampleFrames := SampleFrames;

   Assert(FAudioDataPosition > 0);
   Position := FAudioDataPosition + 8 + DataDecoder.SampleToByte(SamplePosition);

   if Assigned(FOnBeginRead)
    then FOnBeginRead(Self);

   try
    Samples := SamplePosition;
    while Samples - SamplePosition + DataDecoder.SampleFrames < SampleFrames do
     begin
      DataDecoder.LoadFromStream(FStream);
      if Assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);

      Samples := Samples + DataDecoder.SampleFrames;
     end;

     DataDecoder.SampleFrames := SampleFrames - Samples + SamplePosition;
     DataDecoder.LoadFromStream(FStream);
     if Assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);
   finally
    FreeAndNil(DataDecoder);
   end;
  end;
end;

procedure TCustomAudioFileWAV.Encode(SamplePosition, SampleFrames: Cardinal);
var
  DataEncoder  : TCustomChannelDataCoder;
  Samples, Pos : Cardinal;
begin
 inherited;

 // check for no samples to load
 if SampleFrames = 0 then Exit;

 with FStream do
  begin
   DataEncoder := CreateDataCoder;
   if not Assigned(DataEncoder) then exit;
   if FBlockSize <= 0
    then DataEncoder.SampleFrames := SampleFrames;

   if EmptyData then
    begin
     FAudioDataPosition := TypicalAudioDataPosition;
     FStream.Position := FAudioDataPosition;
     WriteDataChunk(FStream);
    end;

   Position := FAudioDataPosition + 8 + DataEncoder.SampleToByte(SamplePosition);

   if Assigned(FOnBeginWrite)
    then FOnBeginWrite(Self);

   try
    Samples := 0;
    Pos := SamplePosition;
    while Samples + DataEncoder.SampleFrames < SampleFrames do
     begin
      if Assigned(FOnEncode) then FOnEncode(Self, DataEncoder, Samples);
      DataEncoder.SaveToStream(FStream);

      Samples := Samples + DataEncoder.SampleFrames;
      Pos := Pos + DataEncoder.SampleFrames;
     end;

     DataEncoder.SampleFrames := SampleFrames - Samples;
     if Assigned(FOnEncode) then FOnEncode(Self, DataEncoder, Pos);
     DataEncoder.SaveToStream(FStream);
   finally
    FreeAndNil(DataEncoder);
   end;
  end;
end;

procedure TCustomAudioFileWAV.ReadAudioDataFromStream(const Stream: TStream);
var
  ChunkName   : TChunkName;
  ChunkSize   : Cardinal;
  DataDecoder : TCustomChannelDataCoder;
  Samples     : Cardinal;
begin
 // check for no samples to load
 if SampleFrames = 0 then Exit;

 with Stream do
  begin
   Assert(FAudioDataPosition > 0);
   Position := FAudioDataPosition;

   Read(ChunkName, 4);
   Assert(ChunkName = 'data');

   Read(ChunkSize, 4);

   DataDecoder := CreateDataCoder;
   if not Assigned(DataDecoder) then Exit;
   if FBlockSize <= 0
    then DataDecoder.SampleFrames := SampleFrames;

   if Assigned(FOnBeginRead)
    then FOnBeginRead(Self);

   with DataDecoder do
    try
     Samples := 0;
     while Samples + SampleFrames < Self.SampleFrames do
      begin
       LoadFromStream(Stream);
       if Assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);

       Samples := Samples + SampleFrames;
      end;

      SampleFrames := Self.SampleFrames - Samples;
      LoadFromStream(Stream);
      if Assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);
    finally
     FreeAndNil(DataDecoder);
    end;
   Assert((Stream.Position - FAudioDataPosition - 8) <= ChunkSize);
  end;
end;

procedure TCustomAudioFileWAV.WriteAudioDataToStream(const Stream: TStream);
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
   WriteDataChunk(Stream);

   // calculate chunk end (to ensure the above value is correct)
   ChunkEnd := Stream.Position + DataSize;

   DataEncoder := CreateDataCoder;
   if not Assigned(DataEncoder) then exit;
   if FBlockSize <= 0
    then DataEncoder.SampleFrames := SampleFrames;

   if Assigned(FOnBeginWrite)
    then FOnBeginWrite(Self);

   with DataEncoder do
    try
     Samples := 0;
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
  RegisterFileFormat(TAudioFileWAV);

end.
