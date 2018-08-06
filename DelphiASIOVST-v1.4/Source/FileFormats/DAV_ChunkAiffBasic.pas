unit DAV_ChunkAiffBasic;

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
  Classes, SysUtils, DAV_Types, DAV_Common, DAV_ChunkClasses;

const
  AIFCVersion1 = $A2805140;

type
  TAIFFCompressionType = (ctNotAvailable, ctNone, ctFL32, ctFL64, ctACE2,
                          ctACE8, ctMACE3, ctMACE6, ctALAW, ctULAW, ctG722,
                          ctG726, ctG728, ctGSM, ctUnknown);

  TAIFFDefinedChunk = class(TDefinedChunk)
  public
    constructor Create; override;
  end;

  TAIFFFixedDefinedChunk = class(TFixedDefinedChunk)
  public
    constructor Create; override;
  end;

  TAIFFUnknownChunk = class(TUnknownChunk)
  public
    constructor Create; override;
  end;

  TAIFFTextChunk = class(TCustomTextChunk)
  public
    constructor Create; override;
  end;
  ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////// Common Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFCommonRecord = packed record
    Channels        : SmallInt;
    SampleFrames    : Cardinal;
    SampleSize      : SmallInt;
    SampleRate      : Extended;
  end;

  TAIFFCommonChunk = class(TAIFFDefinedChunk) // 'COMM'
  private
    FCompressionType      : TAIFFCompressionType;
    FCompressionName      : string;
    FForceReadCompression : Boolean;
    procedure SetChannels(const Value: SmallInt);
    procedure SetSampleRate(const Value: Extended);
    procedure SetSampleSize(const Value: SmallInt);
    procedure SetCompressionType(const Value: TAIFFCompressionType);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateChunkSize; virtual;
  public
    AIFFCommonRecord : TAIFFCommonRecord;
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property Channels: SmallInt read AIFFCommonRecord.Channels write SetChannels;
    property SampleFrames: Cardinal read AIFFCommonRecord.SampleFrames write AIFFCommonRecord.SampleFrames;
    property SampleSize: SmallInt read AIFFCommonRecord.SampleSize write SetSampleSize;
    property SampleRate: Extended read AIFFCommonRecord.SampleRate write SetSampleRate;
    property Compression: TAIFFCompressionType read FCompressionType write SetCompressionType;
    property ForceReadCompression: Boolean read FForceReadCompression write FForceReadCompression;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// Form Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFFormRecord = packed record
    FormType: TChunkName; // type of file
  end;

  TAIFFFormChunk = class(TAIFFFixedDefinedChunk)
  private
    function GetFormType: string;
    procedure SetFormType(Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    AIFFFormRecord : TAIFFFormRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property FormType: string read GetFormType write SetFormType;
  end;

  ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////// Format Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFFormatVersionRecord = packed record
    TimeStamp : Cardinal;  // date of format version
  end;

  TAIFFFormatVersionChunk = class(TAIFFFixedDefinedChunk)
  private
    procedure SetTimeStamp(const Value: Cardinal);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
  public
    AIFFFormatVersionRecord : TAIFFFormatVersionRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property TimeStamp: Cardinal read AIFFFormatVersionRecord.TimeStamp write SetTimeStamp;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////// Sound Data Chunk //////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFSoundDataRecord = packed record
    Offset    : Cardinal;
    BlockSize : Cardinal;
  end;

  TAIFFSoundDataChunk = class(TAIFFDefinedChunk) // 'SSND'
  private
    procedure CalculateChunkSize;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    AIFFSoundDataRecord : TAIFFSoundDataRecord;
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  published
    property Offset: Cardinal read AIFFSoundDataRecord.Offset write AIFFSoundDataRecord.Offset;
    property BlockSize: Cardinal read AIFFSoundDataRecord.BlockSize write AIFFSoundDataRecord.BlockSize;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Marker Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TMarkerID = Smallint;

  TAIFFMarkerRecord = packed record
    MarkerID   : TMarkerID;
    Position   : Cardinal;
  end;

  TAIFFMarkerItem = class(TCollectionItem)
  private
    FMarkerName : string;
    procedure SetMarkerID(const Value: TMarkerID);
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;
  public
    MarkerRecord : TAIFFMarkerRecord;
    function GetSize: Cardinal;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    property DisplayName;
  published
    property MarkerID: TMarkerID read MarkerRecord.MarkerID write SetMarkerID;
    property Position: Cardinal read MarkerRecord.Position write MarkerRecord.Position;
    property MarkerName: string read FMarkerName write FMarkerName;
  end;

  TAIFFMarkerChunk = class(TAIFFDefinedChunk) // 'MARK'
  private
    FMarkers : TOwnedCollection;
    procedure CalculateChunkSize;
    function GetMarkerCount: Byte;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property MarkerCount: Byte read GetMarkerCount;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ///////////////////////////// Comments Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFCommentRecord = packed record
    TimeStamp  : Cardinal;
    MarkerID   : TMarkerID;
  end;

  TAIFFCommentItem = class(TCollectionItem)
  private
    FComment : string;
    procedure SetMarkerID(const Value: TMarkerID);
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;
  public
    CommentRecord : TAIFFCommentRecord;
    function GetSize: Cardinal;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    property DisplayName;
  published
    property MarkerID: TMarkerID read CommentRecord.MarkerID write SetMarkerID;
    property TimeStamp: Cardinal read CommentRecord.TimeStamp write CommentRecord.TimeStamp;
    property Comment: string read FComment write FComment;
  end;

  TAIFFCommentChunk = class(TAIFFDefinedChunk) // 'COMT'
  private
    FComments : TOwnedCollection;
    procedure CalculateChunkSize;
    function GetCommentCount: Byte;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property CommentCount: Byte read GetCommentCount;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////// Instrument Chunk //////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFInstrumentRecord = packed record // 'INST'
    BaseNote     : Byte;
    Detune       : ShortInt;
    LowNote      : Byte;
    HighNote     : Byte;
    LowVelocity  : Byte;
    HighVelocity : Byte;
    Gain         : ShortInt;
    SustainLoop  : TMarkerID;
    ReleaseLoop  : TMarkerID;
  end;

  TAIFFInstrumentChunk = class(TAIFFFixedDefinedChunk)
  private
    procedure SetDetune(Value: ShortInt);
    procedure SetHighVelocity(const Value: Byte);
    procedure SetLowVelocity(const Value: Byte);
    procedure SetBaseNote(const Value: Byte);
    procedure SetHighNote(const Value: Byte);
    procedure SetLowNote(const Value: Byte);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
  public
    AIFFInstrumentRecord : TAIFFInstrumentRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property BaseNote: Byte read AIFFInstrumentRecord.BaseNote write SetBaseNote;
    property Detune: ShortInt read AIFFInstrumentRecord.Detune write SetDetune;
    property LowNote: Byte read AIFFInstrumentRecord.LowNote write SetLowNote;
    property HighNote: Byte read AIFFInstrumentRecord.HighNote write SetHighNote;
    property LowVelocity: Byte read AIFFInstrumentRecord.LowVelocity write SetLowVelocity;
    property HighVelocity: Byte read AIFFInstrumentRecord.HighVelocity write SetHighVelocity;
    property Gain: ShortInt read AIFFInstrumentRecord.Gain write AIFFInstrumentRecord.Gain;
    property SustainLoop: TMarkerID read AIFFInstrumentRecord.SustainLoop write AIFFInstrumentRecord.SustainLoop;
    property ReleaseLoop: TMarkerID read AIFFInstrumentRecord.ReleaseLoop write AIFFInstrumentRecord.ReleaseLoop;
  end;

  ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////// MIDI Chunk /////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFMIDIChunk = class(TAIFFDefinedChunk)
  private
    function GetMIDIData(index: Integer): Byte;
    procedure SetMIDIData(index: Integer; const Value: Byte);
  protected
    FMIDIData : array of Byte;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  public
    class function GetClassChunkName: TChunkName; override;
    property MIDIData[index : Integer]: Byte read GetMIDIData write SetMIDIData;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ///////////////////////// Audio Recording Chunk ////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFAudioRecordingRecord = packed record 
    AESChannelStatusData : array [0..23] of AnsiChar;
  end;

  TAIFFAudioRecordingChunk = class(TAIFFFixedDefinedChunk) // 'AESD'
  private
    function GetAESChannelStatusData: AnsiString;
    procedure SetAESChannelStatusData(const Value: AnsiString);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    AudioRecordingRecord : TAIFFAudioRecordingRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property AESChannelStatusData: AnsiString read GetAESChannelStatusData write SetAESChannelStatusData;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ////////////////////// Application Specific Chunk //////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFApplicationSpecificChunk = class(TAIFFDefinedChunk) // 'APPL'
  private
    function GetApplicationSignature: AnsiString;
    function GetData(index: Integer): Byte;
    procedure CalculateChunkSize;
    procedure SetApplicationSignature(const Value: AnsiString);
    procedure SetData(index: Integer; const Value: Byte);
  protected
    FApplicationSignature : TChunkName;
    FApplicationData      : AnsiString;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    class function GetClassChunkName: TChunkName; override;
    property ApplicationData[index : Integer]: Byte read GetData write SetData;
  published
    property ApplicationSignature: AnsiString read GetApplicationSignature write SetApplicationSignature;
    property ApplicationDataAsString: AnsiString read FApplicationData write FApplicationData;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Text Chunks /////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFNameChunk = class(TAIFFTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  published
    property Name: AnsiString read FText write FText;
  end;

  TAIFFAuthorChunk = class(TAIFFTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  published
    property Author: AnsiString read FText write FText;
  end;

  TAIFFCopyrightChunk = class(TAIFFTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  published
    property Copyright: AnsiString read FText write FText;
  end;

  TAIFFAnnotationChunk = class(TAIFFTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  published
    property Annotation: AnsiString read FText write FText;
  end;


implementation

{ TAIFFDefinedChunk }

constructor TAIFFDefinedChunk.Create;
begin
 inherited;
 FChunkFlags := FChunkFlags + [cfReversedByteOrder, cfPadSize];
end;


{ TAIFFFixedDefinedChunk }

constructor TAIFFFixedDefinedChunk.Create;
begin
 inherited;
 FChunkFlags := FChunkFlags + [cfReversedByteOrder, cfPadSize];
end;


{ TAIFFUnknownChunk }

constructor TAIFFUnknownChunk.Create;
begin
 inherited;
 FChunkFlags := FChunkFlags + [cfReversedByteOrder, cfPadSize];
end;


{ TAIFFTextChunk }

constructor TAIFFTextChunk.Create;
begin
 inherited;
 FChunkFlags := FChunkFlags + [cfReversedByteOrder, cfPadSize];
end;


{ TAIFFCommonChunk }

constructor TAIFFCommonChunk.Create;
begin
 inherited;

 // set defaults
 with AIFFCommonRecord do
  begin
   Channels       := 1;       // one channel
   SampleRate     := 44100;   // 44.1 kHz (CD quality)
   SampleSize     := 16;      // 16bit
   SampleFrames   := 0;       // no data yet
  end;
 FCompressionType := ctNotAvailable;

 CalculateChunkSize;
end;

class function TAIFFCommonChunk.GetClassChunkName: TChunkName;
begin
 Result := 'COMM';
end;

procedure TAIFFCommonChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFCommonChunk then
  with TAIFFCommonChunk(Dest) do
   begin
    AIFFCommonRecord := Self.AIFFCommonRecord;
    FCompressionType := Self.FCompressionType;
  end;
end;

procedure TAIFFCommonChunk.CalculateChunkSize;
begin
 FChunkSize := SizeOf(AIFFCommonRecord);
 if FCompressionType <> ctNotAvailable
  then FChunkSize := FChunkSize + SizeOf(TChunkName) + Cardinal(Length(FCompressionName));
end;

procedure TAIFFCommonChunk.LoadFromStream(Stream: TStream);
var
  CompTypeID : TChunkName;
  CompStrLen : Byte;
  CompString : string;
begin
 inherited;
 with Stream do
  begin
   // load basic header first
   Read(AIFFCommonRecord, SizeOf(TAIFFCommonRecord));

   // flip header
   with AIFFCommonRecord do
    begin
     Flip16(Channels);
     Flip32(SampleFrames);
     Flip16(SampleSize);
     Flip80(SampleRate);
    end;

   // exit if no addition information are available
   if (FChunkSize = SizeOf(TAIFFCommonRecord)) and not FForceReadCompression then
    begin
     Compression := ctNotAvailable;
     Exit;
    end;

   // read additional compression information
   Read(CompTypeID, SizeOf(TChunkName));
   if CompTypeID = 'NONE' then FCompressionType := ctNone else
   if CompTypeID = 'ALAW' then FCompressionType := ctALAW else
   if CompTypeID = 'alaw' then FCompressionType := ctALAW else
   if CompTypeID = 'ULAW' then FCompressionType := ctULAW else
   if CompTypeID = 'fl32' then FCompressionType := ctFL32 else
   if CompTypeID = 'fl64' then FCompressionType := ctFL64 else
   if CompTypeID = 'G722' then FCompressionType := ctG722 else
   if CompTypeID = 'G726' then FCompressionType := ctG726 else
   if CompTypeID = 'G728' then FCompressionType := ctG728 else
   if CompTypeID = 'GSM ' then FCompressionType := ctGSM  else
   if CompTypeID = 'ACE2' then FCompressionType := ctACE2 else
   if CompTypeID = 'ACE8' then FCompressionType := ctACE8 else
   if CompTypeID = 'MAC3' then FCompressionType := ctMACE3 else
   if CompTypeID = 'MAC8' then FCompressionType := ctMACE6
    else FCompressionType := ctUnknown;

   Read(CompStrLen, 1);
   SetLength(CompString, CompStrLen);
   Read(CompString[1], CompStrLen);

   // eventually zero pad chunk
   if (CompStrLen mod 2) <> 0 then Position := Position + 1;
  end;
end;

procedure TAIFFCommonChunk.SaveToStream(Stream: TStream);
var
  FlippedAIFFCommonRecord : TAIFFCommonRecord;
  CompressionTypeID       : TChunkName;
  CompressionNameLength   : Byte;
begin
 CalculateChunkSize;
 inherited;
 with Stream do
  begin
   // save basic header first (need to be flipped first)
   FlippedAIFFCommonRecord := AIFFCommonRecord;
   with FlippedAIFFCommonRecord do
    begin
     Flip16(Channels);
     Flip32(SampleFrames);
     Flip16(SampleSize);
     Flip80(SampleRate);
    end;
   Write(FlippedAIFFCommonRecord, SizeOf(TAIFFCommonRecord));

   // write compression info if necessary
   if FCompressionType <> ctNotAvailable then
    begin
     case FCompressionType of
         ctNone : CompressionTypeID := 'None';
         ctACE2 : CompressionTypeID := 'ACE2';
         ctACE8 : CompressionTypeID := 'ACE8';
        ctMACE3 : CompressionTypeID := 'MAC3';
        ctMACE6 : CompressionTypeID := 'MAC6';
         ctALAW : CompressionTypeID := 'ALAW';
         ctULAW : CompressionTypeID := 'ULAW';
         ctFL32 : CompressionTypeID := 'fl32';
         ctFL64 : CompressionTypeID := 'fl64';
         ctG722 : CompressionTypeID := 'G722';
         ctG726 : CompressionTypeID := 'G726';
         ctG728 : CompressionTypeID := 'G728';
          ctGSM : CompressionTypeID := 'GSM ';
      ctUnknown : raise Exception.Create('Not supported');
     end;
     Write(CompressionTypeID, SizeOf(TChunkName));
     CompressionNameLength := Length(FCompressionName);
     Write(CompressionNameLength, SizeOf(CompressionNameLength));
     Write(FCompressionName[1], Length(FCompressionName));

     // eventually zero pad chunk
     if (CompressionNameLength mod 2) <> 0 then Position := Position + 1;
    end;
  end;
end;

procedure TAIFFCommonChunk.SetChannels(const Value: SmallInt);
begin
 if AIFFCommonRecord.Channels <> Value then
  begin
   if Value <= 0
    then raise Exception.Create('Channel count must be > 0');
   AIFFCommonRecord.Channels := Value;
  end;
end;

procedure TAIFFCommonChunk.SetCompressionType(const Value: TAIFFCompressionType);
begin
 if FCompressionType <> Value then
  begin
   FCompressionType := Value;
   case FCompressionType of
    ctNotAvailable : FCompressionName := 'not available';
            ctNone : FCompressionName := 'not compressed';
            ctACE2 : FCompressionName := 'ACE 2-to-1';
            ctACE8 : FCompressionName := 'ACE 8-to-1';
           ctMACE3 : FCompressionName := 'MACE 3-to-1';
           ctMACE6 : FCompressionName := 'MACE 6-to-1';
            ctALAW : FCompressionName := 'A-Law';
            ctULAW : FCompressionName := 'µ-Law';
            ctFL32 : FCompressionName := 'Floating Point 32 (Single)';
            ctFL64 : FCompressionName := 'Floating Point 64 (Double)';
            ctG722 : FCompressionName := 'G.722';
            ctG726 : FCompressionName := 'G.726';
            ctG728 : FCompressionName := 'G.728';
             ctGSM : FCompressionName := 'GSM';
   end;
  end;
end;

procedure TAIFFCommonChunk.SetSampleRate(const Value: Extended);
begin
 if AIFFCommonRecord.SampleRate <> Value then
  begin
   if Value <= 0
    then raise Exception.Create('Sample rate must be > 0');
   AIFFCommonRecord.SampleRate := Value;
  end;
end;

procedure TAIFFCommonChunk.SetSampleSize(const Value: SmallInt);
begin
 if AIFFCommonRecord.SampleSize <> Value then
  begin
   if Value <= 0
    then raise Exception.Create('Sample rate must be > 0');
   AIFFCommonRecord.SampleSize := Value;
  end;
end;


{ TAIFFFormChunk }

constructor TAIFFFormChunk.Create;
begin
 inherited;
 with AIFFFormRecord do
  begin
   FormType := 'AIFF';
  end;
 StartAddress := @AIFFFormRecord;
end;

procedure TAIFFFormChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFFormChunk then
  begin
   TAIFFFormChunk(Dest).AIFFFormRecord := AIFFFormRecord;
  end;
end;

class function TAIFFFormChunk.GetClassChunkName: TChunkName;
begin
 Result := 'FORM';
end;

class function TAIFFFormChunk.GetClassChunkSize: Integer;
begin
 Result := SizeOf(TAIFFFormRecord);
end;

function TAIFFFormChunk.GetFormType: string;
begin
 Result := string(AIFFFormRecord.FormType);
end;

procedure TAIFFFormChunk.SetFormType(Value: string);
begin
 with AIFFFormRecord do
  if Value = 'AIFF' then FormType := 'AIFF' else
  if Value = 'AIFC' then FormType := 'AIFC'
   else raise Exception.Create('Unknown form type');
end;


{ TAIFFFormatVersionChunk }

constructor TAIFFFormatVersionChunk.Create;
begin
 inherited;
 with AIFFFormatVersionRecord do
  begin
   TimeStamp := AIFCVersion1;
  end;
 StartAddress := @AIFFFormatVersionRecord;
end;

procedure TAIFFFormatVersionChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFFormatVersionChunk then
  begin
   TAIFFFormatVersionChunk(Dest).AIFFFormatVersionRecord := AIFFFormatVersionRecord;
  end;
end;

class function TAIFFFormatVersionChunk.GetClassChunkName: TChunkName;
begin
 Result := 'FVER';
end;

class function TAIFFFormatVersionChunk.GetClassChunkSize: Integer;
begin
 Result := SizeOf(TAIFFFormatVersionRecord);
end;

procedure TAIFFFormatVersionChunk.SetTimeStamp(const Value: Cardinal);
begin
 if Value <> AIFCVersion1
  then raise Exception.Create('Only one version from May 23, 1990, 2:40pm is supported yet');
end;


{ TAIFFSoundDataChunk }

constructor TAIFFSoundDataChunk.Create;
begin
 inherited;
 with AIFFSoundDataRecord do
  begin
   Offset    := 0;
   BlockSize := 0;
  end;
end;

procedure TAIFFSoundDataChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 with Stream do
  begin
   // load basic header first
   Read(AIFFSoundDataRecord, SizeOf(TAIFFSoundDataRecord));

   // flip header
   with AIFFSoundDataRecord do
    begin
     Flip32(Offset);
     Flip32(BlockSize);
    end;

   // set position to end of chunk
   Position := Position + FChunkSize - SizeOf(TAIFFSoundDataRecord);
  end;
end;

procedure TAIFFSoundDataChunk.SaveToStream(Stream: TStream);
var
  FlippedAIFFSoundDataRecord : TAIFFSoundDataRecord;
begin
 CalculateChunkSize;
 inherited;
 with Stream do
  begin
   // save basic header first (need to be flipped first)
   FlippedAIFFSoundDataRecord := AIFFSoundDataRecord;
   with FlippedAIFFSoundDataRecord do
    begin
     Flip32(Offset);
     Flip32(BlockSize);
    end;
   Write(FlippedAIFFSoundDataRecord, SizeOf(TAIFFSoundDataRecord));

  end;
end;

procedure TAIFFSoundDataChunk.CalculateChunkSize;
begin
 FChunkSize := SizeOf(AIFFSoundDataRecord);
end;

procedure TAIFFSoundDataChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFSoundDataChunk then
  begin
   TAIFFSoundDataChunk(Dest).AIFFSoundDataRecord := AIFFSoundDataRecord;
  end;
end;


{ TAIFFMarkerItem }

procedure TAIFFMarkerItem.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFMarkerItem
  then TAIFFMarkerItem(Dest).MarkerRecord := MarkerRecord;
end;

function TAIFFMarkerItem.GetDisplayName: string;
begin
 Result := FMarkerName;
end;

function TAIFFMarkerItem.GetSize: Cardinal;
begin
 Result := SizeOf(TAIFFMarkerRecord) + Length(FMarkerName) + 1;
end;

procedure TAIFFMarkerItem.LoadFromStream(Stream: TStream);
var
  StringSize: Byte;
begin
 with Stream do
  begin
   // read marker header
   Read(MarkerRecord, SizeOf(TAIFFMarkerRecord));

   Flip16(MarkerRecord.MarkerID);
   Flip16(MarkerRecord.Position);

   // now read the marker string
   Read(StringSize, SizeOf(Byte));

   SetLength(FMarkerName, StringSize);
   Read(FMarkerName[1], StringSize);

   // add pad byte if necessary
   if StringSize mod 2 = 0 then Position := Position + 1;
  end;
end;

procedure TAIFFMarkerItem.SaveToStream(Stream: TStream);
var
  StringSize : Integer;
begin
 with Stream do
  begin
   // write marker header
   Write(MarkerRecord, SizeOf(TAIFFMarkerRecord));

   // now write the marker string
   StringSize := Length(FMarkerName);
   Write(StringSize, SizeOf(Byte));
   Write(FMarkerName[1], StringSize);
  end;
end;

procedure TAIFFMarkerItem.SetDisplayName(const Value: string);
begin
 FMarkerName := Value;
 inherited;
end;

procedure TAIFFMarkerItem.SetMarkerID(const Value: TMarkerID);
begin
 with MarkerRecord do
  if Value <> MarkerID then
   begin
    if Value = 0 then raise Exception.Create('MarkerID must be > 0');
    MarkerID := Value;
   end;
end;


{ TAIFFMarkerChunk }

constructor TAIFFMarkerChunk.Create;
begin
 inherited;
 FMarkers := TOwnedCollection.Create(Self, TAIFFMarkerItem);
 CalculateChunkSize;
end;

destructor TAIFFMarkerChunk.Destroy;
begin
 FreeAndNil(FMarkers);
 inherited;
end;

class function TAIFFMarkerChunk.GetClassChunkName: TChunkName;
begin
 Result := 'MARK';
end;

function TAIFFMarkerChunk.GetMarkerCount: Byte;
begin
 Result := FMarkers.Count;
end;

procedure TAIFFMarkerChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFMarkerChunk
  then TAIFFMarkerChunk(Dest).FMarkers.Assign(FMarkers);
end;

procedure TAIFFMarkerChunk.CalculateChunkSize;
var
  i : Integer;
begin
 FChunkSize := SizeOf(Byte);
 for i := 0 to FMarkers.Count - 1
  do FChunkSize := FChunkSize + TAIFFMarkerItem(FMarkers.Items[i]).GetSize;
end;

procedure TAIFFMarkerChunk.LoadFromStream(Stream: TStream);
var
  i           : Integer;
  MarkerCount : Word;
begin
 inherited;
 with Stream do
  begin
   // load number of markers
   Read(MarkerCount, SizeOf(Word));
   Flip16(MarkerCount);
   Assert(MarkerCount < (FChunkSize div SizeOf(TAIFFMarkerRecord)));

   // clear existing markers
   FMarkers.Clear;

   // load every single marker
   for i := 0 to MarkerCount - 1 do
    with TAIFFMarkerItem(FMarkers.Add)
     do LoadFromStream(Stream);

  end;
end;

procedure TAIFFMarkerChunk.SaveToStream(Stream: TStream);
var
  MarkerCount, i : Integer;
begin
 CalculateChunkSize;
 inherited;
 with Stream do
  begin
   // store number of markers
   MarkerCount := FMarkers.Count;
   Write(MarkerCount, SizeOf(Byte));

   for i := 0 to FMarkers.Count - 1
    do TAIFFMarkerItem(FMarkers.Items[i]).SaveToStream(Stream);
  end;
end;


{ TAIFFCommentItem }

procedure TAIFFCommentItem.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFCommentItem
  then TAIFFCommentItem(Dest).CommentRecord := CommentRecord;
end;

function TAIFFCommentItem.GetDisplayName: string;
begin
 Result := FComment;
end;

function TAIFFCommentItem.GetSize: Cardinal;
begin
 Result := SizeOf(TAIFFCommentRecord) + Length(FComment) + 1;
end;

procedure TAIFFCommentItem.LoadFromStream(Stream: TStream);
var
  StringSize : Integer;
begin
 with Stream do
  begin
   // read comment header
   Read(CommentRecord, SizeOf(TAIFFCommentRecord));

   // now read the comment string
   Read(StringSize, SizeOf(Byte));
   SetLength(FComment, StringSize);
   Read(FComment[1], StringSize);
  end;
end;

procedure TAIFFCommentItem.SaveToStream(Stream: TStream);
var
  StringSize : Integer;
begin
 with Stream do
  begin
   // write comment header
   Write(CommentRecord, SizeOf(TAIFFCommentRecord));

   // now write the comment string
   StringSize := Length(FComment);
   Write(StringSize, SizeOf(Byte));
   Write(FComment[1], StringSize);
  end;
end;

procedure TAIFFCommentItem.SetDisplayName(const Value: string);
begin
 FComment := Value;
 inherited;
end;

procedure TAIFFCommentItem.SetMarkerID(const Value: TMarkerID);
begin
 with CommentRecord do
  if Value <> MarkerID then
   begin
    if Value = 0 then raise Exception.Create('MarkerID must be > 0');
    MarkerID := Value;
   end;
end;


{ TAIFFCommentChunk }

constructor TAIFFCommentChunk.Create;
begin
 inherited;
 FComments := TOwnedCollection.Create(Self, TAIFFCommentItem);
 CalculateChunkSize;
end;

destructor TAIFFCommentChunk.Destroy;
begin
 FreeAndNil(FComments);
 inherited;
end;

procedure TAIFFCommentChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFCommentChunk
  then TAIFFCommentChunk(Dest).FComments.Assign(FComments);
end;

procedure TAIFFCommentChunk.CalculateChunkSize;
var
  i : Integer;
begin
 FChunkSize := SizeOf(Byte);
 for i := 0 to FComments.Count - 1
  do FChunkSize := FChunkSize + TAIFFCommentItem(FComments.Items[i]).GetSize;
end;

class function TAIFFCommentChunk.GetClassChunkName: TChunkName;
begin
 Result := 'COMT';
end;

function TAIFFCommentChunk.GetCommentCount: Byte;
begin
 Result := FComments.Count;
end;

procedure TAIFFCommentChunk.LoadFromStream(Stream: TStream);
var
  CommentCount, i : Integer;
begin
 inherited;
 with Stream do
  begin
   // load number of comments
   Read(CommentCount, SizeOf(Byte));

   // clear existing comments
   FComments.Clear;

   // load every single comment
   for i := 0 to CommentCount - 1 do
    with TAIFFCommentItem(FComments.Add)
     do LoadFromStream(Stream);

   // set position to end of chunk
   Position := Position + FChunkSize - SizeOf(Byte);
  end;
end;

procedure TAIFFCommentChunk.SaveToStream(Stream: TStream);
var
  CommentCount, i : Integer;
begin
 CalculateChunkSize;
 inherited;
 with Stream do
  begin
   // store number of comments
   CommentCount := FComments.Count;
   Write(CommentCount, SizeOf(Byte));

   for i := 0 to FComments.Count - 1
    do TAIFFCommentItem(FComments.Items[i]).SaveToStream(Stream);
  end;
end;


{ TAIFFInstrumentChunk }

constructor TAIFFInstrumentChunk.Create;
begin
 inherited;
 with AIFFInstrumentRecord do
  begin
   BaseNote     := 60;
   Detune       := 0;
   LowNote      := 0;
   HighNote     := 127;
   LowVelocity  := 1;
   HighVelocity := 127;
   Gain         := 0;
  end;
 StartAddress := @AIFFInstrumentRecord;
end;

procedure TAIFFInstrumentChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFInstrumentChunk
  then TAIFFInstrumentChunk(Dest).AIFFInstrumentRecord := AIFFInstrumentRecord; 
end;

class function TAIFFInstrumentChunk.GetClassChunkName: TChunkName;
begin
 Result := 'INST';
end;

class function TAIFFInstrumentChunk.GetClassChunkSize: Integer;
begin
 Result := SizeOf(TAIFFInstrumentRecord);
end;

procedure TAIFFInstrumentChunk.SetBaseNote(const Value: Byte);
begin
 if Value > 127 then raise Exception.Create('Note must be smaller then 127');
 with AIFFInstrumentRecord do
  if Value <> BaseNote then
   begin
    BaseNote := Value;
   end;
end;

procedure TAIFFInstrumentChunk.SetDetune(Value: ShortInt);
begin
 // range check (-50..50 cents)
 if Value >  50 then Value :=  50 else
 if Value < -50 then Value := -50;

 with AIFFInstrumentRecord do
  if Value <> Detune then
   begin
    Detune := Value;
   end;
end;

procedure TAIFFInstrumentChunk.SetHighNote(const Value: Byte);
begin
 if Value > 127 then raise Exception.Create('Note must be smaller then 127');
 with AIFFInstrumentRecord do
  if Value <> HighNote then
   begin
    HighNote := Value;
   end;
end;

procedure TAIFFInstrumentChunk.SetHighVelocity(const Value: Byte);
begin
 if Value = 0   then raise Exception.Create('Velocity must be larger than 0');
 if Value > 127 then raise Exception.Create('Velocity must be smaller than 127');
 with AIFFInstrumentRecord do
  if Value <> HighVelocity then
   begin
    HighVelocity := Value;
   end;
end;

procedure TAIFFInstrumentChunk.SetLowNote(const Value: Byte);
begin
 if Value > 127 then raise Exception.Create('Note must be smaller then 127');
 with AIFFInstrumentRecord do
  if Value <> LowNote then
   begin
    LowNote := Value;
   end;
end;

procedure TAIFFInstrumentChunk.SetLowVelocity(const Value: Byte);
begin
 if Value = 0   then raise Exception.Create('Velocity must be larger than 0');
 if Value > 127 then raise Exception.Create('Velocity must be smaller than 127');
 with AIFFInstrumentRecord do
  if Value <> LowVelocity then
   begin
    LowVelocity := Value;
   end;
end;


{ TAIFFMIDIChunk }

procedure TAIFFMIDIChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFMIDIChunk then
  begin
   SetLength(TAIFFMIDIChunk(Dest).FMIDIData, Length(FMIDIData));
   Move(FMIDIData[0], TAIFFMIDIChunk(Dest).FMIDIData[0], Length(FMIDIData));
  end;
end;

class function TAIFFMIDIChunk.GetClassChunkName: TChunkName;
begin
 Result := 'MIDI';
end;

function TAIFFMIDIChunk.GetMIDIData(index: Integer): Byte;
begin
 if (index >= 0) and (index < Length(FMIDIData))
  then Result := FMIDIData[index]
  else Result := 0;
end;

procedure TAIFFMIDIChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 with Stream do
  begin
   SetLength(FMIDIData, FChunkSize);
   Read(FMIDIData[0], FChunkSize);
  end;
end;

procedure TAIFFMIDIChunk.SaveToStream(Stream: TStream);
begin
 FChunkSize := Length(FMIDIData);
 inherited;
 with Stream do
  begin
   Write(FMIDIData[0], FChunkSize);
  end;
end;

procedure TAIFFMIDIChunk.SetMIDIData(index: Integer; const Value: Byte);
begin
 if (index >= 0) and (index < Length(FMIDIData))
  then FMIDIData[index] := Value;
end;


{ TAIFFAudioRecordingChunk }

constructor TAIFFAudioRecordingChunk.Create;
begin
 inherited;
 StartAddress := @AudioRecordingRecord;
 AudioRecordingRecord.AESChannelStatusData  := '';
end;

procedure TAIFFAudioRecordingChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFAudioRecordingChunk then
  begin
   TAIFFAudioRecordingChunk(Dest).AESChannelStatusData := AESChannelStatusData;
  end;
end;

function TAIFFAudioRecordingChunk.GetAESChannelStatusData: AnsiString;
begin
 SetLength(Result, 24);
 Move(AudioRecordingRecord.AESChannelStatusData[0], Result[1], 24);
end;

class function TAIFFAudioRecordingChunk.GetClassChunkName: TChunkName;
begin
 Result := 'AESD';
end;

class function TAIFFAudioRecordingChunk.GetClassChunkSize: Integer;
begin
 Result := SizeOf(TAIFFAudioRecordingRecord);
end;

procedure TAIFFAudioRecordingChunk.SetAESChannelStatusData(const Value: AnsiString);
begin
 Move(Value[1], AudioRecordingRecord.AESChannelStatusData[0], Length(Value));
end;


{ TAIFFApplicationSpecificChunk }

constructor TAIFFApplicationSpecificChunk.Create;
begin
 inherited;
 // default the signature assotiated with this project
 FApplicationSignature := 'DAVD';
end;

procedure TAIFFApplicationSpecificChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFApplicationSpecificChunk then
  begin
   TAIFFApplicationSpecificChunk(Dest).FApplicationSignature := FApplicationSignature;
   SetLength(TAIFFApplicationSpecificChunk(Dest).FApplicationData, Length(FApplicationData));
   Move(FApplicationData[1], TAIFFApplicationSpecificChunk(Dest).FApplicationData[1], Length(FApplicationData));
  end;
end;

procedure TAIFFApplicationSpecificChunk.CalculateChunkSize;
begin
 FChunkSize := Length(FApplicationData) + SizeOf(TChunkName);
end;

function TAIFFApplicationSpecificChunk.GetApplicationSignature: AnsiString;
begin
 Result := AnsiString(FApplicationSignature);
end;

class function TAIFFApplicationSpecificChunk.GetClassChunkName: TChunkName;
begin
 Result := 'APPL';
end;

function TAIFFApplicationSpecificChunk.GetData(Index: Integer): Byte;
begin
 if (Index >= 0) and (Index < Length(FApplicationData))
  then Result := Byte(FApplicationData[Index + 1])
  else Result := 0;
end;

procedure TAIFFApplicationSpecificChunk.SetApplicationSignature(const Value: AnsiString);
var
  ApplicationSignatureSize : Integer;
begin
 ApplicationSignatureSize := Length(Value);
 if ApplicationSignatureSize > 3 then ApplicationSignatureSize := 4;
 Move(Value[1], FApplicationSignature[0], ApplicationSignatureSize);
end;

procedure TAIFFApplicationSpecificChunk.SetData(Index: Integer;
  const Value: Byte);
begin
 if (Index >= 0) and (Index < Length(FApplicationSignature))
  then FApplicationSignature[index + 1] := AnsiChar(Value);
end;

procedure TAIFFApplicationSpecificChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 with Stream do
  begin
   // read application signature
   Read(FApplicationSignature, SizeOf(TChunkName));

   // read application data
   SetLength(FApplicationData, FChunkSize - SizeOf(TChunkName));
   Read(FApplicationData[1], FChunkSize);
  end;
end;

procedure TAIFFApplicationSpecificChunk.SaveToStream(Stream: TStream);
begin
 CalculateChunkSize;
 inherited;
 with Stream do
  begin
   // write application signature
   Write(FApplicationSignature, SizeOf(TChunkName));

   // write application data
   Write(FApplicationData[1], FChunkSize - SizeOf(TChunkName));
  end;
end;


{ TAIFFNameChunk }

class function TAIFFNameChunk.GetClassChunkName: TChunkName;
begin
  Result := 'NAME';
end;


{ TAIFFAuthorChunk }

class function TAIFFAuthorChunk.GetClassChunkName: TChunkName;
begin
 Result := 'AUTH';
end;


{ TAIFFCopyrightChunk }

class function TAIFFCopyrightChunk.GetClassChunkName: TChunkName;
begin
 Result := '(c) ';
end;


{ TAIFFAnnotationChunk }

class function TAIFFAnnotationChunk.GetClassChunkName: TChunkName;
begin
 Result := 'ANNO';
end;

end.
