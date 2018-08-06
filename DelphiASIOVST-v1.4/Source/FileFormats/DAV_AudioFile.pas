unit DAV_AudioFile;

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
  Classes, SysUtils, DAV_ChannelDataCoder;

type
  TCodingEvent = procedure(Sender: TObject;
    const Coder: TCustomChannelDataCoder; var Position: Cardinal) of object;

  {$IFDEF Delphi5}
  TAudioEncoding = (aeInteger, aeFloat, aeMP3, aeACM, aeADPCM,
    aeMSADPCM, aeDVIADPCM, aeMuLaw, aeALaw, aeOther);
  {$ELSE}
  TAudioEncoding = (aeUndefined = -1, aeInteger = 0, aeFloat = 1, aeMP3 = 2,
                    aeACM = 3, aeADPCM = 4, aeMSADPCM = 5, aeDVIADPCM = 6,
                    aeMuLaw = 7, aeALaw = 8, aeOther = 9);
  {$ENDIF}

  IAudioFileBitsPerSample = interface(IInterface)
    ['{1BB97B83-7F50-4BD7-9634-37F4399EA6FC}']
    procedure SetBitsPerSample(const Value: Byte);
    function GetBitsPerSample: Byte;

    property BitsPerSample:Byte read GetBitsPerSample write SetBitsPerSample;
  end;

  IAudioFileEncoding = interface(IInterface)
    ['{AD47E7BE-3DCB-4140-8008-A475618F68B0}']
    procedure SetEncoding(const Value: TAudioEncoding);
    function GetEncoding: TAudioEncoding;

    property AudioEncoding:TAudioEncoding read GetEncoding write SetEncoding;
  end;


  TCustomAudioFile = class(TInterfacedPersistent{$IFDEF Delphi6_Up}, IStreamPersist{$ENDIF})
  private
    FStreamOwned  : Boolean;
  protected
    FOnEncode     : TCodingEvent;
    FOnDecode     : TCodingEvent;
    FOnBeginRead  : TNotifyEvent;
    FOnBeginWrite : TNotifyEvent;
    FStream       : TStream;
    FBlockSize    : Integer;
    function GetChannels: Cardinal; virtual; abstract;
    function GetSampleFrames: Cardinal; virtual; abstract;
    function GetSampleRate: Double; virtual; abstract;
    function GetTotalTime: Double; virtual;
    procedure SetChannels(const Value: Cardinal); virtual; abstract;
    procedure SetSampleFrames(const Value: Cardinal); virtual; abstract;
    procedure SetSampleRate(const Value: Double); virtual; abstract;

    procedure CheckHeader(const Stream: TStream); virtual; abstract;
    procedure ParseStream(const Stream: TStream); virtual; abstract;

    property StreamOwned: Boolean read FStreamOwned;
  public
    constructor Create; overload; virtual;
    constructor Create(const FileName: TFileName); overload; virtual;
    constructor Create(const Stream: TStream); overload; virtual;
    destructor Destroy; override;

    procedure Flush; virtual;

    procedure Decode(SamplePosition: Cardinal; SampleFrames: Cardinal); virtual;
    procedure Encode(SamplePosition: Cardinal; SampleFrames: Cardinal); virtual;

    procedure LoadFromFile(const FileName: TFileName); virtual;
    procedure SaveToFile(const FileName: TFileName); virtual;

    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;

    // file format identifier
    class function DefaultExtension: string; virtual; abstract;
    class function Description: string; virtual; abstract;
    class function FileFormatFilter: string; virtual; abstract;
    class function CanLoad(const FileName: TFileName): Boolean; overload; virtual;
    class function CanLoad(const Stream: TStream): Boolean; overload; virtual; abstract;

    property BlockSize: Integer read FBlockSize write FBlockSize default 16384;
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property ChannelCount: Cardinal read GetChannels write SetChannels;
    property SampleFrames: Cardinal read GetSampleFrames write SetSampleFrames;
    property TotalTime: Double read GetTotalTime; // = SampleFrames / SampleRate

    property OnEncode: TCodingEvent read FOnEncode write FOnEncode;
    property OnDecode: TCodingEvent read FOnDecode write FOnDecode;
    property OnBeginReadAudioData: TNotifyEvent read FOnBeginRead write FOnBeginRead;
    property OnBeginWriteAudioData: TNotifyEvent read FOnBeginWrite write FOnBeginWrite;
  end;
  TAudioFileClass = class of TCustomAudioFile;

var
  GAudioFileFormats: array of TAudioFileClass;

procedure RegisterFileFormat(AClass: TAudioFileClass);
function ExtensionToFileFormat(Extension: string): TAudioFileClass;
function FileNameToFormat(FileName: TFileName): TAudioFileClass;
function StreamToFormat(Stream: TStream): TAudioFileClass;
function GetSimpleFileFilter: string;

implementation

resourcestring
  RCStrFileAlreadyLoaded = 'File already loaded';
  RCStrStreamInUse = 'Stream is already in use';
  RCStrNoStreamAssigned = 'No stream assigned';
  RCStrTooManySampleframes = 'Too many sampleframes!';

procedure RegisterFileFormat(AClass: TAudioFileClass);
var
  i : Integer;
begin
 // check if file format is already registered
 for i := 0 to Length(GAudioFileFormats) - 1 do
  if GAudioFileFormats[i] = AClass then exit;

 // add file format to list
 SetLength(GAudioFileFormats, Length(GAudioFileFormats) + 1);
 GAudioFileFormats[Length(GAudioFileFormats) - 1] := AClass;
end;

function ExtensionToFileFormat(Extension: string): TAudioFileClass;
var
  i : Integer;
begin
 Result := nil;
 Extension := LowerCase(Extension);
 for i := 0 to Length(GAudioFileFormats) - 1 do
  if GAudioFileFormats[i].DefaultExtension = Extension
   then Result := GAudioFileFormats[i];
end;

function FileNameToFormat(FileName: TFileName): TAudioFileClass;
var
  i : Integer;
begin
 Result := nil;
 if not FileExists(FileName) then
  begin
   Result := ExtensionToFileFormat(Lowercase(ExtractFileExt(FileName)));
   Exit;
  end;

 for i := 0 to Length(GAudioFileFormats) - 1 do
  if GAudioFileFormats[i].CanLoad(FileName)
   then Result := GAudioFileFormats[i];
end;

function StreamToFormat(Stream: TStream): TAudioFileClass;
var
  Index : Integer;
begin
 Result := nil;
 if not Assigned(Stream) then Exit;

 for Index := 0 to Length(GAudioFileFormats) - 1 do
  if GAudioFileFormats[Index].CanLoad(Stream)
   then Result := GAudioFileFormats[Index];
end;

function GetSimpleFileFilter: string;
var
  i : Integer;
begin
 Result := '';
 for i := 0 to Length(GAudioFileFormats) - 1 do
  with GAudioFileFormats[i]
   do Result := Result + Description + ' (*' + DefaultExtension + ') |*' +
     DefaultExtension + '|';

 // remove last separator
 if Result <> '' then SetLength(Result, Length(Result) - 1);
end;

{ TCustomAudioFile }

constructor TCustomAudioFile.Create;
begin
 FBlockSize := 16384;
 FStreamOwned := False;
end;

constructor TCustomAudioFile.Create(const FileName: TFileName);
begin
 if FileExists(FileName)
  then Create(TFileStream.Create(FileName, fmOpenReadWrite))
  else Create(TFileStream.Create(FileName, fmCreate));
 FStreamOwned := True;
end;

constructor TCustomAudioFile.Create(const Stream: TStream);
begin
 Create;
 if CanLoad(Stream)
  then LoadFromStream(Stream) else
 if Stream.Size = 0
  then SaveToStream(Stream);
 FStream := Stream;
end;

class function TCustomAudioFile.CanLoad(const FileName: TFileName): Boolean;
var
  FS : TFileStream;
begin
 FS := TFileStream.Create(FileName, fmOpenRead);
 try
  Result := CanLoad(FS);
 finally
  FreeAndNil(FS);
 end;
end;

destructor TCustomAudioFile.Destroy;
begin
 if FStreamOwned and Assigned(FStream)
  then FreeAndNil(FStream);
 inherited;
end;

procedure TCustomAudioFile.Encode(SamplePosition: Cardinal; SampleFrames: Cardinal);
begin
 if not Assigned(FStream)
  then raise Exception.Create(RCStrNoStreamAssigned);

 if SamplePosition + SampleFrames > Self.SampleFrames
  then Self.SampleFrames := SamplePosition + SampleFrames;
end;

procedure TCustomAudioFile.Decode(SamplePosition: Cardinal; SampleFrames: Cardinal);
begin
 if not Assigned(FStream)
  then raise Exception.Create(RCStrNoStreamAssigned);

 if SamplePosition + SampleFrames > Self.ChannelCount * Self.SampleFrames
  then raise Exception.Create(RCStrTooManySampleFrames);
end;

procedure TCustomAudioFile.Flush;
begin
 if Assigned(FStream)
  then SaveToStream(FStream);
end;

function TCustomAudioFile.GetTotalTime: Double;
begin
 Result := SampleFrames / SampleRate;
end;

procedure TCustomAudioFile.LoadFromFile(const FileName: TFileName);
var
  FileStream : TFileStream;
begin
 if Assigned(FStream)
  then raise Exception.Create(RCStrFileAlreadyLoaded);

 FileStream := TFileStream.Create(FileName, fmOpenRead);
 with FileStream do
  try
   LoadFromStream(FileStream);
  finally
   FreeAndNil(FileStream);
  end;
end;

procedure TCustomAudioFile.LoadFromStream(Stream: TStream);
begin
 if Assigned(FStream)
  then raise Exception.Create(RCStrFileAlreadyLoaded);
end;

procedure TCustomAudioFile.SaveToFile(const FileName: TFileName);
var
  FileStream : TFileStream;
begin
 if FileExists(FileName)
  then FileStream := TFileStream.Create(FileName, fmOpenWrite)
  else FileStream := TFileStream.Create(FileName, fmCreate);
 with FileStream do
  try
   SaveToStream(FileStream);
  finally
   FreeAndNil(FileStream);
  end;
end;

procedure TCustomAudioFile.SaveToStream(Stream: TStream);
begin
 if FStream = Stream
  then raise Exception.Create(RCStrStreamInUse);
end;

end.
