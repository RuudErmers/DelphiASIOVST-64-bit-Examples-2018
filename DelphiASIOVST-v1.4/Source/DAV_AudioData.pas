{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_AudioData;

interface

{$I DAV_Compiler.inc}
{$IFDEF DELPHI10_UP} {$REGION 'Documentation'} {$ENDIF}
//
// TAudioDataCollection
// +----------------------------------------+
// |                                        |
// |  TAudioChannels                        |
// |  +----------------------------------+  |
// |  |                                  |  |
// |  |  TAudioChannel                   |  |
// |  |  +------------------------+---+  |  |
// |  |  |                        | 1 |  |  |
// |  |  |  TAudioChannelData     +---+  |  |
// |  |  |  +------------------+      |  |  |
// |  |  |  | actual DATA      |      |  |  |
// |  |  |  +------------------+      |  |  |
// |  |  |                            |  |  |
// |  |  +------------------------+---+  |  |
// |  |  |                        | 2 |  |  |
// |  |  |  TAudioChannelData     +---+  |  |
// |  |  |  +------------------+      |  |  |
// |  |  |  | actual DATA      |      |  |  |
// |  |  |  +------------------+      |  |  |
// |  |  |                            |  |  |
// |  |  +----------------------------+  |  |
// |  |                                  |  |
// |  +----------------------------------+  |
// |                                        |
// +----------------------------------------+
//
{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}

uses
{$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Classes, SysUtils,
  DAV_Classes, DAV_Types, DAV_AudioFile, DAV_ChannelDataCoder;

type
{$IFDEF DELPHI10_UP} {$REGION 'AudioData classes'} {$ENDIF}
  /// /////////////////////
  // TAudioData classes //
  /// /////////////////////

  TCustomAudioData = class;
  TAudioData32 = class;
  TAudioData64 = class;

  TCustomAudioData = class(TDspSampleRatePersistent)
  private
    FSampleCount: Cardinal;
    FExternalData: Boolean;
    procedure SetSampleCount(const Value: Cardinal);
  protected
    function GetSum: Double; virtual; abstract;
    function GetRMS: Double; virtual; abstract;
    function GetPeak: Double; virtual; abstract;
    procedure AllocateMemory(SampleCount: Int64); virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SampleCountChanged(NewSampleCount: Int64); virtual;
  public
    constructor Create; override;

    // some simple processing functions
    procedure Add(Constant: Double); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure GenerateWhiteNoise(Amplitude: Double); virtual; abstract;
    procedure Mix(AudioDataCollection: TCustomAudioData); virtual; abstract;
    procedure Multiply(Factor: Double); virtual; abstract;
    procedure Exponentiate(Exponent: Double); virtual; abstract;
    procedure Rectify; virtual; abstract;
    procedure RemoveDC; virtual; abstract;
    procedure Normalize; virtual; abstract;

    property Sum: Double read GetSum;
    property RMS: Double read GetRMS;
    property Peak: Double read GetPeak;

    property ExternalData: Boolean read FExternalData;
    property SampleCount: Cardinal read FSampleCount write SetSampleCount;
  end;

  TAudioData32 = class(TCustomAudioData)
  private
    FChannelData: PDAVSingleFixedArray;
    function GetChannelData(Sample: Int64): Single;
    procedure SetChannelData(Sample: Int64; const Value: Single);
  protected
    function GetSum: Double; override;
    function GetRMS: Double; override;
    function GetPeak: Double; override;
    procedure AllocateMemory(SampleCount: Int64); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(DataPtr: PDAVSingleFixedArray = nil);
      reintroduce; virtual;
    destructor Destroy; override;

    // some processing functions
    procedure Add(Constant: Double); override;
    procedure Clear; override;
    procedure GenerateWhiteNoise(Amplitude: Double); override;
    procedure Mix(AudioDataCollection: TCustomAudioData); override;
    procedure Multiply(Factor: Double); override;
    procedure Exponentiate(Exponent: Double); override;
    procedure Rectify; override;
    procedure RemoveDC; override;
    procedure Normalize; override;

    // data access properties
    property ChannelData[Sample: Int64]: Single read GetChannelData
      write SetChannelData;
    property ChannelDataPointer: PDAVSingleFixedArray read FChannelData;
  published
    property SampleCount;
    property SampleRate;
  end;

  TAudioData64 = class(TCustomAudioData)
  private
    FChannelData: PDAVDoubleFixedArray;
    function GetChannelData(Sample: Int64): Double;
    procedure SetChannelData(Sample: Int64; const Value: Double);
  protected
    function GetSum: Double; override;
    function GetRMS: Double; override;
    function GetPeak: Double; override;
    procedure AllocateMemory(SampleCount: Int64); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(DataPtr: PDAVDoubleFixedArray = nil);
      reintroduce; virtual;
    destructor Destroy; override;

    // some processing functions
    procedure Add(Constant: Double); override;
    procedure Clear; override;
    procedure GenerateWhiteNoise(Amplitude: Double); override;
    procedure Mix(AudioDataCollection: TCustomAudioData); override;
    procedure Multiply(Factor: Double); override;
    procedure Exponentiate(Exponent: Double); override;
    procedure Rectify; override;
    procedure RemoveDC; override;
    procedure Normalize; override;

    // data access properties
    property ChannelData[Sample: Int64]: Double read GetChannelData
      write SetChannelData;
    property ChannelDataPointer: PDAVDoubleFixedArray read FChannelData;
  published
    property SampleCount;
    property SampleRate;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI10_UP} {$REGION 'AudioDataComponent classes'} {$ENDIF}

  TCustomAudioDataComponent = class(TCustomAudioComponent)
  private
    FAudioData: TCustomAudioData;
    FOnSampleCountChanged: TNotifyEvent;
    procedure SetSampleCount(const Value: Cardinal);
    function GetSampleCount: Cardinal;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    destructor Destroy; override;
    property OnSampleCountChanged: TNotifyEvent read FOnSampleCountChanged
      write FOnSampleCountChanged;
    property AudioData: TCustomAudioData read FAudioData;
    property SampleCount: Cardinal read GetSampleCount write SetSampleCount;
  end;

  TAudioData32Component = class(TCustomAudioDataComponent)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SampleCount;
    property SampleRate;
    property SampleRateSource;
    property OnSampleCountChanged;
  end;

  TAudioData64Component = class(TCustomAudioDataComponent)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SampleCount;
    property SampleRate;
    property SampleRateSource;
    property OnSampleCountChanged;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI10_UP} {$REGION 'AudioChannel classes'} {$ENDIF}
  /// ///////////////////////////////
  // TAudioDataCollection classes //
  /// ///////////////////////////////

  TCustomAudioDataCollection = class;

  TCustomAudioChannels = class(TOwnedCollection)
  private
    FOnChanged: TNotifyEvent;
  protected
{$IFDEF Delphi6_Up}
    procedure Notify(Item: TCollectionItem;
      Action: TCollectionNotification); override;
{$ELSE}
    procedure Update(Item: TCollectionItem); override;
{$ENDIF}
  public
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Items; default;
  end;

  TAudioChannels32 = class(TCustomAudioChannels);
  TAudioChannels64 = class(TCustomAudioChannels);

  TCustomAudioChannel = class(TCollectionItem)
  private
    FDisplayName: string;
    FChannelsList: TCustomAudioChannels;
    FSampleCount: Cardinal;
    function GetAudioDataCollection: TCustomAudioDataCollection;
  protected
    function GetDisplayName: string; override;
    function GetSum: Double; virtual; abstract;
    function GetRMS: Double; virtual; abstract;
    function GetPeak: Double; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;
    procedure SampleCountChanged; virtual;

    property AudioDataCollection: TCustomAudioDataCollection
      read GetAudioDataCollection;
  public
    constructor Create(Collection: TCollection); override;

    // some processing functions
    procedure Add(Constant: Double); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure GenerateWhiteNoise(Amplitude: Double); virtual; abstract;
    procedure Multiply(Factor: Double); virtual; abstract;
    procedure Exponentiate(Exponent: Double); virtual; abstract;
    procedure Rectify; virtual; abstract;
    procedure RemoveDC; virtual; abstract;
    procedure Normalize; virtual; abstract;

    property Sum: Double read GetSum;
    property RMS: Double read GetRMS;
    property Peak: Double read GetPeak;
    property SampleCount: Cardinal read FSampleCount;
  published
    property DisplayName;
  end;

  TAudioChannel32 = class(TCustomAudioChannel)
  private
    FChannelData: TAudioData32;
    function GetChannelData(Sample: Int64): Single;
    procedure SetChannelData(Sample: Int64; const Value: Single);
    function GetChannelDataPointer: PDAVSingleFixedArray;
  protected
    function GetSum: Double; override;
    function GetRMS: Double; override;
    function GetPeak: Double; override;
    procedure SampleCountChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    // some processing functions
    procedure Add(Constant: Double); override;
    procedure Clear; override;
    procedure GenerateWhiteNoise(Amplitude: Double); override;
    procedure Multiply(Factor: Double); override;
    procedure Exponentiate(Exponent: Double); override;
    procedure Rectify; override;
    procedure RemoveDC; override;
    procedure Normalize; override;

    // data access properties
    property ChannelData[Sample: Int64]: Single read GetChannelData
      write SetChannelData;
    property ChannelDataPointer: PDAVSingleFixedArray
      read GetChannelDataPointer;
  end;

  TAudioChannel64 = class(TCustomAudioChannel)
  private
    FChannelData: TAudioData64;
    function GetChannelData(Sample: Int64): Double;
    procedure SetChannelData(Sample: Int64; const Value: Double);
    function GetChannelDataPointer: PDAVDoubleFixedArray;
  protected
    function GetSum: Double; override;
    function GetRMS: Double; override;
    function GetPeak: Double; override;
    procedure SampleCountChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    // some processing functions
    procedure Add(Constant: Double); override;
    procedure Clear; override;
    procedure GenerateWhiteNoise(Amplitude: Double); override;
    procedure Multiply(Factor: Double); override;
    procedure Exponentiate(Exponent: Double); override;
    procedure Rectify; override;
    procedure RemoveDC; override;
    procedure Normalize; override;

    // data access properties
    property ChannelData[Sample: Int64]: Double read GetChannelData
      write SetChannelData;
    property ChannelDataPointer: PDAVDoubleFixedArray
      read GetChannelDataPointer;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI10_UP} {$REGION 'AudioDataCollection classes'} {$ENDIF}

  TAudioDataCollectionClass = class of TCustomAudioDataCollection;

  TCustomAudioDataCollection = class(TCustomAudioComponent)
  private
    FSampleFrames: Cardinal;
    FExternalData: Boolean;
    FOnDataChanged: TNotifyEvent;
    function GetPeak: Double;
    function GetSum: Double;
    function GetChannelCount: Integer;
    procedure SetChannelCount(const Value: Integer);
    procedure SetSampleFrames(const Value: Cardinal);
  protected
    FChannels: TCustomAudioChannels;
    procedure SampleFramesChanged; virtual;
    procedure CreateChannels; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    property ExternalData: Boolean read FExternalData;

    procedure BeginReadAudioData(Sender: TObject);
    procedure BeginWriteAudioData(Sender: TObject);

    procedure DataDecoding(Sender: TObject;
      const Coder: TCustomChannelDataCoder; var Position: Cardinal);
      virtual; abstract;
    procedure DataEncoding(Sender: TObject;
      const Coder: TCustomChannelDataCoder; var Position: Cardinal);
      virtual; abstract;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AChannels: Integer;
      ASampleFrames: Int64; DataPtr: Pointer = nil); reintroduce; overload;
      virtual; abstract;
    destructor Destroy; override;

    procedure Changed; override;

    // some processing functions
    procedure Add(Constant: Double); virtual;
    procedure Clear; virtual;
    procedure GenerateWhiteNoise(Amplitude: Double); virtual;
    procedure Multiply(Factor: Double); virtual;
    procedure Exponentiate(Exponent: Double); virtual;
    procedure Trim; virtual; abstract;
    procedure Rectify; virtual;
    procedure RemoveDC; virtual;
    procedure Normalize; virtual;

    property Sum: Double read GetSum;
    property Peak: Double read GetPeak;

    // File I/O
    procedure LoadFromFile(const FileName: TFileName); virtual;
    procedure SaveToFile(const FileName: TFileName); overload; virtual;
    procedure SaveToFile(const FileName: TFileName; BitsPerSample: Byte;
      Encoding: TAudioEncoding = aeInteger); overload; virtual;
    procedure LoadFromStream(const Stream: TStream); virtual;
    procedure SaveToStream(const Stream: TStream); overload; virtual;
    procedure SaveToStream(const Stream: TStream; BitsPerSample: Byte;
      Encoding: TAudioEncoding = aeInteger); overload; virtual;

    property SampleFrames: Cardinal read FSampleFrames write SetSampleFrames
      default 0;
    property Channels: TCustomAudioChannels read FChannels write FChannels;
    property ChannelCount: Integer read GetChannelCount write SetChannelCount;

    // Events
    property OnDataChanged: TNotifyEvent read FOnDataChanged
      write FOnDataChanged;
  end;

  TCustomAudioDataCollection32 = class(TCustomAudioDataCollection)
  private
    FChannelDataPointerList: array of PDAVSingleFixedArray;
    function GetAudioChannel(index: Integer): TAudioChannel32; virtual;
    function GetChannelDataPointerList(Channel: Integer): PDAVSingleFixedArray;
    procedure RebuildChannelList(Sender: TObject);
    function GetChannelDataPointerListPointer: Pointer;
  protected
    procedure CreateChannels; override;
    procedure DataDecoding(Sender: TObject;
      const Coder: TCustomChannelDataCoder; var Position: Cardinal); override;
    procedure DataEncoding(Sender: TObject;
      const Coder: TCustomChannelDataCoder; var Position: Cardinal); override;

    property ChannelList[index: Integer]: TAudioChannel32
      read GetAudioChannel; default;
  public
    constructor Create(AOwner: TComponent; AChannels: Integer;
      ASampleFrames: Int64; DataPtr: Pointer = nil); override;
    procedure Trim; override;

    property ChannelDataPointer[Channel: Integer]: PDAVSingleFixedArray
      read GetChannelDataPointerList;
    property ChannelDataPointerList: Pointer
      read GetChannelDataPointerListPointer;
  end;

  TAudioDataCollection32 = class(TCustomAudioDataCollection32)
  published
    property Channels;
    property SampleFrames;
    property SampleRate;
    property SampleRateSource;
    property OnDataChanged;
  end;

  TCustomAudioDataCollection64 = class(TCustomAudioDataCollection)
  private
    FChannelDataPointerList: array of PDAVDoubleFixedArray;
    function GetAudioChannel(index: Integer): TAudioChannel64; virtual;
    function GetChannelDataPointerList(Channel: Integer): PDAVDoubleFixedArray;
    procedure RebuildChannelList(Sender: TObject);
  protected
    procedure CreateChannels; override;
    procedure DataDecoding(Sender: TObject;
      const Coder: TCustomChannelDataCoder; var Position: Cardinal); override;
    procedure DataEncoding(Sender: TObject;
      const Coder: TCustomChannelDataCoder; var Position: Cardinal); override;

    property ChannelList[index: Integer]: TAudioChannel64
      read GetAudioChannel; default;
  public
    procedure Trim; override;

    constructor Create(AOwner: TComponent; AChannels: Integer;
      ASampleFrames: Int64; DataPtr: Pointer = nil); override;
    property ChannelDataPointerList[Channel: Integer]: PDAVDoubleFixedArray
      read GetChannelDataPointerList;
  end;

  TAudioDataCollection64 = class(TCustomAudioDataCollection64)
  published
    property Channels;
    property SampleFrames;
    property SampleRate;
    property SampleRateSource;
    property OnDataChanged;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}

implementation

uses
  Math, DAV_BlockProcessing;

resourcestring
  RCStrNoAudioFileFormat = 'No audio file format registered';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrSampleOutOfRange = 'Sample out of range (%d)';

{$IFDEF DELPHI10_UP} {$REGION 'AudioData implementation'} {$ENDIF}
  { TCustomAudioData }

constructor TCustomAudioData.Create;
begin
  inherited;
  SampleCountChanged(0);
end;

procedure TCustomAudioData.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TCustomAudioData then
  begin
    TCustomAudioData(Dest).SampleCount := FSampleCount;
    TCustomAudioData(Dest).FExternalData := FExternalData;
  end;
end;

procedure TCustomAudioData.SampleCountChanged(NewSampleCount: Int64);
begin
  if not FExternalData then
  begin
    AllocateMemory(NewSampleCount);
    FSampleCount := NewSampleCount;
    // if Assigned(FOnSampleCountChanged)
    // then FOnSampleCountChanged(Self);
  end;
end;

procedure TCustomAudioData.SetSampleCount(const Value: Cardinal);
begin
  if (FSampleCount <> Value) and not FExternalData then
  begin
    SampleCountChanged(Value);
  end;
end;
{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI10_UP} {$REGION 'AudioData implementation'} {$ENDIF}
{ TAudioData32 }

constructor TAudioData32.Create(DataPtr: PDAVSingleFixedArray = nil);
begin
  FExternalData := DataPtr <> nil;
  if FExternalData then
    FChannelData := DataPtr;
  inherited Create;
end;

destructor TAudioData32.Destroy;
begin
  if Assigned(FChannelData) then
  begin
    if not FExternalData then
      Dispose(FChannelData);
    FChannelData := nil;
  end;
  inherited;
end;

procedure TAudioData32.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAudioData32 then
    Move(FChannelData, TAudioData32(Dest).FChannelData,
      FSampleCount * SizeOf(Single))
  else if Dest is TAudioData64 then
    ConvertSingleToDouble(FChannelData,
      PDAVDoubleFixedArray(TAudioData64(Dest).FChannelData), FSampleCount);
end;

procedure TAudioData32.Add(Constant: Double);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := Constant + FChannelData^[Sample];
end;

procedure TAudioData32.AllocateMemory(SampleCount: Int64);
begin
  ReallocMem(FChannelData, SampleCount * SizeOf(Single));

  // check if new length is longer than the old length and fill with zeroes if necessary
  if SampleCount > Self.SampleCount then
    FillChar(FChannelData^[Self.SampleCount], (SampleCount - Self.SampleCount) *
      SizeOf(Single), 0);
end;

procedure TAudioData32.Clear;
begin
  FillChar(FChannelData^, SampleCount * SizeOf(Single), 0);
end;

procedure TAudioData32.GenerateWhiteNoise(Amplitude: Double);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := Amplitude * (2 * random - 1);
end;

function TAudioData32.GetChannelData(Sample: Int64): Single;
begin
  if (Sample >= 0) and (Sample < SampleCount) then
    Result := FChannelData[Sample]
  else
    raise Exception.CreateFmt(RCStrSampleOutOfRange, [Sample]);
end;

function TAudioData32.GetPeak: Double;
var
  Sample: Integer;
begin
  Result := 0;
  if SampleCount = 0 then
    exit;

  for Sample := 0 to SampleCount - 1 do
    if abs(FChannelData^[Sample]) > Result then
      Result := abs(FChannelData^[Sample]);
end;

function TAudioData32.GetRMS: Double;
var
  Sample: Integer;
  SquaredSum: Double;
begin
  Result := 0;
  if SampleCount = 0 then
    exit;

  SquaredSum := 0;
  for Sample := 0 to SampleCount - 1 do
    SquaredSum := SquaredSum + sqr(FChannelData^[Sample]);
  Result := sqrt(SquaredSum / SampleCount);
end;

function TAudioData32.GetSum: Double;
var
  Sample: Integer;
begin
  Result := 0;
  if SampleCount = 0 then
    exit;

  for Sample := 0 to SampleCount - 1 do
    Result := Result + FChannelData^[Sample];
end;

procedure TAudioData32.Mix(AudioDataCollection: TCustomAudioData);
var
  Sample: Integer;
begin
  if AudioDataCollection is TAudioData32 then
    with TAudioData32(AudioDataCollection) do
      for Sample := 0 to min(SampleCount, Self.SampleCount) - 1 do
        FChannelData^[Sample] := FChannelData^[Sample] +
          Self.FChannelData^[Sample]
  else if AudioDataCollection is TAudioData64 then
    with TAudioData64(AudioDataCollection) do
      for Sample := 0 to min(SampleCount, Self.SampleCount) - 1 do
        FChannelData^[Sample] := FChannelData^[Sample] +
          Self.FChannelData^[Sample];
end;

procedure TAudioData32.Multiply(Factor: Double);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := FChannelData^[Sample] * Factor;
end;

procedure TAudioData32.Normalize;
var
  Sample: Integer;
  Scale: Single;
begin
  if SampleCount = 0 then
    exit;

  Scale := 1 / Peak;
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := Scale * FChannelData^[Sample];
end;

procedure TAudioData32.Exponentiate(Exponent: Double);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := Sign(FChannelData^[Sample]) *
      Power(abs(FChannelData^[Sample]), Exponent);
end;

procedure TAudioData32.Rectify;
var
  Sample: Integer;
begin
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := abs(FChannelData^[Sample]);
end;

procedure TAudioData32.RemoveDC;
var
  Sample: Integer;
  DC: Double;
begin
  if SampleCount = 0 then
    exit;

  DC := Sum / SampleCount;
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := FChannelData^[Sample] - DC;
end;

procedure TAudioData32.SetChannelData(Sample: Int64; const Value: Single);
begin
  if (Sample >= 0) and (Sample < SampleCount) then
    FChannelData[Sample] := Value
  else
    raise Exception.CreateFmt(RCStrSampleOutOfRange, [Sample]);
end;

{ TAudioData64 }

constructor TAudioData64.Create(DataPtr: PDAVDoubleFixedArray = nil);
begin
  FExternalData := DataPtr <> nil;
  if FExternalData then
    FChannelData := DataPtr;
  inherited Create;
end;

destructor TAudioData64.Destroy;
begin
  if Assigned(FChannelData) then
  begin
    if not FExternalData then
      Dispose(FChannelData);
    FChannelData := nil;
  end;
  inherited;
end;

procedure TAudioData64.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAudioData64 then
    Move(FChannelData, TAudioData64(Dest).FChannelData,
      FSampleCount * SizeOf(Double))
  else if Dest is TAudioData32 then
    ConvertDoubleToSingle(FChannelData,
      PDAVSingleFixedArray(TAudioData32(Dest).FChannelData), FSampleCount);
end;

procedure TAudioData64.Add(Constant: Double);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := Constant + FChannelData^[Sample];
end;

procedure TAudioData64.AllocateMemory(SampleCount: Int64);
begin
  ReallocMem(FChannelData, SampleCount * SizeOf(Double));

  // check if new length is longer than the old length and fill with zeroes if necessary
  if SampleCount > Self.SampleCount then
    FillChar(FChannelData^[Self.SampleCount], (SampleCount - Self.SampleCount) *
      SizeOf(Double), 0);
end;

procedure TAudioData64.Clear;
begin
  FillChar(FChannelData^, SampleCount * SizeOf(Double), 0);
end;

procedure TAudioData64.GenerateWhiteNoise(Amplitude: Double);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := Amplitude * (2 * random - 1);
end;

function TAudioData64.GetChannelData(Sample: Int64): Double;
begin
  if (Sample >= 0) and (Sample < SampleCount) then
    Result := FChannelData[Sample]
  else
    raise Exception.CreateFmt(RCStrSampleOutOfRange, [Sample]);
end;

function TAudioData64.GetPeak: Double;
var
  Sample: Integer;
begin
  Result := 0;
  if SampleCount = 0 then
    exit;

  for Sample := 0 to SampleCount - 1 do
    if abs(FChannelData^[Sample]) > Result then
      Result := abs(FChannelData^[Sample]);
end;

function TAudioData64.GetRMS: Double;
var
  Sample: Integer;
  SquaredSum: Double;
begin
  Result := 0;
  if SampleCount = 0 then
    exit;

  SquaredSum := 0;
  for Sample := 0 to SampleCount - 1 do
    SquaredSum := SquaredSum + sqr(FChannelData^[Sample]);
  Result := sqrt(SquaredSum / SampleCount);
end;

function TAudioData64.GetSum: Double;
var
  Sample: Integer;
begin
  Result := 0;
  if SampleCount = 0 then
    exit;

  for Sample := 0 to SampleCount - 1 do
    Result := Result + FChannelData^[Sample];
end;

procedure TAudioData64.Mix(AudioDataCollection: TCustomAudioData);
var
  Sample: Integer;
begin
  if AudioDataCollection is TAudioData32 then
    with TAudioData32(AudioDataCollection) do
      for Sample := 0 to SampleCount - 1 do
        FChannelData^[Sample] := FChannelData^[Sample] +
          Self.FChannelData^[Sample]
  else if AudioDataCollection is TAudioData64 then
    with TAudioData64(AudioDataCollection) do
      for Sample := 0 to SampleCount - 1 do
        FChannelData^[Sample] := FChannelData^[Sample] +
          Self.FChannelData^[Sample];
end;

procedure TAudioData64.Multiply(Factor: Double);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := FChannelData^[Sample] * Factor;
end;

procedure TAudioData64.Normalize;
var
  Sample: Integer;
  Scale: Double;
begin
  if SampleCount = 0 then
    exit;

  Scale := 1 / Peak;
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := Scale * FChannelData^[Sample];
end;

procedure TAudioData64.Exponentiate(Exponent: Double);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := Sign(FChannelData^[Sample]) *
      Power(abs(FChannelData^[Sample]), Exponent);
end;

procedure TAudioData64.Rectify;
var
  Sample: Integer;
begin
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := abs(FChannelData^[Sample]);
end;

procedure TAudioData64.RemoveDC;
var
  Sample: Integer;
  DC: Double;
begin
  if SampleCount = 0 then
    exit;

  DC := Sum / SampleCount;
  for Sample := 0 to SampleCount - 1 do
    FChannelData^[Sample] := FChannelData^[Sample] - DC;
end;

procedure TAudioData64.SetChannelData(Sample: Int64; const Value: Double);
begin
  if (Sample >= 0) and (Sample < SampleCount) then
    FChannelData[Sample] := Value
  else
    raise Exception.CreateFmt(RCStrSampleOutOfRange, [Sample]);
end;
{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI10_UP} {$REGION AudioDataComponent implementation} {$ENDIF}
{ TCustomAudioDataComponent }

destructor TCustomAudioDataComponent.Destroy;
begin
  FreeAndNil(FAudioData);
  inherited;
end;

function TCustomAudioDataComponent.GetSampleCount: Cardinal;
begin
  Result := FAudioData.FSampleCount;
end;

procedure TCustomAudioDataComponent.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomAudioDataComponent then
    with TCustomAudioDataComponent(Dest) do
    begin
      FAudioData.Assign(Self.FAudioData);
      FOnSampleCountChanged := Self.FOnSampleCountChanged;
    end
  else
    inherited;
end;

procedure TCustomAudioDataComponent.SetSampleCount(const Value: Cardinal);
begin
  if SampleCount <> Value then
  begin
    FAudioData.SampleCount := Value;

    if Assigned(FOnSampleCountChanged) then
      FOnSampleCountChanged(Self);
  end;
end;

{ TAudioData32Component }

constructor TAudioData32Component.Create(AOwner: TComponent);
begin
  inherited;
  FAudioData := TAudioData32.Create;
end;

{ TAudioData64Component }

constructor TAudioData64Component.Create(AOwner: TComponent);
begin
  inherited;
  FAudioData := TAudioData64.Create;
end;

{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI10_UP} {$REGION 'AudioChannel implementation'} {$ENDIF}
{ TCustomAudioChannels }

{$IFDEF Delphi6_Up}

procedure TCustomAudioChannels.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
{$ELSE}

procedure TCustomAudioChannels.Update(Item: TCollectionItem);
{$ENDIF}
begin
  inherited;
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

{ TCustomAudioChannel }

constructor TCustomAudioChannel.Create(Collection: TCollection);
begin
  inherited;
  FDisplayName := 'Channel ' + IntToStr(Collection.Count);
  SampleCountChanged;
end;

procedure TCustomAudioChannel.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomAudioChannel then
  begin
    TCustomAudioChannel(Dest).FDisplayName := FDisplayName;
    FChannelsList.AssignTo(TCustomAudioChannel(Dest).FChannelsList);
  end
  else
    inherited;
end;

function TCustomAudioChannel.GetAudioDataCollection: TCustomAudioDataCollection;
begin
  Assert(Collection is TCustomAudioChannels);
  Assert(TCustomAudioChannels(Collection)
    .GetOwner is TCustomAudioDataCollection);
  Result := TCustomAudioDataCollection(TCustomAudioChannels(GetOwner).GetOwner);
end;

function TCustomAudioChannel.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

procedure TCustomAudioChannel.SampleCountChanged;
begin
  FSampleCount := AudioDataCollection.SampleFrames;
end;

procedure TCustomAudioChannel.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
  inherited;
end;

{ TAudioChannel32 }

constructor TAudioChannel32.Create(Collection: TCollection);
begin
  inherited;
  if AudioDataCollection.ExternalData then
  begin
    Assert(AudioDataCollection is TCustomAudioDataCollection32);
    with TCustomAudioDataCollection32(AudioDataCollection) do
      FChannelData := TAudioData32.Create
        (FChannelDataPointerList[FChannels.Count - 1]);
  end
  else
    FChannelData := TAudioData32.Create;
  SampleCountChanged;
end;

destructor TAudioChannel32.Destroy;
begin
  FreeAndNil(FChannelData);
  inherited;
end;

{$IFDEF DELPHI10_UP} {$REGION 'TAudioChannel32 Wrapper'} {$ENDIF}

procedure TAudioChannel32.Add(Constant: Double);
begin
  FChannelData.Add(Constant);
end;

procedure TAudioChannel32.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAudioChannel32 then
    FChannelData.AssignTo(TAudioChannel32(Dest).FChannelData)
  else if Dest is TAudioChannel64 then
    FChannelData.AssignTo(TAudioChannel64(Dest).FChannelData);
end;

procedure TAudioChannel32.Clear;
begin
  FChannelData.Clear;
end;

procedure TAudioChannel32.GenerateWhiteNoise(Amplitude: Double);
begin
  FChannelData.GenerateWhiteNoise(Amplitude);
end;

function TAudioChannel32.GetChannelData(Sample: Int64): Single;
begin
  Result := FChannelData.ChannelData[Sample];
end;

function TAudioChannel32.GetChannelDataPointer: PDAVSingleFixedArray;
begin
  if FChannelData is TAudioData32 then
    Result := FChannelData.ChannelDataPointer
  else
    Result := nil;
end;

function TAudioChannel32.GetPeak: Double;
begin
  Result := FChannelData.GetPeak;
end;

function TAudioChannel32.GetRMS: Double;
begin
  Result := FChannelData.GetRMS;
end;

function TAudioChannel32.GetSum: Double;
begin
  Result := FChannelData.GetSum;
end;

procedure TAudioChannel32.Multiply(Factor: Double);
begin
  FChannelData.Multiply(Factor);
end;

procedure TAudioChannel32.Normalize;
begin
  FChannelData.Normalize;
end;

procedure TAudioChannel32.Exponentiate(Exponent: Double);
begin
  FChannelData.Exponentiate(Exponent);
end;

procedure TAudioChannel32.Rectify;
begin
  FChannelData.Rectify;
end;

procedure TAudioChannel32.RemoveDC;
begin
  FChannelData.RemoveDC;
end;
{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}

procedure TAudioChannel32.SampleCountChanged;
begin
  inherited;
  if Assigned(FChannelData) then
    FChannelData.SampleCount := FSampleCount;
end;

procedure TAudioChannel32.SetChannelData(Sample: Int64; const Value: Single);
begin
  FChannelData.ChannelData[Sample] := Value;
end;

{ TAudioChannel64 }

constructor TAudioChannel64.Create(Collection: TCollection);
begin
  inherited;
  if AudioDataCollection.ExternalData then
  begin
    Assert(AudioDataCollection is TCustomAudioDataCollection64);
    with TCustomAudioDataCollection64(AudioDataCollection) do
      FChannelData := TAudioData64.Create
        (FChannelDataPointerList[FChannels.Count - 1]);
  end
  else
    FChannelData := TAudioData64.Create;
  SampleCountChanged;
end;

destructor TAudioChannel64.Destroy;
begin
  FreeAndNil(FChannelData);
  inherited;
end;

{$IFDEF DELPHI10_UP} {$REGION 'TAudioChannel64 Wrapper'} {$ENDIF}

procedure TAudioChannel64.Add(Constant: Double);
begin
  FChannelData.Add(Constant);
end;

procedure TAudioChannel64.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAudioChannel64 then
    FChannelData.AssignTo(TAudioChannel64(Dest).FChannelData)
  else if Dest is TAudioChannel32 then
    FChannelData.AssignTo(TAudioChannel32(Dest).FChannelData);
end;

procedure TAudioChannel64.Clear;
begin
  FChannelData.Clear;
end;

procedure TAudioChannel64.GenerateWhiteNoise(Amplitude: Double);
begin
  FChannelData.GenerateWhiteNoise(Amplitude);
end;

function TAudioChannel64.GetChannelData(Sample: Int64): Double;
begin
  Result := FChannelData.ChannelData[Sample];
end;

function TAudioChannel64.GetChannelDataPointer: PDAVDoubleFixedArray;
begin
  Result := FChannelData.ChannelDataPointer;
end;

function TAudioChannel64.GetPeak: Double;
begin
  Result := FChannelData.Peak;
end;

function TAudioChannel64.GetRMS: Double;
begin
  Result := FChannelData.RMS;
end;

function TAudioChannel64.GetSum: Double;
begin
  Result := FChannelData.Sum;
end;

procedure TAudioChannel64.Multiply(Factor: Double);
begin
  FChannelData.Multiply(Factor);
end;

procedure TAudioChannel64.Normalize;
begin
  FChannelData.Normalize;
end;

procedure TAudioChannel64.Exponentiate(Exponent: Double);
begin
  FChannelData.Exponentiate(Exponent);
end;

procedure TAudioChannel64.Rectify;
begin
  FChannelData.Rectify;
end;

procedure TAudioChannel64.RemoveDC;
begin
  FChannelData.RemoveDC;
end;
{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}

procedure TAudioChannel64.SampleCountChanged;
begin
  inherited;
  if Assigned(FChannelData) then
    FChannelData.SampleCount := FSampleCount;
end;

procedure TAudioChannel64.SetChannelData(Sample: Int64; const Value: Double);
begin
  FChannelData.ChannelData[Sample] := Value;
end;
{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}
{$IFDEF DELPHI10_UP} {$REGION 'AudioDataCollection implementation'} {$ENDIF}
{ TCustomAudioDataCollection }

constructor TCustomAudioDataCollection.Create(AOwner: TComponent);
begin
  inherited;
  FSampleFrames := 0;
  CreateChannels;
end;

procedure TCustomAudioDataCollection.Changed;
begin
  if Assigned(FOnDataChanged) then
    FOnDataChanged(Self);

  inherited;
end;

destructor TCustomAudioDataCollection.Destroy;
begin
  if Assigned(FChannels) then
    FreeAndNil(FChannels);
  inherited;
end;

procedure TCustomAudioDataCollection.Add(Constant: Double);
var
  ch: Integer;
begin
  for ch := 0 to FChannels.Count - 1 do
    TCustomAudioChannel(FChannels.Items[ch]).Add(Constant);
end;

procedure TCustomAudioDataCollection.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TCustomAudioDataCollection then
  begin
    TCustomAudioDataCollection(Dest).FSampleFrames := FSampleFrames;
    FChannels.AssignTo(TCustomAudioDataCollection(Dest).FChannels);
  end;
end;

procedure TCustomAudioDataCollection.Clear;
var
  ch: Integer;
begin
  for ch := 0 to FChannels.Count - 1 do
    TCustomAudioChannel(FChannels.Items[ch]).Clear;
end;

procedure TCustomAudioDataCollection.Exponentiate(Exponent: Double);
var
  ch: Integer;
begin
  for ch := 0 to FChannels.Count - 1 do
    TCustomAudioChannel(FChannels.Items[ch]).Exponentiate(Exponent);
end;

procedure TCustomAudioDataCollection.GenerateWhiteNoise(Amplitude: Double);
var
  Channel: Integer;
begin
  for Channel := 0 to FChannels.Count - 1 do
    TCustomAudioChannel(FChannels.Items[Channel]).GenerateWhiteNoise(Amplitude);
end;

procedure TCustomAudioDataCollection.Multiply(Factor: Double);
var
  Channel: Integer;
begin
  for Channel := 0 to FChannels.Count - 1 do
    TCustomAudioChannel(FChannels.Items[Channel]).Multiply(Factor);
end;

procedure TCustomAudioDataCollection.Rectify;
var
  Channel: Integer;
begin
  for Channel := 0 to FChannels.Count - 1 do
    TCustomAudioChannel(FChannels.Items[Channel]).Rectify;
end;

procedure TCustomAudioDataCollection.RemoveDC;
var
  Channel: Integer;
begin
  for Channel := 0 to FChannels.Count - 1 do
    TCustomAudioChannel(FChannels.Items[Channel]).RemoveDC;
end;

procedure TCustomAudioDataCollection.Normalize;
var
  Channel: Integer;
begin
  for Channel := 0 to FChannels.Count - 1 do
    TCustomAudioChannel(FChannels.Items[Channel]).Normalize;
end;

function TCustomAudioDataCollection.GetChannelCount: Integer;
begin
  if Assigned(FChannels) then
    Result := FChannels.Count
  else
    Result := 0;
end;

function TCustomAudioDataCollection.GetPeak: Double;
var
  ChannelIndex: Integer;
  Peak: Double;
begin
  if FChannels.Count = 0 then
  begin
    Result := 0;
    exit;
  end;

  Result := TCustomAudioChannel(FChannels[0]).Peak;

  for ChannelIndex := 0 to FChannels.Count - 1 do
  begin
    Peak := TCustomAudioChannel(FChannels.Items[ChannelIndex]).Peak;
    if Peak > Result then
      Result := Peak;
  end;
end;

function TCustomAudioDataCollection.GetSum: Double;
var
  ChannelIndex: Integer;
begin
  if FChannels.Count = 0 then
  begin
    Result := 0;
    exit;
  end;

  Result := TCustomAudioChannel(FChannels[0]).Peak;

  for ChannelIndex := 0 to FChannels.Count - 1 do
  begin
    Result := Result + TCustomAudioChannel(FChannels.Items[ChannelIndex]).Peak;
  end;
end;

procedure TCustomAudioDataCollection.SampleFramesChanged;
var
  ChannelIndex: Integer;
begin
  for ChannelIndex := 0 to FChannels.Count - 1 do
  begin
    Assert(FChannels.Items[ChannelIndex] is TCustomAudioChannel);
    if TCustomAudioChannel(FChannels.Items[ChannelIndex]).SampleCount <> FSampleFrames
    then
      TCustomAudioChannel(FChannels.Items[ChannelIndex]).SampleCountChanged;
  end;
end;

procedure TCustomAudioDataCollection.BeginReadAudioData(Sender: TObject);
begin
  SampleFrames := TCustomAudioFile(Sender).SampleFrames;
  ChannelCount := TCustomAudioFile(Sender).ChannelCount;
end;

procedure TCustomAudioDataCollection.BeginWriteAudioData(Sender: TObject);
begin
  // nothing to do yet, but lock data in the future...
end;

procedure TCustomAudioDataCollection.LoadFromStream(const Stream: TStream);
var
  CurrentFormat: Integer;
  StreamStart: Int64;
begin
  if Length(GAudioFileFormats) = 0 then
    raise Exception.Create(RCStrNoAudioFileFormat);

  StreamStart := Stream.Position;

  // search file format that can load the file
  for CurrentFormat := 0 to Length(GAudioFileFormats) - 1 do
  begin
    if GAudioFileFormats[CurrentFormat].CanLoad(Stream) then
    begin
      Stream.Position := StreamStart;
      with GAudioFileFormats[CurrentFormat].Create do
        try
          OnDecode := DataDecoding;
          OnBeginReadAudioData := BeginReadAudioData;

          LoadFromStream(Stream);
        finally
          Free;
        end;

      Changed;

      // file loaded succesfully, now exit!
      exit;
    end;
  end;
end;

procedure TCustomAudioDataCollection.SaveToFile(const FileName: TFileName);
begin
  SaveToFile(FileName, 24);
end;

procedure TCustomAudioDataCollection.SaveToFile(const FileName: TFileName;
  BitsPerSample: Byte; Encoding: TAudioEncoding = aeInteger);
var
  FileFormatIndex: Integer;
  AudioFile: TCustomAudioFile;
begin
  if Length(GAudioFileFormats) = 0 then
    raise Exception.Create(RCStrNoAudioFileFormat);

  for FileFormatIndex := 0 to Length(GAudioFileFormats) - 1 do
    if LowerCase(ExtractFileExt(FileName)) = GAudioFileFormats[FileFormatIndex].DefaultExtension
    then
    begin
      AudioFile := GAudioFileFormats[FileFormatIndex].Create;
      with AudioFile do
        try
          if Supports(AudioFile, IAudioFileBitsPerSample) then
            (AudioFile as IAudioFileBitsPerSample).BitsPerSample :=
              BitsPerSample;
          if Supports(AudioFile, IAudioFileEncoding) then
            (AudioFile as IAudioFileEncoding).AudioEncoding := Encoding;
          SampleFrames := Self.SampleFrames;
          ChannelCount := Self.ChannelCount;
          OnEncode := DataEncoding;
          SaveToFile(FileName);
        finally
          FreeAndNil(AudioFile);
        end;
      exit;
    end;

  raise Exception.CreateFmt('Could not save file %s!', [FileName]);
end;

procedure TCustomAudioDataCollection.SaveToStream(const Stream: TStream);
begin
  SaveToStream(Stream, 24);
end;

procedure TCustomAudioDataCollection.SaveToStream(const Stream: TStream;
  BitsPerSample: Byte; Encoding: TAudioEncoding = aeInteger);
var
  FileFormatIndex: Integer;
  AudioFile: TCustomAudioFile;
begin
  if Length(GAudioFileFormats) = 0 then
    raise Exception.Create(RCStrNoAudioFileFormat);

  for FileFormatIndex := 0 to Length(GAudioFileFormats) - 1 do
    if True then
    begin
      AudioFile := GAudioFileFormats[FileFormatIndex].Create;
      with AudioFile do
        try
          SampleFrames := Self.SampleFrames;
          ChannelCount := Self.ChannelCount;
          OnEncode := DataEncoding;

          if Supports(AudioFile, IAudioFileBitsPerSample) then
            (AudioFile as IAudioFileBitsPerSample).BitsPerSample :=
              BitsPerSample;
          if Supports(AudioFile, IAudioFileEncoding) then
            (AudioFile as IAudioFileEncoding).AudioEncoding := Encoding;

          SaveToStream(Stream);
        finally
          FreeAndNil(AudioFile);
        end;
      exit;
    end;
end;

procedure TCustomAudioDataCollection.LoadFromFile(const FileName: TFileName);
var
  i: Integer;
begin
  if Length(GAudioFileFormats) = 0 then
    raise Exception.Create(RCStrNoAudioFileFormat);

  // search file format that can load the file
  for i := 0 to Length(GAudioFileFormats) - 1 do
    if GAudioFileFormats[i].CanLoad(FileName) then
    begin
      with GAudioFileFormats[i].Create do
        try
          OnDecode := DataDecoding;
          OnBeginReadAudioData := BeginReadAudioData;
          LoadFromFile(FileName);
          Self.SampleRate := SampleRate;
        finally
          Free;
        end;

      Changed;

      // file loaded succesfully, now exit!
      exit;
    end;

  // no file format found
  raise Exception.CreateFmt('Could not load file %s!', [FileName]);
end;

procedure TCustomAudioDataCollection.SetChannelCount(const Value: Integer);
begin
  // delete or add channels until the count matches the desired channel count
  if not FExternalData then
  begin
    while Channels.Count > Value do
      Channels.Delete(Channels.Count - 1);
    while Channels.Count < Value do
      Channels.Add;
  end;
end;

procedure TCustomAudioDataCollection.SetSampleFrames(const Value: Cardinal);
begin
  if (FSampleFrames <> Value) and not FExternalData then
  begin
    FSampleFrames := Value;
    SampleFramesChanged;
  end;
end;

{ TCustomAudioDataCollection32 }

constructor TCustomAudioDataCollection32.Create(AOwner: TComponent;
  AChannels: Integer; ASampleFrames: Int64; DataPtr: Pointer = nil);
var
  ch: Integer;
begin
  inherited Create(AOwner);
  FExternalData := DataPtr <> nil;
  if FExternalData then
  begin
    SetLength(FChannelDataPointerList, AChannels);
    FSampleFrames := ASampleFrames;
    FChannels.Clear;
    for ch := 0 to AChannels - 1 do
    begin
      FChannelDataPointerList[ch] := DataPtr;
      FChannels.Add;
    end;
  end
  else
  begin
    ChannelCount := AChannels;
    SampleFrames := ASampleFrames;
  end;
end;

procedure TCustomAudioDataCollection32.CreateChannels;
begin
  FChannels := TCustomAudioChannels.Create(Self, TAudioChannel32);
  FChannels.OnChanged := RebuildChannelList;
end;

procedure TCustomAudioDataCollection32.RebuildChannelList(Sender: TObject);
var
  i: Integer;
begin
  if (not FExternalData) and Assigned(FChannels) then
  begin
    SetLength(FChannelDataPointerList, FChannels.Count);
    for i := 0 to FChannels.Count - 1 do
      if FChannels.Items[i] is TAudioChannel32 then
        with TAudioChannel32(FChannels.Items[i]) do
          // if Assigned(FChannelData) then // it might be necessary to remove this check in the future!
          FChannelDataPointerList[i] := ChannelDataPointer;
  end;
end;

function TCustomAudioDataCollection32.GetAudioChannel(index: Integer)
  : TAudioChannel32;
begin
  if Assigned(FChannels) then
    if (Index < 0) or (Index >= FChannels.Count) then
      raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index])
    else
      Result := TAudioChannel32(FChannels.Items[index])
  else
    raise Exception.Create('Channels not assigned!')
end;

function TCustomAudioDataCollection32.GetChannelDataPointerList
  (Channel: Integer): PDAVSingleFixedArray;
begin
  Result := ChannelList[Channel].ChannelDataPointer;
end;

function TCustomAudioDataCollection32.GetChannelDataPointerListPointer: Pointer;
begin
  Result := @FChannelDataPointerList;
end;

procedure TCustomAudioDataCollection32.DataDecoding(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel: Cardinal;
  Coder32: TCustomChannel32DataCoder absolute Coder;
begin
  Assert(Coder is TCustomChannel32DataCoder);
  Assert(Cardinal(Channels.Count) = Coder.ChannelCount);
  for Channel := 0 to ChannelCount - 1 do
    Move(Coder32.ChannelPointer[Channel]^[0],
      ChannelList[Channel].ChannelDataPointer^[Position],
      Coder.SampleFrames * SizeOf(Single));
  // Position := Position + Coder.SampleFrames; // not necessary, incremented by caller!
end;

procedure TCustomAudioDataCollection32.DataEncoding(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel: Cardinal;
  Coder32: TCustomChannel32DataCoder absolute Coder;
begin
  Assert(Coder is TCustomChannel32DataCoder);
  for Channel := 0 to ChannelCount - 1 do
    Move(ChannelList[Channel].ChannelDataPointer^[Position],
      Coder32.ChannelPointer[Channel]^[0], Coder32.SampleFrames *
      SizeOf(Single));
  // Position := Position + Coder.SampleFrames; // not necessary, incremented by caller!
end;

procedure TCustomAudioDataCollection32.Trim;
var
  ChannelIndex: Integer;
  SampleIndex: Cardinal;
label
  DoneEnd, DoneStart;
begin
  // check if there are any sampleframes to trim
  if SampleFrames = 0 then
    exit;

  SampleIndex := SampleFrames - 1;
  repeat
    for ChannelIndex := 0 to FChannels.Count - 1 do
      if ChannelDataPointer[ChannelIndex]^[SampleIndex] <> 0 then
        goto DoneEnd;
    Dec(SampleIndex);
  until SampleIndex = 0;

DoneEnd:
  SampleFrames := SampleIndex + 1;

  // check if signal is all zero
  if SampleFrames = 0 then
    exit;

  SampleIndex := 0;
  while SampleIndex < SampleFrames do
  begin
    for ChannelIndex := 0 to FChannels.Count - 1 do
      if ChannelDataPointer[ChannelIndex]^[SampleIndex] <> 0 then
        goto DoneStart;
    Inc(SampleIndex);
  end;

DoneStart:
  // move data to start
  for ChannelIndex := 0 to FChannels.Count - 1 do
    Move(ChannelDataPointer[ChannelIndex]^[SampleIndex],
      ChannelDataPointer[ChannelIndex]^[0], SampleFrames - SampleIndex *
      SizeOf(Single));

  // reduce sampleframes
  SampleFrames := SampleFrames - SampleIndex;
end;

{ TCustomAudioDataCollection64 }

constructor TCustomAudioDataCollection64.Create(AOwner: TComponent;
  AChannels: Integer; ASampleFrames: Int64; DataPtr: Pointer = nil);
var
  ch: Integer;
begin
  inherited Create(AOwner);
  FExternalData := DataPtr <> nil;
  if FExternalData then
  begin
    SetLength(FChannelDataPointerList, AChannels);
    FSampleFrames := ASampleFrames;
    FChannels.Clear;
    for ch := 0 to AChannels - 1 do
    begin
      FChannelDataPointerList[ch] := DataPtr;
      FChannels.Add;
    end;
  end
  else
  begin
    ChannelCount := AChannels;
    SampleFrames := ASampleFrames;
  end;
end;

procedure TCustomAudioDataCollection64.CreateChannels;
begin
  FChannels := TCustomAudioChannels.Create(Self, TAudioChannel64);
  FChannels.OnChanged := RebuildChannelList;
end;

procedure TCustomAudioDataCollection64.DataDecoding(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel: Cardinal;
begin
  if Coder is TCustomChannel64DataCoder then
    with TCustomChannel64DataCoder(Coder) do
      for Channel := 0 to ChannelCount - 1 do
        Move(ChannelPointer[Channel]^[Position],
          ChannelList[Channel].ChannelDataPointer^[Position],
          SampleFrames * SizeOf(Single));
  // Position := Position + Coder.SampleFrames; // not necessary, increnmented by caller!
end;

procedure TCustomAudioDataCollection64.DataEncoding(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel: Cardinal;
begin
  if Coder is TCustomChannel64DataCoder then
    with TCustomChannel64DataCoder(Coder) do
      for Channel := 0 to ChannelCount - 1 do
        Move(ChannelList[Channel].ChannelDataPointer^[Position],
          ChannelPointer[Channel]^[Position], SampleFrames * SizeOf(Single));
  // Position := Position + Coder.SampleFrames; // not necessary, increnmented by caller!
end;

procedure TCustomAudioDataCollection64.RebuildChannelList(Sender: TObject);
var
  i: Integer;
begin
  if not FExternalData then
  begin
    SetLength(FChannelDataPointerList, FChannels.Count);
    for i := 0 to FChannels.Count - 1 do
      if FChannels.Items[i] is TAudioChannel64 then
        with TAudioChannel64(FChannels.Items[i]) do
          FChannelDataPointerList[i] := ChannelDataPointer;
  end;
end;

procedure TCustomAudioDataCollection64.Trim;
var
  ChannelIndex: Integer;
  SampleIndex: Integer;
label
  Done;
begin
  SampleIndex := SampleFrames - 1;
  while SampleIndex > 0 do
  begin
    for ChannelIndex := 0 to FChannels.Count - 1 do
      if FChannelDataPointerList[ChannelIndex]^[SampleIndex] <> 0 then
        goto Done;
    Dec(SampleIndex);
  end;
Done:
  SampleFrames := SampleIndex;
end;

function TCustomAudioDataCollection64.GetAudioChannel(index: Integer)
  : TAudioChannel64;
begin
  if (Index < 0) or (Index >= FChannels.Count) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else
    Result := TAudioChannel64(FChannels.Items[index]);
end;

function TCustomAudioDataCollection64.GetChannelDataPointerList
  (Channel: Integer): PDAVDoubleFixedArray;
begin
  Result := ChannelList[Channel].ChannelDataPointer;
end;
{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}

end.
