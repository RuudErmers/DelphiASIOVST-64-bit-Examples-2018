unit DAV_DspHrtf;

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
//                                                                            //
//  This unit uses a spherical coordinate system with the following           //
//  specifications:                                                           //
//                                                                            //
//  - the azimuth is the angle to the front [-Pi..Pi]                         //
//  - the elevation is the angle to the equator [-Pi/2..Pi/2]                 //
//  - the radial distance r is assumed to be 1 and is not used                //
//                                                                            //
//  With this definition a point (0, 0, 1) equals a point in front of the     //
//  listener, while a point (Pi/2, 0, 1) means a point hard left to the       //
//  listener. Finally the point (*, Pi/2, 1) means a point above the          //
//  listener. Note: the azimuth angle is not important anymore in case the    //
//  source is on the pole.                                                    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, Contnrs, DAV_Types, DAV_ChunkClasses,
  DAV_HalfFloat, DAV_Classes, DAV_VectorMath;

type
  THrirEncoding = (heInteger, heFloat);
  THrirHeader = record
    Position       : TSphereVector3D;  // position in spherical coordinates
    Flags          : Integer;          // not used yet
    SampleFrames   : Integer;          // samples per channel
    SampleRate     : Single;           // samplerate
    Encoding       : THrirEncoding;    // encoding (integer or float)
    BytesPerSample : Integer;          // bytes used for one sample
  end;

  TInterpolationType = (itNearest, itLinear, itLinear3);

  TCustomHrir = class(TDefinedChunk)
  private
    procedure SetSampleFrames(const Value: Integer);
    procedure SetBytesPerSample(const Value: Integer);
    function GetPosition: TSphereVector3D;
  protected
    FHrirHeader : THrirHeader;
    FBuffer     : array [0..1] of Pointer;
    procedure CreateBuffers; virtual;
    procedure MoveData32(Destination: PDAVSingleFixedArray; Index: Integer; SampleFrames: Integer); virtual;
    procedure MoveData64(Destination: PDAVDoubleFixedArray; Index: Integer; SampleFrames: Integer); virtual;
    procedure AssignData32(Source: PDAVSingleFixedArray; Index: Integer; SampleFrames: Integer); virtual;
    procedure AssignData64(Source: PDAVDoubleFixedArray; Index: Integer; SampleFrames: Integer); virtual;
    function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; overload; override;
    constructor Create(Azimuth, Polar: Single; const SampleRate: Single;
      const SampleFrames: Integer; const Left, Right: PDAVHalfFloatFixedArray); reintroduce; overload; virtual;
    constructor Create(Azimuth, Polar: Single; const SampleRate: Single;
      const SampleFrames: Integer; const Left, Right: PDAVSingleFixedArray); reintroduce; overload; virtual;
    constructor Create(Azimuth, Polar: Single; const SampleRate: Single;
      const SampleFrames: Integer; const Left, Right: PDAVDoubleFixedArray); reintroduce; overload; virtual;
    constructor Create(Azimuth, Polar, Radius: Single; const SampleRate: Single;
      const SampleFrames: Integer; const Left, Right: PDAVHalfFloatFixedArray); reintroduce; overload; virtual;
    constructor Create(Azimuth, Polar, Radius: Single; const SampleRate: Single;
      const SampleFrames: Integer; const Left, Right: PDAVSingleFixedArray); reintroduce; overload; virtual;
    constructor Create(Azimuth, Polar, Radius: Single; const SampleRate: Single;
      const SampleFrames: Integer; const Left, Right: PDAVDoubleFixedArray); reintroduce; overload; virtual;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SwapChannels;
    procedure MoveLeft32(Destination: PDAVSingleFixedArray; SampleFrames: Integer); virtual;
    procedure MoveRight32(Destination: PDAVSingleFixedArray; SampleFrames: Integer); virtual;
    procedure MoveLeft64(Destination: PDAVDoubleFixedArray; SampleFrames: Integer); virtual;
    procedure MoveRight64(Destination: PDAVDoubleFixedArray; SampleFrames: Integer); virtual;
    procedure AssignLeft32(Source: PDAVSingleFixedArray; SampleFrames: Integer); virtual;
    procedure AssignLeft64(Source: PDAVDoubleFixedArray; SampleFrames: Integer); virtual;
    procedure AssignRight32(Source: PDAVSingleFixedArray; SampleFrames: Integer); virtual;
    procedure AssignRight64(Source: PDAVDoubleFixedArray; SampleFrames: Integer); virtual;
    class function GetClassChunkName: TChunkName; override;

    property Position: TSphereVector3D read GetPosition;
  published
    property Azimuth: Single read FHrirHeader.Position.Azimuth; 
    property Polar: Single read FHrirHeader.Position.Polar;
    property Encoding: THrirEncoding read FHrirHeader.Encoding;
    property BytesPerSample: Integer read FHrirHeader.BytesPerSample write SetBytesPerSample;
    property SampleFrames: Integer read FHrirHeader.SampleFrames write SetSampleFrames;
    property SampleRate: Single read FHrirHeader.SampleRate;
  end;

  //////////////////////////////////////////////////////////////////////////////

  THrirGeneralInformationRecord = record
    Title     : ShortString;
    Date      : TDateTime;
    Context   : ShortString;
    Copyright : ShortString;
    Author    : ShortString;
    Notes     : ShortString;
  end;

  TCustomHrirGeneralInformation = class(TDefinedChunk)
  private
    function GetString(const Index: Integer): string;
    procedure SetString(const Index: Integer; const Value: string);
  protected
    FGeneralInformationRecord : THrirGeneralInformationRecord;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property Title: string index 0 read GetString write SetString;
    property Date: TDateTime read FGeneralInformationRecord.Date write FGeneralInformationRecord.Date;
    property Context: string index 1 read GetString write SetString;
    property Copyright: string index 2 read GetString write SetString;
    property Author: string index 3 read GetString write SetString;
    property Notes: string index 4 read GetString write SetString;
  end;

  //////////////////////////////////////////////////////////////////////////////

  {$Z1}
  THrirSexType = (stUnknown, stGeneric, stMale, stFemale);
  THrirSubjectRecord = record
    ID     : ShortString;
    Sex    : THrirSexType;
    Desc   : ShortString;
  end;

  TCustomHrirSubjectInformation = class(TDefinedChunk)
  private
    function GetString(const Index: Integer): string;
    procedure SetString(const Index: Integer; const Value: string);
  protected
    FSubjectRecord : THrirSubjectRecord;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property ID: string index 0 read GetString write SetString;
    property Sex: THrirSexType read FSubjectRecord.Sex write FSubjectRecord.Sex;
    property Description: string index 1 read GetString write SetString;
  end;

  //////////////////////////////////////////////////////////////////////////////

  THrirRoomRecord = record
    X, Y, Z  : Single;
    RoomType : ShortString;
  end;

  TCustomHrirRoomInformation = class(TDefinedChunk)
  private
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
    procedure SetZ(const Value: Single);
    function GetRoomType: string;
    procedure SetRoomType(const Value: string);
  protected
    FRoomRecord : THrirRoomRecord;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property X: Single read FRoomRecord.X write SetX;
    property Y: Single read FRoomRecord.Y write SetY;
    property Z: Single read FRoomRecord.Z write SetZ;
    property RoomType: string read GetRoomType write SetRoomType;
  end;

  //////////////////////////////////////////////////////////////////////////////

  THrirMicrophoneRecord = record
    MicType      : ShortString;
    Manufacturer : ShortString;
    Notes        : ShortString;
  end;

  TCustomHrirMicrophoneInformation = class(TDefinedChunk)
  private
    function GetString(const Index: Integer): string;
    procedure SetString(const Index: Integer; const Value: string);
  protected
    FMicrophoneRecord : THrirMicrophoneRecord;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property MicType: string index 0 read GetString write SetString;
    property Manufacturer: string index 1 read GetString write SetString;
    property Notes: string index 2 read GetString write SetString;
  end;

  //////////////////////////////////////////////////////////////////////////////

  THrirOutboardRecord = record
    ADConverter : ShortString;
    DAConverter : ShortString;
    Amplifier   : ShortString;
    Loudspeaker : ShortString;
  end;

  TCustomHrirOutboardInformation = class(TDefinedChunk)
  private
    function GetString(const Index: Integer): string;
    procedure SetString(const Index: Integer; const Value: string);
  protected
    FOutboardRecord : THrirOutboardRecord;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property ADConverter: string index 0 read GetString write SetString;
    property DAConverter: string index 1 read GetString write SetString;
    property Amplifier: string index 2 read GetString write SetString;
    property Loudspeaker: string index 3 read GetString write SetString;
  end;

  //////////////////////////////////////////////////////////////////////////////

  THrirMeasurementRecord = record
    Distance        : Single;
    MeasurementType : ShortString;
    MeasuredLength  : Integer;
    ExcitationType  : ShortString;
    Flags           : Integer;
  end;

  TCustomHrirMeasurementInformation = class(TDefinedChunk)
  private
    function GetString(const Index: Integer): string;
    function GetSymmetric: Boolean;
    procedure SetMeasuredLength(const Value: Integer);
    procedure SetString(const Index: Integer; const Value: string);
    procedure SetSymmetric(const Value: Boolean);
  protected
    FMeasurementRecord : THrirMeasurementRecord;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property Distance: Single read FMeasurementRecord.Distance write FMeasurementRecord.Distance;
    property MeasurementType: string index 0 read GetString write SetString;
    property MeasuredLength: Integer read FMeasurementRecord.MeasuredLength write SetMeasuredLength;
    property ExcitationType: string index 1 read GetString write SetString;
    property Symmetric: Boolean read GetSymmetric write SetSymmetric;
  end;


(*
  //////////////////////////////////////////////////////////////////////////////

  THrirBitmapRecord = record
    Width   : Integer;
    Height  : Integer;
    Future1 : array[0..1023] of Integer; //Thumbnail
    Future2 : array[0..63] of Integer;
  end;
*)

  TCustomHrtfs = class(TChunkContainer)
  private
    FOnHrtfChanged     : TNotifyEvent;
    FInterpolationType : TInterpolationType;
    function GetDate: TDateTime;
    function GetDistance: Single;
    function GetGeneralInfoString(const Index: Integer): String;
    function GetMeasuredLength: Integer;
    function GetMeasurementString(const Index: Integer): String;
    function GetMicString(const Index: Integer): String;
    function GetOutboardString(const Index: Integer): String;
    function GetRoomDim(const Index: Integer): Single;
    function GetRoomType: String;
    function GetSex: THrirSexType;
    function GetSubjectString(const Index: Integer): String;
    function GetSymmetric: Boolean;
    function GetHrir(Index: Integer): TCustomHrir;
    function GetHrirCount: Integer;
    procedure SetDate(const Value: TDateTime);
    procedure SetDistance(const Value: Single);
    procedure SetGeneralInfoString(const Index: Integer; const Value: String);
    procedure SetMeasuredLength(const Value: Integer);
    procedure SetMeasurementString(const Index: Integer; const Value: String);
    procedure SetMicString(const Index: Integer; const Value: String);
    procedure SetOutboardString(const Index: Integer; const Value: String);
    procedure SetRoomDim(const Index: Integer; const Value: Single);
    procedure SetRoomType(const Value: String);
    procedure SetSex(const Value: THrirSexType);
    procedure SetSubjectString(const Index: Integer; const Value: String);
    procedure SetSymmetric(const Value: Boolean);
    procedure CalculateScaleFactors(const SpherePos: TSphereVector3D;
      const A, B, C: TSphereVector3D; var ScaleA, ScaleB, ScaleC: Double); overload;
    procedure CalculateScaleFactors(const SpherePos: TSphereVector3D;
      const A, B: TSphereVector3D; var ScaleA, ScaleB: Double); overload;
  protected
    FGeneralInformation     : TCustomHrirGeneralInformation;
    FSubjectInformation     : TCustomHrirSubjectInformation;
    FRoomInformation        : TCustomHrirRoomInformation;
    FMicrophoneInformation  : TCustomHrirMicrophoneInformation;
    FOutboardInformation    : TCustomHrirOutboardInformation;
    FMeasurementInformation : TCustomHrirMeasurementInformation;
    FHrirList               : TObjectList;
    FSampleRate             : Single;
    procedure ConvertStreamToChunk(ChunkClass: TCustomChunkClass;
      Stream: TStream); override;
    procedure Interpolate2Hrir(const Azimuth, Polar: Single;
      const SampleFrames: Integer;
      const Left, Right: PDavSingleFixedArray); overload; virtual;
    procedure Interpolate2Hrir(const Azimuth, Polar: Single;
      const SampleFrames: Integer;
      const Left, Right: PDavDoubleFixedArray); overload; virtual;
    procedure Interpolate3Hrir(const Azimuth, Polar: Single;
      const SampleFrames: Integer;
      const Left, Right: PDavSingleFixedArray); overload; virtual;
    procedure Interpolate3Hrir(const Azimuth, Polar: Single;
      const SampleFrames: Integer;
      const Left, Right: PDavDoubleFixedArray); overload; virtual;
    function FindNearestHrirs(const SpherePos: TSphereVector2D): TCustomHrir; overload;
    function FindNearestHrirs(const SpherePos: TSphereVector3D): TCustomHrir; overload;
    function FindSecondNearestHrirs(const SpherePos: TSphereVector2D;
      const Nearest: TCustomHrir): TCustomHrir; overload; virtual;
    function FindSecondNearestHrirs(const SpherePos: TSphereVector3D;
      const Nearest: TCustomHrir): TCustomHrir; overload; virtual;
    procedure FindNearestHrirs(const SpherePos: TSphereVector2D;
      var A, B, C: TCustomHrir); overload; virtual;
    procedure FindNearestHrirs(const SpherePos: TSphereVector3D;
      var A, B, C: TCustomHrir); overload; virtual;
    function GetChunkSize: Cardinal; override;
    function GetMaximumHrirSize: Integer; virtual;
    function GetMinimumHrirSize: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SwapChannels;
    procedure Clear;
    procedure ClearInformationChunks; virtual;
    procedure ClearHrirs; virtual;

    procedure InterpolateHrir(const Azimuth, Polar: Single;
      const SampleFrames: Integer; const Left, Right: PDavSingleFixedArray); overload; virtual;
    procedure InterpolateHrir(const Azimuth, Polar: Single;
      const SampleFrames: Integer; const Left, Right: PDavDoubleFixedArray); overload; virtual;

    procedure GetHrirByIndex(const Index: Integer; const SampleFrames: Integer;
      const Left, Right: PDavSingleFixedArray); overload; virtual;
    procedure GetHrirByIndex(const Index: Integer; const SampleFrames: Integer;
      const Left, Right: PDavDoubleFixedArray); overload; virtual;

    class function GetClassChunkName: TChunkName; override;
    procedure AddChunk(Chunk: TCustomChunk); override;

    property Hrir[Index: Integer]: TCustomHrir read GetHrir;
    property HrirCount: Integer read GetHrirCount;
    property Title: String index 0 read GetGeneralInfoString write SetGeneralInfoString;
    property Date: TDateTime read GetDate write SetDate;
    property Context: String index 1 read GetGeneralInfoString write SetGeneralInfoString;
    property Copyright: String index 2 read GetGeneralInfoString write SetGeneralInfoString;
    property Author: String index 3 read GetGeneralInfoString write SetGeneralInfoString;
    property Notes: String index 4 read GetGeneralInfoString write SetGeneralInfoString;
    property SubjectID: String index 0 read GetSubjectString write SetSubjectString;
    property SubjectSex: THrirSexType read GetSex write SetSex;
    property SubjectDescription: String index 1 read GetSubjectString write SetSubjectString;
    property RoomType: String read GetRoomType write SetRoomType;
    property RoomLength: Single index 0 read GetRoomDim write SetRoomDim;
    property RoomWidth: Single index 1 read GetRoomDim write SetRoomDim;
    property RoomHeight: Single index 2 read GetRoomDim write SetRoomDim;
    property MicType: String index 0 read GetMicString write SetMicString;
    property MicManufacturer: String index 1 read GetMicString write SetMicString;
    property MicNotes: String index 2 read GetMicString write SetMicString;
    property ADConverter: String index 0 read GetOutboardString write SetOutboardString;
    property DAConverter: String index 1 read GetOutboardString write SetOutboardString;
    property Amplifier: String index 2 read GetOutboardString write SetOutboardString;
    property Loudspeaker: String index 3 read GetOutboardString write SetOutboardString;
    property Distance: Single read GetDistance write SetDistance;
    property MeasurementType: String index 0 read GetMeasurementString write SetMeasurementString;
    property ExcitationType: String index 1 read GetMeasurementString write SetMeasurementString;
    property MeasuredLength: Integer read GetMeasuredLength write SetMeasuredLength;
    property Symmetric: Boolean read GetSymmetric write SetSymmetric;
    property SampleRate: Single read FSampleRate write FSampleRate;
    property InterpolationType: TInterpolationType read FInterpolationType write FInterpolationType default itLinear;
    property MaximumHrirSize: Integer read GetMaximumHrirSize;
    property MinimumHrirSize: Integer read GetMinimumHrirSize;

    property OnHrtfChanged: TNotifyEvent read FOnHrtfChanged write FOnHrtfChanged;
  end;

  THrtfs = class(TCustomHrtfs)
  published
    property Title;
    property Date;
    property Context;
    property Copyright;
    property Author;
    property Notes;
    property SubjectID;
    property SubjectSex;
    property SubjectDescription;
    property RoomType;
    property RoomLength;
    property RoomWidth;
    property RoomHeight;
    property MicType;
    property MicManufacturer;
    property ADConverter;
    property DAConverter;
    property Amplifier;
    property Loudspeaker;
    property Distance;
    property MeasurementType;
    property ExcitationType;
    property MeasuredLength;
    property SampleRate;
    property InterpolationType;
  end;

(*
  public
    constructor Create; overload;
    constructor Create(FileName: TFileName); overload;
    destructor Destroy; override;
    procedure RotateField(Degree: Single);
    procedure GetHRTF(const Horizontal, Vertical, Depth: Single; var HRTF: THRTFArray);

    function Add(Item: THRTFContent): Integer; reintroduce; overload;
    procedure Add(Horizontal, Vertical, Depth: Single; Data: THRTFArray); overload;
    procedure RecalcHorizontalHRTF;
    property Items[Index: Integer]: THRTFContent read Get write Put; default;
    property HorizontalHRTF: THorizontalHRTF read FHorizontal write FHorizontal;
//    property CurrentHRTF: THRTFArray read FCurrent write FCurrent;
  end;
*)

implementation

uses
  Math, DAV_Common, DAV_Complex, DAV_Approximations;

resourcestring
  RCStrPositiveValueOnly = 'Value must be larger than 0!';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrChunkAlreadyExists = 'Chunk already exists';

  
{ TCustomHrir }

constructor TCustomHrir.Create;
begin
 inherited;
 FChunkName := GetClassChunkName;
 FillChar(FHrirHeader, SizeOf(THrirHeader), 0);
 with FHrirHeader do
  begin
   SampleFrames   := 512;
   SampleRate     := 44100;
   Encoding       := heFloat;
   BytesPerSample := 4;
  end;
 CreateBuffers;
end;

constructor TCustomHrir.Create(Azimuth, Polar, Radius: Single;
  const SampleRate: Single; const SampleFrames: Integer; const Left,
  Right: PDAVHalfFloatFixedArray);
begin
 inherited Create;
 FChunkName := GetClassChunkName;
 FillChar(FHrirHeader, SizeOf(THrirHeader), 0);

 FHrirHeader.SampleFrames     := SampleFrames;
 FHrirHeader.Position.Azimuth := Azimuth;
 FHrirHeader.Position.Polar   := Polar;
 FHrirHeader.SampleRate       := SampleRate;
 with FHrirHeader do
  begin
   Encoding       := heFloat;
   BytesPerSample := 2;
  end;
 CreateBuffers;
 Move(Left^[0], PDAVHalfFloatFixedArray(FBuffer[0])^[0], SampleFrames * SizeOf(THalfFloat));
 Move(Right^[0], PDAVHalfFloatFixedArray(FBuffer[1])^[0], SampleFrames * SizeOf(THalfFloat));
 FChunkSize := GetChunkSize;
end;

constructor TCustomHrir.Create(Azimuth, Polar, Radius: Single;
  const SampleRate: Single; const SampleFrames: Integer; const Left,
  Right: PDAVSingleFixedArray);
begin
 inherited Create;
 FChunkName := GetClassChunkName;
 FillChar(FHrirHeader, SizeOf(THrirHeader), 0);

 FHrirHeader.SampleFrames     := SampleFrames;
 FHrirHeader.Position.Azimuth := Azimuth;
 FHrirHeader.Position.Polar   := Polar;
 FHrirHeader.Position.Radius  := Radius;
 FHrirHeader.SampleRate       := SampleRate;
 with FHrirHeader do
  begin
   Encoding       := heFloat;
   BytesPerSample := 4;
  end;
 CreateBuffers;
 Move(Left^[0], PDAVSingleFixedArray(FBuffer[0])^[0], SampleFrames * SizeOf(Single));
 Move(Right^[0], PDAVSingleFixedArray(FBuffer[1])^[0], SampleFrames * SizeOf(Single));
 FChunkSize := GetChunkSize;
end;

constructor TCustomHrir.Create(Azimuth, Polar, Radius: Single;
  const SampleRate: Single; const SampleFrames: Integer; const Left,
  Right: PDAVDoubleFixedArray);
begin
 inherited Create;
 FChunkName := GetClassChunkName;
 FillChar(FHrirHeader, SizeOf(THrirHeader), 0);

 FHrirHeader.SampleFrames     := SampleFrames;
 FHrirHeader.Position.Azimuth := Azimuth;
 FHrirHeader.Position.Polar   := Polar;
 FHrirHeader.Position.Radius  := Radius;
 FHrirHeader.SampleRate       := SampleRate;
 with FHrirHeader do
  begin
   Encoding       := heFloat;
   BytesPerSample := 8;
  end;
 CreateBuffers;
 Move(Left^[0], PDAVDoubleFixedArray(FBuffer[0])^[0], SampleFrames * SizeOf(Double));
 Move(Right^[0], PDAVDoubleFixedArray(FBuffer[1])^[0], SampleFrames * SizeOf(Double));
 FChunkSize := GetChunkSize;
end;

constructor TCustomHrir.Create(Azimuth, Polar: Single;
  const SampleRate: Single; const SampleFrames: Integer; const Left,
  Right: PDAVHalfFloatFixedArray);
begin
 Create(Azimuth, Polar, 1, SampleRate, SampleFrames, Left, Right);
end;

constructor TCustomHrir.Create(Azimuth, Polar: Single;
  const SampleRate: Single; const SampleFrames: Integer;
  const Left, Right: PDAVSingleFixedArray);
begin
 Create(Azimuth, Polar, 1, SampleRate, SampleFrames, Left, Right);
end;

constructor TCustomHrir.Create(Azimuth, Polar: Single;
  const SampleRate: Single; const SampleFrames: Integer;
  const Left, Right: PDAVDoubleFixedArray);
begin
 Create(Azimuth, Polar, 1, SampleRate, SampleFrames, Left, Right);
end;

destructor TCustomHrir.Destroy;
begin
 Dispose(FBuffer[0]);
 Dispose(FBuffer[1]);
 inherited;
end;

procedure TCustomHrir.CreateBuffers;
begin
 ReallocMem(FBuffer[0], SampleFrames * BytesPerSample);
 ReallocMem(FBuffer[1], SampleFrames * BytesPerSample);
end;

procedure TCustomHrir.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomHrir then
  with TCustomHrir(Dest) do
   begin
    FHrirHeader := Self.FHrirHeader;
    CreateBuffers;
    Self.MoveData32(FBuffer[0], 0, Self.SampleFrames * Self.BytesPerSample);
    Self.MoveData32(FBuffer[1], 1, Self.SampleFrames * Self.BytesPerSample);
   end;
end;

function TCustomHrir.GetChunkSize: Cardinal;
begin
 with FHrirHeader
  do Result := SizeOf(FHrirHeader) + 2 * SampleFrames * BytesPerSample;
end;

class function TCustomHrir.GetClassChunkName: TChunkName;
begin
 Result := 'hrir';
end;

function TCustomHrir.GetPosition: TSphereVector3D;
begin
 Result := FHrirHeader.Position;
end;

procedure TCustomHrir.SwapChannels;
var
  Temp : Pointer;
begin
 Temp := FBuffer[0];
 FBuffer[0] := FBuffer[1];
 FBuffer[1] := Temp;
end;

procedure TCustomHrir.SetBytesPerSample(const Value: Integer);
begin
 if FHrirHeader.BytesPerSample <> Value then
  begin
   FHrirHeader.BytesPerSample := Value;
   CreateBuffers;
  end;
end;

procedure TCustomHrir.SetSampleFrames(const Value: Integer);
begin
 if FHrirHeader.SampleFrames <> Value then
  begin
   FHrirHeader.SampleFrames := Value;
   CreateBuffers;
  end;
end;

procedure TCustomHrir.MoveData32(Destination: PDAVSingleFixedArray; Index,
  SampleFrames: Integer);
var
  Sample : Integer;
begin
 Assert(Index in [0..1]);

 // eventually zero pad IR
 if Self.SampleFrames < SampleFrames then
  begin
   FillChar(Destination^[Self.SampleFrames], (SampleFrames - Self.SampleFrames) *
     SizeOf(Single), 0);
   SampleFrames := Self.SampleFrames;
  end;

 // decode
 case Encoding of
  heFloat:
   case BytesPerSample of
    2 : for Sample := 0 to SampleFrames - 1
         do Destination^[Sample] := HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer[Index])^[Sample]);
    4 : move(FBuffer[Index]^, Destination^[0], SampleFrames * SizeOf(Single));
    8 : for Sample := 0 to SampleFrames - 1
         do Destination^[Sample] := PDAVDoubleFixedArray(FBuffer[Index])^[Sample];
    else raise Exception.Create('not yet implemented');
   end;
  heInteger : raise Exception.Create('not yet implemented');
 end;
end;

procedure TCustomHrir.MoveData64(Destination: PDAVDoubleFixedArray; Index,
  SampleFrames: Integer);
var
  Sample : Integer;
begin
 Assert(Index in [0..1]);

 // eventually zero pad IR
 if Self.SampleFrames < SampleFrames then
  begin
   FillChar(Destination^[Self.SampleFrames], (SampleFrames - Self.SampleFrames) *
     SizeOf(Single), 0);
   SampleFrames := Self.SampleFrames;
  end;

 // decode
 case Encoding of
  heFloat:
   case BytesPerSample of
    2 : for Sample := 0 to SampleFrames - 1
         do Destination^[Sample] := HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer[Index])^[Sample]);
    4 : for Sample := 0 to SampleFrames - 1
         do Destination^[Sample] := PDAVSingleFixedArray(FBuffer[Index])^[Sample];
    8 : move(FBuffer[Index]^, Destination^[0], SampleFrames * SizeOf(Single));
    else raise Exception.Create('not yet implemented');
   end;
  heInteger : raise Exception.Create('not yet implemented');
 end;
end;

procedure TCustomHrir.MoveLeft32(Destination: PDAVSingleFixedArray;
  SampleFrames: Integer);
begin
 MoveData32(Destination, 0, SampleFrames);
end;

procedure TCustomHrir.MoveRight32(Destination: PDAVSingleFixedArray;
  SampleFrames: Integer);
begin
 MoveData32(Destination, 1, SampleFrames);
end;

procedure TCustomHrir.MoveLeft64(Destination: PDAVDoubleFixedArray;
  SampleFrames: Integer);
begin
 MoveData64(Destination, 0, SampleFrames);
end;

procedure TCustomHrir.MoveRight64(Destination: PDAVDoubleFixedArray;
  SampleFrames: Integer);
begin
 MoveData64(Destination, 1, SampleFrames);
end;

procedure TCustomHrir.AssignData32(Source: PDAVSingleFixedArray; Index,
  SampleFrames: Integer);
var
  Sample : Integer;
begin
 Assert(Index in [0..1]);

 // decode
 case Encoding of
  heFloat:
   case BytesPerSample of
    2 : for Sample := 0 to SampleFrames - 1
         do PDAVHalfFloatFixedArray(FBuffer[Index])^[Sample] := SingleToHalfFloat(Source^[Sample]);
    4 : move(Source^[0], FBuffer[Index]^, SampleFrames * SizeOf(Single));
    8 : for Sample := 0 to SampleFrames - 1
         do PDAVDoubleFixedArray(FBuffer[Index])^[Sample] := Source^[Sample];
    else raise Exception.Create('not yet implemented');
   end;
  heInteger : raise Exception.Create('not yet implemented');
 end;
end;

procedure TCustomHrir.AssignData64(Source: PDAVDoubleFixedArray; Index,
  SampleFrames: Integer);
var
  Sample : Integer;
begin
 Assert(Index in [0..1]);

 // decode
 case Encoding of
  heFloat:
   case BytesPerSample of
    2 : raise Exception.Create('not yet implemented');
    4 : for Sample := 0 to SampleFrames - 1
         do PDAVSingleFixedArray(FBuffer[Index])^[Sample] := Source^[Sample];
    8 : move(Source^[0], FBuffer[Index]^, SampleFrames * SizeOf(Single));
    else raise Exception.Create('not yet implemented');
   end;
  heInteger : raise Exception.Create('not yet implemented');
 end;
end;

procedure TCustomHrir.AssignLeft32(Source: PDAVSingleFixedArray;
  SampleFrames: Integer);
begin
 AssignData32(Source, 0, SampleFrames);
end;

procedure TCustomHrir.AssignRight32(Source: PDAVSingleFixedArray;
  SampleFrames: Integer);
begin
 AssignData32(Source, 1, SampleFrames);
end;

procedure TCustomHrir.AssignLeft64(Source: PDAVDoubleFixedArray;
  SampleFrames: Integer);
begin
 AssignData64(Source, 0, SampleFrames);
end;

procedure TCustomHrir.AssignRight64(Source: PDAVDoubleFixedArray;
  SampleFrames: Integer);
begin
 AssignData64(Source, 1, SampleFrames);
end;

procedure TCustomHrir.LoadFromStream(Stream: TStream);
begin
 inherited;
 with Stream do
  begin
   // read header
   Read(FHrirHeader, SizeOf(THrirHeader));
   CreateBuffers;

   // check constraints
   Assert(Integer(FChunkSize) - (SizeOf(THrirHeader)) >= 2 * SampleFrames * FHrirHeader.BytesPerSample);
   Assert(Size - Position >= 2 * SampleFrames * BytesPerSample);

   // read data
   Read(FBuffer[0]^, SampleFrames * BytesPerSample);
   Read(FBuffer[1]^, SampleFrames * BytesPerSample);
  end;
end;

procedure TCustomHrir.SaveToStream(Stream: TStream);
begin
 FChunkSize := SizeOf(THrirHeader) + 2 * SampleFrames * SizeOf(Single);
 inherited;
 with Stream do
  begin
   // write header
   Write(FHrirHeader, SizeOf(THrirHeader));

   // read data
   Write(FBuffer[0]^, SampleFrames * BytesPerSample);
   Write(FBuffer[1]^, SampleFrames * BytesPerSample);
  end;
end;

{ TCustomHrirGeneralInformation }

constructor TCustomHrirGeneralInformation.Create;
begin
 inherited;
 with FGeneralInformationRecord do
  begin
   Title     := '';
   Date      := Now;
   Context   := '';
   Copyright := '';
   Author    := '';
   Notes     := '';
  end;
end;

function TCustomHrirGeneralInformation.GetChunkSize: Cardinal;
begin
 // calculate chunk size
 with FGeneralInformationRecord
  do Result := 5 + Byte(Title[0]) + SizeOf(Date) + Byte(Context[0]) +
                   Byte(Copyright[0]) + Byte(Author[0]) + Byte(Notes[0]);
end;

class function TCustomHrirGeneralInformation.GetClassChunkName: TChunkName;
begin
 Result := 'hrgi';
end;

function TCustomHrirGeneralInformation.GetString(const Index: Integer): string;
begin
 case Index of
  0 : Result := string(FGeneralInformationRecord.Title);
  1 : Result := string(FGeneralInformationRecord.Context);
  2 : Result := string(FGeneralInformationRecord.Copyright);
  3 : Result := string(FGeneralInformationRecord.Author);
  4 : Result := string(FGeneralInformationRecord.Notes);
 end;
end;

procedure TCustomHrirGeneralInformation.SetString(const Index: Integer;
  const Value: string);
begin
 case Index of
  0 : FGeneralInformationRecord.Title := ShortString(Value);
  1 : FGeneralInformationRecord.Context := ShortString(Value);
  2 : FGeneralInformationRecord.Copyright := ShortString(Value);
  3 : FGeneralInformationRecord.Author := ShortString(Value);
  4 : FGeneralInformationRecord.Notes := ShortString(Value);
 end;
end;

procedure TCustomHrirGeneralInformation.LoadFromStream(Stream: TStream);
var
  StringSize : Byte;
begin
 inherited;
 with Stream, FGeneralInformationRecord do
  begin
   // read 'Title' string
   Read(StringSize, 1);
   Assert(StringSize + 5 + SizeOf(Date) <= FChunkSize);
   SetLength(Title, StringSize);
   Read(Title[1], StringSize);

   // read date
   Read(Date, SizeOf(Date));

   // read 'Context' string
   Read(StringSize, 1);
   Assert(StringSize + 5 + SizeOf(Date) + Byte(Title[0]) <= FChunkSize);
   SetLength(Context, StringSize);
   Read(Context[1], StringSize);

   // read 'Copyright' string
   Read(StringSize, 1);
   Assert(StringSize + 5 + SizeOf(Date) + Byte(Title[0]) +
     Byte(Context[0]) <= FChunkSize);
   SetLength(Copyright, StringSize);
   Read(Copyright[1], StringSize);

   // read 'Author' string
   Read(StringSize, 1);
   Assert(StringSize + 5 + SizeOf(Date) + Byte(Title[0]) + Byte(Context[0]) +
     Byte(Copyright[0]) <= FChunkSize);
   SetLength(Author, StringSize);
   Read(Author[1], StringSize);

   // read 'Loudspeaker' string
   Read(StringSize, 1);
   Assert(StringSize + 5 + SizeOf(Date) + Byte(Title[0]) + Byte(Context[0]) +
     Byte(Copyright[0]) + Byte(Author[0]) <= FChunkSize);
   SetLength(Notes, StringSize);
   Read(Notes[1], StringSize);
  end;
end;

procedure TCustomHrirGeneralInformation.SaveToStream(Stream: TStream);
begin
 // save current chunk size
 FChunkSize := GetChunkSize;

 // store basic chunk information
 inherited SaveToStream(Stream);

 with Stream, FGeneralInformationRecord do
  begin
   // write 'Title' string
   Write(Title[0], 1);
   Write(Title[1], Integer(Title[0]));

   // write date
   Write(Date, SizeOf(Date));

   // write 'Context' string
   Write(Context[0], 1);
   Write(Context[1], Integer(Context[0]));

   // write 'Copyright' string
   Write(Copyright[0], 1);
   Write(Copyright[1], Integer(Copyright[0]));

   // write 'Author' string
   Write(Author[0], 1);
   Write(Author[1], Integer(Author[0]));

   // write 'Notes' string
   Write(Notes[0], 1);
   Write(Notes[1], Integer(Notes[0]));
  end;
end;

{ TCustomHrirSubjectInformation }

constructor TCustomHrirSubjectInformation.Create;
begin
 inherited;
 with FSubjectRecord do
  begin
   ID   := '';
   Sex  := stUnknown;
   Desc := '';
  end;
end;

function TCustomHrirSubjectInformation.GetChunkSize: Cardinal;
begin
 // calculate chunk size
 with FSubjectRecord
  do Result := 2 + Byte(ID[0]) + SizeOf(Sex) + Byte(Desc[0]);
end;

class function TCustomHrirSubjectInformation.GetClassChunkName: TChunkName;
begin
 Result := 'hrsi';
end;

function TCustomHrirSubjectInformation.GetString(const Index: Integer): string;
begin
 case Index of
  0 : Result := string(FSubjectRecord.ID);
  1 : Result := string(FSubjectRecord.Desc);
 end;
end;

procedure TCustomHrirSubjectInformation.SetString(const Index: Integer;
  const Value: string);
begin
 case Index of
  0 : FSubjectRecord.ID := ShortString(Value);
  1 : FSubjectRecord.Desc := ShortString(Value);
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
 end;
end;

procedure TCustomHrirSubjectInformation.LoadFromStream(Stream: TStream);
var
  StringSize : Byte;
begin
 inherited;
 with Stream, FSubjectRecord do
  begin
   // read 'ID' string
   Read(StringSize, 1);
   Assert(StringSize + 2 <= FChunkSize);
   SetLength(ID, StringSize);
   Read(ID[1], StringSize);

   // read sex
   Read(FSubjectRecord.Sex, 1);

   // read 'Desc' string
   Read(StringSize, 1);
   Assert(StringSize + 2 + Byte(ID[0]) <= FChunkSize);
   SetLength(Desc, StringSize);
   Read(Desc[1], StringSize);
  end;
end;

procedure TCustomHrirSubjectInformation.SaveToStream(Stream: TStream);
begin
 // save current chunk size
 FChunkSize := GetChunkSize;

 // store basic chunk information
 inherited SaveToStream(Stream);

 with Stream, FSubjectRecord do
  begin
   // write 'ID' string
   Write(ID[0], 1);
   Write(ID[1], Integer(ID[0]));

   // write sex
   Write(Sex, 1);

   // write 'Desc' string
   Write(Desc[0], 1);
   Write(Desc[1], Integer(Desc[0]));
  end;
end;

{ TCustomHrirRoomInformation }

constructor TCustomHrirRoomInformation.Create;
begin
 inherited;
 with FRoomRecord do
  begin
   X := 0;
   Y := 0;
   Z := 0;
   RoomType := '';
  end;
end;

function TCustomHrirRoomInformation.GetChunkSize: Cardinal;
begin
 // calculate chunk size
 with FRoomRecord
  do Result := 3 * SizeOf(Single) + Byte(RoomType[0]) + 1;
end;

class function TCustomHrirRoomInformation.GetClassChunkName: TChunkName;
begin
 Result := 'hrri'
end;

function TCustomHrirRoomInformation.GetRoomType: string;
begin
 Result := string(FRoomRecord.RoomType);
end;

procedure TCustomHrirRoomInformation.SetRoomType(const Value: string);
begin
 FRoomRecord.RoomType := ShortString(Value);
end;

procedure TCustomHrirRoomInformation.LoadFromStream(Stream: TStream);
var
  StringSize : Byte;
begin
 inherited;
 with Stream, FRoomRecord do
  begin
   // read dimensions
   Read(X, SizeOf(Single));
   Read(Y, SizeOf(Single));
   Read(Z, SizeOf(Single));

   // read 'RoomType' string
   Read(StringSize, 1);
   Assert(StringSize + 3 * SizeOf(Single) <= FChunkSize);
   SetLength(RoomType, StringSize);
   Read(RoomType[1], StringSize);
  end;
end;

procedure TCustomHrirRoomInformation.SaveToStream(Stream: TStream);
begin
 // save current chunk size
 FChunkSize := GetChunkSize;

 // store basic chunk information
 inherited SaveToStream(Stream);

 with Stream, FRoomRecord do
  begin
   // write dimensions
   Write(X, SizeOf(Single));
   Write(Y, SizeOf(Single));
   Write(Z, SizeOf(Single));

   // write 'RoomType' string
   Write(RoomType[0], 1);
   Write(RoomType[1], Integer(RoomType[0]));
  end;
end;

procedure TCustomHrirRoomInformation.SetX(const Value: Single);
begin
 if Value >= 0
  then FRoomRecord.X := Value
  else raise Exception.Create(RCStrPositiveValueOnly);
end;

procedure TCustomHrirRoomInformation.SetY(const Value: Single);
begin
 if Value >= 0
  then FRoomRecord.Y := Value
  else raise Exception.Create(RCStrPositiveValueOnly);
end;

procedure TCustomHrirRoomInformation.SetZ(const Value: Single);
begin
 if Value >= 0
  then FRoomRecord.Z := Value
  else raise Exception.Create(RCStrPositiveValueOnly);
end;

{ TCustomHrirMicrophoneInformation }

constructor TCustomHrirMicrophoneInformation.Create;
begin
 inherited;
 with FMicrophoneRecord do
  begin
   MicType := '';
   Manufacturer := '';
   Notes := '';
  end;
end;

function TCustomHrirMicrophoneInformation.GetChunkSize: Cardinal;
begin
 // calculate chunk size
 with FMicrophoneRecord
  do Result := 3 + Byte(MicType[0]) + Byte(Manufacturer[0]) + Byte(Notes[0]);
end;

class function TCustomHrirMicrophoneInformation.GetClassChunkName: TChunkName;
begin
 Result := 'hrmi';
end;

function TCustomHrirMicrophoneInformation.GetString(
  const Index: Integer): string;
begin
 case Index of
  0 : Result := string(FMicrophoneRecord.MicType);
  1 : Result := string(FMicrophoneRecord.Manufacturer);
  2 : Result := string(FMicrophoneRecord.Notes);
 end;
end;

procedure TCustomHrirMicrophoneInformation.SetString(const Index: Integer;
  const Value: string);
begin
 case Index of
  0 : FMicrophoneRecord.MicType := ShortString(Value);
  1 : FMicrophoneRecord.Manufacturer := ShortString(Value);
  2 : FMicrophoneRecord.Notes := ShortString(Value);
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
 end;
end;

procedure TCustomHrirMicrophoneInformation.LoadFromStream(Stream: TStream);
var
  StringSize : Byte;
begin
 inherited;
 with Stream, FMicrophoneRecord do
  begin
   // read 'MicType' string
   Read(StringSize, 1);
   Assert(StringSize + 3 <= FChunkSize);
   SetLength(MicType, StringSize);
   Read(MicType[1], StringSize);

   // read 'Manufacturer' string
   Read(StringSize, 1);
   Assert(StringSize + 3 + Byte(MicType[0]) <= FChunkSize);
   SetLength(Manufacturer, StringSize);
   Read(Manufacturer[1], StringSize);

   // read 'Notes' string
   Read(StringSize, 1);
   Assert(StringSize + 3 + Byte(MicType[0]) + Byte(Manufacturer[0]) <= FChunkSize);
   SetLength(Notes, StringSize);
   Read(Notes[1], StringSize);
  end;
end;

procedure TCustomHrirMicrophoneInformation.SaveToStream(Stream: TStream);
begin
 // save current chunk size
 FChunkSize := GetChunkSize;

 // store basic chunk information
 inherited SaveToStream(Stream);

 with Stream, FMicrophoneRecord do
  begin
   // write 'MicType' string
   Write(MicType[0], 1);
   Write(MicType[1], Integer(MicType[0]));

   // write 'Manufacturer' string
   Write(Manufacturer[0], 1);
   Write(Manufacturer[1], Integer(Manufacturer[0]));

   // write 'Notes' string
   Write(Notes[0], 1);
   Write(Notes[1], Integer(Notes[0]));
  end;
end;

{ TCustomHrirOutboardInformation }

constructor TCustomHrirOutboardInformation.Create;
begin
 inherited;
 with FOutboardRecord do
  begin
   ADConverter := '';
   DAConverter := '';
   Amplifier := '';
   Loudspeaker := '';
  end;
end;

function TCustomHrirOutboardInformation.GetChunkSize: Cardinal;
begin
 with FOutboardRecord
  do Result := 4 + Byte(ADConverter[0]) + Byte(DAConverter[0]) +
       Byte(Amplifier[0]) + Byte(Loudspeaker[0]);
end;

class function TCustomHrirOutboardInformation.GetClassChunkName: TChunkName;
begin
 Result := 'hroi';
end;

function TCustomHrirOutboardInformation.GetString(const Index: Integer): string;
begin
 with FOutboardRecord do
  case Index of
   0 : Result := string(ADConverter);
   1 : Result := string(DAConverter);
   2 : Result := string(Amplifier);
   3 : Result := string(Loudspeaker);
  end;
end;

procedure TCustomHrirOutboardInformation.SetString(const Index: Integer;
  const Value: string);
begin
 with FOutboardRecord do
  case Index of
   0 : ADConverter := ShortString(Value);
   1 : DAConverter := ShortString(Value);
   2 : Amplifier := ShortString(Value);
   3 : Loudspeaker := ShortString(Value);
   else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
  end;
end;

procedure TCustomHrirOutboardInformation.LoadFromStream(Stream: TStream);
var
  StringSize : Byte;
begin
 inherited;
 with Stream, FOutboardRecord do
  begin
   // read 'ADConverter' string
   Read(StringSize, 1);
   Assert(StringSize + 3 < FChunkSize);
   SetLength(ADConverter, StringSize);
   Read(ADConverter[1], StringSize);

   // read 'DAConverter' string
   Read(StringSize, 1);
   Assert(StringSize + 3 + Byte(ADConverter[0]) < FChunkSize);
   SetLength(DAConverter, StringSize);
   Read(DAConverter[1], StringSize);

   // read 'Amplifier' string
   Read(StringSize, 1);
   Assert(StringSize + 3 + Byte(ADConverter[0]) + Byte(DAConverter[0]) < FChunkSize);
   SetLength(Amplifier, StringSize);
   Read(Amplifier[1], StringSize);

   // read 'Loudspeaker' string
   Read(StringSize, 1);
   Assert(StringSize + 3 + Byte(ADConverter[0]) + Byte(DAConverter[0]) + Byte(Amplifier[0]) < FChunkSize);
   SetLength(Loudspeaker, StringSize);
   Read(Loudspeaker[1], StringSize);
  end;
end;

procedure TCustomHrirOutboardInformation.SaveToStream(Stream: TStream);
begin
 // save current chunk size
 FChunkSize := GetChunkSize;

 inherited;
 with Stream, FOutboardRecord do
  begin
   // write 'ADConverter' string
   Write(ADConverter[0], 1);
   Write(ADConverter[1], Integer(ADConverter[0]));

   // write 'DAConverter' string
   Write(DAConverter[0], 1);
   Write(DAConverter[1], Integer(DAConverter[0]));

   // write 'Amplifier' string
   Write(Amplifier[0], 1);
   Write(Amplifier[1], Integer(Amplifier[0]));

   // write 'Loudspeaker' string
   Write(Loudspeaker[0], 1);
   Write(Loudspeaker[1], Integer(Loudspeaker[0]));
  end;
end;

{ TCustomHrirMeasurementInformation }

constructor TCustomHrirMeasurementInformation.Create;
begin
 inherited;
 with FMeasurementRecord do
  begin
   Distance := 0;
   MeasurementType := '';
   MeasuredLength := 0;
   ExcitationType := '';
  end;
end;

function TCustomHrirMeasurementInformation.GetChunkSize: Cardinal;
begin
 // calculate chunk size
 with FMeasurementRecord
  do Result := 2 + SizeOf(Distance) + Byte(MeasurementType[0]) +
       SizeOf(MeasuredLength) + Byte(ExcitationType[0]) + SizeOf(Integer);
end;

class function TCustomHrirMeasurementInformation.GetClassChunkName: TChunkName;
begin
 Result := 'hrme';
end;

function TCustomHrirMeasurementInformation.GetString(
  const Index: Integer): string;
begin
 case Index of
  0 : Result := string(FMeasurementRecord.MeasurementType);
  1 : Result := string(FMeasurementRecord.ExcitationType);
 end;
end;

function TCustomHrirMeasurementInformation.GetSymmetric: Boolean;
begin
 Result := (FMeasurementRecord.Flags and 1) > 0;
end;

procedure TCustomHrirMeasurementInformation.LoadFromStream(Stream: TStream);
var
  StringSize : Byte;
  StreamPos  : Int64;
begin
 // load basic chunk information
 inherited LoadFromStream(Stream);

 with Stream, FMeasurementRecord do
  begin
   // store stream position
   StreamPos := Position;

   // read distance
   Read(Distance, SizeOf(Distance));

   // read 'MeasurementType' string
   Read(StringSize, 1);
   Assert(StringSize + 2 + SizeOf(Distance) + SizeOf(MeasuredLength) < FChunkSize);
   SetLength(MeasurementType, StringSize);
   Read(MeasurementType[1], StringSize);

   // read measured length
   Read(MeasuredLength, SizeOf(MeasuredLength));

   // read 'ExcitationType' string
   Read(StringSize, 1);
   Assert(StringSize + 2 + SizeOf(Distance) + SizeOf(MeasuredLength) +
     Byte(MeasurementType[0]) <= FChunkSize);
   SetLength(ExcitationType, StringSize);
   Read(ExcitationType[1], StringSize);

   // read flags
   Read(Flags, 4);

   // reset stream position
   Position := (StreamPos + FChunkSize);
  end;
end;

procedure TCustomHrirMeasurementInformation.SaveToStream(Stream: TStream);
begin
 // save current chunk size
 FChunkSize := GetChunkSize;

 // store basic chunk information
 inherited SaveToStream(Stream);

 with Stream, FMeasurementRecord do
  begin
   // write distance
   Write(Distance, SizeOf(Distance));

   // write 'MeasurementType' string
   Write(MeasurementType[0], 1);
   Write(MeasurementType[1], Byte(MeasurementType[0]));

   // write measured length
   Write(MeasuredLength, SizeOf(MeasuredLength));

   // write 'ExcitationType' string
   Write(ExcitationType[0], 1);
   Write(ExcitationType[1], Byte(ExcitationType[0]));

   // write flags
   Write(Flags, SizeOf(Integer));
  end;
end;

procedure TCustomHrirMeasurementInformation.SetMeasuredLength(
  const Value: Integer);
begin
 if Value > 0
  then FMeasurementRecord.MeasuredLength := Value
  else raise Exception.Create(RCStrPositiveValueOnly);
end;

procedure TCustomHrirMeasurementInformation.SetString(const Index: Integer;
  const Value: string);
begin
 case Index of
  0 : FMeasurementRecord.MeasurementType := ShortString(Value);
  1 : FMeasurementRecord.ExcitationType := ShortString(Value);
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
 end;
end;

procedure TCustomHrirMeasurementInformation.SetSymmetric(const Value: Boolean);
begin
 if Value
  then FMeasurementRecord.Flags := 1
  else FMeasurementRecord.Flags := 0;
end;

{ TCustomHrtfs }

constructor TCustomHrtfs.Create;
begin
 inherited;
 RegisterChunkClasses([TCustomHrir, TCustomHrirGeneralInformation,
   TCustomHrirSubjectInformation, TCustomHrirRoomInformation,
   TCustomHrirMicrophoneInformation, TCustomHrirOutboardInformation,
   TCustomHrirMeasurementInformation]);
 FHrirList := TObjectList.Create(False);
 FInterpolationType := itLinear;
 FSampleRate := 44100;
end;

destructor TCustomHrtfs.Destroy;
begin
 FreeAndNil(FHrirList);
 inherited;
end;

function TCustomHrtfs.GetChunkSize: Cardinal;
begin
 Result := inherited GetChunkSize;
end;

class function TCustomHrtfs.GetClassChunkName: TChunkName;
begin
 Result := 'HRTF';
end;

function TCustomHrtfs.GetDate: TDateTime;
begin
 if Assigned(FGeneralInformation)
  then Result := FGeneralInformation.Date
  else Result := Now;
end;

function TCustomHrtfs.GetDistance: Single;
begin
 if Assigned(FMeasurementInformation)
  then Result := FMeasurementInformation.Distance
  else Result := 0;
end;

function TCustomHrtfs.GetGeneralInfoString(const Index: Integer): String;
begin
 if Assigned(FGeneralInformation) then
  with FGeneralInformation do
   case index of
    0 : Result := Title;
    1 : Result := Context;
    2 : Result := Copyright;
    3 : Result := Author;
    4 : Result := Notes;
    else Result := '';
   end
 else Result := '';
end;

procedure TCustomHrtfs.GetHrirByIndex(const Index: Integer;
  const SampleFrames: Integer; const Left, Right: PDavSingleFixedArray);
begin
 with TCustomHrir(FHrirList[Index]) do
  begin
   MoveLeft32(Left, SampleFrames);
   MoveRight32(Right, SampleFrames);
  end;
end;

function TCustomHrtfs.GetHrir(Index: Integer): TCustomHrir;
begin
 if Index in [0..(FHrirList.Count - 1)]
  then Result := TCustomHrir(FHrirList[Index])
  else Result := nil;
end;

procedure TCustomHrtfs.GetHrirByIndex(const Index: Integer;
  const SampleFrames: Integer; const Left, Right: PDavDoubleFixedArray);
begin
 with TCustomHrir(FHrirList[Index]) do
  begin
   MoveLeft64(Left, SampleFrames);
   MoveRight64(Right, SampleFrames);
  end;
end;

function TCustomHrtfs.GetHrirCount: Integer;
begin
 Result := FHrirList.Count;
end;

function TCustomHrtfs.GetMaximumHrirSize: Integer;
var
  Index : Integer;
begin
 Result := 0;
 for Index := 0 to FHrirList.Count - 1 do
  if TCustomHrir(FHrirList[Index]).SampleFrames > Result
   then Result := TCustomHrir(FHrirList[Index]).SampleFrames;
end;

function TCustomHrtfs.GetMinimumHrirSize: Integer;
var
  Index : Integer;
begin
 if FHrirList.Count = 0
  then Result := 0
  else
   begin
    Result := TCustomHrir(FHrirList[0]).SampleFrames;
    for Index := 1 to FHrirList.Count - 1 do
     if TCustomHrir(FHrirList[Index]).SampleFrames < Result
      then Result := TCustomHrir(FHrirList[Index]).SampleFrames;
   end;
end;

function TCustomHrtfs.GetMeasuredLength: Integer;
begin
 if Assigned(FMeasurementInformation)
  then Result := FMeasurementInformation.MeasuredLength
  else Result := 0;
end;

function TCustomHrtfs.GetMeasurementString(const Index: Integer): String;
begin
 Result := '';
 if Assigned(FMeasurementInformation) then
  with FMeasurementInformation do
   case Index of
    0 : Result := MeasurementType;
    1 : Result := ExcitationType;
   end;
end;

function TCustomHrtfs.GetMicString(const Index: Integer): String;
begin
 Result := '';
 if Assigned(FMicrophoneInformation) then
  with FMicrophoneInformation do
   case Index of
    0 : Result := MicType;
    1 : Result := Manufacturer;
    2 : Result := Notes;
   end;
end;

function TCustomHrtfs.GetOutboardString(const Index: Integer): String;
begin
 Result := '';
 if Assigned(FOutboardInformation) then
  with FOutboardInformation do
   case Index of
    0 : Result := ADConverter;
    1 : Result := DAConverter;
    2 : Result := Amplifier;
    3 : Result := Loudspeaker;
   end;
end;

function TCustomHrtfs.GetRoomDim(const Index: Integer): Single;
begin
 Result := 0;
 if Assigned(FRoomInformation) then
  with FRoomInformation do
   case Index of
    0 : Result := X;
    1 : Result := Y;
    2 : Result := Z;
   end;
end;

function TCustomHrtfs.GetRoomType: String;
begin
 if Assigned(FRoomInformation)
  then Result := FRoomInformation.RoomType
  else Result := '';
end;

function TCustomHrtfs.GetSex: THrirSexType;
begin
 if Assigned(FSubjectInformation)
  then Result := FSubjectInformation.Sex
  else Result := stUnknown;
end;

function TCustomHrtfs.GetSubjectString(const Index: Integer): String;
begin
 if Assigned(FSubjectInformation) then
  with FSubjectInformation do
   case index of
    0 : Result := ID;
    1 : Result := Description;
    else Result := '';
   end
 else Result := '';
end;

function TCustomHrtfs.GetSymmetric: Boolean;
begin
 if not Assigned(FMeasurementInformation)
  then Result := FMeasurementInformation.Symmetric
  else Result := False;
end;

procedure TCustomHrtfs.SetDate(const Value: TDateTime);
begin
 if not Assigned(FGeneralInformation) then
  begin
   FGeneralInformation := TCustomHrirGeneralInformation.Create;
   AddChunk(FGeneralInformation);
  end;
 FGeneralInformation.Date := Value;
end;

procedure TCustomHrtfs.SetDistance(const Value: Single);
begin
 if not Assigned(FMeasurementInformation) then
  begin
   FMeasurementInformation := TCustomHrirMeasurementInformation.Create;
   AddChunk(FMeasurementInformation);
  end;
 FMeasurementInformation.Distance := Value;
end;

procedure TCustomHrtfs.SetGeneralInfoString(const Index: Integer;
  const Value: String);
begin
 if not Assigned(FGeneralInformation) then
  begin
   if Value = '' then exit;
   FGeneralInformation := TCustomHrirGeneralInformation.Create;
   AddChunk(FGeneralInformation);
  end;
 if Assigned(FGeneralInformation) then
  with FGeneralInformation do
   case index of
    0 : Title := Value;
    1 : Context := Value;
    2 : Copyright := Value;
    3 : Author := Value;
    4 : Notes := Value;
    else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [index]);
   end;
end;

procedure TCustomHrtfs.SetMeasuredLength(const Value: Integer);
begin
 if not Assigned(FMeasurementInformation) then
  begin
   FMeasurementInformation := TCustomHrirMeasurementInformation.Create;
   AddChunk(FMeasurementInformation);
  end;
 FMeasurementInformation.MeasuredLength := Value;
end;

procedure TCustomHrtfs.SetMeasurementString(const Index: Integer;
  const Value: String);
begin
 if not Assigned(FMeasurementInformation) then
  begin
   if Value = '' then exit;
   FMeasurementInformation := TCustomHrirMeasurementInformation.Create;
   AddChunk(FMeasurementInformation);
  end;
 if Assigned(FMeasurementInformation) then
  with FMeasurementInformation do
   case index of
    0 : MeasurementType := Value;
    1 : ExcitationType := Value;
    else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [index]);
   end;
end;

procedure TCustomHrtfs.SetMicString(const Index: Integer; const Value: String);
begin
 if not Assigned(FMicrophoneInformation) then
  begin
   if Value = '' then exit;
   FMicrophoneInformation := TCustomHrirMicrophoneInformation.Create;
   AddChunk(FMicrophoneInformation);
  end;
 if Assigned(FMicrophoneInformation) then
  with FMicrophoneInformation do
   case index of
    0 : MicType := Value;
    1 : Manufacturer := Value;
    2 : Notes := Value;
    else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [index]);
   end;
end;

procedure TCustomHrtfs.SetOutboardString(const Index: Integer;
  const Value: String);
begin
 if not Assigned(FOutboardInformation) then
  begin
   if Value = '' then exit;
   FOutboardInformation := TCustomHrirOutboardInformation.Create;
   AddChunk(FOutboardInformation);
  end;
 if Assigned(FOutboardInformation) then
  with FOutboardInformation do
   case index of
    0 : ADConverter := Value;
    1 : DAConverter := Value;
    2 : Amplifier := Value;
    3 : Loudspeaker := Value;
    else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [index]);
   end;
end;

procedure TCustomHrtfs.SetRoomDim(const Index: Integer; const Value: Single);
begin
 if not Assigned(FRoomInformation) then
  begin
   if Value = 0 then exit;
   FRoomInformation := TCustomHrirRoomInformation.Create;
   AddChunk(FRoomInformation);
  end;

 with FRoomInformation do
  case Index of
   0 : X := Value;
   1 : Y := Value;
   2 : Z := Value;
   else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [index]);
  end;
end;

procedure TCustomHrtfs.SetRoomType(const Value: String);
begin
 if not Assigned(FRoomInformation) then
  begin
   if Value = '' then exit;
   FRoomInformation := TCustomHrirRoomInformation.Create;
   AddChunk(FRoomInformation);
  end;
 FRoomInformation.RoomType := Value;
end;

procedure TCustomHrtfs.SetSex(const Value: THrirSexType);
begin
 if not Assigned(FSubjectInformation) then
  begin
   if Value = stUnknown then exit;
   FSubjectInformation := TCustomHrirSubjectInformation.Create;
   AddChunk(FSubjectInformation);
  end;
 FSubjectInformation.Sex := Value;
end;

procedure TCustomHrtfs.SetSubjectString(const Index: Integer;
  const Value: String);
begin
 if not Assigned(FSubjectInformation) then
  begin
   if Value = '' then exit;
   FSubjectInformation := TCustomHrirSubjectInformation.Create;
   AddChunk(FSubjectInformation);
  end;
 if Assigned(FSubjectInformation) then
  with FSubjectInformation do
   case index of
    0 : ID := Value;
    1 : Description := Value;
    else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [index]);
   end;
end;

procedure TCustomHrtfs.SetSymmetric(const Value: Boolean);
begin
 if not Assigned(FMeasurementInformation) then
  begin
   FMeasurementInformation := TCustomHrirMeasurementInformation.Create;
   AddChunk(FMeasurementInformation);
  end;
 FMeasurementInformation.Symmetric := Value;
end;

function GetOrthodromicAngle2DWithoutArcCos(A, B: TSphereVector3D): Double;
var
  CosAzimuth : Double;
begin
 CosAzimuth := cos(A.Azimuth - B.Azimuth);
 Result := 0.5 * ((cos(A.Polar - B.Polar) * (CosAzimuth + 1) +
                   cos(A.Polar + B.Polar) * (CosAzimuth - 1)));
end;

function GetSphericTriangleAngleNoCos(A, B, C: Double): Double;
begin
 Result := arccos((A - B * C) / (sqrt((1 - sqr(B)) * (1 - sqr(C)))));
end;

procedure TCustomHrtfs.CalculateScaleFactors(const SpherePos, A,
  B: TSphereVector3D; var ScaleA, ScaleB: Double);
var
  HrirAngle : Double;
  Angle     : array [0..1] of Single;
begin
 // calculate vector distances
 HrirAngle := GetOrthodromicAngle2D(A, B);
 Angle[0]  := GetOrthodromicAngle2D(A, SpherePos);
 Angle[1]  := GetOrthodromicAngle2D(B, SpherePos);

 // calculate wheighting (todo: check whether still valid!)
// ScaleA := (sqr(Angle[1]) + sqr(HrirAngle) - sqr(Angle[0])) / HrirAngle;
 ScaleA := Angle[1] / (Angle[0] + Angle[1]); 
 ScaleB := 1 - ScaleA;

 // verify constraints
 Assert(ScaleA < 1);
end;

procedure TCustomHrtfs.CalculateScaleFactors(const SpherePos, A, B,
  C: TSphereVector3D; var ScaleA, ScaleB, ScaleC: Double);
var
  AngleA       : array [0..2] of Double;
  PntA         : array [0..2] of Double;
  InnerAngles  : array [0..2] of Double;
  TotalAngle   : array [0..2] of Double;
  HalfAngles   : array [0..1, 0..2] of Double;
  SidePart     : array [0..1, 0..2] of Double;
begin
 // calculate orthodromic angle/distance to desired position (without arccos)
 AngleA[0] := GetOrthodromicAngle2DWithoutArcCos(A, SpherePos); // a'
 AngleA[1] := GetOrthodromicAngle2DWithoutArcCos(B, SpherePos); // b'
 AngleA[2] := GetOrthodromicAngle2DWithoutArcCos(C, SpherePos); // c'

 // calculate orthodromic angle/distance between Hrirs (without arccos)
 PntA[0] := GetOrthodromicAngle2DWithoutArcCos(B, C); // a
 PntA[1] := GetOrthodromicAngle2DWithoutArcCos(C, A); // b
 PntA[2] := GetOrthodromicAngle2DWithoutArcCos(A, B); // c

 // from cos(a) = cos(b) * cos(c) + sin(b) * sin(c) * cos(alpha)
 // <=>  sin(b) * sin(c) * cos(alpha) = cos(a) - cos(b) * cos(c)
 // <=>  cos(alpha) = (cos(a) - cos(b) * cos(c)) / (sin(b) * sin(c))
 // <=>  alpha = arccos((cos(a) - cos(b) * cos(c)) / (sin(b) * sin(c)))

 // calculate inner angles
 InnerAngles[0] := GetSphericTriangleAngleNoCos(PntA[0], AngleA[2], AngleA[1]);
 InnerAngles[1] := GetSphericTriangleAngleNoCos(PntA[1], AngleA[0], AngleA[2]);
 InnerAngles[2] := GetSphericTriangleAngleNoCos(PntA[2], AngleA[1], AngleA[0]);

 // check position lies inside the triangle
 Assert(abs( (InnerAngles[0] + InnerAngles[1] + InnerAngles[2]) - 2 * Pi) < 1E-5);

 // half angles (part 1)
 HalfAngles[0, 0] := GetSphericTriangleAngleNoCos(AngleA[0], AngleA[2], PntA[1]);
 HalfAngles[0, 1] := GetSphericTriangleAngleNoCos(AngleA[1], AngleA[0], PntA[2]);
 HalfAngles[0, 2] := GetSphericTriangleAngleNoCos(AngleA[2], AngleA[1], PntA[0]);

 // half angles (part 2)
 HalfAngles[1, 0] := GetSphericTriangleAngleNoCos(AngleA[0], PntA[2], AngleA[1]);
 HalfAngles[1, 1] := GetSphericTriangleAngleNoCos(AngleA[1], PntA[0], AngleA[2]);
 HalfAngles[1, 2] := GetSphericTriangleAngleNoCos(AngleA[2], PntA[1], AngleA[0]);

 // calculate angles
 TotalAngle[0] := GetSphericTriangleAngleNoCos(PntA[0], PntA[1], PntA[2]); // alpha
 TotalAngle[1] := GetSphericTriangleAngleNoCos(PntA[1], PntA[2], PntA[0]); // beta
 TotalAngle[2] := GetSphericTriangleAngleNoCos(PntA[2], PntA[0], PntA[1]); // gamma

 // check position lies inside the triangle
 Assert(abs((HalfAngles[0, 1] + HalfAngles[1, 2]) - TotalAngle[0]) < 1E-3);
 Assert(abs((HalfAngles[0, 2] + HalfAngles[1, 0]) - TotalAngle[1]) < 1E-3);
 Assert(abs((HalfAngles[0, 0] + HalfAngles[1, 1]) - TotalAngle[2]) < 1E-3);

(*
 // calculate triangle angles (using spherical trigonometry)
 Angles[0] := arccos((PntA[0] - PntA[1] * PntA[2]) / (sqrt(1 - sqr(PntA[1])) * sqrt(1 - sqr(PntA[2]))));
 Angles[1] := arccos((PntA[1] - PntA[2] * PntA[0]) / (sqrt(1 - sqr(PntA[2])) * sqrt(1 - sqr(PntA[0]))));
 Angles[2] := arccos((PntA[2] - PntA[0] * PntA[1]) / (sqrt(1 - sqr(PntA[0])) * sqrt(1 - sqr(PntA[1]))));

 // calculate triangle angles between (using spherical trigonometry)
 HalfAng[0] := arccos(Limit((AngleA[1] - AngleA[0] * PntA[2]) / (sqrt(1 - sqr(AngleA[0])) * sqrt(1 - sqr(PntA[2])))));
 HalfAng[1] := arccos(Limit((AngleA[2] - AngleA[1] * PntA[0]) / (sqrt(1 - sqr(AngleA[1])) * sqrt(1 - sqr(PntA[0])))));
 HalfAng[2] := arccos(Limit((AngleA[0] - AngleA[2] * PntA[1]) / (sqrt(1 - sqr(AngleA[2])) * sqrt(1 - sqr(PntA[1])))));
*)


 // calculate side parts
 // from cot(a) = (sin(beta) * cot(alpha) + cos(c) * cos(beta)) / sin(c)

 SidePart[0, 0] := arccot((sin(TotalAngle[2]) * cot(HalfAngles[0, 1]) + PntA[2] * cos(TotalAngle[2])) / sqrt(1 - sqr(PntA[2])));
 SidePart[0, 1] := arccot((sin(TotalAngle[0]) * cot(HalfAngles[0, 2]) + PntA[0] * cos(TotalAngle[0])) / sqrt(1 - sqr(PntA[0])));
 SidePart[0, 2] := arccot((sin(TotalAngle[1]) * cot(HalfAngles[0, 0]) + PntA[1] * cos(TotalAngle[1])) / sqrt(1 - sqr(PntA[1])));

 Assert(SidePart[0, 0] < arccos(PntA[0]));
 Assert(SidePart[0, 1] < arccos(PntA[1]));
 Assert(SidePart[0, 2] < arccos(PntA[2]));

 SidePart[1, 0] := arccos(PntA[0]) - SidePart[0, 0];
 SidePart[1, 1] := arccos(PntA[1]) - SidePart[0, 1];
 SidePart[1, 2] := arccos(PntA[2]) - SidePart[0, 2];

 ScaleA := sqrt(SidePart[1, 1] / arccos(PntA[1]) * SidePart[1, 2] / arccos(PntA[2]));
 ScaleB := sqrt(SidePart[1, 2] / arccos(PntA[2]) * SidePart[1, 0] / arccos(PntA[0]));
 ScaleC := sqrt(SidePart[1, 0] / arccos(PntA[0]) * SidePart[1, 1] / arccos(PntA[1]));

(*
 Scale[0] := HalfAngles[0, 0] * HalfAngles[1, 0] / (TotalAngle[1] * TotalAngle[2]);
 Scale[1] := HalfAngles[0, 1] * HalfAngles[1, 1] / (TotalAngle[2] * TotalAngle[0]);
 Scale[2] := HalfAngles[0, 2] * HalfAngles[1, 2] / (TotalAngle[0] * TotalAngle[1]);
*)

(*
 // verify scaling order
 Assert(Scale[0] > Scale[1]);
 Assert(Scale[1] > Scale[2]);

 Assert(abs(Scale[0] + Scale[1] + Scale[2] - 1) < 1E-3);
*)
end;

procedure TCustomHrtfs.InterpolateHrir(const Azimuth, Polar: Single;
  const SampleFrames: Integer; const Left, Right: PDavSingleFixedArray);
begin
  case FHrirList.Count of
   0 : raise Exception.Create('No HRIR found!');
   1 : with TCustomHrir(FHrirList[0]) do
        begin
         MoveLeft32(Left, SampleFrames);
         MoveRight32(Right, SampleFrames);
        end;
   2 : Interpolate2Hrir(Azimuth, Polar, SampleFrames, Left, Right);
  else Interpolate3Hrir(Azimuth, Polar, SampleFrames, Left, Right);
 end;
end;

procedure TCustomHrtfs.InterpolateHrir(const Azimuth, Polar: Single;
  const SampleFrames: Integer; const Left, Right: PDavDoubleFixedArray);
begin
 case FHrirList.Count of
   0 : raise Exception.Create('No HRIR found!');
   1 : with TCustomHrir(FHrirList[0]) do
        begin
         MoveLeft64(Left, SampleFrames);
         MoveRight64(Right, SampleFrames);
        end;
   2 : Interpolate2Hrir(Azimuth, Polar, SampleFrames, Left, Right);
  else Interpolate3Hrir(Azimuth, Polar, SampleFrames, Left, Right);
 end;
end;

procedure TCustomHrtfs.Interpolate2Hrir(const Azimuth, Polar: Single;
  const SampleFrames: Integer; const Left, Right: PDavSingleFixedArray);
var
  TempData  : PDavSingleFixedArray;
  Hrirs     : array [0..1] of TCustomHrir;
  Scale     : array [0..1] of Double;
  SpherePos : TSphereVector3D;
  Sample    : Integer;
begin
 SpherePos := MakeSphereVector3D(Azimuth, Polar);
 Hrirs[0] := TCustomHrir(FHrirList[0]);
 Hrirs[1] := TCustomHrir(FHrirList[1]);

 case FInterpolationType of
  itNearest :
   begin
    // select nearest
    if GetOrthodromicAngle2D(Hrirs[1].Position, SpherePos) <
       GetOrthodromicAngle2D(Hrirs[0].Position, SpherePos)
     then Hrirs[0] := Hrirs[1];

    // move data nearest
    Hrirs[0].MoveLeft32(Left, SampleFrames);
    Hrirs[0].MoveRight32(Right, SampleFrames);
   end;
  itLinear, itLinear3 :
   begin
    // calculate wheighting
    CalculateScaleFactors(SpherePos, Hrirs[0].Position, Hrirs[1].Position,
      Scale[0], Scale[1]);

    // allocate a temporary buffer
    ReallocMem(TempData, SampleFrames * SizeOf(Single));
    try
     // linear interpolate left
     Hrirs[0].MoveLeft32(TempData, SampleFrames);
     Hrirs[1].MoveLeft32(Left, SampleFrames);
     for Sample := 0 to SampleFrames - 1
      do Left^[Sample] := Scale[0] * TempData^[Sample] + Scale[1] * Left^[Sample];

     // linear interpolate right
     Hrirs[0].MoveRight32(TempData, SampleFrames);
     Hrirs[1].MoveRight32(Right, SampleFrames);
     for Sample := 0 to SampleFrames - 1
      do Right^[Sample] := Scale[0] * TempData^[Sample] + Scale[1] * Right^[Sample];
    finally
     Dispose(TempData);
    end;
   end;
 end;

end;

procedure TCustomHrtfs.Interpolate2Hrir(const Azimuth, Polar: Single;
  const SampleFrames: Integer; const Left, Right: PDavDoubleFixedArray);
var
  TempData  : PDavDoubleFixedArray;
  Hrirs     : array [0..1] of TCustomHrir;
  Scale     : array [0..1] of Double;
  SpherePos : TSphereVector3D;
  Sample    : Integer;
begin
 SpherePos := MakeSphereVector3D(Azimuth, Polar);
 Hrirs[0] := TCustomHrir(FHrirList[0]);
 Hrirs[1] := TCustomHrir(FHrirList[1]);

 case FInterpolationType of
  itNearest :
   begin
    // select nearest
    if GetOrthodromicAngle2D(Hrirs[1].Position, SpherePos) <
       GetOrthodromicAngle2D(Hrirs[0].Position, SpherePos)
     then Hrirs[0] := Hrirs[1];

    // move data nearest
    Hrirs[0].MoveLeft64(Left, SampleFrames);
    Hrirs[0].MoveRight64(Right, SampleFrames);
   end;
  itLinear, itLinear3 :
   begin
    // calculate wheighting
    CalculateScaleFactors(SpherePos, Hrirs[0].Position, Hrirs[1].Position,
      Scale[0], Scale[1]);

    // allocate a temporary buffer
    ReallocMem(TempData, SampleFrames * SizeOf(Double));
    try
     // linear interpolate left
     Hrirs[0].MoveLeft64(TempData, SampleFrames);
     Hrirs[1].MoveLeft64(Left, SampleFrames);
     for Sample := 0 to SampleFrames - 1
      do Left^[Sample] := Scale[0] * TempData^[Sample] + Scale[1] * Left^[Sample];

     // linear interpolate right
     Hrirs[0].MoveRight64(TempData, SampleFrames);
     Hrirs[1].MoveRight64(Right, SampleFrames);
     for Sample := 0 to SampleFrames - 1
      do Right^[Sample] := Scale[0] * TempData^[Sample] + Scale[1] * Right^[Sample];
    finally
     Dispose(TempData);
    end;
   end;
 end;
end;

procedure TCustomHrtfs.Interpolate3Hrir(const Azimuth, Polar: Single;
  const SampleFrames: Integer; const Left, Right: PDavSingleFixedArray);
var
  TempData     : array [0..1] of PDavSingleFixedArray;
  Hrirs        : array [0..2] of TCustomHrir;
  HrirPos      : array [0..2] of TSphereVector3D;
  SpherePos    : TSphereVector3D;
  AngleA       : array [0..1] of Double;
  i            : Integer;
  MinimumAngle : Single;
  CurrentAngle : Single;
  Scale        : array [0..2] of Double;
  Sample       : Integer;
begin
 SpherePos := MakeSphereVector3D(Azimuth, Polar);

 case FInterpolationType of
  itNearest :
   begin
    Hrirs[0] := FindNearestHrirs(SpherePos);

    // move data nearest
    Hrirs[0].MoveLeft32(Left, SampleFrames);
    Hrirs[0].MoveRight32(Right, SampleFrames);
   end;
  itLinear :
   begin
    // find nearest HRIR and set position shortcut
    Hrirs[0] := FindNearestHrirs(SpherePos);
    HrirPos[0] := Hrirs[0].Position;

    // check if a single Hrir is hit exactly
    if (SpherePos.Polar = HrirPos[0].Polar) and
       (SpherePos.Azimuth = HrirPos[0].Azimuth) then
     begin
      Hrirs[0].MoveLeft32(Left, SampleFrames);
      Hrirs[0].MoveRight32(Right, SampleFrames);
      Exit;
     end;

    // find second nearest HRIR and set its position shortcut
    Hrirs[1] := FindSecondNearestHrirs(SpherePos, Hrirs[0]);
    HrirPos[1] := Hrirs[1].Position;

    // calculate wheighting
    CalculateScaleFactors(SpherePos, Hrirs[0].Position, Hrirs[1].Position,
      Scale[0], Scale[1]);

    // allocate a temporary buffer
    GetMem(TempData[0], SampleFrames * SizeOf(Single));
    try
     // linear interpolate left
     Hrirs[0].MoveLeft32(TempData[0], SampleFrames);
     Hrirs[1].MoveLeft32(Left, SampleFrames);
     for Sample := 0 to SampleFrames - 1
      do Left^[Sample] := Scale[0] * TempData[0]^[Sample] + Scale[1] * Left^[Sample];

     // linear interpolate right
     Hrirs[0].MoveRight32(TempData[0], SampleFrames);
     Hrirs[1].MoveRight32(Right, SampleFrames);
     for Sample := 0 to SampleFrames - 1
      do Right^[Sample] := Scale[0] * TempData[0]^[Sample] + Scale[1] * Right^[Sample];
    finally
     Dispose(TempData[0]);
    end;
   end;
  itLinear3 :
   begin
    // find nearest three HRIRs
    FindNearestHrirs(SpherePos, Hrirs[0], Hrirs[1], Hrirs[2]);

    HrirPos[0] := Hrirs[0].Position;
    HrirPos[1] := Hrirs[1].Position;
    HrirPos[2] := Hrirs[2].Position;

    // check if a single Hrir is hit exactly
    if (SpherePos.Polar = Hrirs[0].Position.Polar) and
       (SpherePos.Azimuth = Hrirs[0].Position.Azimuth) then
     begin
      Hrirs[0].MoveLeft32(Left, SampleFrames);
      Hrirs[0].MoveRight32(Right, SampleFrames);
      Exit;
     end;

    // check if polar angle is identical
    if (SpherePos.Polar <> HrirPos[0].Polar) and
       (HrirPos[0].Polar = HrirPos[1].Polar) and
       (HrirPos[1].Polar = HrirPos[2].Polar) and
       (HrirPos[2].Polar = HrirPos[0].Polar) then
     begin
      i := 0;
      Hrirs[2] := Hrirs[1];
      while i < FHrirList.Count do
       if (TCustomHrir(FHrirList[i]).Polar <> HrirPos[0].Polar) then
        begin
         Hrirs[1] := TCustomHrir(FHrirList[i]);
         HrirPos[1] := Hrirs[1].Position;
         MinimumAngle := GetOrthodromicAngle2D(Hrirs[1].Position, SpherePos);
         inc(i);
         break;
        end else inc(i);
      while i < FHrirList.Count do
       begin
        CurrentAngle := GetOrthodromicAngle2D(TCustomHrir(FHrirList[i]).Position, SpherePos);
        if (TCustomHrir(FHrirList[i]).Polar <> HrirPos[0].Polar) and
           (CurrentAngle < MinimumAngle) then
         begin
          Hrirs[1] := TCustomHrir(FHrirList[i]);
          HrirPos[1] := Hrirs[1].Position;
          MinimumAngle := GetOrthodromicAngle2D(Hrirs[1].Position, SpherePos);
         end;
        inc(i);
       end;
     end;

    // check if polar angle is identical
    if (SpherePos.Azimuth <> HrirPos[0].Azimuth) and
       (HrirPos[0].Azimuth = HrirPos[1].Azimuth) and
       (HrirPos[1].Azimuth = HrirPos[2].Azimuth) and
       (HrirPos[2].Azimuth = HrirPos[0].Azimuth) then
     begin
      i := 0;
      Hrirs[2] := Hrirs[1];
      while i < FHrirList.Count do
       if (TCustomHrir(FHrirList[i]).Azimuth <> HrirPos[0].Azimuth) then
        begin
         Hrirs[1] := TCustomHrir(FHrirList[i]);
         HrirPos[1] := Hrirs[1].Position;
         MinimumAngle := GetOrthodromicAngle2D(Hrirs[1].Position, SpherePos);
         inc(i);
         break;
        end else inc(i);
      while i < FHrirList.Count do
       begin
        CurrentAngle := GetOrthodromicAngle2D(TCustomHrir(FHrirList[i]).Position, SpherePos);
        if (TCustomHrir(FHrirList[i]).Azimuth <> HrirPos[0].Azimuth) and
           (CurrentAngle < MinimumAngle) then
         begin
          Hrirs[1] := TCustomHrir(FHrirList[i]);
          HrirPos[1] := Hrirs[1].Position;
          MinimumAngle := GetOrthodromicAngle2D(Hrirs[1].Position, SpherePos);
         end;
        inc(i);
       end;
     end;

    // check only 1D interpolation
    if ((SpherePos.Polar   = HrirPos[0].Polar) and
        (SpherePos.Polar   = HrirPos[1].Polar)) or
       ((SpherePos.Azimuth = HrirPos[0].Azimuth) and
        (SpherePos.Azimuth = HrirPos[1].Azimuth)) then
     begin
      AngleA[0] := GetOrthodromicAngle2D(Hrirs[1].Position, SpherePos);
      AngleA[1] := GetOrthodromicAngle2D(Hrirs[0].Position, Hrirs[1].Position);

      Assert(AngleA[0] > 0);
      Assert(AngleA[1] > 0);

      // calculate wheighting
      Scale[0] := AngleA[0] / AngleA[1];
      Scale[1] := 1 - Scale[0];

      Assert(Scale[0] >= 0);
      Assert(Scale[1] >= 0);
      Assert(Scale[0] <= 1);
      Assert(Scale[1] <= 1);

      // allocate a temporary buffer
      GetMem(TempData[0], SampleFrames * SizeOf(Single));
      try
       // linear interpolate left
       Hrirs[0].MoveLeft32(TempData[0], SampleFrames);
       Hrirs[1].MoveLeft32(Left, SampleFrames);
       for Sample := 0 to SampleFrames - 1
        do Left^[Sample] := Scale[0] * TempData[0]^[Sample] + Scale[1] * Left^[Sample];

       // linear interpolate right
       Hrirs[0].MoveRight32(TempData[0], SampleFrames);
       Hrirs[1].MoveRight32(Right, SampleFrames);
       for Sample := 0 to SampleFrames - 1
        do Right^[Sample] := Scale[0] * TempData[0]^[Sample] + Scale[1] * Right^[Sample];
      finally
       Dispose(TempData[0]);
      end;
      Exit;
     end;

    CalculateScaleFactors(SpherePos, HrirPos[0], HrirPos[1], HrirPos[2],
      Scale[0], Scale[1], Scale[2]);

    // allocate a temporary buffer
    GetMem(TempData[0], SampleFrames * SizeOf(Single));
    try
     GetMem(TempData[1], SampleFrames * SizeOf(Single));
     try
      // linear interpolate left
      Hrirs[0].MoveLeft32(TempData[0], SampleFrames);
      Hrirs[1].MoveLeft32(TempData[1], SampleFrames);
      Hrirs[2].MoveLeft32(Left, SampleFrames);
      for Sample := 0 to SampleFrames - 1
       do Left^[Sample] := Scale[0] * TempData[0]^[Sample] +
                           Scale[1] * TempData[1]^[Sample] +
                           Scale[2] * Left^[Sample];

      // linear interpolate right
      Hrirs[0].MoveRight32(TempData[0], SampleFrames);
      Hrirs[1].MoveRight32(TempData[1], SampleFrames);
      Hrirs[2].MoveRight32(Right, SampleFrames);
      for Sample := 0 to SampleFrames - 1
       do Right^[Sample] := Scale[0] * TempData[0]^[Sample] +
                            Scale[1] * TempData[1]^[Sample] +
                            Scale[2] * Right^[Sample];
     finally
      Dispose(TempData[1]);
     end;
    finally
     Dispose(TempData[0]);
    end;
   end;
 end;
end;

procedure TCustomHrtfs.Interpolate3Hrir(const Azimuth, Polar: Single;
  const SampleFrames: Integer; const Left, Right: PDavDoubleFixedArray);
var
  TempData  : array [0..1] of PDavDoubleFixedArray;
  Hrirs     : array [0..2] of TCustomHrir;
  HrirPos   : array [0..2] of TSphereVector3D;
  SpherePos : TSphereVector3D;
  AngleA    : array [0..2] of Double;
  PntA      : array [0..2] of Double;
  Angles    : array [0..2] of Double;
  HalfAng   : array [0..2] of Double;
  Relations : array [0..2] of Double;
  Scale     : array [0..2] of Double;
  Sample    : Integer;
begin
 SpherePos := MakeSphereVector3D(Azimuth, Polar);

 case FInterpolationType of
  itNearest :
   begin
    Hrirs[0] := FindNearestHrirs(SpherePos);

    // move data nearest
    Hrirs[0].MoveLeft64(Left, SampleFrames);
    Hrirs[0].MoveRight64(Right, SampleFrames);
   end;
  itLinear :
   begin
    // find nearest HRIR and set position shortcut
    Hrirs[0] := FindNearestHrirs(SpherePos);
    HrirPos[0] := Hrirs[0].Position;

    // check if a single Hrir is hit exactly
    if (SpherePos.Polar = HrirPos[0].Polar) and
       (SpherePos.Azimuth = HrirPos[0].Azimuth) then
     begin
      Hrirs[0].MoveLeft64(Left, SampleFrames);
      Hrirs[0].MoveRight64(Right, SampleFrames);
      Exit;
     end;

    // find second nearest HRIR and set its position shortcut
    Hrirs[1] := FindSecondNearestHrirs(SpherePos, Hrirs[0]);
    HrirPos[1] := Hrirs[1].Position;

    // calculate wheighting
    CalculateScaleFactors(SpherePos, Hrirs[0].Position, Hrirs[1].Position,
      Scale[0], Scale[1]);

    // allocate a temporary buffer
    GetMem(TempData[0], SampleFrames * SizeOf(Double));
    try
     // linear interpolate left
     Hrirs[0].MoveLeft64(TempData[0], SampleFrames);
     Hrirs[1].MoveLeft64(Left, SampleFrames);
     for Sample := 0 to SampleFrames - 1
      do Left^[Sample] := Scale[0] * TempData[0]^[Sample] + Scale[1] * Left^[Sample];

     // linear interpolate right
     Hrirs[0].MoveRight64(TempData[0], SampleFrames);
     Hrirs[1].MoveRight64(Right, SampleFrames);
     for Sample := 0 to SampleFrames - 1
      do Right^[Sample] := Scale[0] * TempData[0]^[Sample] + Scale[1] * Right^[Sample];
    finally
     Dispose(TempData[0]);
    end;
   end;
  itLinear3 :
   begin
    FindNearestHrirs(SpherePos, Hrirs[0], Hrirs[1], Hrirs[2]);

    HrirPos[0] := Hrirs[0].Position;
    HrirPos[1] := Hrirs[1].Position;
    HrirPos[2] := Hrirs[2].Position;

    // calculate orthodromic angle to desired position
    AngleA[0] := GetOrthodromicAngle2D(Hrirs[0].Position, SpherePos);
    AngleA[1] := GetOrthodromicAngle2D(Hrirs[1].Position, SpherePos);
    AngleA[2] := GetOrthodromicAngle2D(Hrirs[2].Position, SpherePos);

    // calculate orthodromic angle between Hrirs
    PntA[0] := GetOrthodromicAngle2D(Hrirs[1].Position, Hrirs[2].Position);
    PntA[1] := GetOrthodromicAngle2D(Hrirs[2].Position, Hrirs[0].Position);
    PntA[2] := GetOrthodromicAngle2D(Hrirs[0].Position, Hrirs[1].Position);

    // calculate triangle angles (using spherical trigonometry)
    Angles[0] := arccos((PntA[0] - PntA[1] * PntA[2]) / (sqrt(1 - sqr(PntA[1])) * sqrt(1 - sqr(PntA[2]))));
    Angles[1] := arccos((PntA[1] - PntA[2] * PntA[0]) / (sqrt(1 - sqr(PntA[2])) * sqrt(1 - sqr(PntA[0]))));
    Angles[2] := arccos((PntA[2] - PntA[0] * PntA[1]) / (sqrt(1 - sqr(PntA[0])) * sqrt(1 - sqr(PntA[1]))));

    // calculate triangle angles between (using spherical trigonometry)
    HalfAng[0] := (AngleA[1] - AngleA[0] * PntA[2]);
    if HalfAng[0] <> 0
     then HalfAng[0] := arccos(Limit(HalfAng[0] / (sqrt(1 - sqr(AngleA[0])) * sqrt(1 - sqr(PntA[2])))));

    HalfAng[1] := (AngleA[2] - AngleA[1] * PntA[0]);
    if HalfAng[1] <> 0
     then HalfAng[1] := arccos(Limit(HalfAng[1] / (sqrt(1 - sqr(AngleA[1])) * sqrt(1 - sqr(PntA[0])))));

    HalfAng[2] := (AngleA[0] - AngleA[2] * PntA[1]);
    if HalfAng[2] <> 0
     then HalfAng[2] := arccos(Limit(HalfAng[2] / (sqrt(1 - sqr(AngleA[2])) * sqrt(1 - sqr(PntA[1])))));

    Relations[0] := HalfAng[0] / Angles[0];
    Relations[1] := HalfAng[1] / Angles[1];
    Relations[2] := HalfAng[2] / Angles[2];

    Scale[0] := (1 - Relations[2]) *  Relations[1];
    Scale[1] := (1 - Relations[0]) *  Relations[2];
    Scale[2] := (1 - Relations[1]) *  Relations[0];

    // allocate a temporary buffer
    GetMem(TempData[0], SampleFrames * SizeOf(Double));
    try
     GetMem(TempData[1], SampleFrames * SizeOf(Double));
     try
      // linear interpolate left
      Hrirs[0].MoveLeft64(TempData[0], SampleFrames);
      Hrirs[1].MoveLeft64(TempData[1], SampleFrames);
      Hrirs[2].MoveLeft64(Left, SampleFrames);
      for Sample := 0 to SampleFrames - 1
       do Left^[Sample] := Scale[0] * TempData[0]^[Sample] +
                           Scale[1] * TempData[1]^[Sample] +
                           Scale[2] * Left^[Sample];

      // linear interpolate right
      Hrirs[0].MoveRight64(TempData[0], SampleFrames);
      Hrirs[1].MoveRight64(TempData[1], SampleFrames);
      Hrirs[2].MoveRight64(Right, SampleFrames);
      for Sample := 0 to SampleFrames - 1
       do Right^[Sample] := Scale[0] * TempData[0]^[Sample] +
                            Scale[1] * TempData[1]^[Sample] +
                            Scale[2] * Right^[Sample];
     finally
      Dispose(TempData[1]);
     end;
    finally
     Dispose(TempData[0]);
    end;
   end;
 end;
end;

function TCustomHrtfs.FindNearestHrirs(const SpherePos: TSphereVector2D): TCustomHrir;
var
  SpherePos3D : TSphereVector3D;
begin
 SpherePos3D.Azimuth := SpherePos.Azimuth;
 SpherePos3D.Polar := SpherePos.Polar;
 SpherePos3D.Radius := 1;
 Result := FindNearestHrirs(SpherePos3D);
end;

function TCustomHrtfs.FindNearestHrirs(const SpherePos: TSphereVector3D): TCustomHrir;
var
  i            : Integer;
  CurrentAngle : Single;
  TempHrir     : TCustomHrir;
  MinimumAngle : Double;
begin
 Assert(FHrirList.Count > 0);

 // initialize with first HRIR
 Result := TCustomHrir(FHrirList[0]);

 // initialize with first angle/distance
 MinimumAngle := GetOrthodromicAngle2D(Result.Position, SpherePos);

 // search for better distances..
 for i := 1 to FHrirList.Count - 1 do
  begin
   TempHrir := TCustomHrir(FHrirList[i]);
   CurrentAngle := GetOrthodromicAngle2D(TempHrir.Position, SpherePos);

   if CurrentAngle < MinimumAngle then
    begin
     MinimumAngle := CurrentAngle;
     Result := TempHrir;
    end;
  end;
end;

function TCustomHrtfs.FindSecondNearestHrirs(const SpherePos: TSphereVector2D; const Nearest: TCustomHrir): TCustomHrir;
var
  SpherePos3D : TSphereVector3D;
begin
 SpherePos3D.Azimuth := SpherePos.Azimuth;
 SpherePos3D.Polar := SpherePos.Polar;
 SpherePos3D.Radius := 1;
 Result := FindSecondNearestHrirs(SpherePos3D, Nearest);
end;

function TCustomHrtfs.FindSecondNearestHrirs(const SpherePos: TSphereVector3D; const Nearest: TCustomHrir): TCustomHrir;
var
  i                  : Integer;
  CurrentAngle       : Single;
  TempHrir           : TCustomHrir;
  OnPolarAxisFound   : Boolean;
  OnAzimuthAxisFound : Boolean;
  MinimumAngle       : Double;
begin
 Assert(FHrirList.Count > 1);

 // initialize with first HRIR
 if TCustomHrir(FHrirList[0]) <> Nearest
  then Result := TCustomHrir(FHrirList[0])
  else Result := TCustomHrir(FHrirList[1]);

 // initialize with first angle/distance
 MinimumAngle := GetOrthodromicAngle2D(Result.Position, SpherePos);

 // check whether an on polar axis situation has been found
 OnPolarAxisFound := (Result.Position.Polar = SpherePos.Polar) and
   (SpherePos.Polar = Nearest.Polar);

 // check whether an on azimuth axis situation has been found
 OnAzimuthAxisFound := (Result.Position.Azimuth = SpherePos.Azimuth) and
   (SpherePos.Azimuth = Nearest.Azimuth);

 // search for better distances..
 for i := 0 to FHrirList.Count - 1 do
  if TCustomHrir(FHrirList[i]) <> Nearest then
   begin
    TempHrir := TCustomHrir(FHrirList[i]);
    CurrentAngle := GetOrthodromicAngle2D(TempHrir.Position, SpherePos);

    if (SpherePos.Polar = Nearest.Polar) and
      (TempHrir.Polar = SpherePos.Polar) then
     begin
      if OnPolarAxisFound then
       if (CurrentAngle < MinimumAngle) then
        begin
         MinimumAngle := CurrentAngle;
         Result := TempHrir;
        end else
       else
        begin
         MinimumAngle := CurrentAngle;
         Result := TempHrir;
         OnPolarAxisFound := True;
        end;
     end;

    if (SpherePos.Azimuth = Nearest.Azimuth) and
      (TempHrir.Azimuth = SpherePos.Azimuth) then
     begin
      if OnAzimuthAxisFound then
       if (CurrentAngle < MinimumAngle) then
        begin
         MinimumAngle := CurrentAngle;
         Result := TempHrir;
        end else
       else
        begin
         MinimumAngle := CurrentAngle;
         Result := TempHrir;
         OnAzimuthAxisFound := True;
        end;
     end;

    // in case no on axis HRIR has been found, search for minimum
    if (not OnPolarAxisFound) and (not OnAzimuthAxisFound) and
       (CurrentAngle < MinimumAngle) then
     begin
      MinimumAngle := CurrentAngle;
      Result := TempHrir;
     end;
   end;
end;

procedure TCustomHrtfs.FindNearestHrirs(const SpherePos: TSphereVector2D;
  var A, B, C: TCustomHrir);
var
  SpherePos3D : TSphereVector3D;
begin
 SpherePos3D.Azimuth := SpherePos.Azimuth;
 SpherePos3D.Polar := SpherePos.Polar;
 SpherePos3D.Radius := 1;
 FindNearestHrirs(SpherePos3D, A, B, C);
end;

procedure TCustomHrtfs.FindNearestHrirs(const SpherePos: TSphereVector3D;
  var A, B, C: TCustomHrir);
var
  i            : Integer;
  CurrentAngle : Single;
  TempHrir     : TCustomHrir;
  Angles       : array [0..2] of Single;
begin
 Assert(FHrirList.Count >= 3);

 // initialize with first three HRIRs
 A := TCustomHrir(FHrirList[0]);
 B := TCustomHrir(FHrirList[1]);
 C := TCustomHrir(FHrirList[2]);

 // initialize with first three distances
 Angles[0] := GetOrthodromicAngle2D(A.Position, SpherePos);
 Angles[1] := GetOrthodromicAngle2D(B.Position, SpherePos);
 Angles[2] := GetOrthodromicAngle2D(C.Position, SpherePos);

 // order distances
 if Angles[1] < Angles[0] then
  begin
   // eventually swap HRIRs
   CurrentAngle  := Angles[0];
   Angles[0] := Angles[1];
   Angles[1] := CurrentAngle;

   TempHrir := A;
   A := B;
   B := TempHrir;
  end;
 if Angles[2] < Angles[0] then
  begin
   // eventually swap HRIRs
   CurrentAngle  := Angles[0];
   Angles[0] := Angles[2];
   Angles[2] := CurrentAngle;

   TempHrir := A;
   A := C;
   C := TempHrir;
  end;
 if Angles[2] < Angles[1] then
  begin
   // eventually swap HRIRs
   CurrentAngle  := Angles[1];
   Angles[1] := Angles[2];
   Angles[2] := CurrentAngle;

   TempHrir := B;
   B := C;
   C := TempHrir;
  end;

 // search for better distances..
 for i := 3 to FHrirList.Count - 1 do
  begin
   TempHrir := TCustomHrir(FHrirList[i]);
   CurrentAngle := GetOrthodromicAngle2D(TempHrir.Position, SpherePos);

   // is first place?
   if CurrentAngle < Angles[0] then
    begin
     Angles[2] := Angles[1];
     Angles[1] := Angles[0];
     Angles[0] := CurrentAngle;
     C := B; B := A;
     A := TempHrir;
    end else

   // or second place?
   if CurrentAngle < Angles[1] then
    begin
     Angles[2] := Angles[1];
     Angles[1] := CurrentAngle;
     C := B;
     B := TempHrir;
    end else

   // or third place?
   if CurrentAngle < Angles[2] then
    begin
     Angles[2] := CurrentAngle;
     C := TempHrir;
    end;
  end;
end;

procedure TCustomHrtfs.SwapChannels;
var
  i : Integer;
begin
 for i := 0 to FHrirList.Count - 1
  do TCustomHrir(FHrirList[i]).SwapChannels;
end;

procedure TCustomHrtfs.AddChunk(Chunk: TCustomChunk);
begin
 inherited;
 if Chunk is TCustomHrir
  then FHrirList.Add(Chunk);
end;

procedure TCustomHrtfs.Clear;
begin
 FHrirList.Clear;
 FChunkList.Clear;
end;

procedure TCustomHrtfs.ClearHrirs;
var
  i : Integer;
begin
 i := 0;
 FHrirList.Clear;
 while i < FChunkList.Count do
  if FChunkList[i] is TCustomHrir
   then FChunkList.Delete(i)
   else inc(i);
end;

procedure TCustomHrtfs.ClearInformationChunks;
begin
 if Assigned(FGeneralInformation)     then FreeAndNil(FGeneralInformation);
 if Assigned(FSubjectInformation)     then FreeAndNil(FSubjectInformation);
 if Assigned(FRoomInformation)        then FreeAndNil(FRoomInformation);
 if Assigned(FMicrophoneInformation)  then FreeAndNil(FMicrophoneInformation);
 if Assigned(FOutboardInformation)    then FreeAndNil(FOutboardInformation);
 if Assigned(FMeasurementInformation) then FreeAndNil(FMeasurementInformation);
end;

procedure TCustomHrtfs.LoadFromStream(Stream: TStream);
begin
 Clear;
 FGeneralInformation     := nil;
 FSubjectInformation     := nil;
 FRoomInformation        := nil;
 FMicrophoneInformation  := nil;
 FOutboardInformation    := nil;
 FMeasurementInformation := nil;
 inherited;

 if Assigned(FOnHrtfChanged)
  then FOnHrtfChanged(Self);
end;

procedure TCustomHrtfs.ConvertStreamToChunk(ChunkClass: TCustomChunkClass; Stream : TStream);
begin
 if ChunkClass = TCustomHrirGeneralInformation then
  begin
   if Assigned(FGeneralInformation)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FGeneralInformation := TCustomHrirGeneralInformation.Create;
   FGeneralInformation.ChunkFlags := ChunkFlags;
   FGeneralInformation.LoadFromStream(Stream);
   AddChunk(FGeneralInformation);
  end else
 if ChunkClass = TCustomHrirSubjectInformation then
  begin
   if Assigned(FSubjectInformation)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FSubjectInformation := TCustomHrirSubjectInformation.Create;
   FSubjectInformation.ChunkFlags := ChunkFlags;
   FSubjectInformation.LoadFromStream(Stream);
   AddChunk(FSubjectInformation);
  end else
 if ChunkClass = TCustomHrirRoomInformation then
  begin
   if Assigned(FRoomInformation)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FRoomInformation := TCustomHrirRoomInformation.Create;
   FRoomInformation.ChunkFlags := ChunkFlags;
   FRoomInformation.LoadFromStream(Stream);
   AddChunk(FRoomInformation);
  end else
 if ChunkClass = TCustomHrirMicrophoneInformation then
  begin
   if Assigned(FMicrophoneInformation)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FMicrophoneInformation := TCustomHrirMicrophoneInformation.Create;
   FMicrophoneInformation.ChunkFlags := ChunkFlags;
   FMicrophoneInformation.LoadFromStream(Stream);
   AddChunk(FMicrophoneInformation);
  end else
 if ChunkClass = TCustomHrirOutboardInformation then
  begin
   if Assigned(FOutboardInformation)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FOutboardInformation := TCustomHrirOutboardInformation.Create;
   FOutboardInformation.ChunkFlags := ChunkFlags;
   FOutboardInformation.LoadFromStream(Stream);
   AddChunk(FOutboardInformation);
  end else
 if ChunkClass = TCustomHrirMeasurementInformation then
  begin
   if Assigned(FMeasurementInformation)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FMeasurementInformation := TCustomHrirMeasurementInformation.Create;
   FMeasurementInformation.ChunkFlags := ChunkFlags;
   FMeasurementInformation.LoadFromStream(Stream);
   AddChunk(FMeasurementInformation);
  end else inherited;
end;

end.
