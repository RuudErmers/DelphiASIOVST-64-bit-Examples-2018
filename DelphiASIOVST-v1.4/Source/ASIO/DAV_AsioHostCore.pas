unit DAV_AsioHostCore;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2003-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LclType, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Controls, DAV_AsioInterface, DAV_Types, DAV_Asio;

const
  {$IFDEF DELPHI10_UP} {$region 'Message constants'} {$ENDIF}
  // private message
  PM_Asio                 = WM_User + 1652; // unique we hope
  PM_BufferSwitch         = PM_Asio + 1;
  PM_BufferSwitchTimeInfo = PM_Asio + 2;
  PM_UpdateSamplePosition = PM_Asio + 3;

type
  {$IFDEF DELPHI10_UP} {$region 'Basic types'} {$ENDIF}

  // Asio message(s), as wParam for PM_Asio
  TAsioMessage = (
    amBufferSwitch,          // new buffer index in lParam
    amBufferSwitchTimeInfo,  // new buffer index in lParam
    amResetRequest,
    amBufferSizeChange,
    amLatencyChanged,
    amResyncRequest);

  TAsioBufferList = array [0..0] of TAsioBufferInfo;
  PAsioBufferList = ^TAsioBufferList;

  TAsioSupport = (asTimeInfo, asTimeCode, asInputMonitor);
  TAsioSupports = set of TAsioSupport;

  TAsioCanDo = (acdInputMonitor, acdTimeInfo, acdTimeCode, acdTransport,
    acdInputGain, acdInputMeter, acdOutputGain, acdOutputMeter, acdSetIoFormat,
    acdGetIoFormat, acdCanDoIoFormat);
  TAsioCanDos = set of TAsioCanDo;

  TBufferSwitchEvent = procedure(Sender: TObject;
    const BufferInfo: PAsioBufferList; const BufferIndex : Integer) of object;

  EAsioHost = Exception;
  {$IFDEF DELPHI10_UP} {$endregion 'Basic types'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TAsioTimeSub'} {$ENDIF}
  TAsioTimeFlag = (atSystemTimeValid, atSamplePositionValid, atSampleRateValid,
             atSpeedValid, atSampleRateChanged, atClockSourceChanged);
  TAsioTimeFlags = set of TAsioTimeFlag;

  TAsioTimeSub = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    function GetSamplePosition: Int64;
    function GetSampleRate: Double;
    function GetSpeed: Double;
    function GetFlags: TAsioTimeFlags;
    procedure SetSamplePosition(Value: Int64);
    procedure SetFlags(Flags: TAsioTimeFlags);
    procedure SetSampleRate(const Value: Double);
    procedure SetSpeed(const Value: Double);
  protected
    FBufferTime: TAsioTime;
    procedure Change; dynamic;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property OnChanged: TNotifyEvent read FOnChange write FOnChange;
    constructor Create;
  published
    property SamplePos: Int64 read GetSamplePosition write SetSamplePosition;
    property Speed: Double read GetSpeed write SetSpeed; //absolute speed (1. = nominal)
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property Flags: TAsioTimeFlags read GetFlags Write SetFlags;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'TAsioTimeSub'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TAsioChannel'} {$ENDIF}
  TCustomAsioChannel = class(TObject)
  private
    FDriver       : IStdCallAsio;
    FChannel      : LongInt;
    FIsActive     : Boolean;
    FChannelGroup : LongInt;
    FSampleType   : TAsioSampleType;
    FName         : string;
    FGain         : Integer;
    function GetAlignedSampleSize: Byte;
  protected
    function GetMeter: Integer; virtual; abstract;
    procedure SetGain(const Value: Integer); virtual; abstract;
  public
    constructor Create(Driver: IStdCallAsio; Channel: Integer);
    procedure UpdateChannelInfo; virtual;

    property Channel: LongInt read FChannel;
    property IsActive: Boolean read FIsActive;
    property ChannelGroup: LongInt read FChannelGroup;
    property SampleType: TAsioSampleType read FSampleType;
    property AlignedSampleSize: Byte read GetAlignedSampleSize;
    property Name: string read FName;
    property Gain: Integer read FGain write SetGain;
    property Meter: Integer read GetMeter;
  end;

  TAsioChannelInput = class(TCustomAsioChannel)
  protected
    function GetMeter: Integer; override;
    procedure SetGain(const Value: Integer); override;
  public
    procedure InputMonitor(OutputChannel: Integer; Gain: Integer;
      Active: Boolean; Pan: Integer); virtual;
  end;
  TAsioChannelInputClass = class of TAsioChannelInput;

  TAsioChannelOutput = class(TCustomAsioChannel)
  protected
    function GetMeter: Integer; override;
    procedure SetGain(const Value: Integer); override;
  end;
  TAsioChannelOutputClass = class of TAsioChannelOutput;
  {$IFDEF DELPHI10_UP} {$endregion 'TAsioChannel'} {$ENDIF}

  TAsioHostCore = class(TObject)
  private
    FDriver              : IStdCallAsio;
    FCallbacks           : TAsioCallbacks;
    FHandle              : THandle;
    {$IFDEF AllowMultipleAsioHosts}
    FAsioDriverIndex     : Integer;
    {$ENDIF}
    FSampleRate          : Double;
    FUnalignedBuffer     : PAsioBufferInfo;
    FInputBuffers        : PAsioBufferInfos;
    FOutputBuffers       : PAsioBufferInfos;
    FAsioTime            : TAsioTimeSub;
    FMin, FMax           : Integer;
    FPrefered            : Integer;
    FGranularity         : Integer;
    FBufferSize          : Cardinal;
    FRunning             : Boolean;

    FInputLatency        : Integer;
    FOutputLatency       : Integer;
    FInputChannels       : array of TAsioChannelInput;
    FOutputChannels      : array of TAsioChannelOutput;
    FClockSources        : array of TAsioClockSource;
    FAsioSupports        : TAsioSupports;
    FAsioCanDos          : TAsioCanDos;
    FDriverName          : string;
    FDriverVersion       : Integer;

    FOnBufferSwitch      : TBufferSwitchEvent;
    FOnReset             : TNotifyEvent;
    FOnLatencyChanged    : TNotifyEvent;
    FOnSampleRateChanged : TNotifyEvent;

    function GetBuffersCreated: Boolean;
    function GetInputChannelCount: Integer;
    function GetOutputChannelCount: Integer;
    function GetInputChannel(Index: Integer): TAsioChannelInput;
    function GetOutputChannel(Index: Integer): TAsioChannelOutput;
    procedure SetSampleRate(Value: Double);
    procedure PostAsioMessage(AsioMessage: TAsioMessage; Value: Integer = 0);
  protected
    class function GetInputChannelClass: TAsioChannelInputClass; virtual;
    class function GetOutputChannelClass: TAsioChannelOutputClass; virtual;
    {$IFDEF FPC}
    procedure WndProc(var Msg: TLMessage);
    procedure PMAsio(var Message: TLMessage); message PM_Asio;
    procedure PMBufferSwitch(var Message: TLMessage); message PM_BufferSwitch;
    procedure PMBufferSwitchTimeInfo(var Message: TLMessage); message PM_BufferSwitchTimeInfo;
    {$ELSE}
    procedure WndProc(var Msg: TMessage);
    procedure PMAsio(var Message: TMessage); message PM_Asio;
    procedure PMBufferSwitch(var Message: TMessage); message PM_BufferSwitch;
    procedure PMBufferSwitchTimeInfo(var Message: TMessage); message PM_BufferSwitchTimeInfo;
    {$ENDIF}

    procedure AquireCanDos; virtual;
    procedure AquireClockSources; virtual;
    procedure AquireDriverName; virtual;
    procedure AquireDriverVersion; virtual;
    procedure AquireSampleRate; virtual;
    procedure BufferSizeChange; virtual;
    procedure BufferSwitch(Index: Integer); virtual;
    procedure BufferSwitchTimeInfo(Index: Integer; const Params: TAsioTime); virtual;
    procedure ClearBuffers; virtual;
    procedure DetermineBuffersize; virtual;
    procedure InitializeCallbacks; virtual;
    procedure InitializeDriver; virtual;
    procedure LatencyChanged; virtual;
    procedure Reset; virtual;
    procedure ResyncRequest; virtual;
    procedure SampleRateChanged; virtual;

    procedure InternalBufferSwitch(DoubleBufferIndex: Integer;
      DirectProcess: TAsioBool); virtual;
    function InternalBufferSwitchTimeInfo(var Params: TAsioTime;
      DoubleBufferIndex: Integer; DirectProcess: TAsioBool): PAsioTime; virtual;
    procedure InternalSampleRateDidChange(SampleRate: TAsioSampleRate); virtual;
    function MessageHandler(Selector, Value: LongInt;
      MessagePointer: Pointer; Optional: PDouble): LongInt; virtual;

    // protected properties
    property BuffersCreated: Boolean read GetBuffersCreated;
  public
    constructor Create(ID: TGUID); virtual;
    destructor Destroy; override;

    procedure DefaultHandler(var Message); override;
    procedure ControlPanel;
    procedure CreateBuffers;
    procedure DisposeBuffers;
    procedure Start;
    procedure Stop;
    function CanSampleRate(SampleRate: TAsioSampleRate): Boolean;

    // properties
    property AsioTime: TAsioTimeSub read FAsioTime Write FAsioTime;
    property BufferGranularity: Integer read FGranularity stored False;
    property BufferMaximum: Integer read FMax stored False;
    property BufferMinimum: Integer read FMin stored False;
    property BufferPreferredSize: Integer read FPrefered stored False;
    property BufferSize: Cardinal read FBufferSize stored False default 1;
    property CanDos: TAsioCanDos read FAsioCanDos;
    property Driver: IStdCallAsio read FDriver;
    property DriverName: string read FDriverName;
    property DriverVersion: Integer read FDriverVersion;
    property InputChannel[Index: Integer]: TAsioChannelInput read GetInputChannel;
    property InputChannelCount: Integer read GetInputChannelCount;
    property InputLatency: Integer read FInputLatency;
    property OutputChannel[Index: Integer]: TAsioChannelOutput read GetOutputChannel;
    property OutputChannelCount: Integer read GetOutputChannelCount;
    property OutputLatency: Integer read FInputLatency;
    property Running: Boolean read FRunning;
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property Supports: TAsioSupports read FAsioSupports write FAsioSupports default [asTimeInfo, asTimeCode];

    // events
    property OnBufferSwitch: TBufferSwitchEvent read FOnBufferSwitch write FOnBufferSwitch;
    property OnLatencyChanged: TNotifyEvent read FOnLatencyChanged write FOnLatencyChanged;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
  end;

var
  {$IFNDEF AllowMultipleAsioHosts}
  GAsioDriver    : TAsioHostCore;
  {$ELSE}
  GAsioDriver    : array [0..3] of TAsioHostCore;
  {$ENDIF}
  PMUpdSamplePos : {$IFDEF FPC} TLMessage; {$ELSE} TMessage; {$ENDIF}

function ChannelTypeToString(ChannelType: TAsioSampleType): string;

implementation

uses
  DAV_AsioConvert, DAV_AsioResourceStrings;

{$IFDEF DELPHI10_UP} {$region 'Global functions'} {$ENDIF}
function ChannelTypeToString(ChannelType: TAsioSampleType): string;
begin
 Result := '';
 case ChannelType of
  CAsioSTInt16MSB   : Result := 'Int16MSB';
  CAsioSTInt24MSB   : Result := 'Int24MSB';
  CAsioSTInt32MSB   : Result := 'Int32MSB';
  CAsioSTFloat32MSB : Result := 'Float32MSB';
  CAsioSTFloat64MSB : Result := 'Float64MSB';

  // these are used for 32 bit data buffer, with different alignment of the data inside
  // 32 bit PCI bus systems can be more easily used with these
  CAsioSTInt32MSB16 : Result := 'Int32MSB16';
  CAsioSTInt32MSB18 : Result := 'Int32MSB18';
  CAsioSTInt32MSB20 : Result := 'Int32MSB20';
  CAsioSTInt32MSB24 : Result := 'Int32MSB24';

  CAsioSTInt16LSB   : Result := 'Int16LSB';
  CAsioSTInt24LSB   : Result := 'Int24LSB';
  CAsioSTInt32LSB   : Result := 'Int32LSB';
  CAsioSTFloat32LSB : Result := 'Float32LSB';
  CAsioSTFloat64LSB : Result := 'Float64LSB';

  // these are used for 32 bit data buffer, with different alignment of the data inside
  // 32 bit PCI bus systems can more easily used with these
  CAsioSTInt32LSB16 : Result := 'Int32LSB16';
  CAsioSTInt32LSB18 : Result := 'Int32LSB18';
  CAsioSTInt32LSB20 : Result := 'Int32LSB20';
  CAsioSTInt32LSB24 : Result := 'Int32LSB24';
 end;
end;
{$IFDEF DELPHI10_UP} {$endregion 'Global functions'} {$ENDIF}

(*
function GetInputConverter(ConverterType: TAsioSampleType): TInConverter;
begin
 case ConverterType of
  CAsioSTInt16MSB   : Result := FromInt16MSB;
  CAsioSTInt24MSB   : Result := FromInt24MSB;
  CAsioSTInt32MSB   : Result := FromInt32MSB;
  CAsioSTFloat32MSB : Result := FromSingleMSB;
  CAsioSTFloat64MSB : Result := FromDoubleMSB;
  CAsioSTInt32MSB16 : Result := FromInt32MSB16;
  CAsioSTInt32MSB18 : Result := FromInt32MSB18;
  CAsioSTInt32MSB20 : Result := FromInt32MSB20;
  CAsioSTInt32MSB24 : Result := FromInt32MSB24;
  CAsioSTInt16LSB   : Result := FromInt16LSB;
  CAsioSTInt24LSB   : Result := FromInt24LSB;
  CAsioSTInt32LSB   : Result := FromInt32LSB;
  CAsioSTFloat32LSB : Result := FromSingleLSB;
  CAsioSTFloat64LSB : Result := FromDoubleLSB;
  CAsioSTInt32LSB16 : Result := FromInt32LSB16;
  CAsioSTInt32LSB18 : Result := FromInt32LSB18;
  CAsioSTInt32LSB20 : Result := FromInt32LSB20;
  CAsioSTInt32LSB24 : Result := FromInt32LSB24;
  else raise EAsioHost.Create(RStrConverterTypeUnknown);
 end;
end;

function GetOutputConverter(ConverterType: TAsioSampleType): TOutConverter;
begin
 case ConverterType of
  CAsioSTInt16MSB   : Result := ToInt16MSB;
  CAsioSTInt24MSB   : Result := ToInt24MSB;
  CAsioSTInt32MSB   : Result := ToInt32MSB;
  CAsioSTFloat32MSB : Result := ToSingleMSB;
  CAsioSTFloat64MSB : Result := ToDoubleMSB;
  CAsioSTInt32MSB16 : Result := ToInt32MSB16;
  CAsioSTInt32MSB18 : Result := ToInt32MSB18;
  CAsioSTInt32MSB20 : Result := ToInt32MSB20;
  CAsioSTInt32MSB24 : Result := ToInt32MSB24;
  CAsioSTInt16LSB   : Result := ToInt16LSB;
  CAsioSTInt24LSB   : Result := ToInt24LSB;
  CAsioSTInt32LSB   : Result := ToInt32LSB;
  CAsioSTFloat32LSB : Result := ToSingleLSB;
  CAsioSTFloat64LSB : Result := ToDoubleLSB;
  CAsioSTInt32LSB16 : Result := ToInt32LSB16;
  CAsioSTInt32LSB18 : Result := ToInt32LSB18;
  CAsioSTInt32LSB20 : Result := ToInt32LSB20;
  CAsioSTInt32LSB24 : Result := ToInt32LSB24;
  else raise EAsioHost.Create(RStrConverterTypeUnknown);
 end;
end;
*)

{$IFDEF DELPHI10_UP} {$region 'Asio callback functions'} {$ENDIF}
procedure DefaultBufferSwitch(DoubleBufferIndex: Integer; DirectProcess: TAsioBool); cdecl;
begin
 {$IFDEF AllowMultipleAsioHosts}
 if Assigned(GAsioDriver[0])
  then GAsioDriver[0].InternalBufferSwitch(DoubleBufferIndex, DirectProcess);
 {$ELSE}
 if Assigned(GAsioDriver)
  then GAsioDriver.InternalBufferSwitch(DoubleBufferIndex, DirectProcess);
 {$ENDIF}
end;

function DefaultBufferSwitchTimeInfo(var Params: TAsioTime;
  DoubleBufferIndex: Integer; DirectProcess: TAsioBool): PAsioTime; cdecl;
begin
 {$IFDEF AllowMultipleAsioHosts}
 if Assigned(GAsioDriver[0])
  then Result := GAsioDriver[0].InternalBufferSwitchTimeInfo(Params, DoubleBufferIndex, DirectProcess)
  else Result := nil;
 {$ELSE}
 if Assigned(GAsioDriver)
  then Result := GAsioDriver.InternalBufferSwitchTimeInfo(Params, DoubleBufferIndex, DirectProcess)
  else Result := nil;
 {$ENDIF}
end;

procedure DefaultSampleRateDidChange(SampleRate: TAsioSampleRate); cdecl;
begin
 {$IFDEF AllowMultipleAsioHosts}
 if Assigned(GAsioDriver[0])
  then GAsioDriver[0].InternalSampleRateDidChange(SampleRate);
 {$ELSE}
 if Assigned(GAsioDriver)
  then GAsioDriver.InternalSampleRateDidChange(SampleRate);
 {$ENDIF}
end;

function DefaultMessageHandler(Selector, Value: Integer; MessagePointer: Pointer;
  Opt: PDouble): Integer; cdecl;
begin
 {$IFDEF AllowMultipleAsioHosts}
 if Assigned(GAsioDriver[0])
  then Result := GAsioDriver[0].MessageHandler(Selector, Value, MessagePointer, Opt)
  else Result := 0;
 {$ELSE}
 if Assigned(GAsioDriver)
  then Result := GAsioDriver.MessageHandler(Selector, Value, MessagePointer, Opt)
  else Result := 0;
 {$ENDIF}
end;

{$IFDEF AllowMultipleAsioHosts}
procedure BufferSwitch1(DoubleBufferIndex: Integer; DirectProcess: TAsioBool); cdecl;
begin
 if Assigned(GAsioDriver[1])
  then GAsioDriver[1].InternalBufferSwitch(DoubleBufferIndex, DirectProcess);
end;

function BufferSwitchTimeInfo1(var Params: TAsioTime;
  DoubleBufferIndex: Integer; DirectProcess: TAsioBool): PAsioTime; cdecl;
begin
 if Assigned(GAsioDriver[1])
  then Result := GAsioDriver[1].InternalBufferSwitchTimeInfo(Params, DoubleBufferIndex, DirectProcess)
  else Result := nil;
end;

procedure SampleRateDidChange1(SampleRate: TAsioSampleRate); cdecl;
begin
 if Assigned(GAsioDriver[1])
  then GAsioDriver[1].InternalSampleRateDidChange(SampleRate);
end;

function MessageHandler1(Selector, Value: Integer; MessagePointer: Pointer;
  Opt: PDouble): Integer; cdecl;
begin
 if Assigned(GAsioDriver[1])
  then Result := GAsioDriver[1].MessageHandler(Selector, Value, MessagePointer, Opt)
  else Result := 0;
end;

procedure BufferSwitch2(DoubleBufferIndex: Integer; DirectProcess: TAsioBool); cdecl;
begin
 if Assigned(GAsioDriver[2])
  then GAsioDriver[2].InternalBufferSwitch(DoubleBufferIndex, DirectProcess);
end;

function BufferSwitchTimeInfo2(var Params: TAsioTime;
  DoubleBufferIndex: Integer; DirectProcess: TAsioBool): PAsioTime; cdecl;
begin
 if Assigned(GAsioDriver[2])
  then Result := GAsioDriver[2].InternalBufferSwitchTimeInfo(Params, DoubleBufferIndex, DirectProcess)
  else Result := nil;
end;

procedure SampleRateDidChange2(SampleRate: TAsioSampleRate); cdecl;
begin
 if Assigned(GAsioDriver[2])
  then GAsioDriver[2].InternalSampleRateDidChange(SampleRate);
end;

function MessageHandler2(Selector, Value: Integer; MessagePointer: Pointer;
  Opt: PDouble): Integer; cdecl;
begin
 if Assigned(GAsioDriver[2])
  then Result := GAsioDriver[2].MessageHandler(Selector, Value, MessagePointer, Opt)
  else Result := 0;
end;

procedure BufferSwitch3(DoubleBufferIndex: Integer; DirectProcess: TAsioBool); cdecl;
begin
 if Assigned(GAsioDriver[3])
  then GAsioDriver[3].InternalBufferSwitch(DoubleBufferIndex, DirectProcess);
end;

function BufferSwitchTimeInfo3(var Params: TAsioTime;
  DoubleBufferIndex: Integer; DirectProcess: TAsioBool): PAsioTime; cdecl;
begin
 if Assigned(GAsioDriver[3])
  then Result := GAsioDriver[3].InternalBufferSwitchTimeInfo(Params, DoubleBufferIndex, DirectProcess)
  else Result := nil;
end;

procedure SampleRateDidChange3(SampleRate: TAsioSampleRate); cdecl;
begin
 if Assigned(GAsioDriver[3])
  then GAsioDriver[3].InternalSampleRateDidChange(SampleRate);
end;

function MessageHandler3(Selector, Value: Integer; MessagePointer: Pointer;
  Opt: PDouble): Integer; cdecl;
begin
 if Assigned(GAsioDriver[3])
  then Result := GAsioDriver[3].MessageHandler(Selector, Value, MessagePointer, Opt)
  else Result := 0;
end;
{$ENDIF}
{$IFDEF DELPHI10_UP} {$endregion 'Asio callback functions'} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TAsioTimeSub implementation'} {$ENDIF}

////////////////////////////////////////////////////////////////////////////////
///////////////////////////////// TAsioTimeSub /////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

constructor TAsioTimeSub.Create;
begin
 with FBufferTime.timeInfo do
  begin
   Speed := 1;
   SampleRate := 44100;
   SamplePosition := Int64ToAsioSamples(0);
  end;
 Flags := [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid];
end;

procedure TAsioTimeSub.Change;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TAsioTimeSub.AssignTo(Dest: TPersistent);
begin
 if Dest is TAsioTimeSub then
  with TAsioTimeSub(Dest) do
   begin
    FBufferTime := Self.FBufferTime;
    Change;
   end
 else inherited AssignTo(Dest);
end;

function TAsioTimeSub.GetFlags: TAsioTimeFlags;
begin
 Result := [];
 if (FBufferTime.TimeInfo.Flags and kSystemTimeValid) <> 0
  then Include(Result, atSystemTimeValid)
  else Exclude(Result, atSystemTimeValid);
 if (FBufferTime.TimeInfo.Flags and kSamplePositionValid) <> 0
  then Include(Result, atSamplePositionValid)
  else Exclude(Result, atSamplePositionValid);
 if (FBufferTime.TimeInfo.Flags and kSampleRateValid) <> 0
  then Include(Result, atSampleRateValid)
  else Exclude(Result, atSampleRateValid);
 if (FBufferTime.TimeInfo.Flags and kSpeedValid) <> 0
  then Include(Result, atSpeedValid)
  else Exclude(Result, atSpeedValid);
 if (FBufferTime.TimeInfo.Flags and kSampleRateChanged) <> 0
  then Include(Result, atSampleRateChanged)
  else Exclude(Result, atSampleRateChanged);
 if (FBufferTime.TimeInfo.Flags and kClockSourceChanged) <> 0
  then Include(Result, atClockSourceChanged)
  else Exclude(Result, atClockSourceChanged);
end;

procedure TAsioTimeSub.SetFlags(Flags: TAsioTimeFlags);
var
  Temp: Integer;
begin
 Temp := 0;
 if (atSystemTimeValid in Flags) then Temp := Temp + kSystemTimeValid;
 if (atSamplePositionValid in Flags) then Temp := Temp + kSamplePositionValid;
 if (atSampleRateValid in Flags) then Temp := Temp + kSampleRateValid;
 if (atSpeedValid in Flags) then Temp := Temp + kSpeedValid;
 if (atSampleRateChanged in Flags) then Temp := Temp + kSampleRateChanged;
 if (atClockSourceChanged in Flags) then Temp := Temp + kClockSourceChanged;
 FBufferTime.TimeInfo.Flags := Temp;
end;

function TAsioTimeSub.GetSamplePosition: Int64;
begin
 Result := AsioSamplesToInt64(FBufferTime.TimeInfo.SamplePosition);
end;

function TAsioTimeSub.GetSampleRate: Double;
begin
 Result := FBufferTime.TimeInfo.SampleRate;
end;

function TAsioTimeSub.GetSpeed: Double;
begin
 Result := FBufferTime.TimeInfo.Speed;
end;

procedure TAsioTimeSub.SetSamplePosition(Value: Int64);
begin
 if Value <> AsioSamplesToInt64(FBufferTime.TimeInfo.SamplePosition) then
  begin
   FBufferTime.TimeInfo.SamplePosition := Int64ToAsioSamples(Value);
   Change;
  end;
end;

procedure TAsioTimeSub.SetSampleRate(const Value: Double);
begin
if Value <> FBufferTime.TimeInfo.sampleRate then
  begin
   FBufferTime.TimeInfo.sampleRate := Value;
   Change;
  end;
end;

procedure TAsioTimeSub.SetSpeed(const Value: Double);
begin
 if Value <> FBufferTime.TimeInfo.speed then
  begin
   FBufferTime.TimeInfo.Speed := Value;
   Change;
  end;
end;
{$IFDEF DELPHI10_UP} {$endregion 'TAsioTimeSub implementation'} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TAsioChannel implementation'} {$ENDIF}



{ TCustomAsioChannel }

constructor TCustomAsioChannel.Create(Driver: IStdCallAsio; Channel: Integer);
begin
 FDriver  := Driver;
 FChannel := Channel;
 FGain    := $20000000; // = 0 dB
end;

function TCustomAsioChannel.GetAlignedSampleSize: Byte;
begin
 // determine sample size
 if SampleType in [CAsioSTInt16MSB, CAsioSTInt16LSB]     then Result := SizeOf(Word) else
 if SampleType in [CAsioSTInt24MSB, CAsioSTInt24LSB]     then Result := 3 else
 if SampleType in [CAsioSTFloat32LSB, CAsioSTFloat32MSB] then Result := SizeOf(Single) else
 if SampleType in [CAsioSTFloat64LSB, CAsioSTFloat64MSB] then Result := SizeOf(Double) else
 if SampleType in [CAsioSTDSDInt8LSB1, CAsioSTDSDInt8LSB1, CAsioSTDSDInt8NER8]
  then Result := SizeOf(Byte)
  else Result := SizeOf(Integer);
end;

procedure TCustomAsioChannel.UpdateChannelInfo;
var
  ChannelInfo : TASIOChannelInfo;
begin
 ChannelInfo.Channel := Channel;
 FDriver.GetChannelInfo(ChannelInfo);
 FSampleType := ChannelInfo.SampleType;
 FName := string(ChannelInfo.Name);
end;


{ TAsioChannelInput }

function TAsioChannelInput.GetMeter: Integer;
var
  ACC : TAsioChannelControls;
begin
 if FDriver.Future(kAsioCanInputMeter, nil) = ASE_SUCCESS then
  begin
   ACC.IsInput := 1;
   ACC.Channel := FChannel;
   Result := ACC.Meter;

   FDriver.Future(kAsioCanInputMeter, @ACC);
  end
 else Result := -1;
end;

procedure TAsioChannelInput.InputMonitor(OutputChannel, Gain: Integer;
  Active: Boolean; Pan: Integer);
var
  AIM : TAsioInputMonitor;
begin
 // check if command can be transmitted
 if FDriver.Future(kAsioSupportsInputMonitor, nil) = ASE_SUCCESS then
  begin
   AIM.Input := FChannel;
   AIM.Output := OutputChannel;
   AIM.Gain := Gain;
   if Active
    then AIM.State := CAsioTrue
    else AIM.State := CAsioFalse;
   AIM.Pan := Pan;

   FDriver.Future(kAsioSupportsInputMonitor, @AIM);
  end;
end;

procedure TAsioChannelInput.SetGain(const Value: Integer);
var
  ACC : TAsioChannelControls;
begin
 // check if command can be transmitted
 if FDriver.Future(kAsioCanInputGain, nil) = ASE_SUCCESS then
  begin
   ACC.IsInput := 1;
   ACC.Channel := FChannel;
   ACC.Gain := Value;

   FDriver.Future(kAsioSetInputGain, @ACC);
  end;
end;


{ TAsioChannelOutput }

function TAsioChannelOutput.GetMeter: Integer;
var
  ACC : TAsioChannelControls;
begin
 if FDriver.Future(kAsioCanOutputMeter, nil) = ASE_SUCCESS then
  begin
   ACC.IsInput := 1;
   ACC.Channel := FChannel;
   Result := ACC.Meter;

   FDriver.Future(kAsioCanOutputMeter, @ACC);
  end
 else Result := -1;
end;

procedure TAsioChannelOutput.SetGain(const Value: Integer);
var
  ACC : TAsioChannelControls;
begin
 // check if command can be transmitted
 if FDriver.Future(kAsioCanOutputGain, nil) = ASE_SUCCESS then
  begin
   ACC.IsInput := 0;
   ACC.Channel := FChannel;
   ACC.Gain := Value;

   FDriver.Future(kAsioCanOutputGain, @ACC);
  end;
end;

{$IFDEF DELPHI10_UP} {$endregion 'TAsioChannel implementation'} {$ENDIF}



{ TAsioHostCore }

constructor TAsioHostCore.Create(ID: TGUID);
begin
 // initialize buffer pointers
 FUnalignedBuffer := nil;
 FInputBuffers := nil;
 FOutputBuffers := nil;

 // allocate handle
 FHandle := AllocateHWnd(WndProc);

 // create interface
 if CreateStdCallAsio(ID, FDriver) then
  begin
   // check driver is assigned
   if FDriver = nil
    then raise EAsioHost.Create(RStrAsioDriverFailed);

   // initialize callbacks
   InitializeCallbacks;

   // initialize driver
   InitializeDriver;
  end;
end;

destructor TAsioHostCore.Destroy;
begin
 // eventually dispose buffers
 if BuffersCreated
  then DisposeBuffers;

 // free asio time
 FreeAndNil(FAsioTime);

 // deallocate handle
 DeallocateHWnd(FHandle);

 FDriver := nil;
 inherited;
end;

procedure TAsioHostCore.InitializeCallbacks;
begin
 {$IFDEF AllowMultipleAsioHosts}
 if GAsioDriver[0] = nil then
  begin
   FAsioDriverIndex := 0;
   GAsioDriver[FAsioDriverIndex] := Self;

   // set the callbacks record fields
   with FCallbacks do
    begin
     BufferSwitch := DefaultBufferSwitch;
     SampleRateDidChange := DefaultSampleRateDidChange;
     BufferSwitchTimeInfo := DefaultBufferSwitchTimeInfo;
     AsioMessage := DefaultMessageHandler;
    end;
  end else
 if GAsioDriver[1] = nil then
  begin
   FAsioDriverIndex := 1;
   GAsioDriver[FAsioDriverIndex] := Self;

   // set the callbacks record fields
   with FCallbacks do
    begin
     BufferSwitch := BufferSwitch1;
     SampleRateDidChange := SampleRateDidChange1;
     BufferSwitchTimeInfo := BufferSwitchTimeInfo1;
     AsioMessage := MessageHandler1;
    end;
  end else
 if GAsioDriver[2] = nil then
  begin
   FAsioDriverIndex := 2;
   GAsioDriver[FAsioDriverIndex] := Self;

   // set the callbacks record fields
   with FCallbacks do
    begin
     BufferSwitch := BufferSwitch2;
     SampleRateDidChange := SampleRateDidChange2;
     BufferSwitchTimeInfo := BufferSwitchTimeInfo2;
     AsioMessage := MessageHandler2;
    end;
  end else
 if GAsioDriver[3] = nil then
  begin
   FAsioDriverIndex := 3;
   GAsioDriver[FAsioDriverIndex] := Self;

   // set the callbacks record fields
   with FCallbacks do
    begin
     BufferSwitch := BufferSwitch3;
     SampleRateDidChange := SampleRateDidChange3;
     BufferSwitchTimeInfo := BufferSwitchTimeInfo3;
     AsioMessage := MessageHandler3;
    end;
  end else raise EAsioHost.Create(RCStrOnlyOneAsioHost);
 {$ELSE}
 if GAsioDriver = nil then
  begin
   GAsioDriver := Self;

   // set the callbacks record fields
   with FCallbacks do
    begin
     BufferSwitch := DefaultBufferSwitch;
     SampleRateDidChange := DefaultSampleRateDidChange;
     BufferSwitchTimeInfo := DefaultBufferSwitchTimeInfo;
     AsioMessage := DefaultMessageHandler;
    end;
  end
 else raise EAsioHost.Create(RCStrOnlyOneAsioHost);
 {$ENDIF}
end;

procedure TAsioHostCore.InitializeDriver;
var
  ErrorMessage : PAnsiChar;
begin
 case FDriver.Init(FHandle) of
  0 : begin
       // equals false
       GetMem(ErrorMessage, 128);
       try
        FDriver.GetErrorMessage(ErrorMessage);
        raise EAsioHost.Create(string(ErrorMessage));
       finally
        Dispose(ErrorMessage);
       end;
      end;

  // the below codes are here due to incompatibility of some soundcards
  ASE_NotPresent       : raise EAsioHost.Create(RCStrDriverNotPresent);
  ASE_HWMalfunction    : raise EAsioHost.Create(RCStrHardwareMalfunction);
  ASE_InvalidParameter : raise EAsioHost.Create(RCStrInputParameterInvalid);
  ASE_InvalidMode      : raise EAsioHost.Create(RCStrInvalidMode);
  ASE_SPNotAdvancing   : raise EAsioHost.Create(RCStrSPNotAdvancing);
  ASE_NoClock          : raise EAsioHost.Create(RCStrNoClock);
  ASE_NoMemory         : raise EAsioHost.Create(RCStrNoMemory);
 end;

 // initialize time
 FSampleRate := 44100;
 FAsioTime := TAsioTimeSub.Create;

 // initialize asio support
 FAsioSupports := [asTimeInfo, asTimeCode];

 // aquire driver name
 AquireDriverName;

 // aquire driver version
 AquireDriverVersion;

 // aquire can dos
 AquireCanDos;

 // aquire clock sources
 AquireClockSources;
end;

procedure TAsioHostCore.CreateBuffers;
var
  Channel      : Integer;
  Buffer       : PAsioBufferInfos;
  ChannelCount : array [0..1] of Integer;
begin
 // eventually dispose buffers
 if BuffersCreated
  then DisposeBuffers;

 // get defaults
 DetermineBuffersize;
 AquireSampleRate;

 // get channel counts (input / output)
 FDriver.GetChannels(ChannelCount[0], ChannelCount[1]);

 // allocate memory for input and output buffers
 GetMem(FUnalignedBuffer, SizeOf(TAsioBufferInfo) * (ChannelCount[0] + ChannelCount[1]) + 16);
 Buffer := PAsioBufferInfos((Integer(FUnalignedBuffer) + 15) and (not $F));

 // setup input channel info and converter
 FInputBuffers := Buffer;

 // eventually free unused channels
 for Channel := ChannelCount[0] to Length(FInputChannels) - 1 do
  if Assigned(FInputChannels)
   then FreeAndNil(FInputChannels);

 // update input channel array
 SetLength(FInputChannels, ChannelCount[0]);

 // eventually create missing channels
 for Channel := 0 to ChannelCount[0] - 1 do
  if not Assigned(FInputChannels[Channel])
   then FInputChannels[Channel] := TAsioChannelInput.Create(FDriver, Channel);

 // update channel info and buffers
 for Channel := 0 to ChannelCount[0] - 1 do
  begin
   FInputChannels[Channel].UpdateChannelInfo;

   Buffer^[0].IsInput := CAsioTrue;
   Buffer^[0].ChannelNum := Channel;
   Buffer^[0].Buffers[0] := nil;
   Buffer^[0].Buffers[1] := nil;
   Inc(Buffer);
  end;

 // setup input channel info and converter
 FOutputBuffers := Buffer;

 // eventually free unused channels
 for Channel := ChannelCount[1] to Length(FOutputChannels) - 1 do
  if Assigned(FOutputChannels)
   then FreeAndNil(FOutputChannels);

 // update Output channel array
 SetLength(FOutputChannels, ChannelCount[1]);

 // eventually create missing channels
 for Channel := 0 to ChannelCount[1] - 1 do
  if not Assigned(FOutputChannels[Channel])
   then FOutputChannels[Channel] := TAsioChannelOutput.Create(FDriver, Channel);

 for Channel := 0 to ChannelCount[1] - 1 do
  begin
   FOutputChannels[Channel].UpdateChannelInfo;

   Buffer^[0].IsInput := CAsioFalse;
   Buffer^[0].ChannelNum := Channel;
   Buffer^[0].Buffers[0] := nil;
   Buffer^[0].Buffers[1] := nil;
   Inc(Buffer);
  end;

 Assert(FBufferSize > 0);

 if not FDriver.CreateBuffers(FInputBuffers,
   (ChannelCount[0] + ChannelCount[1]), FBufferSize, FCallbacks) = ASE_OK
  then raise EAsioHost.Create(RCStrBufferAllocationError);

 // get current latencies
 FDriver.GetLatencies(FInputLatency, FOutputLatency);
end;

procedure TAsioHostCore.DisposeBuffers;
var
  Channel : Integer;
begin
 if (FDriver = nil)
  then Exit;

 if BuffersCreated then
  begin
   // dispose buffers from interface
   FDriver.DisposeBuffers;

   // dispose allocated memory and clear pointers
   FInputBuffers := nil;
   FOutputBuffers := nil;
   Dispose(FUnalignedBuffer);
   FUnalignedBuffer := nil;

   // free channels
   for Channel := 0 to Length(FInputChannels) - 1
    do FreeAndNil(FInputChannels[Channel]);
   for Channel := 0 to Length(FOutputChannels) - 1
    do FreeAndNil(FOutputChannels[Channel]);

   SetLength(FInputChannels, 0);
   SetLength(FOutputChannels, 0);
  end;
end;

function TAsioHostCore.GetBuffersCreated: Boolean;
begin
 Result := FUnalignedBuffer <> nil;
end;

class function TAsioHostCore.GetInputChannelClass: TAsioChannelInputClass;
begin
 Result := TAsioChannelInput;
end;

class function TAsioHostCore.GetOutputChannelClass: TAsioChannelOutputClass;
begin
 Result := TAsioChannelOutput;
end;

function TAsioHostCore.GetInputChannel(Index: Integer): TAsioChannelInput;
begin
 if (Index >= 0) and (Index < Length(FInputChannels))
  then Result := FInputChannels[Index]
  else raise EAsioHost.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TAsioHostCore.GetInputChannelCount: Integer;
begin
 Result := Length(FInputChannels);
end;

function TAsioHostCore.GetOutputChannel(Index: Integer): TAsioChannelOutput;
begin
 if (Index >= 0) and (Index < Length(FOutputChannels))
  then Result := FOutputChannels[Index]
  else raise EAsioHost.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TAsioHostCore.GetOutputChannelCount: Integer;
begin
 Result := Length(FOutputChannels);
end;

procedure TAsioHostCore.DetermineBuffersize;
begin
 // query buffer sizes
 FDriver.GetBufferSize(FMin, FMax, FPrefered, FGranularity);

 if FMin = FMax then FPrefered := FMin;

 // check prefered buffersize is valid
 if FPrefered <= 0
  then raise EAsioHost.Create(RCStrPreferedBufferSize);

 FBufferSize := FPrefered;
end;

procedure TAsioHostCore.AquireSampleRate;
begin
 FDriver.GetSampleRate(FSampleRate);

 // synchronize asio time with current samplerate
 AsioTime.SampleRate := FSampleRate;
end;

procedure TAsioHostCore.AquireClockSources;
var
  ClockSources     : PAsioClockSources;
  ClockSourceCount : Integer;
begin
 ClockSourceCount := 8;
 GetMem(ClockSources, ClockSourceCount * SizeOf(TAsioClockSource));
 try
  FillChar(ClockSources^, ClockSourceCount * SizeOf(TAsioClockSource), 0);
  FDriver.GetClockSources(ClockSources, ClockSourceCount);

  SetLength(FClockSources, ClockSourceCount);
  Move(ClockSources^, FClockSources[0], ClockSourceCount * SizeOf(TAsioClockSource));
 finally
  Dispose(ClockSources);
 end;
end;

procedure TAsioHostCore.AquireCanDos;
begin
 // check whether driver is has been assigned
 if FDriver = nil then
  begin
   FAsioCanDos := [];
   Exit;
  end;

 // test can do 'Time Info'
 if FDriver.Future(kAsioCanTimeInfo, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdTimeInfo]
  else FAsioCanDos := FAsioCanDos - [acdTimeInfo];

 // test can do 'time code'
 if FDriver.Future(kAsioCanTimeCode, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdTimeCode]
  else FAsioCanDos := FAsioCanDos - [acdTimeCode];

 // test can do 'transport'
 if FDriver.Future(kAsioCanTransport, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdTransport]
  else FAsioCanDos := FAsioCanDos - [acdTransport];

 // test can do 'input gain'
 if FDriver.Future(kAsioCanInputGain, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdInputGain]
  else FAsioCanDos := FAsioCanDos - [acdInputGain];

 // test can do 'input meter'
 if FDriver.Future(kAsioCanInputMeter, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdInputMeter]
  else FAsioCanDos := FAsioCanDos - [acdInputMeter];

 // test can do 'output gain'
 if FDriver.Future(kAsioCanOutputGain, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdOutputGain]
  else FAsioCanDos := FAsioCanDos - [acdOutputGain];

 // test can do 'output meter'
 if FDriver.Future(kAsioCanOutputMeter, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdOutputMeter]
  else FAsioCanDos := FAsioCanDos - [acdOutputMeter];

 // test can do 'set I/O format'
 if FDriver.Future(kAsioSetIoFormat, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdSetIoFormat]
  else FAsioCanDos := FAsioCanDos - [acdSetIoFormat];

 // test can do 'get I/O format'
 if FDriver.Future(kAsioSetIoFormat, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdGetIoFormat]
  else FAsioCanDos := FAsioCanDos - [acdGetIoFormat];

 // test can do 'can do I/O format'
 if FDriver.Future(kAsioSetIoFormat, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdCanDoIoFormat]
  else FAsioCanDos := FAsioCanDos - [acdCanDoIoFormat];
end;

procedure TAsioHostCore.AquireDriverName;
var
  DriverName : array [0..255] of AnsiChar;
begin
 FDriver.GetDriverName(DriverName);
 if DriverName <> ''
  then FDriverName := string(DriverName);
end;

procedure TAsioHostCore.AquireDriverVersion;
begin
 FDriverVersion := FDriver.GetDriverVersion;
end;

procedure TAsioHostCore.ControlPanel;
begin
 FDriver.ControlPanel;
end;


{$IFNDEF FPC}
procedure TAsioHostCore.DefaultHandler(var Message);
begin
 with TMessage(Message)
  do Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;

procedure TAsioHostCore.WndProc(var Msg: TMessage);
begin
 with Msg do Dispatch(Msg);
end;

{$ELSE}

function DefWindowProc(hWnd: THandle; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LResult; external 'user32' name 'DefWindowProcA';

procedure TAsioHostCore.DefaultHandler(var Message);
begin
 with TLMessage(Message)
  do Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;

procedure TAsioHostCore.WndProc(var Msg: TLMessage);
begin
 with Msg do Dispatch(Msg);
end;
{$ENDIF}

{$IFDEF FPC}
procedure TAsioHostCore.PMAsio(var Message: TLMessage);
{$ELSE}
procedure TAsioHostCore.PMAsio(var Message: TMessage);
{$ENDIF}
begin
 if FDriver = nil then exit;
 case TAsioMessage(Message.WParam) of
  amBufferSwitch         : BufferSwitch(Message.LParam); // process a buffer
  amBufferSwitchTimeInfo : BufferSwitchTimeInfo(Message.LParam, AsioTime.FBufferTime);  // process a buffer with time
  amResetRequest         : Reset;
  amBufferSizeChange     : BufferSizeChange;
  amLatencyChanged       : LatencyChanged;
  amResyncRequest        : ResyncRequest;
 end;
end;

{$IFDEF FPC}
procedure TAsioHostCore.PMBufferSwitch(var Message: TLMessage);
{$ELSE}
procedure TAsioHostCore.PMBufferSwitch(var Message: TMessage);
{$ENDIF}
begin
 BufferSwitch(Message.LParam);
end;

{$IFDEF FPC}
procedure TAsioHostCore.PMBufferSwitchTimeInfo(var Message: TLMessage);
{$ELSE}
procedure TAsioHostCore.PMBufferSwitchTimeInfo(var Message: TMessage);
{$ENDIF}
begin
 BufferSwitchTimeInfo(Message.LParam, AsioTime.FBufferTime);
end;

procedure TAsioHostCore.Reset;
var
  CurrentBuffersCreated : Boolean;
begin
 // store whether the buffers have been created
 CurrentBuffersCreated := BuffersCreated;
 if BuffersCreated
  then DisposeBuffers;

 // re-initialize driver
 InitializeDriver;

 // eventually recreate buffers
 if CurrentBuffersCreated
  then CreateBuffers;

 if Assigned(FOnReset) then FOnReset(Self);
end;

procedure TAsioHostCore.ResyncRequest;
begin
 LatencyChanged;
end;

procedure TAsioHostCore.LatencyChanged;
begin
 if Assigned(FDriver)
  then FDriver.GetLatencies(FInputLatency, FOutputLatency);

 if Assigned(FOnLatencyChanged)
  then FOnLatencyChanged(Self);
end;

procedure TAsioHostCore.BufferSizeChange;
begin
 Reset;
end;

procedure TAsioHostCore.InternalBufferSwitch(DoubleBufferIndex: Integer;
  DirectProcess: TAsioBool);
begin
 case DirectProcess of
  CAsioFalse: PostMessage(FHandle, PM_BufferSwitch, Integer(amBufferSwitch),
    DoubleBufferIndex);
  CAsioTrue : BufferSwitch(DoubleBufferIndex);
 end;
end;

function TAsioHostCore.InternalBufferSwitchTimeInfo(var Params: TAsioTime;
  DoubleBufferIndex: Integer; DirectProcess: TAsioBool): PAsioTime;
begin
 case DirectProcess of
  CAsioFalse :
   begin
    AsioTime.FBufferTime := Params;
    PostMessage(FHandle, PM_BufferSwitchTimeInfo,
      Integer(amBufferSwitchTimeInfo), DoubleBufferIndex);
   end;
  CAsioTrue : BufferSwitchTimeInfo(DoubleBufferIndex, Params);
 end;

 Result := nil;
end;

procedure TAsioHostCore.InternalSampleRateDidChange(
  SampleRate: TAsioSampleRate);
begin
 SetSampleRate(SampleRate);
 SampleRateChanged;
end;

procedure TAsioHostCore.BufferSwitch(Index: Integer);
begin
 with AsioTime.FBufferTime do
  begin
   FillChar(TimeCode, SizeOf(TAsioTimeCode), 0);

   // get the time stamp of the buffer, not necessary if no
   // synchronization to other media is required
   if FDriver.GetSamplePosition(TimeInfo.SamplePosition, TimeInfo.SystemTime) = ASE_OK
    then AsioTime.Flags := AsioTime.Flags + [atSystemTimeValid, atSamplePositionValid];
  end;

 BufferSwitchTimeInfo(Index, AsioTime.FBufferTime);
end;

procedure TAsioHostCore.BufferSwitchTimeInfo(Index: Integer;
  const Params: TAsioTime);
begin
 if FDriver = nil then exit;
 PMUpdSamplePos.WParam := Params.TimeInfo.SamplePosition.Hi;
 PMUpdSamplePos.LParam := Params.TimeInfo.SamplePosition.Lo;
 Dispatch(PMUpdSamplePos);

 if Assigned(FOnBufferSwitch)
  then FOnBufferSwitch(Self, @(FInputBuffers^), Index);

 FDriver.OutputReady;
end;

function TAsioHostCore.MessageHandler(Selector, Value: Integer;
  MessagePointer: Pointer; Optional: PDouble): LongInt;
begin
 Result := 0;
 case Selector of
  kAsioSelectorSupported    : Result := CAsioTrue; // return 1 if a selector is supported
  kAsioEngineVersion        : Result := 2;         // return 2 if Asio 2 is supported
  kAsioResetRequest         : PostAsioMessage(amResetRequest);
  kAsioBufferSizeChange     : PostAsioMessage(amBufferSizeChange);
  kAsioResyncRequest        : PostAsioMessage(amResyncRequest);
  kAsioLatenciesChanged     : PostAsioMessage(amLatencyChanged);
  kAsioSupportsTimeInfo     : Result := Integer(asTimeInfo in Supports);
  kAsioSupportsTimeCode     : Result := Integer(asTimeCode in Supports);
  kAsioSupportsInputMonitor : Result := Integer(asInputMonitor in Supports);
 end;
end;

procedure TAsioHostCore.PostAsioMessage(AsioMessage: TAsioMessage;
  Value: Integer = 0);
begin
 PostMessage(FHandle, PM_Asio, Integer(AsioMessage), Value);
end;

procedure TAsioHostCore.SampleRateChanged;
begin
 AsioTime.SampleRate := FSampleRate;
 if Assigned(FOnSampleRateChanged)
  then FOnSampleRateChanged(Self);
end;

procedure TAsioHostCore.SetSampleRate(Value: Double);
begin
 // check for a valid samplerate
 Value := Abs(Value);
 if (Value = 0)
  then raise EAsioHost.Create(RCStrWrongSamplerate);

 // check if samplerate is supported
 if Assigned(FDriver) then
  if FDriver.CanSampleRate(Value) <> ASE_OK
   then Exit;

 if FSampleRate <> Value then
  if FDriver.SetSampleRate(Value) = ASE_OK then
   begin
    FSampleRate := Value;
    SampleRateChanged;
   end;
end;

procedure TAsioHostCore.Start;
begin
 FRunning := FDriver.Start = ASE_OK;
end;

procedure TAsioHostCore.Stop;
begin
 FRunning := False;
 FDriver.Stop;

 if BuffersCreated
  then ClearBuffers;
end;

function TAsioHostCore.CanSampleRate(SampleRate: TAsioSampleRate): Boolean;
begin
 Result := FDriver.CanSampleRate(SampleRate) = ASE_OK;
end;

procedure TAsioHostCore.ClearBuffers;
var
  Buffer     : PAsioBufferInfos;
  Channel    : Integer;
  SampleSize : Word;
begin
 try
  // clear output buffer
  Buffer := FOutputBuffers;
  if Assigned(Buffer) then
   for Channel := 0 to Length(FOutputChannels) - 1 do
    with FOutputChannels[Channel] do
     begin
      SampleSize := AlignedSampleSize;

      // finally clear buffer
      Assert(Assigned(Buffer));
      with Buffer^[0] do
       begin
        if Assigned(Buffers[0]) then FillChar(Buffers[0]^, FBufferSize * SampleSize, 0);
        if Assigned(Buffers[1]) then FillChar(Buffers[1]^, FBufferSize * SampleSize, 0);
       end;
      Inc(Buffer);
     end;

  // clear input buffer
  Buffer := FInputBuffers;
  if Assigned(Buffer) then
   for Channel := 0 to Length(FInputChannels) - 1 do
    with FInputChannels[Channel] do
     begin
      // determine sample size
      SampleSize := AlignedSampleSize;

      // finally clear buffer
      Assert(Assigned(Buffer));
      with Buffer^[0] do
       begin
        if Assigned(Buffers[0]) then FillChar(Buffers[0]^, FBufferSize * SampleSize, 0);
        if Assigned(Buffers[1]) then FillChar(Buffers[1]^, FBufferSize * SampleSize, 0);
       end;
      Inc(Buffer);
     end;
 except
 end;
end;

initialization
 PMUpdSamplePos.Msg := PM_UpdateSamplePosition;

end.
