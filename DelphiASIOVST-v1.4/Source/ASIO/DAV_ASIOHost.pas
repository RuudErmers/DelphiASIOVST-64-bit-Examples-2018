unit DAV_AsioHost;

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
//  The initial developer of this code is Tobias Fleischer and                //
//  Christian-W. Budde, based on a code snipped by Frederic Vanmol            //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2003-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
//  Contributor(s):                                                           //
//    Martin Fay (original Delphi ASIO interface, author of OpenAsio)         //
//    Benjamin Rosseaux (author of the stdcall interface)                     //
//    Maik Menz (various refactorings)                                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LclType, LMessages, LResources,
  {$ELSE}Windows, Messages,{$ENDIF}
  {$IFDEF OpenASIO} DAV_OpenAsio {$ELSE} DAV_AsioInterface {$ENDIF},
  {$IFDEF ASIOMixer} Forms, ComCtrls, Graphics, StdCtrls, DAV_ASIOMixer,{$ENDIF}
  {$IFDEF DELPHI5} Forms, DsgnIntf, {$ENDIF}
  SysUtils, Classes, DAV_Types, DAV_Asio, DAV_AsioList,
  DAV_AsioConvert, DAV_AsioGenerator;

const
  {$IFDEF SUPPORTS_REGION} {$region 'Message constants'} {$ENDIF}
  // private message
  PM_Asio = WM_User + 1652;        // unique we hope

  // Asio message(s), as wParam for PM_Asio
  AM_ResetRequest         = 0;
  AM_BufferSwitch         = 1;     // new buffer index in lParam
  AM_BufferSwitchTimeInfo = 2;     // new buffer index in lParam
  AM_LatencyChanged       = 3;

  PM_UpdateSamplePos      = PM_Asio + 1;  // sample pos in wParam (hi) and lParam (lo)
  PM_BufferSwitch         = PM_Asio + 2;
  PM_BufferSwitchTimeInfo = PM_Asio + 3;
  PM_Reset                = PM_Asio + 4;
  {$IFDEF SUPPORTS_REGION} {$endregion 'Message constants'} {$ENDIF}

type
  {$IFDEF SUPPORTS_REGION} {$region 'Basic types'} {$ENDIF}
  TAsioBufferList = array [0..0] of TAsioBufferInfo;
  PAsioBufferList = ^TAsioBufferList;

  TAsioSupport = (assSupportsTimeInfo, assSupportsTimeCode,
    assSupportsInputMonitor);
  TAsioSupports = set of TAsioSupport;                        

  TAsioCanDo = (acdInputMonitor, acdTimeInfo, acdTimeCode, acdTransport,
                acdInputGain, acdInputMeter, acdOutputGain, acdOutputMeter);
  TAsioCanDos = set of TAsioCanDo;
  TAsioOutputDither = (odNone, odUDF, odTDF);

  TConvertMethod = (cmNone, cm32, cm64);
  TConvertOptimization = (coSSE, co3DNow);
  TConvertOptimizations = set of TConvertOptimization;

  TSamplePositionUpdateEvent = procedure(Sender: TObject; SamplePosition: Int64) of object;
  TBufferSwitchEvent32 = procedure(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray) of object;
  TBufferSwitchEvent64 = procedure(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray) of object;
  TBufferSwitchEventNative = procedure(Sender: TObject; const BufferInfo: PAsioBufferList; const BufferIndex : Integer) of object;

  TBufferPreFill = (bpfNone, bpfZero, bpfNoise, bpfCustom);
  TPreventClipping = (pcNone, pcDigital, pcAnalog);

  TCustomAudioDevice = class(TComponent);

  {$IFDEF SUPPORTS_REGION} {$endregion 'Basic types'} {$ENDIF}

  {$IFDEF SUPPORTS_REGION} {$region 'TAsioTimeSub'} {$ENDIF}
  TATFlag = (atSystemTimeValid, atSamplePositionValid, atSampleRateValid,
             atSpeedValid, atSampleRateChanged, atClockSourceChanged);
  TATFlags = set of TATFlag;

  TAsioTimeSub = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    function GetATInt64(Index: Integer): Int64;
    function GetATdouble(Index: Integer): Double;
    function GetATFlags: TATFlags;
    procedure SetATInt64(Index: Integer; Value: Int64);
    procedure SetATdouble(Index: Integer; Value: Double);
    procedure SetATFlags(Flags: TATFlags);
  protected
    FBufferTime: TAsioTime;
    procedure Change; dynamic;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property OnChanged: TNotifyEvent read FOnChange write FOnChange;
    constructor Create;
  published
    property SamplePos: Int64 index 0 read GetATInt64 write SetATInt64;
    property Speed : Double index 0 read  GetATdouble write SetATdouble; //absolute speed (1. = nominal)
    property SampleRate: Double Index 1 read GetATdouble write SetATdouble;
    property Flags : TATFlags read GetATFlags Write SetATFlags;
  end;
  {$IFDEF SUPPORTS_REGION} {$endregion 'TAsioTimeSub'} {$ENDIF}

  {$IFDEF SUPPORTS_REGION} {$region 'Delphi5 Control panel'} {$ENDIF}
  {$IFDEF D5CP}
  TAsioControlPanel = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
  {$ENDIF}
  {$IFDEF SUPPORTS_REGION} {$endregion 'Delphi5 Control panel'} {$ENDIF}

  {$IFDEF SUPPORTS_REGION} {$region 'TAsioHostBasic'} {$ENDIF}
  TCustomAsioHostBasic = class(TCustomAudioDevice)
  private
    FMin, FMax            : Integer;
    FPref, FGran          : Integer;
    FOnBufferSwitchNative : TBufferSwitchEventNative;
    function GetInputChannelInfo(Index: Integer): TAsioChannelInfo;
    function GetOutputChannelInfo(Index: Integer): TAsioChannelInfo;
    function GetOutConverter(ConverterType: TAsioSampleType): TOutConverter;
    procedure UpdateCanDos;
    procedure ResetDriverSpecificData;
    procedure ClearBuffers;
    procedure GetCurrentClockSource;
  protected
    {$IFDEF OpenAsio}
    FDriver               : IOpenAsio;
    {$ELSE}
    FDriver               : IStdCallAsio;
    {$ENDIF}
    FHandle               : THandle;
    FAsioTime             : TAsioTimeSub;
    FEngineVersion        : Integer;
    FBuffersCreated       : Boolean;
    FOnCreate             : TNotifyEvent;
    FOnDestroy            : TNotifyEvent;
    FOnReset              : TNotifyEvent;
    FOnDriverChanged      : TNotifyEvent;
    FOnLatencyChanged     : TNotifyEvent;
    FOnSampleRateChanged  : TNotifyEvent;
    FOnBuffersCreate      : TNotifyEvent;
    FOnBuffersDispose     : TNotifyEvent;
    FOnUpdateSamplePos    : TSamplePositionUpdateEvent;
    FOnBufferSwitch       : TBufferSwitchEventNative;
    FAsioCanDos           : TAsioCanDos;
    FAsioDriverList       : TDAVAsioDriverList;
    FCallbacks            : TAsioCallbacks;
    FUnAlignedBuffer      : PAsioBufferInfo;
    FSampleRate           : Double;
    FInputBuffers         : PAsioBufferInfos;
    FOutputBuffers        : PAsioBufferInfos;
    FActive               : Boolean;
    FDriverIndex          : Integer;
    FDriverName           : String;
    FDriverVersion        : Integer;
    FInputLatency         : Integer;
    FOutputLatency        : Integer;
    FInputChannelCount    : Integer;
    FOutputChannelCount   : Integer;
    FBufferSize           : Cardinal;
    FInputChannelInfos    : array of TAsioChannelInfo;
    FOutputChannelInfos   : array of TAsioChannelInfo;
    FInConverters         : array of TInConverter;
    FOutConverters        : array of TOutConverter;
    FAsioSupports         : TAsioSupports;
    function GetDriverList: TStrings;
    procedure SetActive(Value: Boolean); virtual;
    procedure SetDriverIndex(Value: Integer); virtual;
    procedure SetDriverName(const s: String); virtual;
    {$IFDEF FPC}
    procedure WndProc(var Msg: TLMessage);
    procedure PMAsio(var Message: TLMessage); message PM_Asio;
    procedure PMReset(var Message: TLMessage); message PM_Reset;
    procedure PMUpdateSamplePos(var Message: TLMessage); message PM_UpdateSamplePos;
    procedure PMBufferSwitch(var Message: TLMessage); message PM_BufferSwitch;
    procedure PMBufferSwitchTimeInfo(var Message: TLMessage); message PM_BufferSwitchTimeInfo;
    {$ELSE}
    procedure WndProc(var Msg: TMessage);
    procedure PMAsio(var Message: TMessage); message PM_Asio;
    procedure PMReset(var Message: TMessage); message PM_Reset;
    procedure PMUpdateSamplePos(var Message: TMessage); message PM_UpdateSamplePos;
    procedure PMBufferSwitch(var Message: TMessage); message PM_BufferSwitch;
    procedure PMBufferSwitchTimeInfo(var Message: TMessage); message PM_BufferSwitchTimeInfo;
    {$ENDIF}
    function CreateBuffers: Boolean; virtual;
    function GetInputMeter(Channel: Integer): Integer; virtual;
    function GetOutputMeter(Channel: Integer): Integer; virtual;
    function GetInConverter(ConverterType: TAsioSampleType): TInConverter;
    procedure BufferSwitch(Index: Integer); virtual;
    procedure BufferSwitchTimeInfo(Index: Integer; const params: TAsioTime); virtual;
    procedure DisposeBuffers; virtual;
    procedure ReadState(Reader: TReader); override;
    procedure DetermineBuffersize; virtual;
    procedure AquireCurrentSampleRate;
    procedure SetSampleRate(Value: Double); virtual;
    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefaultHandler(var Message); override;

    function CanSampleRate(SampleRate: TAsioSampleRate): TAsioError; virtual;
    function GetNumDrivers: Integer; virtual;
    procedure CloseDriver; virtual;
    procedure ControlPanel; virtual;
    procedure OpenDriver; virtual;
    procedure Reset; virtual;
    procedure SetInputGain(Channel, Gain: Integer); virtual;
    procedure SetOutputGain(Channel, Gain: Integer); virtual;

    procedure SetIgnoredDriver(ignore: TGuid);

    property Active: Boolean read FActive write SetActive default False;
    property AsioTime: TAsioTimeSub read FAsioTime Write FAsioTime;
    property EngineVersion: Integer read FEngineVersion write FEngineVersion default 2;
    property BufferGranularity: Integer read FGran stored False;
    property BufferMaximum: Integer read FMax stored False;
    property BufferMinimum: Integer read FMin stored False;
    property BufferPreferredSize: Integer read FPref stored False;
    property BufferSize: Cardinal read FBufferSize stored False default 1;
    property CanDos : TAsioCanDos read FAsioCanDos;
    property DriverIndex: Integer read FDriverIndex Write SetDriverIndex default -1;
    property DriverList: TStrings read GetDriverList;
    property DriverName: string read FDriverName write SetDriverName;
    property DriverVersion: Integer read FDriverVersion;
    property InputChannelCount: Integer read FInputChannelCount stored False default 0;
    property InputChannelInfos[index : Integer] : TAsioChannelInfo read GetInputChannelInfo;
    property InputLatency: Integer read FInputLatency stored False default 0;
    property InputMeter[Channel:Integer]: Integer read GetInputMeter;
    property OnBuffersCreate: TNotifyEvent read FOnBuffersCreate write FOnBuffersCreate;
    property OnBuffersDispose: TNotifyEvent read FOnBuffersDispose write FOnBuffersDispose;
    property OnBufferSwitch: TBufferSwitchEventNative read FOnBufferSwitchNative write FOnBufferSwitchNative;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnDriverChanged: TNotifyEvent read FOnDriverChanged write FOnDriverChanged;
    property OnLatencyChanged: TNotifyEvent read FOnLatencyChanged write FOnLatencyChanged;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
    property OnSampleRateChanged: TNotifyEvent read FOnSampleRateChanged write FOnSampleRateChanged;
    property OnUpdateSamplePos: TSamplePositionUpdateEvent read FOnUpdateSamplePos write FOnUpdateSamplePos;
    property OutputChannelCount: Integer read FOutputChannelCount stored False default 0;
    property OutputChannelInfos[index : Integer] : TAsioChannelInfo read GetOutputChannelInfo;
    property OutputLatency: Integer read FOutputLatency stored False default 0;
    property OutputMeter[Channel:Integer]: Integer read GetOutputMeter;
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property Supports: TAsioSupports read FAsioSupports write FAsioSupports default [assSupportsTimeInfo, assSupportsTimeCode];
  end;

  TAsioHostBasic = class(TCustomAsioHostBasic)
  published
    property Active;
    property AsioTime;
    property BufferGranularity;
    property BufferMaximum;
    property BufferMinimum;
    property BufferPreferredSize;
    property BufferSize;
    property CanDos;
    property DriverIndex;
    property DriverList;
    property DriverName;
    property DriverVersion;
    property InputChannelCount;
    property InputLatency;
    property OutputChannelCount;
    property OutputLatency;
    property SampleRate;
    property Supports;
    property OnBuffersCreate;
    property OnBuffersDispose;
    property OnBufferSwitch;
    property OnCreate;
    property OnDestroy;
    property OnDriverChanged;
    property OnLatencyChanged;
    property OnReset;
    property OnSampleRateChanged;
    property OnUpdateSamplePos;
  end;
  {$IFDEF SUPPORTS_REGION} {$endregion 'TAsioHostBasic'} {$ENDIF}

  {$IFDEF SUPPORTS_REGION} {$region 'TAsioHost'} {$ENDIF}
  TCustomAsioHost = class(TCustomAsioHostBasic)
  private
    FPreventClipping      : TPreventClipping;
    FInBufferPreFill      : TBufferPreFill;
    FOutBufferPreFill     : TBufferPreFill;
    FOnBufferSwitch32     : TBufferSwitchEvent32;
    FOnBufferSwitch64     : TBufferSwitchEvent64;
    FOnBufferSwitchNative : TBufferSwitchEventNative;
    FAsioGenerator        : TAsioGenerator;
    FSingleInBuffer       : TDAVArrayOfSingleFixedArray;
    FSingleOutBuffer      : TDAVArrayOfSingleFixedArray;
    FDoubleInBuffer       : TDAVArrayOfDoubleFixedArray;
    FDoubleOutBuffer      : TDAVArrayOfDoubleFixedArray;
    FInputMonitor         : Boolean;
    FConvertOptimizations : TConvertOptimizations;
    FOutputVolume         : TDAVSingleDynArray;
    FClipPrevent          : TClipBuffer;
    FConvertMethod        : TConvertMethod;
    FOutputDither         : TAsioOutputDither;
    {$IFDEF AsioMixer}
    FAsioMixer            : TFmAsioMixer;
    {$ENDIF}
    procedure SetConvertOptimizations(const Value: TConvertOptimizations);
    procedure SetAsioGenerator(const Value: TAsioGenerator);
    procedure SetPreventClipping(Value: TPreventClipping);
    {$IFDEF AsioMixer}
    procedure SetupMixer;
    procedure VolumeChange(Sender: TObject);
    {$ENDIF}
    procedure SetOnBufferSwitch32(const Value: TBufferSwitchEvent32);
    procedure SetOnBufferSwitch64(const Value: TBufferSwitchEvent64);
    procedure SetOutputDither(const Value: TAsioOutputDither);
    procedure SetConvertMethod(const Value: TConvertMethod);
    procedure OnBufferSwitchChanged;
  protected
    function CreateBuffers: Boolean; override;
    procedure ConvertMethodChanged; virtual;
    procedure BufferSwitchTimeInfo(Index: Integer; const Params: TAsioTime); override;
    procedure DetermineBuffersize; override;
    procedure ConvertOptimizationsChanged; virtual;
    procedure AsioGeneratorChanged; virtual;
    procedure PreventClippingChanged; virtual;
    procedure CreateFloatBuffers; virtual;

    property ConvertMethod: TConvertMethod read FConvertMethod write SetConvertMethod default cmNone;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF AsioMixer}
    procedure Mixer;
    {$ENDIF}
    property ConvertOptimizations: TConvertOptimizations read FConvertOptimizations write SetConvertOptimizations default [coSSE, co3DNow];
    property CustomGenerator: TAsioGenerator read FAsioGenerator Write SetAsioGenerator;
    property InputMonitor: Boolean read FInputMonitor write FInputMonitor default False;
    property OnBufferSwitch32: TBufferSwitchEvent32 read FOnBufferSwitch32 write SetOnBufferSwitch32;
    property OnBufferSwitch64: TBufferSwitchEvent64 read FOnBufferSwitch64 write SetOnBufferSwitch64;
    property OnBufferSwitchNative: TBufferSwitchEventNative read FOnBufferSwitchNative write FOnBufferSwitchNative;
    property OutputDither: TAsioOutputDither read FOutputDither write SetOutputDither default odNone;
    property PreFillInBuffer: TBufferPreFill read FInBufferPreFill write FInBufferPreFill default bpfNone;
    property PreFillOutBuffer: TBufferPreFill read FOutBufferPreFill write FOutBufferPreFill default bpfNone;
    property PreventClipping: TPreventClipping read FPreventClipping write SetPreventClipping default pcNone;
  end;

  TAsioHost = class(TCustomAsioHost)
  published
    property Active;
    property AsioTime;
    property BufferGranularity;
    property BufferMaximum;
    property BufferMinimum;
    property BufferPreferredSize;
    property BufferSize;
    property CanDos;
    property ConvertOptimizations;
    property ConvertMethod;
    property CustomGenerator;
    property DriverIndex;
    property DriverList;
    property DriverName;
    property DriverVersion;
    property InputChannelCount;
    property InputLatency;
    property InputMonitor;
    property OutputChannelCount;
    property OutputDither;
    property OutputLatency;
    property PreFillInBuffer;
    property PreFillOutBuffer;
    property PreventClipping;
    property SampleRate;
    property Supports;
    property OnBuffersCreate;
    property OnBuffersDispose;
    property OnBufferSwitch32;
    property OnBufferSwitch64;
    property OnBufferSwitchNative;
    property OnCreate;
    property OnDestroy;
    property OnDriverChanged;
    property OnLatencyChanged;
    property OnReset;
    property OnSampleRateChanged;
    property OnUpdateSamplePos;
  end;
  {$IFDEF SUPPORTS_REGION} {$endregion 'TAsioHost'} {$ENDIF}

var
  GAsioHost           : TCustomAsioHostBasic;
  PMUpdSamplePos      : {$IFDEF FPC} TLMessage; {$ELSE} TMessage; {$ENDIF}
  PMBufSwitch         : {$IFDEF FPC} TLMessage; {$ELSE} TMessage; {$ENDIF}
  PMBufSwitchTimeInfo : {$IFDEF FPC} TLMessage; {$ELSE} TMessage; {$ENDIF}
  PMReset             : {$IFDEF FPC} TLMessage; {$ELSE} TMessage; {$ENDIF}

function ChannelTypeToString(vType: TAsioSampleType): string;

implementation

uses
  Registry, ComObj, Math, DAV_Common {$IFDEF AsioMixer}, DAV_AsioChannelStrip {$ENDIF};

resourcestring
  RStrAsioDriverFailed        = 'Asio driver failed!';
  RStrAsioNoBuffersCreated    = 'Asio buffers could not be created!';
  RStrConverterTypeUnknown    = 'Converter type unknown';
  RCStrIndexOutOfBounds       = 'Index out of bounds %d';
  RCStrOnlyOneAsioHost        = 'Only one Asio host is allowed per instance';
  RCStrPreferedBufferSize     = 'Prefered buffer size invalid!';
  RCStrDriverNotPresent       = 'Driver not present';
  RCStrHardwareMalfunction    = 'Hardware malfunctioning';
  RCStrInputParameterInvalid  = 'Input parameter invalid';
  RCStrInvalidMode            = 'Hardware is in a bad mode or used in a bad mode';
  RCStrSPNotAdvancing         = 'Hardware is not running when sample position is inquired';
  RCStrNoClock                = 'Sample clock or rate cannot be determined or is not present';
  RCStrNoMemory               = 'Not enough memory for completing the request';

const
  CInprocServer = 'InprocServer32';
  CAsioPath     = 'software\asio';
  CComClsId     = 'clsid';

{$IFDEF SUPPORTS_REGION} {$region 'Delphi5 Control panel implementation'} {$ENDIF}
{$IFDEF DELPHI5}
{$IFDEF D5CP}
procedure TAsioControlPanel.Edit;
begin
 ExecuteVerb(0);
end;

function TAsioControlPanel.GetVerb(Index: Integer): string;
begin
 case Index of
 0: Result := 'Control Panel';
 end;
end;

function TAsioControlPanel.GetVerbCount: Integer;
begin
 Result := Integer((Component as TCustomAsioHost).DriverIndex >= 0);
end;

procedure TAsioControlPanel.ExecuteVerb(Index: Integer);
begin
 case Index of
 0: if (Component as TCustomAsioHost).DriverIndex >= 0
  then (Component as TCustomAsioHost).ControlPanel;
 end;
end;
{$ENDIF}
{$ENDIF}
{$IFDEF SUPPORTS_REGION} {$endregion 'Delphi5 Control panel implementation'} {$ENDIF}

{$IFDEF SUPPORTS_REGION} {$region 'TAsioTimeSub implementation'} {$ENDIF}
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

function TAsioTimeSub.GetATFlags: TATFlags;
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

procedure TAsioTimeSub.SetATFlags(Flags: TATFlags);
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

function TAsioTimeSub.GetATdouble(Index :Integer): Double;
begin
 Result := 0;
 case Index of
  0: Result := FBufferTime.TimeInfo.speed;
  1: Result := FBufferTime.TimeInfo.sampleRate;
 end;
end;

procedure TAsioTimeSub.SetATdouble(Index :Integer; Value: Double);
begin
 case Index of
  0: if Value <> FBufferTime.TimeInfo.speed then
  begin
   FBufferTime.TimeInfo.speed := Value;
   Change;
  end;
  1: if Value <> FBufferTime.TimeInfo.sampleRate then
  begin
   FBufferTime.TimeInfo.sampleRate := Value;
   Change;
  end;
 end;
end;

function TAsioTimeSub.GetATInt64(Index :Integer): Int64;
begin
 Result := 0;
 case Index of
  0: Result := AsioSamplesToInt64(FBufferTime.TimeInfo.samplePosition);
 end;
end;

procedure TAsioTimeSub.SetATInt64(Index :Integer; Value: Int64);
begin
 case Index of
  0: if Value <> AsioSamplesToInt64(FBufferTime.TimeInfo.samplePosition) then
       begin
        FBufferTime.TimeInfo.SamplePosition := Int64ToAsioSamples(Value);
        Change;
       end;
 end;
end;
{$IFDEF SUPPORTS_REGION} {$endregion 'TAsioTimeSub implementation'} {$ENDIF}


{$IFDEF SUPPORTS_REGION} {$region 'Global functions'} {$ENDIF}
function ChannelTypeToString(vType: TAsioSampleType): string;
begin
 Result := '';
 case vType of
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
{$IFDEF SUPPORTS_REGION} {$endregion 'Global functions'} {$ENDIF}

{$IFDEF SUPPORTS_REGION} {$region 'Asio callback functions'} {$ENDIF}
procedure AsioBufferSwitch(DoubleBufferIndex: Integer; DirectProcess: TAsioBool); cdecl;
begin
 if Assigned(GAsioHost) then
  case DirectProcess of
   CAsioFalse: PostMessage(GAsioHost.FHandle, PMBufSwitch.Msg, AM_BufferSwitch, DoubleBufferIndex);
   CAsioTrue : GAsioHost.BufferSwitch(DoubleBufferIndex);
  end;
end;

function AsioBufferSwitchTimeInfo(var Params: TAsioTime;
  DoubleBufferIndex: Integer; DirectProcess: TAsioBool): PAsioTime; cdecl;
begin
 if Assigned(GAsioHost) then
  case DirectProcess of
   CAsioFalse :
    begin
     GAsioHost.AsioTime.FBufferTime := Params;
     PostMessage(GAsioHost.FHandle, PMBufSwitchTimeInfo.Msg, AM_BufferSwitchTimeInfo, DoubleBufferIndex);
    end;
   CAsioTrue : GAsioHost.BufferSwitchTimeInfo(DoubleBufferIndex, params);
  end;
 Result := nil;
end;

procedure AsioSampleRateDidChange(SampleRate: TAsioSampleRate); cdecl;
begin
 if Assigned(GAsioHost) then
  begin
   GAsioHost.SampleRate := SampleRate;
   if Assigned(GAsioHost.FOnSampleRateChanged)
    then GAsioHost.FOnSampleRateChanged(GAsioHost);
  end;
end;

function AsioMessageHandler(Selector, Value: Integer; message: Pointer; Opt: PDouble): Integer; cdecl;
begin
 Result := 0;
 case Selector of
  kAsioSelectorSupported : Result := CAsioTrue; // return 1 if a selector is supported
  kAsioEngineVersion :
   if Assigned(GAsioHost)
    then Result := GAsioHost.FEngineVersion
    else Result := 2; // return 2 if Asio 2 is supported 
  kAsioResetRequest :
   if Assigned(GAsioHost) then
    begin
     PostMessage(GAsioHost.FHandle, PM_Asio, AM_ResetRequest, 0);
     Result := 1;
    end;
  kAsioBufferSizeChange :
   if Assigned(GAsioHost) then
    begin
     PostMessage(GAsioHost.FHandle, PM_Asio, AM_ResetRequest, 0);
     Result := 1;
    end;
  kAsioResyncRequest :
   if Assigned(GAsioHost) then
    begin
     PostMessage(GAsioHost.FHandle, PM_Asio, AM_LatencyChanged, 0);
     Result := 1;
    end;
  kAsioLatenciesChanged :
   if Assigned(GAsioHost) then
    begin
     PostMessage(GAsioHost.FHandle, PM_Asio, AM_LatencyChanged, 0);
     Result := 1;
    end;
  kAsioSupportsTimeInfo :
   if Assigned(GAsioHost)
     then Result := Integer(assSupportsTimeInfo in GAsioHost.Supports)
     else Result := 1;
  kAsioSupportsTimeCode :
   if Assigned(GAsioHost)
     then Result := Integer(assSupportsTimeInfo in GAsioHost.Supports)
     else Result := 1;
  kAsioSupportsInputMonitor :
   if Assigned(GAsioHost)
     then Result := Integer(assSupportsInputMonitor in GAsioHost.Supports)
     else Result := 1;
 end;
end;
{$IFDEF SUPPORTS_REGION} {$endregion 'Asio callback functions'} {$ENDIF}

////////////////////////////////////////////////////////////////////////////////
///////////////////////////// TCustomAsioHostBasic /////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{$IFDEF SUPPORTS_REGION} {$region 'TCustomAsioHostBasic implementation'} {$ENDIF}

{ TCustomAsioHostBasic }

constructor TCustomAsioHostBasic.Create(AOwner: TComponent);
begin
  FHandle := AllocateHWnd(WndProc);

  {$IFNDEF AllowMultipleAsioHosts}
  if GAsioHost <> nil
   then raise Exception.Create(RCStrOnlyOneAsioHost) else
  {$ENDIF}
  GAsioHost        := Self;
  FUnAlignedBuffer := nil;
  FInputBuffers    := nil;
  FOutputBuffers   := nil;
  FSampleRate      := 44100;
  FAsioTime        := TAsioTimeSub.Create;

  FEngineVersion   := 2;
  FAsioSupports    := [assSupportsTimeInfo, assSupportsTimeCode];

  // set the callbacks record fields
  with FCallbacks do
   begin
    BufferSwitch := AsioBufferSwitch;
    SampleRateDidChange := AsioSampleRateDidChange;
    BufferSwitchTimeInfo := AsioBufferSwitchTimeInfo;
    AsioMessage := AsioMessageHandler;
   end;

  // set the driver itself to nil for now
  FDriver := nil;
  FBuffersCreated := False;

  // and make sure all controls are enabled or disabled
  FDriverIndex := -1;

  FAsioDriverList := TDAVAsioDriverList.Create;
  try
   FAsioDriverList.UpdateList;
  except
  end;

  inherited;
end;

destructor TCustomAsioHostBasic.Destroy;
begin
 try
  if GAsioHost = Self
   then GAsioHost := nil;
  if Assigned(FOnDestroy) then FOnDestroy(Self);
  if Active then Active := False;
  CloseDriver;
  DeallocateHWnd(FHandle);
  FAsioDriverList.Free;
  SetLength(FInConverters, 0);
  SetLength(FOutConverters, 0);
  FreeAndNil(FAsioTime);
 finally
  inherited;
  GAsioHost := nil;
 end;
end;


{$IFNDEF FPC}

procedure TCustomAsioHostBasic.DefaultHandler(var Message);
begin
  with TMessage(Message) do Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;

procedure TCustomAsioHostBasic.WndProc(var Msg: TMessage);
begin
  with Msg do Dispatch(Msg);//Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;

{$ELSE}
function DefWindowProc(hWnd:THandle; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LResult; external 'user32' name 'DefWindowProcA';

procedure TCustomAsioHostBasic.DefaultHandler(var Message);
begin
  with TLMessage(Message) do Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;

procedure TCustomAsioHostBasic.WndProc(var Msg: TLMessage);
begin
 with Msg do Dispatch(Msg);//Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////

procedure TCustomAsioHostBasic.GetCurrentClockSource;
var
  ClockSources     : array [0..3] of TAsioClockSource;
  ClockSourceCount : Integer;
//  ClockSourceIndex : Integer;
begin
 FillChar(ClockSources, SizeOf(ClockSources), 0); 
 if Assigned(FDriver)
  then FDriver.GetClockSources(@ClockSources[0], ClockSourceCount);

(*
 for ClockSourceIndex := 0 to ClockSourceCount - 1 do
  if ClockSources[ClockSourceIndex].IsCurrentSource <> 0
   then Exit;
*)
end;

procedure TCustomAsioHostBasic.ResetDriverSpecificData;
begin
 FDriverName := '';
 FInputLatency := 0;
 FOutputLatency := 0;
 FInputChannelCount := 0;
 FOutputChannelCount := 0;
 FBufferSize := 0;
end;

function TCustomAsioHostBasic.GetDriverList: TStrings;
begin
  result := FAsioDriverList.DriverNames;
end;

procedure TCustomAsioHostBasic.SetIgnoredDriver(ignore: TGuid);
begin
  FAsioDriverList.SetIgnoredDriver(ignore);
  FAsioDriverList.UpdateList;
end;

procedure TCustomAsioHostBasic.SetDriverName(const s: string);
begin
  DriverIndex := FAsioDriverList.DriverNumberByName(s);
end;

procedure TCustomAsioHostBasic.SetDriverIndex(Value: Integer);
var
  DrName    : array[0..255] of AnsiChar;
  tmpActive : Boolean;
begin
 if (Value <> FDriverIndex) then
  begin
   tmpActive := Active;
   Active := False;
   if Value < -1 then FDriverIndex := -1 else
    if Value >= FAsioDriverList.Count
     then FDriverIndex := -1
     else FDriverIndex := Value;

   // check if no driver has been selected and reset all driver specific data  
   if FDriverIndex = -1 then
    begin
     ResetDriverSpecificData;
     CloseDriver;
    end
   else
    begin
     try
      CloseDriver;
      FDriverName := FAsioDriverList.Items[FDriverIndex].Name;
      OpenDriver;
     except
      FDriverIndex := -1;
      ResetDriverSpecificData;
      raise;
     end;

     if Assigned(FDriver) then
      begin
       FDriver.GetDriverName(DrName);
       if DrName <> ''
        then FDriverName := string(DrName);
       FDriverVersion := FDriver.GetDriverVersion;
       UpdateCanDos;
       GetCurrentClockSource;
      end;
    end;
   if Assigned(FOnDriverChanged)
    then FOnDriverChanged(self);
   Active := tmpActive;
  end;
end;

procedure TCustomAsioHostBasic.DetermineBuffersize;
begin
 FDriver.GetBufferSize(FMin, FMax, FPref, FGran);
 if FMin = FMax then FPref := FMin;

 // check prefered buffersize is valid
 if FPref <= 0
  then raise Exception.Create(RCStrPreferedBufferSize);

 FBufferSize := FPref;
end;

procedure TCustomAsioHostBasic.AquireCurrentSampleRate;
begin
 FDriver.GetSampleRate(FSampleRate);
 AsioTime.SampleRate := FSampleRate;
end;

procedure TCustomAsioHostBasic.UpdateCanDos;
begin
 // check whether driver is has been assigned
 if FDriver = nil then
  begin
   FAsioCanDos := [];
   Exit;
  end;

 // test "Time Info"
 if FDriver.Future(kAsioCanTimeInfo, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdTimeInfo]
  else FAsioCanDos := FAsioCanDos - [acdTimeInfo];

 // test "Time Code"
 if FDriver.Future(kAsioCanTimeCode, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdTimeCode]
  else FAsioCanDos := FAsioCanDos - [acdTimeCode];

 // test "Transport"
 if FDriver.Future(kAsioCanTransport, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdTransport]
  else FAsioCanDos := FAsioCanDos - [acdTransport];

 // test "Input Gain"
 if FDriver.Future(kAsioCanInputGain, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdInputGain]
  else FAsioCanDos := FAsioCanDos - [acdInputGain];

 // test "Input Meter"
 if FDriver.Future(kAsioCanInputMeter, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdInputMeter]
  else FAsioCanDos := FAsioCanDos - [acdInputMeter];

 // test "Output Gain"
 if FDriver.Future(kAsioCanOutputGain, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdOutputGain]
  else FAsioCanDos := FAsioCanDos - [acdOutputGain];

 // test "Output Meter"
 if FDriver.Future(kAsioCanOutputMeter, nil) = ASE_SUCCESS
  then FAsioCanDos := FAsioCanDos + [acdOutputMeter]
  else FAsioCanDos := FAsioCanDos - [acdOutputMeter];
end;

function TCustomAsioHostBasic.GetInConverter(ConverterType: TAsioSampleType): TInConverter;
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
  else raise Exception.Create(RStrConverterTypeUnknown);
 end;
end;

function TCustomAsioHostBasic.GetOutConverter(ConverterType: TAsioSampleType): TOutConverter;
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
  else raise Exception.Create(RStrConverterTypeUnknown);
 end;
end;

function TCustomAsioHostBasic.CreateBuffers: Boolean;
var
  Channel : Integer;
  Buffer  : PAsioBufferInfos;
begin
 // make sure a driver has been selected
 if FDriver = nil then
  begin
   Result := False;
   Exit;
  end;

 // eventually dispose buffers
 if FBuffersCreated then DisposeBuffers;

 // get default values of the current driver
 DetermineBuffersize;
 AquireCurrentSampleRate;
 FDriver.GetChannels(FInputChannelCount, FOutputChannelCount);

 // allocate memory for input and output buffers
 GetMem(FUnAlignedBuffer, SizeOf(TAsioBufferInfo) * (FInputChannelCount + FOutputChannelCount) + 16);
 Buffer := PAsioBufferInfos(Integer(FUnAlignedBuffer) + 16 - (Integer(FUnAlignedBuffer) mod 16));

 // setup input channel info and converter
 FInputBuffers := Buffer;
 SetLength(FInputChannelInfos, FInputChannelCount);
 SetLength(FInConverters, FInputChannelCount);
 for Channel := 0 to FInputChannelCount - 1 do
  begin
   FInputChannelInfos[Channel].Channel := Channel;
   FInputChannelInfos[Channel].IsInput := CAsioTrue;
   FDriver.GetChannelInfo(FInputChannelInfos[Channel]);
   FInConverters[Channel] := GetInConverter(FInputChannelInfos[Channel].SampleType);

   Buffer^[0].IsInput := CAsioTrue;
   Buffer^[0].ChannelNum := Channel;
   Buffer^[0].Buffers[0] := nil;
   Buffer^[0].Buffers[1] := nil;
   Inc(Buffer);
  end;

 // setup input channel info and converter
 FOutputBuffers := Buffer;
 SetLength(FOutputChannelInfos, FOutputChannelCount);
 SetLength(FOutConverters, FOutputChannelCount);
 for Channel := 0 to FOutputChannelCount - 1 do
  begin
   FOutputChannelInfos[Channel].Channel := Channel;
   FOutputChannelInfos[Channel].IsInput := CAsioFalse;   //  output
   FDriver.GetChannelInfo(FOutputChannelInfos[Channel]);
   FOutConverters[Channel] := GetOutConverter(FOutputChannelInfos[Channel].SampleType);

   Buffer^[0].IsInput := CAsioFalse;
   Buffer^[0].ChannelNum := Channel;
   Buffer^[0].Buffers[0] := nil;
   Buffer^[0].Buffers[1] := nil;
   Inc(Buffer);
  end;

 Assert(FBufferSize > 0);

 Result := FDriver.CreateBuffers(FInputBuffers,
   (FInputChannelCount + FOutputChannelCount), FBufferSize, FCallbacks) = ASE_OK;
 if not Result then Exit;
  
 if Assigned (FOnBuffersCreate) then FOnBuffersCreate(Self);

 // get current latencies
 FDriver.GetLatencies(FInputLatency, FOutputLatency);
 if Assigned (FOnLatencyChanged) then FOnLatencyChanged(Self);

 // initialize random generator for online noise processing
 Randomize;
end;

procedure TCustomAsioHostBasic.DisposeBuffers;
begin
 if (FDriver = nil) then Exit;
 if FBuffersCreated then
  begin
   if Assigned (FOnBuffersDispose)
    then FOnBuffersDispose(Self);
   FInputBuffers := nil;
   FOutputBuffers := nil;
   Dispose(FUnAlignedBuffer);
   FUnAlignedBuffer := nil;
   try
    FDriver.DisposeBuffers;
   except
   end;
   FBuffersCreated := False;
   SetLength(FInputChannelInfos, 0);
   SetLength(FOutputChannelInfos, 0);
  end;
end;

procedure TCustomAsioHostBasic.OpenDriver;
var
  OldActive    : Boolean;
  ErrorMessage : PAnsiChar;
begin
 // store last active state and deactivate current driver
 OldActive := False;
 if Assigned(FDriver) then
  try
   Active := False;
   CloseDriver;
  except
  end;

 // if a driver index has been assigned open/create Asio interface
 if FDriverIndex >= 0 then
  try
   {$IFDEF OpenAsio}
   if OpenAsioCreate(FAsioDriverList.Items[FDriverIndex].Guid, FDriver) then
   {$ELSE}
   if CreateStdCallAsio(FAsioDriverList.Items[FDriverIndex].Guid, FDriver) then
    {$ENDIF}
    try
     if Assigned(FDriver) then
      case FDriver.Init(FHandle) of
       0 : begin
            // equals false
            GetMem(ErrorMessage, 128);
            try
             FDriver.GetErrorMessage(ErrorMessage);
             raise Exception.Create(string(ErrorMessage));
            finally
             Dispose(ErrorMessage);
            end;
           end;
       // the below codes are here due to incompatibility of some soundcards
       ASE_NotPresent       : raise Exception.Create(RCStrDriverNotPresent);
       ASE_HWMalfunction    : raise Exception.Create(RCStrHardwareMalfunction);
       ASE_InvalidParameter : raise Exception.Create(RCStrInputParameterInvalid);
       ASE_InvalidMode      : raise Exception.Create(RCStrInvalidMode);
       ASE_SPNotAdvancing   : raise Exception.Create(RCStrSPNotAdvancing);
       ASE_NoClock          : ;// ??raise Exception.Create(RCStrNoClock);
       ASE_NoMemory         : raise Exception.Create(RCStrNoMemory);
      end;
    except
     FDriver := nil;
    end;
  except
   FDriver := nil;
  end;

 // check driver is assigned
 if FDriver = nil
  then raise Exception.Create(RStrAsioDriverFailed);

 // create and check buffers
 FBuffersCreated := CreateBuffers;
 if not FBuffersCreated
  then raise Exception.Create(RStrAsioNoBuffersCreated);

 // eventually reactivate
 Active := OldActive and FBuffersCreated;
end;

procedure TCustomAsioHostBasic.CloseDriver;
begin
 // release driver
 if Assigned(FDriver) then
 begin
  try
   if FBuffersCreated then DisposeBuffers;
  except
  end;
  FDriver := nil;
 end;

 // reset some default values in case another driver is querried
 FInputLatency := 0;
 FOutputLatency := 0;
 FInputChannelCount := 0;
 FOutputChannelCount := 0;
end;

procedure TCustomAsioHostBasic.ControlPanel;
begin
 if Assigned(FDriver)
  then FDriver.ControlPanel;
end;

{$IFDEF AsioMixer}
procedure TCustomAsioHostBasic.Mixer;
begin
 FAsioMixer.Show;
end;
{$ENDIF}

procedure TCustomAsioHostBasic.ReadState(Reader: TReader);
begin
 inherited;
 if Assigned(FOnCreate) then FOnCreate(Self);
end;

procedure TCustomAsioHostBasic.Reset;
begin
 OpenDriver; // restart the driver
 if Assigned (FOnReset) then FOnReset(Self);
end;

{$IFDEF FPC}
procedure TCustomAsioHostBasic.PMAsio(var Message: TLMessage);
{$ELSE}
procedure TCustomAsioHostBasic.PMAsio(var Message: TMessage);
{$ENDIF}
begin
 if FDriver = nil then exit;
 case Message.WParam of
  AM_ResetRequest:         Reset;
  AM_BufferSwitch:         BufferSwitch(Message.LParam); // process a buffer
  AM_BufferSwitchTimeInfo: BufferSwitchTimeInfo(Message.LParam, AsioTime.FBufferTime);  // process a buffer with time
  AM_LatencyChanged:
   begin
    if Assigned(FDriver)
     then FDriver.GetLatencies(FInputLatency, FOutputLatency);
    if Assigned(FOnLatencyChanged) then FOnLatencyChanged(Self);
   end;
 end;
end;

{$IFDEF FPC}
procedure TCustomAsioHostBasic.PMReset(var Message: TLMessage);
{$ELSE}
procedure TCustomAsioHostBasic.PMReset(var Message: TMessage);
{$ENDIF}
begin
  Reset;
end;

{$IFDEF FPC}
procedure TCustomAsioHostBasic.PMUpdateSamplePos(var Message: TLMessage);
{$ELSE}
procedure TCustomAsioHostBasic.PMUpdateSamplePos(var Message: TMessage);
{$ENDIF}
var
  Samples: TAsioSamples;
begin
 Samples.hi := Message.wParam;
 Samples.lo := Message.LParam;
 if Assigned(FOnUpdateSamplePos)
  then FOnUpdateSamplePos(Self, AsioSamplesToInt64(Samples));
end;

procedure TCustomAsioHostBasic.BufferSwitch(Index: Integer);
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

procedure TCustomAsioHostBasic.BufferSwitchTimeInfo(Index: Integer;
  const Params: TAsioTime);
begin
 if FDriver = nil then Exit;
 PMUpdSamplePos.wParam := params.TimeInfo.samplePosition.hi;
 PMUpdSamplePos.LParam := params.TimeInfo.samplePosition.lo;
 Dispatch(PMUpdSamplePos);

 if Assigned(FOnBufferSwitch)
  then FOnBufferSwitch(Self, @(FInputBuffers^), Index);
 FDriver.OutputReady;
end;

procedure TCustomAsioHostBasic.SetSampleRate(Value: Double);
begin
 // check for a valid samplerate
 Value := abs(Value);
 if (Value = 0) or (Value > 1048575)
  then Value := 44100;

 // check if samplerate is supported 
 if Assigned(FDriver) then
  if FDriver.CanSampleRate(Value) <> ASE_OK
   then Exit;

 if FSampleRate <> Value then
  begin
   if Assigned(FDriver) then
    if FDriver.SetSampleRate(Value) = ASE_OK then
     begin
      FSampleRate := Value;
      AsioTime.SampleRate := FSampleRate;
     end
   else
    begin
     FSampleRate := Value;
     AsioTime.SampleRate := FSampleRate;
    end;
  end;
end;

procedure TCustomAsioHostBasic.SetActive(Value: Boolean);
begin
 // make sure a driver is assigned and something changed
 if (FDriver = nil) then Value := False;
 if FActive = Value then exit;

 if Value = True then
  begin
   FActive := (FDriver.Start = ASE_OK);
   if FActive = False then FDriver.Stop;
  end
 else
  begin
   FActive := False;
   if Assigned(FDriver)
    then FDriver.Stop;
   if FBuffersCreated
    then ClearBuffers;
  end;
end;

procedure TCustomAsioHostBasic.ClearBuffers;
var
  Buffer     : PAsioBufferInfos;
  Channel    : Integer;
  SampleSize : Word;
begin
 try
  // clear output buffer
  Buffer := FOutputBuffers;
  if Assigned(Buffer) then
   for Channel := 0 to FOutputChannelCount - 1 do
    with FOutputChannelInfos[Channel] do
     begin
      // determine sample size
      if SampleType in [CAsioSTInt16MSB, CAsioSTInt16LSB]     then SampleSize := SizeOf(Word) else
      if SampleType in [CAsioSTInt24MSB, CAsioSTInt24LSB]     then SampleSize := 3 else
      if SampleType in [CAsioSTFloat32LSB, CAsioSTFloat32MSB] then SampleSize := SizeOf(Single) else
      if SampleType in [CAsioSTFloat64LSB, CAsioSTFloat64MSB] then SampleSize := SizeOf(Double)
       else SampleSize := SizeOf(Integer);

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
   for Channel := 0 to FInputChannelCount - 1 do
    with FInputChannelInfos[Channel] do
     begin
      // determine sample size
      if SampleType in [CAsioSTInt16MSB, CAsioSTInt16LSB]     then SampleSize := SizeOf(Word) else
      if SampleType in [CAsioSTInt24MSB, CAsioSTInt24LSB]     then SampleSize := 3 else
      if SampleType in [CAsioSTFloat32LSB, CAsioSTFloat32MSB] then SampleSize := SizeOf(Single) else
      if SampleType in [CAsioSTFloat64LSB, CAsioSTFloat64MSB] then SampleSize := SizeOf(Double)
       else SampleSize := SizeOf(Integer);

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

function TCustomAsioHostBasic.GetNumDrivers: Integer;
begin
 Result := FAsioDriverList.Count;
end;

function TCustomAsioHostBasic.CanSampleRate(SampleRate: TAsioSampleRate): TAsioError;
begin
 if Assigned(FDriver)
  then Result := FDriver.CanSampleRate(SampleRate)
  else Result := ASE_NotPresent;
end;

{$IFDEF FPC}
procedure TCustomAsioHostBasic.PMBufferSwitch(var Message: TLMessage);
{$ELSE}
procedure TCustomAsioHostBasic.PMBufferSwitch(var Message: TMessage);
{$ENDIF}
begin
 BufferSwitch(Message.LParam);
end;

{$IFDEF FPC}
procedure TCustomAsioHostBasic.PMBufferSwitchTimeInfo(var Message: TLMessage);
{$ELSE}
procedure TCustomAsioHostBasic.PMBufferSwitchTimeInfo(var Message: TMessage);
{$ENDIF}
begin
 BufferSwitchTimeInfo(Message.LParam, AsioTime.FBufferTime);
end;

function TCustomAsioHostBasic.GetInputChannelInfo(Index: Integer): TAsioChannelInfo;
begin
 if (Index < 0) or (Index >= FInputChannelCount)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 Result := FInputChannelInfos[Index];
end;

function TCustomAsioHostBasic.GetOutputChannelInfo(Index: Integer): TAsioChannelInfo;
begin
 if (Index < 0) or (Index >= FOutputChannelCount)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 Result := FOutputChannelInfos[Index];
end;

function TCustomAsioHostBasic.GetInputMeter(Channel: Integer): Integer;
var
  ACC : TAsioChannelControls;
begin
 // check if command can be transmitted
 if (FDriver = nil) and not (acdInputMeter in FAsioCanDos) then
  begin
   Result := -1;
   Exit;
  end;

 ACC.isInput := 1; ACC.Channel := Channel;
 FDriver.Future(kAsioGetInputMeter,@ACC);
 Result := ACC.meter;
end;

function TCustomAsioHostBasic.GetOutputMeter(Channel: Integer): Integer;
var
  ACC : TAsioChannelControls;
begin
 // check if command can be transmitted
 if (FDriver = nil) and not (acdOutputMeter in FAsioCanDos) then
  begin
   Result := -1;
   Exit;
  end;

 if FDriver = nil then
 ACC.isInput := 0; ACC.Channel := Channel;
 FDriver.Future(kAsioGetOutputMeter, @ACC);
 Result := ACC.meter;
end;

procedure TCustomAsioHostBasic.SetInputGain(Channel:Integer; Gain: Integer);
var
  ACC : TAsioChannelControls;
begin
 // check if command can be transmitted
 if (FDriver = nil) and not (acdInputGain in FAsioCanDos)
  then Exit;

 ACC.IsInput := 1;
 ACC.Channel := Channel;
 ACC.Gain := Gain;
 FDriver.Future(kAsioSetInputGain, @ACC);
end;

procedure TCustomAsioHostBasic.SetOutputGain(Channel:Integer; Gain: Integer);
var
  ACC : TAsioChannelControls;
begin
 // check if command can be transmitted
 if (FDriver = nil) and not (acdOutputGain in FAsioCanDos)
  then Exit;

 ACC.isInput := 0; ACC.Channel := Channel; ACC.Gain := Gain;
 FDriver.Future(kAsioSetOutputGain, @ACC);
end;

{$IFDEF SUPPORTS_REGION} {$endregion} {$ENDIF}

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// TCustomAsioHost ////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{$IFDEF SUPPORTS_REGION} {$region 'TCustomAsioHost implementation'} {$ENDIF}

constructor TCustomAsioHost.Create(AOwner: TComponent);
begin
  FClipPrevent          := ClipDigital;
  FConvertOptimizations := [coSSE, co3DNow];
  FOutputDither         := odNone;
  FInputMonitor         := False;
  FConvertMethod        := cmNone;

  {$IFDEF AsioMixer} FAsioMixer := TFmAsioMixer.Create(nil); {$ENDIF}
  inherited;
end;

destructor TCustomAsioHost.Destroy;
var
  Channel : Integer;
begin
 SetLength(FOutputVolume, 0);

 // dispose single input buffers
 for Channel := 0 to Length(FSingleInBuffer) - 1
  do Dispose(FSingleInBuffer[Channel]);

 // dispose single output buffers
 for Channel := 0 to Length(FSingleOutBuffer) - 1
  do Dispose(FSingleOutBuffer[Channel]);

 // dispose double input buffers
 for Channel := 0 to Length(FDoubleInBuffer) - 1
  do Dispose(FDoubleInBuffer[Channel]);

 // dispose double output buffers
 for Channel := 0 to Length(FDoubleOutBuffer) - 1
  do Dispose(FDoubleOutBuffer[Channel]);

 {$IFDEF AsioMixer} FAsioMixer.Free; {$ENDIF}
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TCustomAsioHost.SetOnBufferSwitch32(const Value: TBufferSwitchEvent32);
begin
 FOnBufferSwitch32 := Value;
 OnBufferSwitchChanged;
end;

procedure TCustomAsioHost.SetOnBufferSwitch64(const Value: TBufferSwitchEvent64);
begin
 FOnBufferSwitch64 := Value;
 OnBufferSwitchChanged;
end;

procedure TCustomAsioHost.OnBufferSwitchChanged;
begin
 if Assigned(FOnBufferSwitch64) then ConvertMethod := cm64 else
 if Assigned(FOnBufferSwitch32) then ConvertMethod := cm32
  else ConvertMethod := cmNone;
end;

procedure TCustomAsioHost.SetConvertMethod(const Value: TConvertMethod);
begin
 if ConvertMethod <> Value then
  begin
   FConvertMethod := Value;
   ConvertMethodChanged;
  end;
end;

procedure TCustomAsioHost.ConvertMethodChanged;
begin
 CreateFloatBuffers;
end;

procedure TCustomAsioHost.SetConvertOptimizations(const Value: TConvertOptimizations);
begin
 if FConvertOptimizations <> Value then
  begin
   FConvertOptimizations := Value;
   ConvertOptimizationsChanged;
  end;
end;

procedure TCustomAsioHost.ConvertOptimizationsChanged;
begin
 Use_FPU;
 case ProcessorType of
  ptSSE: if coSSE in FConvertOptimizations then Use_SSE;
  pt3DNow: if co3DNow in FConvertOptimizations then Use_3DNow;
 end;
end;

procedure TCustomAsioHost.SetAsioGenerator(const Value: TAsioGenerator);
begin
 if Value <> FAsioGenerator then
  begin
   FAsioGenerator := Value;
   AsioGeneratorChanged;
  end;
end;

procedure TCustomAsioHost.AsioGeneratorChanged;
begin
 if Assigned(FAsioGenerator) then
  begin
   FAsioGenerator.BlockSize := FBufferSize;
   FAsioGenerator.SampleRate := FSampleRate;
  end;
end;

procedure TCustomAsioHost.SetPreventClipping(Value : TPreventClipping);
begin
 if FPreventClipping <> Value then
  begin
   FPreventClipping := Value;
   PreventClippingChanged;
  end;
end;

procedure TCustomAsioHost.PreventClippingChanged;
begin
 case FPreventClipping of
  pcDigital: FClipPrevent := ClipDigital;
  pcAnalog: FClipPrevent := ClipAnalog;
 end;
end;

procedure TCustomAsioHost.DetermineBuffersize;
begin
 inherited;
 if Assigned(FAsioGenerator)
  then FAsioGenerator.BlockSize := FBufferSize;
end;

{$IFDEF AsioMixer}
procedure TCustomAsioHost.VolumeChange(Sender: TObject);
begin
 Assert(Sender is TFrChannelStrip);
 with TFrChannelStrip(Sender) do
  begin
   FOutputVolume[Channel] := Volume;
   if Mute then FOutputVolume[Channel] := 0;
  end;
end;

procedure TCustomAsioHost.SetupMixer;
var
  Channel: Integer;
begin
 with FAsioMixer do
  begin
   for Channel := 0 to Length(ChannelsStrips) - 1
    do FreeAndNil(ChannelsStrips[Channel]);
   SetLength(ChannelsStrips, FOutputChannels);
   for Channel := FOutputChannels - 1 downto 0 do
    begin
     ChannelsStrips[Channel] := TFrChannelStrip.Create(FAsioMixer);
     with ChannelsStrips[Channel] do
      begin
       Width := 44;
       Name := 'ChannelStrip' + IntToStr(Channel);
       Parent := FAsioMixer.MixerPanel;
       Align := alLeft;
       OnVolumeChange := VolumeChange;
       OnMuteChange := VolumeChange;
       Channel := FOutputChannels - 1 - Channel;
      end;
    end;
   if FOutputChannels > 0 then
    begin
     ClientHeight := 20 + ChannelsStrips[0].Height;
     ClientWidth := 20 + FOutputChannels * ChannelsStrips[0].Width;
    end;
  end;  
end;
{$ENDIF AsioMixer}

function TCustomAsioHost.CreateBuffers: Boolean;
var
  Channel : Integer;
begin
 Result := inherited CreateBuffers;

 if Result then
  begin
   SetLength(FOutputVolume, FOutputChannelCount);
   for Channel := 0 to FOutputChannelCount - 1
    do FOutputVolume[Channel] := 1;
   {$IFDEF AsioMixer} SetupMixer; {$ENDIF}

   CreateFloatBuffers;
  end;
end;

procedure TCustomAsioHost.CreateFloatBuffers;
var
  Channel : Integer;
begin
 if FBufferSize > 0 then
  case FConvertMethod of
   cm32 :
    begin
     // input buffers
     if Length(FSingleInBuffer) > FInputChannelCount then
      for Channel := FInputChannelCount to Length(FSingleInBuffer) - 1
       do Dispose(FSingleInBuffer[Channel]);

     SetLength(FSingleInBuffer, FInputChannelCount);
     for Channel := 0 to Length(FSingleInBuffer) - 1 do
      begin
       ReallocMem(FSingleInBuffer[Channel], FBufferSize * SizeOf(Single));
       FillChar(FSingleInBuffer[Channel, 0], FBufferSize * SizeOf(Single), 0);
      end;


     // output buffers
     if Length(FSingleOutBuffer) > FOutputChannelCount then
      for Channel := FOutputChannelCount to Length(FSingleOutBuffer) - 1
       do Dispose(FSingleOutBuffer[Channel]);
     SetLength(FSingleOutBuffer, FOutputChannelCount);
     for Channel := 0 to Length(FSingleOutBuffer) - 1 do
      begin
       ReallocMem(FSingleOutBuffer[Channel], FBufferSize * SizeOf(Single));
       FillChar(FSingleOutBuffer[Channel, 0], FBufferSize * SizeOf(Single), 0);
      end;

     // dispose unused input buffers
     for Channel := 0 to Length(FDoubleInBuffer) - 1
      do Dispose(FDoubleInBuffer[Channel]);
     SetLength(FDoubleInBuffer, 0);

     // dispose unused output buffers
     for Channel := 0 to Length(FDoubleOutBuffer) - 1
      do Dispose(FDoubleOutBuffer[Channel]);
     SetLength(FDoubleOutBuffer, 0);
    end;
   cm64 :
    begin
     // input buffers
     if Length(FDoubleInBuffer) > FInputChannelCount then
      for Channel := FInputChannelCount to Length(FDoubleInBuffer) - 1
       do Dispose(FDoubleInBuffer[Channel]);
     SetLength(FDoubleInBuffer, FInputChannelCount);
     for Channel := 0 to FInputChannelCount - 1 do
      begin
       ReallocMem(FDoubleInBuffer[Channel], FBufferSize * SizeOf(Double));
       FillChar(FDoubleInBuffer[Channel, 0], FBufferSize * SizeOf(Double), 0);
      end;

     // output buffers
     if Length(FDoubleOutBuffer) > FOutputChannelCount then
      for Channel := FOutputChannelCount to Length(FDoubleOutBuffer) - 1
       do Dispose(FDoubleOutBuffer[Channel]);
     SetLength(FDoubleOutBuffer, FOutputChannelCount);
     for Channel := 0 to FOutputChannelCount - 1 do
      begin
       ReallocMem(FDoubleOutBuffer[Channel], FBufferSize * SizeOf(Double));
       FillChar(FDoubleOutBuffer[Channel, 0], FBufferSize * SizeOf(Double), 0);
      end;

     // dispose unused input buffers
     for Channel := 0 to Length(FSingleInBuffer) - 1
      do Dispose(FSingleInBuffer[Channel]);
     SetLength(FSingleInBuffer, 0);

     // dispose unused output buffers
     for Channel := 0 to Length(FSingleOutBuffer) - 1
      do Dispose(FSingleOutBuffer[Channel]);
     SetLength(FSingleOutBuffer, 0);
    end;
   cmNone :
    begin
     // dispose unused single input buffers
     for Channel := 0 to Length(FSingleInBuffer) - 1
      do Dispose(FSingleInBuffer[Channel]);
     SetLength(FSingleInBuffer, 0);

     // dispose unused double input buffers
     for Channel := 0 to Length(FDoubleInBuffer) - 1
      do Dispose(FDoubleInBuffer[Channel]);
     SetLength(FDoubleInBuffer, 0);

     // dispose unused single output buffers
     for Channel := 0 to Length(FSingleOutBuffer) - 1
      do Dispose(FSingleOutBuffer[Channel]);
     SetLength(FSingleOutBuffer, 0);

     // dispose unused double output buffers
     for Channel := 0 to Length(FDoubleOutBuffer) - 1
      do Dispose(FDoubleOutBuffer[Channel]);
     SetLength(FDoubleOutBuffer, 0);
    end;
  end;
end;

{$IFDEF AsioMixer}
procedure TCustomAsioHost.Mixer;
begin
 FAsioMixer.Show;
end;
{$ENDIF}

procedure TCustomAsioHost.BufferSwitchTimeInfo(Index: Integer; const Params: TAsioTime);
var
  Sample, Channel : Integer;
  CurrentBuffer   : PAsioBufferInfos;
  ChannelData     : Pointer;
begin
 if FDriver = nil then exit;
 PMUpdSamplePos.wParam := Params.TimeInfo.samplePosition.hi;
 PMUpdSamplePos.LParam := Params.TimeInfo.samplePosition.lo;
 Dispatch(PMUpdSamplePos);

 CurrentBuffer := FInputBuffers;

 // native processing
 if Assigned(FOnBufferSwitchNative)
  then FOnBufferSwitchNative(Self, @(FInputBuffers^), Index);

 if FConvertMethod = cm64 then
  begin
   // 64bit float processing
   case FInBufferPreFill of
      bpfZero : for Channel := 0 to FInputChannelCount - 1
                 do FillChar(FDoubleInBuffer[Channel, 0], FBufferSize * SizeOf(Double), 0);
     bpfNoise : for Channel := 0 to FInputChannelCount - 1 do
                 for Sample := 0 to FBufferSize - 1
                  do FDoubleInBuffer[Channel, Sample] := 2 * Random - 1;
    bpfCustom : if Assigned(FAsioGenerator) then FAsioGenerator.ProcessBuffer64(FDoubleInBuffer, False);
    else
     for Channel := 0 to FInputChannelCount - 1 do
      begin
       ChannelData := CurrentBuffer^[0].Buffers[Index];
       Assert(Assigned(FDoubleInBuffer[Channel]));
       Assert(Length(FInConverters) > Channel);
       Assert(Assigned(FInConverters[Channel].ic64));
       if Assigned(ChannelData)
        then FInConverters[Channel].ic64(ChannelData, @FDoubleInBuffer[Channel, 0], FBufferSize);
       inc(CurrentBuffer);
      end;
   end;

   if FPreventClipping <> pcNone then
    for Channel := 0 to FInputChannelCount - 1
     do FClipPrevent.cb64(@FDoubleInBuffer[Channel, 0], FBufferSize);

   case FOutBufferPreFill of
    bpfZero : for Channel := 0 to FOutputChannelCount - 1
               do FillChar(FDoubleOutBuffer[Channel, 0], FBufferSize * SizeOf(Double), 0);
    bpfNoise: for Channel := 0 to FOutputChannelCount - 1 do
               for Sample := 0 to FBufferSize - 1
                do FDoubleOutBuffer[Channel, Sample] := 2 * Random - 1;
    bpfCustom: if Assigned(FAsioGenerator)
                then FAsioGenerator.ProcessBuffer64(FDoubleOutBuffer, True);
   end;

   if FInputMonitor then
    for Channel := 0 to min(FInputChannelCount, FOutputChannelCount) - 1
     do Move(FDoubleInBuffer[Channel, 0],
             FDoubleOutBuffer[Channel, 0],
             FBufferSize * SizeOf(Double));

   FOnBufferSwitch64(Self, FDoubleInBuffer, FDoubleOutBuffer);

   if FPreventClipping <> pcNone then
    for Channel := 0 to FOutputChannelCount - 1
     do FClipPrevent.cb64(@FDoubleOutBuffer[Channel, 0] ,FBufferSize);

   CurrentBuffer := FOutputBuffers;
   for Channel := 0 to FOutputChannelCount - 1 do
    begin
     ChannelData := CurrentBuffer^[0].Buffers[Index];
     if Assigned(ChannelData)
      then FOutConverters[Channel].oc64(@FDoubleOutBuffer[Channel, 0], ChannelData, FBufferSize);
     inc(CurrentBuffer);
    end;
  end else
 if FConvertMethod = cm32 then
  begin
   // 32bit float processing
   case FInBufferPreFill of
      bpfZero : for Channel := 0 to FInputChannelCount - 1
                 do FillChar(FSingleInBuffer[Channel, 0], FBufferSize * SizeOf(Single), 0);
     bpfNoise : for Channel := 0 to FInputChannelCount - 1 do
                 for Sample := 0 to FBufferSize - 1 do FSingleInBuffer[Channel, Sample] := 2 * Random - 1;
    bpfCustom : if Assigned(FAsioGenerator) then FAsioGenerator.ProcessBuffer32(FSingleInBuffer, False);
    else
     begin
      for Channel := 0 to FInputChannelCount - 1 do
       begin
        ChannelData := CurrentBuffer^[0].Buffers[Index];
        Assert(ChannelData <> nil);
        Assert(Assigned(FSingleInBuffer[Channel]));
        Assert(Length(FInConverters) > Channel);
        Assert(Assigned(FInConverters[Channel].ic32));
        if Assigned(ChannelData)
         then FInConverters[Channel].ic32(ChannelData, @FSingleInBuffer[Channel, 0], FBufferSize);
        inc(CurrentBuffer);
       end;
     end;
   end;

   if FPreventClipping <> pcNone then
    for Channel := 0 to FInputChannelCount - 1
     do FClipPrevent.cb32(@FSingleInBuffer[Channel, 0], FBufferSize);

   case FOutBufferPreFill of
    bpfZero : for Channel := 0 to FOutputChannelCount - 1 do
               begin
                Assert(FSingleOutBuffer[Channel] <> nil);
                FillChar(FSingleOutBuffer[Channel, 0], FBufferSize * SizeOf(Single), 0);
               end;
    bpfNoise: for Channel := 0 to FOutputChannelCount - 1 do
               begin
                Assert(FSingleOutBuffer[Channel] <> nil);
                for Sample := 0 to FBufferSize - 1
                 do FSingleOutBuffer[Channel, Sample] := 2 * Random - 1;
               end;
    bpfCustom: if Assigned(FAsioGenerator)
                then FAsioGenerator.ProcessBuffer32(FSingleOutBuffer, True);
   end;

   if FInputMonitor then
    for Channel := 0 to min(FInputChannelCount, FOutputChannelCount) - 1
     do Move(FSingleInBuffer[Channel, 0],
             FSingleOutBuffer[Channel, 0],
             FBufferSize * SizeOf(Single));

   if Assigned(FOnBufferSwitch32)
    then FOnBufferSwitch32(Self, FSingleInBuffer, FSingleOutBuffer);

   if FPreventClipping <> pcNone then
    for Channel := 0 to FOutputChannelCount - 1
     do FClipPrevent.cb32(@FSingleOutBuffer[Channel, 0], FBufferSize);

   CurrentBuffer := FOutputBuffers;
   for Channel := 0 to FOutputChannelCount - 1 do
    begin
     ChannelData := CurrentBuffer^[0].Buffers[Index];
     if Assigned(ChannelData)
      then FOutConverters[Channel].oc32(@FSingleOutBuffer[Channel, 0], ChannelData, FBufferSize);
     Inc(CurrentBuffer);
    end;
  end;

 FDriver.OutputReady;
end;

procedure TCustomAsioHost.SetOutputDither(const Value: TAsioOutputDither);
begin
 if FOutputDither <> Value then
  begin
   FOutputDither := Value;
   case FOutputDither of
    odNone :
      begin
       Use_FPU;
       case ProcessorType of
        ptSSE: if coSSE in FConvertOptimizations then Use_SSE;
        pt3DNow: if co3DNow in FConvertOptimizations then Use_3DNow;
       end;
      end;
    odUDF  : Use_FPU_UDF;
    odTDF  : Use_FPU_TDF;
   end;
  end;
end;

{$IFDEF SUPPORTS_REGION} {$endregion} {$ENDIF}

initialization
 PMUpdSamplePos.Msg := PM_UpdateSamplePos;
 PMBufSwitch.Msg := PM_BufferSwitch;
 PMBufSwitchTimeInfo.Msg := PM_BufferSwitchTimeInfo;
 PMReset.Msg := PM_Reset;
 IsMultiThread := True;

end.
