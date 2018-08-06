unit ASIOVIObject;

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

{$I DAV_Compiler.inc}

{$IFNDEF FPC} {$INLINE AUTO} {$ENDIF}
{$DEFINE _Debug}

uses
  {FastMove,} Windows, {$IFDEF FPC} LCLIntf, {$ENDIF} Classes,
  DelphiASIO, ASIO, DAV_AsioConvert;

const
  {$EXTERNALSYM WM_USER}
  WM_USER             = $0400;

  PM_ASIO = WM_User + 1652;   // unique we hope
  // ASIO message(s), as wParam for PM_ASIO
  AM_ResetRequest         = 0;
  AM_BufferSwitch         = 1;     // new buffer index in lParam
  AM_BufferSwitchTimeInfo = 2;     // new buffer index in lParam
                                      // time passed in MainForm.BufferTime
  AM_LatencyChanged       = 3;
  PM_UpdateSamplePos      = PM_ASIO + 1;  // sample pos in wParam (hi) and lParam (lo)

  PM_BufferSwitch         = PM_ASIO + 2;
  PM_BufferSwitchTimeInfo = PM_ASIO + 3;

  HKEY_CLASSES_ROOT     = LongWord($80000000);
  HKEY_LOCAL_MACHINE    = LongWord($80000002);

type
  PMessage = ^TMessage;
  TMessage = packed record
    Msg: Cardinal;
    case Integer of
      0: (
        WParam: Longint;
        LParam: Longint;
        Result: Longint);
      1: (
        WParamLo: Word;
        WParamHi: Word;
        LParamLo: Word;
        LParamHi: Word;
        ResultLo: Word;
        ResultHi: Word);
    end;

  LongRec = packed record
    case Integer of
      0: (Lo, Hi: Word);
      1: (Words: array [0..1] of Word);
      2: (Bytes: array [0..3] of Byte);
  end;

  TSingleArray = array of Single;
  PSingleArray = ^TSingleArray;
  TDoubleArray = array of Double;
  PDoubleArray = ^TDoubleArray;
  TArrayOfSingleArray = array of TSingleArray;
  PArrayOfSingleArray = ^TArrayOfSingleArray;

  TLVSingleArray = Array [0..0] of Single;
  PLVSingleArray = ^TLVSingleArray;
  TLVDoubleArray = Array [0..0] of Double;
  PLVDoubleArray = ^TLVDoubleArray;
  TLVArray = record
              Pointer : PDoubleArray;
              Data    : TDoubleArray;
             end;
  PLVArray = ^TLVArray;
  TComplex = record
              Im : Double;
              Re : Double;
             end;

  TAsioDriverDesc = packed record
    id   : TCLSID;
    name : array[0..511] of char;
    path : array[0..511] of char;
  end;
  PAsioDriverDesc = ^TAsioDriverDesc;

  TAsioDriverList = array of TAsioDriverDesc;

  TConvertOptimization = (coSSE, co3DNow);
  TConvertOptimizations = set of TConvertOptimization;

  TInConvertor = procedure(source: pointer; target: PSingle; frames: longint);
  TOutConvertor = procedure(source: PSingle; target: pointer; frames: longint);
  TClipPreventer = procedure(InBuffer: PSingle; Samples: Integer);

  TSamplePositionUpdateEvent = procedure(Sender: TObject; SamplePosition: Int64) of object;
  TSample2Event = procedure(Sender: TObject; Sample: array of Single) of object;

  TBufferPreFill = (bpfNone, bpfZero, bpfNoise, bpfSine);

  TPreventClipping = (pcNone, pcDigital, pcAnalog);

  TInputMonitor = (imDisabled, imMono, imStereo, imAll);

  TATFlag = (atSystemTimeValid, atSamplePositionValid, atSampleRateValid,
             atSpeedValid, atSampleRateChanged, atClockSourceChanged);
  TATFlags = set of TATFlag;
  TNotifyEvent = procedure(Sender: TObject) of object;

  TASIOTimeSub = class(TObject)
  private
    FOnChange: TNotifyEvent;
    function GetATInt64(Index: Integer): Int64;
    function GetATdouble(Index: Integer): Double;
    function GetATflags: TATFlags;
    procedure SetATInt64(Index: Integer; Value: Int64);
    procedure SetATdouble(Index: Integer; Value: Double);
    procedure SetATflags(Flags: TATFlags);
  protected
    FBufferTime: TASIOTime;
    procedure Change; dynamic;
  public
    property OnChanged: TNotifyEvent read FOnChange write FOnChange;
    constructor Create;
  published
    property SamplePos: Int64 index 0 read GetATInt64 write SetATInt64;
    property Speed : Double index 0 read  GetATdouble write SetATdouble; //absolute speed (1. = nominal)
    property SampleRate: Double Index 1 read GetATdouble write SetATdouble;
    property Flags : TATFlags read GetATflags Write SetATflags;
  end;

type TLabviewASIO=class(TObject)
  private
    FActive               : Boolean;
    FPreventClipping      : TPreventClipping;
    FInBufferPreFill      : TBufferPreFill;
    FOutBufferPreFill     : TBufferPreFill;
    FDriverIndex          : Integer;
    FDriverList           : TStrings;
    FDriverName           : String;
    FDriverVersion        : integer;
    FInputLatency         : Integer;
    FOutputLatency        : Integer;
    FInputChannels        : Integer;
    FOutputChannels       : Integer;
    FSampleRate           : Double;
    FBufferSize           : Cardinal;
    FASIOTime             : TASIOTimeSub;
    FOnCreate             : TNotifyEvent;
    FOnDestroy            : TNotifyEvent;
    FOnReset              : TNotifyEvent;
    FOnDriverChanged      : TNotifyEvent;
    FOnLatencyChanged     : TNotifyEvent;
    FOnSampleRateChanged  : TNotifyEvent;
    FOnSample2Output      : TSample2Event;
    FOnInput2Sample       : TSample2Event;
    FOnUpdateSamplePos    : TSamplePositionUpdateEvent;
    FInputChannelOffset   : Word;
    FOutputChannelOffset  : Word;
    Fmin, Fmax,
    Fpref, Fgran          : Integer;
    FInConvertors         : array of TInConvertor;
    FOutConvertors        : array of TOutConvertor;
    ASIOdriverlist        : TASIODriverList;
    Driver                : IDelphiASIO;
    BuffersCreated        : Boolean;
    callbacks             : TASIOCallbacks;
    SingleInBuffer        : TArrayOfSingleArray;
    SingleOutBuffer       : TArrayOfSingleArray;
    UnAlignedBuffer       : PASIOBufferInfo;
    InputBuffer           : PASIOBufferInfo;
    OutputBuffer          : PASIOBufferInfo;
    FInputMonitor         : TInputMonitor;
    FConvertOptimizations : TConvertOptimizations;
    FOutputVolume         : TSingleArray;
    FClipPrevent          : TClipPreventer;
    FASIODone             : Boolean;
    FInMeter              : Array of Single;
    FOutMeter             : Array of Single;
    FSineFrequencies      : Array of Single;
    FSineStarts           : Array of TComplex;
    FSineStates           : Array of TComplex;
    FXBSize               : Integer;
    FXBSizeH, FXBSizeV    : Integer;
    FCalcMeters           : Boolean;
    FWatchDog             : Boolean;
    {$IFDEF Debug}
    FLog                  : TStringList;
    {$ENDIF}
    procedure SetActive(Value: Boolean);
    procedure SetDriverIndex(Value: Integer);
    function CreateBuffers: Boolean;
    procedure DestroyBuffers;
    procedure BufferSwitch(index: integer);
    procedure BufferSwitchTimeInfo(index: integer; const params: TASIOTime);
    procedure SetSampleRate(const Value: Double);
    procedure SetDriverName(const s: String);
    procedure SetInputChannelOffset(const w: Word);
    procedure SetOutputChannelOffset(const w: Word);
    procedure SetConvertOptimizations(const co: TConvertOptimizations);
    procedure SetPreventClipping(v: TPreventClipping);
    function GetBufferSize: Cardinal;
    function GetSampleRate: Double;
    function GetSineFrequency(index: Integer): Single;
    procedure SetSineFrequency(index: Integer; const Value: Single);
    procedure SetXBSize(const Value: Integer);
    procedure ResetPositions;
  protected
    procedure PMASIO(var Message: TMessage); message PM_ASIO;
    procedure PMUpdateSamplePos(var Message: TMessage); message PM_UpdateSamplePos;
    procedure PMBufferSwitch(var Message: TMessage); message PM_BufferSwitch;
    procedure PMBufferSwitchTimeInfo(var Message: TMessage); message PM_BufferSwitchTimeInfo;
    function GetDriverList: TStrings;
  public
    InputChannelInfos   : array of TASIOChannelInfo;
    OutputChannelInfos  : array of TASIOChannelInfo;
    OB,IB               : TArrayOfSingleArray;
    fLastSamples        : TSingleArray;
    LoopCounts,
    BufferUnderruns,
    ReadPosition,
    WritePosition       : Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    function GetNumDrivers: integer;
    procedure OpenDriver;
    procedure CloseDriver;
    function ControlPanel:Integer;
    function CanSampleRate(sampleRate: TASIOSampleRate): TASIOError;
    property SineFrequency[index: Integer]: Single read GetSineFrequency write SetSineFrequency;
  published
    property Active: Boolean read FActive write SetActive;
    property PreventClipping: TPreventClipping read FPreventClipping write SetPreventClipping;
    property PreFillInBuffer: TBufferPreFill read FInBufferPreFill write FInBufferPreFill;
    property PreFillOutBuffer: TBufferPreFill read FOutBufferPreFill write FOutBufferPreFill;
    property DriverName: string read FDriverName write SetDriverName;
    property DriverVersion: integer read FDriverVersion;
    property DriverIndex: Integer read FDriverIndex Write SetDriverIndex default -1;
    property BufferSize: Cardinal read GetBufferSize default 1;
    property BufferMinimum: Integer read Fmin;
    property BufferMaximum: Integer read Fmax;
    property BufferPreferredSize: Integer read Fpref;
    property BufferGranularity: Integer read Fgran;
    property InputLatency: Integer read FInputLatency default 0;
    property InputChannels: Integer read FInputChannels default 0;
    property InputChannelOffset : Word read FInputChannelOffset write SetInputChannelOffset default 0;
    property OutputLatency: Integer read FOutputLatency default 0;
    property OutputChannels: Integer read FOutputChannels default 0;
    property OutputChannelOffset: Word read FOutputChannelOffset write SetOutputChannelOffset default 0;
    property ConvertOptimizations: TConvertOptimizations read FConvertOptimizations write SetConvertOptimizations;
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property ASIOTime: TASIOTimeSub read FASIOTime Write FASIOTime;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnUpdateSamplePos: TSamplePositionUpdateEvent read FOnUpdateSamplePos write FOnUpdateSamplePos;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
    property OnDriverChanged: TNotifyEvent read FOnDriverChanged write FOnDriverChanged;
    property OnLatencyChanged: TNotifyEvent read FOnLatencyChanged write FOnLatencyChanged;
    property OnInput2Sample: TSample2Event read FOnInput2Sample write FOnInput2Sample;
    property OnSample2Output: TSample2Event read FOnSample2Output write FOnSample2Output;
    property OnSampleRateChanged: TNotifyEvent read FOnSampleRateChanged write FOnSampleRateChanged;
    property InputMonitor: TInputMonitor read FInputMonitor write FInputMonitor default imDisabled;
    property DriverList: TStrings read FDriverList;
    property ASIODone: Boolean read FASIODone;
    property ExtraBufferSize: Integer read FXBSize write SetXBSize;
  end;

function ASIOInitDriver(DriverName: PChar): Integer; cdecl;
function ASIOInitDriverIndex(Index: Integer): Integer; cdecl;
function ASIOGetNumDevices:Integer; cdecl;
function ASIOSetDriverIndex(Index: Integer):Integer; cdecl;
function ASIOGetDriverName(Index: Integer): pchar; cdecl;
function ASIOGetDriverNames(Names :pchar; lmaxDriverAnzahl, lDriverNumber: Integer):Integer; cdecl;
function ASIODriverStart:Integer; cdecl;
function ASIODriverStop:Integer; cdecl;
function ASIOGetBufferSize(minSize, maxSize, preferredSize, granularity : PInteger):Integer; cdecl;
function ASIOControlPanel:Integer; cdecl;
function ASIOCanSampleRate(SampleRate: Double): Integer; cdecl;
function ASIOSetSampleRate(SampleRate: Double): Integer; cdecl;
function ASIOGetChannels(InputChannels, OutputChannels: PInteger):Integer; cdecl;
function ASIOOutputType(Index:Integer):Integer; cdecl;
function ASIOSetOutputVolume(Channel:Integer;Volume:Single):Integer; cdecl;
function ASIOSetOutputVolumedB(Channel:Integer;Volume:Single):Integer; cdecl;
function ASIOSetSineFrequency(Channel:Integer;Frequency:Single):Integer; cdecl;
function ASIOGetInputLevel(Channel:Integer):Single; cdecl;
function ASIOGetOutputLevel(Channel:Integer):Single; cdecl;
function ASIOReadWriteSize: Integer; cdecl;
function ASIOReadWriteSizeFixed: Integer; cdecl;
function ASIOReadWrite(Buffer: PDouble; Length, Channel: Integer): Integer; cdecl;
function ASIOReadWriteX(Buffer: Pointer; Length: Integer): Integer; cdecl;
function ASIOAutopilot(Buffer: Pointer; Length: Integer): Integer; cdecl;
function ASIOSetExtraBufferSize(Size: Integer): Integer; cdecl;
function ASIOBufferUnderrun: Integer; cdecl;
procedure ASIOResetBufferUnderruns; cdecl;
function ASIOGetLoopCounts: Integer; cdecl;
procedure ASIOSetLoopCounts(Loops: Integer); cdecl;
function ASIOSetClipFunction(ClipFunction: Integer): Integer; cdecl;
procedure ASIOCalcMeters(CalcMeters: Integer); cdecl;

var theHost             : TLabviewASIO;
    PMUpdSamplePos      : TMessage;
    PMBufSwitch         : TMessage;
    PMBufSwitchTimeInfo : TMessage;
    PMReset             : TMessage;
    ptb                 : Integer;

implementation

uses Registry, SysUtils;

const ASIODRV_DESC  = 'description';
      INPROC_SERVER = 'InprocServer32';
      ASIO_PATH     = 'software\asio';
      COM_CLSID     = 'clsid';

var ASIODriverInfo : TASIODriverInfo;
    RandSeed: Cardinal = $DEADBEAF;

function ASIOInitDriver(DriverName: PChar): Integer; cdecl;
var i : Integer;
    s : string;
begin
 s:=DriverName;
 if s=theHost.DriverName
  then
   begin
    result:=0;
    exit;
   end
  else result:=1;
 for i:=0 to theHost.DriverList.Count-1 do
  begin
   if theHost.DriverList[i]=s then
    begin
     theHost.DriverIndex:=i;
     result:=0;
     break;
    end;
  end;
end;

function ASIOInitDriverIndex(Index: Integer): Integer; cdecl;
begin
 try
  theHost.DriverIndex:=Index;
  result:=0;
 except
  result:=1;
 end;
end;

function ASIOGetNumDevices:Integer; cdecl;
begin
 if assigned(theHost)
  then result:=theHost.DriverList.Count
  else result:=0;
end;

function ASIOBufferUnderrun: Integer; cdecl;
begin
 result:=theHost.BufferUnderruns;
end;

procedure ASIOResetBufferUnderruns; cdecl;
begin
 theHost.BufferUnderruns:=0;
end;

function ASIOGetLoopCounts: Integer; cdecl;
begin
 result:=theHost.LoopCounts;
end;

procedure ASIOSetLoopCounts(Loops: Integer); cdecl;
begin
 theHost.LoopCounts:=Loops;
 {$IFDEF Debug}
 theHost.FLog.Add('Set Loopcounts to: '+IntToStr(Loops));
 theHost.FLog.SaveToFile('ASIOLabVIEW.log');
 {$ENDIF}
end;

function ASIOSetOutputVolume(Channel:Integer;Volume:Single):Integer; cdecl;
var i : Integer;
begin
 result:=0;
 if assigned(theHost) then
  if (Channel>0) and (Channel<=theHost.FOutputChannels)
   then theHost.FOutputVolume[Channel-1]:=Volume
   else
    for i:=0 to theHost.FOutputChannels-1
     do theHost.FOutputVolume[i]:=Volume
  else result:=1;
 {$IFDEF Debug}
 theHost.FLog.Add('Set Output Volume on Channel '+IntToStr(Channel)+' to: '+FloatToStr(Volume));
 theHost.FLog.SaveToFile('ASIOLabVIEW.log');
 {$ENDIF}
end;

// Convert a value in dB's to a linear amplitude
function dB_to_Amp(g:Single):Single;
begin
 if (g>-90.0)
  then Result:=Exp(g*0.05 * Ln(10))
  else Result:=0;
end;

function ASIOSetOutputVolumedB(Channel:Integer;Volume:Single):Integer; cdecl;
var i : Integer;
begin
 Volume:=dB_to_Amp(Volume);
 result:=0;
 if assigned(theHost) then
  if (Channel>0) and (Channel<=theHost.FOutputChannels)
   then theHost.FOutputVolume[Channel-1]:=Volume
   else
    for i:=0 to theHost.FOutputChannels-1
     do theHost.FOutputVolume[i]:=Volume
  else result:=1;
 {$IFDEF Debug}
 theHost.FLog.Add('Set Output Volume on Channel '+IntToStr(Channel)+' to: '+FloatToStr(Volume)+' dB');
 theHost.FLog.SaveToFile('ASIOLabVIEW.log');
 {$ENDIF}
end;

function ASIOSetSineFrequency(Channel:Integer;Frequency:Single):Integer; cdecl;
begin
 result:=0;
 if assigned(theHost)
  then theHost.SineFrequency[Channel]:=Frequency
  else result:=1;
end;

function ASIOGetInputLevel(Channel:Integer):Single; cdecl;
begin
 if (Channel>0) and (Channel<=theHost.FInputChannels)
  then result:=theHost.FInMeter[Channel-1]
  else result:=0;
end;

function ASIOGetOutputLevel(Channel:Integer):Single; cdecl;
begin
 if (Channel>0) and (Channel<=theHost.FOutputChannels)
  then result:=theHost.FOutMeter[Channel-1]
  else result:=0;
end;

procedure ASIOCalcMeters(CalcMeters: Integer); cdecl;
begin
 try
  theHost.FCalcMeters:=CalcMeters>0;
 except
 end;
end;

function ASIOReadWriteSize: Integer; cdecl;
begin
 with theHost do
  begin
   if WritePosition<ReadPosition
    then result:=FXBSize-ReadPosition+WritePosition
    else result:=WritePosition-ReadPosition;
 end;
end;

function ASIOReadWriteSizeFixed: Integer; cdecl;
begin
 with theHost do
  begin
   if WritePosition<ReadPosition
    then result:=FXBSize-ReadPosition+WritePosition
    else result:=WritePosition-ReadPosition;
   if result>FXBSizeV
    then result:=FXBSizeV
    else result:=0;
 end;
end;

function ASIOSetExtraBufferSize(Size: Integer): Integer; cdecl;
begin
 with theHost do
  begin
   {$IFDEF Debug}
   FLog.Add('Extra Buffer set to: '+IntToStr(Size)+' samples');
   FLog.SaveToFile('ASIOLabVIEW.log');
   {$ENDIF}
   ExtraBufferSize:=Size;
   result:=ExtraBufferSize;
  end;
end;

function ASIOReadWrite(Buffer: PDouble; Length, Channel: Integer): Integer; cdecl;
var tmp : TDoubleArray absolute Buffer;
    n   : Integer;
begin
 result:=0;
 with theHost do
  if (Channel>0) then
   if (Channel<=OutputChannels) and (Channel<=InputChannels) then
    try
     while result<Length do
      begin
       theHost.OB[Channel-1,ReadPosition]:=tmp[result];
       tmp[result]:=theHost.IB[Channel-1,ReadPosition];
       inc(ReadPosition); if ReadPosition>=FXBSize then ReadPosition:=0;
       inc(result);
      end;
    except
     result:=0;
    end
  else
   try
    while result<Length do
     begin
      for n:=0 to theHost.FOutputChannels-1
       do theHost.OB[n,ReadPosition]:=tmp[result];
      for n:=0 to theHost.FInputChannels-1
       do tmp[result]:=theHost.IB[n,ReadPosition];
      inc(ReadPosition); if ReadPosition>=FXBSize then ReadPosition:=0;
      inc(result);
     end;
   except
    result:=0;
   end;
end;

function ASIOReadWriteX(Buffer: Pointer; Length: Integer): Integer; cdecl;
var i,j      : Integer;
    Channels : Integer;
{
    OutBuf   : Pointer;
    InBuf    : Pointer;
    RP,EBS   : Integer;
}
begin
 try
  result:=Length;
  Buffer:=Pointer(Buffer^);

  i:=PInteger(Buffer)^;
  if i=Length then Channels:=1 else
   begin
    Channels:=i;
    inc(Integer(Buffer),4);
    i:=PInteger(Buffer)^;
    if i<>Length then raise Exception.Create('ASIO: Length doesn''t fit');
    inc(Integer(Buffer),4);
   end;

{
  OutBuf:=@theHost.OB[0,0];
  InBuf:=@theHost.IB[0,0];
  if Channels>theHost.OutputChannels then Channels:=theHost.OutputChannels;
  RP:=theHost.ReadPosition;
  EBS:=theHost.FXBSize;
  asm
   push ebx
   push esi
   mov esi,RP                // esi = ReadPosition

   mov eax,EBS
   sub eax,RP                // eax=End-RP
   mov edx,eax               // eax=End-RP
   mov ecx,length            // ecx = Length
   sub edx,ecx               // edx =End-RP - Length
   jae @OuterLoop

   @OuterLoop:
    push ecx                 // Rette sich wer kann, hier ecx!
    mov edx,ecx              // edx = i+1
    dec edx                  // edx = i
    shl edx,3                // edx = 8*i
    add edx,Buffer           // edx = Buffer + 8*i
    mov ecx,Channels         // ecx = Channels
    @InnerLoop:
     mov eax,ecx             // eax=j+1
     dec eax                 // eax=j
     mul eax,Length          // eax=j*Length
     mov ebx,eax             // ebx=eax=j*Length
     shl ebx,3               // ebx=8*j*Length
     add ebx,edx             // ebx=Buffer+8*(i+j*Length)
     fld [ebx].Double        // fld [Buffer+8*(i+j*Length)]

     add eax,esi             // eax = j*Length+RP
     shl eax,2               // eax = 4*(j*Length+RP)
     push eax                // Rette sich wer kann, hier eax!
     add eax,OutBuf          // eax=OutBuf+4*(j*Length+RP)
     fstp [eax].Single       // fstp [OutBuf[j*Length+RP]
     pop eax                 // eax = 4*(j*Length+RP)
     add eax,InBuf           // eax=OutBuf+4*(j*Length+RP)
     fld [eax].Single        // fld [InBuf[j*Length+RP]]
     fstp [ebx].Double       // fstp [Buffer+8*(i+j*Length)]
    loop @InnerLoop
    pop ecx

   loop @OuterLoop

   @TwoLoops:

   pop esi
   pop ebx
  end;
  theHost.ReadPosition:=RP;
}

 with theHost do
  begin
   if Channels>OutputChannels then Channels:=OutputChannels;
   if ReadPosition+Length>FXBSize then
    begin
     for j:=0 to Channels-1 do
      for i:=0 to FXBSize-ReadPosition-1 do
       begin
        OB[j,ReadPosition+i]:=PLVDoubleArray(Buffer)^[j*Length+i];
        PLVDoubleArray(Buffer)^[j*Length+i]:=IB[j,ReadPosition+i];
       end;
     ReadPosition:=Length-(FXBSize-ReadPosition);
     for j:=0 to Channels-1 do
      for i:=0 to ReadPosition-1 do
       begin
        OB[j,i]:=PLVDoubleArray(Buffer)^[j*Length+i];
        PLVDoubleArray(Buffer)^[j*Length+i]:=IB[j,i];
       end;
    end
   else
    begin
     for j:=0 to Channels-1 do
      for i:=0 to Length-1 do
       begin
        OB[j,ReadPosition+i]:=PLVDoubleArray(Buffer)^[j*Length+i];
        PLVDoubleArray(Buffer)^[j*Length+i]:=IB[j,ReadPosition+i];
       end;
     ReadPosition:=ReadPosition+Length;
    end;
  end;
 except
  result:=0;
 end;
end;

function ASIOAutopilot(Buffer: Pointer; Length: Integer): Integer; cdecl;
var i,j       : Integer;
    Channels  : Integer;
    oldXBufSz : Integer;
begin
 try
  Buffer:=Pointer(Buffer^);

  i:=PInteger(Buffer)^;
  if i=Length then Channels:=1 else
   begin
    Channels:=i;
    inc(Integer(Buffer),4);
    i:=PInteger(Buffer)^;
    if i<>Length then raise Exception.Create('ASIO: Length doesn''t fit');
    inc(Integer(Buffer),4);
   end;

  with theHost do
   begin
    {$IFDEF Debug}
    FLog.Add('Autopilot started');
    FLog.SaveToFile('ASIOLabVIEW.log');
    {$ENDIF}
    PreventClipping:=pcDigital;
    Active:=False;
    oldXBufSz:=ExtraBufferSize;
    ExtraBufferSize:=Length+FInputLatency+FOutputLatency+BufferSize;
    ReadPosition:=0; WritePosition:=0;
    result:=ExtraBufferSize;
    if Channels>OutputChannels then Channels:=OutputChannels;

    for j:=0 to Channels-1 do
     try
      FillChar(OB[j,0],System.Length(OB[j])*SizeOf(Single),0);
      for i:=0 to Length-1
       do OB[j,i]:=TLVDoubleArray(PLVDoubleArray(Buffer)^)[j*Length+i];
     except
     end;

    {$IFDEF Debug}
    FLog.Add('Autopilot before playing/recording');
    FLog.SaveToFile('ASIOLabVIEW.log');
    {$ENDIF}
    FWatchDog:=False;
    LoopCounts:=1;
    Active:=True;

    if Active then
     begin
      Sleep(Round(2008/SampleRate*(FInputLatency+FOutputLatency+BufferSize)));
      if FWatchDog then
       begin
        Sleep(Round(1008/SampleRate*(Length)));
        if FWatchDog then
         while (BufferUnderruns<LoopCounts) do
          begin
           FWatchDog:=False;
           Sleep(Round(2008/SampleRate*BufferSize));
           if FWatchDog=False
            then Break;
          end;
       end;   
     end;
    Active:=False;
    {$IFDEF Debug}
    FLog.Add('Autopilot before copying');
    FLog.SaveToFile('ASIOLabVIEW.log');
    {$ENDIF}

    for j:=0 to Channels-1 do
     try
      for i:=0 to Length-1
       do PLVDoubleArray(Buffer)^[j*Length+i]:=IB[j,i+FInputLatency+FOutputLatency];
     except
     end;
    ExtraBufferSize:=oldXBufSz;
    {$IFDEF Debug}
    FLog.Add('Autopilot finished');
    FLog.SaveToFile('ASIOLabVIEW.log');
    {$ENDIF}
   end;
 except
  result:=0;
  {$IFDEF Debug}
  with theHost do
   begin
    FLog.Add('Autopilot error');
    FLog.SaveToFile('ASIOLabVIEW.log');
   end; 
  {$ENDIF}
 end;
end;

function ASIOSetClipFunction(ClipFunction: Integer): Integer; cdecl;
begin
 result:=0;
 try
  if (ClipFunction<0) or (ClipFunction>2)
   then theHost.PreventClipping:=TPreventClipping(ClipFunction)
   else result:=1;
 except
  result:=1;
 end;
end;

function ASIOSetDriverIndex(Index: Integer): Integer; cdecl;
begin
 theHost.DriverIndex:=Index;
 result:=Integer(theHost.DriverIndex=Index);
 {$IFDEF Debug}
 theHost.FLog.Add('Driver set to: '+theHost.FDriverList[Index]);
 theHost.FLog.SaveToFile('ASIOLabVIEW.log');
 {$ENDIF}
end;

function ASIOGetDriverName(Index: Integer):pchar; cdecl;
var dn : shortstring;
begin
 GetMem(result,254);
 if (Index<theHost.DriverList.Count) and (Index>=0)
  then dn:=theHost.DriverList[Index]
  else dn:='';
 StrCopy(result,@dn)
end;

function ASIOGetDriverNames(Names :pchar; lmaxDriverAnzahl, lDriverNumber: Integer):Integer; cdecl;
begin
 try
  if (lDriverNumber<theHost.DriverList.Count) and (lDriverNumber>=0)
   then StrCopy(Names,@theHost.DriverList[lDriverNumber][1]);
  result:=0;
 except
  result:=1;
 end;
end;

function ASIOGetBufferSize(minSize, maxSize, preferredSize, granularity : PInteger):Integer; cdecl;
begin
 if assigned(theHost) then
  begin
   minSize^:=theHost.Fmin;
   maxSize^:=theHost.Fmax;
   preferredSize^:=theHost.Fpref;
   granularity^:=theHost.Fgran;
   result:=0;
  end
 else result:=1;
end;

function ASIOGetChannels(InputChannels, OutputChannels: PInteger):Integer; cdecl;
begin
 if assigned(theHost) then
  begin
   InputChannels^:=theHost.InputChannels;
   OutputChannels^:=theHost.OutputChannels;
   result:=0;
  end
 else result:=1;
end;

function ASIOOutputType(Index:Integer):Integer; cdecl;
begin
 case Index of
  0: theHost.PreFillOutBuffer:=bpfNone;
  1: theHost.PreFillOutBuffer:=bpfNoise;
  2: theHost.PreFillOutBuffer:=bpfSine;
 else theHost.PreFillOutBuffer:=bpfZero;
 end;
 Result:=0;
end;

function ASIODriverStart:Integer; cdecl;
begin
 {$IFDEF Debug}
 theHost.FLog.Add('Start Audio (before)');
 theHost.FLog.SaveToFile('ASIOLabVIEW.log');
 {$ENDIF}
 if assigned(theHost)
  then theHost.Active:=True;
 Result:=Integer(not theHost.Active);
 {$IFDEF Debug}
 theHost.FLog.Add('Start Audio (after)');
 theHost.FLog.SaveToFile('ASIOLabVIEW.log');
 {$ENDIF}
end;

function ASIODriverStop:Integer; cdecl;
begin
 {$IFDEF Debug}
 theHost.FLog.Add('Stop Audio (before)');
 theHost.FLog.SaveToFile('ASIOLabVIEW.log');
 {$ENDIF}
 if assigned(theHost)
  then theHost.Active:=False;
 Result:=Integer(theHost.Active);
 {$IFDEF Debug}
 theHost.FLog.Add('Stop Audio (after)');
 theHost.FLog.SaveToFile('ASIOLabVIEW.log');
 {$ENDIF}
end;

function ASIOCanSampleRate(SampleRate: Double): Integer; cdecl;
begin
 result:=Integer(theHost.CanSampleRate(SampleRate)<>0);
end;

function ASIOSetSampleRate(SampleRate: Double): Integer; cdecl;
begin
 theHost.SampleRate:=SampleRate;
 result:=Integer(theHost.SampleRate=SampleRate);
 {$IFDEF Debug}
 theHost.FLog.Add('Extra Buffer set to: '+IntToStr(Round(SampleRate))+' Hz');
 theHost.FLog.SaveToFile('ASIOLabVIEW.log');
 {$ENDIF}
end;

function ASIOControlPanel:Integer; cdecl;
begin
 if assigned(theHost)
  then result:=theHost.ControlPanel
  else result:=1;
end;

{ TLabviewASIO }

function findDrvPath(const clsidstr: string; var dllpath: string): longint;
var
   reg     : TRegistry;
   success : boolean;
   buf     : array[0..1024] of char;
   s       : ansistring;
   temps   : string;
begin
  Result := -1;

  //CharLowerBuff(clsidstr,strlen(clsidstr));
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    success := reg.OpenKeyReadOnly(COM_CLSID + '\' + clsidstr + '\' + INPROC_SERVER);
    if success then
    begin
      dllpath := reg.ReadString('');
      if (ExtractFilePath(dllpath) = '') and (dllpath <> '') then
      begin
        buf[0] := #0;
        temps := dllpath;   // backup the value
        if GetSystemDirectory(buf, 1023) <> 0 then   // try the system directory first
        begin
          s := buf;
          dllpath := s + '\' + temps;
        end;

        if not FileExists(dllpath) then              // try the windows dir if necessary
        begin
          buf[0] := #0;
          if GetWindowsDirectory(buf, 1023) <> 0 then   // try the system directory first
          begin
            s := buf;
            dllpath := s + '\' + temps;
          end;
        end;
      end;

      if FileExists(dllpath) then
        Result := 0;
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

procedure ListAsioDrivers(var List: TAsioDriverList);
var r       : TRegistry;
    keys    : TStringList;
    success : boolean;
    i       : integer;
    id      : string;
    dllpath : string;
    count   : integer;
begin
  SetLength(List, 0);

  keys := TStringList.Create;
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_LOCAL_MACHINE;
    success := r.OpenKeyReadOnly(ASIO_PATH);
    if success then
    begin
     r.GetKeyNames(keys);
     r.CloseKey;
    end;
    count := 0;
    for i := 0 to keys.Count - 1 do
    begin
      success := r.OpenKeyReadOnly(ASIO_PATH + '\' + keys[i]);
      if success then
      begin
        id := r.ReadString(COM_CLSID);
        if findDrvPath(id, dllpath) = 0 then  // check if the dll exists
        begin
          SetLength(List, count+1);
          List[count].id := StringToGUID(id);
          StrPLCopy(List[count].name, keys[i], 512);
          StrPLCopy(List[count].path, dllpath, 512);
          inc(count);
        end;
        r.CloseKey;
      end;
    end;
  finally
    keys.Free;
    r.Free;
  end;
end;

constructor TASIOTimeSub.Create;
begin
 FBufferTime.timeInfo.speed := 1;
 FBufferTime.timeInfo.sampleRate := 44100;
 FBufferTime.timeInfo.samplePosition := Int64ToASIOSamples(0);
 Flags := [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid];
end;

procedure TASIOTimeSub.Change;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

function TASIOTimeSub.GetATflags: TATFlags;
begin
 result := [];
 if (FBufferTime.timeInfo.flags and kSystemTimeValid) <> 0 then
  result := result + [atSystemTimeValid] else
  result := result - [atSystemTimeValid];
 if (FBufferTime.timeInfo.flags and kSamplePositionValid) <> 0 then
  result := result + [atSamplePositionValid] else
  result := result - [atSamplePositionValid];
 if (FBufferTime.timeInfo.flags and kSampleRateValid) <> 0 then
  result := result + [atSampleRateValid] else
  result := result - [atSampleRateValid];
 if (FBufferTime.timeInfo.flags and kSpeedValid) <> 0 then
  result := result + [atSpeedValid] else
  result := result - [atSpeedValid];
 if (FBufferTime.timeInfo.flags and kSampleRateChanged) <> 0 then
  result := result + [atSampleRateChanged] else
  result := result - [atSampleRateChanged];
 if (FBufferTime.timeInfo.flags and kClockSourceChanged) <> 0 then
  result := result + [atClockSourceChanged] else
  result := result - [atClockSourceChanged];
end;

procedure TASIOTimeSub.SetATflags(Flags: TATFlags);
var temp: Integer;
begin
 temp := 0;
 if (atSystemTimeValid in Flags) then temp := temp + kSystemTimeValid;
 if (atSamplePositionValid in Flags) then temp := temp + kSamplePositionValid;
 if (atSampleRateValid in Flags) then temp := temp + kSampleRateValid;
 if (atSpeedValid in Flags) then temp := temp + kSpeedValid;
 if (atSampleRateChanged in Flags) then temp := temp + kSampleRateChanged;
 if (atClockSourceChanged in Flags) then temp := temp + kClockSourceChanged;
 FBufferTime.timeInfo.flags := temp;
end;

function TASIOTimeSub.GetATdouble(Index :Integer): Double;
begin
 Result := 0;
 case Index of
  0: Result := FBufferTime.timeInfo.speed;
  1: Result := FBufferTime.timeInfo.sampleRate;
 end;
end;

procedure TASIOTimeSub.SetATdouble(Index :Integer; Value: Double);
begin
 case Index of
  0: if Value <> FBufferTime.timeInfo.speed then
  begin
   FBufferTime.timeInfo.speed:=Value;
   Change;
  end;
  1: if Value <> FBufferTime.timeInfo.sampleRate then
  begin
   FBufferTime.timeInfo.sampleRate:=Value;
   Change;
  end;
 end;
end;

function TASIOTimeSub.GetATInt64(Index :Integer): Int64;
begin
 Result := 0;
 case Index of
  0: Result := ASIOSamplesToInt64(FBufferTime.timeInfo.samplePosition);
 end;
end;

procedure TASIOTimeSub.SetATInt64(Index :Integer; Value: Int64);
begin
 case Index of
  0: if Value <> ASIOSamplesToInt64(FBufferTime.timeInfo.samplePosition) then
       begin
        FBufferTime.timeInfo.SamplePosition:=Int64ToASIOSamples(Value);
        Change;
       end;
 end;
end;

function ChannelTypeToString(vType: TASIOSampleType): string;
begin
  Result := '';
  case vType of
    ASIOSTInt16MSB   :  Result := 'Int16MSB';
    ASIOSTInt24MSB   :  Result := 'Int24MSB';
    ASIOSTInt32MSB   :  Result := 'Int32MSB';
    ASIOSTFloat32MSB :  Result := 'Float32MSB';
    ASIOSTFloat64MSB :  Result := 'Float64MSB';

    // these are used for 32 bit data buffer, with different alignment of the data inside
    // 32 bit PCI bus systems can be more easily used with these
    ASIOSTInt32MSB16 :  Result := 'Int32MSB16';
    ASIOSTInt32MSB18 :  Result := 'Int32MSB18';
    ASIOSTInt32MSB20 :  Result := 'Int32MSB20';
    ASIOSTInt32MSB24 :  Result := 'Int32MSB24';

    ASIOSTInt16LSB   :  Result := 'Int16LSB';
    ASIOSTInt24LSB   :  Result := 'Int24LSB';
    ASIOSTInt32LSB   :  Result := 'Int32LSB';
    ASIOSTFloat32LSB :  Result := 'Float32LSB';
    ASIOSTFloat64LSB :  Result := 'Float64LSB';

    // these are used for 32 bit data buffer, with different alignment of the data inside
    // 32 bit PCI bus systems can more easily used with these
    ASIOSTInt32LSB16 :  Result := 'Int32LSB16';
    ASIOSTInt32LSB18 :  Result := 'Int32LSB18';
    ASIOSTInt32LSB20 :  Result := 'Int32LSB20';
    ASIOSTInt32LSB24 :  Result := 'Int32LSB24';
  end;
end;

procedure ASIOBufferSwitch(doubleBufferIndex: longint;
 directProcess: TASIOBool); cdecl;
begin
  directProcess := ASIOFalse;
  case directProcess of
    ASIOFalse:
    begin
     PMBufSwitch.WParam := AM_BufferSwitch;
     PMBufSwitch.LParam := doublebufferindex;
     theHost.Dispatch(PMBufSwitch);
    end;
    ASIOTrue:  theHost.BufferSwitch(doubleBufferIndex);
  end;
end;

function ASIOBufferSwitchTimeInfo(var params: TASIOTime;
 doubleBufferIndex: longint; directProcess: TASIOBool): PASIOTime; cdecl;
begin
  directProcess := ASIOFalse;
  case directProcess of
    ASIOFalse:
    begin
     theHost.ASIOTime.FBufferTime := params;
     PMBufSwitchTimeInfo.WParam := AM_BufferSwitchTimeInfo;
     PMBufSwitchTimeInfo.LParam := doublebufferindex;
     theHost.Dispatch(PMBufSwitchTimeInfo);
      end;
    ASIOTrue:  theHost.BufferSwitchTimeInfo(doubleBufferIndex, params);
  end;
  Result := nil;
end;

procedure ASIOSampleRateDidChange(sRate: TASIOSampleRate); cdecl;
begin
 if Assigned(theHost.FOnSampleRateChanged) then
  theHost.FOnSampleRateChanged(theHost);
end;

function ASIOMessage(selector, value: longint;
 message: pointer; opt: pdouble): longint; cdecl;
begin
  Result := 0;
  case selector of
    kASIOSelectorSupported    :   // return 1 if a selector is supported
      begin
        case value of
          kASIOEngineVersion        :  Result := 1;
          kASIOResetRequest         :  Result := 1;
          kASIOBufferSizeChange     :  Result := 0;
          kASIOResyncRequest        :  Result := 1;
          kASIOLatenciesChanged     :  Result := 1;
          kASIOSupportsTimeInfo     :  Result := 1;
          kASIOSupportsTimeCode     :  Result := 1;
          kASIOSupportsInputMonitor :  Result := 0;
        end;
      end;
    kASIOEngineVersion        :  Result := 2;   // ASIO 2 is supported
    kASIOResetRequest         :
      begin
        PMReset.Msg := PM_ASIO;
        PMReset.WParam := AM_ResetRequest;
        PMReset.LParam := 0;
        theHost.Dispatch(PMReset);
        Result := 1;
      end;
    kASIOBufferSizeChange     :
      begin
        PMReset.Msg := PM_ASIO;
        PMReset.WParam := AM_ResetRequest;
        PMReset.LParam := 0;
        theHost.Dispatch(PMReset);
        Result := 1;
      end;
    kASIOResyncRequest        :  ;
    kASIOLatenciesChanged     :
      begin
        PMReset.Msg := PM_ASIO;
        PMReset.WParam := AM_LatencyChanged;
        PMReset.LParam := 0;
        theHost.Dispatch(PMReset);
        Result := 1;
      end;
    kASIOSupportsTimeInfo     :  Result := 1;
    kASIOSupportsTimeCode     :
      Result := 0;
    kASIOSupportsInputMonitor :  Result := 0;
  end;
end;

constructor TLabviewASIO.Create;
begin
 {$IFDEF Debug}
 FLog:=TStringList.Create;
 FLog.Add('Initialize Host');
 FLog.SaveToFile('ASIOLabVIEW.log');
 {$ENDIF}
 FClipPrevent := ClipDigital.cb32;
 FXBSize:=0; LoopCounts:=0;
 BufferUnderruns:=0;
 theHost := Self;
 FCalcMeters:=True;
 UnAlignedBuffer:=nil;
 InputBuffer := nil;
 OutputBuffer := nil;
 ASIOTime := TASIOTimeSub.Create;
 FDriverList := GetDriverList;
 FConvertOptimizations := [coSSE, co3DNow];

 // set the callbacks record fields
 callbacks.bufferSwitch := {$IFDEF FPC}@{$ENDIF}ASIOBufferSwitch;
 callbacks.sampleRateDidChange := {$IFDEF FPC}@{$ENDIF}ASIOSampleRateDidChange;
 callbacks.ASIOMessage := {$IFDEF FPC}@{$ENDIF}ASIOMessage;
 callbacks.bufferSwitchTimeInfo := {$IFDEF FPC}@{$ENDIF}ASIOBufferSwitchTimeInfo;

 // set the driver itself to nil for now
 Driver := nil;
 BuffersCreated := FALSE;

 // and make sure all controls are enabled or disabled
 FDriverIndex := -1;
 FInputMonitor := imDisabled;
 ResetPositions;

 inherited;
 if Assigned(FOnCreate) then FOnCreate(Self);
end;

destructor TLabviewASIO.Destroy;
begin
 if Assigned(FOnDestroy) then FOnDestroy(Self);
 callbacks.bufferSwitchTimeInfo := nil;
 if Active then Active := False;
 CloseDriver;
 SetLength(ASIOdriverlist, 0);
 SetLength(FInConvertors, 0);
 SetLength(FOutConvertors, 0);
 SetLength(FOutputVolume, 0);
 SetLength(FInMeter,0);
 SetLength(FOutMeter,0);
 SetLength(FSineFrequencies, 0);
 SetLength(FSineStarts, 0);
 SetLength(FSineStarts, 0);
 SetLength(ASIOdriverlist, 0);
 SetLength(FInConvertors,0);
 SetLength(FOutConvertors,0);
 SetLength(FOutputVolume,0);
 FDriverList.Free;
 ASIOTime.Free;
 inherited;
 {$IFDEF Debug}
 FLog.Add('Host closed');
 FLog.SaveToFile('ASIOLabVIEW.log');
 FLog.Free;
 {$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////


procedure TLabviewASIO.ResetPositions;
begin
 ReadPosition:=0;
 if FBufferSize<FXBSize
  then ReadPosition:=FXBSize-FBufferSize
  else ReadPosition:=0;
 WritePosition:=0;
end;

function TLabviewASIO.GetDriverList: TStrings;
var i : Integer;
begin
 Result := TStringList.Create;
 SetLength(ASIOdriverlist, 0);
 ListASIODrivers(ASIOdriverlist);
 for i := Low(ASIOdriverlist) to High(ASIOdriverlist) do
  Result.Add(ASIOdriverlist[i].name);
end;

procedure TLabviewASIO.SetDriverName(const s:String);
begin
 if FDriverList.IndexOf(s) > -1
  then DriverIndex := FDriverList.IndexOf(s);
end;

procedure TLabviewASIO.SetInputChannelOffset(const w: Word);
begin
 if (w <> FInputChannelOffset) and (w < FInputChannels)
  then FInputChannelOffset := w;
end;

procedure TLabviewASIO.SetOutputChannelOffset(const w: Word);
begin
 if (w <> FOutputChannelOffset) and (w < FOutputChannels)
  then FOutputChannelOffset := w;
end;

procedure TLabviewASIO.SetConvertOptimizations(const co: TConvertOptimizations);
begin
 Use_FPU;
 case ProcessorType of
  ptFPU: if coSSE in co then Use_SSE;
  pt3DNow: if co3DNow in co then Use_3DNow;
 end;
 FConvertOptimizations := co;
end;

procedure TLabviewASIO.SetDriverIndex(Value: Integer);
var DrName: array[0..255] of Char;
    tmpActive : Boolean;
begin
 if (Value <> FDriverIndex) then
  begin
   tmpActive := Active;
   Active := False;
   if Value < -1 then FDriverIndex := -1 else
    if Value >= FDriverList.Count then FDriverIndex := FDriverList.Count - 1
     else FDriverIndex := Value;
   if FDriverIndex = -1 then
    begin
     FDriverName := '';
     FInputLatency := 0;
     FOutputLatency := 0;
     FInputChannels := 0;
     FOutputChannels := 0;
     FBufferSize := 0;
     CloseDriver;
    end
   else
    begin
     try
      CloseDriver;
      FDriverName := FDriverList[FDriverIndex];
      OpenDriver;
     except
      exit;
     end;
     if assigned(Driver) then
      begin
       Driver.GetDriverName(DrName);
       FDriverVersion := Driver.GetDriverVersion;
      end;
    end;
   if assigned(fOnDriverChanged) then OnDriverChanged(self);
   Active := tmpActive;
  end;
end;

procedure TLabviewASIO.SetPreventClipping(v : TPreventClipping);
begin
 fPreventClipping:=v;
 case fPreventClipping of
  pcDigital: FClipPrevent := ClipDigital.cb32;
  pcAnalog: FClipPrevent := ClipAnalog.cb32;
 end;
end;

procedure TLabviewASIO.SetXBSize(const Value: Integer);
var i : Integer;

 function Max(const A, B: Integer): Integer;
 begin if A > B then Result := A else Result := B; end;

begin
 if FXBSize<>Value then
  try
   BufferUnderruns:=0;
   ResetPositions;
   FXBSize := max(Value,BufferSize);
   for i := 0 to FInputChannels - 1
    do SetLength(IB[i],FXBSize);
   for i := 0 to FOutputChannels - 1
    do SetLength(OB[i],FXBSize);
   FXBSizeH:=FXBSize div 2;
   FXBSizeV:=FXBSize div 4;
  finally
   ResetPositions;
  end;
end;

function TLabviewASIO.CreateBuffers: Boolean;
var i             : integer;
    currentbuffer : PASIOBufferInfo;
begin
 if Driver = nil then
 begin
  result := false;
  Exit;
 end;
 if BuffersCreated then DestroyBuffers;
 Driver.GetBufferSize(Fmin, Fmax, Fpref, Fgran);
 if Fmin = Fmax then Fpref := Fmin;
 FBufferSize := Fpref;
 Driver.GetSampleRate(FSampleRate);
 if FSampleRate < 0 then FSampleRate := 44100;
 SetSampleRate(FSampleRate);
 Driver.GetChannels(FInputChannels, FOutputChannels);
 SetLength(FOutputVolume, FOutputChannels);
 SetLength(FOutMeter,FOutputChannels);
 SetLength(FSineFrequencies, FOutputChannels);
 SetLength(FSineStates, FOutputChannels);
 SetLength(FSineStarts, FOutputChannels);
 SetLength(fLastSamples, FOutputChannels);
 SetLength(OB,FOutputChannels);
 SineFrequency[0]:=1000;

 for i := 0 to FOutputChannels - 1 do
  begin
   FOutputVolume[i] := 1;
   FSineStates[i].Re:=0;
   FSineStates[i].Im:=1;
  end;

 GetMem(UnAlignedBuffer, SizeOf(TAsioBufferInfo) * (FInputChannels + FOutputChannels) + 16);
 InputBuffer := Pointer(Integer(UnAlignedBuffer)+16-(Integer(UnAlignedBuffer) mod 16));

 SetLength(InputChannelInfos, FInputChannels);
 SetLength(SingleInBuffer, FInputChannels);
 SetLength(FInConvertors, FInputChannels);
 SetLength(FInMeter, FInputChannels);
 SetLength(IB,FInputChannels);
 currentbuffer := InputBuffer;
 for i := 0 to FInputChannels - 1 do
  begin
   InputChannelInfos[i].channel := i;
   InputChannelInfos[i].isInput := ASIOTrue;
   Driver.GetChannelInfo(InputChannelInfos[i]);
    case InputChannelInfos[i].vType of
     ASIOSTInt16MSB:   FInConvertors[i] := ToInt16MSB;
     ASIOSTInt24MSB:   FInConvertors[i] := ToInt24MSB;
     ASIOSTInt32MSB:   FInConvertors[i] := ToInt32MSB;
     ASIOSTFloat32MSB: FInConvertors[i] := ToFloat32MSB;
     ASIOSTFloat64MSB: FInConvertors[i] := ToFloat64MSB;
     ASIOSTInt32MSB16: FInConvertors[i] := ToInt32MSB16;
     ASIOSTInt32MSB18: FInConvertors[i] := ToInt32MSB18;
     ASIOSTInt32MSB20: FInConvertors[i] := ToInt32MSB20;
     ASIOSTInt32MSB24: FInConvertors[i] := ToInt32MSB24;
     ASIOSTInt16LSB:   FInConvertors[i] := ToInt16LSB;
     ASIOSTInt24LSB:   FInConvertors[i] := ToInt24LSB;
     ASIOSTInt32LSB:   FInConvertors[i] := ToInt32LSB;
     ASIOSTFloat32LSB: FInConvertors[i] := ToFloat32LSB;
     ASIOSTFloat64LSB: FInConvertors[i] := ToFloat64LSB;
     ASIOSTInt32LSB16: FInConvertors[i] := ToInt32LSB16;
     ASIOSTInt32LSB18: FInConvertors[i] := ToInt32LSB18;
     ASIOSTInt32LSB20: FInConvertors[i] := ToInt32LSB20;
     ASIOSTInt32LSB24: FInConvertors[i] := ToInt32LSB24;
    end;

   SetLength(SingleInBuffer[i], BufferSize);
   FillChar(SingleInBuffer[i,0], BufferSize * SizeOf(Single), 0);
   currentbuffer^.isInput := ASIOTrue;
   currentbuffer^.channelNum := i;
   currentbuffer^.buffers[0] := nil;
   currentbuffer^.buffers[1] := nil;
   inc(currentbuffer);
  end;

 OutputBuffer := currentbuffer;
 SetLength(OutputChannelInfos, FOutputChannels);
 SetLength(SingleOutBuffer, FOutputChannels);
 SetLength(FOutConvertors, FOutputChannels);
 for i := 0 to FOutputChannels - 1 do
  begin
   OutputChannelInfos[i].channel := i;
   OutputChannelInfos[i].isInput := ASIOFalse;   //  output
   Driver.GetChannelInfo(OutputChannelInfos[i]);
   case OutputChannelInfos[i].vType of
    ASIOSTInt16MSB:   FOutConvertors[i] := FromInt16MSB;
    ASIOSTInt24MSB:   FOutConvertors[i] := FromInt24MSB;
    ASIOSTInt32MSB:   FOutConvertors[i] := FromInt32MSB;
    ASIOSTFloat32MSB: FOutConvertors[i] := FromFloat32MSB;
    ASIOSTFloat64MSB: FOutConvertors[i] := FromFloat64MSB;
    ASIOSTInt32MSB16: FOutConvertors[i] := FromInt32MSB16;
    ASIOSTInt32MSB18: FOutConvertors[i] := FromInt32MSB18;
    ASIOSTInt32MSB20: FOutConvertors[i] := FromInt32MSB20;
    ASIOSTInt32MSB24: FOutConvertors[i] := FromInt32MSB24;
    ASIOSTInt16LSB:   FOutConvertors[i] := FromInt16LSB;
    ASIOSTInt24LSB:   FOutConvertors[i] := FromInt24LSB;
    ASIOSTInt32LSB:   FOutConvertors[i] := FromInt32LSB;
    ASIOSTFloat32LSB: FOutConvertors[i] := FromFloat32LSB;
    ASIOSTFloat64LSB: FOutConvertors[i] := FromFloat64LSB;
    ASIOSTInt32LSB16: FOutConvertors[i] := FromInt32LSB16;
    ASIOSTInt32LSB18: FOutConvertors[i] := FromInt32LSB18;
    ASIOSTInt32LSB20: FOutConvertors[i] := FromInt32LSB20;
    ASIOSTInt32LSB24: FOutConvertors[i] := FromInt32LSB24;
   end;
   SetLength(SingleOutBuffer[i], BufferSize);
   FillChar(SingleOutBuffer[i,0], BufferSize * SizeOf(Single), 0);
   currentbuffer^.isInput := ASIOfalse;  // create an output buffer
   currentbuffer^.channelNum := i;
   currentbuffer^.buffers[0] := nil;
   currentbuffer^.buffers[1] := nil;
   inc(currentbuffer);
  end;

 result := (Driver.CreateBuffers(InputBuffer,
  (FInputChannels + FOutputChannels), Fpref, callbacks) = ASE_OK);
 Driver.GetLatencies(FInputLatency, FOutputLatency);
 if Assigned (FOnLatencyChanged) then FOnLatencyChanged(Self);
 Randomize;
 ExtraBufferSize:=8192;
end;

procedure TLabviewASIO.DestroyBuffers;
begin
 if (Driver = nil) then Exit;
 if BuffersCreated then
 begin
  FreeMem(UnAlignedBuffer);
  UnAlignedBuffer := nil;
  InputBuffer := nil;
  OutputBuffer := nil;
  try
   Driver.DisposeBuffers;
  except
  end;
  BuffersCreated := false;
  SingleInBuffer := nil;
  SingleOutBuffer := nil;
  SetLength(InputChannelInfos, 0);
  SetLength(OutputChannelInfos, 0);
 end;
end;

procedure TLabviewASIO.OpenDriver;
var tmpActive: Boolean;
begin
 tmpActive := false;
 if assigned(Driver) then
 begin
  try
   tmpActive := Active;
   Active := False;
   CloseDriver;
  except
  end;
 end;
 if FDriverIndex >= 0 then
 begin
  try
   if CreateDelphiASIO(ASIOdriverlist[FDriverIndex].id, Driver) then
   if not Succeeded(Driver.Init(LongWord(ASIODriverInfo.SysRef))) then
    Driver := nil;
  except
   Driver := nil;
  end;
 end;
// if Driver = nil then raise Exception.Create('ASIO Driver Failed!');
 BuffersCreated := CreateBuffers;
 if tmpActive then Active := True;
end;

procedure TLabviewASIO.CloseDriver;
begin
 if assigned(Driver) then
 begin
  try
   if BuffersCreated then DestroyBuffers;
  except
  end;
  Driver := nil;  // RELEASE;
 end;
 FInputLatency := 0;
 FOutputLatency := 0;
 FInputChannels := 0;
 FOutputChannels := 0;
 FSampleRate := 44100;
end;

function TLabviewASIO.ControlPanel:Integer;
begin
 if assigned(Driver)
  then result:=Driver.ControlPanel
  else result:=1;
end;

procedure TLabviewASIO.Reset;
begin
 OpenDriver; // restart the driver
 if Assigned (FOnReset) then FOnReset(Self);
end;

procedure TLabviewASIO.PMASIO(var Message: TMessage);
var inp, outp: integer;
begin
 if Driver = nil then exit;
 case Message.WParam of
  AM_ResetRequest:
   begin
    OpenDriver; // restart the driver
    if Assigned (FOnReset) then FOnReset(Self);
   end;
  AM_BufferSwitch: BufferSwitch(Message.LParam); // process a buffer
  AM_BufferSwitchTimeInfo: BufferSwitchTimeInfo(Message.LParam,
   ASIOTime.FBufferTime);  // process a buffer with time
  AM_LatencyChanged:
   begin
    if assigned(Driver) then Driver.GetLatencies(inp, outp);
    if assigned(FOnLatencyChanged) then FOnLatencyChanged(Self);
   end;
 end;
end;

procedure TLabviewASIO.PMUpdateSamplePos(var Message: TMessage);
var Samples: TASIOSamples;
begin
 Samples.hi := Message.WParam;
 Samples.lo := Message.LParam;
 if Assigned(FOnUpdateSamplePos)
  then FOnUpdateSamplePos(Self,ASIOSamplesToInt64(Samples));
end;

procedure TLabviewASIO.BufferSwitch(index: integer);
begin
 FillChar(ASIOTime.FBufferTime, SizeOf(TASIOTime), 0);
 // get the time stamp of the buffer, not necessary if no
 // synchronization to other media is required
 if Driver.GetSamplePosition(ASIOTime.FBufferTime.timeInfo.samplePosition,
  ASIOTime.FBufferTime.timeInfo.systemTime) = ASE_OK then
   ASIOTime.Flags := ASIOTime.Flags + [atSystemTimeValid,atSamplePositionValid];
 BufferSwitchTimeInfo(index, ASIOTime.FBufferTime);
end;

procedure TLabviewASIO.BufferSwitchTimeInfo(index: integer;
 const params: TASIOTime);
var i, j, n       : Integer;
    currentbuffer : PASIOBufferInfo;
    PChannelArray : Pointer;
    tmp           : Single;
(*
function DitherNoise:Double;
const two2neg32: double = 10E-7 * ((1.0/$10000) / $10000);  // 2^-32
asm
 imul  edx,RandSeed,08088405H
 inc   edx
 mov   RandSeed,edx
 fld   two2neg32
 fild  RandSeed
 fmulp st(1), st(0)
 imul  edx,RandSeed,08088405H
 inc   edx
 mov   RandSeed,edx
 fld   two2neg32
 fild  RandSeed
 fmulp st(1), st(0)
 fsubp
end;
*)
begin
 if Driver = nil then exit;
 PMUpdSamplePos.wParam := params.timeInfo.samplePosition.hi;
 PMUpdSamplePos.LParam := params.timeInfo.samplePosition.lo;
 Dispatch(PMUpdSamplePos);

 currentbuffer := InputBuffer;
 for j := 0 to FInputChannels - 1 do
 begin
  if FInBufferPreFill = bpfZero
   then FillChar(SingleInBuffer[j,0], BufferSize * SizeOf(Single), 0)
  else if FInBufferPreFill = bpfNoise then
   for i := 0 to BufferSize - 1 do SingleInBuffer[j,i] := 2 * Random - 1
  else begin
   PChannelArray := currentbuffer^.buffers[Index];
   if Assigned(PChannelArray)
    then FInConvertors[j](PChannelArray, PSingle(SingleInBuffer[j]), BufferSize);
   inc(currentbuffer);
  end;
 end;

 if fPreventClipping<>pcNone
  then for j := 0 to FInputChannels - 1 do FClipPrevent(@SingleInBuffer[j,0], BufferSize);

(*
 {$IFDEF Debug}
 FLog.Add('inside');
 FLog.SaveToFile('ASIOLabVIEW.log');
 {$ENDIF}
*)

 if FOutBufferPreFill = bpfZero then
  for j := 0 to FOutputChannels - 1 do FillChar(SingleOutBuffer[j,0], BufferSize * SizeOf(Single), 0)
 else if FOutBufferPreFill = bpfSine then
  for j := 0 to FOutputChannels - 1 do
   for i := 0 to BufferSize - 1 do
    begin
     SingleOutBuffer[j,i] := (FSineStarts[j].Re*FSineStates[j].Re-FSineStarts[j].Im*FSineStates[j].Im);
     FSineStates[j].Im:=FSineStates[j].Im*FSineStarts[j].Re+FSineStates[j].Re*FSineStarts[j].Im;
     FSineStates[j].Re:=SingleOutBuffer[j,i];
     SingleOutBuffer[j,i] := FOutputVolume[j] * SingleOutBuffer[j,i];
     fLastSamples[j]:=SingleOutBuffer[j,i];
    end
 else if FOutBufferPreFill = bpfNoise then
  for j := 0 to FOutputChannels - 1 do
   for i := 0 to BufferSize - 1 do SingleOutBuffer[j,i] := FOutputVolume[j] * (2 * Random - 1)
 else if (LoopCounts>0) and (BufferUnderruns>=LoopCounts) then
  for j := 0 to FOutputChannels - 1 do FillChar(SingleOutBuffer[j,0], BufferSize * SizeOf(Single), 0)
 else
  try
   if FXBSize-WritePosition>=FBufferSize then
    begin
     for n:=0 to FOutputChannels-1 do
      begin
       Move(OB[n,WritePosition],SingleOutBuffer[n,0],FBufferSize*SizeOf(Single));
       Move(SingleInBuffer[n,0],IB[n,WritePosition],FBufferSize*SizeOf(Single));
      end;
     if (WritePosition<ReadPosition) then
      if (WritePosition+FBufferSize>ReadPosition)
       then Inc(BufferUnderruns);
     WritePosition:=WritePosition+FBufferSize;
     if WritePosition>=FXBSize
      then WritePosition:=0;
    end
   else
    begin
     for n:=0 to FOutputChannels-1 do
      begin
       Move(OB[n,WritePosition],SingleOutBuffer[n,0],(FXBSize-WritePosition)*SizeOf(Single));
       Move(SingleInBuffer[n,0],IB[n,WritePosition],(FXBSize-WritePosition)*SizeOf(Single));
      end;
     if (WritePosition<ReadPosition) then Inc(BufferUnderruns);
     WritePosition:=FBufferSize-(FXBSize-WritePosition);
     if (LoopCounts>0) and (BufferUnderruns<LoopCounts) then
      for n:=0 to FOutputChannels-1 do
       begin
        Move(OB[n,0],SingleOutBuffer[n,0],WritePosition*SizeOf(Single));
        Move(SingleInBuffer[n,0],IB[n,0],WritePosition*SizeOf(Single));
       end;
    end;
  except
  end;

 if FCalcMeters then
  begin
   for j := 0 to FOutputChannels - 1 do
    for i := 0 to BufferSize - 1 do
     begin
      tmp:=f_abs(SingleOutBuffer[j,i]);
      if tmp>FOutMeter[j]
       then FOutMeter[j]:=tmp
       else FOutMeter[j]:=0.9999*FOutMeter[j];
     end;

   for j := 0 to FInputChannels - 1 do
    for i := 0 to BufferSize - 1 do
     begin
      tmp:=f_abs(SingleInBuffer[j,i]);
      if tmp>FInMeter[j]
       then FInMeter[j]:=tmp
       else FInMeter[j]:=0.9999*FInMeter[j];
     end;
  end;

 if fPreventClipping<>pcNone then
  for j := 0 to FOutputChannels - 1 do FClipPrevent(@SingleOutBuffer[j,0] ,BufferSize);

 currentbuffer := OutputBuffer;
 for j := 0 to FOutputChannels - 1 do
  begin
   PChannelArray := currentbuffer^.buffers[Index];
   if assigned(PChannelArray)
    then FOutConvertors[j](PSingle(SingleOutBuffer[j]),PChannelArray, BufferSize);
   inc(currentbuffer);
  end;
 if assigned(Driver)
  then Driver.OutputReady;
 FWatchDog:=True;
end;

procedure TLabviewASIO.SetSampleRate(const Value: Double);
begin
 FSampleRate := Value;
 ASIOTime.SampleRate := Value;
 if assigned(Driver) then Driver.SetSampleRate(Value);
end;

procedure TLabviewASIO.SetActive(Value: Boolean);
var currentbuffer : PASIOBufferInfo;
    i : Integer;
begin
 if Driver = nil then exit;
 if FActive = Value then exit;
 if Value = True then
  begin
   try
    {$IFDEF Debug}
    FLog.Add('Start Audio (internal)');
    FLog.SaveToFile('ASIOLabVIEW.log');
    {$ENDIF}
    FActive := (Driver.Start = ASE_OK);
    BufferUnderruns:=0;
    ResetPositions;
   except
    FBufferSize := 2048;
    FSampleRate := 44100;
   end;
   if FActive = False then
    begin
     {$IFDEF Debug}
     FLog.Add('Stop Audio (internal)');
     FLog.SaveToFile('ASIOLabVIEW.log');
     {$ENDIF}
     Driver.Stop;
    end;
  end
 else
  begin
   FActive := False;
   try
    {$IFDEF Debug}
    FLog.Add('Stop Audio (internal)');
    FLog.SaveToFile('ASIOLabVIEW.log');
    {$ENDIF}
    Driver.Stop;
   except
   end;
   if bufferscreated and False then
    try
     currentbuffer := OutputBuffer;
     for i := 0 to FOutputChannels - 1 do
     begin
      FillChar(currentbuffer^.buffers[0]^, BufferSize * SizeOf(Single), 0);
      FillChar(currentbuffer^.buffers[1]^, BufferSize * SizeOf(Single), 0);
      inc(currentbuffer);
     end;
     currentbuffer := InputBuffer;
     for i := 0 to FInputChannels - 1 do
     begin
      FillChar(currentbuffer^.buffers[0]^, BufferSize * SizeOf(Single), 0);
      FillChar(currentbuffer^.buffers[1]^, BufferSize * SizeOf(Single), 0);
      inc(currentbuffer);
     end;
    except
    end;
  end;
end;

function TLabviewASIO.GetNumDrivers: integer;
begin
 result := length(ASIOdriverlist);
end;

function TLabviewASIO.CanSampleRate(sampleRate: TASIOSampleRate): TASIOError;
begin
 if assigned(Driver) then
  result := Driver.CanSampleRate(sampleRate)
 else
  result := ASE_NotPresent;
end;

procedure TLabviewASIO.PMBufferSwitch(var Message: TMessage);
begin
 BufferSwitch(Message.LParam);
end;

procedure TLabviewASIO.PMBufferSwitchTimeInfo(var Message: TMessage);
begin
 BufferSwitchTimeInfo(Message.LParam, ASIOTime.FBufferTime);
end;

function TLabviewASIO.GetBufferSize: Cardinal;
begin
 if (FBufferSize < 1) or (FBufferSize > 65530) then
  FBufferSize := 4096;
 Result := FBufferSize;
end;

function TLabviewASIO.GetSampleRate: Double;
begin
 if (FSampleRate < 1) or (FSampleRate > 1048575) then
  FSampleRate := 44100;
 Result := FSampleRate;
end;

function TLabviewASIO.GetSineFrequency(index: Integer): Single;
begin
 result:=FSineFrequencies[index];
end;

procedure TLabviewASIO.SetSineFrequency(index: Integer;
  const Value: Single);
var d : Double;
    i : Integer;
begin
 if index=0 then
  begin
   d:=(2*Value*PI)/fSampleRate;
   for i := 0 to OutputChannels-1 do
    begin
     FSineStarts[i].Re:=cos(d);
     FSineStarts[i].Im:=sin(d);
     FSineFrequencies[i]:=Value;
    end;
  end
 else
  begin
   d:=(2*Value*PI)/fSampleRate;
   FSineStarts[index-1].Re:=cos(d);
   FSineStarts[index-1].Im:=sin(d);
   FSineFrequencies[index-1]:=Value;
  end;
end;

initialization

 PMUpdSamplePos.Msg := PM_UpdateSamplePos;
 PMBufSwitch.Msg := PM_BufferSwitch;
 PMBufSwitchTimeInfo.Msg := PM_BufferSwitchTimeInfo;
 theHost:=TLabviewASIO.Create;

finalization
 if Assigned(theHost) then
  try
   theHost.PreFillOutBuffer:=bpfNone;
   sleep(20);
   theHost.Active:=False;
   sleep(20);
   theHost.Free;
   theHost:=nil;
  except
  end;

end.
