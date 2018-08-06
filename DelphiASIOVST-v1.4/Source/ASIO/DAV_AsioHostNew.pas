unit DAV_AsioHostNew;

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
//    Benjamin Rosseaux (author of the stdcall interface)                     //
//    Maik Menz (various refactorings)                                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

{$DEFINE AllowMultipleHosts}

uses
  {$IFDEF FPC}LCLIntf, LclType, LMessages, LResources,
  {$ELSE}Windows, Messages, {$ENDIF}
  SysUtils, Classes, Controls,
  DAV_AsioInterface, DAV_Types, DAV_Asio, DAV_AsioList, DAV_AsioConvert,
  DAV_AsioHostCore;

type
  {$IFDEF DELPHI10_UP} {$region 'Basic types'} {$ENDIF}

  TConvertMethod = (cmNone, cm32, cm64);
  TConvertOptimization = (coSSE, co3DNow);
  TConvertOptimizations = set of TConvertOptimization;

  TBufferSwitchEvent32 = procedure(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray) of object;
  TBufferSwitchEvent64 = procedure(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray) of object;

  TBufferPreFill = (bpfNone, bpfZero, bpfNoise, bpfCustom);

  TCustomAudioDeviceNew = class(TComponent);

  // forward decleration
  TCustomAsioHostNative = class;
  {$IFDEF DELPHI10_UP} {$endregion 'Basic types'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TAsioHostBasic'} {$ENDIF}
  TCustomAsioHostNative = class(TComponent)
  private
    procedure ResetDriverSpecificData;
    function GetAsioTime: TAsioTimeSub;
    function GetBufferSize: Cardinal;
    function GetCanDos: TAsioCanDos;
    function GetDriverVersion: Integer;
    function GetInputChannelCount: Integer;
    function GetInputLatency: Integer;
    function GetOutputChannelCount: Integer;
    function GetOutputLatency: Integer;
    function GetSampleRate: Double;
    function GetSupports: TAsioSupports;
    procedure SetSupports(const Value: TAsioSupports);
  protected
    FCore                : TAsioHostCore;
    FOnCreate            : TNotifyEvent;
    FOnDestroy           : TNotifyEvent;
    FOnReset             : TNotifyEvent;
    FOnDriverChanged     : TNotifyEvent;
    FOnLatencyChanged    : TNotifyEvent;
    FOnSampleRateChanged : TNotifyEvent;
    FOnBuffersChanged    : TNotifyEvent;
    FOnBufferSwitch      : TBufferSwitchEvent;
    FAsioDriverList      : TDAVAsioDriverList;
    FActive              : Boolean;
    FDriverIndex         : Integer;
    FDriverName          : string;
    function GetDriverList: TStrings;
    procedure SetActive(Value: Boolean); virtual;
    procedure SetDriverIndex(Value: Integer); virtual;

    procedure ActiveChanged; virtual;
    procedure BuffersChanged; virtual;
    procedure DriverIndexChanged; virtual;

    // ASIO core handler
    procedure BufferSwitchHandler(Sender: TObject;
      const BufferInfo: PAsioBufferList; const BufferIndex : Integer);
    procedure LatencyChangedHandler(Sender: TObject);
    procedure ResetHandler(Sender: TObject);
    procedure SampleRateChangedHandler(Sender: TObject);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    function GetNumDrivers: Integer; virtual;
    procedure Reset; virtual;
    procedure SetIgnoredDriver(ID: TGuid);

    // properties
    property Active: Boolean read FActive write SetActive default False;
    property AsioTime: TAsioTimeSub read GetAsioTime;
    property BufferSize: Cardinal read GetBufferSize;
    property CanDos: TAsioCanDos read GetCanDos;
    property DriverIndex: Integer read FDriverIndex Write SetDriverIndex default -1;
    property DriverList: TStrings read GetDriverList;
    property DriverName: string read FDriverName;
    property DriverVersion: Integer read GetDriverVersion;
    property InputChannelCount: Integer read GetInputChannelCount;
    property InputLatency: Integer read GetInputLatency;
    property OutputChannelCount: Integer read GetOutputChannelCount;
    property OutputLatency: Integer read GetOutputLatency;
    property SampleRate: Double read GetSampleRate;
    property Supports: TAsioSupports read GetSupports write SetSupports;

    // events
    property OnBuffersChanged: TNotifyEvent read FOnBuffersChanged write FOnBuffersChanged;
    property OnBufferSwitch: TBufferSwitchEvent read FOnBufferSwitch write FOnBufferSwitch;
    property OnDriverChanged: TNotifyEvent read FOnDriverChanged write FOnDriverChanged;
    property OnLatencyChanged: TNotifyEvent read FOnLatencyChanged write FOnLatencyChanged;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
    property OnSampleRateChanged: TNotifyEvent read FOnSampleRateChanged write FOnSampleRateChanged;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'TAsioHostBasic'} {$ENDIF}

  TAsioHostNative = class(TCustomAsioHostNative)
  published
    property Active;
    property AsioTime;
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
    property OnBuffersChanged;
    property OnBufferSwitch;
    property OnDriverChanged;
    property OnLatencyChanged;
    property OnReset;
    property OnSampleRateChanged;
  end;

(*
  {$IFDEF DELPHI10_UP} {$region 'TAsioHost'} {$ENDIF}
  TCustomAsioHostNew = class(TCustomAudioDeviceNew)
  private
    FAsioHostCore         : TCustomAsioHostNative;
    FPreventClipping      : Boolean;
    FInBufferPreFill      : TBufferPreFill;
    FOutBufferPreFill     : TBufferPreFill;
    FOnBufferSwitch32     : TBufferSwitchEvent32;
    FOnBufferSwitch64     : TBufferSwitchEvent64;
    FOnBufferSwitchNative : TBufferSwitchEventNative;
    FSingleInBuffer       : TDAVArrayOfSingleFixedArray;
    FSingleOutBuffer      : TDAVArrayOfSingleFixedArray;
    FDoubleInBuffer       : TDAVArrayOfDoubleFixedArray;
    FDoubleOutBuffer      : TDAVArrayOfDoubleFixedArray;
    FInputMonitor         : Boolean;
    FConvertOptimizations : TConvertOptimizations;
    FOutputVolume         : TDAVSingleDynArray;
    FClipPrevent          : TClipBuffer;
    FConvertMethod        : TConvertMethod;
    procedure SetConvertOptimizations(const Value: TConvertOptimizations);
    procedure SetPreventClipping(Value: Boolean);
    procedure SetOnBufferSwitch32(const Value: TBufferSwitchEvent32);
    procedure SetOnBufferSwitch64(const Value: TBufferSwitchEvent64);
    procedure SetConvertMethod(const Value: TConvertMethod);
    procedure OnBufferSwitchChanged;
  protected
    procedure ConvertMethodChanged; virtual;
{
    procedure BufferSwitchTimeInfo(Index: Integer; const Params: TAsioTime); override;
    procedure DetermineBuffersize; override;
}
    procedure ConvertOptimizationsChanged; virtual;
    procedure PreventClippingChanged; virtual;
    procedure CreateFloatBuffers; virtual;

    property ConvertMethod: TConvertMethod read FConvertMethod write SetConvertMethod default cmNone;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ConvertOptimizations: TConvertOptimizations read FConvertOptimizations write SetConvertOptimizations default [coSSE, co3DNow];
    property InputMonitor: Boolean read FInputMonitor write FInputMonitor default False;
    property OnBufferSwitch32: TBufferSwitchEvent32 read FOnBufferSwitch32 write SetOnBufferSwitch32;
    property OnBufferSwitch64: TBufferSwitchEvent64 read FOnBufferSwitch64 write SetOnBufferSwitch64;
    property OnBufferSwitchNative: TBufferSwitchEventNative read FOnBufferSwitchNative write FOnBufferSwitchNative;
    property PreFillInBuffer: TBufferPreFill read FInBufferPreFill write FInBufferPreFill default bpfNone;
    property PreFillOutBuffer: TBufferPreFill read FOutBufferPreFill write FOutBufferPreFill default bpfNone;
    property PreventClipping: Boolean read FPreventClipping write SetPreventClipping default False;
  end;

  TCustomAsioHostNative = class(TAsioHostNative)
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
    property DriverIndex;
    property DriverList;
    property DriverName;
    property DriverVersion;
    property InputChannelCount;
    property InputLatency;
    property InputMonitor;
    property OutputChannelCount;
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
    property OnDriverChanged;
    property OnLatencyChanged;
    property OnReset;
    property OnSampleRateChanged;
    property OnUpdateSamplePos;
  end;
*)

  {$IFDEF DELPHI10_UP} {$endregion 'TAsioHost'} {$ENDIF}

procedure Register;

implementation

uses
  Registry, ComObj, Math, DAV_Common, DAV_AsioResourceStrings;

const
  CInprocServer = 'InprocServer32';
  CAsioPath     = 'software\asio';
  CComClsId     = 'clsid';


{ TCustomAsioHostNative }

{$IFDEF DELPHI10_UP} {$region 'TCustomAsioHostNative implementation'} {$ENDIF}

{ TCustomAsioHostNative }

constructor TCustomAsioHostNative.Create(Owner: TComponent);
begin
 // set the driver itself to nil for now
 FCore := nil;

 // and make sure all controls are enabled or disabled
 FDriverIndex := -1;

 FAsioDriverList := TDAVAsioDriverList.Create;
 try
  FAsioDriverList.UpdateList;
 except
 end;

 inherited;
end;

destructor TCustomAsioHostNative.Destroy;
begin
 try
  if Assigned(FOnDestroy) then FOnDestroy(Self);
//  if Active then Active := False;

  if Assigned(FCore)
   then FreeAndNil(FCore);

  FreeAndNil(FAsioDriverList);
 finally
  inherited;
 end;
end;

procedure TCustomAsioHostNative.ResetDriverSpecificData;
begin
(*
 FDriverName := '';
 FInputLatency := 0;
 FOutputLatency := 0;
 FInputChannelCount := 0;
 FOutputChannelCount := 0;
 FBufferSize := 0;
*)
end;

function TCustomAsioHostNative.GetAsioTime: TAsioTimeSub;
begin
 if Assigned(FCore)
  then Result := FCore.AsioTime
  else Result := nil;
end;

function TCustomAsioHostNative.GetBufferSize: Cardinal;
begin
 if Assigned(FCore)
  then Result := FCore.BufferSize
  else Result := 0;
end;

function TCustomAsioHostNative.GetCanDos: TAsioCanDos;
begin
 if Assigned(FCore)
  then Result := FCore.CanDos
  else Result := [];
end;

function TCustomAsioHostNative.GetDriverList: TStrings;
begin
 Result := FAsioDriverList.DriverNames;
end;

function TCustomAsioHostNative.GetDriverVersion: Integer;
begin
 if Assigned(FCore)
  then Result := FCore.DriverVersion
  else Result := 0;
end;

function TCustomAsioHostNative.GetInputChannelCount: Integer;
begin
 if Assigned(FCore)
  then Result := FCore.InputChannelCount
  else Result := 0;
end;

function TCustomAsioHostNative.GetInputLatency: Integer;
begin
 if Assigned(FCore)
  then Result := FCore.InputLatency
  else Result := 0;
end;

procedure TCustomAsioHostNative.SetIgnoredDriver(ID: TGuid);
begin
 FAsioDriverList.SetIgnoredDriver(ID);
 FAsioDriverList.UpdateList;
end;

procedure TCustomAsioHostNative.SetSupports(const Value: TAsioSupports);
begin
 if Assigned(FCore)
  then FCore.Supports := Value;
end;

procedure TCustomAsioHostNative.BufferSwitchHandler(Sender: TObject;
  const BufferInfo: PAsioBufferList; const BufferIndex: Integer);
begin
 if Assigned(FOnBufferSwitch)
  then FOnBufferSwitch(Self, BufferInfo, BufferIndex);
end;

procedure TCustomAsioHostNative.ResetHandler(Sender: TObject);
begin
 if Assigned(FOnReset)
  then FOnReset(Self);
end;

procedure TCustomAsioHostNative.SampleRateChangedHandler(Sender: TObject);
begin
 if Assigned(FOnSampleRateChanged)
  then FOnSampleRateChanged(Self);
end;

procedure TCustomAsioHostNative.LatencyChangedHandler(Sender: TObject);
begin
 if Assigned(FOnLatencyChanged)
  then FOnLatencyChanged(Self);
end;

procedure TCustomAsioHostNative.SetActive(Value: Boolean);
begin
 if (FCore = nil) then Value := False;

 if FActive <> Value then
  begin
   FActive := Value;
   ActiveChanged;
  end;
end;

procedure TCustomAsioHostNative.SetDriverIndex(Value: Integer);
begin
 if (Value < -1) or (Value >= FAsioDriverList.Count)
  then Value := -1;

 if (Value <> FDriverIndex) then
  begin
   FDriverIndex := Value;
   DriverIndexChanged;
  end;
end;

procedure TCustomAsioHostNative.ActiveChanged;
begin
 if FActive then
  begin
   FCore.Start;
   FActive := FCore.Running;
   if FActive = False then FCore.Stop;
  end
 else
  begin
   FActive := False;
   if Assigned(FCore)
    then FCore.Stop;
  end;
end;

procedure TCustomAsioHostNative.DriverIndexChanged;
var
  WasActive : Boolean;
begin
 WasActive := Active;
 Active := False;

 if Assigned(FCore)
  then FreeAndNil(FCore);

 // check if no driver has been selected and reset all driver specific data
 if FDriverIndex = -1
  then ResetDriverSpecificData
  else
   with FAsioDriverList.Items[FDriverIndex] do
    try
     // create ASIO host core
     FCore := TAsioHostCore.Create(Guid);

     // assign default event handlers
     with FCore do
      begin
       OnLatencyChanged := LatencyChangedHandler;
       OnReset := ResetHandler;
       OnSampleRateChanged := SampleRateChangedHandler;
      end;

     // set driver name
     FDriverName := FAsioDriverList.Items[FDriverIndex].Name;

     // create buffers
     FCore.CreateBuffers;
     BuffersChanged;
    except
     FDriverIndex := -1;
     ResetDriverSpecificData;
     Exit;
    end;

 if Assigned(FOnDriverChanged)
  then FOnDriverChanged(self);

 Active := WasActive;
end;

procedure TCustomAsioHostNative.Reset;
begin
 if Assigned(FCore)
  then Reset;
end;

procedure TCustomAsioHostNative.BuffersChanged;
begin
 if Assigned(FOnBuffersChanged)
  then FOnBuffersChanged(Self);
end;

function TCustomAsioHostNative.GetNumDrivers: Integer;
begin
 Result := FAsioDriverList.Count;
end;

function TCustomAsioHostNative.GetOutputChannelCount: Integer;
begin
 if Assigned(FCore)
  then Result := FCore.OutputChannelCount
  else Result := 0;
end;

function TCustomAsioHostNative.GetOutputLatency: Integer;
begin
 if Assigned(FCore)
  then Result := FCore.OutputLatency
  else Result := 0;
end;

function TCustomAsioHostNative.GetSampleRate: Double;
begin
 if Assigned(FCore)
  then Result := FCore.SampleRate
  else Result := 44100;
end;

function TCustomAsioHostNative.GetSupports: TAsioSupports;
begin
 if Assigned(FCore)
  then Result := FCore.Supports
  else Result := [];
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

(*
////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// TCustomAsioHostNew ////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$region 'TCustomAsioHostNew implementation'} {$ENDIF}

constructor TCustomAsioHostNew.Create(AOwner: TComponent);
begin
 FAsioHostCore         := TCustomAsioHostNative.Create;
 FClipPrevent          := ClipDigital;
 FConvertOptimizations := [coSSE, co3DNow];
 FInputMonitor         := False;
 FConvertMethod        := cmNone;

 {$IFDEF AsioMixer} FAsioMixer := TFmAsioMixer.Create(nil); {$ENDIF}
 inherited;
end;

destructor TCustomAsioHostNew.Destroy;
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

procedure TCustomAsioHostNew.SetOnBufferSwitch32(const Value: TBufferSwitchEvent32);
begin
 FOnBufferSwitch32 := Value;
 OnBufferSwitchChanged;
end;

procedure TCustomAsioHostNew.SetOnBufferSwitch64(const Value: TBufferSwitchEvent64);
begin
 FOnBufferSwitch64 := Value;
 OnBufferSwitchChanged;
end;

procedure TCustomAsioHostNew.OnBufferSwitchChanged;
begin
 if Assigned(FOnBufferSwitch64) then ConvertMethod := cm64 else
 if Assigned(FOnBufferSwitch32) then ConvertMethod := cm32
  else ConvertMethod := cmNone;
end;

procedure TCustomAsioHostNew.SetConvertMethod(const Value: TConvertMethod);
begin
 if ConvertMethod <> Value then
  begin
   FConvertMethod := Value;
   ConvertMethodChanged;
  end;
end;

procedure TCustomAsioHostNew.ConvertMethodChanged;
begin
 CreateFloatBuffers;
end;

procedure TCustomAsioHostNew.SetConvertOptimizations(const Value: TConvertOptimizations);
begin
 if FConvertOptimizations <> Value then
  begin
   FConvertOptimizations := Value;
   ConvertOptimizationsChanged;
  end;
end;

procedure TCustomAsioHostNew.ConvertOptimizationsChanged;
begin
 Use_FPU;
 case ProcessorType of
  ptSSE: if coSSE in FConvertOptimizations then Use_SSE;
  pt3DNow: if co3DNow in FConvertOptimizations then Use_3DNow;
 end;
end;

procedure TCustomAsioHostNew.SetPreventClipping(Value : Boolean);
begin
 if FPreventClipping <> Value then
  begin
   FPreventClipping := Value;
   PreventClippingChanged;
  end;
end;

procedure TCustomAsioHostNew.PreventClippingChanged;
begin
 // nothing here yet
end;

{
function TCustomAsioHostNew.CreateBuffers: Boolean;
var
  Channel : Integer;
begin
 Result := inherited CreateBuffers;

 if Result then
  begin
   SetLength(FOutputVolume, FOutputChannelCount);
   for Channel := 0 to FOutputChannelCount - 1
    do FOutputVolume[Channel] := 1;

   CreateFloatBuffers;
  end;
end;
}

procedure TCustomAsioHostNew.CreateFloatBuffers;
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

procedure TCustomAsioHostNew.BufferSwitchTimeInfo(Index: Integer; const Params: TAsioTime);
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
    else
     for Channel := 0 to FInputChannelCount - 1 do
      begin
       ChannelData := CurrentBuffer^[0].Buffers[Index];
       Assert(Assigned(FDoubleInBuffer[Channel]));
       Assert(Length(FInputChannels) > Channel);
       Assert(Assigned(FInputChannels[Channel].Converter.ic64));
       if Assigned(ChannelData)
        then FInputChannels[Channel].Converter.ic64(ChannelData, @FDoubleInBuffer[Channel, 0], FBufferSize);
       Inc(CurrentBuffer);
      end;
   end;

   if FPreventClipping then
    for Channel := 0 to FInputChannelCount - 1
     do ; // TODO: perform clipping  FClipPrevent.cb64(@FDoubleInBuffer[Channel, 0], FBufferSize);

   case FOutBufferPreFill of
    bpfZero : for Channel := 0 to FOutputChannelCount - 1
               do FillChar(FDoubleOutBuffer[Channel, 0], FBufferSize * SizeOf(Double), 0);
    bpfNoise: for Channel := 0 to FOutputChannelCount - 1 do
               for Sample := 0 to FBufferSize - 1
                do FDoubleOutBuffer[Channel, Sample] := 2 * Random - 1;
   end;

   if FInputMonitor then
    for Channel := 0 to min(FInputChannelCount, FOutputChannelCount) - 1
     do Move(FDoubleInBuffer[Channel, 0],
             FDoubleOutBuffer[Channel, 0],
             FBufferSize * SizeOf(Double));

   FOnBufferSwitch64(Self, FDoubleInBuffer, FDoubleOutBuffer);

   if FPreventClipping then
    for Channel := 0 to FOutputChannelCount - 1
     do ; // TODO: perform clipping  FClipPrevent.cb64(@FDoubleOutBuffer[Channel, 0] ,FBufferSize);

   CurrentBuffer := FOutputBuffers;
   for Channel := 0 to FOutputChannelCount - 1 do
    begin
     ChannelData := CurrentBuffer^[0].Buffers[Index];
     if Assigned(ChannelData)
      then FOutputChannels[Channel].Converter.oc64(@FDoubleOutBuffer[Channel, 0], ChannelData, FBufferSize);
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
    else
     begin
      for Channel := 0 to FInputChannelCount - 1 do
       begin
        ChannelData := CurrentBuffer^[0].Buffers[Index];
        Assert(ChannelData <> nil);
        Assert(Assigned(FSingleInBuffer[Channel]));
        Assert(Length(FInputChannels) > Channel);
        Assert(Assigned(FInputChannels[Channel].Converter.ic32));
        if Assigned(ChannelData)
         then FInputChannels[Channel].Converter.ic32(ChannelData, @FSingleInBuffer[Channel, 0], FBufferSize);
        inc(CurrentBuffer);
       end;
     end;
   end;

   if FPreventClipping then
    for Channel := 0 to FInputChannelCount - 1
     do ; // TODO: perform clipping  FClipPrevent.cb32(@FSingleInBuffer[Channel, 0], FBufferSize);

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
   end;

   if FInputMonitor then
    for Channel := 0 to min(FInputChannelCount, FOutputChannelCount) - 1
     do Move(FSingleInBuffer[Channel, 0],
             FSingleOutBuffer[Channel, 0],
             FBufferSize * SizeOf(Single));

   if Assigned(FOnBufferSwitch32)
    then FOnBufferSwitch32(Self, FSingleInBuffer, FSingleOutBuffer);

   if FPreventClipping then
    for Channel := 0 to FOutputChannelCount - 1
     do ; // TODO: perform clipping  FClipPrevent.cb32(@FSingleOutBuffer[Channel, 0], FBufferSize);

   CurrentBuffer := FOutputBuffers;
   for Channel := 0 to FOutputChannelCount - 1 do
    begin
     ChannelData := CurrentBuffer^[0].Buffers[Index];
     if Assigned(ChannelData)
      then FOutputChannels[Channel].Converter.oc32(@FSingleOutBuffer[Channel, 0], ChannelData, FBufferSize);
     Inc(CurrentBuffer);
    end;
  end;

 FDriver.OutputReady;
end;
*)

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

procedure Register;
begin
 RegisterComponents('ASIO/VST Basics', [TAsioHostNative]);
end;

end.
