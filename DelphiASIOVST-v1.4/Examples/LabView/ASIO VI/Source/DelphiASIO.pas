unit DelphiASIO;

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

{$IFDEF WIN32}
{-$IFNDEF FPC}
uses
  ASIO;

type
  PCLSID = PGUID;
  TCLSID = TGUID;
  PIID = PGUID;
  TIID = TGUID;

  IDelphiASIO = interface(IUnknown)
    function Init(SysHandle: LongWord): TASIOBool; stdcall;
    procedure GetDriverName(Name: PAnsiChar); stdcall;
    function GetDriverVersion: LongInt; stdcall;
    procedure GetErrorMessage(ErrorString: PAnsiChar); stdcall;
    function Start: TASIOError; stdcall;
    function Stop: TASIOError; stdcall;
    function GetChannels(out NumInputChannels, NumoutputChannels: LongInt): TASIOError; stdcall;
    function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; stdcall;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; stdcall;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError; stdcall;
    function SetClockSource(Reference: LongInt): Hresult; stdcall;
    function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; stdcall;
    function GetChannelInfo(out Info: TASIOChannelInfo): TASIOError; stdcall;
    function CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; stdcall;
    function DisposeBuffers: TASIOError; stdcall;
    function ControlPanel: TASIOError; stdcall;
    function Future(Selector: LongInt; Opt: Pointer): TASIOError; stdcall;
    function OutputReady: TASIOError; stdcall;
   end;

   TDelphiASIO = class(TInterfacedObject,IDelphiASIO)
   private
     ASIODriverinterface: IDelphiASIO;
   public
     constructor Create(AsioCLSID: TClsID;var Okay: Boolean);
     destructor Destroy; override;
     function Init(SysHandle: LongWord): TASIOBool; stdcall;
     procedure GetDriverName(Name: PAnsiChar); stdcall;
     function GetDriverVersion: LongInt; stdcall;
     procedure GetErrorMessage(ErrorString: PAnsiChar); stdcall;
     function Start: TASIOError; stdcall;
     function Stop: TASIOError; stdcall;
     function GetChannels(out NumInputChannels, NumoutputChannels:LongInt): TASIOError; stdcall;
     function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; stdcall;
     function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; stdcall;
     function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
     function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; stdcall;
     function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
     function GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError; stdcall;
     function SetClockSource(Reference: LongInt): Hresult; stdcall;
     function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; stdcall;
     function GetChannelInfo(out Info: TASIOChannelInfo): TASIOError; stdcall;
     function CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; stdcall;
     function DisposeBuffers: TASIOError; stdcall;
     function ControlPanel: TASIOError; stdcall;
     function Future(Selector: LongInt; Opt: Pointer): TASIOError; stdcall;
     function OutputReady: TASIOError; stdcall;
   end;

{$EXTERNALSYM CoInitialize}
function CoInitialize(pvReserved: Pointer): HResult; stdcall;
{$EXTERNALSYM CoCreateInstance}
function CoCreateInstance(const clsid: TCLSID; unkOuter: IUnknown; dwClsContext: Longint; const iid: TIID; out pv): HResult; stdcall;
{$EXTERNALSYM CoUninitialize}
procedure CoUninitialize; stdcall;

function CreateDelphiASIO(const AsioCLSID:TClsId; var ASIODriver:IDelphiASIO):Boolean; overload;
function CreateDelphiASIO(const AsioCLSID:TClsId; var ASIODriver:TDelphiASIO):Boolean; overload;
{$ENDIF}
{-$ENDIF}

implementation

{$IFDEF WIN32}
{-$IFNDEF FPC}
const
  baQueryinterface = 0;
  baAddRef = 1;
  baRelease = 2;
  baInit = 12;
  baGetDriverName = 16;
  baGetDriverVersion = 20;
  baGetErrorMessage = 24;
  baStart = 28;
  baStop = 32;
  baGetChannels = 36;
  baGetLatencies = 40;
  baGetBufferSize = 44;
  baCanSampleRate = 48;
  baGetSampleRate = 52;
  baSetSampleRate = 56;
  baGetClockSources = 60;
  baSetClockSource = 64;
  baGetSamplePosition = 68;
  baGetChannelInfo = 72;
  baCreateBuffers = 76;
  baDisposeBuffers = 80;
  baControlPanel = 84;
  baFuture = 88;
  baOutputReady = 92;

function CoInitialize; external 'ole32.dll' name 'CoInitialize';
function CoCreateInstance; external 'ole32.dll' name 'CoCreateInstance';
procedure CoUninitialize; external 'ole32.dll' name 'CoUninitialize';

constructor TDelphiASIO.Create(AsioCLSID: TClsID; var Okay: Boolean);
begin
 inherited Create;
 CoInitialize(nil);
 CoCreateInstance(AsioCLSID, nil, 1, AsioCLSID, ASIODriverinterface);
 Okay:=assigned(ASIODriverinterface);
end;

destructor TDelphiASIO.Destroy;
begin
 if assigned(ASIODriverinterface)
  then ASIODriverinterface := nil;
 CoUninitialize;
 inherited Destroy;
end;

function TDelphiASIO.Init(SysHandle: LongWord): TASIOBool; assembler;
asm
 PUSH DWORD PTR SysHandle
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baInit]
end;

procedure TDelphiASIO.GetDriverName(Name: PAnsiChar); assembler;
asm
 PUSH DWORD PTR Name
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetDriverName]
end;

function TDelphiASIO.GetDriverVersion: LongInt; assembler;
asm
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetDriverVersion]
end;

procedure TDelphiASIO.GetErrorMessage(ErrorString: PAnsiChar); assembler;
asm
 PUSH DWORD PTR ErrorString
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetErrorMessage]
end;

function TDelphiASIO.Start: TASIOError; assembler;
asm
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baStart]
end;

function TDelphiASIO.Stop:TASIOError; assembler;
asm
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baStop]
end;

function TDelphiASIO.GetChannels(out NumInputChannels, NumoutputChannels: LongInt): TASIOError; assembler;
asm
 PUSH DWORD PTR NumoutputChannels
 PUSH DWORD PTR NumInputChannels
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetChannels]
end;

function TDelphiASIO.GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; assembler;
asm
 PUSH DWORD PTR outputLatency
 PUSH DWORD PTR InputLatency
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetLatencies]
end;

function TDelphiASIO.GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; assembler;
asm
 PUSH DWORD PTR Granularity
 PUSH DWORD PTR PreferredSize
 PUSH DWORD PTR MaxSize
 PUSH DWORD PTR MinSize
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetBufferSize]
end;

function TDelphiASIO.CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; assembler;
asm
 PUSH DWORD PTR [SampleRate + 4]
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baCanSampleRate]
end;

function TDelphiASIO.GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
asm
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetSampleRate]
end;

function TDelphiASIO.SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; assembler;
asm
 PUSH DWORD PTR [SampleRate + 4]
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baSetSampleRate]
end;

function TDelphiASIO.GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError; assembler;
asm
 PUSH DWORD PTR NumSources
 PUSH DWORD PTR Clocks
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetClockSources]
end;

function TDelphiASIO.SetClockSource(Reference: LongInt): Hresult; assembler;
asm
 PUSH DWORD PTR Reference
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baSetClockSource]
end;

function TDelphiASIO.GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; assembler;
asm
 PUSH DWORD PTR TimeStamp
 PUSH DWORD PTR SamplePosition
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetSamplePosition]
end;

function TDelphiASIO.GetChannelInfo(out Info: TASIOChannelInfo): TASIOError; assembler;
asm
 PUSH DWORD PTR Info
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetChannelInfo]
end;

function TDelphiASIO.CreateBuffers(BufferInfos: PASIOBufferInfo;
  NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; assembler;
asm
 PUSH DWORD PTR Callbacks
 PUSH DWORD PTR BufferSize
 PUSH DWORD PTR NumChannels
 PUSH DWORD PTR BufferInfos
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baCreateBuffers]
end;

function TDelphiASIO.DisposeBuffers: TASIOError; assembler;
asm
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baDisposeBuffers]
end;

function TDelphiASIO.ControlPanel: TASIOError; assembler;
asm
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baControlPanel]
end;

function TDelphiASIO.Future(Selector: LongInt; Opt: Pointer): TASIOError; assembler;
asm
 PUSH DWORD PTR Opt
 PUSH DWORD PTR Selector
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baFuture]
end;

function TDelphiASIO.OutputReady: TASIOError; assembler;
asm
 MOV ECX,DWORD PTR [EAX]
 MOV ECX,DWORD PTR [ECX + ASIODriverinterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baOutputReady]
end;

function CreateDelphiASIO(const AsioCLSID: TClsId; var ASIODriver: IDelphiASIO): Boolean; overload;
var
  DelphiASIO: TDelphiASIO;
begin
 DelphiASIO := TDelphiASIO.Create(AsioCLSID, Result);
 if Result
  then ASIODriver := DelphiASIO
  else ASIODriver := nil;
 Result := assigned(ASIODriver);
end;

function CreateDelphiASIO(const AsioCLSID:TClsId;var ASIODriver:TDelphiASIO):Boolean; overload;
begin
 ASIODriver := TDelphiASIO.Create(AsioCLSID, Result);
 if not Result then
  begin
   ASIODriver.Destroy;
   ASIODriver := nil;
  end;
 Result := assigned(ASIODriver);
end;

initialization
finalization
{-$ENDIF}
{$ENDIF}
end.


