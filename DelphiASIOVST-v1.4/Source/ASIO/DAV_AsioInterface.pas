unit DAV_AsioInterface;

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
//                                                                            //
//  The IStdCallAsio has been written and contributed as IBEROASIO interface  //
//  as an ASIO interface wrapper for Delphi & FreePascal by Benjamin Rosseaux //
//  see http://bero.0ok.de/ Copyright (C) 2005-2006,                          //
//  The IStdCallAsio is basically identical to this (except for the name).    //
//                                                                            //
//  Furthermore an IDelphiAsio interface has been developed to simplify the   //
//  interface for an ASIO driver written in Delphi                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

{$IFDEF MSWINDOWS}
uses
  Windows, ActiveX, DAV_ASIO;

type
  IStdCallAsio = interface(IUnknown)
    // never ever change the order of the functions!!!
    function Init(SysHandle: HWND): TASIOBool; stdcall;
    procedure GetDriverName(Name: PAnsiChar); stdcall;
    function GetDriverVersion: LongInt; stdcall;
    procedure GetErrorMessage(ErrorString: PAnsiChar); stdcall;
    function Start: TASIOError; stdcall;
    function Stop: TASIOError; stdcall;
    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; stdcall;
    function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; stdcall;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; stdcall;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetClockSources(Clocks: PASIOClockSources; out NumSources: LongInt): TASIOError; stdcall;
    function SetClockSource(Reference: LongInt): TASIOError; stdcall;
    function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; stdcall;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; stdcall;
    function CreateBuffers(BufferInfos: PASIOBufferInfos; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; stdcall;
    function DisposeBuffers: TASIOError; stdcall;
    function ControlPanel: TASIOError; stdcall;
    function Future(Selector: LongInt; Opt: Pointer): TASIOError; stdcall;
    function OutputReady: TASIOError; stdcall;
  end;

  TStdCallAsio = class(TInterfacedObject, IStdCallAsio)
  private
    ASIODriverInterface: IStdCallAsio;
  public
    constructor Create(AsioCLSID: TClsID; var Success: Boolean);
    destructor Destroy; override;
    function Init(SysHandle: HWND): TASIOBool; stdcall;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function ControlPanel: TASIOError; stdcall;
    function CreateBuffers(BufferInfos: PASIOBufferInfos; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; stdcall;
    function DisposeBuffers: TASIOError; stdcall;
    function Future(Selector: LongInt; Opt: Pointer): TASIOError; stdcall;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; stdcall;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; stdcall;
    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; stdcall;
    function GetClockSources(Clocks: PASIOClockSources; out NumSources: LongInt): TASIOError; stdcall;
    function GetDriverVersion: LongInt; stdcall;
    function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; stdcall;
    function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; stdcall;
    function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function OutputReady: TASIOError; stdcall;
    function SetClockSource(Reference: LongInt): TASIOError; stdcall;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function Start: TASIOError; stdcall;
    function Stop: TASIOError; stdcall;
    procedure GetDriverName(Name: PAnsiChar); stdcall;
    procedure GetErrorMessage(ErrorString: PAnsiChar); stdcall;
  end;

function CreateStdCallASIO(const AsioCLSID: TClsId; var ASIODriver: IStdCallAsio): Boolean; overload;
function CreateStdCallASIO(const AsioCLSID: TClsId; var ASIODriver: TStdCallAsio): Boolean; overload;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
{$IFDEF CPU32}
const
  baQueryInterface    = 0;
  baAddRef            = 4;
  baRelease           = 8;
  baInit              = 12;
  baGetDriverName     = 16;
  baGetDriverVersion  = 20;
  baGetErrorMessage   = 24;
  baStart             = 28;
  baStop              = 32;
  baGetChannels       = 36;
  baGetLatencies      = 40;
  baGetBufferSize     = 44;
  baCanSampleRate     = 48;
  baGetSampleRate     = 52;
  baSetSampleRate     = 56;
  baGetClockSources   = 60;
  baSetClockSource    = 64;
  baGetSamplePosition = 68;
  baGetChannelInfo    = 72;
  baCreateBuffers     = 76;
  baDisposeBuffers    = 80;
  baControlPanel      = 84;
  baFuture            = 88;
  baOutputReady       = 92;
{$ENDIF}
{$IFDEF CPUx86_64}
const
  baQueryInterface    = $0;
  baAddRef            = $8;
  baRelease           = $10;
  baInit              = $18;
  baGetDriverName     = $20;
  baGetDriverVersion  = $28;
  baGetErrorMessage   = $30;
  baStart             = $38;
  baStop              = $40;
  baGetChannels       = $48;
  baGetLatencies      = $50;
  baGetBufferSize     = $58;
  baCanSampleRate     = $60;
  baGetSampleRate     = $68;
  baSetSampleRate     = $70;
  baGetClockSources   = $78;
  baSetClockSource    = $80;
  baGetSamplePosition = $88;
  baGetChannelInfo    = $90;
  baCreateBuffers     = $98;
  baDisposeBuffers    = $A0;
  baControlPanel      = $A8;
  baFuture            = $B0;
  baOutputReady       = $B8;
{$ENDIF}

constructor TStdCallAsio.Create(AsioCLSID: TClsID; var Success: Boolean);
begin
  inherited Create;
  CoInitialize(nil);
  CoCreateInstance(AsioCLSID, nil, CLSCTX_INPROC_SERVER, AsioCLSID,
    ASIODriverInterface);
  Success := Assigned(ASIODriverInterface);
end;

destructor TStdCallAsio.Destroy;
begin
 if Assigned(ASIODriverInterface) then ASIODriverInterface := nil;
 CoUninitialize;
 inherited Destroy;
end;

{$IFDEF CPUx86_64}
{$define PUREPASCAL}  // Changed by RE, otherwise 64 bit crashes??
{$ENDIF}

function TStdCallAsio.Init(SysHandle: HWND): TASIOBool; assembler;
{$IFDEF PUREPASCAL}
begin
  Result:= ASIODriverInterface.Init(SysHandle);
{$ELSE}
asm
{$IFDEF CPUx86_64}
  // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baInit]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR SysHandle
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baInit]
{$ENDIF}
{$ENDIF}
end;

procedure TStdCallAsio.GetDriverName(Name: PAnsiChar); assembler;
{$IFDEF PUREPASCAL}
begin
  ASIODriverInterface.GetDriverName(Name);
{$ELSE}
asm
{$IFDEF CPUx86_64}
  // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baGetDriverName]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR Name
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baGetDriverName]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.GetDriverVersion: LongInt; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.GetDriverVersion;
{$ELSE}
asm
{$IFDEF CPUx86_64}
  // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baGetDriverVersion]
{$ELSE}
    // x86 (32-Bit)
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baGetDriverVersion]
{$ENDIF}
{$ENDIF}
end;

procedure TStdCallAsio.GetErrorMessage(ErrorString: PAnsiChar); assembler;
{$IFDEF PUREPASCAL}
begin
  ASIODriverInterface.GetErrorMessage(ErrorString);
{$ELSE}
asm
{$IFDEF CPUx86_64}
    // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baGetErrorMessage]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR ErrorString
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baGetErrorMessage]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.Start: TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.Start;
{$ELSE}
asm
{$IFDEF CPUx86_64}
    // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baStart]
{$ELSE}
    // x86 (32-Bit)
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baStart]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.Stop: TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.Stop;
{$ELSE}
asm
{$IFDEF CPUx86_64}
    // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baStop]
{$ELSE}
  // x86 (32-Bit)
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baStop]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.GetChannels(NumInputChannels, NumOutputChannels);
{$ELSE}
asm
{$IFDEF CPUx86_64}
    // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baGetChannels]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR NumOutputChannels
    PUSH    DWORD PTR NumInputChannels
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baGetChannels]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.GetLatencies(InputLatency, OutputLatency);
{$ELSE}
asm
{$IFDEF CPUx86_64}
    // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baGetLatencies]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR OutputLatency
    PUSH    DWORD PTR InputLatency
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baGetLatencies]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.GetBufferSize(MinSize, MaxSize, PreferredSize, Granularity);
{$ELSE}
asm
{$IFDEF CPUx86_64}
    // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baGetBufferSize]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR Granularity
    PUSH    DWORD PTR PreferredSize
    PUSH    DWORD PTR MaxSize
    PUSH    DWORD PTR MinSize
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baGetBufferSize]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.CanSampleRate(SampleRate);
{$ELSE}
asm
{$IFDEF CPUx86_64}
    // x86 (64-Bit)
(*
    PUSH    QWORD PTR [SampleRate + 4]
    PUSH    QWORD PTR SampleRate
*)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baCanSampleRate]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR [SampleRate + 4]
    PUSH    DWORD PTR SampleRate
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baCanSampleRate]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.GetSampleRate(SampleRate);
{$ELSE}
asm
{$IFDEF CPUx86_64}
    // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baGetSampleRate]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR SampleRate
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baGetSampleRate]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.SetSampleRate(SampleRate);
{$ELSE}
asm
{$IFDEF CPUx86_64}
    // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baSetSampleRate]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR [SampleRate + 4]
    PUSH    DWORD PTR SampleRate
{$IFDEF FPC}
    MOV     ECX, DWORD PTR SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baSetSampleRate]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.GetClockSources(Clocks: PASIOClockSources; out NumSources: LongInt): TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.GetClockSources(Clocks, NumSources);
{$ELSE}
asm
{$IFDEF CPUx86_64}
    // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baGetClockSources]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR NumSources
    PUSH    DWORD PTR Clocks
{$IFDEF FPC}
    MOV     ECX, DWORD PTR SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baGetClockSources]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.SetClockSource(Reference: LongInt): TAsioError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.SetClockSource(Reference);
{$ELSE}
asm
{$IFDEF CPUx86_64}
  // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baSetClockSource]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR Reference
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baSetClockSource]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.GetSamplePosition(out SamplePosition: TASIOSamples;
  out TimeStamp: TASIOTimeStamp): TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.GetSamplePosition(SamplePosition, TimeStamp);
{$ELSE}
asm
{$IFDEF CPUx86_64}
  // x86 (64-Bit)
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baGetSamplePosition]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR TimeStamp
    PUSH    DWORD PTR SamplePosition
{$IFDEF FPC}
    MOV     ECX, DWORD PTR SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baGetSamplePosition]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.GetChannelInfo(Info);
{$ELSE}
asm
{$IFDEF CPUx86_64}
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baGetChannelInfo]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR Info
{$IFDEF FPC}
    MOV     ECX, DWORD PTR SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baGetChannelInfo]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.CreateBuffers(BufferInfos: PASIOBufferInfos; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.CreateBuffers(BufferInfos, NumChannels, BufferSize, Callbacks);
{$ELSE}
asm
{$IFDEF CPUx86_64}
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baCreateBuffers]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR Callbacks
    PUSH    DWORD PTR BufferSize
    PUSH    DWORD PTR NumChannels
    PUSH    DWORD PTR BufferInfos
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baCreateBuffers]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.DisposeBuffers: TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.DisposeBuffers;
{$ELSE}
asm
{$IFDEF CPUx86_64}
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baDisposeBuffers]
{$ELSE}
  // x86 (32-Bit)
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baDisposeBuffers]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.ControlPanel: TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.ControlPanel;
{$ELSE}
asm
{$IFDEF CPUx86_64}
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baControlPanel]
{$ELSE}
    // x86 (32-Bit)
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baControlPanel]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.Future(Selector: LongInt; Opt: Pointer): TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.Future(Selector, Opt);
{$ELSE}
asm
{$IFDEF CPUx86_64}
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baFuture]
{$ELSE}
    // x86 (32-Bit)
    PUSH    DWORD PTR Opt
    PUSH    DWORD PTR Selector
{$IFDEF FPC}
    MOV     ECX, SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baFuture]
{$ENDIF}
{$ENDIF}
end;

function TStdCallAsio.OutputReady: TASIOError; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ASIODriverInterface.OutputReady;
{$ELSE}
asm
{$IFDEF CPUx86_64}
{$IFDEF FPC}
    MOV     RAX, QWORD PTR [RCX + ASIODriverInterface]
{$ELSE}
    MOV     RAX, QWORD PTR [SELF + ASIODriverInterface]
{$ENDIF}
    MOV     RAX, [RAX]
    CALL    QWORD PTR [RAX + baOutputReady]
{$ELSE}
  // x86 (32-Bit)
{$IFDEF FPC}
    MOV     ECX, DWORD PTR SELF
{$ELSE}
    MOV     ECX, DWORD PTR [SELF]
{$ENDIF}
    MOV     ECX, DWORD PTR [ECX + ASIODriverInterface]
    MOV     EAX, DWORD PTR [ECX]
    CALL    DWORD PTR [EAX + baOutputReady]
{$ENDIF}
{$ENDIF}
end;

function CreateStdCallASIO(const AsioCLSID: TClsId; var ASIODriver: IStdCallAsio): Boolean; overload;
var
  StdCallASIO: TStdCallAsio;
begin
 try
  StdCallASIO := TStdCallAsio.Create(AsioCLSID, Result);
  if Result
   then ASIODriver := StdCallASIO
   else ASIODriver := nil;
  Result := Assigned(ASIODriver);
 except
  Result := False;
 end;
end;

function CreateStdCallASIO(const AsioCLSID: TClsId; var ASIODriver: TStdCallAsio): Boolean; overload;
begin
 try
  ASIODriver := TStdCallAsio.Create(AsioCLSID, Result);
  if not Result then
   begin
    ASIODriver.Destroy;
    ASIODriver := nil;
   end;
  Result := Assigned(ASIODriver);
 except
  Result := False;
 end;
end;

{$ENDIF}

end.
