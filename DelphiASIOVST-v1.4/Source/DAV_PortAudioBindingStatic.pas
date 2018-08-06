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

unit DAV_PortAudioBindingStatic;

interface

{$I DAV_Compiler.inc}

uses
  Types, DAV_PortAudioTypes;

const
{$IF Defined(MSWINDOWS)}
  LibName = 'portaudio.dll';
{$ELSEIF Defined(MACOS)}
  // this is for portaudio version 19
  LibName = 'libportaudio.2.dylib';
{$IFDEF FPC}
{$LINKLIB libportaudio.2}
{$ENDIF}
{$ELSEIF Defined(UNIX)}
  LibName = 'libportaudio.so';
{$IFEND}
{$IFDEF MACOS}

function Pa_GetVersion: LongInt; cdecl; external LibName name '_Pa_GetVersion';
function Pa_GetVersionText: PAnsiChar; cdecl;
  external LibName name '_Pa_GetVersionText';
function Pa_GetErrorText(ErrorCode: TPaError): PAnsiChar; cdecl;
  external LibName name '_Pa_GetErrorText';
function Pa_Initialize: TPaError; cdecl; external LibName name '_Pa_Initialize';
function Pa_Terminate: TPaError; cdecl; external LibName name '_Pa_Terminate';
function Pa_GetHostApiCount: TPaHostApiIndex; cdecl;
  external LibName name '_Pa_GetHostApiCount';
function Pa_GetDefaultHostApi: TPaHostApiIndex; cdecl;
  external LibName name '_Pa_GetDefaultHostApi';
function Pa_GetHostApiInfo(HostApi: TPaHostApiIndex): PPaHostApiInfo; cdecl;
  external LibName name '_Pa_GetHostApiInfo';
function Pa_HostApiTypeIdToHostApiIndex(HostApiTypeId: TPaHostApiTypeId)
  : TPaHostApiIndex; cdecl;
  external LibName name '_Pa_HostApiTypeIdToHostApiIndex';
function Pa_HostApiDeviceIndexToDeviceIndex(HostApi: TPaHostApiIndex;
  HostApiDeviceIndex: LongInt): TPaDeviceIndex; cdecl;
  external LibName name '_Pa_HostApiDeviceIndexToDeviceIndex';
function Pa_GetLastHostErrorInfo: PPaHostErrorInfo; cdecl;
  external LibName name '_Pa_GetLastHostErrorInfo';
function Pa_GetDeviceCount: TPaDeviceIndex; cdecl;
  external LibName name '_Pa_GetDeviceCount';
function Pa_GetDefaultInputDevice: TPaDeviceIndex; cdecl;
  external LibName name '_Pa_GetDefaultInputDevice';
function Pa_GetDefaultOutputDevice: TPaDeviceIndex; cdecl;
  external LibName name '_Pa_GetDefaultOutputDevice';
function Pa_GetDeviceInfo(Device: TPaDeviceIndex): PPaDeviceInfo; cdecl;
  external LibName name '_Pa_GetDeviceInfo';
function Pa_IsFormatSupported(InputParameters: PPaStreamParameters;
  OutputParameters: PPaStreamParameters; SampleRate: Double): TPaError; cdecl;
  external LibName name '_Pa_IsFormatSupported';
function Pa_OpenStream(var Stream: PPaStream;
  InputParameters: PPaStreamParameters; OutputParameters: PPaStreamParameters;
  SampleRate: Double; FramesPerBuffer: NativeUInt; StreamFlags: TPaStreamFlags;
  StreamCallback: PPaStreamCallback; UserData: Pointer): TPaError; cdecl;
  external LibName name '_Pa_OpenStream';
function Pa_OpenDefaultStream(var Stream: PPaStream; NumInputChannels: LongInt;
  NumOutputChannels: LongInt; SampleFormat: TPaSampleFormat; SampleRate: Double;
  FramesPerBuffer: NativeUInt; StreamCallback: PPaStreamCallback;
  UserData: Pointer): TPaError; cdecl;
  external LibName name '_Pa_OpenDefaultStream';
function Pa_CloseStream(Stream: PPaStream): TPaError; cdecl;
  external LibName name '_Pa_CloseStream ';
function Pa_SetStreamFinishedCallback(Stream: PPaStream;
  StreamFinishedCallback: PPaStreamFinishedCallback): TPaError; cdecl;
  external LibName name '_Pa_SetStreamFinishedCallback ';
function Pa_StartStream(Stream: PPaStream): TPaError; cdecl;
  external LibName name '_Pa_StartStream';
function Pa_StopStream(Stream: PPaStream): TPaError; cdecl;
  external LibName name '_Pa_StopStream';
function Pa_AbortStream(Stream: PPaStream): TPaError; cdecl;
  external LibName name '_Pa_AbortStream';
function Pa_IsStreamStopped(Stream: PPaStream): TPaError; cdecl;
  external LibName name '_Pa_IsStreamStopped';
function Pa_IsStreamActive(Stream: PPaStream): TPaError; cdecl;
  external LibName name '_Pa_IsStreamActive';
function Pa_GetStreamInfo(Stream: PPaStream): PPaStreamInfo; cdecl;
  external LibName name '_Pa_GetStreamInfo';
function Pa_GetStreamTime(Stream: PPaStream): TPaTime; cdecl;
  external LibName name '_Pa_GetStreamTime';
function Pa_GetStreamCpuLoad(Stream: PPaStream): Double; cdecl;
  external LibName name '_Pa_GetStreamCpuLoad';
function Pa_ReadStream(Stream: PPaStream; Buffer: Pointer; Frames: NativeUInt)
  : TPaError; cdecl; external LibName name '_Pa_ReadStream';
function Pa_WriteStream(Stream: PPaStream; Buffer: Pointer; Frames: NativeUInt)
  : TPaError; cdecl; external LibName name '_Pa_WriteStream';
function Pa_GetStreamReadAvailable(Stream: PPaStream): NativeInt; cdecl;
  external LibName name '_Pa_GetStreamReadAvailable';
function Pa_GetStreamWriteAvailable(Stream: PPaStream): NativeInt; cdecl;
  external LibName name '_Pa_GetStreamWriteAvailable';
function Pa_GetStreamHostApiType(Stream: PPaStream): TPaHostApiTypeId; cdecl;
  external LibName name '_Pa_GetStreamHostApiType';
function Pa_GetSampleSize(Format: TPaSampleFormat): TPaError; cdecl;
  external LibName name '_Pa_GetSampleSize';
procedure Pa_Sleep(MSec: Int64); cdecl; external LibName name '_Pa_Sleep';
{$ELSE}
function Pa_GetVersion: LongInt; cdecl; external LibName;
function Pa_GetVersionText: PAnsiChar; cdecl; external LibName;
function Pa_GetErrorText(ErrorCode: TPaError): PAnsiChar; cdecl;
  external LibName;
function Pa_Initialize: TPaError; cdecl; external LibName;
function Pa_Terminate: TPaError; cdecl; external LibName;
function Pa_GetHostApiCount: TPaHostApiIndex; cdecl; external LibName;
function Pa_GetDefaultHostApi: TPaHostApiIndex; cdecl; external LibName;
function Pa_GetHostApiInfo(HostApi: TPaHostApiIndex): PPaHostApiInfo; cdecl;
  external LibName;
function Pa_HostApiTypeIdToHostApiIndex(HostApiTypeId: TPaHostApiTypeId)
  : TPaHostApiIndex; cdecl; external LibName;
function Pa_HostApiDeviceIndexToDeviceIndex(HostApi: TPaHostApiIndex;
  HostApiDeviceIndex: LongInt): TPaDeviceIndex; cdecl; external LibName;
function Pa_GetLastHostErrorInfo: PPaHostErrorInfo; cdecl; external LibName;
function Pa_GetDeviceCount: TPaDeviceIndex; cdecl; external LibName;
function Pa_GetDefaultInputDevice: TPaDeviceIndex; cdecl; external LibName;
function Pa_GetDefaultOutputDevice: TPaDeviceIndex; cdecl; external LibName;
function Pa_GetDeviceInfo(Device: TPaDeviceIndex): PPaDeviceInfo; cdecl;
  external LibName;
function Pa_IsFormatSupported(InputParameters: PPaStreamParameters;
  OutputParameters: PPaStreamParameters; SampleRate: Double): TPaError; cdecl;
  external LibName;
function Pa_OpenStream(var Stream: PPaStream;
  InputParameters: PPaStreamParameters; OutputParameters: PPaStreamParameters;
  SampleRate: Double; FramesPerBuffer: NativeUInt; StreamFlags: TPaStreamFlags;
  StreamCallback: PPaStreamCallback; UserData: Pointer): TPaError; cdecl;
  external LibName;
function Pa_OpenDefaultStream(var Stream: PPaStream; NumInputChannels: LongInt;
  NumOutputChannels: LongInt; SampleFormat: TPaSampleFormat; SampleRate: Double;
  FramesPerBuffer: NativeUInt; StreamCallback: PPaStreamCallback;
  UserData: Pointer): TPaError; cdecl; external LibName;
function Pa_CloseStream(Stream: PPaStream): TPaError; cdecl; external LibName;
function Pa_SetStreamFinishedCallback(Stream: PPaStream;
  StreamFinishedCallback: PPaStreamFinishedCallback): TPaError; cdecl;
  external LibName;
function Pa_StartStream(Stream: PPaStream): TPaError; cdecl; external LibName;
function Pa_StopStream(Stream: PPaStream): TPaError; cdecl; external LibName;
function Pa_AbortStream(Stream: PPaStream): TPaError; cdecl; external LibName;
function Pa_IsStreamStopped(Stream: PPaStream): TPaError; cdecl;
  external LibName;
function Pa_IsStreamActive(Stream: PPaStream): TPaError; cdecl;
  external LibName;
function Pa_GetStreamInfo(Stream: PPaStream): PPaStreamInfo; cdecl;
  external LibName;
function Pa_GetStreamTime(Stream: PPaStream): TPaTime; cdecl; external LibName;
function Pa_GetStreamCpuLoad(Stream: PPaStream): Double; cdecl;
  external LibName;
function Pa_ReadStream(Stream: PPaStream; Buffer: Pointer; Frames: NativeUInt)
  : TPaError; cdecl; external LibName;
function Pa_WriteStream(Stream: PPaStream; Buffer: Pointer; Frames: NativeUInt)
  : TPaError; cdecl; external LibName;
function Pa_GetStreamReadAvailable(Stream: PPaStream): NativeInt; cdecl;
  external LibName;
function Pa_GetStreamWriteAvailable(Stream: PPaStream): NativeInt; cdecl;
  external LibName;
function Pa_GetStreamHostApiType(Stream: PPaStream): TPaHostApiTypeId; cdecl;
  external LibName;
function Pa_GetSampleSize(Format: TPaSampleFormat): TPaError; cdecl;
  external LibName;
procedure Pa_Sleep(MSec: Int64); cdecl; external LibName;
{$ENDIF}

implementation

end.
