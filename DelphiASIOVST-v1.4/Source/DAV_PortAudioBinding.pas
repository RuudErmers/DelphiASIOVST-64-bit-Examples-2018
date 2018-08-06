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

unit DAV_PortAudioBinding;

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF MSWindows}Windows, {$ENDIF} Types, DAV_PortAudioTypes;

type
  TPa_GetVersion = function: LongInt; cdecl;
  TPa_GetVersionText = function: PAnsiChar; cdecl;
  TPa_GetErrorText = function(ErrorCode: TPaError): PAnsiChar; cdecl;

  TPa_Initialize = function: TPaError; cdecl;
  TPa_Terminate = function: TPaError; cdecl;

  TPa_GetHostApiCount = function: TPaHostApiIndex; cdecl;
  TPa_GetDefaultHostApi = function: TPaHostApiIndex; cdecl;
  TPa_GetHostApiInfo = function(HostApi: TPaHostApiIndex): PPaHostApiInfo; cdecl;
  TPa_HostApiTypeIdToHostApiIndex = function(HostApiTypeId: TPaHostApiTypeId)
    : TPaHostApiIndex; cdecl;
  TPa_HostApiDeviceIndexToDeviceIndex = function(HostApi: TPaHostApiIndex;
    hostApiDeviceIndex: LongInt): TPaDeviceIndex; cdecl;
  TPa_GetLastHostErrorInfo = function: PPaHostErrorInfo; cdecl;
  TPa_GetDeviceCount = function: TPaDeviceIndex; cdecl;
  TPa_GetDefaultInputDevice = function: TPaDeviceIndex; cdecl;
  TPa_GetDefaultOutputDevice = function: TPaDeviceIndex; cdecl;
  TPa_GetDeviceInfo = function(Device: TPaDeviceIndex): PPaDeviceInfo; cdecl;
  TPa_IsFormatSupported = function(InputParameters: PPaStreamParameters;
    OutputParameters: PPaStreamParameters; SampleRate: Double): TPaError; cdecl;
  TPa_OpenStream = function(var Stream: PPaStream;
    InputParameters: PPaStreamParameters; OutputParameters: PPaStreamParameters;
    SampleRate: Double; FramesPerBuffer: NativeUInt;
    StreamFlags: TPaStreamFlags; StreamCallback: PPaStreamCallback;
    UserData: Pointer): TPaError; cdecl;
  TPa_OpenDefaultStream = function(var Stream: PPaStream;
    NumInputChannels: LongInt; NumOutputChannels: LongInt;
    SampleFormat: TPaSampleFormat; SampleRate: Double;
    FramesPerBuffer: NativeUInt; StreamCallback: PPaStreamCallback;
    UserData: Pointer): TPaError; cdecl;
  TPa_CloseStream = function(Stream: PPaStream): TPaError; cdecl;
  TPa_SetStreamFinishedCallback = function(Stream: PPaStream;
    StreamFinishedCallback: PPaStreamFinishedCallback): TPaError; cdecl;
  TPa_StartStream = function(Stream: PPaStream): TPaError; cdecl;
  TPa_StopStream = function(Stream: PPaStream): TPaError; cdecl;
  TPa_AbortStream = function(Stream: PPaStream): TPaError; cdecl;
  TPa_IsStreamStopped = function(Stream: PPaStream): TPaError; cdecl;
  TPa_IsStreamActive = function(Stream: PPaStream): TPaError; cdecl;
  TPa_GetStreamInfo = function(Stream: PPaStream): PPaStreamInfo; cdecl;
  TPa_GetStreamTime = function(Stream: PPaStream): TPaTime; cdecl;
  TPa_GetStreamCpuLoad = function(Stream: PPaStream): Double; cdecl;
  TPa_ReadStream = function(Stream: PPaStream; Buffer: Pointer;
    Frames: NativeUInt): TPaError; cdecl;
  TPa_WriteStream = function(Stream: PPaStream; Buffer: Pointer;
    Frames: NativeUInt): TPaError; cdecl;
  TPa_GetStreamReadAvailable = function(Stream: PPaStream): NativeInt; cdecl;
  TPa_GetStreamWriteAvailable = function(Stream: PPaStream): NativeInt; cdecl;
  TPa_GetStreamHostApiType = function(Stream: PPaStream)
    : TPaHostApiTypeId; cdecl;
  TPa_GetSampleSize = function(Format: TPaSampleFormat): TPaError; cdecl;
  TPa_Sleep = procedure(MSec: Int64); cdecl;

var
  Pa_GetVersion: TPa_GetVersion;
  Pa_GetVersionText: TPa_GetVersionText;
  Pa_GetErrorText: TPa_GetErrorText;
  Pa_Initialize: TPa_Initialize;
  Pa_Terminate: TPa_Terminate;
  Pa_GetHostApiCount: TPa_GetHostApiCount;
  Pa_GetDefaultHostApi: TPa_GetDefaultHostApi;
  Pa_GetHostApiInfo: TPa_GetHostApiInfo;
  Pa_HostApiTypeIdToHostApiIndex: TPa_HostApiTypeIdToHostApiIndex;
  Pa_HostApiDeviceIndexToDeviceIndex: TPa_HostApiDeviceIndexToDeviceIndex;
  Pa_GetLastHostErrorInfo: TPa_GetLastHostErrorInfo;
  Pa_GetDeviceCount: TPa_GetDeviceCount;
  Pa_GetDefaultInputDevice: TPa_GetDefaultInputDevice;
  Pa_GetDefaultOutputDevice: TPa_GetDefaultOutputDevice;
  Pa_GetDeviceInfo: TPa_GetDeviceInfo;
  Pa_IsFormatSupported: TPa_IsFormatSupported;
  Pa_OpenStream: TPa_OpenStream;
  Pa_OpenDefaultStream: TPa_OpenDefaultStream;
  Pa_CloseStream: TPa_CloseStream;
  Pa_SetStreamFinishedCallback: TPa_SetStreamFinishedCallback;
  Pa_StartStream: TPa_StartStream;
  Pa_StopStream: TPa_StopStream;
  Pa_AbortStream: TPa_AbortStream;
  Pa_IsStreamStopped: TPa_IsStreamStopped;
  Pa_IsStreamActive: TPa_IsStreamActive;
  Pa_GetStreamInfo: TPa_GetStreamInfo;
  Pa_GetStreamTime: TPa_GetStreamTime;
  Pa_GetStreamCpuLoad: TPa_GetStreamCpuLoad;
  Pa_ReadStream: TPa_ReadStream;
  Pa_WriteStream: TPa_WriteStream;
  Pa_GetStreamReadAvailable: TPa_GetStreamReadAvailable;
  Pa_GetStreamWriteAvailable: TPa_GetStreamWriteAvailable;
  Pa_GetStreamHostApiType: TPa_GetStreamHostApiType;
  Pa_GetSampleSize: TPa_GetSampleSize;
  Pa_Sleep: TPa_Sleep;

implementation

var
  PortAudioLibHandle: HINST;
{$IF Defined(MSWINDOWS)}
  PortAudioDLL: PAnsiChar = 'PortAudio.dll';
{$ELSEIF Defined(DARWIN)}
  // this is for portaudio version 19
  PortAudioDLL: PAnsiChar = 'libportaudio.2.dylib';
{$LINKLIB libportaudio.2}
{$ELSEIF Defined(UNIX)}
  PortAudioDLL: PAnsiChar = 'libportaudio.so';
{$IFEND}

procedure InitDLL;
begin
  PortAudioLibHandle := LoadLibraryA(PortAudioDLL);
  if PortAudioLibHandle <> 0 then
    try
      Pa_GetVersion := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetVersion'));
      Pa_GetVersionText := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetVersionText'));
      Pa_GetErrorText := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetErrorText'));
      Pa_Initialize := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_Initialize'));
      Pa_Terminate := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_Terminate'));
      Pa_GetHostApiCount := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetHostApiCount'));
      Pa_GetDefaultHostApi := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetDefaultHostApi'));
      Pa_GetHostApiInfo := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetHostApiInfo'));
      Pa_HostApiTypeIdToHostApiIndex := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_HostApiTypeIdToHostApiIndex'));
      Pa_HostApiDeviceIndexToDeviceIndex := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_HostApiDeviceIndexToDeviceIndex'));
      Pa_GetLastHostErrorInfo := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetLastHostErrorInfo'));
      Pa_GetDeviceCount := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetDeviceCount'));
      Pa_GetDefaultInputDevice := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetDefaultInputDevice'));
      Pa_GetDefaultOutputDevice := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetDefaultOutputDevice'));
      Pa_GetDeviceInfo := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetDeviceInfo'));
      Pa_IsFormatSupported := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_IsFormatSupported'));
      Pa_OpenStream := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_OpenStream'));
      Pa_OpenDefaultStream := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_OpenDefaultStream'));
      Pa_CloseStream := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_CloseStream'));
      Pa_SetStreamFinishedCallback := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_SetStreamFinishedCallback'));
      Pa_StartStream := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_StartStream'));
      Pa_StopStream := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_StopStream'));
      Pa_AbortStream := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_AbortStream'));
      Pa_IsStreamStopped := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_IsStreamStopped'));
      Pa_IsStreamActive := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_IsStreamActive'));
      Pa_GetStreamInfo := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetStreamInfo'));
      Pa_GetStreamTime := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetStreamTime'));
      Pa_GetStreamCpuLoad := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetStreamCpuLoad'));
      Pa_ReadStream := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_ReadStream'));
      Pa_WriteStream := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_WriteStream'));
      Pa_GetStreamReadAvailable := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetStreamReadAvailable'));
      Pa_GetStreamWriteAvailable := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetStreamWriteAvailable'));
      Pa_GetStreamHostApiType := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetStreamHostApiType'));
      Pa_GetSampleSize := GetProcAddress(PortAudioLibHandle,
        PAnsiChar('Pa_GetSampleSize'));
      Pa_Sleep := GetProcAddress(PortAudioLibHandle, PAnsiChar('Pa_Sleep'));
    except
      FreeLibrary(PortAudioLibHandle);
      PortAudioLibHandle := 0;
    end;
end;

procedure FreeDLL;
begin
  if PortAudioLibHandle <> 0 then
    FreeLibrary(PortAudioLibHandle);
end;

initialization

InitDLL;

finalization

FreeDLL;

end.
