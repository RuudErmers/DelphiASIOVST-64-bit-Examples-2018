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

unit DAV_PortAudioTypes;

interface

{$I DAV_Compiler.inc}

type
  TPaError = LongInt;
  TPaErrorCode = LongInt;
  TPaDeviceIndex = LongInt;
  TPaHostApiIndex = LongInt;
  TPaHostApiTypeId = LongInt;
  TPaTime = Double;
  TPaSampleFormat = NativeUInt;
  PPaStream = Pointer;
  TPaStreamFlags = NativeUInt;
  TPaStreamCallbackFlags = NativeUInt;
  TPaStreamCallbackResult = LongInt;

  PPaHostApiInfo = ^TPaHostApiInfo;
  TPaHostApiInfo = record
    StructVersion: LongInt;
    HostApiTypeId: TPaHostApiTypeId;
    Name: PAnsiChar;
    DeviceCount: LongInt;
    DefaultInputDevice: TPaDeviceIndex;
    DefaultOutputDevice: TPaDeviceIndex;
  end;

  PPaHostErrorInfo = ^TPaHostErrorInfo;
  TPaHostErrorInfo = record
    HostApiType: TPaHostApiTypeId;
    ErrorCode: Int64;
    ErrorText: PAnsiChar;
  end;

  PPaDeviceInfo = ^TPaDeviceInfo;
  TPaDeviceInfo = record
    StructVersion: LongInt;
    Name: PAnsiChar;
    HostApi: TPaHostApiIndex;

    MaxInputChannels: LongInt;
    MaxOutputChannels: LongInt;

    DefaultLowInputLatency: TPaTime;
    DefaultLowOutputLatency: TPaTime;
    DefaultHighInputLatency: TPaTime;
    DefaultHighOutputLatency: TPaTime;

    DefaultSampleRate: Double;
  end;

  PPaStreamParameters = ^TPaStreamParameters;
  TPaStreamParameters = record
    Device: TPaDeviceIndex;
    ChannelCount: LongInt;
    SampleFormat: TPaSampleFormat;
    SuggestedLatency: TPaTime;
    HostApiSpecificStreamInfo: Pointer;
  end;

  PPaStreamCallbackTimeInfo = ^TPaStreamCallbackTimeInfo;
  TPaStreamCallbackTimeInfo = record
    InputBufferAdcTime: TPaTime;
    CurrentTime: TPaTime;
    OutputBufferDacTime: TPaTime;
  end;

  PPaStreamCallback = ^TPaStreamCallback;
  TPaStreamCallback = function(Input: Pointer; Output: Pointer;
    FrameCount: NativeUInt; TimeInfo: PPaStreamCallbackTimeInfo;
    StatusFlags: TPaStreamCallbackFlags; UserData: Pointer): LongInt; cdecl;

  PPaStreamFinishedCallback = ^TPaStreamFinishedCallback;
  TPaStreamFinishedCallback = procedure(UserData: Pointer); cdecl;

  PPaStreamInfo = ^TPaStreamInfo;
  TPaStreamInfo = record
    StructVersion: LongInt;
    InputLatency: TPaTime;
    OutputLatency: TPaTime;
    SampleRate: Double;
  end;

const
  { enum_begin PaErrorCode }
  paNoError = 0;
  paNotInitialized = -10000;
  paUnanticipatedHostError = (paNotInitialized + 1);
  paInvalidChannelCount = (paNotInitialized + 2);
  paInvalidSampleRate = (paNotInitialized + 3);
  paInvalidDevice = (paNotInitialized + 4);
  paInvalidFlag = (paNotInitialized + 5);
  paSampleFormatNotSupported = (paNotInitialized + 6);
  paBadIODeviceCombination = (paNotInitialized + 7);
  paInsufficientMemory = (paNotInitialized + 8);
  paBufferTooBig = (paNotInitialized + 9);
  paBufferTooSmall = (paNotInitialized + 10);
  paNullCallback = (paNotInitialized + 11);
  paBadStreamPtr = (paNotInitialized + 12);
  paTimedOut = (paNotInitialized + 13);
  paInternalError = (paNotInitialized + 14);
  paDeviceUnavailable = (paNotInitialized + 15);
  paIncompatibleHostApiSpecificStreamInfo = (paNotInitialized + 16);
  paStreamIsStopped = (paNotInitialized + 17);
  paStreamIsNotStopped = (paNotInitialized + 18);
  paInputOverflowed = (paNotInitialized + 19);
  paOutputUnderflowed = (paNotInitialized + 20);
  paHostApiNotFound = (paNotInitialized + 21);
  paInvalidHostApi = (paNotInitialized + 22);
  paCanNotReadFromACallbackStream = (paNotInitialized + 23);
  paCanNotWriteToACallbackStream = (paNotInitialized + 24);
  paCanNotReadFromAnOutputOnlyStream = (paNotInitialized + 25);
  paCanNotWriteToAnInputOnlyStream = (paNotInitialized + 26);
  paIncompatibleStreamHostApi = (paNotInitialized + 27);
  paBadBufferPtr = (paNotInitialized + 28);
  { enum_end PaErrorCode }

const
  paNoDevice = TPaDeviceIndex(-1);

const
  paUseHostApiSpecificDeviceSpecification = TPaDeviceIndex(-2);

const
  { enum_begin PaHostApiTypeId }
  paInDevelopment = 0; { * use while developing support for a new host API * }
  paDirectSound = 1;
  paMME = 2;
  paASIO = 3;
  paSoundManager = 4;
  paCoreAudio = 5;
  paOSS = 7;
  paALSA = 8;
  paAL = 9;
  paBeOS = 10;
  paWDMKS = 11;
  paJACK = 12;
  paWASAPI = 13;
  paAudioScienceHPI = 14;
  { enum_end PaHostApiTypeId }

const
  paFloat32 = TPaSampleFormat($00000001);
  paInt32 = TPaSampleFormat($00000002);
  paInt24 = TPaSampleFormat($00000004);
  paInt16 = TPaSampleFormat($00000008);
  paInt8 = TPaSampleFormat($00000010);
  paUInt8 = TPaSampleFormat($00000020);
  paCustomFormat = TPaSampleFormat($00010000);
  paNonInterleaved = TPaSampleFormat($80000000);

const
  paFormatIsSupported = 0;

const
  paFramesPerBufferUnspecified = 0;

const
  paNoFlag = TPaStreamFlags(0);
  paClipOff = TPaStreamFlags($00000001);
  paDitherOff = TPaStreamFlags($00000002);
  paNeverDropInput = TPaStreamFlags($00000004);
  paPrimeOutputBuffersUsingStreamCallback = TPaStreamFlags($00000008);
  paPlatformSpecificFlags = TPaStreamFlags($FFFF0000);

const
  paInputUnderflow = TPaStreamCallbackFlags($00000001);
  paInputOverflow = TPaStreamCallbackFlags($00000002);
  paOutputUnderflow = TPaStreamCallbackFlags($00000004);
  paOutputOverflow = TPaStreamCallbackFlags($00000008);
  paPrimingOutput = TPaStreamCallbackFlags($00000010);

const
  paContinue = 0;
  paComplete = 1;
  paAbort = 2;

implementation

end.
