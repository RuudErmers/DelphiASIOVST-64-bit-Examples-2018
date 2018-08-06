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

unit DAV_CudaRuntime;

interface

{$I DAV_Compiler.inc}

uses
{$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} SysUtils, DAV_Complex;

type
  TCudaError = (ceSuccess = 0, ceErrorMissingConfiguration, ceMemoryAllocation,
    ceInitializationError, ceLaunchFailure, cePriorLaunchFailure,
    ceLaunchTimeout, ceLaunchOutOfResources, ceInvalidDeviceFunction,
    ceInvalidConfiguration, ceInvalidDevice, ceInvalidValue,
    ceInvalidPitchValue, ceInvalidSymbol, ceMapBufferObjectFailed,
    ceUnmapBufferObjectFailed, ceInvalidHostPointer, ceInvalidDevicePointer,
    ceInvalidTexture, ceInvalidTextureBinding, ceInvalidChannelDescriptor,
    ceInvalidMemcpyDirection, ceAddressOfConstant, ceTextureFetchFailed,
    ceTextureNotBound, ceSynchronizationError, ceInvalidFilterSetting,
    ceInvalidNormSetting, ceMixedDeviceExecution, ceCudartUnloading, ceUnknown,
    ceNotYetImplemented, ceMemoryValueTooLarge, ceInvalidResourceHandle,
    ceNotReady, ceStartupFailure = $7F, ceApiFailureBase = 10000);

  TCudaStream = Cardinal;
  TCudaEvent = Cardinal;

  TCudaChannelFormatKind = (ccfkSigned, ccfkUnsigned, ccfkFloat);

  PCudaChannelFormatDesc = ^TCudaChannelFormatDesc;

  TCudaChannelFormatDesc = record
    X: Integer;
    Y: Integer;
    Z: Integer;
    W: Integer;
    Format: TCudaChannelFormatKind;
  end;

  PCudaArray = Pointer;

  TCudaMemcpyKind = (cmkHostToHost = 0, cmkHostToDevice, cmkDeviceToHost,
    cmkDeviceToDevice);

  PCudaPitchedPtr = ^TCudaPitchedPtr;

  TCudaPitchedPtr = record
    Ptr: Pointer;
    Pitch: Cardinal;
    XSize: Cardinal;
    YSize: Cardinal;
  end;

  TCudaExtent = record
    Width: Cardinal;
    Height: Cardinal;
    Depth: Cardinal;
  end;

  TCudaPos = record
    X: Cardinal;
    Y: Cardinal;
    Z: Cardinal;
  end;

  PCudaMemcpy3DParms = ^TCudaMemcpy3DParms;

  TCudaMemcpy3DParms = record
    SourceArray: Pointer;
    SourcePos: TCudaPos;
    SourcePtr: TCudaPitchedPtr;
    DestArray: Pointer;
    DestPos: TCudaPos;
    DestPtr: TCudaPitchedPtr;
    Extent: TCudaExtent;
    Kind: TCudaMemcpyKind;
  end;

  PCudaDeviceProp = ^TCudaDeviceProp;

  TCudaDeviceProp = record
    Name: array [0 .. 256 - 1] of Char;
    TotalGlobalMem: Cardinal;
    SharedMemPerBlock: Cardinal;
    RegsPerBlock: Integer;
    WarpSize: Integer;
    MemPitch: Cardinal;
    MaxThreadsPerBlock: Integer;
    MaxThreadsDim: array [0 .. 3 - 1] of Integer;
    MaxGridSize: array [0 .. 3 - 1] of Integer;
    ClockRate: Integer;
    TotalConstMem: Cardinal;
    Major: Integer;
    Minor: Integer;
    TextureAlignment: Cardinal;
    DeviceOverlap: Integer;
    MultiProcessorCount: Integer;
    __CudaReserved: array [0 .. 40 - 1] of Integer;
  end;

  /// //////////////////////////////////////
  // Allocate Memory Function Prototypes //
  /// //////////////////////////////////////

  TCudaMalloc = function(var GpuMemoryPtr: Pointer; Size: Cardinal)
    : TCudaError; stdcall;
  TCudaMallocHost = function(var Ptr: Pointer; Size: Cardinal)
    : TCudaError; stdcall;
  TCudaMallocPitch = function(var GpuMemoryPtr: Pointer; var Pitch: Cardinal;
    Width, Height: Cardinal): TCudaError; stdcall;
  TCudaMallocArray = function(var aarray: Pointer;
    var desc: TCudaChannelFormatDesc; Width, Height: Cardinal)
    : TCudaError; stdcall;
  TCudaFree = function(GpuMemoryPtr: Pointer): TCudaError; stdcall;
  TCudaFreeHost = function(Ptr: Pointer): TCudaError; stdcall;
  TCudaFreeArray = function(const aarray: Pointer): TCudaError; stdcall;

  /// /////////////////////////////////////////
  // Allocate 3D Memory Function Prototypes //
  /// /////////////////////////////////////////

  TCudaMalloc3D = function(PitchDevPtr: PCudaPitchedPtr; Extent: TCudaExtent)
    : TCudaError; stdcall;
  TCudaMalloc3DArray = function(var arrayPtr: PCudaArray;
    const desc: PCudaChannelFormatDesc; Extent: TCudaExtent)
    : TCudaError; stdcall;
  TCudaMemset3D = function(PitchDevPtr: TCudaPitchedPtr; Value: Integer;
    Extent: TCudaExtent): TCudaError; stdcall;
  TCudaMemcpy3D = function(const p: PCudaMemcpy3DParms): TCudaError; stdcall;
  TCudaMemcpy3DAsync = function(const p: PCudaMemcpy3DParms;
    Stream: TCudaStream): TCudaError; stdcall;

  /// //////////////////////////////
  // MemCopy Function Prototypes //
  /// //////////////////////////////

  TCudaMemcpy = function(Dest: Pointer; const Source: Pointer; Count: Cardinal;
    Kind: TCudaMemcpyKind): TCudaError; stdcall;
  TCudaMemcpyToArray = function(Dest: PCudaArray; WOffset, HOffset: Cardinal;
    const Source: Pointer; Count: Cardinal; Kind: TCudaMemcpyKind)
    : TCudaError; stdcall;
  TCudaMemcpyFromArray = function(Dest: Pointer; const Source: PCudaArray;
    WOffset, HOffset: Cardinal; Count: Cardinal; Kind: TCudaMemcpyKind)
    : TCudaError; stdcall;
  TCudaMemcpy2D = function(Dest: Pointer; DPitch: Cardinal;
    const Source: Pointer; SPitch: Cardinal; Width, Height: Cardinal;
    Kind: TCudaMemcpyKind): TCudaError; stdcall;
  TCudaMemcpy2DToArray = function(Dest: PCudaArray; WOffset, HOffset: Cardinal;
    const Source: Pointer; SPitch: Cardinal; Width, Height: Cardinal;
    Kind: TCudaMemcpyKind): TCudaError; stdcall;
  TCudaMemcpy2DFromArray = function(Dest: Pointer; DeltaPitch: Cardinal;
    const Source: PCudaArray; WOffset, HOffset: Cardinal;
    Width, Height: Cardinal; Kind: TCudaMemcpyKind): TCudaError; stdcall;

  /// ////////////////////////////////
  // MemCopyEx Function Prototypes //
  /// ////////////////////////////////

  TCudaMemcpyAsync = function(Dest: Pointer; const Source: Pointer;
    Count: Cardinal; Kind: TCudaMemcpyKind; Stream: TCudaStream)
    : TCudaError; stdcall;
  TCudaMemcpyToArrayAsync = function(Dest: PCudaArray;
    WOffset, HOffset: Cardinal; const Source: Pointer; Count: Cardinal;
    Kind: TCudaMemcpyKind; Stream: TCudaStream): TCudaError; stdcall;
  TCudaMemcpyFromArrayAsync = function(Dest: Pointer; const Source: PCudaArray;
    WOffset, HOffset: Cardinal; Count: Cardinal; Kind: TCudaMemcpyKind;
    Stream: TCudaStream): TCudaError; stdcall;
  TCudaMemcpy2DAsync = function(Dest: Pointer; DeltaPitch: Cardinal;
    const Source: Pointer; SPitch: Cardinal; Width, Height: Cardinal;
    Kind: TCudaMemcpyKind; Stream: TCudaStream): TCudaError; stdcall;
  TCudaMemcpy2DToArrayAsync = function(Dest: PCudaArray;
    WOffset, HOffset: Cardinal; const Source: Pointer; SPitch: Cardinal;
    Width, Height: Cardinal; Kind: TCudaMemcpyKind; Stream: TCudaStream)
    : TCudaError; stdcall;
  TCudaMemcpy2DFromArrayAsync = function(Dest: Pointer; DPitch: Cardinal;
    const Source: PCudaArray; WOffset, HOffset: Cardinal;
    Width, Height: Cardinal; Kind: TCudaMemcpyKind; Stream: TCudaStream)
    : TCudaError; stdcall;
  TCudaMemcpyToSymbolAsync = function(const Symbol: Pchar;
    const Source: Pointer; Count: Cardinal; Offset: Cardinal;
    Kind: TCudaMemcpyKind; Stream: TCudaStream): TCudaError; stdcall;
  TCudaMemcpyFromSymbolAsync = function(Dest: Pointer; const Symbol: Pchar;
    Count: Cardinal; Offset: Cardinal; Kind: TCudaMemcpyKind;
    Stream: TCudaStream): TCudaError; stdcall;

  /// /////////////////////////////
  // MemSet Function Prototypes //
  /// /////////////////////////////

  TCudaMemset = function(GpuMemoryPtr: Pointer; Value: Integer; Count: Cardinal)
    : TCudaError; stdcall;
  TCudaMemset2D = function(GpuMemoryPtr: Pointer; Pitch: Cardinal;
    Value: Integer; Width, Height: Cardinal): TCudaError; stdcall;

  /// /////////////////////////////
  // Symbol Function Prototypes //
  /// /////////////////////////////

  TCudaGetSymbolAddress = function(var GpuMemoryPtr: Pointer;
    const Symbol: Pchar): TCudaError; stdcall;
  TCudaGetSymbolSize = function(var Size: Cardinal; const Symbol: Pchar)
    : TCudaError; stdcall;

  /// //////////////////////////////////////
  // Device Handling Function Prototypes //
  /// //////////////////////////////////////

  TCudaGetDeviceCount = function(var Count: Integer): TCudaError; stdcall;

  TCudaGetDeviceProperties = function(var Prop: TCudaDeviceProp;
    Device: Integer): TCudaError; stdcall;

  TCudaChooseDevice = function(var Device: Integer; const Prop: PCudaDeviceProp)
    : TCudaError; stdcall;

  TCudaSetDevice = function(Device: Integer): TCudaError; stdcall;

  TCudaGetDevice = function(var Device: Integer): TCudaError; stdcall;

  TCudaGetChannelDesc = function(var desc: TCudaChannelFormatDesc;
    const aarray: PCudaArray): TCudaError; stdcall;
  TCudaCreateChannelDesc = function(X, Y, Z, W: Integer;
    Format: TCudaChannelFormatKind): TCudaError; stdcall;

  TCudaGetLastError = function: TCudaError; stdcall;
  TCudaGetErrorString = function(Error: TCudaError): TCudaError; stdcall;

  // TCudaConfigureCall = function (dim3 gridDim, dim3 blockDim, Cardinal sharedMem __dv(0), Stream TCudaStream __dv(0)): TCudaError; stdcall;
  TCudaSetupArgument = function(const Arg: Pointer; Size: Cardinal;
    Offset: Cardinal): TCudaError; stdcall;
  TCudaLaunch = function(const Symbol: Pchar): TCudaError; stdcall;

  TCudaStreamCreate = function(var Stream: TCudaStream): TCudaError; stdcall;
  TCudaStreamDestroy = function(Stream: TCudaStream): TCudaError; stdcall;
  TCudaStreamSynchronize = function(Stream: TCudaStream): TCudaError; stdcall;
  TCudaStreamQuery = function(Stream: TCudaStream): TCudaError; stdcall;

  TCudaEventCreate = function(var Event: TCudaEvent): TCudaError; stdcall;
  TCudaEventRecord = function(Event: TCudaEvent; Stream: TCudaStream)
    : TCudaError; stdcall;
  TCudaEventQuery = function(Event: TCudaEvent): TCudaError; stdcall;
  TCudaEventSynchronize = function(Event: TCudaEvent): TCudaError; stdcall;
  TCudaEventDestroy = function(Event: TCudaEvent): TCudaError; stdcall;
  TCudaEventElapsedTime = function(var ms: Single; AStart, AEnd: TCudaEvent)
    : TCudaError; stdcall;

  TCudaSetDoubleForDevice = function(var d: Double): TCudaError; stdcall;
  TCudaSetDoubleForHost = function(var d: Double): TCudaError; stdcall;

  TCudaThreadExit = function: TCudaError; stdcall;
  TCudaThreadSynchronize = function: TCudaError; stdcall;

const
  CCudaErrorStrings: array [0 .. 36] of string = ('Success',
    'ErrorMissingConfiguration', 'ErrorMemoryAllocation',
    'ErrorInitializationError', 'ErrorLaunchFailure', 'ErrorPriorLaunchFailure',
    'ErrorLaunchTimeout', 'ErrorLaunchOutOfResources',
    'ErrorInvalidDeviceFunction', 'ErrorInvalidConfiguration',
    'ErrorInvalidDevice', 'ErrorInvalidValue', 'ErrorInvalidPitchValue',
    'ErrorInvalidSymbol', 'ErrorMapBufferObjectFailed',
    'ErrorUnmapBufferObjectFailed', 'ErrorInvalidHostPointer',
    'ErrorInvalidDevicePointer', 'ErrorInvalidTexture',
    'ErrorInvalidTextureBinding', 'ErrorInvalidChannelDescriptor',
    'ErrorInvalidMemcpyDirection', 'ErrorAddressOfConstant',
    'ErrorTextureFetchFailed', 'ErrorTextureNotBound',
    'ErrorSynchronizationError', 'ErrorInvalidFilterSetting',
    'ErrorInvalidNormSetting', 'ErrorMixedDeviceExecution',
    'ErrorCudartUnloading', 'ErrorUnknown', 'ErrorNotYetImplemented',
    'ErrorMemoryValueTooLarge', 'ErrorInvalidResourceHandle', 'ErrorNotReady',
    'ErrorStartupFailure', 'ErrorApiFailureBase');

var
  CudaMalloc: TCudaMalloc;
  CudaMallocHost: TCudaMallocHost;
  CudaMallocPitch: TCudaMallocPitch;
  CudaMallocArray: TCudaMallocArray;
  CudaFree: TCudaFree;
  CudaFreeHost: TCudaFreeHost;
  CudaFreeArray: TCudaFreeArray;
  CudaMalloc3D: TCudaMalloc3D;
  CudaMalloc3DArray: TCudaMalloc3DArray;
  CudaMemset3D: TCudaMemset3D;
  CudaMemcpy3D: TCudaMemcpy3D;
  CudaMemcpy3DAsync: TCudaMemcpy3DAsync;
  CudaMemcpy: TCudaMemcpy;
  CudaMemcpyToArray: TCudaMemcpyToArray;
  CudaMemcpyFromArray: TCudaMemcpyFromArray;
  CudaMemcpy2D: TCudaMemcpy2D;
  CudaMemcpyAsync: TCudaMemcpyAsync;
  CudaMemcpyToArrayAsync: TCudaMemcpyToArrayAsync;
  CudaMemcpyFromArrayAsync: TCudaMemcpyFromArrayAsync;
  CudaMemcpy2DAsync: TCudaMemcpy2DAsync;
  CudaMemcpy2DToArrayAsync: TCudaMemcpy2DToArrayAsync;
  CudaMemcpy2DFromArrayAsync: TCudaMemcpy2DFromArrayAsync;
  CudaMemcpyToSymbolAsync: TCudaMemcpyToSymbolAsync;
  CudaMemcpyFromSymbolAsync: TCudaMemcpyFromSymbolAsync;
  CudaMemset: TCudaMemset;
  CudaMemset2D: TCudaMemset2D;
  CudaGetSymbolAddress: TCudaGetSymbolAddress;
  CudaGetSymbolSize: TCudaGetSymbolSize;
  CudaGetChannelDesc: TCudaGetChannelDesc;
  CudaCreateChannelDesc: TCudaCreateChannelDesc;
  CudaGetDeviceCount: TCudaGetDeviceCount;
  CudaGetDeviceProperties: TCudaGetDeviceProperties;
  CudaChooseDevice: TCudaChooseDevice;
  CudaSetDevice: TCudaSetDevice;
  CudaGetDevice: TCudaGetDevice;
  CudaGetLastError: TCudaGetLastError;
  CudaGetErrorString: TCudaGetErrorString;
  CudaSetupArgument: TCudaSetupArgument;
  CudaLaunch: TCudaLaunch;
  CudaStreamCreate: TCudaStreamCreate;
  CudaStreamDestroy: TCudaStreamDestroy;
  CudaStreamSynchronize: TCudaStreamSynchronize;
  CudaStreamQuery: TCudaStreamQuery;
  CudaEventCreate: TCudaEventCreate;
  CudaEventRecord: TCudaEventRecord;
  CudaEventQuery: TCudaEventQuery;
  CudaEventSynchronize: TCudaEventSynchronize;
  CudaEventDestroy: TCudaEventDestroy;
  CudaEventElapsedTime: TCudaEventElapsedTime;
  CudaSetDoubleForDevice: TCudaSetDoubleForDevice;
  CudaSetDoubleForHost: TCudaSetDoubleForHost;
  CudaThreadExit: TCudaThreadExit;
  CudaThreadSynchronize: TCudaThreadSynchronize;

function CudaErrorToString(CudaError: TCudaError): string;

var
  CudaRuntimeLoaded: Boolean;

implementation

function CudaErrorToString(CudaError: TCudaError): string;
begin
  case CudaError of
    ceSuccess .. ceNotReady:
      result := CCudaErrorStrings[Integer(CudaError)];
    ceStartupFailure:
      result := CCudaErrorStrings[35];
    ceApiFailureBase:
      result := CCudaErrorStrings[36];
  end;
end;

var
  CudaRuntimeDLL: HMODULE;

const
  CCudaRuntimeDLLName = 'CudaRt.dll';

procedure InitializeCudaRuntimeLibrary;
begin
  CudaRuntimeDLL := LoadLibraryEx(CCudaRuntimeDLLName, 0, 0);
  if CudaRuntimeDLL = 0 then
    raise Exception.CreateFmt('File: %s could not be found',
      [CCudaRuntimeDLLName])
  else
  begin
    CudaRuntimeLoaded := True;
    CudaMalloc := GetProcAddress(CudaRuntimeDLL, 'cudaMalloc');
    CudaMallocHost := GetProcAddress(CudaRuntimeDLL, 'cudaMallocHost');
    CudaMallocPitch := GetProcAddress(CudaRuntimeDLL, 'cudaMallocPitch');
    CudaMallocArray := GetProcAddress(CudaRuntimeDLL, 'cudaMallocArray');
    CudaFree := GetProcAddress(CudaRuntimeDLL, 'cudaFree');
    CudaFreeHost := GetProcAddress(CudaRuntimeDLL, 'cudaFreeHost');
    CudaFreeArray := GetProcAddress(CudaRuntimeDLL, 'cudaFreeArray');
    CudaMalloc3D := GetProcAddress(CudaRuntimeDLL, 'cudaMalloc3D');
    CudaMalloc3DArray := GetProcAddress(CudaRuntimeDLL, 'cudaMalloc3DArray');
    CudaMemset3D := GetProcAddress(CudaRuntimeDLL, 'cudaMemset3D');
    CudaMemcpy3D := GetProcAddress(CudaRuntimeDLL, 'cudaMemcpy3D');
    CudaMemcpy3DAsync := GetProcAddress(CudaRuntimeDLL, 'cudaMemcpy3DAsync');
    CudaMemcpy := GetProcAddress(CudaRuntimeDLL, 'cudaMemcpy');
    CudaMemcpyToArray := GetProcAddress(CudaRuntimeDLL, 'cudaMemcpyToArray');
    CudaMemcpyFromArray := GetProcAddress(CudaRuntimeDLL,
      'cudaMemcpyFromArray');
    CudaMemcpy2D := GetProcAddress(CudaRuntimeDLL, 'cudaMemcpy2D');
    CudaMemcpyAsync := GetProcAddress(CudaRuntimeDLL, 'cudaMemcpyAsync');
    CudaMemcpyToArrayAsync := GetProcAddress(CudaRuntimeDLL,
      'cudaMemcpyToArrayAsync');
    CudaMemcpyFromArrayAsync := GetProcAddress(CudaRuntimeDLL,
      'cudaMemcpyFromArrayAsync');
    CudaMemcpy2DAsync := GetProcAddress(CudaRuntimeDLL, 'cudaMemcpy2DAsync');
    CudaMemcpy2DToArrayAsync := GetProcAddress(CudaRuntimeDLL,
      'cudaMemcpy2DToArrayAsync');
    CudaMemcpy2DFromArrayAsync := GetProcAddress(CudaRuntimeDLL,
      'cudaMemcpy2DFromArrayAsync');
    CudaMemcpyToSymbolAsync := GetProcAddress(CudaRuntimeDLL,
      'cudaMemcpyToSymbolAsync');
    CudaMemcpyFromSymbolAsync := GetProcAddress(CudaRuntimeDLL,
      'cudaMemcpyFromSymbolAsync');
    CudaMemset := GetProcAddress(CudaRuntimeDLL, 'cudaMemset');
    CudaMemset2D := GetProcAddress(CudaRuntimeDLL, 'cudaMemset2D');
    CudaGetSymbolAddress := GetProcAddress(CudaRuntimeDLL,
      'cudaGetSymbolAddress');
    CudaGetSymbolSize := GetProcAddress(CudaRuntimeDLL, 'cudaGetSymbolSize');
    CudaGetChannelDesc := GetProcAddress(CudaRuntimeDLL, 'cudaGetChannelDesc');
    CudaCreateChannelDesc := GetProcAddress(CudaRuntimeDLL,
      'cudaCreateChannelDesc');
    CudaGetDeviceCount := GetProcAddress(CudaRuntimeDLL, 'cudaGetDeviceCount');
    CudaGetDeviceProperties := GetProcAddress(CudaRuntimeDLL,
      'cudaGetDeviceProperties');
    CudaChooseDevice := GetProcAddress(CudaRuntimeDLL, 'cudaChooseDevice');
    CudaSetDevice := GetProcAddress(CudaRuntimeDLL, 'cudaSetDevice');
    CudaGetDevice := GetProcAddress(CudaRuntimeDLL, 'cudaGetDevice');
    CudaGetLastError := GetProcAddress(CudaRuntimeDLL, 'cudaGetLastError');
    CudaGetErrorString := GetProcAddress(CudaRuntimeDLL, 'cudaGetErrorString');
    CudaSetupArgument := GetProcAddress(CudaRuntimeDLL, 'cudaSetupArgument');
    CudaLaunch := GetProcAddress(CudaRuntimeDLL, 'cudaLaunch');
    CudaStreamCreate := GetProcAddress(CudaRuntimeDLL, 'cudaStreamCreate');
    CudaStreamDestroy := GetProcAddress(CudaRuntimeDLL, 'cudaStreamDestroy');
    CudaStreamSynchronize := GetProcAddress(CudaRuntimeDLL,
      'cudaStreamSynchronize');
    CudaStreamQuery := GetProcAddress(CudaRuntimeDLL, 'cudaStreamQuery');
    CudaEventCreate := GetProcAddress(CudaRuntimeDLL, 'cudaEventCreate');
    CudaEventRecord := GetProcAddress(CudaRuntimeDLL, 'cudaEventRecord');
    CudaEventQuery := GetProcAddress(CudaRuntimeDLL, 'cudaEventQuery');
    CudaEventSynchronize := GetProcAddress(CudaRuntimeDLL,
      'cudaEventSynchronize');
    CudaEventDestroy := GetProcAddress(CudaRuntimeDLL, 'cudaEventDestroy');
    CudaEventElapsedTime := GetProcAddress(CudaRuntimeDLL,
      'cudaEventElapsedTime');
    CudaSetDoubleForDevice := GetProcAddress(CudaRuntimeDLL,
      'cudaSetDoubleForDevice');
    CudaSetDoubleForHost := GetProcAddress(CudaRuntimeDLL,
      'cudaSetDoubleForHost');
    CudaThreadExit := GetProcAddress(CudaRuntimeDLL, 'cudaThreadExit');
    CudaThreadSynchronize := GetProcAddress(CudaRuntimeDLL,
      'cudaThreadSynchronize');
  end;
end;

procedure FinalizeCudaRuntimeLibrary;
begin
  FreeLibrary(CudaRuntimeDLL);
  CudaRuntimeLoaded := False;
end;

initialization

InitializeCudaRuntimeLibrary;

finalization

FinalizeCudaRuntimeLibrary;

end.
