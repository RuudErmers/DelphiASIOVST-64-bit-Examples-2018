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

unit DAV_CudaFft;

interface

{$I DAV_Compiler.inc}
{ -$DEFINE Emulation }

uses
{$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} SysUtils, DAV_Complex;

type
  TCudaFftResult = (cfrSuccess = 0, cfrInvalidPlan = 1, cfrAllocFailed = 2,
    cfrInvalidType = 3, cfrInvalidValue = 4, cfrInternalError = 5,
    cfrExecutionFailed = 6, cfrSetupFailed = 7, cfrInvalidSize = 8);

  // CudaFft defines and supports the following data types:

  // TCudaFftHandle is a handle type used to store and access CudaFft plans.
  TCudaFftHandle = THandle;

  // TCudaFftReal is a single-precision, floating-point real data type.
  TCudaFftReal = Single;

  // TCudaFftComplex is a single-precision, floating-point complex data type that
  // consists of interleaved real and imaginary components.
  TCudaFftComplex = TComplex32;

  // CudaFft supports the following transform types
  TCudaFftType = (CCudaFftR2C = $2A, CCudaFftC2R = $2C, CCudaFftC2C = $29);

  // Function Prototypes
  TCudaFftPlan1d = function(var Plan: TCudaFftHandle; FFTSize: Integer;
    FftType: TCudaFftType; Batch: Integer): TCudaFftResult; stdcall;

  TCudaFftPlan2d = function(var Plan: TCudaFftHandle;
    FFTSizeX, FFTSizeY: Integer; FftType: TCudaFftType)
    : TCudaFftResult; stdcall;

  TCudaFftPlan3d = function(var Plan: TCudaFftHandle;
    FFTSizeX, FFTSizeY, FFTSizeZ: Integer; FftType: TCudaFftType)
    : TCudaFftResult; stdcall;

  TCudaFftDestroy = function(Plan: TCudaFftHandle): TCudaFftResult; stdcall;

  TCudaFftExecC2C = function(Plan: TCudaFftHandle;
    InputData, OutputData: Pointer; Direction: Integer)
    : TCudaFftResult; stdcall;

  TCudaFftExecR2C = function(Plan: TCudaFftHandle;
    InputData, OutputData: Pointer): TCudaFftResult; stdcall;

  TCudaFftExecC2R = function(Plan: THandle; InputData, OutputData: Pointer)
    : TCudaFftResult; stdcall;

const
  // CudaFft transform directions
  CCudaFftForward = -1; // Forward FFT
  CCudaFftInverse = 1; // Inverse FFT

  // CudaFft API function return values
  CCudaFftResultStrings: array [TCudaFftResult] of String = ('Success',
    'Invalid Plan', 'Alloc Failed', 'Invalid Type', 'Invalid Value',
    'Internal Error', 'Execution Failed', 'Setup Failed', 'Invalid Size');

var
  CudaFftPlan1d: TCudaFftPlan1d;
  CudaFftPlan2d: TCudaFftPlan2d;
  CudaFftPlan3d: TCudaFftPlan3d;
  CudaFftDestroy: TCudaFftDestroy;
  CudaFftExecC2C: TCudaFftExecC2C;
  CudaFftExecR2C: TCudaFftExecR2C;
  CudaFftExecC2R: TCudaFftExecC2R;

implementation

var
  CudaFftDLL: HMODULE;
  CudaFftLoaded: Boolean;

const
{$IFDEF Emulation}
  CCudaFFTDLLName = 'CuFftEmu.dll';
{$ELSE}
  CCudaFFTDLLName = 'CuFft.dll';
{$ENDIF}

procedure InitializeCudaFftLibrary;
begin
  CudaFftDLL := LoadLibraryEx(CCudaFFTDLLName, 0, 0);
  if CudaFftDLL = 0 then
    raise Exception.CreateFmt('File: %s could not be found', [CCudaFFTDLLName])
  else
  begin
    CudaFftLoaded := True;
    CudaFftPlan1d := GetProcAddress(CudaFftDLL, 'cufftPlan1d');
    CudaFftPlan2d := GetProcAddress(CudaFftDLL, 'cufftPlan2d');
    CudaFftPlan3d := GetProcAddress(CudaFftDLL, 'cufftPlan3d');
    CudaFftDestroy := GetProcAddress(CudaFftDLL, 'cufftDestroy');
    CudaFftExecC2C := GetProcAddress(CudaFftDLL, 'cufftExecC2C');
    CudaFftExecR2C := GetProcAddress(CudaFftDLL, 'cufftExecR2C');
    CudaFftExecC2R := GetProcAddress(CudaFftDLL, 'cufftExecC2R');
  end;
end;

procedure FinalizeCudaFftLibrary;
begin
  FreeLibrary(CudaFftDLL);
  CudaFftLoaded := False;
end;

initialization

InitializeCudaFftLibrary;

finalization

FinalizeCudaFftLibrary;

end.
