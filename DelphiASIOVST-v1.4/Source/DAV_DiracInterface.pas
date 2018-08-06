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

unit DAV_DiracInterface;

interface

uses
  Windows, Classes, DAV_Types, DAV_Classes;

type
  PDiracHandle = THandle;

  TDiracProperties = (dpPitchFactor = 100, dpTimeFactor, dpFormantFactor,
    dpCompactSupport, dpCacheGranularity, dpDoPitchCorrection,
    dpPitchCorrectionBasicTuning, dpPitchCorrectionTolerance,
    dpPitchCorrectionSlur);

  TDiracLambda = (dlPreview = 200, dl1, dl2, dl3, dl4, dl5);
  TDiracQuality = (dqPreview = 300, dqGood, dqBetter, dqBest);

  TDiracLE = class(TDspObject)
  private
    FDiracHandle: PDiracHandle;
    FLambda: TDiracLambda;
    FQuality: TDiracQuality;
    FSampleRate: Single;
    FInternalData: PDAVSingleFixedArray;
    FIntDataPos: Integer;
    procedure SetLambda(const Value: TDiracLambda);
    procedure SetQuality(const Value: TDiracQuality);
    procedure SetSampleRate(const Value: Single);
    procedure CreateDiracHandle;
    function GetCacheGranularity: Integer;
    function GetCompactSupport: Boolean;
    function GetFormantFactor: Extended;
    function GetPitchCorrection: Boolean;
    function GetPitchCorrectionBasicTuning: Extended;
    function GetPitchCorrectionSlur: Extended;
    function GetPitchCorrectionTolerance: Extended;
    function GetPitchFactor: Extended;
    function GetTimeFactor: Extended;
    procedure SetCacheGranularity(const Value: Integer);
    procedure SetCompactSupport(const Value: Boolean);
    procedure SetFormantFactor(const Value: Extended);
    procedure SetPitchCorrection(const Value: Boolean);
    procedure SetPitchCorrectionBasicTuning(const Value: Extended);
    procedure SetPitchCorrectionSlur(const Value: Extended);
    procedure SetPitchCorrectionTolerance(const Value: Extended);
    procedure SetPitchFactor(const Value: Extended);
    procedure SetTimeFactor(const Value: Extended);
  protected
    procedure LambdaChanged; virtual;
    procedure QualityChanged; virtual;
    procedure SampleRateChanged; virtual;
    function ReadFromChannelsCallback(Data: PDAVSingleFixedArray;
      const FrameCount: Integer): Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    class function GetDiracVersion: string;
    procedure ProcessBuffer(const Input, Output: PDAVSingleFixedArray;
      const SampleFrames: Integer);
    property Lambda: TDiracLambda read FLambda write SetLambda;
    property Quality: TDiracQuality read FQuality write SetQuality;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property PitchFactor: Extended read GetPitchFactor write SetPitchFactor;
    property TimeFactor: Extended read GetTimeFactor write SetTimeFactor;
    property FormantFactor: Extended read GetFormantFactor
      write SetFormantFactor;
    property CompactSupport: Boolean read GetCompactSupport
      write SetCompactSupport;
    property CacheGranularity: Integer read GetCacheGranularity
      write SetCacheGranularity;
    property DoPitchCorrection: Boolean read GetPitchCorrection
      write SetPitchCorrection;
    property PitchCorrectionBasicTuning: Extended
      read GetPitchCorrectionBasicTuning write SetPitchCorrectionBasicTuning;
    property PitchCorrectionTolerance: Extended read GetPitchCorrectionTolerance
      write SetPitchCorrectionTolerance;
    property PitchCorrectionSlur: Extended read GetPitchCorrectionSlur
      write SetPitchCorrectionSlur;
    property Version: string read GetDiracVersion;
  end;

  // Prototypes
  PDiracreadFromChannelsCallback = ^TDiracreadFromChannelsCallback;
  TDiracreadFromChannelsCallback = function(Data: PDAVSingleFixedArray;
    FrameCount: Integer; UserData: Pointer): Integer; cdecl;

  TDiracVersion = function: PChar; cdecl;
  TDiracCreate = function(Lambda, Quality, ChannelCount: Integer;
    SampleRate: Single; ReadFromChannelsCallback
    : PDiracreadFromChannelsCallback): PDiracHandle; cdecl;
  TDiracSetProperty = function(Selector: Integer; Value: Double;
    DiracHandle: PDiracHandle): Integer; cdecl;
  TDiracGetProperty = function(Selector: Integer; DiracHandle: PDiracHandle)
    : Double; cdecl;
  TDiracReset = procedure(Clear: Boolean; DiracHandle: PDiracHandle); cdecl;
  TDiracProcess = function(AudioOut: PDAVSingleFixedArray; FrameCount: Integer;
    UserData: Pointer; DiracHandle: PDiracHandle): Integer; cdecl;
  TDiracDestroy = procedure(DiracHandle: PDiracHandle); cdecl;
  TDiracGetInputBufferSizeInFrames = function(DiracHandle: PDiracHandle)
    : Integer; cdecl;

var
  DiracVersion: TDiracVersion;
  DiracCreate: TDiracCreate;
  DiracSetProperty: TDiracSetProperty;
  DiracGetProperty: TDiracGetProperty;
  DiracReset: TDiracReset;
  DiracProcess: TDiracProcess;
  DiracDestroy: TDiracDestroy;
  DiracGetInputBufferSizeInFrames: TDiracGetInputBufferSizeInFrames;

implementation

uses
  SysUtils, DAV_DLLLoader;

function ReadFromChannelsStaticCallback(Data: PDAVSingleFixedArray;
  FrameCount: Integer; UserData: Pointer): Integer; cdecl;
begin
  result := TDiracLE(UserData).ReadFromChannelsCallback(Data, FrameCount);
end;

{ TDiracLE }

constructor TDiracLE.Create;
begin
  inherited;
  FLambda := dlPreview;
  FQuality := dqPreview;
  FSampleRate := 44100;
  CreateDiracHandle;
end;

procedure TDiracLE.CreateDiracHandle;
var
  OldHandle: THandle;
begin
  OldHandle := FDiracHandle;
  FDiracHandle := DiracCreate(Integer(FLambda), Integer(FQuality), 1,
    FSampleRate, @ReadFromChannelsStaticCallback);
  if OldHandle <> 0 then
    DiracDestroy(OldHandle);
end;

destructor TDiracLE.Destroy;
begin
  DiracDestroy(FDiracHandle);
  inherited;
end;

function TDiracLE.GetCacheGranularity: Integer;
begin
  result := round(DiracGetProperty(Integer(dpCacheGranularity), FDiracHandle));
end;

function TDiracLE.GetCompactSupport: Boolean;
begin
  result := DiracGetProperty(Integer(dpCompactSupport), FDiracHandle) > 0.5;
end;

class function TDiracLE.GetDiracVersion: string;
var
  Vers: PChar;
begin
  Vers := DiracVersion;
  result := StrPas(Vers)
end;

function TDiracLE.GetFormantFactor: Extended;
begin
  result := DiracGetProperty(Integer(dpFormantFactor), FDiracHandle);
end;

function TDiracLE.GetPitchCorrection: Boolean;
begin
  result := DiracGetProperty(Integer(dpDoPitchCorrection), FDiracHandle) > 0.5;
end;

function TDiracLE.GetPitchCorrectionBasicTuning: Extended;
begin
  result := DiracGetProperty(Integer(dpPitchCorrectionBasicTuning),
    FDiracHandle);
end;

function TDiracLE.GetPitchCorrectionSlur: Extended;
begin
  result := DiracGetProperty(Integer(dpPitchCorrectionSlur), FDiracHandle);
end;

function TDiracLE.GetPitchCorrectionTolerance: Extended;
begin
  result := DiracGetProperty(Integer(dpPitchCorrectionTolerance), FDiracHandle);
end;

function TDiracLE.GetPitchFactor: Extended;
begin
  result := DiracGetProperty(Integer(dpPitchFactor), FDiracHandle);
end;

function TDiracLE.GetTimeFactor: Extended;
begin
  result := DiracGetProperty(Integer(dpTimeFactor), FDiracHandle);
end;

procedure TDiracLE.SetCacheGranularity(const Value: Integer);
begin
  DiracSetProperty(Integer(dpCacheGranularity), Value, FDiracHandle);
end;

procedure TDiracLE.SetCompactSupport(const Value: Boolean);
begin
  DiracSetProperty(Integer(dpCompactSupport), Integer(Value), FDiracHandle);
end;

procedure TDiracLE.SetFormantFactor(const Value: Extended);
begin
  DiracSetProperty(Integer(dpFormantFactor), Value, FDiracHandle);
end;

procedure TDiracLE.SetLambda(const Value: TDiracLambda);
begin
  if Lambda <> Value then
  begin
    FLambda := Value;
    LambdaChanged;
  end;
end;

procedure TDiracLE.SetPitchCorrection(const Value: Boolean);
begin
  DiracSetProperty(Integer(dpDoPitchCorrection), Integer(Value), FDiracHandle);
end;

procedure TDiracLE.SetPitchCorrectionBasicTuning(const Value: Extended);
begin
  DiracSetProperty(Integer(dpPitchCorrectionBasicTuning), Value, FDiracHandle);
end;

procedure TDiracLE.SetPitchCorrectionSlur(const Value: Extended);
begin
  DiracSetProperty(Integer(dpPitchCorrectionSlur), Value, FDiracHandle);
end;

procedure TDiracLE.SetPitchCorrectionTolerance(const Value: Extended);
begin
  DiracSetProperty(Integer(dpPitchCorrectionTolerance), Value, FDiracHandle);
end;

procedure TDiracLE.SetPitchFactor(const Value: Extended);
begin
  DiracSetProperty(Integer(dpPitchFactor), Value, FDiracHandle);
end;

procedure TDiracLE.SetQuality(const Value: TDiracQuality);
begin
  if Quality <> Value then
  begin
    FQuality := Value;
    QualityChanged;
  end;
end;

procedure TDiracLE.SetSampleRate(const Value: Single);
begin
  if SampleRate <> Value then
  begin
    FSampleRate := Value;
    SampleRateChanged;
  end;
end;

procedure TDiracLE.SetTimeFactor(const Value: Extended);
begin
  DiracSetProperty(Integer(dpTimeFactor), Value, FDiracHandle);
end;

procedure TDiracLE.LambdaChanged;
begin
  CreateDiracHandle;
end;

procedure TDiracLE.QualityChanged;
begin
  CreateDiracHandle;
end;

procedure TDiracLE.SampleRateChanged;
begin
  CreateDiracHandle;
end;

procedure TDiracLE.ProcessBuffer(const Input, Output: PDAVSingleFixedArray;
  const SampleFrames: Integer);
var
  SampleCount: Integer;
begin
  SampleCount := 0;
  FIntDataPos := 0;
  FInternalData := Input;
  while SampleCount < SampleFrames do
    SampleCount := SampleCount + DiracProcess(Output, SampleFrames, Self,
      FDiracHandle);
end;

function TDiracLE.ReadFromChannelsCallback(Data: PDAVSingleFixedArray;
  const FrameCount: Integer): Integer;
begin
  move(FInternalData^[FIntDataPos], Data^, FrameCount * SizeOf(Single));
  FIntDataPos := FIntDataPos + FrameCount;
  result := FrameCount;
end;

const
  CDiracLibName = 'DiracLE.DLL';

var
  GLibDiracHandle: HMODULE;
  GLibDiracLoaded: Boolean;

procedure LoadDiracLib;
begin
  try
    GLibDiracHandle := LoadLibraryEx(CDiracLibName, 0, 0);
    if GLibDiracHandle <> 0 then
    begin
      GLibDiracLoaded := True;
      DiracCreate := GetProcAddress(GLibDiracHandle, MakeIntResource(1));
      DiracDestroy := GetProcAddress(GLibDiracHandle, MakeIntResource(3));
      DiracGetInputBufferSizeInFrames := GetProcAddress(GLibDiracHandle,
        MakeIntResource(4));
      DiracGetProperty := GetProcAddress(GLibDiracHandle, MakeIntResource(5));
      DiracProcess := GetProcAddress(GLibDiracHandle, MakeIntResource(6));
      DiracReset := GetProcAddress(GLibDiracHandle, MakeIntResource(8));
      DiracSetProperty := GetProcAddress(GLibDiracHandle, MakeIntResource(9));
      DiracVersion := GetProcAddress(GLibDiracHandle, MakeIntResource(10));
    end;
  except
    raise Exception.CreateFmt('Error loading library %s!', [CDiracLibName]);
  end;
end;

procedure UnloadDiracLib;
begin
  if GLibDiracLoaded then
  begin
    FreeLibrary(GLibDiracHandle);
    GLibDiracLoaded := False;
  end;
end;

initialization

LoadDiracLib;

finalization

UnloadDiracLib;

end.
