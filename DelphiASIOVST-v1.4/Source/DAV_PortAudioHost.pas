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

unit DAV_PortAudioHost;

interface

{$I DAV_Compiler.inc}

uses
  SysUtils, Classes, DAV_Types, {$IFDEF MSWindows} DAV_PortAudioBinding,
  {$ENDIF} {$IFDEF MacOS} DAV_PortAudioBindingStatic, {$ENDIF}
  DAV_PortAudioTypes;

type
  EPortAudio = class(Exception);
  TPortAudioStreamCallbackEvent = function(Sender: TObject;
    InputBuffers, OutputBuffers: TDAVArrayOfSingleFixedArray;
    FrameCount: NativeUInt; TimeInfo: PPaStreamCallbackTimeInfo;
    StatusFlags: TPaStreamCallbackFlags): LongInt of object;

  TPortAudioHost = class
  private
    FInputDevice: LongInt;
    FOutputDevice: LongInt;
    FSampleRate: Double;
    FDeviceList: TStringList;
    FInputDeviceList: TStringList;
    FOutputDeviceList: TStringList;
    FOnSampleRateChanged: TNotifyEvent;
    FOutputLatency: Double;
    FInputLatency: Double;
    FInputBuffers: TDAVArrayOfSingleFixedArray;
    FOutputBuffers: TDAVArrayOfSingleFixedArray;
    function GetActive: Boolean;
    function GetDefaultInputDevice: LongInt;
    function GetDefaultOutputDevice: LongInt;
    function GetDeviceCount: LongInt;
    function GetDeviceInfo(Index: Integer): PPaDeviceInfo;
    function GetHostApiCount: LongInt;
    function GetHostApiDefaultIndex: LongInt;
    function GetHostApiInfo(Index: Integer): PPaHostApiInfo;
    function StreamCallback(Input: Pointer; Output: Pointer;
      FrameCount: NativeUInt; TimeInfo: PPaStreamCallbackTimeInfo;
      StatusFlags: TPaStreamCallbackFlags): LongInt;
    procedure SetActive(const Value: Boolean);
    procedure SetInputDevice(const Value: LongInt);
    procedure SetOutputDevice(const Value: LongInt);
    procedure SetSampleRate(const Value: Double);
  protected
    FInputStreamParameters: TPaStreamParameters;
    FOutputStreamParameters: TPaStreamParameters;
    FStream: PPaStream;
    FOnStreamCallback: TPortAudioStreamCallbackEvent;
    procedure UpdateDeviceList;
    procedure UpdateInputStreamParameters;
    procedure UpdateOutputStreamParameters;
    procedure InputDeviceChanged;
    procedure OutputDeviceChanged;
    procedure SampleRateChanged;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Open;
    procedure Close;

    procedure Start;
    procedure Stop;
    procedure Abort;

    property Active: Boolean read GetActive write SetActive;
    property DeviceCount: LongInt read GetDeviceCount;
    property InputDevice: LongInt read FInputDevice write SetInputDevice;
    property OutputDevice: LongInt read FOutputDevice write SetOutputDevice;
    property InputLatency: Double read FInputLatency;
    property OutputLatency: Double read FOutputLatency;
    property DefaultInputDevice: LongInt read GetDefaultInputDevice;
    property DefaultOutputDevice: LongInt read GetDefaultOutputDevice;
    property HostApiCount: LongInt read GetHostApiCount;
    property HostApiDefaultIndex: LongInt read GetHostApiDefaultIndex;
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property DeviceInfo[Index: Integer]: PPaDeviceInfo read GetDeviceInfo;
    property HostApiInfo[Index: Integer]: PPaHostApiInfo read GetHostApiInfo;
    property InputDeviceList: TStringList read FInputDeviceList;
    property OutputDeviceList: TStringList read FOutputDeviceList;

    property OnSampleRateChanged: TNotifyEvent read FOnSampleRateChanged
      write FOnSampleRateChanged;
    property OnStreamCallback: TPortAudioStreamCallbackEvent
      read FOnStreamCallback write FOnStreamCallback;
  end;

implementation

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrPortAudioIsCurrent = 'PortAudio is current active';
  RCStrInvalidIndex = 'Invalid Index (%d)';
  RCStrVersionNotSupported = 'Version not supported (%s)';

  { TPortAudioHost }

constructor TPortAudioHost.Create;
var
  PaError: TPaError;
begin
  FStream := nil;
  FSampleRate := 44100;

  inherited;
  if Pa_GetVersion < 1899 then
    raise EPortAudio.CreateFmt(RCStrVersionNotSupported, [Pa_GetVersionText]);

  PaError := Pa_Initialize;

  if PaError <> 0 then
    raise EPortAudio.Create(string(Pa_GetErrorText(PaError)));

  FDeviceList := TStringList.Create;
  FInputDeviceList := TStringList.Create;
  FOutputDeviceList := TStringList.Create;
  UpdateDeviceList;
  FInputDevice := Pa_GetDefaultInputDevice;
  FOutputDevice := Pa_GetDefaultOutputDevice;

  UpdateInputStreamParameters;
  UpdateOutputStreamParameters;
end;

destructor TPortAudioHost.Destroy;
var
  PaError: TPaError;
begin
  PaError := Pa_Terminate;

  if PaError <> 0 then
    raise EPortAudio.Create(string(Pa_GetErrorText(PaError)));

  FreeAndNil(FDeviceList);
  FreeAndNil(FInputDeviceList);
  FreeAndNil(FOutputDeviceList);

  inherited;
end;

function TPortAudioHost.GetActive: Boolean;
var
  PaError: TPaError;
begin
  if Assigned(FStream) then
  begin
    PaError := Pa_IsStreamActive(FStream);

    if PaError < 0 then
      raise EPortAudio.Create(string(Pa_GetErrorText(PaError)));

    Result := PaError = 1;
  end
  else
    Result := False;
end;

function TPortAudioHost.GetDefaultInputDevice: LongInt;
begin
  Result := Pa_GetDefaultInputDevice;
end;

function TPortAudioHost.GetDefaultOutputDevice: LongInt;
begin
  Result := Pa_GetDefaultOutputDevice;
end;

function TPortAudioHost.GetDeviceCount: LongInt;
begin
  Result := Pa_GetDeviceCount;
end;

function TPortAudioHost.GetDeviceInfo(Index: Integer): PPaDeviceInfo;
begin
  if (Index >= 0) and (Index < DeviceCount) then
    Result := Pa_GetDeviceInfo(Index)
  else
    raise EPortAudio.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TPortAudioHost.GetHostApiInfo(Index: Integer): PPaHostApiInfo;
begin
  if (Index >= 0) and (Index < HostApiCount) then
    Result := Pa_GetHostApiInfo(Index)
  else
    raise EPortAudio.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TPortAudioHost.InputDeviceChanged;
begin
  UpdateInputStreamParameters;
end;

function PortAudioStreamCallback(Input: Pointer; Output: Pointer;
  FrameCount: NativeUInt; TimeInfo: PPaStreamCallbackTimeInfo;
  StatusFlags: TPaStreamCallbackFlags; UserData: Pointer): LongInt; cdecl;
begin
  Assert(Assigned(UserData));

  // call object callback
  Result := TPortAudioHost(UserData).StreamCallback(Input, Output, FrameCount,
    TimeInfo, StatusFlags);
end;

procedure TPortAudioHost.Open;
var
  PaError: TPaError;
  StreamInfo: PPaStreamInfo;
  ISP, OSP: PPaStreamParameters;
begin
  ISP := nil;
  if FInputDevice >= 0 then
    ISP := @FInputStreamParameters;
  OSP := nil;
  if FOutputDevice >= 0 then
    OSP := @FOutputStreamParameters;
  PaError := Pa_OpenStream(FStream, ISP, OSP, FSampleRate, 64,
    paPrimeOutputBuffersUsingStreamCallback, @PortAudioStreamCallback, Self);

  // raise any error with the correct error text
  if PaError <> 0 then
    raise EPortAudio.Create(string(Pa_GetErrorText(PaError)));

  // read stream info
  StreamInfo := Pa_GetStreamInfo(FStream);

  // check if current sample rate has changed
  if StreamInfo.SampleRate <> FSampleRate then
  begin
    FSampleRate := StreamInfo.SampleRate;
    SampleRateChanged;
  end;

  // check if input latency has changed
  if StreamInfo.InputLatency <> FInputLatency then
    FInputLatency := StreamInfo.InputLatency;

  // check if output latency has changed
  if StreamInfo.OutputLatency <> FOutputLatency then
    FOutputLatency := StreamInfo.OutputLatency;
end;

procedure TPortAudioHost.Close;
var
  PaError: TPaError;
begin
  if Assigned(FStream) then
  begin
    PaError := Pa_CloseStream(FStream);

    // raise any error with the correct error text
    if PaError <> 0 then
      raise EPortAudio.Create(string(Pa_GetErrorText(PaError)));

    // clear stream pointer
    FStream := nil;
  end;
end;

procedure TPortAudioHost.OutputDeviceChanged;
begin
  UpdateOutputStreamParameters;
end;

procedure TPortAudioHost.UpdateInputStreamParameters;
var
  DevInfo: PPaDeviceInfo;
begin
  if FInputDevice >= 0 then
    with FInputStreamParameters do
    begin
      Device := FInputDevice;
      DevInfo := DeviceInfo[FInputDevice];
      ChannelCount := DevInfo.MaxInputChannels;
      SetLength(FInputBuffers, ChannelCount);
      SampleFormat := paFloat32 + paNonInterleaved;
      FInputLatency := DevInfo.DefaultLowInputLatency;
      SuggestedLatency := FInputLatency;
      HostApiSpecificStreamInfo := nil;
    end
  else
    SetLength(FInputBuffers, 0);
end;

procedure TPortAudioHost.UpdateOutputStreamParameters;
var
  DevInfo: PPaDeviceInfo;
begin
  if FOutputDevice >= 0 then
    with FOutputStreamParameters do
    begin
      Device := FOutputDevice;
      DevInfo := DeviceInfo[FOutputDevice];
      ChannelCount := DevInfo.MaxOutputChannels;
      SetLength(FOutputBuffers, ChannelCount);
      SampleFormat := paFloat32 + paNonInterleaved;
      FOutputLatency := DevInfo.DefaultLowOutputLatency;
      SuggestedLatency := FOutputLatency;
      HostApiSpecificStreamInfo := nil;
    end
  else
    SetLength(FOutputBuffers, 0);
end;

function TPortAudioHost.GetHostApiCount: LongInt;
begin
  Result := Pa_GetHostApiCount;
end;

function TPortAudioHost.GetHostApiDefaultIndex: LongInt;
begin
  Result := Pa_GetDefaultHostApi;
end;

procedure TPortAudioHost.SampleRateChanged;
begin
  if Assigned(FOnSampleRateChanged) then
    FOnSampleRateChanged(Self);
end;

procedure TPortAudioHost.SetActive(const Value: Boolean);
begin
  if Active <> Value then
  begin
    if Value then
      Start
    else
      Stop;
  end;
end;

procedure TPortAudioHost.SetInputDevice(const Value: LongInt);
begin
  if Active then
    raise EPortAudio.Create(RCStrPortAudioIsCurrent);

  if (Value < -1) or (Value >= DeviceCount) then
    raise EPortAudio.CreateFmt(RCStrInvalidIndex, [Value]);

  if FInputDevice <> Value then
  begin
    FInputDevice := Value;
    InputDeviceChanged;
  end;
end;

procedure TPortAudioHost.SetOutputDevice(const Value: LongInt);
begin
  if Active then
    raise EPortAudio.Create(RCStrPortAudioIsCurrent);

  if (Value < -1) or (Value >= DeviceCount) then
    raise EPortAudio.CreateFmt(RCStrInvalidIndex, [Value]);

  if FOutputDevice <> Value then
  begin
    FOutputDevice := Value;
    OutputDeviceChanged;
  end;
end;

procedure TPortAudioHost.SetSampleRate(const Value: Double);
begin
  if Active then
    EPortAudio.Create(RCStrPortAudioIsCurrent);

  if FSampleRate <> Value then
  begin
    FSampleRate := Value;
    SampleRateChanged;
  end;
end;

procedure TPortAudioHost.Start;
var
  PaError: TPaError;
begin
  if Assigned(FStream) then
  begin
    // try to start the currently opened stream
    PaError := Pa_StartStream(FStream);

    // raise any error with the correct error text
    if PaError <> 0 then
      raise EPortAudio.Create(string(Pa_GetErrorText(PaError)));
  end
end;

procedure TPortAudioHost.Stop;
var
  PaError: TPaError;
begin
  if Assigned(FStream) then
  begin
    // try to stop the currently opened stream
    PaError := Pa_StopStream(FStream);

    // raise any error with the correct error text
    if PaError <> 0 then
      raise EPortAudio.Create(string(Pa_GetErrorText(PaError)));
  end;
end;

function TPortAudioHost.StreamCallback(Input, Output: Pointer;
  FrameCount: NativeUInt; TimeInfo: PPaStreamCallbackTimeInfo;
  StatusFlags: TPaStreamCallbackFlags): LongInt;
begin
  if Assigned(FOnStreamCallback) then
  begin
    if Input <> nil then
      Move(Input^, FInputBuffers[0], Length(FInputBuffers) * SizeOf(Pointer));
    if Output <> nil then
      Move(Output^, FOutputBuffers[0], Length(FOutputBuffers) *
        SizeOf(Pointer));

    Result := FOnStreamCallback(Self, FInputBuffers, FOutputBuffers, FrameCount,
      TimeInfo, StatusFlags);
  end
  else
    Result := paContinue;
end;

procedure TPortAudioHost.Abort;
var
  PaError: TPaError;
begin
  if Assigned(FStream) then
  begin
    // try to stop the currently opened stream
    PaError := Pa_AbortStream(FStream);

    // raise any error with the correct error text
    if PaError <> 0 then
      raise EPortAudio.Create(string(Pa_GetErrorText(PaError)));
  end;
end;

procedure TPortAudioHost.UpdateDeviceList;
var
  Index: Integer;
  DevInfo: PPaDeviceInfo;
  HostApi: PPaHostApiInfo;
begin
  FInputDeviceList.Clear;
  FOutputDeviceList.Clear;
  FDeviceList.Clear;
  for Index := 0 to DeviceCount - 1 do
  begin
    DevInfo := DeviceInfo[Index];
    HostApi := HostApiInfo[DevInfo^.HostApi];
    FDeviceList.Add(string(HostApi^.Name) + ': ' + string(DevInfo^.Name));
    if DevInfo.MaxInputChannels > 0 then
      FInputDeviceList.AddObject(string(HostApi^.Name) + ': ' +
        string(DevInfo^.Name), TObject(Index));
    if DevInfo.MaxOutputChannels > 0 then
      FOutputDeviceList.AddObject(string(HostApi^.Name) + ': ' +
        string(DevInfo^.Name), TObject(Index));
  end;
end;

end.
