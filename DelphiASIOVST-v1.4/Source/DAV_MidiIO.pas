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
{  The MIDI device classes were originally written by Adrian Meyer             }
{  See http://www.midimountain.com                                             }
{                                                                              }
{  It has been adapted for this project by Christian-W. Budde                  }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_MidiIO;

interface

{$I DAV_Compiler.inc}

uses
{$IFDEF FPC}LCLIntf, {$ELSE} Windows, {$ENDIF} Classes, SysUtils, Math,
  Contnrs, MMSystem;

const
  // size of system exclusive buffer
  CSysExBufferSize = 2048;

  // MIDI Status Bytes for Channel Voice Messages
  CMIDIMsgNoteOff = $80;
  CMIDIMsgNoteOn = $90;
  CMIDIMsgPolyKeyPressure = $A0;
  CMIDIMsgControlChange = $B0;
  CMIDIMsgProgramChange = $C0;
  CMIDIMsgChannelKeyPressure = $D0;
  CMIDIMsgAftertouch = CMIDIMsgChannelKeyPressure; // Synonym
  CMIDIMsgPitchWheelChange = $E0;

  // MIDI Status Bytes for System Common Messages
  CMIDIMsgSysEx = $F0;
  CMIDIMsgMTCQtrFrame = $F1; // MIDI Time Code Qtr. Frame
  CMIDIMsgSongPositionPtr = $F2;
  CMIDIMsgSongSelect = $F3;
  CMIDIMsgTuneRequest = $F6;
  CMIDIMsgEOX = $F7; // marks end of system exclusive message

  // MIDI Status Bytes for System Real-Time Messages
  CMIDIMsgTimingClock = $F8;
  CMIDIMsgStartSequence = $FA;
  CMIDIMsgContinueSequence = $FB;
  CMIDIMsgStopSequence = $FC;
  CMIDIMsgActiveSensing = $FE;
  CMIDIMsgSystemReset = $FF;

  // MIDICC...: MIDI Control Change Messages

  // Continuous Controllers MSB
  CMIDICCBankSelect = $00;
  CMIDICCModulationWheel = $01;
  CMIDICCBreathControl = $02;
  CMIDICCFootController = $04;
  CMIDICCPortamentoTime = $05;
  CMIDICCDataEntry = $06;
  CMIDICCChannelVolume = $07;
  CMIDICCMainVolume = CMIDICCChannelVolume;
  CMIDICCBalance = $08;
  CMIDICCPan = $0A;
  CMIDICCExpression = $0B;
  CMIDICCEffectControl = $0C;
  CMIDICCEffectControl2 = $0D;
  CMIDICCGeneralPurpose1 = $10;
  CMIDICCGeneralPurpose2 = $11;
  CMIDICCGeneralPurpose3 = $12;
  CMIDICCGeneralPurpose4 = $13;

  // Continuous Controllers LSB
  CMIDICCBankSelectLSB = $20;
  CMIDICCModulationWheelLSB = $21;
  CMIDICCBreathControlLSB = $22;
  CMIDICCFootControllerLSB = $24;
  CMIDICCPortamentoTimeLSB = $25;
  CMIDICCDataEntryLSB = $26;
  CMIDICCChannelVolumeLSB = $27;
  CMIDICCMainVolumeLSB = CMIDICCChannelVolumeLSB;
  CMIDICCBalanceLSB = $28;
  CMIDICCPanLSB = $2A;
  CMIDICCExpressionLSB = $2B;
  CMIDICCEffectControlLSB = $2C;
  CMIDICCEffectControl2LSB = $2D;
  CMIDICCGeneralPurpose1LSB = $30;
  CMIDICCGeneralPurpose2LSB = $31;
  CMIDICCGeneralPurpose3LSB = $32;
  CMIDICCGeneralPurpose4LSB = $33;

  // Switches
  CMIDICCSustain = $40;
  CMIDICCPortamento = $41;
  CMIDICCSustenuto = $42;
  CMIDICCSoftPedal = $43;
  CMIDICCLegato = $44;
  CMIDICCHold2 = $45;

  CMIDICCSound1 = $46; // (Sound Variation)
  CMIDICCSound2 = $47; // (Timbre/Harmonic Intens.)
  CMIDICCSound3 = $48; // (Release Time)
  CMIDICCSound4 = $49; // (Attack Time)
  CMIDICCSound5 = $4A; // (Brightness)
  CMIDICCSound6 = $4B; // (Decay Time)
  CMIDICCSound7 = $4C; // (Vibrato Rate)
  CMIDICCSound8 = $4D; // (Vibrato Depth)
  CMIDICCSound9 = $4E; // (Vibrato Delay)
  CMIDICCSound10 = $4F; //

  CMIDICCGeneralPurpose5 = $50;
  CMIDICCGeneralPurpose6 = $51;
  CMIDICCGeneralPurpose7 = $52;
  CMIDICCGeneralPurpose8 = $53;
  CMIDICCPortamentoControl = $54;

  CMIDICCReverbSendLevel = $5B;
  CMIDICCEffects2Depth = $5C;
  CMIDICCTremoloDepth = CMIDICCEffects2Depth;
  CMIDICCChorusSendLevel = $5D;
  CMIDICCEffects4Depth = $5E;
  CMIDICCCelesteDepth = CMIDICCEffects4Depth;
  CMIDICCEffects5Depth = $5F;
  CMIDICCPhaserDepth = CMIDICCEffects5Depth;

  CMIDICCDataEntryInc = $60;
  CMIDICCDataEntryDec = $61;
  CMIDICCNonRegParamNumLSB = $62;
  CMIDICCNonRegParamNumMSB = $63;
  CMIDICCRegParamNumLSB = $64;
  CMIDICCRegParamNumMSB = $65;

  // Registered Parameter Numbers [CC# 65H,64H]
  // -----------------------------------------------------------
  // CC#65 (MSB) | CC#64 (LSB) | Function
  // Hex|Dec|    |  Hex|Dec|   |
  // - - - - - - | - - - - - - |- - - - - - - - - - - - - - - -
  // 00 = 0     |  00 = 0     | Pitch Bend Sensitivity
  // 00 = 0     |  01 = 1     | Channel Fine Tuning
  // 00 = 0     |  02 = 2     | Channel Coarse Tuning
  // 00 = 0     |  03 = 3     | Tuning Program Change
  // 00 = 0     |  04 = 4     | Tuning Bank Select

  // Channel Mode Messages (Control Change >= $78)
  CMIDICCAllSoundOff = $78;
  CMIDICCResetAllControllers = $79;
  CMIDICCLocalControl = $7A;
  CMIDICCAllNotesOff = $7B;

  CMIDICCOmniModeOff = $7C;
  CMIDICCOmniModeOn = $7D;
  CMIDICCMonoModeOn = $7E;
  CMIDICCPolyModeOn = $7F;

type
  TMIDIChannel = 1 .. 16;
  TMIDIDataByte = 0 .. $7F; // 7 bits
  TMIDIDataWord = 0 .. $3FFF; // 14 bits
  TMIDIStatusByte = $80 .. $FF;
  TMIDIVelocity = TMIDIDataByte;
  TMIDIKey = TMIDIDataByte;
  TMIDINote = TMIDIKey;

  // event if data is received
  TOnMidiInData = procedure(const ADeviceIndex: Integer;
    const AStatus, AData1, AData2: Byte) of object;
  // event of system exclusive data is received
  TOnSysExData = procedure(const ADeviceIndex: Integer;
    const AStream: TMemoryStream) of object;

  EMidiDevices = Exception;

  // base class for MIDI devices
  TMidiDevices = class
  private
    FDevices: TStringList;
    FMidiResult: MMResult;
    procedure SetMidiResult(const Value: MMResult);
  protected
    property MidiResult: MMResult read FMidiResult write SetMidiResult;
    function GetHandle(const ADeviceIndex: Integer): THandle;
  public
    constructor Create; virtual; // create the MIDI devices
    destructor Destroy; override; // whack the devices
    procedure Open(const ADeviceIndex: Integer); virtual; abstract;
    // open a specific device
    procedure Close(const ADeviceIndex: Integer); virtual; abstract;
    // close a specific device
    procedure CloseAll; // close all devices
    function IsOpen(ADeviceIndex: Integer): Boolean; // check if open

    property Devices: TStringList read FDevices; // THE devices
  end;

  // MIDI input devices
  TMidiInput = class(TMidiDevices)
  private
    FOnMidiData: TOnMidiInData;
    FOnSysExData: TOnSysExData;
    FSysExData: TObjectList;
  protected
    procedure DoSysExData(const ADeviceIndex: Integer);
  public
    constructor Create; override; // create an input device
    destructor Destroy; override; // what the input devices
    procedure Open(const ADeviceIndex: Integer); override;
    // open a specific input device
    procedure Close(const ADeviceIndex: Integer); override;
    // close a specific device

    property OnMidiData: TOnMidiInData read FOnMidiData write FOnMidiData;
    // midi data event
    property OnSysExData: TOnSysExData read FOnSysExData write FOnSysExData;
    // midi system exclusive is received
  end;

  // MIDI output devices
  TMidiOutput = class(TMidiDevices)
    constructor Create; override;
    procedure Open(const ADeviceIndex: Integer); override;
    // open a specific input device
    procedure Close(const ADeviceIndex: Integer); override;
    // close a specific device
    procedure Send(const ADeviceIndex: Integer;
      const AStatus, AData1, AData2: Byte);
    // send some midi data to the indexed device
    procedure SendSysEx(const ADeviceIndex: Integer;
      const AStream: TMemoryStream); overload;
    // send system exclusive data to a device
    procedure SendSysEx(const ADeviceIndex: Integer;
      const AString: string); overload;
  end;

function SysExStreamToStr(const AStream: TMemoryStream): string;
// convert the stream into xx xx xx xx string
procedure StrToSysExStream(const AString: string; const AStream: TMemoryStream);
// fill the string in a xx xx xx xx into the stream

implementation

type
  TSysExBuffer = array [0 .. CSysExBufferSize] of AnsiChar;

  TSysExData = class
  private
    FSysExStream: TMemoryStream;
  public
    SysExHeader: {$IFDEF FPC}_midihdr{$ELSE}TMidiHdr{$ENDIF};
    SysExData: TSysExBuffer;
    constructor Create;
    destructor Destroy; override;
    property SysExStream: TMemoryStream read FSysExStream;
  end;

{ TMidiDevices }

constructor TMidiDevices.Create;
begin
  FDevices := TStringList.Create;
end;

destructor TMidiDevices.Destroy;
begin
  FreeAndNil(FDevices);
  inherited;
end;


{ TMidiInput }

type
{$IFDEF FPC}
  PHMIDIIN = ^HMIDIIN;
  TMidiHdr = _midihdr;
  TMidiInCapsA = _MIDIINCAPSA;
  TMidiInCapsW = _MIDIINCAPSW;
  TMidiInCaps = TMidiInCapsA;
  TMidiOutCapsA = _MIDIOUTCAPSA;
  TMidiOutCapsW = _MIDIOUTCAPSW;
  TMidiOutCaps = TMidiOutCapsA;
{$ENDIF}

  TMidiInputDeviceRecord = record
    MidiInput: TMidiInput;
    DeviceNumber: Integer;
  end;

  PMidiInputDeviceRecord = ^TMidiInputDeviceRecord;

procedure midiInCallback(aMidiInHandle: PHMIDIIN; aMsg: Integer;
  aInstance, aMidiData, aTimeStamp: Integer); stdcall;
begin
  if aInstance <> 0 then
    with PMidiInputDeviceRecord(aInstance)^ do
      case aMsg of
        MIM_DATA:
          begin
            if Assigned(MidiInput.OnMidiData) then
              MidiInput.OnMidiData(DeviceNumber, aMidiData and $000000FF,
                (aMidiData and $0000FF00) shr 8,
                (aMidiData and $00FF0000) shr 16);
          end;
        MIM_LONGDATA:
          MidiInput.DoSysExData(DeviceNumber);
        MIM_ERROR, MIM_LONGERROR:
          raise Exception.Create('Midi In Error!');
      end;
end;


{ TMidiInput }

constructor TMidiInput.Create;
var
  ll, i: Integer;
  lInCaps: TMidiInCaps;
begin
  inherited;
  FSysExData := TObjectList.Create(True);
  try
    ll := midiInGetNumDevs;
  except
    ll := 0;
  end;
  for i := 0 to ll - 1 do
    try
      MidiResult := midiInGetDevCaps(i, @lInCaps, SizeOf(TMidiInCaps));
      if MidiResult = MMSYSERR_NOERROR then
      begin
        FDevices.Add(StrPas(lInCaps.szPname));
        FSysExData.Add(TSysExData.Create);
      end;
    except
      // do nothing but supress the exception to the end user
    end;
end;

destructor TMidiInput.Destroy;
begin
  FreeAndNil(FSysExData);
  inherited;
end;

procedure TMidiInput.Close(const ADeviceIndex: Integer);
var
  DeviceHandle: THandle;
begin
  DeviceHandle := GetHandle(ADeviceIndex);
  if DeviceHandle = 0 then
    Exit
  else
  begin
    MidiResult := midiInStop(DeviceHandle);
    MidiResult := midiInReset(DeviceHandle);
    MidiResult := midiInUnprepareHeader(DeviceHandle,
      @TSysExData(FSysExData[ADeviceIndex]).SysExHeader, SizeOf(TMidiHdr));
    MidiResult := midiInClose(DeviceHandle);
    FDevices.Objects[ADeviceIndex] := nil;
  end;
end;

procedure TMidiDevices.CloseAll;
var
  Device: Integer;
begin
  for Device := 0 to FDevices.Count - 1 do
    Close(Device);
end;

procedure TMidiInput.Open(const ADeviceIndex: Integer);
var
  lHandle: THandle;
  lMidiInputDevice: PMidiInputDeviceRecord;
begin
  if GetHandle(ADeviceIndex) <> 0 then
    Exit;
  GetMem(lMidiInputDevice, SizeOf(TMidiInputDeviceRecord));
  lMidiInputDevice^.MidiInput := Self;
  lMidiInputDevice^.DeviceNumber := ADeviceIndex;
  MidiResult := midiInOpen(@lHandle, ADeviceIndex, Cardinal(@midiInCallback),
    Integer(lMidiInputDevice), CALLBACK_FUNCTION);
  FDevices.Objects[ADeviceIndex] := TObject(lHandle);

  with TSysExData(FSysExData[ADeviceIndex]) do
  begin
    SysExHeader.dwFlags := 0;

    MidiResult := midiInPrepareHeader(lHandle, @SysExHeader, SizeOf(TMidiHdr));
    MidiResult := midiInAddBuffer(lHandle, @SysExHeader, SizeOf(TMidiHdr));
    MidiResult := midiInStart(lHandle);
  end;
end;

procedure TMidiInput.DoSysExData(const ADeviceIndex: Integer);
begin
  with TSysExData(FSysExData[ADeviceIndex]) do
  begin
    if SysExHeader.dwBytesRecorded = 0 then
      Exit;
    SysExStream.Write(SysExData, SysExHeader.dwBytesRecorded);
    if SysExHeader.dwFlags and MHDR_DONE = MHDR_DONE then
    begin
      SysExStream.Position := 0;
      if Assigned(FOnSysExData) then
        FOnSysExData(ADeviceIndex, SysExStream);
      SysExStream.Clear;
    end;
    SysExHeader.dwBytesRecorded := 0;
    MidiResult := midiInPrepareHeader(GetHandle(ADeviceIndex), @SysExHeader,
      SizeOf(TMidiHdr));
    MidiResult := midiInAddBuffer(GetHandle(ADeviceIndex), @SysExHeader,
      SizeOf(TMidiHdr));
  end;
end;


{ TMidiOutput }

constructor TMidiOutput.Create;
var
  ll, i: Integer;
  lOutCaps: TMidiOutCaps;
begin
  inherited;
  try
    ll := midiOutGetNumDevs;
  except
    ll := 0;
  end;
  for i := 0 to ll - 1 do
  begin
    MidiResult := midiOutGetDevCaps(i, @lOutCaps, SizeOf(TMidiOutCaps));
    FDevices.Add(lOutCaps.szPname);
  end;
end;

procedure TMidiOutput.Open(const ADeviceIndex: Integer);
var
  lHandle: THandle;
begin
  // device already open;
  if GetHandle(ADeviceIndex) <> 0 then
    Exit;
  MidiResult := midiOutOpen(@lHandle, ADeviceIndex, 0, 0, CALLBACK_NULL);
  FDevices.Objects[ADeviceIndex] := TObject(lHandle);
end;

procedure TMidiOutput.Close(const ADeviceIndex: Integer);
begin
  if GetHandle(ADeviceIndex) = 0 then
    Exit;
  MidiResult := midiOutClose(GetHandle(ADeviceIndex));
  FDevices.Objects[ADeviceIndex] := nil;
end;

procedure TMidiOutput.Send(const ADeviceIndex: Integer;
  const AStatus, AData1, AData2: Byte);
var
  lMsg: Cardinal;
begin
  // open the device is not open
  if not Assigned(FDevices.Objects[ADeviceIndex]) then
    Open(ADeviceIndex);
  lMsg := AStatus + (AData1 * $100) + (AData2 * $10000);
  MidiResult := midiOutShortMsg(GetHandle(ADeviceIndex), lMsg);
end;

procedure TMidiDevices.SetMidiResult(const Value: MMResult);
var
  lError: array [0 .. MAXERRORLENGTH] of char;
begin
  FMidiResult := Value;
  if FMidiResult <> MMSYSERR_NOERROR then
    if midiInGetErrorText(FMidiResult, @lError, MAXERRORLENGTH) = MMSYSERR_NOERROR
    then
      raise EMidiDevices.Create(StrPas(lError));
end;

function TMidiDevices.GetHandle(const ADeviceIndex: Integer): THandle;
begin
  try
    if FDevices.Count = 0 then
      Result := 0
    else if not((ADeviceIndex >= 0) and (ADeviceIndex < FDevices.Count)) then
      raise EMidiDevices.CreateFmt('%s: Device index out of bounds! (%d)',
        [ClassName, ADeviceIndex])
    else
      Result := THandle(FDevices.Objects[ADeviceIndex]);
  except
    Result := 0;
  end;
end;

function TMidiDevices.IsOpen(ADeviceIndex: Integer): Boolean;
begin
  Result := GetHandle(ADeviceIndex) <> 0;
end;

procedure TMidiOutput.SendSysEx(const ADeviceIndex: Integer;
  const AString: string);
var
  lStream: TMemoryStream;
begin
  lStream := TMemoryStream.Create;
  try
    StrToSysExStream(AString, lStream);
    SendSysEx(ADeviceIndex, lStream);
  finally
    FreeAndNil(lStream);
  end;
end;

procedure TMidiOutput.SendSysEx(const ADeviceIndex: Integer;
  const AStream: TMemoryStream);
var
  lSysExHeader: TMidiHdr;
begin
  if AStream.Size > 0 then
  begin
    AStream.Position := 0;
    lSysExHeader.dwBufferLength := AStream.Size;
    lSysExHeader.lpData := AStream.Memory;
    lSysExHeader.dwFlags := 0;
    MidiResult := midiOutPrepareHeader(GetHandle(ADeviceIndex), @lSysExHeader,
      SizeOf(TMidiHdr));
    MidiResult := midiOutLongMsg(GetHandle(ADeviceIndex), @lSysExHeader,
      SizeOf(TMidiHdr));
    MidiResult := midiOutUnprepareHeader(GetHandle(ADeviceIndex), @lSysExHeader,
      SizeOf(TMidiHdr));
  end;
end;


{ TSysExData }

constructor TSysExData.Create;
begin
  SysExHeader.dwBufferLength := CSysExBufferSize;
  SysExHeader.lpData := SysExData;
  FSysExStream := TMemoryStream.Create;
end;

destructor TSysExData.Destroy;
begin
  FreeAndNil(FSysExStream);
end;

function SysExStreamToStr(const AStream: TMemoryStream): string;
var
  i: Integer;
begin
  Result := '';
  AStream.Position := 0;
  for i := 0 to AStream.Size - 1 do
    Result := Result + Format('%.2x ', [Byte(PChar(AStream.Memory)[i])]);
end;

procedure StrToSysExStream(const AString: string; const AStream: TMemoryStream);
const
  cHex = '123456789ABCDEF';
var
  i: Integer;
  lStr: string;
begin
  lStr := StringReplace(AnsiUpperCase(AString), ' ', '', [rfReplaceAll]);
  AStream.Size := Length(lStr) div 2 - 1;
  AStream.Position := 0;
  for i := 1 to AStream.Size do
    PChar(AStream.Memory)[i - 1] :=
      char(AnsiPos(lStr[i * 2 - 1], cHex) shl 4 + AnsiPos(lStr[i * 2], cHex));
end;

end.
