unit MidiBase;

interface

implementation

uses
  classes, SysUtils, mmsystem, Math, Windows, Contnrs,UMidiPorts,  Generics.Collections;

const
  // size of system exclusive buffer
  cSysExBufferSize = 2048;

type

  EMidiDevices = Exception;

  // MIDI input devices
  TMidiInputDevices = class(TMidiInputsBase)
  private
    fSysExData: TObjectList;
    fIsHWInput: array of boolean;
  protected
    procedure DoSysExData(aDeviceIndex: integer);
    function GetHandle(const aDeviceIndex: integer): THandle;

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Open(aDeviceIndex: integer); override;
    procedure Close(aDeviceIndex: integer); override;
    function IsHWDevice(aDeviceIndex: integer): boolean;override;
  end;

  // MIDI output devices
  TMidiOutputDevices = class(TMidiOutputsBase)
  private
  protected
    function GetHandle(const aDeviceIndex: integer): THandle;
  public
    constructor Create; override;
    procedure Open(aDeviceIndex: integer); override;
    procedure Close(aDeviceIndex: integer); override;
    procedure WriteMidi(aDeviceIndex: integer; aStatus, aData1, aData2: byte); override;
    procedure WriteSysEx(aDeviceIndex: integer; syx:string); override;
  end;

procedure SetMidiResult(const Value: MMResult);
var
  lError: array [0 .. MAXERRORLENGTH] of char;
begin
  if Value <> MMSYSERR_NOERROR then
    if midiInGetErrorText(Value, @lError, MAXERRORLENGTH) = MMSYSERR_NOERROR
    then
      raise EMidiDevices.Create(StrPas(lError));
end;

type
  TSysExBuffer = array [0 .. cSysExBufferSize] of Ansichar;

  TCSysExData = class
  private
    fSysExStream: TMemoryStream;
  public
    SysExHeader: TMidiHdr;
    SysExData: TSysExBuffer;
    constructor Create;
    destructor Destroy; override;
    property SysExStream: TMemoryStream read fSysExStream;
  end;

{ TMidiInput }

VAR MidiInput:TMidiInputDevices;

procedure midiInCallback(aMidiInHandle: PHMIDIIN; aMsg: UInt; aData, aMidiData, aTimeStamp: integer); stdcall;
begin
  try
  case aMsg of
    MIM_DATA:
      begin
        MidiInput.OnMidiData(aData,aMidiData and $FF,(aMidiData and $0000FF00) shr 8, (aMidiData and $00FF0000) shr 16);
      end;
    MIM_LONGDATA:
       MidiInput.DoSysExData(aData);
  end;
  except
  end;
end;

procedure TMidiInputDevices.Close(aDeviceIndex: integer);
begin
  if GetHandle(aDeviceIndex) <> 0 then
  begin
    SetMidiResult( midiInStop(GetHandle(aDeviceIndex)));
    SetMidiResult( midiInReset(GetHandle(aDeviceIndex)));
    SetMidiResult( midiInUnprepareHeader(GetHandle(aDeviceIndex),
      @TCSysExData(fSysExData[aDeviceIndex]).SysExHeader, SizeOf(TMidiHdr)));
    try
      SetMidiResult( midiInClose(GetHandle(aDeviceIndex)));
    except end;
    fDevices.Objects[aDeviceIndex] := nil;
  end;
end;

function CheckForMidiMix(name,s:string):string;
VAR i,amps,p:integer;
begin
  result:='';
  if name <> 'MIDI Mix' then exit;
  amps:=0;
  for i:=1 to length(s) do if
    s[i]='&' then
    begin
      inc(amps);
      case amps of
        3: p:=i;
        4: result:=Copy(s,p+1,i-p-1);
      end;
    end;
  if result = '1d9c695c' then result:=' Left'
  else if result='2d67a182' then result:=' Left'
  else if result='4868820' then result:=' Left'
  else if result='2335b881' then result:=' Right'
  else if result='910a50b' then result:=' Left'
  else if result='2bd48ab4' then result:=' Right'
  else result:=' ['+result+']';
end;

function Nice(s:string):string;
VAR i:integer;
begin
  result:='';
// only fix behringers...
  if pos('2000',s)<>0 then
  begin
    for i:=1 to length(s) do if s[i]<chr(128) then result:=result+s[i];
  end
  else
    result:=s;
end;


constructor TMidiInputDevices.Create;
var
  i,j: integer;
  lInCaps: TMidiInCaps;
  buf: array[0..255] of char;
  usbPath,DeviceName:string;
begin
  inherited;
  fSysExData := TObjectList.Create(true);
  SetLength(fIsHWInput,midiInGetNumDevs);
  for i := 0 to integer(midiInGetNumDevs) - 1 do
  try
    SetMidiResult( midiInGetDevCaps(i, @lInCaps, SizeOf(TMidiInCaps)));
    DeviceName:=Nice(StrPas(lInCaps.szPname));
    fSysExData.Add(TCSysExData.Create);

    midiInMessage (i,$80c,integer(@buf),256);
    usbPath:='';
    j:=0;
    while buf[j]<>#0 do begin usbPath:=usbPath+buf[j]; inc(j); end;
    fPaths.Add(usbPath);
    fIsHWInput[i]:=Pos('\\?\usb',usbPath)=1;
//    fIsHWInput[i]:=fIsHWInput[i] or (Pos('nanoKONTROL',DeviceName)=1);
    fDevices.Add(DeviceName+CheckForMidiMix(DeviceName,usbPath))
  except end;
end;

procedure TMidiInputDevices.Open(aDeviceIndex: integer);
var
  lHandle: THandle;
  lSysExData: TCSysExData;
begin
  if GetHandle(aDeviceIndex) <> 0 then
    Exit;

  SetMidiResult( midiInOpen(@lHandle, aDeviceIndex, cardinal(@midiInCallback), aDeviceIndex, CALLBACK_FUNCTION));
  fDevices.Objects[aDeviceIndex] := TObject(lHandle);
  lSysExData := TCSysExData(fSysExData[aDeviceIndex]);

  lSysExData.SysExHeader.dwFlags := 0;

  SetMidiResult( midiInPrepareHeader(lHandle, @lSysExData.SysExHeader,
    SizeOf(TMidiHdr)));
  SetMidiResult( midiInAddBuffer(lHandle, @lSysExData.SysExHeader,
    SizeOf(TMidiHdr)));
  SetMidiResult( midiInStart(lHandle));
end;

procedure TMidiInputDevices.DoSysExData(aDeviceIndex: integer);
var
  lSysExData: TCSysExData;
  syx:string;
  size,i:integer;
  b:byte;
begin
  lSysExData := TCSysExData(fSysExData[aDeviceIndex]);
  if lSysExData.SysExHeader.dwBytesRecorded = 0 then
    Exit;

  lSysExData.SysExStream.Write(lSysExData.SysExData,
    lSysExData.SysExHeader.dwBytesRecorded);
  if lSysExData.SysExHeader.dwFlags and MHDR_DONE = MHDR_DONE then
  begin
    lSysExData.SysExStream.Position := 0;
    size:=lSysExData.SysExStream.Size;
    syx:='';
    for i:=0 to size - 1 do
      begin
        lSysExData.SysExStream.Read(b,1);
        syx:=syx+chr(b);
      end;
    if size>0 then
      onSysexData(aDeviceIndex,syx);
    lSysExData.SysExStream.Clear;
  end;

  lSysExData.SysExHeader.dwBytesRecorded := 0;
  SetMidiResult( midiInPrepareHeader(GetHandle(aDeviceIndex),
    @lSysExData.SysExHeader, SizeOf(TMidiHdr)));
  SetMidiResult( midiInAddBuffer(GetHandle(aDeviceIndex),
    @lSysExData.SysExHeader, SizeOf(TMidiHdr)));
end;

function TMidiInputDevices.GetHandle(const aDeviceIndex: integer): THandle;
begin
  if not InRange(aDeviceIndex, 0, fDevices.Count - 1) then
    raise EMidiDevices.CreateFmt('%s: Device index out of bounds! (%d)',
      [ClassName, aDeviceIndex]);

  Result := THandle(fDevices.Objects[aDeviceIndex]);
end;

function TMidiInputDevices.IsHWDevice(aDeviceIndex: integer): boolean;
begin
  result:=fIsHWInput[aDeviceIndex];
end;

destructor TMidiInputDevices.Destroy;
begin
  FreeAndNil(fSysExData);
  inherited;
end;

{ TMidiOutput }

procedure TMidiOutputDevices.Close(aDeviceIndex: integer);
VAR handle:THandle;
begin
  inherited;
  Handle:=GetHandle(aDeviceIndex);
  SetMidiResult( midiOutClose(Handle));
  fDevices.Objects[aDeviceIndex] := nil;
end;

constructor TMidiOutputDevices.Create;
var
  i,j: integer;
  lOutCaps: TMidiOutCaps;
  buf: array[0..255] of char;
  usbPath,DeviceName:string;
begin
  inherited;
  for i := 0 to midiOutGetNumDevs - 1 do
  begin
    SetMidiResult( midiOutGetDevCaps(i, @lOutCaps, SizeOf(TMidiOutCaps)));
    DeviceName:=Nice(lOutCaps.szPname);
    midiOutMessage (i,$80c,integer(@buf),256);
    usbPath:='';
    j:=0;
    while buf[j]<>#0 do begin usbPath:=usbPath+buf[j]; inc(j); end;
    fPaths.Add(usbPath);
    fDevices.Add(DeviceName+CheckForMidiMix(DeviceName,usbPath));
  end;
end;

procedure TMidiOutputDevices.Open(aDeviceIndex: integer);
var
  lHandle: THandle;
begin
  inherited;
  // device already open;
  if GetHandle(aDeviceIndex) <> 0 then
    Exit;

  SetMidiResult( midiOutOpen(@lHandle, aDeviceIndex, 0, 0, CALLBACK_NULL));
  fDevices.Objects[aDeviceIndex] := TObject(lHandle);
end;

procedure TMidiOutputDevices.WriteMidi( aDeviceIndex: integer; aStatus, aData1, aData2: byte);
var
  lMsg: cardinal;
begin
  // open the device is not open
  if not assigned(fDevices.Objects[aDeviceIndex]) then
    Open(aDeviceIndex);

  lMsg := aStatus + (aData1 * $100) + (aData2 * $10000);
  SetMidiResult( midiOutShortMsg(GetHandle(aDeviceIndex), lMsg));
end;


function TMidiOutputDevices.GetHandle(const aDeviceIndex: integer): THandle;
begin
  if not InRange(aDeviceIndex, 0, fDevices.Count - 1) then
    raise EMidiDevices.CreateFmt('%s: Device index out of bounds! (%d)',
      [ClassName, aDeviceIndex]);

  Result := THandle(fDevices.Objects[aDeviceIndex]);
end;


procedure TMidiOutputDevices.WriteSysEx(aDeviceIndex: integer; syx:string);
var
  lSysExHeader: TMidiHdr;
  buffer:array of byte;
  i,len:integer;
begin
  len:=length(syx);
  setLength(buffer,len);
  for i:=0 to len-1 do
    buffer[i]:=ord(syx[i+1]);
  lSysExHeader.dwBufferLength := len;
  lSysExHeader.lpData := @buffer[0];
  lSysExHeader.dwFlags := 0;

  SetMidiResult( midiOutPrepareHeader(GetHandle(aDeviceIndex), @lSysExHeader,
    SizeOf(TMidiHdr)));
  SetMidiResult( midiOutLongMsg(GetHandle(aDeviceIndex), @lSysExHeader,
    SizeOf(TMidiHdr)));
  SetMidiResult( midiOutUnprepareHeader(GetHandle(aDeviceIndex), @lSysExHeader,
    SizeOf(TMidiHdr)));
end;

{ TCSysExData }

constructor TCSysExData.Create;
begin
  SysExHeader.dwBufferLength := cSysExBufferSize;
  SysExHeader.lpData := SysExData;
  fSysExStream := TMemoryStream.Create;
end;

destructor TCSysExData.Destroy;
begin
  FreeAndNil(fSysExStream);
end;


Initialization
begin
  MidiInput:=TMidiInputDevices.Create;
  TMidiDeviceTypeManager.Create(MidiInput,TMidiOutputDevices.Create);
end;

end.
