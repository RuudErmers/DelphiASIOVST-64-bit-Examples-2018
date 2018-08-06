unit UMidiPorts;

interface

uses System.Classes,UMidiEvent,UMidinrpn, ExtCtrls, Generics.Collections;

const MAXMIDIPORT = 63;

type TMidiInPort = class;
     TMidiChannel= integer;
     EmidiFlags = (midiRealTime,midiSysExNoTRanslation);
     sMidiFlags = set of EmidiFlags;
(**************** SW Devices *******************)
     TMidiInPort = class  (TMidiPort)
  private
    FonMidiData:TonMidiData;
    Nrpn:TNrpnIn;
    MidiInIndex:integer;
    FSustainDownTime:integer;
    FPrevSustainTime:Int64;
    FFirstSustain,FSustainReversed:boolean;
    function FisOpen:boolean;
    procedure DoMidiData(const midievent:TMidiEvent);
    procedure DoSysExData(const data:TSysExData);
  public
     Flags:sMidiFlags;
     function OpenByIndex(index:integer):boolean;
     function Open(inname:string;partialok:boolean=false;forceIndex:integer=-1):boolean;overload;
     procedure Close;
     function CheckSustain(data2:integer):integer;
     function ConnectStatus: string;virtual;
     constructor Create;virtual;
     destructor Destroy;override;
     property OnMidiData: TonMidiData read FonMidiData write FonMidiData;
     property IsOpen: boolean read FisOpen;
    end;
     TMidiOutPort = class (TMidiPort)
  private
     MidiOutIndex:integer;
     Nrpn:TNrpnOut;
    function FisOpen:boolean;
  public
     constructor Create;
     function Open(outname:string;partialok:boolean=false):boolean;overload;
     function OpenByIndex(index:integer):boolean;
     procedure Close;
     destructor Destroy;override;
     procedure WriteMidi(midiEvent:TmidiEvent);
     procedure WriteBCL(sl:TStringList);
     procedure WriteSysEx(const data:TSysExData);overload;
     procedure WriteSysEx(const midiEvent:TmidiEvent);overload;
     procedure WriteSysEx(const Args: array of const);overload;
     property IsOpen: boolean read FisOpen;
     function ConnectStatus: string;
    end;
  TOnDetectHWChange = procedure of object;

(*************** HW Devices *********************)
     TMidiInDevice = class
     private
       FdeviceIndex:integer;
     protected
     public
       constructor Create;virtual;
       function Open:boolean;virtual;abstract;
       procedure Close;virtual;
       function IsHWDevice:boolean;virtual;
       function Name:string;virtual;abstract;
       function Path:string;virtual;abstract;
       function ConnectStatus:string;virtual;
       procedure DoMidiData(status,data1,data2:integer);
       procedure DoSysExData(syx:string);
      end;
     TMidiOutDevice = class
     private
       FdeviceIndex:integer;
     public
       constructor Create;virtual;
       function Open:boolean;virtual;abstract;
       procedure Close;virtual;
       function Name:string;virtual;abstract;
       function Path:string;virtual;abstract;
       procedure WriteSysEx(syx:string);virtual;
       procedure WriteMidi(status, data1, data2: integer);virtual;abstract;
       function ConnectStatus:string;virtual;
      end;

(***********************************************************************)
type TMidiDeviceTypeManager = class;
     TmidiInManagedDevice = class(TMidiInDevice)
  FWinIndex:integer;
  Fmanager: TMidiDeviceTypeManager;
  constructor Create(winIndex:integer;manager:TMidiDeviceTypeManager);
  function Open:boolean;override;
  procedure Close;override;
  function IsHWDevice:boolean;override;
  function Name:string;override;
  function Path:string;override;
end;

    TmidiOutManagedDevice = class(TMidiOutDevice)
  FWinIndex:integer;
  Fmanager: TMidiDeviceTypeManager;
  constructor Create(winIndex:integer;manager:TMidiDeviceTypeManager);
  function Open:boolean;override;
  procedure Close;override;
  function Name:string;override;
  function Path:string;override;
  procedure WriteSysEx(syx:string);override;
  procedure WriteMidi(status, data1, data2: integer);override;
end;
  TonMidiDataBytes = procedure (index,status,data1,data2:integer) of object;
  TonSysexDataBytes = procedure (index:integer;syx:string) of object;
  TMidiIOBase = class
  private
  protected
    fDevices: TStringList;
    fPaths: Tstringlist;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Open(aDeviceIndex: integer); virtual; abstract;
    procedure Close(aDeviceIndex: integer); virtual; abstract;
    procedure CloseAll;
    property Devices: TStringList read fDevices;
    property Paths: TStringlist read FPaths;
    function ConnectStatus(id: integer): string;virtual;
  end;

    TMidiInputsBase = class(TMidiIOBase)
  public
    onMidiData:TonMididataBytes;
    onSysexData:TonSysexdataBytes;
    function IsHWDevice( aDeviceIndex: integer):boolean; virtual;abstract;
  end;
  TMidiOutputsBase = class(TMidiIOBase)
    procedure WriteMidi( aDeviceIndex: integer; aStatus, aData1, aData2: byte);virtual;abstract;
    procedure WriteSysEx( aDeviceIndex: integer;  syx:string);virtual;abstract;
  end;

    TMidiDeviceTypeManager = class
  private

    FMidiInputs:TMidiInputsBase;
    FMidiOutputs: TMidiOutputsBase;
    FmidiInPortDef:TList<TmidiInManagedDevice>;
    FmidiOutPortDef:TList<TmidiOutManagedDevice>;

    procedure DoOnMidiData(index,status,data1,data2:integer);
    procedure DoOnSysexData(index:integer; syx:string);
    procedure OpenInput(winIndex: integer);
    procedure OpenOutput(winIndex: integer);
  public
    constructor Create(MidiInputs:TMidiInputsBase;  MidiOutputs: TMidiOutputsBase);
    destructor destroy; override;
end;
(*********************************************************************************)


  TMidiPortManager = class
  private
     InPorts:array[0..MAXMIDIPORT] of TMidiInPort;
     OutPorts:array[0..MAXMIDIPORT] of TMidiOutPort;
     InPortDefinition:TList<TMidiInDevice>;
     OutPortDefinition:TList<TMidiOutDevice>;
     FDetectHWChanges:boolean;
     FMidiInNames,FMidiOutNames:TstringList;
     constructor create;
     procedure DoMidiData(aDeviceIndex:integer; aStatus, aData1, aData2: byte);
     procedure DoSysExData(aDeviceIndex:integer;syx:string);

    function ConnectStatus(port: TMidiInPort): string;overload;
    function ConnectStatus(port: TMidiOutPort): string;overload;
     procedure WriteSysEx(aDeviceIndex:integer; syx:string);
     procedure WriteMidi(aDeviceIndex:integer; status,data1,data2:integer);

   strict private
    procedure Attach(port:TMidiInPort);overload;
    procedure Detach(port:TMidiInPort);overload;
    procedure Attach(port:TMidiOutPort);overload;
    procedure Detach(port:TMidiOutPort);overload;

  public
     destructor Destroy;override;
    procedure CloseAll;
    procedure OpenHWDevices(onMidiIn: TonMidiData);
    function  OpenInputByIndex(port:TMidiInPort;index: integer): boolean;
    function  OpenInputByName(port:TMidiInPort;inname: string;partialok:boolean;forceIndex:integer):boolean;
    function  OpenOutputByIndex(port:TMidiOutPort;index: integer): boolean;
    function  OpenOutputByName(port:TMidiOutPort;outname: string;partialok:boolean;forceIndex:integer):boolean;
    procedure CloseInput(port:TMidiInPort);
    procedure CloseOutput(port:TMidiOutPort);
    function  IsHWDevice(const aDeviceIndex:integer):boolean;
    function MidiOutNames:TStringList;
    function MidiInNames:TStringList;
    procedure AddInputPort(port:TMidiInDevice);
    procedure AddOutputPort(port: TMidiOutDevice);
  end;

function MidiPortManager:TMidiPortManager;
implementation

uses System.SysUtils,Windows;

VAR _IMidiPortManager:TMidiPortManager;

const   MIDI_START = $FA;
const   MIDI_CONTINUE = $FB;
const   MIDI_TICK = $F8;
const   MIDI_STOP = $FC;
const   MIDI_SYSEX = $F0;
const   MIDI_CC = $B0;

function MidiPortManager:TMidiPortManager;
begin
  if _IMidiPortManager = NIL then
  _IMidiPortManager:=TMidiPortManager.create;
  result:=_IMidiPortManager;
end;

procedure TMidiPortManager.Attach(port: TMidiInPort);
begin
  InPorts[port.MidiInIndex]:=port;
end;

procedure TMidiPortManager.AddInputPort(port: TMidiInDevice);
begin
  port.FdeviceIndex:=InPortDefinition.Count;
  InPortDefinition.Add(port);
end;

procedure TMidiPortManager.AddOutputPort(port: TMidiOutDevice);
begin
  port.FdeviceIndex:=OutPortDefinition.Count;
  OutPortDefinition.Add(port);
end;


procedure TMidiPortManager.Attach(port: TMidiOutPort);
begin
  OutPorts[port.MidiOutIndex]:=port;
end;

procedure TMidiPortManager.CloseAll;
VAR i:integer;
begin
  for i:=0 to MAXMIDIPORT do
  begin
    if InPorts[i]<>NIL then  InPorts[i].Close;
    if OutPorts[i]<>NIL then  OutPorts[i].Close;
  end;
end;

procedure TMidiPortManager.CloseInput(port: TMidiInPort);
begin
  if (port.MidiInIndex >= 0) and (port.MidiInIndex<InportDefinition.Count) then
  try
    InportDefinition[port.MidiInIndex].close;
  except
  end;
  Detach(port);
  port.MidiInIndex := -1;
end;

procedure TMidiPortManager.CloseOutput(port: TMidiOutPort);
begin
  if (port.MidiOutIndex >= 0) and (port.MidiOutIndex<OutportDefinition.Count) then
  try
    OutPortDefinition[port.MidiOutIndex].close;
  except end;
  Detach(port);
  port.MidiOutindex := -1;
end;

constructor TMidiPortManager.create;
begin
  InPortDefinition:=TList<TMidiInDevice>.Create;
  OutPortDefinition:=TList<TMidiOutDevice>.Create;
end;

destructor TMidiPortManager.Destroy;
begin
  CloseAll;
  inherited;
end;

procedure TMidiPortManager.Detach(port: TMidiOutPort);
begin
  if port.MidiOutIndex>=0 then
    OutPorts[port.MidiOutIndex]:=NIL;
end;

procedure TMidiPortManager.Detach(port: TMidiInPort);
begin
  if port.MidiInIndex>=0 then
    InPorts[port.MidiInIndex]:=NIL;
end;

procedure TMidiPortManager.DoSysExData(aDeviceIndex:integer;syx:string);
var
  port:TMidiInPort;
  data:TSysExData;
begin
  port:=InPorts[aDeviceIndex];
  if port<>NIL then
    port.DoSysExData(StrToSysex(syx));
end;

procedure TMidiPortManager.DoMidiData( aDeviceIndex:integer; aStatus, aData1, aData2: byte);
var
  port:TMidiInPort;
  status,midichannel,Data2:integer;
begin
  if aDeviceIndex>MAXMIDIPORT then raise Exception.Create('Too Many Midiparts');
  port:=InPorts[aDeviceIndex];
  if port=NIL then exit;
  if aStatus >= $F0 then
  begin
    if not ( midiRealTime in port.Flags ) then exit;
    if (aStatus<>MIDI_START) and (aStatus<>MIDI_STOP) and (aStatus<>MIDI_CONTINUE) and (aStatus<>MIDI_TICK) then exit;
    status:=aStatus;
//    if status = MIDI_STOP then
//      debug:=1;
    midichannel:=0;
  end
  else
  begin
    midichannel:=aStatus and $F;
    Status:=aStatus and $F0;
  end;

  if (pos('CME',MidiInNames[aDeviceIndex])>0) and (status = MIDI_CC) and (aData1 = 27) then exit;
  if (status = MIDI_CC) and (aData1 = 64) then
    Data2:=port.CheckSustain(aData2)
  else
    Data2:=aData2; // solves const 'problem'
  port.DoMidiData(MidiEvent(midichannel, Status, aData1, Data2));
end;

function TMidiPortManager.IsHWDevice(const aDeviceIndex: integer): boolean;
begin
  result:=InPortDefinition[aDeviceIndex].IsHWDevice;
end;

function TMidiPortManager.MidiInNames: TStringList;
VAR i:integer;
begin
  if FmidiInNames = NIL then
  begin
    FmidiInNames:=TStringList.Create;
    for i:=0 to InPortDefinition.Count-1 do
      FmidiInNames.Add(InPortDefinition[i].Name);
  end;
  result:=FmidiInNames;
end;

function TMidiPortManager.MidiOutNames: TStringList;
VAR i:integer;
begin
  if FmidiOutNames = NIL then
  begin
    FmidiOutNames:=TStringList.Create;
    for i:=0 to OutPortDefinition.Count-1 do
      FmidiOutNames.Add(OutPortDefinition[i].Name);
  end;
  result:=FmidiOutNames;
end;

procedure TMidiPortManager.OpenHWDevices(onMidiIn: TonMidiData);
VAR i:integer;
begin
  for i:=0 to InPortDefinition.Count-1 do
  begin
    InPorts[i]:=TMidiInPort.Create;
    InPorts[i].OnMidiData:=onMidiIn;
    if InPortDefinition[i].IsHWDevice then
      OpenInputByIndex(InPorts[i],i);
  end;
end;

function TMidiPortManager.OpenInputByIndex(port:TMidiInPort;index: integer): boolean;
begin
  result:=false;
  if (index>=0) and (index<InPortDefinition.Count) then
  try
    InPortDefinition[index].Open;
    port.Name:=InPortDefinition[index].Name;
    port.MidiInIndex := index;
    Attach(port);
    port.Path:=InPortDefinition[index].Path;
    result:=true;
  except end;
end;

function TMidiPortManager.OpenInputByName(port: TMidiInPort; inname: string;
  partialok: boolean; forceIndex: integer): boolean;
VAR i,index:integer;
begin
  result:=false;
//??  Name:=inname;
  index:=forceIndex;
  if index = -1 then
  for i := 0 to InPortDefinition.Count - 1 do
    if (UpperCase(inname)=Uppercase(InPortDefinition[i].Name)) or (partialOk and (0<>pos(UpperCase(inname),Uppercase(InPortDefinition[i].Name)))) then
    begin
      index:=i;
      break;
    end;
  if index<>-1 then
        result:=OpenInputByIndex(port,index);
end;

function TMidiPortManager.OpenOutputByName(port: TMidiOutPort; outname: string;
  partialok: boolean; forceIndex: integer): boolean;
VAR i,index:integer;
begin
  result:=false;
//??  Name:=inname;
  index:=forceIndex;
  if index = -1 then
  for i := 0 to OutPortDefinition.Count - 1 do
    if (UpperCase(outname)=Uppercase(OutPortDefinition[i].Name)) or (partialOk and (0<>pos(UpperCase(outname),OutPortDefinition[i].Name))) then
    begin
      index:=i;
      break;
    end;
  if index<>-1 then
        result:=OpenOutputByIndex(port,index);

end;


function TMidiPortManager.OpenOutputByIndex(port: TMidiOutPort;  index: integer): boolean;
begin
  result:=false;
  port.Close;
  if (index>=0) and (index<OutPortDefinition.Count) then
  try
    OutPortDefinition[index].Open;
    port.name:=OutPortDefinition[index].Name;
    port.MidiOutIndex := index;
    Attach(port);
    port.Path:=OutPortDefinition[index].Path;
    result:=true;
  except end;
end;


procedure TMidiPortManager.WriteMidi(aDeviceIndex: integer; status, data1, data2: integer);
begin
  OutPortDefinition[aDeviceIndex].WriteMidi(status,data1,data2);
end;

procedure TMidiPortManager.WriteSysEx(aDeviceIndex: integer; syx: string);
begin
  OutPortDefinition[aDeviceIndex].WriteSysEx(syx);
end;

function TMidiPortManager.ConnectStatus(port:TMidiInPort): string;
begin
  if not port.IsOpen then result:='Not Connected'
  else
    result:=InPortDefinition[port.MidiInIndex].ConnectStatus;
end;

function TMidiPortManager.ConnectStatus(port: TMidiOutPort): string;
begin
  if not port.IsOpen then result:='Not Connected'
  else
    result:=OutPortDefinition[port.MidiOutIndex].ConnectStatus;
end;

{ TMidiInPort }

constructor TMidiInPort.Create;
begin
  MidiInIndex := -1;
  Nrpn:=TNrpnIn.Create;
end;

procedure TMidiInPort.DoMidiData(const midievent:TMidiEvent);
VAR s:string;
begin
  s:=Name;

  Nrpn.MidiIn(midievent, procedure (const midievent:TMidiEvent)
                         begin
                           if assigned(FonMidiData) then
                             FonMidiData(self,MidiEvent);
                         end);
end;

function TMidiInPort.FisOpen: boolean;
begin
  result:=MidiInIndex<>-1;
end;

procedure TMidiInPort.DoSysExData(const data: TSysExData);
VAR midiEvent:TMidiEvent;
    procedure assigntomidievent;
    VAR i:integer;
        s:string;
    begin
      s:='';
      for i:=0 to length(data)-1 do s:=s+chr(data[i]);
      midievent.dataS:=s;
      midievent.status:=MIDI_SYSEX;
    end;
begin
  if assigned(FonMidiData) then
    if (length(data)>0) then
  begin
    if (midiSysExNoTranslation in Flags) or not midievent.fromSysEx(data) then
        assigntomidievent;
      FonMidiData(self,MidiEvent)
  end;
end;

destructor TMidiInPort.Destroy;
begin
  Close;
  inherited;
end;

function TMidiInPort.OpenByIndex(index: integer): boolean;
begin
  result:=MidiPortManager.OpenInputByIndex(self,index);
  FFirstSustain:=true;
end;

function TMidiInPort.Open(inname: string;partialok:boolean;forceIndex:integer):boolean;
begin
  result:=MidiPortManager.OpenInputByName(self,inname,partialok,forceIndex);
end;

function TMidiInPort.CheckSustain(data2: integer): integer;
VAR period:Int64;
begin
  if FFirstSustain then
  begin
    FFirstSustain:=False;
    FSustainReversed:=data2=0;
  end
  else
  begin
    Period:=getTickCount64 - FPrevSustainTime;
    if data2=0 then begin if FSustainDownTime<1000000 then FSustainDownTime:=FSustainDownTime + Period end
               else begin if FSustainDownTime>-1000000 then FSustainDownTime:=FSustainDownTime - Period; end;
    if abs(FSustainDownTime) > 5000 then
      FSustainReversed:=FSustainDownTime > 0 ;
  end;
  FPrevSustainTime:=getTickCount64;
  if FSustainReversed then result:=127-data2 else result:=data2
end;

procedure TMidiInPort.Close;
begin
  MidiPortManager.CloseInput(self);
end;

function TMidiInPort.ConnectStatus: string;
begin
  result:=MidiPortManager.ConnectStatus(self);
end;

{ TMidiOutPort }

procedure TMidiOutPort.Close;
begin
  MidiPortManager.CloseOutput(self);
end;

constructor TMidiOutPort.Create;
begin
  MidiOutIndex:=-3;   // de zin hievan ontgaat mij even...
  Nrpn:=TNrpnOut.Create;
end;

destructor TMidiOutPort.Destroy;
begin
  Close;
  inherited;
end;

function TMidiOutPort.FisOpen: boolean;
begin
  result:=MidiOutIndex>=0;
end;

function TMidiOutPort.OpenByIndex(index: integer): boolean;
begin
  result:=MidiPortManager.OpenOutputByIndex(self,index);
end;

function TMidiOutPort.Open(outname: string;partialok:boolean):boolean;
begin
  result:=MidiPortManager.OpenOutputByName(self,outname,partialok,-1);
end;

procedure TMidiOutPort.WriteBCL(sl: TStringList);
  procedure WriteBCLLine(s:string;index:integer);
  VAR data: TSysExData;
    i:integer;
  begin
    setLength(data,length(s)+10);
    for i:=0 to length(s)-1 do
      data[i+9]:=ord(s[i+1]);
    data[0]:=$F0;            // SysEx start
    data[1]:=$00;            // Behringer
    data[2]:=$20;            // Behringer
    data[3]:=$32;            // Behringer
    data[4]:=$7F;            // Device
    data[5]:=$7F;            // Model
    data[6]:=$20;            // BCL Message
    data[7]:=index DIV 128;  // Index MSB
    data[8]:=index MOD 128;  // Index LCB

    data[length(data)-1]:=$F7;   // SysEx End

    if MidiOutIndex >=0 then
      MidiPortManager.WriteSysEx(MidiOutIndex,SysExToStr(data));
  end;
VAR i:integer;
begin
  if MidiOutIndex < 0 then exit;
  for i:=0 to sl.Count-1 do
  begin
    WriteBCLLine(sl[i],i);
    Sleep(20);
  end;
end;

procedure TMidiOutPort.WriteMidi(midiEvent:TmidiEvent);
VAR handleNormal,handleSysEx:boolean;
  procedure SendSysExRaw;
  VAR data: TSysExData;
    i:integer;
  begin
    with midievent do
    begin
      setLength(data,length(dataS));
      for i:=1 to length(dataS) do data[i-1]:=ord(dataS[i]);
      WriteSysEx(data);
    end;
  end;
begin
  if MidiOutIndex < 0 then exit;
  try
  with midievent do
  begin
    if status = MIDI_SYSEX then begin SendSysExRaw; exit; end;
    handleSysEx:=(dataS<>'') or (midichannel>=16) or (data1>127) or (data2>127);
    handleNormal:=(dataS='') and (data2<=127) and ((status = MIDI_CC) or (midiChannel<16));
    if handleSysEx  then
    begin
      WriteSysEx(midievent.toSysEx);
    end;
    if handleNormal then
      Nrpn.MidiOut(midievent,procedure (const midievent:TMidiEvent)
                             begin
                               with midievent do
                                 MidiPortManager.WriteMidi(MidiOutIndex, status OR midichannel,data1,data2);
                             end);
  end;
  except end;
end;

procedure TMidiOutPort.WriteSysEx(const Args: array of const);
VAR sysex:TSysExData;
    i:integer;
begin
  SetLength(sysEx,High(Args)+1);
  for i:=0 to High(Args) do
    sysEx[i]:=Args[i].VInteger;
  WriteSysEx(sysEx);
end;

procedure TMidiOutPort.WriteSysEx(const midiEvent: TmidiEvent);
begin
  WriteSysEx(midievent.toSysEx);
end;

procedure TMidiOutPort.WriteSysEx(const data:TSysExData);
begin
  if MidiOutIndex < 0 then exit;
  MidiPortManager.WriteSysEx(MidiOutIndex,SysExToStr(data));
end;

function TMidiOutPort.ConnectStatus: string;
begin
  result:=MidiPortManager.ConnectStatus(self);
end;


{ TMidiInDevice }

procedure TMidiInDevice.Close;
begin

end;

function TMidiInDevice.ConnectStatus: string;
begin

end;

constructor TMidiInDevice.Create;
begin
end;

procedure TMidiInDevice.DoSysExData(syx:string);
begin
  MidiPortManager.DoSysExData(FdeviceIndex,syx);
end;

procedure TMidiInDevice.DoMidiData(status, data1, data2: integer);
begin
  MidiPortManager.DoMidiData(FdeviceIndex,Status, Data1, Data2);
end;

function TMidiInDevice.IsHWDevice: boolean;
begin
  result:=false;
end;

{ TMidiOutDevice }

procedure TMidiOutDevice.Close;
begin

end;

function TMidiOutDevice.ConnectStatus: string;
begin

end;

constructor TMidiOutDevice.Create;
begin
end;


procedure TMidiOutDevice.WriteSysEx(syx:string);
begin
end;

constructor TMidiDeviceTypeManager.Create(MidiInputs:TMidiInputsBase;  MidiOutputs: TMidiOutputsBase);
VAR i:integer;
begin
  FMidiInputs:=MidiInputs;
  FMidiInputs.OnMidiData:=DoOnMidiData;
  FMidiInputs.onSysexData:=DoOnSysexData;
  FMidiOutputs:=MidiOutputs;
  FmidiInportDef:=TList<TmidiInManagedDevice>.Create;
  FmidiOutPortDef:=TList<TmidiOutManagedDevice>.Create;

  for i:=0 to FMidiInputs.Devices.Count-1 do
  begin
    FmidiInportDef.Add(TmidiInManagedDevice.Create(i,self));
    MidiPortManager.AddInputPort(FmidiInportDef[i]);
  end;
  for i:=0 to FMidiOutputs.Devices.Count-1 do
  begin
    FmidiOutportDef.Add(TmidiOutManagedDevice.Create(i,self));
    MidiPortManager.AddOutputPort(FmidiOutportDef[i]);
  end;
end;

destructor TMidiDeviceTypeManager.destroy;
begin
  FreeAndNil(FMidiInputs);
  FreeAndNil(FMidiOutputs);
end;

procedure TMidiDeviceTypeManager.DoOnMidiData(index,status,data1,data2:integer);
begin
  FmidiInportDef[Index].DoMidiData(status,data1,data2);
end;

procedure TMidiDeviceTypeManager.DoOnSysexData(index: integer; syx:string);
begin
  FmidiInportDef[Index].DoSysexData(syx);
end;

procedure TMidiDeviceTypeManager.OpenInput(winIndex: integer);
begin
  FMidiInputs.Open(winIndex);
end;

procedure TMidiDeviceTypeManager.OpenOutput(winIndex: integer);
begin
  FMidiOutputs.Open(winIndex);
end;

{ TmidiInManagedDevice }

procedure TmidiInManagedDevice.Close;
begin
  inherited;
  FManager.FMidiInputs.Close(FWinIndex);
end;

constructor TmidiInManagedDevice.Create(winIndex: integer;manager:TMidiDeviceTypeManager);
begin
  inherited Create;
  FManager:=manager;
  FWinIndex:=winIndex;
end;

function TmidiInManagedDevice.IsHWDevice: boolean;
begin
  result:=FManager.FMidiInputs.IsHWDevice(FWinIndex);
end;

function TmidiInManagedDevice.Name: string;
begin
  result:=FManager.FMidiInputs.Devices[FWinIndex];
end;

function TmidiInManagedDevice.Open: boolean;
begin
  FManager.OpenInput(FWinIndex);
  result:=true;
end;

function TmidiInManagedDevice.Path: string;
begin
  result:=FManager.FMidiInputs.Paths[FWinIndex];
end;


{ TmidiOutManagedDevice }
procedure TmidiOutManagedDevice.Close;
begin
  inherited;
  FManager.FMidiOutputs.Close(FWinIndex);
end;

constructor TmidiOutManagedDevice.Create(winIndex: integer;manager:TMidiDeviceTypeManager);
begin
  inherited Create;
  FManager:=manager;
  FWinIndex:=winIndex;
end;

function TmidiOutManagedDevice.Name: string;
begin
  result:=FManager.FMidiOutputs.Devices[FWinIndex];
end;

function TmidiOutManagedDevice.Open: boolean;
begin
  FManager.OpenOutput(FWinIndex);
  result:=true;
end;

function TmidiOutManagedDevice.Path: string;
begin
  result:=FManager.FMidiOutputs.Paths[FWinIndex];
end;

procedure TmidiOutManagedDevice.WriteMidi(status, data1, data2: integer);
begin
  FManager.FMidiOutputs.WriteMidi(FWinIndex,status, data1, data2);
end;

procedure TmidiOutManagedDevice.WriteSysEx(syx:string);
begin
  FManager.FMidiOutputs.WriteSysEx(FWinIndex,syx);
end;


{ TMidiIOBase }

procedure TMidiIOBase.CloseAll;
var i: integer;
begin
  for i := 0 to fDevices.Count - 1 do
    Close(i);
end;

function TMidiIOBase.ConnectStatus(id: integer): string;
begin
  result:='connected';
end;

constructor TMidiIOBase.Create;
begin
  fDevices := TStringList.Create;
  fPaths   := TStringList.Create;
end;

destructor TMidiIOBase.Destroy;
begin
  FreeAndNil(fDevices);
end;

Initialization
begin
end;

Finalization
  if _IMidiPortManager<>NIL then
  _IMidiPortManager.CloseAll;
end.



