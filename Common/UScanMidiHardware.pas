unit UScanMidiHardware;

interface

uses UMidiIODevices, System.Classes,UMidiPorts,UMidiEvent;

type TScanMidiHardware = class
  private
     FCheckDevice:integer;
    procedure onTestMidiData(Sender: TMidiPort; const midiEvent: TMidiEvent);
    function Lookup(substr: string; exactMatch: boolean): string;
     constructor create;
  public
     procedure RescanDevices;
end;

procedure ScanMidiHardware;

implementation

uses System.SysUtils;

{ TMidiIOManager }

procedure ScanMidiHardware;
begin
  TScanMidiHardware.create.Free;
end;

constructor TScanMidiHardware.create;
begin
  RescanDevices;
end;

procedure TScanMidiHardware.RescanDevices;
VAR oldfile:TStringlist;
   procedure ReplaceLine(id:TMidiIODeviceID;s:string);
   VAR j:integer;
   begin
     for j:=0 to oldfile.Count-1 do
      if 1=pos(midiIODeviceName[id],oldfile[j]) then
        oldfile[j]:=midiIODeviceName[id]+'~'+s;
   end;

   procedure CheckBehringers;
   VAR i:integer;
    data: TSysExData;
    ok:boolean;
    TestMidiInPort:  TMidiInPort;
    TestMidiOutPort: TMidiOutPort;
    procedure SetDevice(device:integer;s:string);
    VAR id:TMidiIODeviceID;
        idk:integer;
    begin
      idk:=-1;
      case device of
        4: id:= midiIO_BCR2000_Left;
        3: begin id:=midiIO_BCR2000_DKLeft; idk:=0; end;
        0: begin id:=midiIO_BCF2000_DKRight;idk:=1; end;
        1: id:=midiIO_BCR2000_Right;
      end;
      ReplaceLine(id,s);
      if idk<>-1 then
      begin
        s[length(s)]:='2'; // port 2
        ReplaceLine(TMidiIODeviceID(idk),s);
      end;
    end;
VAR midiInname:string;
begin
  for i:=0 to MidiPortManager.MidiInNames.Count-1 do
  begin
    midiInname:=MidiPortManager.MidiInNames[i];
    if pos('2000',MidiInName)>0 then
    begin
      TestMidiInPort:=TmidiInPort.Create;
      TestMidiInPort.Open(MidiInName);
      TestMidiInPort.OnMidiData:=onTestMidiData;
      TestMidiOutPort:=TmidiOutPort.Create;
      TestMidiOutPort.Open(MidiInName);
      setLength(data,8);
      data[0]:=$F0;            // SysEx start
      data[1]:=$00;            // Behringer
      data[2]:=$20;            // Behringer
      data[3]:=$32;            // Behringer
      data[4]:=$7F;            // Device
      data[5]:=$7F;            // Model
      data[6]:=$01;            // Request Identity
      data[7]:=$F7;   // SysEx End
      TestMidiOutPort.WriteSysEx(data);
      FCheckDevice:=-1;
      Sleep(100); // wait for reply...
      if FCheckDevice<>-1 then SetDevice(FCheckDevice,MidiInName);
      TestMidiInPort.Free;
      TestMidiOutPort.Free;
    end
  end;
end;
VAR keynameTop,s:string;
begin
  oldfile:=TStringlist.create;
  try
    oldFile.LoadFromFile(midiIODevicesFilename);
  except end;
  CheckBehringers;
(*
  s:=Lookup('Steinberg CMC-QC-2',false);
  if s<>'' then ReplaceLine(midiIO_ControllerLeft,s);
  s:=Lookup('Steinberg CMC-FD',false);
  if s<>'' then ReplaceLine(midiIO_ControllerRight,s);  *)
  keynameTop:=Lookup('CME UF',true);
//  if keynametop='' then
//    keynameTop:=Lookup('SAMSON Graphite 49',true);
  if keynameTop <>'' then
     ReplaceLine(midiIO_Keyboard(2),keynameTop);
  try
  oldFile.SaveToFile(midiIODevicesFilename);
  except end;
  oldFile.free;
end;


function TScanMidiHardware.Lookup(substr:string;exactMatch:boolean):string;
VAR i:integer;
   ok:boolean;
begin
  result:='';
  with MidiPortManager do
  for i:=0 to MidiInNames.Count-1 do
  begin
    if exactMatch then ok:=substr =MidiInNames[i]
                  else ok:=pos(substr,MidiInNames[i])>0;
    if ok then result:=MidiInNames[i]
  end;
end;

procedure TScanMidiHardware.onTestMidiData(Sender: TMidiPort; const midiEvent: TMidiEvent);
begin
  try
    FCheckDevice:=ord(midievent.dataS[5]);
  except end;
end;


begin
end.


