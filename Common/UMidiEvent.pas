unit UMidiEvent;

interface

uses
  System.Classes;

type TSysExData = array of byte;
type TMidiPort  = class
                    path:string;
                    // If you encounter this, you have to change something in your code... InternalId:integer; // used by SerialMidi to identify
                    Tag:integer;        // used by MidiIOManager
                    name:string;
                  end;
type TMidiEvent = record
                    midichannel:integer;
                    fxIndex:integer;
                    status,data1,data2:integer;
                    dataS:string;
                    function toString:string;
                    function toSysEx: TSysExData;
                    function fromSysEx(const data: TSysExData):boolean;
                    function isNoteOn:boolean;
                    function isNoteOff:boolean;
                    function isNoteEvent:boolean;
                    function toLongInt:integer;
                    procedure FromLongInt(l:integer);
                  end;
type TCMidiEvent = class(TObject)
                         midievent:TMidiEvent;
                         constructor Create(midievent:Tmidievent);
                        end;
     TonMidiData = procedure (Sender: TMidiPort; const midiEvent: TMidiEvent) of object;
     TonSysExData = procedure (Sender: TMidiPort; const data:TSysExData) of object;

function MidiEvent(midichannel,status:integer;data1:integer=0;data2:integer=0;fxindex:integer=0;dataS:string=''):TMidiEvent;overload;
function MidiEvent(sysEx:TSysExData):TMidiEvent;overload;
function DataBytes(status:integer):integer;
function SysExEvent(s:string;manufacturer:char):TSysExData;
function StrToSysEx(s:string):TSysExData;
function SysExToStr(syx:TSysExData):string;
type TMidiEventCCProc = procedure (cc,value:integer) of object;

const MIDI_NOTE_ON = $90;
  MIDI_NOTE_OFF = $80;
  MIDI_POLYPRESSURE = $A0;
  MIDI_CC = $B0;
  MIDI_PRG = $C0;
  MIDI_CHNLPRESSURE = $D0;
  MIDI_PITCHWHEEL = $E0;
  MIDI_SYSEX = $F0;
  MIDI_ALL_NOTES_OFF = 123;
  MIDI_START = $FA;
  MIDI_CONTINUE = $FB;
  MIDI_STOP = $FC;
  MIDI_TICK = $F8;

const MIDI_MIXER = 16;
const MIDI_TRANSPORT = 17;
const MIDI_FXCHANGE = 18;
const MIDI_UI = 19;
const MIDI_HOSTCMD = 20;
const MIDI_FXCMD = 21;
const MIDI_LCDTXT = 22;
const MIDI_S52VCC = 23;
const MIDI_S52RC = 24;
const MIDI_S52KEY = 25;

const FX_EFFECTS = 126;
const FX_MAIN = 127;


const CMD_OPEN_PLUGIN = 4;
const CMD_CLOSE_PLUGIN = 5;
const CMD_HOST_CAPTION = 29;
const CMD_HOST_ICON = 30;
const CMD_HOST_PatchName = 31;
const CMD_SETTEMPO = 0;
const CMD_STARTSEQUENCER = 1;
const CMD_STOPSEQUENCER = 2;
const CMD_STOPTOCONSOLE = 18;
const CMD_RESUMETOCONSOLE = 19;
const CMD_INVALIDATETRACK = 11;
const CMD_STOPPPCHANGES = 16;
const CMD_RESUMEPPCHANGES = 17;
const CMD_INSERT_CC = 20;
const CMD_INSERT_AFTERTOUCH = 21;
const CMD_GETPatchName = 3;
const CMD_UI_SHOWMAIN = 0;
const CMD_UI_HIDEMUCH = 2;
const CMD_TRACKVOLUME = 1;
const CMD_TRACKENABLE = 2;
const CMD_TRACKALLFXENABLE = 3;
const CMD_PROGRAM_CHANGE = 4;
const CMD_FXENABLE = 1;
const CMD_FXDISABLE = 0;

implementation

uses SysUtils;

function MidiEvent(midichannel,status:integer;data1:integer=0;data2:integer=0;fxindex:integer=0;dataS:string=''):TMidiEvent;
begin
  result.midichannel:=midichannel;
  result.status:=status;
  result.data1:=data1;
  result.data2:=data2;
  result.dataS:=dataS;
  result.fxIndex:=fxIndex;
end;

function MidiEvent(sysEx:TSysExData):TMidiEvent;
begin
  result.fromSysEx(sysex);
end;

{ TCMidiEvent }

constructor TCMidiEvent.Create(midievent: Tmidievent);
begin
  self.midievent:=midievent;
end;

{ TMidiEvent }
const MANUFACTURER_CRUMAR = $25;
function TMidiEvent.isNoteEvent: boolean;
begin
  result:=(status = MIDI_NOTE_ON) or (status = MIDI_NOTE_OFF);
end;

function TMidiEvent.isNoteOff: boolean;
begin
  result:=((status = MIDI_NOTE_ON) and (data2=0)) or (status = MIDI_NOTE_OFF);
end;

function TMidiEvent.isNoteOn:boolean;
begin
  result:=(status = MIDI_NOTE_ON) and (data2>0);
end;

function TMidiEvent.toLongInt: integer;
begin
  result:=midichannel+status+data1 SHL 8 + data2 SHL 16;
end;

function TMidiEvent.toString: string;
VAR i:integer;
begin
  if status = MIDI_SYSEX then
  begin
     result:='SysEx: '+ inttostr(length(dataS))+' bytes:';
     for i:=1 to length(dataS) do result:=result+inttohex(ord(dataS[i]),2)+',';
  end
  else
  begin
    result:='chn:'+inttostr(midichannel)+' status:' + inttostr(status)+ ' d1:'+inttostr(data1)+' d2:'+inttostr(data2);
    if dataS<>'' then result:=result+' '+dataS;
  end;
end;

function  EncodeSysex(const midievent:TMidiEvent):TSysExData;
VAR i,l:integer;
    data: TSysExData;
begin
  setLength(data,length(midievent.dataS)+12); // truncate later...
  with midievent do
  begin
    data[0]:=$F0;
    data[1]:=MANUFACTURER_CRUMAR;
    data[2]:=midichannel and $7F;
//    assert(status>=$80, 'STATUS INVALID');
    if (status>=$80) then
      data[3]:=status SHR 4
    else
      data[3]:=status and $7F;
    data[4]:=data1 MOD 128;
    data[5]:=data2 MOD 128;
    data[6]:=(data1 and $3FFF) DIV 128;
    data[7]:=(data2 and $3FFF) DIV 128;
    // now adjust buffer to correct length
    if dataS <> '' then setlength(data,9+length(dataS))
                   else begin
                          l:=8;
                          while data[l-1] = 0 do dec(l);
                          SetLength(data,l+1);  // Changed 20170323 from l+2 to l+1
                        end;
    for i:=1 to length(dataS) do data[7+i]:=ord(dataS[i]);
    data[length(data)-1]:=$F7;
  end;
  result:=data;
end;

function DecodeSysex(const data: TSysExData; VAR midievent:TMidiEvent):boolean;
   function Valid(index:integer):integer;
   begin
     if index<length(data)-1  // careful: -1 because of 0xF7 SysExEOF
     then
       result:=data[index]
     else result:=0;
   end;
VAR i:integer;
begin
  result:=false;
  if data[0]<>$F0 then exit;
  if data[1]<>MANUFACTURER_CRUMAR then exit;
  if data[length(data)-1]<>$F7 then exit;
  result:=true;
  with midievent do
  begin
    midichannel:=data[2];
    if data[3]<16 then
      status:=data[3] SHR 4
    else
      status:=data[3];
    dataS:='';
    data1:=valid(4)+128*valid(6);
    data2:=valid(5)+128*valid(7);
    if length(data)>9 then
      for i:=8 to length(data)-2 do dataS:=dataS+char(data[i]);
  end;
end;

procedure TMidiEvent.FromLongInt(l: integer);
begin
  midichannel:=l and $F;
  status:= l and $F0;
  data1:=(l SHR 8) and $7F;
  data2:=(l SHR 16) and $7F;
end;

function TMidiEvent.fromSysEx(const data: TSysExData):boolean;
begin
  result:=DecodeSysex(data,self);
end;

function TMidiEvent.toSysEx: TSysExData;
begin
  result:=EncodeSysex(self);
end;


function SysExEvent(s:string;manufacturer:char):TSysExData;
VAR  i:integer;
begin
  SetLength(result,length(s)+3);
  result[0]:=$F0;
  result[1]:=ord(manufacturer);
  for i:=1 to length(S) do
    result[1+i]:=ord(s[i]);
  result[2+length(s)]:=$F7;
end;

function DataBytes(status:integer):integer;
begin
  case status and $F0 of
    MIDI_PRG, MIDI_CHNLPRESSURE:   result:=1;
  else
    result:=2;
  end;
end;

function StrToSysEx(s:string):TSysExData;
VAR i:integer;
begin
  SetLength(result,length(s));
  for i:=0 to length(s)-1 do
    result[i]:=ord(s[i+1]);
end;
function SysExToStr(syx:TSysExData):string;
VAR i:integer;
begin
  result:='';
  for i:=0 to length(syx)-1 do
    result:=result+chr(syx[i]);
end;


end.
