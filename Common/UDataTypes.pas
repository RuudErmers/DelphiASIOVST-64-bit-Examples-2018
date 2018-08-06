unit UDataTypes;

interface

uses System.Classes, Vcl.Forms;

type
  TStepRange = 1 .. 8;
  TStepRangePlus0 = 0..8;
  TQuartNoteRange = 1..16; // one measure
  TQuartNoteRangePlus0 = 0..16;
  TChain = (sChain1, sChain2, sChain4);

  T0_7 = 0..7;
  T0_127 = 0..127;

  TMidiclock = (mcInternal, mcInternalAndSend, mcExternal);
  TCCArray = array[0..3] of integer;
  TInstrumentRange = 1..33;
  TKeyboardRange = (kr0110,kr1111,kr0111,kr1110);
  TDKViewMode = (dkInstrument,dkMaster);
  TInstrumentViewType = (vtPads,vtLeadSynth,vtModular);

  TMidiBits = UInt64;
  TEViewSettingsCommand = (vcToggleSequencerBig, vcShowPosition, vcHostTogglePosition, vcHostToggleShowPlugins,
                           vcSetInstrument, vcTogglePatchesView,  vcToggleSynthView);

  TClickMode = (cmDown,cmUpShort,cmUpLong,cmRepeat,cmTakesLong,cmRotated,cmNone);
  type TES52UtilCommand = (scFormSettings,scFormDrums,scToggleAkaiView, scCloseApp,scInvalidate,scPanic);


{$ifndef BUILDCOMPONENTS}
procedure ViewSetInstrument(instrument:Tinstrumentrange);
procedure ViewCommand(index:integer;command:TEViewSettingsCommand;data1:integer=0;data2:integer=0);
procedure S52Command(command:TES52UtilCommand);
procedure raiseError(s:string);

{$endif}


type IModelObserver = Tobject;
     ImodelObservant = TComponent;
     IImplementsInterface = TComponent;
type TVirtControllerArray = array[0..255] of integer;
     TPhysControllerArray = array[0..255] of integer;
     ArrayOfInteger = TArray<integer>;

const
(*
  uccToggleEditor=14; *)
  MIDI_NRPN = 2; // as defined by Cubase
  MIDI_REAPER = $52;
  MIDI_MIDDLEC = 60;

const MAX_EFFECT = 16; // keep this in sync UVirtCC.__MAX_EFFECT
      EFFECT_RECORDING = 14;
      EFFECT_PAUSING = 15;


const BEHRINGERLEFTVIEW = 0;
const LEADCONTROLLERVIEW = 1;  // Let op: Zijn OOK devices, zoals hieronder
const CURRENTINSTRUMENTVIEW = 2;
const BEHRINGERRIGHTVIEW = 3;

const WORKSTATION_CELLWIDTH = 100;     // Se also in RMCCurrInstrument!
  MIDI_DEVICE_W5 = 0;
  MIDI_DEVICE_D70 = 1;
  MIDI_DEVICE_LEAD = 2;
//  MIDI_DEVICE_AKAI_MOOG = 3;
//  MIDI_DEVICE_AKAI_EFFECTS = 4;


  MIDI_DEVICE_W5_STRING = 'Yamaha W5';
  MIDI_DEVICE_D70_STRING = 'Roland D70';
  MIDI_DEVICE_LEAD_STRING = 'Crumar DS2';
//  MIDI_DEVICE_AKAI_MOOG_STRING = 'Akai LPK25 Left';
//  MIDI_DEVICE_AKAI_EFFECTS_STRING = 'Akai LPK25 Left';


(*  MIDI_DEVICE_INPUT_CONTROLLER = 3; *)


type TMidiChannel = integer;
const S52_RMSMIDI_IN = 'Super52 RMS In';
const S52_RMSMIDI_OUT = 'Super52 RMS Out';
const S52_MIDITIMECODE = 'Super52 Timecode';

const S52_RC_SERVER_PORT = 4752;
const S52_RC_SLAVE_PORT =  4756;

const INSTRUMENT_UNIVERSESTART = 13;
CONST INSTRUMENT_UNIVERSEEND = 16;
const INSTRUMENT_EFFECTSSTART = 17;
const INSTRUMENT_EFFECTSEND = 24;

const INSTRUMENT_STDSTART = 1;
const INSTRUMENT_STDEND   = 16;
const INSTRUMENT_MASTERKEYBOARD0 = 5;
const INSTRUMENT_MASTERKEYBOARD1 = 7;
const INSTRUMENT_SUBKEYBOARD0 = 6;
const INSTRUMENT_SUBKEYBOARD1 = 8;
const INSTRUMENT_LEAD0 = 9;
const INSTRUMENT_BIGMOOG0 = 1;
const INSTRUMENT_FAKE_FULLRANGE = 25;
const INSTRUMENT_FAKE_FULLRANGE0 = 25;
const INSTRUMENT_FAKE_FULLRANGE1 = 26;
const INSTRUMENT_DRUMS = 27;
const INSTRUMENT_REVOX0 = 28;
const INSTRUMENT_REVOX1 = 29;
const INSTRUMENT_REVOX2 = 30;
const REVOX_COUNT = 3;
const REVOX_VOCODER = 2;
const INSTRUMENT_VOCODER = 30;   // Vocoder is also a Revox !
const INSTRUMENT_BASS  = 31;
const INSTRUMENT_FARFISA = 32;

const CURRENT_FX = $7F;
const CC_VCF = 74;



type
  TChannelRange = 1 .. 4;
const  DefaultLaneHeight = 140;
       DefaulLaneSmall = false;
       HGCount = 5;

type IFrameWithInit = interface
    procedure Init(n:integer;s:string);
    procedure OnShow;
  end;

type TPlayState = (psStop,psPause,psPlay,psContinue);
     TLSPlayMode = (lsRandom,lsSequencer,lsOneNote,lsArp);
     TARPMode = (arpRandom, arpUpDown,arpDown,arpUp);
     TLSSteps = 1..16;

function ToStopPlay(value:integer):TPlayState;
function instrarray_d70: ArrayOfInteger;
function instrarray_lead: ArrayOfInteger;
function instrarray_none: ArrayOfInteger;
function instrarray_w5: ArrayOfInteger;
function instrarray_w5_d70: ArrayOfInteger;
function instrarray_d70_w5: ArrayOfInteger;
function instrarray_bigmoog: ArrayOfInteger;
function instrarray_effects:ArrayOfInteger;
function instrarray_d70_short:ArrayOfInteger;
function instrarray_w5_short:ArrayOfInteger;
function instrarray_nonsmallmoog:ArrayOfInteger;
function instrarray_nond70_short:ArrayOfInteger;
function instrarray_nonw5_short:ArrayOfInteger;
function instrarray_w5_d70small:ArrayOfInteger;
function instrarray_smallmoog:ArrayOfInteger;


function DebugMode:boolean;

VAR RMCDefaultNrOfKnobs:integer;
function GetAttrib(sl:TstringList;s:string):string;overload;
function Getattrib(s,spart:string):string;overload;
function To0_127(s:string):integer;overload;
function To0_127(b:boolean):integer;overload;
function ThreeStr(n:integer):string;
function TwoStr(n: integer): string;
procedure bound(VAR m:integer;min,max:integer);
function GetDataDirectory:string;
//function ReadHostIp:string;

implementation

{$ifndef BUILDCOMPONENTS}
uses USingletons, System.SysUtils,US52Utils;

procedure ViewCommand(index:integer;command:TEViewSettingsCommand;data1:integer=0;data2:integer=0);
begin
  if IViewSettings<>NIL then
    IViewSettings.Command(index,command,data1,data2);
end;

procedure S52Command(command:TES52UtilCommand);
begin
  if IS52Utils<>NIL then
    IS52Utils.Command(command);
end;

procedure ViewSetInstrument(instrument:Tinstrumentrange);
begin
    ViewCommand(Instrument,vcSetInstrument);
end;
{$else}
uses System.SysUtils;
{$endif}

procedure raiseError(s:string);
begin
  raise Exception.Create(s);
end;

function ToStopPlay(value:integer):TPlayState;
begin
  if value=0 then result:=psStop else result:=psPlay;
end;

procedure bound(VAR m:integer;min,max:integer);
begin
  if m<min then m:=min;
  if m>max then m:=max;
end;

function GetDataDirectory:string;
begin
  result:=ExtractFilePath(ParamStr(0))+'Data\';
end;

function DebugMode:boolean;
VAR i:integer;
begin
  result:=false;
  for i:=1 to ParamCount do if Paramstr(i)='d' then
     result:=true;
end;

function GetAttrib(sl:TstringList;s:string):string;
VAr i:integer;
begin
  result:='';
  for i:=0 to sl.count-1 do if
   1 = pos(s+'=',sl[i]) then result:=Copy(sl[i],length(s)+2);
end;

function To0_127(s:string):integer;
begin
  Result:=StrToIntDef(s,0);
  if Result>127 then result:=127;
  if Result<0   then result:=0;
end;

function To0_127(b:boolean):integer;
begin
  result:=127*ord(b);
end;

function ThreeStr(n:integer):string;
begin
  result:=inttostr(n);
  if n<10 then result:='00'+result
  else if n<100 then result:='0'+result;
end;

function TwoStr(n: integer): string;
begin
  result := inttostr(n);
  if n < 10 then
    result := '0' + result;
end;

function Getattrib(s,spart:string):string;
VAR p:integer;
begin
  p:=pos(spart+'=',s);
  if p>0 then
  begin
    result:=Copy(s,p+length(spart)+1);
    p:=pos(';',result);
    if p>0 then result:=Copy(result,1,p-1);
  end
  else result:='';
end;


function instrarray_d70: ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create(INSTRUMENT_UNIVERSESTART+2,INSTRUMENT_MASTERKEYBOARD1,INSTRUMENT_SUBKEYBOARD1,INSTRUMENT_UNIVERSESTART+3);
end;

function instrarray_lead: ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create(INSTRUMENT_LEAD0,INSTRUMENT_LEAD0+1,INSTRUMENT_LEAD0+2,INSTRUMENT_LEAD0+3);
end;

function instrarray_none: ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create();
end;

function instrarray_w5: ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create(INSTRUMENT_UNIVERSESTART,INSTRUMENT_MASTERKEYBOARD0,INSTRUMENT_SUBKEYBOARD0,INSTRUMENT_UNIVERSESTART+1);
end;


function instrarray_w5_d70: ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create(INSTRUMENT_UNIVERSESTART,INSTRUMENT_MASTERKEYBOARD0,INSTRUMENT_SUBKEYBOARD0,INSTRUMENT_UNIVERSESTART+1,
                                INSTRUMENT_UNIVERSESTART+2,INSTRUMENT_MASTERKEYBOARD1,INSTRUMENT_SUBKEYBOARD1,INSTRUMENT_UNIVERSESTART+3);
end;

function instrarray_d70_w5: ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create(INSTRUMENT_UNIVERSESTART+2,INSTRUMENT_MASTERKEYBOARD1,INSTRUMENT_SUBKEYBOARD1,INSTRUMENT_UNIVERSESTART+3,
                                INSTRUMENT_UNIVERSESTART,INSTRUMENT_MASTERKEYBOARD0,INSTRUMENT_SUBKEYBOARD0,INSTRUMENT_UNIVERSESTART+1);
end;


function instrarray_bigmoog: ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create(INSTRUMENT_BIGMOOG0,INSTRUMENT_BIGMOOG0+1,INSTRUMENT_BIGMOOG0+2,INSTRUMENT_BIGMOOG0+3);
end;

function instrarray_effects:ArrayOfInteger;
VAR i,l:integer;
begin
  l:=INSTRUMENT_EFFECTSEND-INSTRUMENT_EFFECTSSTART+1;
  setlength(result,l);
  for i:=0 to l-1 do
  result[i]:=INSTRUMENT_EFFECTSSTART+i;
end;

function instrarray_d70_short:ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create(6,7);
end;

function instrarray_w5_short:ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create(4,5);
end;


function instrarray_nonsmallmoog:ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create(4,5,6,7);
end;

function instrarray_nond70_short:ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create(1,2,4,5);
end;

function instrarray_nonw5_short:ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create(1,2,6,7);
end;

function instrarray_w5_d70small:ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create(1,2,4,5,6,7,8,9);
end;


function instrarray_smallmoog:ArrayOfInteger;
begin
  result:=ArrayOfInteger.Create(INSTRUMENT_BIGMOOG0,INSTRUMENT_BIGMOOG0+1);
end;

(*

function ReadHostIp:string;
const Filename = 'C:\MIDI\Programs\Super52\Data\MidiHWDevices.txt';
VAr i:integer;
    sl:Tstringlist;
begin
  sl:=Tstringlist.Create;
  sl.loadFromFile(filename);
  result:='000.000.000.000';
  for i:=0 to sl.Count-1 do if pos('HostIp=',sl[i])=1 then result:=Copy(sl[i],8);
  sl.free;
end; *)
end.




