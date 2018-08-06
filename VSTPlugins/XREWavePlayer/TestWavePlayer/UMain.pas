unit UMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,UMidiPorts, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    MidioutPort:TMidiOutport;
    procedure SetStreamName(streamname: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses UMidiEvent;

const CC_ID_START = 80;
type TPlayState = (psStop,psPause,psPlay,psContinue);

procedure TForm1.Button2Click(Sender: TObject);
begin
  MidiOutPort.WriteMidi(MidiEvent(0,MIDI_CC,CC_ID_START+2,ord(psPlay)));
end;

procedure TForm1.SetStreamName(streamname: string);
VAR i,value:integer;
begin
  for i:=0 to length(streamname) do
  begin
    if i=0 then value:=0
           else value:=ord(streamname[i]);
    MidioutPort.WriteMidi(MidiEvent(0,MIDI_CC,CC_ID_START+0,value));
  end;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  MidioutPort:=TMidiOutport.Create;
  MidiOutPort.Open('Super52 Out-01-16');
  SetStreamName('C:\Midi\Data\Music\02 Sense.MP3');
end;

end.


unit UVSTPlayer;

interface

uses Classes, ExtCtrls, Messages,UDataTypes,URevoxController,UInstrumentNotificationMask,UInstrument;

type TVSTPlayer = class (IImplementsInterface,IMP3Player,IModelObserverInstrument)
private
  FonPosUpdate: TOnPosUpdateEvent;
  { property }MidiChannel:integer;
  Fid,FPosition,FLength:integer;
  CC_ID_START:integer;
  function Instrument:TInstrument;
  procedure SetStreamName(streamname:string);
  procedure SetVolume(value:integer);
  function GetVolume:integer;
  procedure SetEffect(index,value:integer);
  function GetEffect(index:integer):integer;
  procedure Stop;
  procedure Play;
  procedure SetPause(Pause:boolean);
  procedure OnInstrumentChanged(const mask: TInstrumentNotification); virtual;
public
  constructor Create(OnPosUpdate:TOnPosUpdateEvent;FId:integer);
end;

implementation

uses USingletons,SysUtils;

constructor TVSTPlayer.Create(OnPosUpdate:TOnPosUpdateEvent;FID:integer);
VAR i,dev:integer;
begin
  MidiChannel:=FId+INSTRUMENT_REVOX0-1;
  self.Fid:=Fid;
  CC_ID_START := 80;
  FonPosUpdate:=OnPosUpdate;
  IObserverManager.AddObserver(self);
end;

function TVSTPlayer.GetEffect(index: integer): integer;
begin
  result:=instrument.Effects[index];
end;

function TVSTPlayer.GetVolume: integer;
begin
  result:=instrument.Volume
end;

function TVSTPlayer.Instrument: TInstrument;
begin
  result:=IInstruments[MidiChannel+1];
end;

procedure TVSTPlayer.OnInstrumentChanged(const mask: TInstrumentNotification);
VAR cntrl,value,index:integer;
  // value can be 15 bits,  (500 minutes)
  // we use 2 lower bits for making sure the value changes
  function SetValue(VAR x:integer;value,part:integer):boolean;
  begin
    value:=value DIV 4;
    case part of
      2: x:=(x and  $3FF) + (value SHL 10);
      1: x:=(x and $7C1F) + (value SHL 5);
      0: x:=(x and $7FE0) + (value SHL 0);
    end;
    result:=part = 2;
  end;
VAR upd:boolean;
begin
  if mask.isMask([INSTRUMENT_CHANGED_CONTROLLER]) and (mask.instrument = INSTRUMENT_REVOX0+ Fid) then
  begin
    cntrl:=mask.cntrlnr;
    value:=mask.cntrlvalue;
    upd:=false;
    case cntrl of
      10,11,12: upd:=SetValue(FPosition,value,cntrl-10);
      13,14,15: upd:=SetValue(FLength ,value,cntrl-13);
    end;
    if upd and assigned(FOnPosUpdate) then FOnPosUpdate(Fid,FPosition,FLength);
  end;
end;

procedure TVSTPlayer.Play;
begin
  IMidiToHost.MidiCC(MidiChannel,CC_ID_START+2,ord(psPlay));
end;

procedure TVSTPlayer.Stop;
begin
  IMidiToHost.MidiCC(MidiChannel,CC_ID_START+2,ord(psStop));
end;

procedure TVSTPlayer.SetEffect(index, value: integer);
begin
  Instrument.Effects[index]:=value;
end;

procedure TVSTPlayer.SetPause(Pause: boolean);
VAR cmd:integer;
begin
  if pause then cmd:=ord(psPause) else cmd:=ord(psContinue);
  IMidiToHost.MidiCC(MidiChannel,CC_ID_START+2,cmd);
end;

procedure TVSTPlayer.SetStreamName(streamname: string);
VAR i,value:integer;
begin
  for i:=0 to length(streamname) do
  begin
    if i=0 then value:=0
           else value:=ord(streamname[i]);
    IMidiToHost.MidiCC(MidiChannel,CC_ID_START+0,value);
  end;
end;

procedure TVSTPlayer.SetVolume(value: integer);
begin
  instrument.Volume:=Value;
end;



