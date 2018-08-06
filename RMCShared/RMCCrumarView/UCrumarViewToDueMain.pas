unit UCrumarViewToDueMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,UCrumarView,UAckedSerialRMSProtocol,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    serial:TAckedSerialRMSProtocolMidi;
    procedure OnParameterChanged(Sender:TObject;Index,Value: Integer);
    procedure onMidiKeyOn(Sender: TObject; pitch,velocity:integer);
    procedure onMidiKeyOff(Sender: TObject; pitch:integer);
    procedure OnConnect(connected: boolean);
    procedure OnDebug(debug: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses UMidiEvent;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Left:=100;
  with TCrumarView.Create(self) do
  begin
    Parent:=self;
    align:=alClient;
    Visible:=true;
    OnParameterChanged:=self.OnParameterChanged;
    onMidiKeyOn:=self.onMidiKeyOn;
    onMidiKeyOff:=self.onMidiKeyOff;
  end;
  serial:=TAckedSerialRMSProtocolMidi.Create(NIL,RMSID_DueCrumar,true,true,OnConnect);
  serial.OnDebug:=OnDebug;
end;

procedure TForm1.OnDebug(debug:string);
begin
  Memo1.Lines.Add(debug);
end;

procedure TForm1.OnConnect (connected:boolean);
begin
  if connected then Memo1.Lines.Add('Connected!')
               else Memo1.Lines.Add('Not Connected {:')
end;

procedure TForm1.onMidiKeyOff(Sender: TObject; pitch: integer);
VAR m: TMidiEvent;
begin
  m:=MidiEvent(0,MIDI_NOTE_OFF,pitch,64);
  Memo1.Lines.Add('Sending To Due: '+m.toString);
  serial.WriteMidi(m);
end;

procedure TForm1.onMidiKeyOn(Sender: TObject; pitch, velocity: integer);
VAR m: TMidiEvent;
begin
  m:=MidiEvent(0,MIDI_NOTE_ON,pitch,velocity);
  Memo1.Lines.Add('Sending To Due: '+m.toString);
  serial.WriteMidi(m);
end;


const ParamToPhys: array[0..44] of integer = (
13,10,11,14,23,20,21,24,111,112,50,53,26,55,58,27,28,60,61,62,63,66,75,76,77,78,72,74,71,79,29,30,31,32,33,34,35,36,37,38,39,107,5,65,9);

procedure TForm1.OnParameterChanged(Sender: TObject; Index, Value: Integer);
VAR m: TMidiEvent;
  cc:integer;
begin
  cc:=ParamToPhys[index];
  if cc=-1 then exit;
  m:=MidiEvent(0,MIDI_CC,cc,value);
  Memo1.Lines.Add('Sending To Due: '+m.toString);
  serial.WriteMidi(m);
end;

end.
