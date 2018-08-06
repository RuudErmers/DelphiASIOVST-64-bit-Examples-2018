unit UMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,UMidiPorts,UMidiEvent,
  Vcl.Samples.Spin;

type
  TForm1 = class(TForm)
    ComboBoxOut: TComboBox;
    Memo1: TMemo;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure ComboBoxOutChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    MidiOutport:TMidiOutport;
    MidiOutputDevices: TstringList;
    procedure InitMidi(instring,outstring: string);
    procedure MidiOutOpen(index: integer; _open: boolean);
    procedure SetStreamName(streamname: string);
  public
    { Public declarations }
end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
const
  MIDI_CC = $B0;
  MIDI_PRG = $C0;
const   CC_ID_START = 80;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MidiOutPort.WriteMidi(MidiEvent(0,MIDI_CC,CC_ID_START+2,2));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  MidiOutPort.WriteMidi(MidiEvent(0,MIDI_CC,CC_ID_START+2,0));
end;

procedure TForm1.ComboBoxOutChange(Sender: TObject);
VAR
  index: integer;
begin
  index := ComboBoxOut.ItemIndex - 1;
  MidiOutOpen(index, index <> -1);
end;


procedure TForm1.SetStreamName(streamname: string);
VAR m:TMidievent;
    s:string;
    i:integer;
begin
  m.midichannel:=0;
  m.status:=MIDI_CC;
  m.data1:=CC_ID_START+0;
  for i:=0 to length(streamname) do
  begin
    if i=0 then m.data2:=0
           else m.data2:=ord(streamname[i]);
    MidiOutPort.WriteMidi(m);
  end;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  SetStreamName('D:\MPTree\Progressive\Alan Parsons Project\Alan Parsons Project - 1980 - The Turn Of A Friendly Card\01 may be a price to pay.mp3');
end;

procedure TForm1.MidiOutOpen(index: integer; _open: boolean);
begin
  MidiOutPort.Close;
  MidiOutPort.OpenByIndex(index);
end;


procedure TForm1.InitMidi(instring,outstring:string);
VAR
  i: integer;
begin
  MidiOutPort:=TMidiOutPort.Create;

  MidiOutputDevices := TstringList.Create;
  MidiOutputDevices.Assign(MidiPortManager.MidiOutNames);
  MidiOutputDevices.Insert(0, '<none>');

  ComboBoxOut.Items.Assign(MidiOutputDevices);
  ComboBoxOut.ItemIndex := 0;
  for i := 0 to ComboBoxOut.Items.Count - 1 do
    if pos(outstring,ComboBoxOut.Items[i])>0 then
      ComboBoxOut.ItemIndex := i;
  ComboBoxOutChange(NIL);
end;


procedure TForm1.FormShow(Sender: TObject);
begin
  Left:=0;
  InitMidi('CME UF','Super52 Out-01-16');
end;


end.
