unit UViewMidiForm;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC} LCLType, LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Forms, Classes, Controls, StdCtrls,UIXPlugin,UMidiPorts,UMidiEvent, Vcl.ExtCtrls;

type
  TViewMidiForm = class(TForm)
    XSynthPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    ComboBoxOut: TComboBox;
    ComboBoxIn: TComboBox;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxInChange(Sender: TObject);
    procedure ComboBoxOutChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FModel : IXMidiPlugin;

    MidiOutport:TMidiOutport;
    MidiInPort:TMidiInPort;
    MidiInputDevices,MidiOutputDevices: TstringList;
    procedure InitMidi(instring,outstring: string);
    procedure MidiOutOpen(index: integer; _open: boolean);
    procedure DoMidiData(Sender: TMidiPort; const midievent:TMidiEvent);
    procedure MidiInOpen(index: integer; _open: boolean);
    procedure WriteMidiEvent(const m: TMidiEvent);
    procedure Load;
    procedure Save;

  public
     property Model : IXMidiPlugin read FModel;
  end;
var
  ViewMidiForm        : TViewMidiForm;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  SysUtils, Inifiles,XPluginFactory,DAV_VSTEffect;


procedure TViewMidiForm.Button1Click(Sender: TObject);
begin
  Load;
end;

procedure TViewMidiForm.Load;
VAR m:TMemoryStream;
begin
  m:=TMemoryStream.Create;
  try
    m.LoadFromFile('C:\temp\TestStream.txt');
  except end;
  m.Position:=0;
  FModel.LoadFromStream(m,0,false);
  m.Free;
end;

procedure TViewMidiForm.Button2Click(Sender: TObject);
begin
  Save;
end;

procedure TViewMidiForm.Save;
VAR m:TMemoryStream;
begin
  m:=TMemoryStream.Create;
  FModel.SaveToStream(m,0,false);
  m.Position:=0;
  m.SaveToFile('C:\temp\TestStream.txt');
  m.Free;
end;

procedure TViewMidiForm.ComboBoxInChange(Sender: TObject);
VAR
  index: integer;
begin
  index := ComboBoxIn.ItemIndex - 1;
  MidiInOpen(index, index <> -1);
end;

procedure TViewMidiForm.ComboBoxOutChange(Sender: TObject);
VAR
  index: integer;
begin
  index := ComboBoxOut.ItemIndex - 1;
  MidiOutOpen(index, index <> -1);
end;

procedure TViewMidiForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Save;
end;

procedure TViewMidiForm.FormCreate(Sender: TObject);
VAR XSynthFrame:TForm;
begin
  FModel := XPluginFactory.CreateObject(NIL,44100);
  FModel.SetWriteMidiEventProc(WriteMidiEvent);
  XSynthFrame:=FModel.GetFormClass.Create(XSynthPanel);
  XSynthFrame.Parent:=XSynthPanel;
  FModel.SetEditor(XSynthFrame);
  XSynthFrame.Visible:=true;
  XSynthPanel.ClientWidth:=XSynthFrame.Width;
  XSynthPanel.ClientHeight:=XSynthFrame.Height;
  ClientWidth:=XSynthPanel.Left+XSynthPanel.Width;
  ClientHeight:=XSynthPanel.Top+XSynthPanel.Height;
  InitMidi('D70Virt','Super52 Out-01-16');
  Load;
end;

procedure TViewMidiForm.WriteMidiEvent(const m:TMidiEvent);
begin
  MidiOutPort.WriteMidi(m);
end;

procedure TViewMidiForm.InitMidi(instring,outstring:string);
VAR
  i: integer;
begin
  MidiInPort:=TMidiInPort.Create;
  MidiOutPort:=TMidiOutPort.Create;
  MidiInputDevices := TstringList.Create;
  MidiInputDevices.Assign(MidiPortManager.MidiInNames);
  MidiInputDevices.Insert(0, '<none>');

  ComboBoxIn.Items.Assign(MidiInputDevices);
  ComboBoxIn.ItemIndex := 0;
  for i := 0 to ComboBoxIn.Items.Count - 1 do
    if pos(instring,ComboBoxIn.Items[i])>0 then
      ComboBoxIn.ItemIndex := i;
  ComboBoxInChange(NIL);
  // -------------------------------------------

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

procedure TViewMidiForm.DoMidiData(Sender: TMidiPort; const midievent:TMidiEvent);
    function MidiEventToVstMidiEvent(m:TMidiEvent):TVstMidiEvent;
    begin
      result.MidiData[0]:=m.midichannel+m.status;
      result.MidiData[1]:=m.data1;
      result.MidiData[2]:=m.data2;
    end;
VAR VstMidiEvent:TVstMidiEvent;
begin
  FModel.ProcessMidi(self,MidiEventToVstMidiEvent(midievent));
end;

procedure TViewMidiForm.MidiInOpen(index: integer; _open: boolean);
begin
  MidiInPort.Close;
  MidiInPort.OpenByIndex(index);
  MidiInPort.OnMidiData:=DoMidiData;
end;

procedure TViewMidiForm.MidiOutOpen(index: integer; _open: boolean);
begin
  MidiOutPort.Close;
  MidiOutPort.Open(index);
end;



end.

