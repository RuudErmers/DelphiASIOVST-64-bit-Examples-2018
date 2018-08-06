unit UCrumarViewTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,URMCCrumarView, Vcl.StdCtrls,UMidiPorts,UMidiEvent,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    PanelCrumarView: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    MidiOutPort:TMidiOutPort;
    procedure OnPCCChanged(Sender: TObject; pcc, Value: Integer);
    { Private declarations }

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses UVirtCC;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Left:=20;
  MidiOutPort:=TMidiOutPort.Create;
  MidiOutPort.Open('Super52 Out-01-16');
  with TRMCCrumarView.Create(PanelCrumarView) do
  begin
    Parent:=PanelCrumarView;
    Visible:=true;
    OnPCCChanged:=self.OnPCCChanged;
    PanelCrumarView.Width:=Width;
    PanelCrumarView.Height:=Height;
  end;
  Width:=PanelCrumarView.Width+8;
  Height:=PanelCrumarView.Height+Memo1.Height+16;
end;


procedure TForm1.OnPCCChanged(Sender:TObject;pcc,Value: Integer);
begin
  Memo1.Lines.Add('PCC Changed: '+inttostr(pcc)+' '+inttostr(value));
  MidiOutPort.WriteMidi(MidiEvent(0,MIDI_CC,pcc,value));
end;


end.
