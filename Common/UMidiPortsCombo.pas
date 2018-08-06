unit UMidiPortsCombo;

interface

uses UMidiPorts,UMidiEvent,StdCtrls;

type   TMidiInPortCombo = class (TMidiInPort)
  public
    constructor Create(combobox:TComboBox;OnMidiData:TonMidiData);
    procedure Open(s: string);
  private
    FComboBox:TComboBox;
    FOnMidiData:TonMidiData;
    procedure ComboBoxChange(Sender: TObject);
  end;
  TMidiOutPortCombo = class (TMidiOutPort)
    constructor Create(combobox:TComboBox);
    procedure Open(s: string);
  private
    FComboBox:TComboBox;
    procedure ComboBoxChange(Sender: TObject);
  end;


implementation

{ TMidiInPortCombo }

constructor TMidiInPortCombo.Create(combobox: TComboBox;  OnMidiData: TonMidiData);
begin
  inherited Create;
  FComboBox:=ComboBox;
  FonMidiData:=OnMidiData;
  FComboBox.Items.Assign(MidiPortManager.MidiInNames);
  FComboBox.Items.Insert(0, '<none>');
  FComboBox.ItemIndex := 0;
  FComboBox.OnChange:=ComboBoxChange;
  ComboBoxChange(NIL);
end;

procedure TMidiInPortCombo.ComboBoxChange(Sender:TObject);
VAR
  index: integer;
begin
  index := FComboBox.ItemIndex - 1;
  Close;
  OpenByIndex(index);
  OnMidiData:=FonMidiData;
end;

procedure TMidiInPortCombo.Open(s:string);
VAR i:integer;
begin
  for i := 0 to FComboBox.Items.Count - 1 do
    if pos(s,FComboBox.Items[i])>0 then
      FComboBox.ItemIndex := i;
  ComboBoxChange(NIL);
end;


{ TMidiOutPortCombo }

procedure TMidiOutPortCombo.ComboBoxChange(Sender: TObject);
VAR
  index: integer;
begin
  index := FComboBox.ItemIndex - 1;
  Close;
  inherited OpenByIndex(index);
end;

constructor TMidiOutPortCombo.Create(combobox: TComboBox);
begin
  inherited Create;
  FComboBox:=ComboBox;
  FComboBox.Items.Assign(MidiPortManager.MidiInNames);
  FComboBox.Items.Insert(0, '<none>');
  FComboBox.ItemIndex := 0;
  FComboBox.OnChange:=ComboBoxChange;
  ComboBoxChange(NIL);
end;

procedure TMidiOutPortCombo.Open(s: string);
VAR i:integer;
begin
  for i := 0 to FComboBox.Items.Count - 1 do
    if pos(s,FComboBox.Items[i])>0 then
      FComboBox.ItemIndex := i;
  ComboBoxChange(NIL);
end;


end.
