unit VocoderGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, Graphics,
  DAV_Types, DAV_VSTModule, DAV_GuiMidiKeys, DAV_GuiBaseControl;

type
  TVSTGUI = class(TForm)
    MidiKeys: TGuiMidiKeys;
    SBInputLevel: TScrollBar;
    LbInput: TLabel;
    LbSynthLevel: TLabel;
    LbVocoderLevel: TLabel;
    SBSynthLevel: TScrollBar;
    SBVocoderLevel: TScrollBar;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SBInputLevelChange(Sender: TObject);
    procedure SBSynthLevelChange(Sender: TObject);
    procedure SBVocoderLevelChange(Sender: TObject);
    procedure MidiKeysNoteOn(Sender: TObject; KeyNr: Byte; Velocity: Single);
    procedure MidiKeysNoteOff(Sender: TObject; KeyNr: Byte);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateSynthVolume;
    procedure UpdateInputVolume;
    procedure UpdateVocoderVolume;
  end;

implementation

{$R *.DFM}

uses
  VocoderModule, VocoderVoice, VoiceList;

procedure TVSTGUI.FormShow(Sender: TObject);
begin
 UpdateSynthVolume;
 UpdateInputVolume;
 UpdateVocoderVolume;
end;

procedure TVSTGUI.MidiKeysNoteOn(Sender: TObject; KeyNr: Byte;
  Velocity: Single);
var
  newNote : TVocoderVoice;
begin 
  TVSTSSModule(Owner).MidiNoteOn(0, KeyNr, round(Velocity * 128));
  newNote := TVocoderVoice.Create(TVSTSSModule(Owner));
  newNote.MidiKeyNr := KeyNr;
  newNote.Velocity := round(Velocity * 127);
  newNote.NoteOn(Midi2Pitch[KeyNr], Velocity);
  TVSTSSModule(Owner).Voices.Add(newNote);
end;

procedure TVSTGUI.MidiKeysNoteOff(Sender: TObject; KeyNr: Byte);
var i : Integer;
begin
  TVSTSSModule(Owner).MidiNoteOff(0, KeyNr, 0);
  with TVSTSSModule(Owner) do
  for i := Voices.Count - 1 downto 0 do
   if (Voices[i].MidiKeyNr = KeyNr) then
    begin
     Voices.Delete(i);
     Break;
    end;
end;

procedure TVSTGUI.SBInputLevelChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[0] := SBInputLevel.Position;
end;

procedure TVSTGUI.SBSynthLevelChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[1] := SBSynthLevel.Position;
end;

procedure TVSTGUI.SBVocoderLevelChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[2] := SBVocoderLevel.Position;
end;

procedure TVSTGUI.UpdateInputVolume;
begin
 with TVSTSSModule(Owner) do
  begin
   SBInputLevel.Position := round(Parameter[0]);
  end;
end;

procedure TVSTGUI.UpdateSynthVolume;
begin
 with TVSTSSModule(Owner) do
  begin
   SBSynthLevel.Position := round(Parameter[1]);
  end;
end;

procedure TVSTGUI.UpdateVocoderVolume;
begin
 with TVSTSSModule(Owner) do
  begin
   SBVocoderLevel.Position := round(Parameter[2]);
  end;
end;

procedure TVSTGUI.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i       : Integer;
  newNote : TVocoderVoice;
  Note    : Byte;
const
  CVeloDiv : Single = 1/128;
begin
 case Key of
  89  : Note := 60;
  83  : Note := 61;
  88  : Note := 62;
  68  : Note := 63;
  67  : Note := 64;
  86  : Note := 65;
  71  : Note := 66;
  66  : Note := 67;
  72  : Note := 68;
  78  : Note := 69;
  74  : Note := 70;
  77  : Note := 71;
  188 : Note := 72;
  81  : Note := 72;
  87  : Note := 74;
  69  : Note := 76;
  82  : Note := 77;
  else Exit;
 end;

 assert(Owner is TVSTSSModule);

 with TVSTSSModule(Owner) do
  begin
   for i := 0 to Voices.Count - 1 do
    if (Voices[i].MidiKeyNr = Note) then Exit;
   MidiNoteOn(0, Note, 100);
  end;
 with newNote do
  begin
   newNote := TVocoderVoice.Create(TVSTSSModule(Owner));
   MidiKeyNr := Note;
   Velocity := 100;
   NoteOn(Midi2Pitch[Note], Velocity * CVeloDiv);
   TVSTSSModule(Owner).Voices.Add(newNote);
  end;
end;

procedure TVSTGUI.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var i    : Integer;
    Note : Byte;
begin
 case Key of
  89  : Note := 60;
  83  : Note := 61;
  88  : Note := 62;
  68  : Note := 63;
  67  : Note := 64;
  86  : Note := 65;
  71  : Note := 66;
  66  : Note := 67;
  72  : Note := 68;
  78  : Note := 69;
  74  : Note := 70;
  77  : Note := 71;
  188 : Note := 72;
  81  : Note := 72;
  87  : Note := 74;
  69  : Note := 76;
  82  : Note := 77;
  else Exit;
 end;
 TVSTSSModule(Owner).MidiNoteOff(0, Note, 100);
 with TVSTSSModule(Owner) do
  for i := 0 to Voices.Count - 1 do
   if (Voices[i].MidiKeyNr = Note) then
    begin
     Voices.Delete(i);
     Break;
    end;
end;

end.
