unit SineSynthGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, Graphics,
  DAV_Types, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiMidiKeys;

type
  TVSTGUI = class(TForm)
    MidiKeys: TGuiMidiKeys;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MidiKeysNoteOff(Sender: TObject; KeyNr: Byte);
    procedure MidiKeysNoteOn(Sender: TObject; KeyNr: Byte; Velocity: Single);
  end;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  SineSynthModule, SineSynthVoice, VoiceList;

procedure TVSTGUI.MidiKeysNoteOn(Sender: TObject; KeyNr: Byte;
  Velocity: Single);
var
  newNote : TSineSynthVoice;
begin
 with TVSTSSModule(Owner) do
  begin
   MidiNoteOn(0, KeyNr, round(Velocity * 128));
   newNote := TSineSynthVoice.Create(TVSTSSModule(Owner));
   newNote.MidiKeyNr := KeyNr;
   newNote.Velocity := round(Velocity * 127);
   newNote.NoteOn(Midi2Pitch[KeyNr], Velocity);
   Voices.Add(newNote);
  end;
end;

procedure TVSTGUI.MidiKeysNoteOff(Sender: TObject; KeyNr: Byte);
var
  i : Integer;
begin
 with TVSTSSModule(Owner) do
  begin
   MidiNoteOff(0, KeyNr, 0);
   for i := Voices.Count - 1 downto 0 do
    if (Voices[i].MidiKeyNr = KeyNr) then
     begin
      Voices.Delete(i);
      Break;
     end;
  end;
end;

procedure TVSTGUI.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i       : Integer;
  newNote : TSineSynthVoice;
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
   newNote := TSineSynthVoice.Create(TVSTSSModule(Owner));
   newNote.MidiKeyNr := Note;
   newNote.Velocity := 100;
   newNote.NoteOn(Midi2Pitch[Note], newNote.Velocity * CVeloDiv);
   Voices.Add(newNote);
  end;
end;

procedure TVSTGUI.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i    : Integer;
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
 if Owner is TVSTSSModule then
  with TVSTSSModule(Owner) do
   begin
    MidiNoteOff(0, Note, 100);
    for i := 0 to Voices.Count - 1 do
     if (Voices[i].MidiKeyNr=Note) then
      begin
       Voices.Delete(i);
       Break;
      end;
   end;
end;

end.
