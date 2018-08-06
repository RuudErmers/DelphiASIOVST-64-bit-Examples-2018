unit SineSynthModule;

interface

uses
  Windows, SysUtils, Classes, Forms, DAV_Types, DAV_VSTEffect, DAV_VSTModule,
  SineSynthVoice, Generics.Collections;

type
  TVoiceList = TList<TSineSynthVoice>;
  TVSTSSModule = class(TVSTModule)
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess32Replacing(const Inputs,
      Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64Replacing(const Inputs,
      Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
  private
    FVoices : TVoiceList;
  public
    property Voices: TVoiceList read FVoices;
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  SineSynthGUI, Math;

procedure TVSTSSModule.VSTModuleOpen(Sender: TObject);
begin
 FVoices := TVoiceList.Create;

 // set editor form class
 EditorFormClass := TVSTGUI;
end;

procedure TVSTSSModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FVoices);
end;

procedure TVSTSSModule.VSTModuleProcess32Replacing(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex, SampleIndex : Integer;
begin
 FillChar(Outputs[0, 0], SampleFrames * SizeOf(Single), 0);
 FillChar(Outputs[1, 0], SampleFrames * SizeOf(Single), 0);

 for SampleIndex := 0 to SampleFrames - 1 do
  for ChannelIndex := 0 to Voices.Count - 1
   do Outputs[0, SampleIndex] := Outputs[0, SampleIndex] + Voices[ChannelIndex].Process;

 for ChannelIndex := 1 to numOutputs - 1
  do Move(Outputs[0, 0], Outputs[ChannelIndex, 0], SampleFrames * SizeOf(Single));
end;

procedure TVSTSSModule.VSTModuleProcess64Replacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex, SampleIndex : Integer;
begin
 FillChar(Outputs[0, 0], SampleFrames * SizeOf(Double), 0);
 FillChar(Outputs[1, 0], SampleFrames * SizeOf(Double), 0);

 for SampleIndex := 0 to SampleFrames - 1 do
  for ChannelIndex := 0 to Voices.Count - 1
   do Outputs[0, SampleIndex] := Outputs[0, SampleIndex] + Voices[ChannelIndex].Process;

 for ChannelIndex := 1 to numOutputs - 1
  do Move(Outputs[0, 0], Outputs[ChannelIndex, 0], SampleFrames * SizeOf(Double));
end;

procedure TVSTSSModule.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
var
  Status  : Byte;
  Index   : Integer;
  newNote : TSineSynthVoice;
const
  CVeloDiv : Single = 1 / 128;
begin
 Status := MidiEvent.MidiData[0] and $F0; // channel information is removed
 if (Status = $90) and (MidiEvent.MidiData[2] > 0) then // "note on" ?
  begin
   if Voices.Count > 7 then Voices.Remove(Voices.Items[0]);
   newNote := TSineSynthVoice.Create(self);
   with newNote do
    begin
     MidiKeyNr := MidiEvent.MidiData[1];
     Velocity  := MidiEvent.MidiData[2];
     NoteOn(Midi2Pitch[MidiKeyNr], Velocity * CVeloDiv);
    end;
   Voices.Add(newNote);
  end
 else if ((Status = $90) and (MidiEvent.MidiData[2] = 0)) or (Status = $80) then // "note off" ?
  begin
   for Index := 0 to Voices.Count - 1 do
    begin
     if (Voices.Items[Index].MidiKeyNr = MidiEvent.MidiData[1]) then
      begin
       Voices.Delete(Index);
       Break;
      end;
    end;
  end
 else if (Status = $B0) and (MidiEvent.MidiData[1] = $7E) then
  begin
   // all notes off
   Voices.Clear;
  end;
end;

end.
