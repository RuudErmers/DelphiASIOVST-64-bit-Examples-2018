unit SimpleSamplerModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTEffect,
  DAV_VSTModule, DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU,
  DAV_AudioData, SimpleSamplerVoice, SimpleSamplerGUI, VoiceList;

type
  TVSTSSModule = class(TVSTModule)
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess64Replacing(const Inputs,
      Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32Replacing(const Inputs,
      Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
  private
    FVoices       : TVoiceList;
    FSample       : PDAVSingleFixedArray;
    FSampleLength : Integer;
  public
    procedure LoadFromFile(const FileName: TFileName);
    property Voices: TVoiceList read FVoices;
    property Sample: PDAVSingleFixedArray read FSample;
    property SampleLength: Integer read FSampleLength;
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TVSTSSModule.VSTModuleOpen(Sender: TObject);
begin
 FVoices := TVoiceList.Create(True);

 // set editor form class
 EditorFormClass := TVSTGUI;
end;

procedure TVSTSSModule.LoadFromFile(const FileName: TFileName);
var
  ADC : TAudioDataCollection32;
begin
 ADC := TAudioDataCollection32.Create(Self);
 with ADC do
  try
   LoadFromFile(FileName);
   FSampleLength := ADC.SampleFrames;
   ReallocMem(FSample, FSampleLength * SizeOf(Single));
   Move(ADC[0].ChannelDataPointer^[0], FSample^[0], FSampleLength * SizeOf(Single));
  finally
   FreeAndNil(ADC);
  end;
end;

procedure TVSTSSModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FVoices);
end;

procedure TVSTSSModule.VSTModuleProcess32Replacing(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  VoiceIndex, SampleIndex : Integer;
begin
 FillChar(Outputs[0,0], SampleFrames * SizeOf(Single), 0);
 FillChar(Outputs[1,0], SampleFrames * SizeOf(Single), 0);

 for SampleIndex := 0 to SampleFrames - 1 do
  for VoiceIndex := 0 to Voices.Count - 1
   do Outputs[0, SampleIndex] := Outputs[0, SampleIndex] + Voices[VoiceIndex].Process;

 for VoiceIndex := 1 to numOutputs - 1
  do Move(Outputs[0, 0], Outputs[VoiceIndex, 0], SampleFrames * SizeOf(Single));
end;

procedure TVSTSSModule.VSTModuleProcess64Replacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  VoiceIndex, SampleIndex : Integer;
begin
 FillChar(Outputs[0, 0], SampleFrames * SizeOf(Double), 0);
 FillChar(Outputs[1, 0], SampleFrames * SizeOf(Double), 0);

 for SampleIndex := 0 to SampleFrames - 1 do
  for VoiceIndex := 0 to Voices.Count - 1
   do Outputs[0, SampleIndex] := Outputs[0, SampleIndex] + Voices[VoiceIndex].Process;

 for VoiceIndex := 1 to numOutputs - 1
  do Move(Outputs[0, 0], Outputs[VoiceIndex, 0], SampleFrames * SizeOf(Double));
end;

procedure TVSTSSModule.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
var
  Status  : Byte;
  i       : Integer;
  newNote : TSimpleSamplerVoice;
const
  VeloDiv : Single = 1 / 128;
begin
 Status:=MidiEvent.midiData[0] and $F0; // channel information is removed
 if (Status = $90) and (MidiEvent.mididata[2] > 0) then // "note on" ?
  begin
   if Voices.Count > 7 then Voices.Remove(Voices.Items[0]);
   newNote:=TSimpleSamplerVoice.Create(self);
   with newNote do
    begin
     MidiKeyNr := MidiEvent.midiData[1];
     Velocity  := MidiEvent.midiData[2];
     NoteOn(Midi2Pitch[MidiKeyNr], Velocity * VeloDiv);
    end;
   Voices.Add(newNote);
  end
 else if ((status = $90) and (MidiEvent.mididata[2] = 0)) or (status = $80) then // "note off" ?
  begin
   for i := 0 to Voices.Count - 1 do
    begin
     if (Voices.Items[i].MidiKeyNr = MidiEvent.midiData[1]) then
      begin
       Voices.Delete(i);
       Break;
      end;
    end;
  end
 else if ((status = $B0) and (MidiEvent.midiData[1] = $7E)) then
  begin
   // all notes off
   Voices.Clear;
  end;
end;

end.
