unit DAV_VSTModuleWithMidi;

interface

{$I ..\DAV_Compiler.Inc}

uses
  Classes, DAV_Types, DAV_VSTCustomModule, DAV_VSTEffect;

type
  TProcessEvents = procedure(Sender: TObject; const Events: TVstEvents) of object;
  TProcessEvent = procedure(Sender: TObject; const Event: TVstEvent) of object;
  TProcessMidiEvent = procedure(Sender: TObject; const MidiEvent: TVstMidiEvent) of object;
  TProcessMidiSysExEvent = procedure(Sender: TObject; const MidiSysExEvent: TVstMidiSysexEvent) of object;

  TVSTModuleWithMidi = class(TCustomVSTModule)
  protected
    FVstEvents          : TVstEvents;
    FOnProcessEvents    : TProcessEvents;
    FOnProcessEvent     : TProcessEvent;
    FOnProcessMidi      : TProcessMidiEvent;
    FOnProcessMidiSysEx : TProcessMidiSysExEvent;
    procedure ProcessEvents(const Events: TVstEvents); virtual;
    procedure ProcessEvent(const Event: TVstEvent); virtual;
    procedure ProcessMidiEvent(const MidiEvent: TVstMidiEvent); virtual;
    procedure ProcessMidiSysExEvent(const MidiSysExEvent: TVstMidiSysexEvent); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure HostCallProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal); override;
    procedure HostCallProcess32Replacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal); override;
    procedure HostCallProcess64Replacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal); override;

    function HostCallProcessEvents(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetCurrentMidiProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr; override;

    procedure MidiOut(const b1, b2, b3: Byte; b4: Byte = 0; const Offset: Integer = 0);
    procedure MidiSendSysEx(const Data: array of Byte; const Offset: Integer = 0);
    procedure MidiCC(const Channel, Number, Value: Integer; const Offset: Integer = 0);
    procedure MidiChannelAftertouch(const Channel, Value: Integer; const Offset: Integer = 0);
    procedure MidiNoteOff(const Channel, Note: Integer; const Value: Integer = 0; const Offset: Integer = 0);
    procedure MidiNoteOn(const Channel, Note, Value: Integer; const Offset: Integer = 0);
    procedure MidiPitchBend(ch, val: Integer; const Offset: Integer = 0);
    procedure MidiPitchBend2(const Channel, x1, x2: Integer; const Offset: Integer = 0);
    procedure MidiPolyAftertouch(const Channel, Note, Value: Integer; const Offset: Integer = 0);
    procedure MidiProgramChange(const Channel, Value: Integer; const Offset: Integer = 0);

    property OnProcessEvents: TProcessEvents read FOnProcessEvents write FOnProcessEvents;
    property OnProcessEvent: TProcessEvent read FOnProcessEvent write FOnProcessEvent;
    property OnProcessMidi: TProcessMidiEvent read FOnProcessMidi write FOnProcessMidi;
    property OnProcessMidiSysEx: TProcessMidiSysExEvent read FOnProcessMidiSysEx write FOnProcessMidiSysEx;
  end;
  
implementation

const
  CMaxMidiEvents = 1024;

constructor TVSTModuleWithMidi.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  FVstEvents.numEvents := 0;

  for i := 0 to CMaxMidiEvents - 1 do
   begin
    GetMem(FVstEvents.Events[i], SizeOf(TVstMidiEvent));
    FillChar(FVstEvents.Events[i]^, SizeOf(TVstMidiEvent), 0);
    PVstMidiEvent(FVstEvents.Events[i])^.EventType := etMidi;
    PVstMidiEvent(FVstEvents.Events[i])^.ByteSize := 24;
   end;
end;

destructor TVSTModuleWithMidi.Destroy;
var
  i : Integer;
begin
 try
  for i := 0 to CMaxMidiEvents - 1 do
   if Assigned(FVstEvents.Events[i]) then
    begin
     Dispose(FVstEvents.Events[i]);
     FVstEvents.Events[i] := nil;
    end;
 finally
  inherited;
 end;
end;

function TVSTModuleWithMidi.HostCallProcessEvents(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallProcessEvents'); {$ENDIF}
 Result:= inherited HostCallProcessEvents(Index, Value, ptr, opt);
 if Assigned(ptr)
  then ProcessEvents(PVstEvents(ptr)^);
end;

procedure TVSTModuleWithMidi.ProcessEvents(const Events: TVstEvents);
var
  Event: Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('ProcessEvents'); {$ENDIF}
 if Assigned(FOnProcessEvents) then FOnProcessEvents(Self, Events);
 with Events do
  for Event := 0 to numEvents - 1 do
   case Events[Event]^.EventType of
    etMidi  : ProcessMidiEvent(PVstMidiEvent(Events[Event])^);
    etSysEx : ProcessMidiSysExEvent(PVstMidiSysExEvent(Events[Event])^);
    else ProcessEvent(Events[Event]^);
   end;
end;

procedure TVSTModuleWithMidi.ProcessEvent(const Event: TVstEvent);
begin
 if Assigned(FOnProcessMidi) then FOnProcessEvent(Self, Event);
end;

procedure TVSTModuleWithMidi.ProcessMidiEvent(const MidiEvent: TVstMidiEvent);
begin
 if Assigned(FOnProcessMidi) then FOnProcessMidi(Self, MidiEvent);
end;

procedure TVSTModuleWithMidi.ProcessMidiSysExEvent(const MidiSysExEvent: TVstMidiSysexEvent);
begin
 if Assigned(FOnProcessMidiSysEx) then FOnProcessMidiSysEx(Self, MidiSysExEvent);
end;

function TVSTModuleWithMidi.HostCallGetCurrentMidiProgram(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
  Result := -1;
end;


procedure TVSTModuleWithMidi.HostCallProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
 inherited;
 {$IFDEF DebugLog} AddLogMessage('HostCallProcess - MIDI Processing'); {$ENDIF}
 if FVstEvents.numEvents > 0 then
  begin
   SendVstEventsToHost(FVstEvents);
   FVstEvents.numEvents := 0;
  end;
end;

procedure TVSTModuleWithMidi.HostCallProcess32Replacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
 inherited;

 if FVstEvents.numEvents > 0 then
  begin
   {$IFDEF DebugLog} AddLogMessage('HostCallProcess32Replacing - MIDI Processing'); {$ENDIF}
   SendVstEventsToHost(FVstEvents);
   FVstEvents.numEvents := 0;
  end;
end;

procedure TVSTModuleWithMidi.HostCallProcess64Replacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
begin
 inherited;
 {$IFDEF DebugLog} AddLogMessage('HostCallProcess64Replacing - MIDI Processing'); {$ENDIF}
 if FVstEvents.numEvents > 0 then
  begin
   SendVstEventsToHost(FVstEvents);
   FVstEvents.numEvents := 0;
  end;
end;

procedure TVSTModuleWithMidi.MidiOut(const b1, b2, b3: Byte; b4: Byte = 0; const Offset: Integer = 0);
begin
 {$IFDEF DebugLog} AddLogMessage('MidiOut'); {$ENDIF}
 with PVstMidiEvent(FVstEvents.Events[FVstEvents.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := b1;
   MidiData[1] := b2;
   MidiData[2] := b3;
   MidiData[3] := b4;
   DeltaFrames := offset;
   if FVstEvents.numEvents < CMaxMidiEvents - 1 then Inc(FVstEvents.numEvents);
  end;
end;

procedure TVSTModuleWithMidi.MidiCC(const Channel, Number, Value: Integer; const Offset: Integer = 0);
begin
 {$IFDEF DebugLog} AddLogMessage('MidiCC'); {$ENDIF}
 with PVstMidiEvent(FVstEvents.Events[FVstEvents.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $B0 + Channel;
   MidiData[1] := Number;
   MidiData[2] := Value;
   DeltaFrames := Offset;
   if FVstEvents.numEvents < CMaxMidiEvents - 1 then Inc(FVstEvents.numEvents);
  end;
end;

procedure TVSTModuleWithMidi.MidiChannelAftertouch(const Channel, Value: Integer; const Offset: Integer = 0);
begin
 {$IFDEF DebugLog} AddLogMessage('MidiChannelAftertouch'); {$ENDIF}
 with PVstMidiEvent(FVstEvents.Events[FVstEvents.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $D0 + Channel;
   MidiData[1] := Value;
   MidiData[2] := 0;
   DeltaFrames := Offset;
   if FVstEvents.numEvents < CMaxMidiEvents - 1 then Inc(FVstEvents.numEvents);
  end;
end;

procedure TVSTModuleWithMidi.MidiNoteOff(const Channel, Note: Integer; const Value: Integer = 0; const Offset: Integer = 0);
begin
 {$IFDEF DebugLog} AddLogMessage('MidiNoteOff'); {$ENDIF}
 with PVstMidiEvent(FVstEvents.Events[FVstEvents.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $80 + Channel;
   MidiData[1] := Note;
   MidiData[2] := Value;
   DeltaFrames := Offset;
   if FVstEvents.numEvents < CMaxMidiEvents - 1 then Inc(FVstEvents.numEvents);
  end;
end;

procedure TVSTModuleWithMidi.MidiNoteOn(const Channel, Note, Value: Integer; const Offset: Integer = 0);
begin
 {$IFDEF DebugLog} AddLogMessage('MidiNoteOn'); {$ENDIF}
 with PVstMidiEvent(FVstEvents.Events[FVstEvents.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $90 + Channel;
   MidiData[1] := Note;
   MidiData[2] := Value;
   DeltaFrames := Offset;
   if FVstEvents.numEvents < CMaxMidiEvents - 1
    then Inc(FVstEvents.numEvents);
  end;
end;

procedure TVSTModuleWithMidi.MidiPitchBend(ch, val: Integer; const Offset: Integer = 0);
var
  a, b: Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('MidiPitchBend'); {$ENDIF}
 with PVstMidiEvent(FVstEvents.Events[FVstEvents.numEvents])^ do
  begin
   EventType := etMidi;
   a := (val div 128) + 64;
   b := (val div 128);
   b := val - b * 128;
   MidiData[0] := $E0 + ch;
   MidiData[1] := b;
   MidiData[2] := a;
   DeltaFrames := offset;
   if FVstEvents.numEvents < CMaxMidiEvents - 1 then Inc(FVstEvents.numEvents);
  end;
end;

procedure TVSTModuleWithMidi.MidiPitchBend2(const Channel, x1, x2: Integer; const Offset: Integer = 0);
begin
 {$IFDEF DebugLog} AddLogMessage('MidiPitchBend2'); {$ENDIF}
 with PVstMidiEvent(FVstEvents.Events[FVstEvents.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $E0 + Channel;
   MidiData[1] := x1;
   MidiData[2] := x2;
   DeltaFrames := Offset;
   if FVstEvents.numEvents < CMaxMidiEvents - 1 then Inc(FVstEvents.numEvents);
  end;
end;

procedure TVSTModuleWithMidi.MidiPolyAftertouch(const Channel, Note, Value: Integer; const Offset: Integer = 0);
begin
 {$IFDEF DebugLog} AddLogMessage('MidiPolyAftertouch'); {$ENDIF}
 with PVstMidiEvent(FVstEvents.Events[FVstEvents.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $A0 + Channel;
   MidiData[1] := Note;
   MidiData[2] := Value;
   DeltaFrames := Offset;
   if FVstEvents.numEvents < CMaxMidiEvents - 1 then Inc(FVstEvents.numEvents);
  end;
end;

procedure TVSTModuleWithMidi.MidiProgramChange(const Channel, Value: Integer; const Offset: Integer = 0);
begin
 {$IFDEF DebugLog} AddLogMessage('MidiProgramChange'); {$ENDIF}
 with PVstMidiEvent(FVstEvents.Events[FVstEvents.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $C0 + Channel;
   MidiData[1] := Value;
   MidiData[2] := 0;
   DeltaFrames := Offset;
   if FVstEvents.numEvents < CMaxMidiEvents - 1
    then Inc(FVstEvents.numEvents);
  end;
end;

procedure TVSTModuleWithMidi.MidiSendSysEx(const Data: array of Byte; const Offset: Integer = 0);
begin
 {$IFDEF DebugLog} AddLogMessage('MidiSendSysEx'); {$ENDIF}
 with PVstMidiSysexEvent(FVstEvents.Events[FVstEvents.numEvents])^ do
  begin
   dumpBytes := Length(Data);
   if EventType = etSysEx
    then ReallocMem(sysexDump, dumpBytes)
    else GetMem(sysexDump, dumpBytes);
   EventType := etSysEx;
   DeltaFrames := Offset;
   Move(Data[0], sysexDump^, dumpBytes);
   if FVstEvents.numEvents < CMaxMidiEvents - 1
    then Inc(FVstEvents.numEvents);
  end;
end;

end.
