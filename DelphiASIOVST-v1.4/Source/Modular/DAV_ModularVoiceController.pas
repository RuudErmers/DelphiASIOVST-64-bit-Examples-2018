unit DAV_ModularVoiceController;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_ModularBaseComponent, DAV_Types, DAV_ModularVoiceList, DAV_ModularVoice;

type
  TDspVoiceController = class;
  TDspLimitVoiceType = (lvtKillOldest, lvtKillNearest, lvtIgnoreNew);

  TDspOnVCCreateVoice       = procedure(Sender: TDspVoiceController; MidiEvent: TDAVMidiEvent; var NewVoice: TDspVoice) of object;
  TDspOnVCVoiceNoteOff      = procedure(Sender: TDspVoiceController; Voice: TDspVoice) of object;
  TDspOnVCBeforeVoiceFree   = procedure(Sender: TDspVoiceController; Voice: TDspVoice) of object;
  TDspOnVCVoiceCountChanged = procedure(Sender: TDspVoiceController; ActiveVoices, AllVoices: Integer) of object;


  TDspVoiceController = class(TDspBaseComponent)
  protected
    FMaxVoices           : Integer;
    FLimitVoices         : TDspLimitVoiceType;
    FFilterNoteEvents    : Boolean;
    FVoiceList           : TDspVoiceList;

    FSampleProcessRun    : array of integer;

    FOnBeforeVoiceFree   : TDspOnVCBeforeVoiceFree;
    FOnCreateVoice       : TDspOnVCCreateVoice;
    FOnVoiceCountChanged : TDspOnVCVoiceCountChanged;
    FOnVoiceNoteOff      : TDspOnVCVoiceNoteOff;

    procedure SetEnabled(const Value: Boolean); override;
    procedure SetMaxVoices(const Value: Integer);
    procedure SampleRateChanged; override;
    procedure ChannelsChanged; override;
    procedure BeforeDestroy; override;

    procedure IncProcessSampleCount(SampleCount: Integer = 0; Channel: integer = -1); override;


    procedure Process(var Data: Single; const channel: integer); overload;
    procedure Process(var Data: Double; const channel: integer); overload;
    procedure Process(var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer); overload;
    procedure Process(var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer); overload;
    procedure Process(var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer); overload;
    procedure Process(var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer); overload;
  public
    procedure Init; override;
    procedure Reset; override;
    procedure ProcessMidiEvent(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean); override;

    function AddVoice(v: TDspVoice): Boolean; // returns false if failed
    procedure RemoveVoice(v: TDspVoice);

    procedure MidiNoteOn(MidiEvent: TDAVMidiEvent);
    procedure MidiNoteOff(MidiEvent: TDAVMidiEvent);
    procedure MidiAllNotesOff;

    {
    procedure SendParamChange(ParamId: Integer; ParamValue: Single); overload;
    procedure SendParamChange(ParamId: Integer; ParamValue: Pointer); overload; }
  published
    property MaxVoices: Integer read FMaxVoices write SetMaxVoices default 16;
    property LimitVoices: TDspLimitVoiceType read FLimitVoices write FLimitVoices default lvtIgnoreNew;
    property FilterNoteEvents: Boolean read FFilterNoteEvents write FFilterNoteEvents default true;

    property OnCreateVoice: TDspOnVCCreateVoice read FOnCreateVoice write FOnCreateVoice;
    property OnBeforeVoiceFree: TDspOnVCBeforeVoiceFree read FOnBeforeVoiceFree write FOnBeforeVoiceFree;
    property OnVoiceCountChanged: TDspOnVCVoiceCountChanged read FOnVoiceCountChanged write FOnVoiceCountChanged;
    property OnVoiceNoteOff: TDspOnVCVoiceNoteOff read FOnVoiceNoteOff write FOnVoiceNoteOff;
  end;
  
implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  SysUtils
  {$IFDEF PUREPASCAL},DAV_BufferMathPascal{$ELSE},DAV_BufferMathAsm{$ENDIF},
  DAV_ProcessingComponent;

{ TDspVoiceController }

procedure TDspVoiceController.Init;
begin
  inherited;

  fStdProcessS   := Process;
  fStdProcessD   := Process;
  fStdProcessSA  := Process;
  fStdProcessDA  := Process;
  fStdProcessSAA := Process;
  fStdProcessDAA := Process;

  FMaxVoices := 16;
  FLimitVoices := lvtIgnoreNew;
  FFilterNoteEvents := true;
  
  FVoiceList := TDspVoiceList.Create;
end;

procedure TDspVoiceController.Reset;
begin
  while FVoiceList.count>0 do RemoveVoice(FVoiceList.items[0]);
  if assigned(FOnVoiceCountChanged) then
    FOnVoiceCountChanged(self, FVoiceList.PlayingVoiceCount, FVoiceList.Count);
end;

procedure TDspVoiceController.BeforeDestroy;
begin
  inherited;
  Reset;
  FVoiceList.Free;
  setlength(FSampleProcessRun, 0);
end;

procedure TDspVoiceController.ChannelsChanged;
begin
  inherited;
  FVoiceList.SetChannels(fChannels);
  setlength(FSampleProcessRun, fChannels);
  fillchar(FSampleProcessRun, fChannels * SizeOf(integer), 0);
end;

procedure TDspVoiceController.SampleRateChanged;
begin
  inherited;
  FVoiceList.SetSampleRate(fSampleRate);
end;

procedure TDspVoiceController.SetEnabled(const Value: Boolean);
begin
  inherited;
  if not fEnabled then reset;
end;

procedure TDspVoiceController.SetMaxVoices(const Value: Integer);
begin
  FMaxVoices := Value;

  if FMaxVoices>0 then
    while FVoiceList.count>FMaxVoices do
      RemoveVoice(FVoiceList.items[0]);
end;

function TDspVoiceController.AddVoice(v: TDspVoice): Boolean;
begin
  Result := false;
  if FVoiceList.Count<FMaxVoices then
  begin
    FVoiceList.Add(v);
    Result := true;
  end else case FLimitVoices of
    lvtKillOldest:  begin
                      RemoveVoice(FVoiceList.GetOldestVoice);
                      FVoiceList.Add(v);
                      Result := true;
                    end;
    lvtKillNearest: begin
                      RemoveVoice(FVoiceList.GetNearestVoice(v.VoiceInfo.NoteNr));
                      FVoiceList.Add(v);
                      Result := true;
                    end;
  end;

  if Result then
  begin
    v.SampleRate := fSampleRate;
    v.Channels   := fChannels;
    if assigned(FOnVoiceCountChanged) then
      FOnVoiceCountChanged(self, FVoiceList.PlayingVoiceCount, FVoiceList.Count);
  end;
end;

procedure TDspVoiceController.RemoveVoice(v: TDspVoice);
begin
  if v=nil then exit;

  if assigned(FOnBeforeVoiceFree) then FOnBeforeVoiceFree(self, v);
  
  if FVoiceList.IndexOf(v)>=0 then FVoiceList.Remove(v);
  v.Free;

  if assigned(FOnVoiceCountChanged) then
    FOnVoiceCountChanged(self, FVoiceList.PlayingVoiceCount, FVoiceList.Count);
end;


procedure TDspVoiceController.MidiNoteOn(MidiEvent: TDAVMidiEvent);
var newVoice: TDspVoice;
begin
  // if same key has a playing voice: force it to go off (should never happen)
  MidiNoteOff(MidiEvent);

  if not fEnabled then exit;
  
  newVoice := nil;
  if assigned(FOnCreateVoice) then FOnCreateVoice(self, MidiEvent, newVoice);
  if not assigned(newVoice) then exit;

  if AddVoice(newVoice) then
  begin
    if assigned(FOnVoiceCountChanged) then
      FOnVoiceCountChanged(self, FVoiceList.PlayingVoiceCount, FVoiceList.Count);
  end else newVoice.Free;
end;

procedure TDspVoiceController.MidiNoteOff(MidiEvent: TDAVMidiEvent);
var tmp: TDspVoice;
begin
  tmp := FVoiceList.GetVoiceByKey(MidiEvent.MidiData[1]);
  if tmp<>nil then
  begin
    tmp.VoiceNoteOff;
    if assigned(FOnVoiceNoteOff) then FOnVoiceNoteOff(self, tmp);

    if assigned(FOnVoiceCountChanged) then
      FOnVoiceCountChanged(self, FVoiceList.PlayingVoiceCount, FVoiceList.Count);
  end;
end;

procedure TDspVoiceController.MidiAllNotesOff;
var i: integer;
begin
  for i := FVoiceList.Count - 1 downto 0 do
  begin
    FVoiceList.items[i].VoiceNoteOff;
    if assigned(FOnVoiceNoteOff) then FOnVoiceNoteOff(self, FVoiceList.items[i]);
  end;

  if assigned(FOnVoiceCountChanged) then
    FOnVoiceCountChanged(self, FVoiceList.PlayingVoiceCount, FVoiceList.Count);
end;

{
procedure TDspVoiceController.SendParamChange(ParamId: Integer; ParamValue: Pointer);
begin

end;

procedure TDspVoiceController.SendParamChange(ParamId: Integer; ParamValue: Single);
begin

end;
 }



procedure TDspVoiceController.Process(var Data: Single; const channel: integer);
var i: integer; tmp, backup: single;
begin
  IncProcessSampleCount(1, channel);

  backup := Data;
  Data   := 0;

  for i := FVoiceList.Count - 1 downto 0 do
  begin
    tmp := backup;
    FVoiceList.items[i].ProcessS(tmp, channel);
    Data := Data + tmp;
  end;
end;

procedure TDspVoiceController.Process(var Data: Double; const channel: integer);
var
  i           : Integer;
  tmp, backup : Double;
begin
  IncProcessSampleCount(1, channel);

  backup := Data;
  Data   := 0;

  for i := FVoiceList.Count - 1 downto 0 do
   begin
    tmp := backup;
    FVoiceList.items[i].ProcessD(tmp, channel);
    Data := Data + tmp;
   end;
end;

procedure TDspVoiceController.Process(var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer);
var
  i           : Integer;
  tmp, backup : TDAVSingleDynArray;
begin
  IncProcessSampleCount(SampleFrames, channel);

  backup := copy(ProcessBuffer);
  fillchar(ProcessBuffer, SampleFrames * SizeOf(Single), 0);

  for i := FVoiceList.Count - 1 downto 0 do
  begin
    tmp := copy(backup);
    FVoiceList.items[i].ProcessSA(tmp, channel, SampleFrames);

    AddArrays(tmp, ProcessBuffer, ProcessBuffer, SampleFrames);
  end;
end;

procedure TDspVoiceController.Process(var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer);
var
  i           : Integer;
  tmp, backup : TDAVDoubleDynArray;
begin
  IncProcessSampleCount(SampleFrames, channel);

  backup := copy(ProcessBuffer);
  fillchar(ProcessBuffer, SampleFrames * SizeOf(Double), 0);

  for i := FVoiceList.Count - 1 downto 0 do
   begin
    tmp := copy(backup);
    FVoiceList.items[i].ProcessDA(tmp, channel, SampleFrames);

    AddArrays(tmp, ProcessBuffer, ProcessBuffer, SampleFrames);
   end;
end;

procedure TDspVoiceController.Process(var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer);
var
  i           : Integer;
  tmp, backup : TDAVArrayOfSingleDynArray;
begin
  IncProcessSampleCount(SampleFrames);

  CreateArrayCopy(ProcessBuffer, backup, fChannels, SampleFrames);
  ClearArrays(ProcessBuffer, fChannels, SampleFrames);

  for i := FVoiceList.Count - 1 downto 0 do
   begin
    CreateArrayCopy(backup, tmp, fChannels, SampleFrames);
    FVoiceList.items[i].ProcessSAA(tmp, SampleFrames);

    AddArrays(tmp, ProcessBuffer, ProcessBuffer, fChannels, SampleFrames);
   end;
end;

procedure TDspVoiceController.Process(var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer);
var i: integer; tmp, backup: TDAVArrayOfDoubleDynArray;
begin
  IncProcessSampleCount(SampleFrames);

  CreateArrayCopy(ProcessBuffer, backup, fChannels, SampleFrames);
  ClearArrays(ProcessBuffer, fChannels, SampleFrames);

  for i := FVoiceList.Count - 1 downto 0 do
   begin
    CreateArrayCopy(backup, tmp, fChannels, SampleFrames);
    FVoiceList.items[i].ProcessDAA(tmp, SampleFrames);

    AddArrays(tmp, ProcessBuffer, ProcessBuffer, fChannels, SampleFrames);
   end;
end;

procedure TDspVoiceController.IncProcessSampleCount(SampleCount, Channel: integer);
var
  i        : Integer;
  doUpdate : Boolean;
begin
  doUpdate := channel < 0;

  if not doUpdate then
  begin
    FSampleProcessRun[Channel] := FSampleProcessRun[Channel] + SampleCount;
    doUpdate := (FSampleProcessRun[Channel] > 511);
    if doUpdate then FSampleProcessRun[Channel] := 0;
  end;

  if doUpdate then
  begin
    for i := FVoiceList.Count - 1 downto 0 do
      if not FVoiceList.items[i].IsAlive then
        RemoveVoice(FVoiceList.items[i])
      else
        FVoiceList.items[i].DecrementTrailing(SampleCount, Channel);
  end;
end;

procedure TDspVoiceController.ProcessMidiEvent(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean);
var
  Status: Byte;
begin
  Status := MidiEvent.midiData[0] and $F0; // channel information is removed

  if (Status = $90) and (MidiEvent.mididata[2] > 0) then // "note on" ?
  begin
    FilterEvent := FFilterNoteEvents;
    MidiNoteOn(MidiEvent);
  end else if ((status = $90) and (MidiEvent.mididata[2] = 0)) or (status = $80) then // "note off" ?
  begin
    FilterEvent := FFilterNoteEvents;
    MidiNoteOff(MidiEvent);
  end else if ((status = $B0) and (MidiEvent.midiData[1] = $7E)) then
  begin
    FilterEvent := FFilterNoteEvents;
    MidiAllNotesOff;
  end;

  if not FilterEvent or not fEnabled
   then FVoiceList.ProcessMidiEvent(MidiEvent, FilterEvent);
end;

end.
