unit DAV_ModularVoice;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_ProcessingComponent;

type
  TDspVoice = class;

  TDspOnVoiceNoteOff = procedure(Sender: TDspVoice; var CanGoOff: Boolean) of object;

  TDspVoiceProcessingMode = (pmDspQueue, pmUser);

  TDspVoiceInfo = class(TObject)
  public
    InitialMidiEvent: TDAVMidiEvent;
    NoteNr: Byte;
    Velocity: single;
    Offset: LongInt;

    constructor Create(MidiEvent: TDAVMidiEvent);
  end;

  TDspVoiceTrailingType = (vttAutomatic, vttManually);
  TDspVoice = class(TDataModule)
  protected
    FEnabled:         Boolean;
    FSampleRate:      Single;
    FChannels:        Integer;

    FIsVoiceNoteOn:   Boolean;
    FTrailingSamples: Integer;
    FTrailingType:    TDspVoiceTrailingType;

    FOffTrailingCounter: array of Integer;

    FVoiceInfo:       TDspVoiceInfo;
    FDspQueueList:    TDAVProcessingComponentList;
    FDspDirectProcessItem: TDAVProcessingComponent;

    FProcessS:   TDspBaseProcessFuncS;
    FProcessD:   TDspBaseProcessFuncD;
    FProcessSA:  TDspBaseProcessFuncSA;
    FProcessDA:  TDspBaseProcessFuncDA;
    FProcessSAA: TDspBaseProcessFuncSAA;
    FProcessDAA: TDspBaseProcessFuncDAA;

    FDefaultProcessS:   TDspBaseProcessFuncS;
    FDefaultProcessD:   TDspBaseProcessFuncD;
    FDefaultProcessSA:  TDspBaseProcessFuncSA;
    FDefaultProcessDA:  TDspBaseProcessFuncDA;
    FDefaultProcessSAA: TDspBaseProcessFuncSAA;
    FDefaultProcessDAA: TDspBaseProcessFuncDAA;

    FUserProcessS:   TDspBaseProcessFuncS;
    FUserProcessD:   TDspBaseProcessFuncD;
    FUserProcessSA:  TDspBaseProcessFuncSA;
    FUserProcessDA:  TDspBaseProcessFuncDA;
    FUserProcessSAA: TDspBaseProcessFuncSAA;
    FUserProcessDAA: TDspBaseProcessFuncDAA;

    FVoiceProcessingMode: TDspVoiceProcessingMode;

    FOnVoiceNoteOff: TDspOnVoiceNoteOff;

    procedure UpdateProcessingFunctions; virtual;
    procedure InitializeTrailing; virtual;

    procedure SetEnabled(const Value: Boolean);   virtual;
    procedure SetSampleRate(const Value: Single); virtual;
    procedure SetChannels(const Value: Integer);  virtual;

    // placeholders for unset processing procedures
    procedure ProcessBasic      (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;

    // for enabled = false
    procedure ProcessSilence    (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessSilence    (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessSilence    (var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessSilence    (var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessSilence    (var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessSilence    (var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;

    // for dsp direct processing mode
    procedure ProcessDspItem    (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessDspItem    (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessDspItem    (var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessDspItem    (var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessDspItem    (var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessDspItem    (var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;

    procedure SetUserProcessD   (const Value: TDspBaseProcessFuncD);
    procedure SetUserProcessDA  (const Value: TDspBaseProcessFuncDA);
    procedure SetUserProcessDAA (const Value: TDspBaseProcessFuncDAA);
    procedure SetUserProcessS   (const Value: TDspBaseProcessFuncS);
    procedure SetUserProcessSA  (const Value: TDspBaseProcessFuncSA);
    procedure SetUserProcessSAA (const Value: TDspBaseProcessFuncSAA);

    procedure SetDspDirectProcessItem(v: TDAVProcessingComponent); virtual;

    procedure SetVoiceProcessingMode(const Value: TDspVoiceProcessingMode);

    function GetIsAlive: Boolean;

    procedure SetTrailingSamples(const Value: Integer);  virtual;
    procedure SetTrailingType(const Value: TDspVoiceTrailingType); virtual;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; VoiceInfo: TDspVoiceInfo); reintroduce; overload;
    destructor Destroy; override;

    procedure RegisterDSPItem(item: TDAVProcessingComponent);
    procedure UnRegisterDSPItem(item: TDAVProcessingComponent);

    procedure Init;  virtual;
    procedure Reset; virtual;

    procedure ProcessMidiEvent(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean); virtual;

    procedure VoiceNoteOff; virtual;
    procedure DecrementTrailing(DecrementTrailing: Integer = 0; Channel: integer = -1);

    procedure UpdateTrailingSamples; virtual;

    property IsAlive:        Boolean read GetIsAlive;
    property IsVoiceNoteOn:  Boolean read FIsVoiceNoteOn;

    property ProcessS:   TDspBaseProcessFuncS   read FProcessS;
    property ProcessD:   TDspBaseProcessFuncD   read FProcessD;
    property ProcessSA:  TDspBaseProcessFuncSA  read FProcessSA;
    property ProcessDA:  TDspBaseProcessFuncDA  read FProcessDA;
    property ProcessSAA: TDspBaseProcessFuncSAA read FProcessSAA;
    property ProcessDAA: TDspBaseProcessFuncDAA read FProcessDAA;

    property VoiceInfo: TDspVoiceInfo read FVoiceInfo;

    property Channels:   Integer read FChannels   write SetChannels   default 2;
    property SampleRate: Single  read FSampleRate write SetSampleRate;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default true;

    property DspDirectProcessItem: TDAVProcessingComponent read fDspDirectProcessItem write SetDspDirectProcessItem default nil;

    property VoiceProcessingMode: TDspVoiceProcessingMode read FVoiceProcessingMode write SetVoiceProcessingMode;

    property OnProcessS:   TDspBaseProcessFuncS   read FUserProcessS   write SetUserProcessS;
    property OnProcessD:   TDspBaseProcessFuncD   read FUserProcessD   write SetUserProcessD;
    property OnProcessSA:  TDspBaseProcessFuncSA  read FUserProcessSA  write SetUserProcessSA;
    property OnProcessDA:  TDspBaseProcessFuncDA  read FUserProcessDA  write SetUserProcessDA;
    property OnProcessSAA: TDspBaseProcessFuncSAA read FUserProcessSAA write SetUserProcessSAA;
    property OnProcessDAA: TDspBaseProcessFuncDAA read FUserProcessDAA write SetUserProcessDAA;

    property OnVoiceNoteOff: TDspOnVoiceNoteOff read FOnVoiceNoteOff write FOnVoiceNoteOff;

    property TrailingType: TDspVoiceTrailingType read FTrailingType write SetTrailingType;
    property TrailingSamples: Integer read FTrailingSamples write SetTrailingSamples;
  end;

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Forms, SysUtils, DAV_Common
  {$IFDEF PUREPASCAL}, DAV_BufferMathPascal{$ELSE}, DAV_BufferMathAsm{$ENDIF};

constructor TDspVoiceInfo.Create(MidiEvent: TDAVMidiEvent);
begin
  inherited Create;
  NoteNr := MidiEvent.MidiData[1];

  Velocity := MidiEvent.MidiData[2] / 127;
  Offset := MidiEvent.DeltaFrames;
  InitialMidiEvent := MidiEvent;
end;



{ TDspVoice }

constructor TDspVoice.Create(AOwner: TComponent);
begin
  FDefaultProcessS   := ProcessSilence;
  FDefaultProcessD   := ProcessBasic;
  FDefaultProcessSA  := ProcessBasic;
  FDefaultProcessDA  := ProcessBasic;
  FDefaultProcessSAA := ProcessBasic;
  FDefaultProcessDAA := ProcessBasic;

  FUserProcessS   := nil;
  FUserProcessD   := nil;
  FUserProcessSA  := nil;
  FUserProcessDA  := nil;
  FUserProcessSAA := nil;
  FUserProcessDAA := nil;

  FDspQueueList   := TDAVProcessingComponentList.Create;
  FVoiceInfo      := nil;
  FEnabled        := true;
  FIsVoiceNoteOn  := true;

  FDspDirectProcessItem:=nil;
  FTrailingType := vttAutomatic;
  
  inherited Create(AOwner);

  Init;

  UpdateProcessingFunctions;
end;

constructor TDspVoice.Create(AOwner: TComponent; VoiceInfo: TDspVoiceInfo);
begin
  Create(AOwner);
  FVoiceInfo:=VoiceInfo;
end;

destructor TDspVoice.Destroy;
begin
  FDspQueueList.Free;
  if assigned(FVoiceInfo) then FVoiceInfo.free;
  inherited;
end;

procedure TDspVoice.Init;
begin

end;

procedure TDspVoice.Reset;
begin
  if (FVoiceProcessingMode = pmDspQueue) and Assigned(FDspDirectProcessItem) then
    FDspDirectProcessItem.ResetQueue;
end;

procedure TDspVoice.RegisterDSPItem(Item: TDAVProcessingComponent);
begin
  with FDspQueueList do
  begin
    if IndexOf(item) < 0 then
    begin
      Add(item);
      if (FVoiceProcessingMode = pmDspQueue) and not Assigned(FDspDirectProcessItem) then
      begin
        FDspDirectProcessItem:=item;
        UpdateProcessingFunctions;
      end;
    end;  
    Item.SampleRate:=FSampleRate;
    Item.Channels:=FChannels;  
  end;
end;

procedure TDspVoice.UnRegisterDSPItem(item: TDAVProcessingComponent);
begin
 with FDspQueueList do
  if IndexOf(item)>=0
   then Remove(item);

  if FDspDirectProcessItem=item then
  begin
    if FDspQueueList.Count>0 then FDspDirectProcessItem:=FDspQueueList.Items[0]
    else FDspDirectProcessItem:=nil;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetEnabled(const Value: Boolean);
begin
  if FEnabled<>value then
  begin
    FEnabled := value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetChannels(const Value: Integer);
begin
  if FChannels<>Value then
  begin
    FChannels := Value;
    FDspQueueList.SetChannels(FChannels);
    InitializeTrailing;
  end;
end;

procedure TDspVoice.SetSampleRate(const Value: Single);
begin
  if FSampleRate<>Value then
  begin
    FSampleRate := Value;
    FDspQueueList.SetSampleRate(FSampleRate);
  end;
end;

procedure TDspVoice.InitializeTrailing;
var i: integer;
begin
  setlength(FOffTrailingCounter, FChannels);
  for i:=FChannels-1 downto 0 do
    FOffTrailingCounter[i] := FTrailingSamples;
end;

procedure TDspVoice.UpdateProcessingFunctions;
begin
  if FEnabled then
  begin
    if FVoiceProcessingMode = pmUser then
    begin
      if assigned(FUserProcessS)   then FProcessS   := FUserProcessS   else FProcessS   := FDefaultProcessS;
      if assigned(FUserProcessD)   then FProcessD   := FUserProcessD   else FProcessD   := FDefaultProcessD;
      if assigned(FUserProcessSA)  then FProcessSA  := FUserProcessSA  else FProcessSA  := FDefaultProcessSA;
      if assigned(FUserProcessDA)  then FProcessDA  := FUserProcessDA  else FProcessDA  := FDefaultProcessDA;
      if assigned(FUserProcessSAA) then FProcessSAA := FUserProcessSAA else FProcessSAA := FDefaultProcessSAA;
      if assigned(FUserProcessDAA) then FProcessDAA := FUserProcessDAA else FProcessDAA := FDefaultProcessDAA;
    end else if assigned(FDspDirectProcessItem) then begin
      FProcessS   := ProcessDspItem;
      FProcessD   := ProcessDspItem;
      FProcessSA  := ProcessDspItem;
      FProcessDA  := ProcessDspItem;
      FProcessSAA := ProcessDspItem;
      FProcessDAA := ProcessDspItem;
    end else begin
      FProcessS   := ProcessSilence;
      FProcessD   := ProcessSilence;
      FProcessSA  := ProcessSilence;
      FProcessDA  := ProcessSilence;
      FProcessSAA := ProcessSilence;
      FProcessDAA := ProcessSilence;
    end;
  end else begin
    FProcessS   := ProcessSilence;
    FProcessD   := ProcessSilence;
    FProcessSA  := ProcessSilence;
    FProcessDA  := ProcessSilence;
    FProcessSAA := ProcessSilence;
    FProcessDAA := ProcessSilence;
  end;
end;

procedure TDspVoice.SetVoiceProcessingMode(const Value: TDspVoiceProcessingMode);
begin
  // no check for changes here!!!
  FVoiceProcessingMode := Value;
  UpdateProcessingFunctions;
end;

procedure TDspVoice.SetDspDirectProcessItem(v: TDAVProcessingComponent);
begin
  if v<>FDspDirectProcessItem then
  begin
    if v=nil then
    begin
      FDspDirectProcessItem:=v;
      UpdateProcessingFunctions;
    end else if FDspQueueList.IndexOf(v)>=0 then
    begin
      FDspDirectProcessItem:=v;
      SetVoiceProcessingMode(pmDspQueue);
    end else
      raise Exception.Create('DspDirectProcessItem has to be the first item of a queue');
  end;
end;




procedure TDspVoice.SetUserProcessD(const Value: TDspBaseProcessFuncD);
begin
  if @FUserProcessD<>@Value then
  begin
    FUserProcessD := Value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetUserProcessDA(const Value: TDspBaseProcessFuncDA);
begin
  if @FUserProcessDA<>@Value then
  begin
    FUserProcessDA := Value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetUserProcessDAA(const Value: TDspBaseProcessFuncDAA);
begin
  if @FUserProcessDAA<>@Value then
  begin
    FUserProcessDAA := Value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetUserProcessS(const Value: TDspBaseProcessFuncS);
begin
  if @FUserProcessS<>@Value then
  begin
    FUserProcessS := Value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetUserProcessSA(const Value: TDspBaseProcessFuncSA);
begin
  if @FUserProcessSA<>@Value then
  begin
    FUserProcessSA := Value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetUserProcessSAA(const Value: TDspBaseProcessFuncSAA);
begin
  if @FUserProcessSAA<>@Value then
  begin
    FUserProcessSAA := Value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetTrailingSamples(const Value: Integer);
begin
  if FTrailingSamples<>Value then
  begin
    FTrailingSamples := Value;
    FTrailingType := vttManually;
  end;
end;

procedure TDspVoice.SetTrailingType(const Value: TDspVoiceTrailingType);
begin
  if FTrailingType<>Value then
  begin
    FTrailingType := Value;
    UpdateTrailingSamples;
  end;
end;



procedure TDspVoice.ProcessSilence(var Data: Single; const channel: integer);
begin
  Data := 0;
end;

procedure TDspVoice.ProcessSilence(var Data: Double; const channel: integer);
begin
  Data := 0;
end;

procedure TDspVoice.ProcessSilence(var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer);
begin
 FillChar(ProcessBuffer[0], SampleFrames * SizeOf(Single), 0);
end;

procedure TDspVoice.ProcessSilence(var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer);
begin
 FillChar(ProcessBuffer[0], SampleFrames * SizeOf(Double), 0);
end;

procedure TDspVoice.ProcessSilence(var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer);
begin
  ClearArrays(ProcessBuffer, FChannels, SampleFrames);
end;

procedure TDspVoice.ProcessSilence(var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer);
begin
  ClearArrays(ProcessBuffer, FChannels, SampleFrames);
end;




procedure TDspVoice.ProcessBasic(var Data: Double; const channel: integer);
var tmp: single;
begin
  tmp := Data;
  FProcessS(tmp, channel);
  Data := tmp;
end;

procedure TDspVoice.ProcessBasic(var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer);
var i: integer;
begin
 for i := 0 to SampleFrames - 1
  do FProcessS(ProcessBuffer[i], channel);
end;

procedure TDspVoice.ProcessBasic(var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer);
var i: integer;
begin
 for i := 0 to SampleFrames - 1
  do FProcessD(ProcessBuffer[i], channel);
end;

procedure TDspVoice.ProcessBasic(var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer);
var i: integer;
begin
 for i := 0 to FChannels - 1
  do FProcessSA(ProcessBuffer[i], i, SampleFrames);
end;

procedure TDspVoice.ProcessBasic(var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer);
var i: integer;
begin
 for i := 0 to FChannels - 1
  do FProcessDA(ProcessBuffer[i], i, SampleFrames);
end;





procedure TDspVoice.ProcessDspItem(var Data: Single; const channel: integer);
begin
  FDspDirectProcessItem.ProcessQueueS(Data, channel);
end;

procedure TDspVoice.ProcessDspItem(var Data: Double; const channel: integer);
begin
  FDspDirectProcessItem.ProcessQueueD(Data, channel);
end;

procedure TDspVoice.ProcessDspItem(var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer);
begin
  FDspDirectProcessItem.ProcessQueueSA(ProcessBuffer, channel, SampleFrames);
end;

procedure TDspVoice.ProcessDspItem(var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer);
begin
  FDspDirectProcessItem.ProcessQueueDA(ProcessBuffer, channel, SampleFrames);
end;

procedure TDspVoice.ProcessDspItem(var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer);
begin
  FDspDirectProcessItem.ProcessQueueSAA(ProcessBuffer, SampleFrames);
end;

procedure TDspVoice.ProcessDspItem(var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer);
begin
  FDspDirectProcessItem.ProcessQueueDAA(ProcessBuffer, SampleFrames);
end;



procedure TDspVoice.UpdateTrailingSamples;
begin
  if FTrailingType = vttManually then exit;

  FTrailingSamples := FDspQueueList.TrailingSamplesQueue;
end;

procedure TDspVoice.ProcessMidiEvent(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean);
begin
  FDspQueueList.ProcessMidiEventQueue(MidiEvent, FilterEvent);
end;

procedure TDspVoice.VoiceNoteOff;
var CanGoOff: Boolean;
begin
  CanGoOff := true;
  if Assigned(FOnVoiceNoteOff) then FOnVoiceNoteOff(self, CanGoOff);
  if CanGoOff then
  begin
    FDspQueueList.NoteOffQueue;
    InitializeTrailing;
    FIsVoiceNoteOn:=false;
  end;
end;

procedure TDspVoice.DecrementTrailing(DecrementTrailing: Integer = 0; Channel: integer = -1);
var i: Integer;
begin
  if not FIsVoiceNoteOn and (DecrementTrailing>0) then
    if Channel>=0 then
      FOffTrailingCounter[Channel] := FOffTrailingCounter[Channel] - DecrementTrailing
    else for i:=FChannels-1 downto 0 do
      FOffTrailingCounter[i] := FOffTrailingCounter[i] - DecrementTrailing;
end;

function TDspVoice.GetIsAlive: Boolean;
var i: integer;
begin
  Result:=true;
  if FIsVoiceNoteOn then exit;

  for i:=FChannels-1 downto 0 do
    if FOffTrailingCounter[i]>0 then exit;

  Result:=false;
end;

end.
