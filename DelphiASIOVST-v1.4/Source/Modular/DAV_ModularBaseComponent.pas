unit DAV_ModularBaseComponent;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, Contnrs, DAV_ProcessingComponent;

type
  TDspQueueList = TComponentList;

  TDspBaseComponent = class(TDAVProcessingComponent)
  protected
    FNextDspQueueItem   : TDspBaseComponent;
    FPrevDspQueueItem   : TDspBaseComponent;

    FStdProcessS        : TDspBaseProcessFuncS;
    FStdProcessD        : TDspBaseProcessFuncD;
    FStdProcessSA       : TDspBaseProcessFuncSA;
    FStdProcessDA       : TDspBaseProcessFuncDA;
    FStdProcessSAA      : TDspBaseProcessFuncSAA;
    FStdProcessDAA      : TDspBaseProcessFuncDAA;

    FStdProcessQueueS   : TDspBaseProcessFuncS;
    FStdProcessQueueD   : TDspBaseProcessFuncD;
    FStdProcessQueueSA  : TDspBaseProcessFuncSA;
    FStdProcessQueueDA  : TDspBaseProcessFuncDA;
    FStdProcessQueueSAA : TDspBaseProcessFuncSAA;
    FStdProcessQueueDAA : TDspBaseProcessFuncDAA;

    function  GetTrailingSamplesQueue: integer; override;

    procedure SetBypass(const Value: Boolean); override;
    procedure SetEnabled(const Value: Boolean); override;
    procedure SetSampleRate(const Value: Single); override;
    procedure SetChannels(const Value: Integer); override;
    procedure SetTrailingSamples(const Value: Integer); override;

    procedure SetNextDspQueueItem(const Value: TDspBaseComponent); virtual;
    procedure SampleRateChanged; virtual;
    procedure ChannelsChanged; virtual;
    procedure UpdateParameters; virtual;
    procedure TrailingSamplesChanged; virtual;
    procedure BeforeDestroy; virtual;
    procedure UpdateProcessingFunc; virtual;

    procedure RegisterInOwner(item: TDspBaseComponent);
    procedure UnRegisterInOwner(item: TDspBaseComponent);

    procedure IncProcessSampleCount(SampleCount: Integer = 0; Channel: integer = -1); virtual;

    procedure ProcessSilence    (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessBypass     (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessQueueBasic (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessQueueBypass(var Data: Single; const channel: integer); overload; virtual;

    procedure ProcessSilence    (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessBypass     (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessBasic      (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessQueueBasic (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessQueueBypass(var Data: Double; const channel: integer); overload; virtual;

    procedure ProcessSilence    (var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessBypass     (var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBasic (var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBypass(var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer); overload; virtual;

    procedure ProcessSilence    (var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessBypass     (var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBasic (var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBypass(var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;

    procedure ProcessSilence    (var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessBypass     (var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBasic (var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBypass(var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;

    procedure ProcessSilence    (var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessBypass     (var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBasic (var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBypass(var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; UseSampleRate: Integer); reintroduce; overload;
    destructor Destroy; override;

    procedure Init; override;       // called automaticaly in constructor
    procedure Reset; override;      // called manualy
    procedure ResetQueue; override;

    procedure NoteOff; override;
    procedure NoteOffQueue; override;

    function  GetFollowingItems(var items: TDspQueueList): boolean; virtual; // Returns false on loopback
    function  GetPreviousItems(var items: TDspQueueList): boolean; virtual; // Returns false on loopback
    function  GetQueueItems(var items: TDspQueueList): boolean; virtual; // Returns false on loopback



    procedure ProcessMidiEvent(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean); override;
    procedure ProcessMidiEventQueue(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean); override;

    property PrevDspQueueItem: TDspBaseComponent read FPrevDspQueueItem write FPrevDspQueueItem;
  published
    property Enabled: Boolean                    read fEnabled          write SetEnabled    default true;
    property Bypass: Boolean                     read fBypass           write SetBypass     default false;
    property Channels: Integer                   read fChannels         write SetChannels   default 2;
    property SampleRate: Single                  read fSampleRate       write SetSampleRate;
    property NextDspQueueItem: TDspBaseComponent read FNextDspQueueItem write SetNextDspQueueItem;
  end;

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Sysutils, Math, DAV_Common, DAV_VSTModuleWithDsp, DAV_ModularVoice
  {$IFDEF PUREPASCAL}, DAV_BufferMathPascal{$ELSE}, DAV_BufferMathAsm{$ENDIF};

constructor TDspBaseComponent.Create(AOwner: TComponent);
begin
  inherited;

  FNextDspQueueItem := nil;
  FPrevDspQueueItem := nil;
  fEnabled          := true;
  fBypass           := false;
  fSampleRate       := 44100;
  fChannels         := 2;
  fTrailingSamples  := 0;

  FStdProcessS   := ProcessBypass;
  FStdProcessD   := ProcessBasic;
  FStdProcessSA  := ProcessBasic;
  FStdProcessDA  := ProcessBasic;
  FStdProcessSAA := ProcessBasic;
  FStdProcessDAA := ProcessBasic;

  FStdProcessQueueS  := ProcessQueueBasic;
  FStdProcessQueueD  := ProcessQueueBasic;
  FStdProcessQueueSA := ProcessQueueBasic;
  FStdProcessQueueDA := ProcessQueueBasic;
  FStdProcessQueueSAA:= ProcessQueueBasic;
  FStdProcessQueueDAA:= ProcessQueueBasic;

  RegisterInOwner(self);

  Init;

  UpdateProcessingFunc;
  
end;

constructor TDspBaseComponent.Create(AOwner: TComponent; UseSampleRate: Integer);
begin
  Create(AOwner);
  SetSampleRate(UseSampleRate);
end;

destructor TDspBaseComponent.Destroy;
begin
  BeforeDestroy;

  if assigned(FNextDspQueueItem) then
  begin
    FNextDspQueueItem.PrevDspQueueItem := FPrevDspQueueItem;
    if not assigned(FPrevDspQueueItem) then RegisterInOwner(FNextDspQueueItem);
  end;
  if assigned(FPrevDspQueueItem) then FPrevDspQueueItem.NextDspQueueItem := FNextDspQueueItem;

  UnRegisterInOwner(self);
  inherited;
end;

procedure TDspBaseComponent.BeforeDestroy;
begin end;

procedure TDspBaseComponent.RegisterInOwner(item: TDspBaseComponent);
begin
  if      Owner is TDspVSTModule then (Owner as TDspVSTModule).RegisterDSPItem(item)
  else if Owner is TDspVoice     then (Owner as TDspVoice    ).RegisterDSPItem(item);
end;

procedure TDspBaseComponent.UnRegisterInOwner(item: TDspBaseComponent);
begin
  if      Owner is TDspVSTModule then (Owner as TDspVSTModule).UnRegisterDSPItem(item)
  else if Owner is TDspVoice     then (Owner as TDspVoice    ).UnRegisterDSPItem(item);
end;

// abstract, but may be never overridden
procedure TDspBaseComponent.Init; begin end;
procedure TDspBaseComponent.Reset; begin end;
procedure TDspBaseComponent.UpdateParameters; begin end;
procedure TDspBaseComponent.NoteOff; begin end;
procedure TDspBaseComponent.IncProcessSampleCount(SampleCount, Channel: integer);
begin end;

procedure TDspBaseComponent.SampleRateChanged;
begin
  UpdateParameters;
end;

procedure TDspBaseComponent.ChannelsChanged;
begin
  UpdateParameters;
end;

procedure TDspBaseComponent.TrailingSamplesChanged;
begin
  UpdateParameters;
end;

procedure TDspBaseComponent.ResetQueue;
begin
  Reset;
  if assigned(FNextDspQueueItem) then FNextDspQueueItem.ResetQueue;
end;


function TDspBaseComponent.GetFollowingItems(var items: TDspQueueList): boolean;
var i: integer;
begin
  Result:=true;
  if not assigned(items) then
    items:=TDspQueueList.Create(false);

  if assigned(FNextDspQueueItem) then
  begin
    for i:=items.Count-1 downto 0 do
      if items[i]=FNextDspQueueItem then
      begin
        Result:=false;
        exit;
      end;

    items.Add(FNextDspQueueItem);
    Result:=Result and FNextDspQueueItem.GetFollowingItems(items);
  end;
end;

function TDspBaseComponent.GetPreviousItems(var items: TDspQueueList): boolean;
var i: integer;
begin
  Result:=true;
  if not assigned(items) then
    items:=TDspQueueList.Create(false);

  if assigned(FPrevDspQueueItem) then
  begin
    for i:=items.Count-1 downto 0 do
      if items[i]=FPrevDspQueueItem then
      begin
        Result:=false;
        exit;
      end;

    items.Insert(0, FPrevDspQueueItem);
    Result:=Result and FNextDspQueueItem.GetPreviousItems(items);
  end;
end;

function TDspBaseComponent.GetQueueItems(var items: TDspQueueList): boolean;
var mypos: integer;
begin
  result:=GetPreviousItems(items);
  if not result then exit;

  mypos:=items.Count;

  result:=result and GetFollowingItems(items);
  if not result then exit;
  items.Insert(mypos,self);
end;

procedure TDspBaseComponent.SetNextDspQueueItem(const Value: TDspBaseComponent);
var x: TDspQueueList; backup: TDspBaseComponent;
begin
  if (Value<>self) and (FNextDspQueueItem<>Value) then
  begin
    backup := FNextDspQueueItem;
    FNextDspQueueItem := Value;
    if Value<>nil then
    begin
      x:=nil;
      if (FPrevDspQueueItem=Value) or not (GetQueueItems(x)) then
      begin
        FNextDspQueueItem := backup;
        raise Exception.Create('Processing queue loopback');
        exit;
      end else begin
        if not assigned(FNextDspQueueItem.PrevDspQueueItem) then
          UnRegisterInOwner(FNextDspQueueItem);

        FNextDspQueueItem.PrevDspQueueItem:=self;
      end;
    end;

    // Important
    UpdateProcessingFunc;
  end;
end;

procedure TDspBaseComponent.SetSampleRate(const Value: Single);
begin
  if (fSampleRate<>Value) and (Value>0) then
  begin
    fSampleRate := Value;
    SampleRateChanged;
    
    if assigned(FNextDspQueueItem) then FNextDspQueueItem.SampleRate:=fSampleRate;
  end;
end;

procedure TDspBaseComponent.SetChannels(const Value: Integer);
begin
  if (fChannels<>Value) and (Value>0) then
  begin
    fChannels := Value;
    ChannelsChanged;

    if assigned(FNextDspQueueItem) then FNextDspQueueItem.Channels:=fChannels;
  end;
end;

procedure TDspBaseComponent.SetTrailingSamples(const Value: Integer);
begin
  if (fTrailingSamples<>Value) and (Value>=0) then
  begin
    fTrailingSamples := Value;
    TrailingSamplesChanged;

     if Owner is TDspVoice then
      (Owner as TDspVoice).UpdateTrailingSamples;
  end;
end;

procedure TDspBaseComponent.SetBypass(const Value: Boolean);
begin
  if fBypass <> Value then
  begin
    fBypass := Value;
    UpdateProcessingFunc;
  end;
end;

procedure TDspBaseComponent.SetEnabled(const Value: Boolean);
begin
  if fEnabled <> Value then
  begin
    fEnabled := Value;
    UpdateProcessingFunc;
  end;
end;

procedure TDspBaseComponent.UpdateProcessingFunc;
begin
  if fEnabled then
  begin
    if fBypass then
    begin
      // bypass everything
      fProcessS  := ProcessBypass;
      fProcessD  := ProcessBypass;
      fProcessSA := ProcessBypass;
      fProcessDA := ProcessBypass;
      fProcessSAA:= ProcessBypass;
      fProcessDAA:= ProcessBypass;

      if assigned(FNextDspQueueItem) then
      begin
        // ignore this item and call directly the next item
        fProcessQueueS  := ProcessQueueBypass;
        fProcessQueueD  := ProcessQueueBypass;
        fProcessQueueSA := ProcessQueueBypass;
        fProcessQueueDA := ProcessQueueBypass;
        fProcessQueueSAA:= ProcessQueueBypass;
        fProcessQueueDAA:= ProcessQueueBypass;
      end else begin
        // bypass this item and there is no next item, so don't call it
        fProcessQueueS  := ProcessBypass;
        fProcessQueueD  := ProcessBypass;
        fProcessQueueSA := ProcessBypass;
        fProcessQueueDA := ProcessBypass;
        fProcessQueueSAA:= ProcessBypass;
        fProcessQueueDAA:= ProcessBypass;
      end;
    end else begin
      // process item
      fProcessS  := FStdProcessS;
      fProcessD  := FStdProcessD;
      fProcessSA := FStdProcessSA;
      fProcessDA := FStdProcessDA;
      fProcessSAA:= FStdProcessSAA;
      fProcessDAA:= FStdProcessDAA;

      if assigned(FNextDspQueueItem) then
      begin
        // process this item and pass output to the next
        fProcessQueueS  := FStdProcessQueueS;
        fProcessQueueD  := FStdProcessQueueD;
        fProcessQueueSA := FStdProcessQueueSA;
        fProcessQueueDA := FStdProcessQueueDA;
        fProcessQueueSAA:= FStdProcessQueueSAA;
        fProcessQueueDAA:= FStdProcessQueueDAA;
      end else begin
        // only process this item
        fProcessQueueS  := FStdProcessS;
        fProcessQueueD  := FStdProcessD;
        fProcessQueueSA := FStdProcessSA;
        fProcessQueueDA := FStdProcessDA;
        fProcessQueueSAA:= FStdProcessSAA;
        fProcessQueueDAA:= FStdProcessDAA;
      end;
    end;
  end else begin
    // disable everything
    fProcessS  := ProcessSilence;
    fProcessD  := ProcessSilence;
    fProcessSA := ProcessSilence;
    fProcessDA := ProcessSilence;
    fProcessSAA:= ProcessSilence;
    fProcessDAA:= ProcessSilence;

    // disable this item and don't process the next one
    fProcessQueueS  := ProcessSilence;
    fProcessQueueD  := ProcessSilence;
    fProcessQueueSA := ProcessSilence;
    fProcessQueueDA := ProcessSilence;
    fProcessQueueSAA:= ProcessSilence;
    fProcessQueueDAA:= ProcessSilence;
  end;  
end;

procedure TDspBaseComponent.ProcessSilence(var Data: Single; const channel: integer);
begin
  Data := 0;
end;

procedure TDspBaseComponent.ProcessSilence(var Data: Double; const channel: integer);
begin
  Data := 0;
end;

procedure TDspBaseComponent.ProcessSilence(var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer);
begin
 FillChar(ProcessBuffer[0], SampleFrames * SizeOf(Single), 0);
end;

procedure TDspBaseComponent.ProcessSilence(var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer);
begin
 FillChar(ProcessBuffer[0], SampleFrames * SizeOf(Double), 0);
end;

procedure TDspBaseComponent.ProcessSilence(var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer);
begin
  ClearArrays(ProcessBuffer, fChannels, SampleFrames);
end;

procedure TDspBaseComponent.ProcessSilence(var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer);
begin
  ClearArrays(ProcessBuffer, fChannels, SampleFrames);
end;





procedure TDspBaseComponent.ProcessBypass(var Data: Single; const channel: integer);
begin
  // Do nothing with the data
  IncProcessSampleCount(1, channel);
end;

procedure TDspBaseComponent.ProcessBypass(var Data: Double; const channel: integer);
begin
 // Do nothing with the data
 IncProcessSampleCount(1, channel);
end;

procedure TDspBaseComponent.ProcessBypass(var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer);
begin
 // Do nothing with the buffer
 IncProcessSampleCount(SampleFrames, channel);
end;

procedure TDspBaseComponent.ProcessBypass(var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer);
begin
 // Do nothing with the buffer
 IncProcessSampleCount(SampleFrames, channel);
end;

procedure TDspBaseComponent.ProcessBypass(var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer);
begin
 // Do nothing with the buffer
 IncProcessSampleCount(SampleFrames);
end;

procedure TDspBaseComponent.ProcessBypass(var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer);
begin
 // Do nothing with the buffer 
 IncProcessSampleCount(SampleFrames);
end;






procedure TDspBaseComponent.ProcessBasic(var Data: Double; const channel: integer);
var tmp: single;
begin
  tmp := Data;
  fProcessS(tmp, channel);
  Data := tmp;
end;

procedure TDspBaseComponent.ProcessBasic(var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer);
var i: integer;
begin
 for i := 0 to SampleFrames - 1
  do fProcessS(ProcessBuffer[i], channel);
end;

procedure TDspBaseComponent.ProcessBasic(var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer);
var i: integer;
begin
 for i := 0 to SampleFrames - 1
  do fProcessD(ProcessBuffer[i], channel);
end;

procedure TDspBaseComponent.ProcessBasic(var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer);
var i: integer;
begin
 for i := 0 to fChannels - 1
  do fProcessSA(ProcessBuffer[i], i, SampleFrames);
end;

procedure TDspBaseComponent.ProcessBasic(var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer);
var i: integer;
begin
 for i := 0 to fChannels - 1
  do fProcessDA(ProcessBuffer[i], i, SampleFrames);
end;




procedure TDspBaseComponent.ProcessQueueBasic(var Data: Single; const channel: integer);
begin
 fProcessS(Data, channel);
 FNextDspQueueItem.ProcessQueueS(Data, channel);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var Data: Double; const channel: integer);
begin
 fProcessD(Data, channel);
 FNextDspQueueItem.ProcessQueueD(Data, channel);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer);
begin
 fProcessSA(ProcessBuffer, channel, SampleFrames);
 FNextDspQueueItem.ProcessQueueSA(ProcessBuffer, channel, SampleFrames);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer);
begin
 fProcessDA(ProcessBuffer, channel, SampleFrames);
 FNextDspQueueItem.ProcessQueueDA(ProcessBuffer, channel, SampleFrames);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer);
begin
 fProcessSAA(ProcessBuffer, SampleFrames);
 FNextDspQueueItem.ProcessQueueSAA(ProcessBuffer, SampleFrames);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer);
begin
 fProcessDAA(ProcessBuffer, SampleFrames);
 FNextDspQueueItem.ProcessQueueDAA(ProcessBuffer, SampleFrames);
end;




procedure TDspBaseComponent.ProcessQueueBypass(var Data: Single; const channel: integer);
begin
 FNextDspQueueItem.ProcessQueueS(Data, channel);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var Data: Double; const channel: integer);
begin
 FNextDspQueueItem.ProcessQueueD(Data, channel);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var ProcessBuffer: TDAVSingleDynArray; const channel, SampleFrames: integer);
begin
 FNextDspQueueItem.ProcessQueueSA(ProcessBuffer, channel, SampleFrames);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var ProcessBuffer: TDAVDoubleDynArray; const channel, SampleFrames: integer);
begin
 FNextDspQueueItem.ProcessQueueDA(ProcessBuffer, channel, SampleFrames);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var ProcessBuffer: TDAVArrayOfSingleDynArray; const SampleFrames: integer);
begin
 FNextDspQueueItem.ProcessQueueSAA(ProcessBuffer, SampleFrames);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var ProcessBuffer: TDAVArrayOfDoubleDynArray; const SampleFrames: integer);
begin
 FNextDspQueueItem.ProcessQueueDAA(ProcessBuffer, SampleFrames);
end;

procedure TDspBaseComponent.ProcessMidiEvent(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean);
begin end;

procedure TDspBaseComponent.ProcessMidiEventQueue(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean);
begin
  FilterEvent:=false;
  ProcessMidiEvent(MidiEvent, FilterEvent);

  if not FilterEvent and assigned(FNextDspQueueItem) then
    FNextDspQueueItem.ProcessMidiEventQueue(MidiEvent, FilterEvent);
end;

function TDspBaseComponent.GetTrailingSamplesQueue: integer;
begin
  result:=fTrailingSamples;
  if assigned(FNextDspQueueItem) then result:=max(result, FNextDspQueueItem.TrailingSamplesQueue);
end;

procedure TDspBaseComponent.NoteOffQueue;
begin
  NoteOff;
  if assigned(FNextDspQueueItem) then FNextDspQueueItem.NoteOffQueue;
end;

end.
