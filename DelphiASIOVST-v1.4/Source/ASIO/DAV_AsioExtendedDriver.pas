unit DAV_ASIOExtendedDriver;

interface

uses Classes, messages, windows, forms, DAV_ASIO, DAV_ASIODriver;

type
  TDavASIOExtendedDriver = class;

  TDavASIOExtDrvrDoubleBuffer = array [0..1] of Pointer;

  TDavASIOExtDrvrTimings = record
    SystemTime: TASIOTimeStamp;
    SamplePos: TASIOSamples;
  end;

  TDavASIOExtDrvrSupported = record
    RequestsDone,      // only true, when all requests to the host are made
    EngineVersion,
    ResetRequest,
    BufferSizeChange,
    ResyncRequest,
    LatenciesChanged,
    SupportsTimeInfo,
    SupportsTimeCode: Boolean;
  end;

  {$IFDEF DELPHI10_UP} {$region 'Processing thread declaration'} {$ENDIF}
  TDavASIOExtDrvrProcessingThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Driver: TDavASIOExtendedDriver;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'Processing thread declaration'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Buffersizes declaration'} {$ENDIF}
  TDavASIOExtDrvrBufferSizes = record
    Minimum,
    Maximum,
    Prefered,
    Granularity,
    Current: LongInt;
  end;  
  {$IFDEF DELPHI10_UP} {$endregion 'Buffersizes declaration'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Clock list declaration'} {$ENDIF}
  TDavASIOExtDrvrClockListItem = class
    ClockName: string;
    ChannelGroup: LongInt;
    IsCurrentSource: boolean;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'Clock list declaration'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Channel list declaration'} {$ENDIF}
  TDavASIOExtDrvrChannelListItem = class
    IsActive: boolean;
    ChannelGroup: LongInt;
    SampleType: TASIOSampleType;
    ChannelName: string;
    DoubleBuffer: TDavASIOExtDrvrDoubleBuffer;
    constructor Create(cname: string; cchannelgroup: LongInt; cSampleType: TASIOSampleType; cIsInput: Boolean);
    destructor Destroy; override;
    function CreateBuffers(out Buffers: array of Pointer; size: Integer): boolean;
    procedure DisposeBuffers;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'Channel list declaration'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Samplerate list declaration'} {$ENDIF}
 TDavASIOExtDrvrSampleRateItem = class
    SampleRate: Double;
  end;


  TTDavASIOExtDrvrSampleRateMode = (edsrm_Single, // Only one sample rate
                                    edsrm_Range,  // A range of sample rates eg. 11025..44100
                                    edsrm_List,   // A list of sample rates eg. 11025,22050,44100
                                    edsrm_All);   // Sample rate doesn't matter, everything is accepted

  TTDavASIOExtDrvrSampleRateManager = class
  private
    fSampleRateMode: TTDavASIOExtDrvrSampleRateMode;
    fSampleRateList: TList;
    fDefaultSampleRate: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSampleRate(sr: double);
    procedure SetDefaultSampleRate(sr: double);
    procedure SetSampleRateMode(md: TTDavASIOExtDrvrSampleRateMode);
    function CanSampleRate(sr: double): boolean;
    function GetDefaultSampleRate: Double;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'Samplerate list declaration'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Extended driver declaration'} {$ENDIF}
  TDavASIOExtendedDriver = class(TDavASIODriver)
  protected
    fDriverName: string;
    fDriverVersion: LongInt;
    fLastErrorMsg: string;
    fSampleRateManager: TTDavASIOExtDrvrSampleRateManager;
    fClockList: TList;
    fInChannelList: TList;
    fOutChannelList: TList;
    fBufferSize: TDavASIOExtDrvrBufferSizes;
    fClocksAreDefault: boolean;
    fChannelsAreDefault: boolean;
    fSampleRate: Double;
    fInputLatency: LongInt;
    fOutputLatency: LongInt;
    fHostCallbacks: PASIOCallbacks;
    fBuffersCreated: boolean;
    fSupportedSelectors: TDavASIOExtDrvrSupported;
    fHostEngineVersion: LongInt;
    fSupportsTimeInfo: boolean;
    fSupportsTimeCode: boolean;
    fAsioTime: TASIOTime;
    fSwitchTimings: TDavASIOExtDrvrTimings;
    fIsProcessing: boolean;
    fProcessingThread: TDavASIOExtDrvrProcessingThread;
    fCurrentBuffer: LongInt;

    procedure InitializeDriverParams; virtual;
    procedure SetDriverName(name: string);
    procedure SetDriverVersion(version: LongInt);
    procedure SetErrorMessage(s: string);
    procedure AddClock(name: string; channelgroup: LongInt);
    procedure AddChannel(name: string; channelgroup: LongInt; SampleType: TASIOSampleType; IsInput: Boolean);
    procedure ChangeClockSource(fromIndex, ToIndex: LongInt); virtual;
    function GetCurrentClockSource: Integer;
    procedure AddSampleRate(sr: double);
    procedure SetSampleRateMode(md: TTDavASIOExtDrvrSampleRateMode);
    function CheckBufferSize(test: Integer): boolean;

    procedure ClearClockList;
    procedure ClearChannelLists;
    function GetFirstGroupChannel(GroupNr: Longint; IsInput: Boolean): Longint;

    procedure ClearSupportedSelectors; virtual;
    procedure CheckSupportedSelectors; virtual;
    procedure InitializeTimeInfo; virtual;

    function GetTimecodeSamples: TASIOSamples; virtual;
    procedure UpdateTimings; virtual;
    procedure StartProcessingThread; virtual;
    procedure StopProcessingThread; virtual;

    procedure ProcessBuffers; virtual;

    procedure LoadDriverSettings; virtual;
    procedure SaveDriverSettings; virtual;
  public
    constructor Create(TCWrapper: TDavASIOTCWrapper; InterfaceGUID: TGuid); override;
    destructor Destroy; override;

    function GetClockSources(Clocks: PASIOClockSources; out NumSources: LongInt): TASIOError; override;
    function SetClockSource(Reference: LongInt): TASIOError; override;
    function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; override;
    procedure SetBufferSizes(MinSize, MaxSize, PreferredSize, Granularity: LongInt); virtual;
    procedure SetLatencies(InLatency, OutLatency: LongInt); virtual;
    procedure GetNanoSeconds(var Time: TASIOTimeStamp);


    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; override;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; override;
    function GetDriverName: string; override;
    function GetDriverVersion: LongInt; override;
    function GetErrorMessage: string; override;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; override;
    function CreateBuffers(BufferInfos: PASIOBufferInfos; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; override;
    function DisposeBuffers: TASIOError; override; 
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; override;
    function GetSampleRate(out nSampleRate: TASIOSampleRate): TASIOError; override;
    function SetSampleRate(nSampleRate: TASIOSampleRate): TASIOError; override;
    function GetLatencies(out InLatency, OutLatency: LongInt): TASIOError; override;
    function Start: TASIOError; override;
    function Stop: TASIOError; override;

    procedure ASIOBufferSwitch(DoubleBufferIndex: Integer; DirectProcess: TASIOBool); virtual;
    procedure ASIOBufferSwitchTimeInfo(DoubleBufferIndex: Integer; DirectProcess: TASIOBool); virtual;
    procedure ASIOSampleRateDidChange(SampleRate: TASIOSampleRate); virtual;
    function ASIOMessage(Selector, Value: Integer; msg: Pointer; Opt: PDouble): Integer; virtual;
    procedure ASIORequestReset;
    procedure ASIOResyncRequest;
    procedure ASIOBufferSizeChange(newsize: LongInt);
    procedure ASIOLatenciesChanged;

    property SampleRate: Double read fSampleRate;
    property BufferSize: TDavASIOExtDrvrBufferSizes read fBufferSize;
    property HostEngineVersion: Longint read fHostEngineVersion;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'Extended driver declaration'} {$ENDIF}

implementation

uses SysUtils, Math, MMSystem;

{ TDavASIOExtDrvrProcessingThread }

{$IFDEF DELPHI10_UP} {$region 'Processing thread implementation'} {$ENDIF}

procedure TDavASIOExtDrvrProcessingThread.Execute;
begin
  while not Terminated do
    if assigned(Driver) then
      Driver.ProcessBuffers
    else Terminate;
end;

{$IFDEF DELPHI10_UP} {$endregion 'Processing thread implementation'} {$ENDIF}


{ TDavASIOExtDrvrChannelListItem }

{$IFDEF DELPHI10_UP} {$region 'Channel list implementation'} {$ENDIF}

constructor TDavASIOExtDrvrChannelListItem.Create(cname: string; cchannelgroup: Integer; cSampleType: TASIOSampleType; cIsInput: Boolean);
begin
  ChannelName := copy(cname,0,32);
  ChannelGroup := cchannelgroup;
  SampleType := cSampleType;
  IsActive := false;
  DoubleBuffer[0] := nil;
  DoubleBuffer[1] := nil;
end;

destructor TDavASIOExtDrvrChannelListItem.Destroy;
begin
  DisposeBuffers;
  inherited;
end;

function TDavASIOExtDrvrChannelListItem.CreateBuffers(out Buffers: array of pointer; size: Integer): boolean;
var samplesize: integer;
begin
  if IsActive then DisposeBuffers;

  samplesize := 4;
  case SampleType of
    ASIOSTDSDInt8LSB1,
    ASIOSTDSDInt8MSB1, ASIOSTDSDInt8NER8: samplesize := 1;
    ASIOSTInt16MSB,    ASIOSTInt16LSB:    samplesize := 2;
    ASIOSTInt24MSB,    ASIOSTInt24LSB:    samplesize := 3;
    ASIOSTFloat64MSB,  ASIOSTFloat64LSB:  samplesize := 8;
  end;                                                      

  GetMem(DoubleBuffer[0], size * samplesize);
  GetMem(DoubleBuffer[1], size * samplesize);

  if not assigned(DoubleBuffer[0]) or not assigned(DoubleBuffer[1]) then
  begin
    IsActive := false;
    if assigned(DoubleBuffer[0]) then FreeMem(DoubleBuffer[0]);
    if assigned(DoubleBuffer[1]) then FreeMem(DoubleBuffer[1]);
  end else begin
    IsActive := true;
    Buffers[0] := DoubleBuffer[0];
    Buffers[1] := DoubleBuffer[1];
  end;

  result := IsActive;
end;

procedure TDavASIOExtDrvrChannelListItem.DisposeBuffers;
begin
  if not IsActive then exit;
  IsActive := false;

  if assigned(DoubleBuffer[0]) then FreeMem(DoubleBuffer[0]);
  if assigned(DoubleBuffer[1]) then FreeMem(DoubleBuffer[1]);
end;

{$IFDEF DELPHI10_UP} {$endregion 'Channel list implementation'} {$ENDIF}


{ TTDavASIOExtDrvrSampleRateManager }

{$IFDEF DELPHI10_UP} {$region 'Samplerate list implementation'} {$ENDIF}

constructor TTDavASIOExtDrvrSampleRateManager.Create;
begin
  fSampleRateList := TList.Create;
  fSampleRateList.Clear;
  fSampleRateMode := edsrm_All;
  fDefaultSampleRate := -1;
end;

destructor TTDavASIOExtDrvrSampleRateManager.Destroy;
var i: integer;
begin
  for i := fSampleRateList.count-1 downto 0 do
    TDavASIOExtDrvrSampleRateItem(fSampleRateList.Items[i]).Free;

  fSampleRateList.Clear;
  inherited;
end;

procedure TTDavASIOExtDrvrSampleRateManager.AddSampleRate(sr: double);
var t: TDavASIOExtDrvrSampleRateItem;
begin
  t:=TDavASIOExtDrvrSampleRateItem.Create;
  t.SampleRate:=sr;
  fSampleRateList.Add(t);
end;

function TTDavASIOExtDrvrSampleRateManager.CanSampleRate(sr: double): boolean;
var i: integer;
begin
  case fSampleRateMode of
    edsrm_Single:
      begin
        if fSampleRateList.Count<1 then
          result := false
        else
          result := TDavASIOExtDrvrSampleRateItem(fSampleRateList.Items[0]).SampleRate = sr;

        exit;
      end;

    edsrm_Range:
      begin
         if fSampleRateList.Count<2 then
          result := false
        else
          result := (TDavASIOExtDrvrSampleRateItem(fSampleRateList.Items[0]).SampleRate <= sr) and (TDavASIOExtDrvrSampleRateItem(fSampleRateList.Items[1]).SampleRate >= sr)
                 or (TDavASIOExtDrvrSampleRateItem(fSampleRateList.Items[0]).SampleRate >= sr) and (TDavASIOExtDrvrSampleRateItem(fSampleRateList.Items[1]).SampleRate <= sr);

        exit;
      end;

    edsrm_List:
      begin
        result := false;

        if fSampleRateList.Count>0 then
          for i := 0 to fSampleRateList.Count-1 do
            if TDavASIOExtDrvrSampleRateItem(fSampleRateList.Items[i]).SampleRate=sr then
            begin
              result:=true;
              break;
            end;

        exit;
      end;
  end;

  // ALL
  result:=true;
end;

procedure TTDavASIOExtDrvrSampleRateManager.SetDefaultSampleRate(sr: double);
begin
  fDefaultSampleRate := sr;
end;

function TTDavASIOExtDrvrSampleRateManager.GetDefaultSampleRate: Double;
begin
  if fDefaultSampleRate>0 then result := fDefaultSampleRate
  else begin
    result := 44100;
    if CanSampleRate(result) or (fSampleRateList.Count<1) then exit;

    result := TDavASIOExtDrvrSampleRateItem(fSampleRateList.Items[0]).SampleRate;
  end;
end;

procedure TTDavASIOExtDrvrSampleRateManager.SetSampleRateMode(md: TTDavASIOExtDrvrSampleRateMode);
begin
  fSampleRateMode := md;
end;

{$IFDEF DELPHI10_UP} {$endregion 'Samplerate list implementation'} {$ENDIF}


{ TDavASIOExtendedDriver }

{$IFDEF DELPHI10_UP} {$region 'Extended driver implemention'} {$ENDIF}

constructor TDavASIOExtendedDriver.Create(TCWrapper: TDavASIOTCWrapper; InterfaceGUID: TGuid);
begin
  inherited;
  fClockList:=TList.Create;
  fInChannelList     := TList.Create;
  fOutChannelList    := TList.Create;
  fProcessingThread  := nil;
  fHostCallbacks     := nil;
  fDriverName        := 'DAV Abstract Ext';
  fDriverVersion     := 1;
  fHostEngineVersion := 1;
  fSupportsTimeInfo  := false;
  fSupportsTimeCode  := false;
  fBuffersCreated    := false;
  fInputLatency      := 0;
  fOutputLatency     := 0;
  fCurrentBuffer     := 0;
  fIsProcessing      := false;

  with fSwitchTimings do
  begin
    SystemTime.Lo := 0;
    SystemTime.Hi := 0;
    SamplePos.Lo  := 0;
    SamplePos.Hi  := 0;
  end;

  with fBufferSize do
  begin
    Minimum     := 64;
    Maximum     := 4096;
    Prefered    := 512;
    Granularity := -1;
    Current     := 512;
  end;

  ClearSupportedSelectors;

  fLastErrorMsg := '';
  fSampleRateManager := TTDavASIOExtDrvrSampleRateManager.Create;

  fClocksAreDefault:=false;
  fChannelsAreDefault:=false;

  AddClock('Default Clock', 0);
  AddChannel('Default Input', 0, ASIOSTFloat32LSB, true);
  AddChannel('Default Output', 0, ASIOSTFloat32LSB, false);

  fClocksAreDefault:=true;
  fChannelsAreDefault:=true;

  InitializeDriverParams;
  LoadDriverSettings;

  fSampleRate := fSampleRateManager.GetDefaultSampleRate;
  
  FillChar(fAsioTime.Reserved, 4, 0);
  InitializeTimeInfo;

  InitControlPanel;
end;

destructor TDavASIOExtendedDriver.destroy;
begin
  Stop;
  DisposeBuffers;

  FreeAndNil(fSampleRateManager);

  ClearClockList;
  FreeAndNil(fClockList);

  ClearChannelLists;
  FreeAndNil(fInChannelList);
  FreeAndNil(fOutChannelList);

  inherited;
end;

procedure TDavASIOExtendedDriver.LoadDriverSettings;
begin
  // this is default: does nothing
end;

procedure TDavASIOExtendedDriver.SaveDriverSettings;
begin
  // this is default: does nothing
end;

procedure TDavASIOExtendedDriver.ClearClockList;
var i:integer;
begin
  for i := fClockList.Count-1 downto 0 do TDavASIOExtDrvrClockListItem(fClockList.Items[i]).Free;
  fClockList.Clear;
end;

procedure TDavASIOExtendedDriver.ClearChannelLists;
var i:integer;
begin
  for i := fInChannelList.Count-1 downto 0 do TDavASIOExtDrvrChannelListItem(fInChannelList.Items[i]).Free;
  for i := fOutChannelList.Count-1 downto 0 do TDavASIOExtDrvrChannelListItem(fOutChannelList.Items[i]).Free;
  fInChannelList.Clear;
  fOutChannelList.Clear;
end;

procedure TDavASIOExtendedDriver.InitializeDriverParams;
begin
  raise Exception.Create('You have to overwrite InitializeDriverParams');
end;

procedure TDavASIOExtendedDriver.SetDriverName(name: string);
begin
  fDriverName := name;
end;

procedure TDavASIOExtendedDriver.SetDriverVersion(version: LongInt);
begin
  fDriverVersion := version;
end;

procedure TDavASIOExtendedDriver.SetErrorMessage(s: string);
begin
  fLastErrorMsg := s;
end;

procedure TDavASIOExtendedDriver.AddClock(name: string; channelgroup: Integer);
var t: TDavASIOExtDrvrClockListItem;
begin
  if fClocksAreDefault then
  begin
    ClearClockList;
    fClocksAreDefault := false;
  end;

  t := TDavASIOExtDrvrClockListItem.Create;
  t.ClockName := copy(name,0,32);
  t.ChannelGroup := channelgroup;
  t.IsCurrentSource := false;
  fClockList.Add(t);
end;

procedure TDavASIOExtendedDriver.AddChannel(name: string; channelgroup: Integer; SampleType: TASIOSampleType; IsInput: Boolean);
var t: TDavASIOExtDrvrChannelListItem;
begin
  if fChannelsAreDefault then
  begin
    ClearChannelLists;
    fChannelsAreDefault := false;
  end;

  t := TDavASIOExtDrvrChannelListItem.Create(name, channelgroup, SampleType, IsInput);

  if IsInput then
    fInChannelList.Add(t)
  else
    fOutChannelList.Add(t);
end;  

function TDavASIOExtendedDriver.GetCurrentClockSource: Integer;
var i: integer;
begin
  result:=-1;
  if fClockList.Count<1 then exit;

  for i:=0 to fClockList.Count-1 do
    with TDavASIOExtDrvrClockListItem(fClockList.Items[i]) do
    begin
      if IsCurrentSource then
      begin
        result:=i;
        break;
      end;
    end;
end;

procedure TDavASIOExtendedDriver.AddSampleRate(sr: double);
begin
  fSampleRateManager.AddSampleRate(sr);
end;

procedure TDavASIOExtendedDriver.SetSampleRateMode(md: TTDavASIOExtDrvrSampleRateMode);
begin
  fSampleRateManager.SetSampleRateMode(md);
end;

procedure TDavASIOExtendedDriver.SetBufferSizes(MinSize, MaxSize, PreferredSize, Granularity: Integer);
begin
  if PreferredSize<MinSize then MinSize := PreferredSize;
  if PreferredSize>MaxSize then MaxSize := PreferredSize;
  if MinSize=MaxSize then Granularity := 0;

  fBufferSize.Minimum     := MinSize;
  fBufferSize.Maximum     := MaxSize;
  fBufferSize.Prefered    := PreferredSize;
  fBufferSize.Granularity := Granularity;

  if not CheckBufferSize(fBufferSize.Current) then ASIOBufferSizeChange(PreferredSize);
end;

procedure TDavASIOExtendedDriver.SetLatencies(InLatency, OutLatency: LongInt);
begin
  if (fInputLatency=InLatency) and (fOutputLatency=OutLatency) then exit;

  fInputLatency := InLatency;
  fOutputLatency := OutLatency;
  ASIOLatenciesChanged;
end;

function TDavASIOExtendedDriver.GetFirstGroupChannel(GroupNr: Longint; IsInput: Boolean): Longint;
var querylist: TList;
    i: integer;
begin
  result := -1;
  if IsInput then
    querylist := fInChannelList
  else
    querylist := fOutChannelList;

  if querylist.Count>0 then
    for i := 0 to querylist.Count-1 do
      with TDavASIOExtDrvrChannelListItem(querylist.Items[i]) do
        if ChannelGroup=GroupNr then
        begin
          result := i;
          break;
        end;
end;

function TDavASIOExtendedDriver.GetClockSources(Clocks: PASIOClockSources; out NumSources: LongInt): TASIOError;
var i: integer;
begin
  Result := ASE_NotPresent;

  NumSources := min(NumSources,fClockList.Count);
  if NumSources>0 then
  begin
    for i := 0 to fClockList.Count-1 do
      with TDavASIOExtDrvrClockListItem(fClockList.Items[i]) do
      begin
        Clocks^[i].Index := i;
        Clocks^[i].AssociatedChannel := GetFirstGroupChannel(ChannelGroup,true);
        Clocks^[i].AssociatedGroup := ChannelGroup ;
        Clocks^[i].IsCurrentSource := TASIOBool(IsCurrentSource);
        StrCopy(Clocks^[i].Name, PChar(ClockName));
      end;

    result := ASE_OK;
  end;
end;

function TDavASIOExtendedDriver.SetClockSource(Reference: Integer): TASIOError;
var last: integer;
begin
  result := ASE_OK;

  if (Reference>=fClockList.Count) or (Reference<0) then
  begin
    result := ASE_InvalidParameter;
    exit;
  end;

  last:=GetCurrentClockSource;
  if Reference=last then exit;

  ChangeClockSource(last,Reference);

  if fSupportsTimeInfo then
  begin
    fAsioTime.TimeInfo.Flags := fAsioTime.TimeInfo.Flags or kClockSourceChanged;
  end;
end;

procedure TDavASIOExtendedDriver.ChangeClockSource(fromIndex, ToIndex: LongInt);
begin
  // you can override this and fill the sample rate list here, to have clock dependent samplerates
  if fromIndex>=0 then TDavASIOExtDrvrClockListItem(fClockList.Items[fromIndex]).IsCurrentSource := false;
  TDavASIOExtDrvrClockListItem(fClockList.Items[ToIndex]).IsCurrentSource := true;
end;

function TDavASIOExtendedDriver.GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
begin
  TimeStamp := fSwitchTimings.SystemTime;
  SamplePosition := fSwitchTimings.SamplePos;
  Result := ASE_OK;
end;

function TDavASIOExtendedDriver.GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError;
begin
  NumInputChannels:=fInChannelList.Count;
  NumOutputChannels:=fOutChannelList.Count;

  if NumInputChannels+NumOutputChannels<1 then
    result := ASE_NotPresent
  else
    result := ASE_OK;
end;

function TDavASIOExtendedDriver.GetChannelInfo(var Info: TASIOChannelInfo): TASIOError;
var querylist: TList;
begin
  if Info.IsInput=ASIOTrue then
    querylist := fInChannelList
  else
    querylist := fOutChannelList;

  if Info.Channel>=querylist.Count then
    result := ASE_InvalidParameter
  else with TDavASIOExtDrvrChannelListItem(querylist.Items[Info.Channel]) do begin
    Info.SampleType := SampleType;
    Info.ChannelGroup := ChannelGroup;
    Info.IsActive := TASIOBool(IsActive);
    StrPCopy(Info.Name, Pchar(ChannelName));
    result := ASE_OK;
  end;
end;

function TDavASIOExtendedDriver.GetDriverName: string;
begin
  result := fDriverName;
end;

function TDavASIOExtendedDriver.GetDriverVersion: LongInt;
begin
  result := fDriverVersion;
end;

function TDavASIOExtendedDriver.GetErrorMessage: string;
begin
  result := fLastErrorMsg;
end;

function TDavASIOExtendedDriver.CanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
  if fSampleRateManager.CanSampleRate(SampleRate) then
    result := ASE_OK
  else
    result := ASE_NoClock;
end;

function TDavASIOExtendedDriver.GetSampleRate(out nSampleRate: TASIOSampleRate): TASIOError;
begin
  nSampleRate := fSampleRate;
  result := ASE_OK;
end;

function TDavASIOExtendedDriver.SetSampleRate(nSampleRate: TASIOSampleRate): TASIOError;
begin
  if fSampleRateManager.CanSampleRate(nSampleRate) then
  begin
    if fSampleRate <> nSampleRate then
    begin
      fSampleRate := nSampleRate;
      if fSupportsTimeInfo then
      begin
        fAsioTime.TimeInfo.SampleRate := fSampleRate;
        fAsioTime.TimeInfo.Flags := fAsioTime.TimeInfo.Flags or kSampleRateChanged;
      end;
      ASIOSampleRateDidChange(fSampleRate);
    end;
    result := ASE_OK;
  end else
    result := ASE_NoClock;
end;

function TDavASIOExtendedDriver.GetLatencies(out InLatency, OutLatency: LongInt): TASIOError;
begin
  InLatency := fInputLatency;
  OutLatency := fOutputLatency;

  result := ASE_OK;
end;

function TDavASIOExtendedDriver.GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: Integer): TASIOError;
begin
  MinSize       := fBufferSize.Minimum;
  MaxSize       := fBufferSize.Maximum;
  PreferredSize := fBufferSize.Prefered;
  Granularity   := fBufferSize.Granularity;

  Result := ASE_OK;
end;

function TDavASIOExtendedDriver.CheckBufferSize(test: Integer): boolean;
var tmp: LongInt;
begin
  with fBufferSize do
  begin
    if (test=Minimum) or (test=Maximum) or (test=Prefered) then
    begin
      // in this case it should be valid
      result := true;
      exit;
    end;

    if Granularity=0 then
    begin
      // no chance
      result:=false;
      exit;
    end;

    result := false;
    if (test>=Minimum) and (test<=Maximum) then
    begin
      if Granularity>0 then
      begin
        result:=(test-Minimum) mod Granularity = 0;
      end else begin
        if Minimum>0 then begin
          tmp := test div Minimum;
          result := ( (test mod Minimum) = 0) and (tmp and (tmp-1) = 0);
        end else if Maximum>0 then begin
          tmp := Maximum div test;
          result := ( (Maximum mod test) = 0) and (tmp and (tmp-1) = 0);
        end;
      end;
    end;
  end;
end;


procedure TDavASIOExtendedDriver.ClearSupportedSelectors;
begin
  with fSupportedSelectors do
  begin
    RequestsDone      := false;
    EngineVersion     := false;
    ResetRequest      := false;
    BufferSizeChange  := false;
    ResyncRequest     := false;
    LatenciesChanged  := false;
    SupportsTimeInfo  := false;
    SupportsTimeCode  := false;
  end;
end;

procedure TDavASIOExtendedDriver.CheckSupportedSelectors;
begin
  with fSupportedSelectors do
  begin
    EngineVersion     := ASIOMessage(kAsioSelectorSupported,kAsioEngineVersion,    nil, nil) > 0;
    ResetRequest      := ASIOMessage(kAsioSelectorSupported,kAsioResetRequest,     nil, nil) > 0;
    BufferSizeChange  := ASIOMessage(kAsioSelectorSupported,kAsioBufferSizeChange, nil, nil) > 0;
    ResyncRequest     := ASIOMessage(kAsioSelectorSupported,kAsioResyncRequest,    nil, nil) > 0;
    LatenciesChanged  := ASIOMessage(kAsioSelectorSupported,kAsioLatenciesChanged, nil, nil) > 0;
    SupportsTimeInfo  := ASIOMessage(kAsioSelectorSupported,kAsioSupportsTimeInfo, nil, nil) > 0;
    SupportsTimeCode  := ASIOMessage(kAsioSelectorSupported,kAsioSupportsTimeCode, nil, nil) > 0;

    RequestsDone := true;
  end;
end;

procedure TDavASIOExtendedDriver.InitializeTimeInfo;
begin
  with fAsioTime.TimeInfo do
  begin
    Speed := 1;
    SystemTime.Lo := 0;
    SystemTime.Hi := 0;
    SamplePosition.Lo := 0;
    SamplePosition.Hi := 0;
    SampleRate := fSampleRate;
    Flags := kSystemTimeValid or kSamplePositionValid or kSampleRateValid;
  end;

  with fAsioTime.TimeCode do
  begin
    Speed := 1;
    TimeCodeSamples.Lo := 0;
    TimeCodeSamples.Hi := 0;
    Flags := kTcValid or kTcRunning;
  end;
end;

function TDavASIOExtendedDriver.CreateBuffers(BufferInfos: PASIOBufferInfos; NumChannels, BufferSize: Integer; const Callbacks: TASIOCallbacks): TASIOError;
var i: integer;
    querylist: TList;
begin
  ClearSupportedSelectors;

  fSupportsTimeInfo := false;
  fSupportsTimeCode := false;

  DisposeBuffers; // clear the previous buffers
  result := ASE_OK;

  if not CheckBufferSize(BufferSize) then
    result := ASE_InvalidMode
  else begin
    fBufferSize.Current := BufferSize;

    for i := 0 to NumChannels-1 do with BufferInfos^[i] do
    begin
      if IsInput=ASIOTrue then
        querylist := fInChannelList
      else
        querylist := fOutChannelList;

      if (ChannelNum<0) or (ChannelNum>=querylist.Count) then
      begin
        result := ASE_InvalidParameter;
        break;
      end;

      if not TDavASIOExtDrvrChannelListItem(querylist.Items[ChannelNum]).CreateBuffers(Buffers, fBufferSize.Current) then
      begin
        result := ASE_NoMemory;
        break;
      end;
    end;
  end;

  if result<>ASE_OK then
  begin
    DisposeBuffers;
    exit;
  end;

  fBuffersCreated := true;
  fHostCallbacks := @Callbacks;
  CheckSupportedSelectors;

  if fSupportedSelectors.SupportsTimeInfo then fSupportsTimeInfo  := ASIOMessage(kAsioSupportsTimeInfo,0,nil, nil) > 0;
  if fSupportedSelectors.SupportsTimeCode then fSupportsTimeCode  := ASIOMessage(kAsioSupportsTimeCode,0,nil, nil) > 0;
  if fSupportedSelectors.EngineVersion    then fHostEngineVersion := ASIOMessage(kAsioEngineVersion,0,nil, nil);

  if fSupportsTimeInfo then InitializeTimeInfo;  
end;

function TDavASIOExtendedDriver.DisposeBuffers: TASIOError;
var i: integer;
begin
  for i := 0 to fInChannelList.Count-1 do TDavASIOExtDrvrChannelListItem(fInChannelList.Items[i]).DisposeBuffers;
  for i := 0 to fOutChannelList.Count-1 do TDavASIOExtDrvrChannelListItem(fOutChannelList.Items[i]).DisposeBuffers;

  fBuffersCreated := false;
  result := ASE_OK;
end;

function TDavASIOExtendedDriver.Start: TASIOError;
begin
  if assigned(fHostCallbacks) then
  begin
    InitializeTimeInfo;
    fCurrentBuffer := 0;
    StartProcessingThread;
    fIsProcessing := true;
    result := ASE_OK;
  end else
    result := ASE_NotPresent;
end;

function TDavASIOExtendedDriver.Stop: TASIOError;
begin
  fIsProcessing := false;
  StopProcessingThread;
  result := ASE_OK;
end;

procedure TDavASIOExtendedDriver.StartProcessingThread;
begin
  if assigned(fProcessingThread) then StopProcessingThread;

  fProcessingThread := TDavASIOExtDrvrProcessingThread.Create(true);
  fProcessingThread.Driver := self;
  fProcessingThread.Resume;
end;

procedure TDavASIOExtendedDriver.StopProcessingThread;
begin
  if assigned(fProcessingThread) then
  begin
    with fProcessingThread do
    begin
      if Suspended then Resume;

      Terminate;
      WaitFor;
    end;
    FreeAndNil(fProcessingThread);
  end;
end;

procedure TDavASIOExtendedDriver.ProcessBuffers;
begin
  // this is a dummy, override it
  ASIOBufferSwitch(fCurrentBuffer, ASIOTrue);
  Sleep(round(1000 * fBufferSize.Current / fSampleRate));
  fCurrentBuffer := 1-fCurrentBuffer;
end;






procedure TDavASIOExtendedDriver.GetNanoSeconds(var Time: TASIOTimeStamp);
var NanoSeconds : Double;
const CTwoRaisedTo32 : Double = 4294967296;
begin
 // it looks stupid, but this has to be in to lines, otherwise it would be an integer multiplication
 // this fucking bullshit took me 10 hours to find it :)
 NanoSeconds := timegettime;
 NanoSeconds := NanoSeconds*1000000;
 Time.Hi := floor(NanoSeconds / CTwoRaisedTo32);
 Time.Lo := floor(NanoSeconds - Time.Hi * CTwoRaisedTo32);
end;

function TDavASIOExtendedDriver.GetTimecodeSamples: TASIOSamples;
begin
  // override this if you need a custom timecode
  result := fSwitchTimings.SamplePos;
end;

procedure TDavASIOExtendedDriver.UpdateTimings;
var tmp: int64;
begin
  with fSwitchTimings.SamplePos do
  begin
    tmp := int64(Lo) + fBufferSize.Current;

    Lo := tmp and $FFFFFFFF;
    Hi := Hi + (tmp shr 32);
  end;

  GetNanoSeconds(fSwitchTimings.SystemTime);

  if fSupportsTimeInfo then
  begin
    fAsioTime.timeInfo.SamplePosition := fSwitchTimings.SamplePos;
    fAsioTime.timeInfo.SystemTime := fSwitchTimings.SystemTime;

    if fSupportsTimeCode then fAsioTime.timeCode.TimecodeSamples := GetTimecodeSamples;
  end;
end;

procedure TDavASIOExtendedDriver.ASIOBufferSwitch(DoubleBufferIndex: Integer; DirectProcess: TASIOBool);
begin
  if fSupportsTimeInfo then
  begin
    ASIOBufferSwitchTimeInfo(DoubleBufferIndex, DirectProcess);
    exit;
  end;

  UpdateTimings;

  if assigned(fHostCallbacks) and assigned(fHostCallbacks^.bufferSwitch) then
    fHostCallbacks^.bufferSwitch(DoubleBufferIndex, DirectProcess);
end;

procedure TDavASIOExtendedDriver.ASIOBufferSwitchTimeInfo(DoubleBufferIndex: Integer; DirectProcess: TASIOBool);
begin
  if not fSupportsTimeInfo then
  begin
    ASIOBufferSwitch(DoubleBufferIndex, DirectProcess);
    exit;
  end;

  UpdateTimings;

  if assigned(fHostCallbacks) and assigned(fHostCallbacks^.bufferSwitchTimeInfo) then
    fHostCallbacks^.bufferSwitchTimeInfo(fAsioTime, DoubleBufferIndex, DirectProcess);

  fAsioTime.TimeInfo.Flags := fAsioTime.TimeInfo.Flags and not (kSampleRateChanged or kClockSourceChanged);
end;

procedure TDavASIOExtendedDriver.ASIOSampleRateDidChange(SampleRate: TASIOSampleRate);
begin
  if assigned(fHostCallbacks) and assigned(fHostCallbacks^.sampleRateDidChange) then
    fHostCallbacks^.sampleRateDidChange(SampleRate);
end;

function TDavASIOExtendedDriver.ASIOMessage(Selector, Value: Integer; msg: Pointer; Opt: PDouble): Integer;
begin
  if Selector = kAsioResetRequest then SaveDriverSettings;

  if assigned(fHostCallbacks) and assigned(fHostCallbacks^.asioMessage) then
    result := fHostCallbacks^.asioMessage(Selector, Value, msg, Opt)
  else result := 0;
end;

procedure TDavASIOExtendedDriver.ASIORequestReset;
begin
  if fSupportedSelectors.ResetRequest then ASIOMessage(kAsioResetRequest, 0, nil, nil);
end;

procedure TDavASIOExtendedDriver.ASIOResyncRequest;
begin
  if fSupportedSelectors.ResyncRequest then ASIOMessage(kAsioResyncRequest, 0, nil, nil);
end;

procedure TDavASIOExtendedDriver.ASIOBufferSizeChange(newsize: LongInt);
var tmp: Longint;
begin
  tmp := 0;
  if fSupportedSelectors.BufferSizeChange then tmp := ASIOMessage(kAsioBufferSizeChange, newsize, nil, nil);
  if tmp=0 then ASIORequestReset;
end;

procedure TDavASIOExtendedDriver.ASIOLatenciesChanged;
begin
  if fSupportedSelectors.LatenciesChanged then ASIOMessage(kAsioLatenciesChanged, 0, nil, nil);
end;

{$IFDEF DELPHI10_UP} {$endregion 'Extended driver implemention'} {$ENDIF}

end.
