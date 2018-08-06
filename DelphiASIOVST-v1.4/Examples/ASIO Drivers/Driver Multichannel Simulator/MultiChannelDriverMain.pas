unit MultiChannelDriverMain;

interface

{$I DAV_Compiler.inc}

{$WARN SYMBOL_PLATFORM OFF}

uses
  Windows, SysUtils, Classes, Forms, ComObj, DAV_Common, DAV_Types, DAV_ASIO,
  DAV_ASIODriver, DAV_AsioHost;

const
  // Basic driver constants
  CClass_AsioHostDriver: TGUID = '{A8DD45FD-34CC-4996-9695-CDD2AE483B49}';
  CDriverName = 'DAV MultiChannel Driver';
  CDriverDescription = 'DAV MultiChannel Driver';

  // other
  CBlockFrames = 256;
  //CNumInputs = 2;
  //CNumOutputs = 2;

type
  TMultiChannelSettings = record
    DriverIndex: Integer;
    InputAssignment: array [1..16] of integer;
    OutputAssignment: array [1..16] of integer;
    Inputs: Integer;
    Outputs: Integer;
  end;

  IAsioHostDriver = interface(IDavASIODriverInterface)
    ['{A8DD45FD-34CC-4996-9695-CDD2AE483B49}']
  end;

  TAsioHostDriverWrapper = class(TDavASIOTCWrapper, IAsioHostDriver)
  protected
    function GetDriverClass: TTDavASIODriver; override;
  end;

  TAsioHostDriver = class(TDavASIODriver)
  private
    FSamplePosition : Double;
    FCallbacks      : PASIOCallbacks;
    FAsioTime       : TASIOTime;
    FSystemTime     : TASIOTimeStamp;
    FInMap          : array of LongInt;
    FOutMap         : array of LongInt;
    FBlockFrames    : LongInt;
    FActiveInputs   : LongInt;
    FActiveOutputs  : LongInt;
    FToggle         : LongInt;
    FActive         : Boolean;
    FStarted        : Boolean;
    FTimeCodeRead   : Boolean;
    FTimeInfoMode   : Boolean;
    FDriverVersion  : Integer;
    FSystemHandle   : HWND;
    FAsioHost       : TAsioHost;
    FErrorMessage   : array [0..127] of Char;
    FInputBuffers   : TDAVArrayOfSingleFixedArray;
    FOutputBuffers  : TDAVArrayOfSingleFixedArray;

    procedure TimerOn;
    procedure TimerOff;
    procedure BufferSwitchX;
    procedure BufferSwitch32EventHandler(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ResetRequestedHandler(Sender: TObject);
  public
    Settings: TMultiChannelSettings;
    constructor Create(TCWrapper: TDavASIOTCWrapper; InterfaceGUID: TGuid); override;
    destructor Destroy; override;

    procedure LoadDriverSettings;
    procedure SaveDriverSettings;
    procedure SaveAndReset(Sender: TObject);

    function Init(SysHandle: HWND): boolean; override;
    function GetDriverName: string; override;
    function GetDriverVersion: LongInt; override;
    function GetErrorMessage: string; override;
    function Start: TASIOError; override;
    function Stop: TASIOError; override;
    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; override;
    function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; override;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; override;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; override;
    function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; override;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; override;
    function GetClockSources(Clocks: PASIOClockSources; out NumSources: LongInt): TASIOError; override;
    function SetClockSource(Reference: LongInt): TASIOError; override;
    function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; override;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; override;
    function CreateBuffers(BufferInfos: PASIOBufferInfos; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; override;
    function DisposeBuffers: TASIOError; override;
    function Future(Selector: LongInt; Opt: Pointer): TASIOError; override;
    function OutputReady: TASIOError; override;

    function GetMilliSeconds: LongInt;

    property AsioHost: TAsioHost read FAsioHost; 
  end;

// extern
procedure GetNanoSeconds(var Time: TASIOTimeStamp);

// local
function AsioSamples2Double(const Samples: TASIOSamples): Double;

implementation

uses
  Math, MMSystem, ComServ, Registry, MultiChannelDriverControlPanel;

const
  CTwoRaisedTo32 : Double = 4294967296;
  CTwoRaisedTo32Reciprocal : Double = 1 / 4294967296;

////////////////////////////////////////////////////////////////////////////////

function AsioSamples2Double(const Samples: TASIOSamples): Double;
begin
 Result := Samples.Lo;
 if Samples.Hi <> 0
  then Result := Result + Samples.Hi * CTwoRaisedTo32;
end;

procedure GetNanoSeconds(var Time: TASIOTimeStamp);
var
  NanoSeconds : Double;
begin
 // it looks stupid, but this has to be in two lines, otherwise it would be an
 // integer multiplication this fucking bullshit took me 10 hours to find it :)
 NanoSeconds := TimeGetTime;
 NanoSeconds := NanoSeconds * 1000000;
 Time.Hi := Floor(NanoSeconds / CTwoRaisedTo32);
 Time.Lo := Floor(NanoSeconds - Time.Hi * CTwoRaisedTo32);
end;


////////////////////////////////////////////////////////////////////////////////

{ TAsioHostDriverWrapper }

function TAsioHostDriverWrapper.GetDriverClass: TTDavASIODriver;
begin
  Result := TAsioHostDriver;
end;

////////////////////////////////////////////////////////////////////////////////

{ TAsioHostDriver }

procedure TAsioHostDriver.LoadDriverSettings;
var
  i : Integer;
  s : string;
begin
 with Settings do
  begin
   DriverIndex := -1;
   for i := 1 to 16 do
    begin
     InputAssignment[i] := 0;
     OutputAssignment[i] := 0;
    end;
   Inputs := 0;
   Outputs := 0;
  end;

  with TRegistry.Create, Settings do
   try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly('software\asio\' + CDriverName) then
     begin
      if ValueExists('DriverIndex') then DriverIndex := ReadInteger('DriverIndex');
      for i := 16 downto 1 do
       begin
        if i < 10 then s := '0' + IntToStr(i) else s := IntToStr(i);

        if ValueExists('in' + s) then InputAssignment[i] := ReadInteger('in' + s);
        if ValueExists('out' + s) then OutputAssignment[i] := ReadInteger('out' + s);

        if InputAssignment[i] = 0 then Inputs := i - 1;
        if OutputAssignment[i] = 0 then Outputs := i - 1;
       end;

    end;
   finally
    Free;
   end;

  if Settings.Inputs < 1 then Settings.Inputs := 2;
  if Settings.Outputs < 1 then Settings.Outputs := 2;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.SaveDriverSettings;
var
  i : Integer;
  s : string;
begin
 with TRegistry.Create, Settings do
  try
   RootKey := HKEY_LOCAL_MACHINE;
   if OpenKey('software\asio\' + CDriverName, True) then
    begin
     WriteInteger('DriverIndex', DriverIndex);

     for i := 1 to 16 do
      begin
       if i < 10 then s := '0' + IntToStr(i) else s := IntToStr(i);

       WriteInteger('in' + s, InputAssignment[i]);
       WriteInteger('out' + s, OutputAssignment[i]);
      end;
    end;
  finally
   Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TAsioHostDriver.Create(TCWrapper: TDavASIOTCWrapper; InterfaceGUID: TGuid);
var
  Channel : Integer;
begin
 inherited;

 LoadDriverSettings;

 // typically blockFrames * 2; try to get 1 by offering direct buffer
 // access, and using asioPostOutput for lower latency
 FSamplePosition := 0;
 FActive         := False;
 FStarted        := False;
 FTimeInfoMode   := False;
 FTimeCodeRead   := False;
 FDriverVersion  := 1;

 // input channels
 SetLength(FInputBuffers, Settings.Inputs);
 SetLength(FInMap, Settings.Inputs);
 for Channel := 0 to Settings.Inputs - 1 do
  begin
   FInputBuffers[Channel] := nil;
   FInMap[Channel] := 0;
  end;

 // output channels
 SetLength(FOutputBuffers, Settings.Outputs);
 SetLength(FOutMap, Settings.Outputs);
 for Channel := 0 to Settings.Outputs - 1 do
  begin
   FOutputBuffers[Channel] := nil;
   FOutMap[Channel] := 0;
  end;

 FCallbacks := nil;
 FActiveInputs := 0;
 FActiveOutputs := 0;
 FToggle := 0;

 FAsioHost := TAsioHost.Create(nil);
 with FAsioHost do
  begin
   SetIgnoredDriver(InterfaceGUID);
   if Settings.DriverIndex < 0
    then Settings.DriverIndex := AsioHost.DriverList.IndexOf('ASIO4ALL v2');

   DriverIndex := Settings.DriverIndex;
   OnBufferSwitch32 := BufferSwitch32EventHandler;
   OnDriverChanged := SaveAndReset;
   OnReset := ResetRequestedHandler;
   FBlockFrames := FAsioHost.BufferSize;
  end;
  
 SetControlPanelClass(TFmAsioDriverControlPanel);
 InitControlPanel;
end;

////////////////////////////////////////////////////////////////////////////////

destructor TAsioHostDriver.Destroy;
begin
 Stop;
 FreeAndNil(FAsioHost);
 DisposeBuffers;
 SetLength(FInputBuffers, 0);
 SetLength(FOutputBuffers, 0);
 SetLength(FInMap, 0);
 SetLength(FOutMap, 0);

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.Init(SysHandle: HWND): boolean;
begin
 Result := True;
 FSystemHandle := SysHandle;
 if FActive then Exit;

 StrCopy(FErrorMessage, 'ASIO Driver Init Failure');
 FActive := True;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetDriverName: string;
begin
  Result := CDriverName;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetDriverVersion: LongInt;
begin
 Result := FDriverVersion;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetErrorMessage: string;
begin
 Result := FErrorMessage;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.Start: TASIOError;
begin
 if Assigned(FCallbacks) then
  begin
   FStarted := False;
   FSamplePosition := 0;
   FSystemTime.Lo := 0;
   FSystemTime.Hi := 0;
   FToggle := 0;

   // activate 'hardware'
   TimerOn;
   FStarted := True;

   Result := ASE_OK;
  end                         
 else Result := ASE_NotPresent;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.Stop: TASIOError;
begin
 FStarted := False;
 TimerOff;    // de-activate 'hardware'
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.TimerOn;
begin
 if Assigned(FAsioHost)
  then FAsioHost.Active := True;
end;

procedure TAsioHostDriver.TimerOff;
begin
 if Assigned(FAsioHost)
  then FAsioHost.Active := False;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError;
begin
 NumInputChannels := Settings.Inputs;
 NumOutputChannels := Settings.Outputs;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError;
begin
 InputLatency := FAsioHost.InputLatency;
 OutputLatency := FAsioHost.OutputLatency;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetMilliSeconds: LongInt;
begin
 Result := 0;
end;
   
////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: Integer): TASIOError;
begin
 // allow one fixed size only
 if FAsioHost.DriverIndex >= 0 then
  begin
   MinSize := FAsioHost.BufferMinimum;
   MaxSize := FAsioHost.BufferMaximum;
   PreferredSize := FAsioHost.BufferPreferredSize;
   Granularity := FAsioHost.BufferGranularity;
   Result := ASE_OK;
  end
 else Result := ASE_NotPresent;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.CanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
 if FAsioHost.DriverIndex >= 0
  then Result := FAsioHost.CanSampleRate(SampleRate)
  else Result := ASE_NoClock;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
begin
 SampleRate := FAsioHost.SampleRate;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.SetSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
 if FAsioHost.DriverIndex < 0
  then Result := ASE_NoClock
  else
   begin
    Result := FAsioHost.CanSampleRate(SampleRate);

    if Result = ASE_OK then
     if (SampleRate <> FAsioHost.SampleRate) then
      begin
       FAsioHost.SampleRate := SampleRate;
       with FAsioTime do
        begin
         TimeInfo.SampleRate := SampleRate;
         TimeInfo.Flags:= TimeInfo.flags or kSampleRateChanged;
        end;
       if Assigned(FCallbacks) and Assigned(FCallbacks^.SampleRateDidChange)
        then FCallbacks^.SampleRateDidChange(FAsioHost.SampleRate);
      end;
   end;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetClockSources(Clocks: PASIOClockSources; out NumSources: LongInt): TASIOError;
begin
 with Clocks^[0] do
  begin
   Index := 0;
   AssociatedChannel := -1;
   AssociatedGroup := -1;
   IsCurrentSource := ASIOTrue;
   StrCopy(Name, 'Internal');
  end;
 NumSources := 1;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.SetClockSource(Reference: LongInt): TASIOError;
begin
 if Reference = 0 then
  begin
   FAsioTime.TimeInfo.Flags:= FAsioTime.TimeInfo.Flags or kClockSourceChanged;
   Result := ASE_OK;
  end
 else Result := ASE_NotPresent;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
begin
 TimeStamp.Lo := FSystemTime.Lo;
 TimeStamp.Hi := FSystemTime.Hi;
 if (FSamplePosition >= CTwoRaisedTo32) then
  begin
   SamplePosition.Hi := Round(FSamplePosition * CTwoRaisedTo32Reciprocal);
   SamplePosition.Lo := Round(FSamplePosition - (SamplePosition.Hi * CTwoRaisedTo32));
  end
 else
  begin
   SamplePosition.Hi := 0;
   SamplePosition.Lo := Round(FSamplePosition);
  end;

 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetChannelInfo(var Info: TASIOChannelInfo): TASIOError;
var
  i : Integer;
begin
 if (Info.Channel < 0) then
  begin
   Result := ASE_InvalidParameter;
   Exit;
  end else
 if Info.IsInput <> 0 then
  if Info.Channel >= Settings.Inputs then
   begin
    Result := ASE_InvalidParameter;
    Exit;
   end else else
  if Info.Channel >= Settings.Outputs then
   begin
    Result := ASE_InvalidParameter;
    Exit;
   end;

 Info.SampleType := ASIOSTFloat32LSB;

 Info.ChannelGroup := 0;
 Info.IsActive := ASIOFalse;

 if Info.IsInput <> 0 then
  begin
   for i := 0 to FActiveInputs - 1 do
    begin
     if FInMap[i] = Info.Channel then
      begin
       Info.IsActive := ASIOTrue;
       Break;
      end;
    end;
  end
 else
  begin
   for i := 0 to FActiveOutputs - 1 do
    begin
     if (FOutMap[i] = Info.Channel) then
      begin
       Info.IsActive := ASIOTrue;
       Break;
      end;
    end;
  end;

 StrPCopy(Info.Name, 'Channel ' + IntToStr(Info.Channel + 1));
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.CreateBuffers(BufferInfos: PASIOBufferInfos; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError;
var
  BufferInfo   : PASIOBufferInfos;
  Channel      : Integer;
  NotEnoughMem : Boolean;
begin
 BufferInfo := BufferInfos;
 NotEnoughMem := False;

 FActiveInputs := 0;
 FActiveOutputs := 0;
 FBlockFrames := BufferSize;
 for Channel := 0 to numChannels - 1 do
  begin
   if BufferInfo^[Channel].IsInput <> 0 then
    begin
     if (BufferInfo^[Channel].ChannelNum < 0) or (BufferInfo^[Channel].ChannelNum >= Settings.Inputs) then
      begin
       DisposeBuffers;
       Result := ASE_InvalidParameter;
       Exit;
      end;
     FInMap[FActiveInputs] := BufferInfo^[Channel].ChannelNum;

     // double buffer
     GetMem(FInputBuffers[FActiveInputs], 2 * FBlockFrames * SizeOf(Single));
     if Assigned(FInputBuffers[FActiveInputs]) then
      begin
       BufferInfo^[Channel].Buffers[0] := @FInputBuffers[FActiveInputs]^[0];
       BufferInfo^[Channel].Buffers[1] := @FInputBuffers[FActiveInputs]^[FBlockFrames];
      end
     else
      begin
       BufferInfo^[Channel].Buffers[0] := nil;
       BufferInfo^[Channel].Buffers[1] := nil;
       NotEnoughMem := True;
      end;

     FActiveInputs:= FActiveInputs + 1;
     if (FActiveInputs > Settings.Inputs) then
      begin 
       DisposeBuffers;
       Result := ASE_InvalidParameter;
       Exit;
      end;
    end
   else  // output
    begin
     if (BufferInfo^[Channel].ChannelNum < 0) or (BufferInfo^[Channel].ChannelNum >= Settings.Outputs) then
      begin
       DisposeBuffers;
       Result := ASE_InvalidParameter;
       Exit;
      end;
     FOutMap[FActiveOutputs] := BufferInfo^[Channel].ChannelNum;

     // double buffer
     GetMem(FOutputBuffers[FActiveOutputs], 2 * FBlockFrames * SizeOf(Single));
     if Assigned(FOutputBuffers[FActiveOutputs]) then
      begin
       BufferInfo^[Channel].Buffers[0] := @FOutputBuffers[FActiveOutputs]^[0];
       BufferInfo^[Channel].Buffers[1] := @FOutputBuffers[FActiveOutputs]^[FBlockFrames];
      end
     else
      begin
       BufferInfo^[Channel].Buffers[0] := nil;
       BufferInfo^[Channel].Buffers[1] := nil;
       NotEnoughMem := True;
      end;
     FActiveOutputs:= FActiveOutputs + 1;
     if (FActiveOutputs > Settings.Outputs) then
      begin
       FActiveOutputs := FActiveOutputs - 1;
       DisposeBuffers;
       Result := ASE_InvalidParameter;
       Exit;
      end;
    end;

   //Inc(BufferInfo);
  end;

 if NotEnoughMem then
  begin
   DisposeBuffers;
   Result := ASE_NoMemory;
   Exit;
  end;

 FCallbacks := @Callbacks;
 if (Callbacks.AsioMessage(kAsioSupportsTimeInfo, 0, nil, nil)) <> 0 then
  with FAsioTime do
   begin
    FTimeInfoMode := True;
    with TimeInfo do
     begin
      Speed := 1.;
      SystemTime.Hi := 0;
      SystemTime.Lo := 0;
      SamplePosition.Hi := 0;
      SamplePosition.Lo := 0;
      SampleRate := FAsioHost.SampleRate;
      Flags := kSystemTimeValid or kSamplePositionValid or kSampleRateValid;
     end;

    with TimeCode do
     begin
      Speed := 1.;
      TimeCodeSamples.Lo := 0;
      TimeCodeSamples.Hi := 0;
      Flags := kTcValid or kTcRunning;
     end;
   end
 else FTimeInfoMode := False;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.DisposeBuffers: TASIOError;
var
  Channel : Integer;
begin
 FCallbacks := nil;
 Stop;

 for Channel := 0 to FActiveInputs - 1 do
   if Assigned(FInputBuffers[Channel]) then
   begin
     Dispose(FInputBuffers[Channel]);
     FInputBuffers[Channel] := nil;
   end;

 FActiveInputs := 0;

 for Channel := 0 to FActiveOutputs - 1 do 
   if Assigned(FOutputBuffers[Channel]) then
   begin
     Dispose(FOutputBuffers[Channel]);
     FOutputBuffers[Channel] := nil;
   end;

 FActiveOutputs := 0;

 Result := ASE_OK; 
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.Future(Selector: Integer; Opt: Pointer): TASIOError;
begin
 Result := ASE_SUCCESS;
 case Selector of
   kAsioEnableTimeCodeRead : FTimeCodeRead := True;
  kAsioDisableTimeCodeRead : FTimeCodeRead := False;
      kAsioSetInputMonitor : Result := ASE_SUCCESS;  // for testing!!!
      kAsioCanInputMonitor : Result := ASE_SUCCESS;  // for testing!!!
          kAsioCanTimeInfo : Result := ASE_SUCCESS;
          kAsioCanTimeCode : Result := ASE_SUCCESS;
  else Result := ASE_NotPresent;
 end;
end;


////////////////////////////////////////////////////////////////////////////////
// asio2 buffer switch
////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.BufferSwitchX;
begin
 with FAsioTime, TimeInfo, TimeCode do
  begin
   GetSamplePosition(SamplePosition, SystemTime);

   if FTimeCodeRead then
    begin
     TimeCodeSamples.Lo := SamplePosition.Lo;
     TimeCodeSamples.Hi := SamplePosition.Hi;
    end;
   FCallbacks^.BufferSwitchTimeInfo(FAsioTime, FToggle, ASIOFalse);
   Flags := Flags and  not (kSampleRateChanged or kClockSourceChanged);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.OutputReady: TASIOError;
begin
 Result := ASE_NotPresent;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.SaveAndReset(Sender: TObject);
begin
 SaveDriverSettings;
 ResetRequestedHandler(Sender);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.ResetRequestedHandler(Sender: TObject);
begin
 if Assigned(FCallbacks) then
  if Assigned(FCallbacks.AsioMessage)
   then FCallbacks.AsioMessage(kAsioResetRequest, 0, nil, nil);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.BufferSwitch32EventHandler(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel : Integer;
  Offset  : Integer;
begin
  if FStarted and Assigned(FCallbacks) then
  begin
   GetNanoSeconds(FSystemTime);      // latch system time

   if FToggle = 0
    then Offset := 0
    else Offset := FBlockFrames;
  // Min(FActiveInputs,AsioHost.InputChannelCount) - 1

  // Fill Zero
   for Channel := 0 to AsioHost.OutputChannelCount - 1
    do Fillchar(OutBuffer[Channel]^[0], 0, FBlockFrames * SizeOf(Single));

   for Channel := 0 to FActiveInputs - 1 do
    if Settings.InputAssignment[FOutMap[Channel] + 1] > 1
     then Move(InBuffer[Settings.InputAssignment[FInMap[Channel] + 1] - 2]^[0], FInputBuffers[Channel]^[Offset], FBlockFrames * SizeOf(Single))
     else Fillchar(FInputBuffers[Channel]^[Offset], 0, FBlockFrames * SizeOf(Single));

   for Channel := 0 to FActiveOutputs - 1 do
    if Settings.OutputAssignment[FOutMap[Channel] + 1] > 1
     then Move(FOutputBuffers[Channel]^[Offset], OutBuffer[Settings.OutputAssignment[FOutMap[Channel] + 1] - 2]^[0], FBlockFrames * SizeOf(Single));

   FSamplePosition := FSamplePosition + FBlockFrames;

   if FTimeInfoMode
    then BufferSwitchX
    else FCallbacks^.BufferSwitch(FToggle, ASIOFalse);

   FToggle := 1 - FToggle;
  end;
end;

initialization
  TDavAsioDriverFactory.Create(ComServer, TAsioHostDriverWrapper, CClass_AsioHostDriver,
    'AsioHostDriver', CDriverDescription, ciMultiInstance, tmApartment);

end.
