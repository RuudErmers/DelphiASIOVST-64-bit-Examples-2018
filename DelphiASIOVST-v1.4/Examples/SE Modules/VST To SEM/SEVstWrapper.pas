unit SEVstWrapper;

interface

uses
  Windows, Classes, DAV_Types, DAV_SECommon, DAV_SEModule, DAV_VSTEffect,
  DAV_VSTHost;

{$DEFINE UseMidi}

type
  TCustomVST2SEModule = class(TSEModuleBase)
  protected
    FVSTHost    : TVstHost;
    FInputPtr   : array of PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputPtr  : array of PDAVSingleFixedArray;
    FVSTInputs  : array of PDAVSingleFixedArray;
    FVSTOutputs : array of PDAVSingleFixedArray;
    {$IFDEF UseMidi}
    FVstEvents  : TVstEvents;
    FMidiOutPin : Integer;
    procedure MidiData(AClock: Cardinal; AMidiMsg: Cardinal; PinID: Integer); override;
    procedure ProcessEvents(Sender: TObject; VstEvents: PVstEvents);
    {$ENDIF}
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    procedure Open; override;
    function GetPinProperties(const Index: Integer; Properties : PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;

    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TStaticVST2SEModule = class(TCustomVST2SEModule)
  protected
    FParamPtr   : TDAVSingleDynArray;
    FParamEnum  : array of Integer;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    function GetPinProperties(const Index: Integer; Properties : PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  end;

  TAutomatableVST2SEModule = class(TCustomVST2SEModule)
  protected
    FParamPtr    : array of PDAVSingleFixedArray;
    FStaticCount : Integer;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    function GetPinProperties(const Index: Integer; Properties : PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  end;

implementation

uses
  Math, SysUtils;

var
  FS : TFormatSettings;

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;

constructor TCustomVST2SEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
var
  Channel  : Integer;
  RS : TResourceStream;
  CM : TStringList;
begin
 inherited Create(SEAudioMaster, Reserved);
 FVSTHost := TVstHost.Create(nil);

 {$IFDEF UseMidi}
 with FVstEvents do
  begin
   NumEvents := 0;
   Reserved := 0;
   for Channel := 0 to Length(Events) - 1 do
    begin
     GetMem(Events[Channel], SizeOf(TVstEvent));
     FillChar(Events[Channel]^, SizeOf(TVstEvent), 0);
    end;
  end;
 {$ENDIF}

 CM := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'VST', @EnumNamesFunc, DWord(CM));
  FVSTHost.VstPlugIns.Clear;
  assert(CM.Count <= 1);
  for Channel := 0 to CM.Count - 1 do
   begin
    FVSTHost.VstPlugIns.Add;
    RS := TResourceStream.Create(HInstance, CM[Channel], 'VST');
    try
     FVSTHost.VstPlugIns[Channel].LoadFromStream(RS);
     FVSTHost.VstPlugIns[Channel].Open;
     {$IFDEF UseMidi}
     FVSTHost.VstPlugIns[Channel].OnProcessEvents := ProcessEvents;
     {$ENDIF}
     SetLength(FInputPtr, FVSTHost[Channel].numInputs);
     SetLength(FOutputPtr, FVSTHost[Channel].numOutputs);
     SetLength(FVSTInputs, FVSTHost[Channel].numInputs);
     SetLength(FVSTOutputs, FVSTHost[Channel].numOutputs);
    finally
     FreeAndNil(RS);
    end;
   end;
 finally
  FreeAndNil(CM);
 end;
end;

destructor TCustomVST2SEModule.Destroy;
{$IFDEF UseMidi}
var
  Channel  : Integer;
{$ENDIF}
begin
 FreeAndNil(FVSTHost);
 {$IFDEF UseMidi}
 with FVstEvents do
  for Channel := 0 to Length(Events) - 1
   do Dispose(Events[Channel]);
 {$ENDIF}
 inherited;
end;

procedure TCustomVST2SEModule.Open;
begin
 OnProcess := SubProcess;
 if FVSTHost.Count = 0
  then CallHost(SEAudioMasterSleepMode);
 inherited Open; // always call the base class
end;

class procedure TCustomVST2SEModule.GetModuleProperties(Properties: PSEModuleProperties);
var
  CM  : TStringList;
  plg : string;
  str : string;
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   str := 'DAV VST-Wrapper';
   plg := 'Wrapper';
   CM := TStringList.Create;
   try
    EnumResourceNames(HInstance, 'VST', @EnumNamesFunc, DWord(CM));
    if CM.Count = 1
     then plg := CM[0]
     else plg := 'error!';
    str := str + ' - ' + plg;
   finally
    FreeAndNil(CM);
   end;

   str := str + #0;
   GetMem(Name, Length(str));
   Move(str[1], Name[0], Length(str));

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   str := 'VST2SEM - ' + plg + #0;
   GetMem(ID, Length(str));
   Move(str[1], ID[0], Length(str));

   // Info, may include Author, Web page whatever
   About := 'Wrapper created by Christian-W. Budde';

   SDKVersion := CSeSdkVersion;
//   GuiFlags := [gfControlView, gfStructureView];
  end;
end;

// describe the pins (plugs)
function TCustomVST2SEModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
var
  str : AnsiString;
begin
 if FVSTHost.Count = 0 then
  begin
   Result := False;
   exit;
  end;
 Result := True;
 if Index < FVSTHost[0].numInputs then
  with Properties^ do
   begin
    str             := 'Input ' + IntToStr(Index + 1);
    Name            := PAnsiChar(str);
    VariableAddress := @FInputPtr[Index];
    Direction       := drIn;
    Datatype        := dtFSample;
    Flags           := [iofPolyphonicActive];
   end else
 if Index - FVSTHost[0].numInputs < FVSTHost[0].numOutputs then
  with Properties^ do
   begin
    str             := 'Output ' + IntToStr(Index - FVSTHost[0].numInputs + 1);
    Name            := PAnsiChar(str);
    VariableAddress := @FOutputPtr[Index - FVSTHost[0].numInputs];
    Direction       := drOut;
    Datatype        := dtFSample;
 {$IFDEF UseMidi}
   end else
 if Index - FVSTHost[0].numInputs - FVSTHost[0].numOutputs = 0 then
  with Properties^ do
   begin
    Name      := 'MIDI Input';
    Direction := drIn;
    Datatype  := dtMidi2;
   end else
 if Index - FVSTHost[0].numInputs - FVSTHost[0].numOutputs = 1 then
  with Properties^ do
   begin
    Name        := 'MIDI Output';
    Direction   := drOut;
    Datatype    := dtMidi2;
    FMidiOutPin := Index;
 {$ENDIF}
   end else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
end;

{$IFDEF UseMidi}
procedure TCustomVST2SEModule.ProcessEvents(Sender: TObject; VstEvents: PVstEvents);
var
  EventIndex : Integer;
  AMidiMsg   : Integer;
begin
 if VstEvents <> nil then
  with VstEvents^ do
   for EventIndex := 0 to NumEvents - 1 do
    if Events[EventIndex]^.EventType = etMidi then
     with TVstMidiEvent(Events[EventIndex]^) do
      begin
       AMidiMsg := MidiData[0] + MidiData[1] shl 8 + MidiData[2] shl 16;
       Pin[FMidiOutPin].TransmitMIDI(SampleClock, AMidiMsg);
      end;
end;

procedure TCustomVST2SEModule.MidiData(AClock, AMidiMsg: Cardinal; PinID: Integer);
var
  IsSystemMsg : Boolean;
begin
 // system messages are for timing info etc
 // they don't have a channel, just pass them right through
 IsSystemMsg := (AMidiMsg and $F0) = $F0;

 if not IsSystemMsg then
  begin
   // The first byte of a midi message contains the status (note on/off etc) and the channel
   // the next two bytes depend on the type of message

   with FVstEvents do
    if NumEvents < Length(Events) then
     begin
      Inc(NumEvents);
      with PVstMidiEvent(FVstEvents.Events[0])^ do
       begin
        EventType   := etMidi;
        ByteSize    := 24;
        MidiData[0] := AMidiMsg;
        MidiData[1] := (AMidiMsg shr 8) and $FF;
        MidiData[2] := (AMidiMsg shr 16) and $FF;
       end;
     end;
  end;

 inherited;
end;
{$ENDIF}


{ TStaticVST2SEModule }

constructor TStaticVST2SEModule.Create(SEAudioMaster: TSE2audioMasterCallback;
  Reserved: Pointer);
begin
 inherited;
 if FVSTHost.Count > 0 then
  begin
   SetLength(FParamPtr, FVSTHost[0].numParams);
   SetLength(FParamEnum, FVSTHost[0].numParams);
  end;
end;

procedure TStaticVST2SEModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FVSTInputs)  - 1 do FVSTInputs[Channel]  := @FInputPtr[Channel, BufferOffset];
 for Channel := 0 to Length(FVSTOutputs) - 1 do FVSTOutputs[Channel] := @FOutputPtr[Channel, BufferOffset];

 {$IFDEF UseMidi}
 // process MIDI
 if FVstEvents.NumEvents > 0 then
  begin
   FVSTHost[0].ProcessEvents(FVstEvents);
   FVstEvents.NumEvents := 0;
  end;
 {$ENDIF}

 // process AUDIO
 FVSTHost[0].ProcessAudio(@FVSTInputs[0], @FVSTOutputs[0], SampleFrames);
end;

function TryStrToFloatX(const S: string): Boolean;
var
 dbl : Double;
 FS  : TFormatSettings;
begin
 FS.DecimalSeparator := '.';
 Result := TryStrToFloat(s, dbl, FS);
 FS.DecimalSeparator := ',';
 if TryStrToFloat(s, dbl, FS)
  then Result := True;
end;

class procedure TStaticVST2SEModule.GetModuleProperties(Properties: PSEModuleProperties);
var
  str : string;
begin
 inherited;

 with Properties^ do
  begin
   str := StrPas(Name) + ' (static)';
   str := str + #0;
   ReallocMem(Name, Length(str));
   Move(str[1], Name[0], Length(str));

   str := StrPas(ID) + ' (static)';
   str := str + #0;
   ReallocMem(ID, Length(str));
   Move(str[1], ID[0], Length(str));
  end;
end;

// describe the pins (plugs)
function TStaticVST2SEModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
var
  Channel : Integer;
  s, p    : Single;
  str     : string;
  Param   : Integer;
  pd, un  : array [0..1] of string;
  IsFlt   : Boolean;
begin
 if FVSTHost.Count = 0 then
  begin
   Result := False;
   Exit;
  end;

 Result := inherited GetPinProperties(Index, Properties);

 // eventually add parameter
 if not Result then
  begin
   // get parameter number
   Param := (Index - FVSTHost[0].numInputs - FVSTHost[0].numOutputs {$IFDEF UseMidi} - 2 {$ENDIF});
   Assert((Param >= 0) and (Param <= FVSTHost[0].numParams));

   if Param < FVSTHost[0].numParams then
    with Properties^ do
     begin
      Result := True;

      // parameter name
      str := FVSTHost[0].ParameterName[Param] + #0;
      GetMem(Name, Length(str));
      Move(str[1], Name[0], Length(str));
      Direction := drParameter;

      // detect
      FVSTHost[0].Parameter[Param] := 0;
      pd[0] := FVSTHost[0].ParameterDisplay[Param] + FVSTHost[0].ParameterLabel[Param];
      FVSTHost[0].Parameter[Param] := 0.01;
      pd[1] := FVSTHost[0].ParameterDisplay[Param] + FVSTHost[0].ParameterLabel[Param];

      if pd[0] = pd[1] then
       begin
        VariableAddress := @FParamPtr[Param];
        Datatype        := dtEnum;

        // build enum values
        str := '';
        with TStringList.Create do
         try
          FVSTHost[0].Parameter[Param] := 0;
          s := 0.01;
          p := FVSTHost[0].Parameter[Param];
          pd[0] := FVSTHost[0].ParameterDisplay[Param];
          un[0] := FVSTHost[0].ParameterLabel[Param];
          IsFlt := TryStrToFloatX(pd[0]);
          Add(pd[0] + un[0]);
          while FVSTHost[0].Parameter[Param] < 1 do
           begin
            FVSTHost[0].Parameter[Param] := FVSTHost[0].Parameter[Param] + s;
            if FVSTHost[0].Parameter[Param] = p then
             begin
              s := s + 0.03;
              if s >= 1
               then Break
               else Continue;
             end;
            pd[1] := FVSTHost[0].ParameterDisplay[Param];
            un[1] := FVSTHost[0].ParameterLabel[Param];
            if pd[0] + un[0] <> pd[1] + un[1] then
             begin
              pd[0] := pd[1];
              un[0] := un[1]; 
              Add(pd[0] + un[0]);
              if not TryStrToFloatX(pd[0])
               then IsFlt := False;
             end;
           end;

          if (Count = 1) or (IsFlt and (Count > 10)) then
           begin
            VariableAddress := @FParamPtr[Param];
            Datatype        := dtSingle;
            FParamEnum[Param] := 0;
            DefaultValue    := '0';
            Exit;
           end;

          FParamEnum[Param] := Count;
          for Channel := 0 to Count - 1
           do str := str + Strings[Channel] + '=' + IntToStr(Channel) + ',';
         finally
          Free;
         end;

        if Length(str) > 0 then SetLength(str, Length(str) - 1);
        DatatypeExtra := PAnsiChar(str);
        DefaultValue  := '0';
       end
      else
       begin
        VariableAddress   := @FParamPtr[Param];
        Datatype          := dtSingle;
        FParamEnum[Param] := 0;
        DefaultValue      := '0';
       end;
     end
    else Result := False; 
  end;
end;

// An input plug has changed value
procedure TStaticVST2SEModule.PlugStateChange(const CurrentPin: TSEPin);
var
  Channel  : Integer;
  Param    : Integer;
  InState  : TSEStateType;
  OutState : TSEStateType;
begin
 if FVSTHost.Count >= 0 then
  with FVSTHost[0] do
   begin
    if (CurrentPin.PinID >= {$IFDEF UseMidi} 2 + {$ENDIF} numInputs + numOutputs) and
       (CurrentPin.PinID < {$IFDEF UseMidi} 2 + {$ENDIF} numInputs + numOutputs + numParams)  then
     begin
      Param := CurrentPin.PinID {$IFDEF UseMidi} - 2 {$ENDIF} - numInputs - numOutputs;
      Assert((Param >= 0) and (Param < FVSTHost[0].numParams));
      if FParamEnum[Param] > 0
       then Parameter[Param] := PInteger(@FParamPtr[Param])^ / FParamEnum[Param]
       else Parameter[Param] := FParamPtr[Param]
     end;

    // query the 'state of the input plugs...
    //   stRun    = Normal Streaming Audio        (e.g. from an oscillator)
    //   stStatic = Fixed, unchanging input value (e.g. a slider at rest)
    InState := stRun;
    for Channel := 0 to numInputs - 1 do
     if Pin[Channel].Status < InState then InState := Pin[Channel].Status;

    OutState := stRun;
    for Channel := numInputs to numInputs + numOutputs - 1 do
     if Pin[Channel].Status < OutState then OutState := Pin[Channel].Status;

    if InState > OutState
     then OutState := InState;

    // 'transmit' new output status to next module 'downstream'
    for Channel := numInputs to numInputs + numOutputs - 1
     do Pin[Channel].TransmitStatusChange(SampleClock, OutState);
   end;
 inherited;
end;


{ TAutomatableVST2SEModule }

constructor TAutomatableVST2SEModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 if FVSTHost.Count > 0
  then SetLength(FParamPtr, FVSTHost[0].numParams);
end;

class procedure TAutomatableVST2SEModule.GetModuleProperties(Properties: PSEModuleProperties);
var
  str : string;
begin
 inherited;
 with Properties^ do
  begin
   str := StrPas(Name) + ' (automatable)';
   str := str + #0;
   ReallocMem(Name, Length(str));
   Move(str[1], Name[0], Length(str));

   str := StrPas(ID) + ' (automatable)';
   str := str + #0;
   ReallocMem(ID, Length(str));
   Move(str[1], ID[0], Length(str));
  end;
end;

function TAutomatableVST2SEModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
var
  Param : Integer;
  str   : string;
begin
 if FVSTHost.Count = 0 then
  begin
   Result := False;
   Exit;
  end;

 Result := inherited GetPinProperties(Index, Properties);

 if not Result then
  begin
   Param := Index {$IFDEF UseMidi} - 2 {$ENDIF} - FVSTHost[0].numInputs - FVSTHost[0].numOutputs;
   Assert((Param >= 0) and (Param <= FVSTHost[0].numParams));

   if Param < FVSTHost[0].numParams then
    with Properties^ do
     begin
      // parameter name
      str := FVSTHost[0].ParameterName[Param] + #0;
      GetMem(Name, Length(str));
      Move(str[1], Name[0], Length(str));
      Direction       := drIn;
      Datatype        := dtFSample;
      VariableAddress := @FParamPtr[Param];
      DefaultValue    := '0';
      Result          := True;
     end
    else Result := False; 
   end;
end;

procedure TAutomatableVST2SEModule.PlugStateChange(const CurrentPin: TSEPin);
var
  Channel  : Integer;
  Param    : Integer;
  s        : Single;
  InState  : TSEStateType;
  OutState : TSEStateType;
begin
 if not Assigned(CurrentPin) then exit;

 if FVSTHost.Count >= 0 then
  with FVSTHost[0] do
   begin
    if (CurrentPin.PinID >= {$IFDEF UseMidi} 2 + {$ENDIF} numInputs + numOutputs) and
       (CurrentPin.PinID < {$IFDEF UseMidi} 2 + {$ENDIF} numInputs + numOutputs + numParams)  then
     try
      Param := CurrentPin.PinID {$IFDEF UseMidi} - 2 {$ENDIF} - numInputs - numOutputs;
      Assert((Param >= 0) and (Param < FVSTHost[0].numParams));
      s   := CurrentPin.Value;
      if not IsNaN(s)
       then Parameter[Param] := s;
     except
     end;

    // query the 'state of the input plugs...
    //   stRun    = Normal Streaming Audio        (e.g. from an oscillator)
    //   stStatic = Fixed, unchanging input value (e.g. a slider at rest)

    if (Index >= 0) and (Index < numInputs + numOutputs) then
     begin
      InState := stRun;
      for Channel := 0 to numInputs - 1 do
       if Pin[Channel].Status < InState then InState := Pin[Channel].Status;

      OutState := stRun;
      for Channel := numInputs to numInputs + numOutputs - 1 do
       if Pin[Channel].Status < OutState then OutState := Pin[Channel].Status;

      if InState > OutState
       then OutState := InState;

      // 'transmit' new output status to next module 'downstream'
      for Channel := numInputs to numInputs + numOutputs - 1
       do Pin[Channel].TransmitStatusChange(SampleClock, OutState);

      // setup 'sleep mode' or not
      if (OutState < stRun) then
       begin
        FStaticCount := BlockSize;
        OnProcess := SubProcessStatic;
       end
      else OnProcess := SubProcess;
     end;
   end;

 inherited;
end;

procedure TAutomatableVST2SEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FVSTInputs)  - 1 do FVSTInputs[Channel]  := @FInputPtr[Channel, BufferOffset];
 for Channel := 0 to Length(FVSTOutputs) - 1 do FVSTOutputs[Channel] := @FOutputPtr[Channel, BufferOffset];

 {$IFDEF UseMidi}
 // process MIDI
 if FVstEvents.NumEvents > 0 then
  begin
   FVSTHost[0].ProcessEvents(FVstEvents);
   FVstEvents.NumEvents := 0;
  end;
 {$ENDIF}

 // process AUDIO
 FVSTHost[0].ProcessAudio(@FVSTInputs[0], @FVSTOutputs[0], SampleFrames);
end;

procedure TAutomatableVST2SEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

initialization
 GetLocaleFormatSettings(GetThreadLocale, FS);

end.
