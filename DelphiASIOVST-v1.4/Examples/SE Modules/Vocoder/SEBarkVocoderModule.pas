unit SEBarkVocoderModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspVocoder;

type
  // define some constants to make referencing in/outs clearer
  TSEBarkVocoderPins = (pinInput, pinCarrier, pinOutput, pinBandwidth, pinAttack,
    pinRelease);

  TCustomSEBarkVocoderModule = class(TSEModuleBase)
  private
    FInputBuffer   : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer  : PDAVSingleFixedArray;
    FCarrierBuffer : PDAVSingleFixedArray;
    FStaticCount   : Integer;
    FBandwidth     : Single;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FVocoder : TBarkScaleVocoder;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  TSEBarkVocoderStaticModule = class(TCustomSEBarkVocoderModule)
  private
    FAttack    : Single;
    FRelease   : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEBarkVocoderControllableModule = class(TSEBarkVocoderStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEBarkVocoderAutomatableModule = class(TCustomSEBarkVocoderModule)
  protected
    FAttackBuffer  : PDAVSingleFixedArray;
    FReleaseBuffer : PDAVSingleFixedArray;
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSEBarkVocoderModule }

constructor TCustomSEBarkVocoderModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FVocoder := TBarkScaleVocoder.Create;
 FVocoder.SynthLevel := 0;
 FVocoder.InputLevel := 0;
 FVocoder.VocoderLevel := 1;
end;

destructor TCustomSEBarkVocoderModule.Destroy;
begin
 FreeAndNil(FVocoder);
 inherited;
end;

procedure TCustomSEBarkVocoderModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEBarkVocoderModule.SampleRateChanged;
begin
 inherited;
 FVocoder.SampleRate := SampleRate;
end;

procedure TCustomSEBarkVocoderModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Inp     : PDAVSingleFixedArray;
  Carrier : PDAVSingleFixedArray;
  Outp    : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp     := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp    := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Carrier := PDAVSingleFixedArray(@FCarrierBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FVocoder.ProcessSample(Inp^[Sample], Carrier^[Sample]);
end;

procedure TCustomSEBarkVocoderModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEBarkVocoderModule.ChooseProcess;
begin
 if (Pin[Integer(pinInput)].Status = stRun) and
    (Pin[Integer(pinCarrier)].Status = stRun)
  then
   begin
    OnProcess := SubProcess;
    Pin[2].TransmitStatusChange(SampleClock, stRun);
   end
  else
   begin
    FStaticCount := BlockSize + Round(0.001 * FVocoder.Release * FVocoder.SampleRate);
    OnProcess := SubProcessStatic;
    Pin[2].TransmitStatusChange(SampleClock, stStop);
   end;
end;

// describe your module
class procedure TCustomSEBarkVocoderModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEBarkVocoderModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEBarkVocoderPins(index) of
  pinInput:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
     Flags           := [iofLinearInput];
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  pinCarrier:
   with Properties^ do
    begin
     Name            := 'Carrier';
     VariableAddress := @FCarrierBuffer;
     Direction       := drIn;
     Flags           := [iofLinearInput];
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  pinOutput:
   with Properties^ do
    begin
     Name            := 'Output';
     VariableAddress := @FOutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinBandwidth:
   with Properties^ do
    begin
     Name            := 'Bandwidth';
     VariableAddress := @FBandwidth;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     Result          := True;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEBarkVocoderModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEBarkVocoderPins(CurrentPin.PinID) of
  pinInput,
  pinCarrier   : ChooseProcess;
  pinBandwidth : FVocoder.SynthesisBandwidth := FBandwidth * sqrt(0.5);
 end;
end;


{ TSEBarkVocoderStaticModule }

// describe your module
class procedure TSEBarkVocoderStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Bark Third Octave Vocoder (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Bark Vocoder (static)';
  end;
end;

// describe the pins (plugs)
function TSEBarkVocoderStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEBarkVocoderPins(index) of
  pinAttack:
   with Properties^ do
    begin
     Name            := 'Attack [ms]';
     VariableAddress := @FAttack;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.1';
     Result          := True;
    end;
  pinRelease:
   with Properties^ do
    begin
     Name            := 'Release [ms]';
     VariableAddress := @FRelease;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEBarkVocoderStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEBarkVocoderPins(CurrentPin.PinID) of
     pinAttack : FVocoder.Attack := FAttack;
    pinRelease : FVocoder.Release := FRelease;
 end;
end;


{ TSEBarkVocoderControllableModule }

class procedure TSEBarkVocoderControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Bark Third Octave Vocoder';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Bark Vocoder';
  end;
end;

function TSEBarkVocoderControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSEBarkVocoderPins(index) in [pinBandwidth..pinRelease]
  then with Properties^ do Direction := drIn;
end;

{ TSEBarkVocoderAutomatableModule }

class procedure TSEBarkVocoderAutomatableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Bark Third Octave Vocoder (automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Bark Vocoder (automatable)';
  end;
end;

function TSEBarkVocoderAutomatableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEBarkVocoderPins(index) of
  pinAttack:
   with Properties^ do
    begin
     Name            := 'Attack [ms]';
     VariableAddress := @FAttackBuffer;
     Direction       := drIn;
     Datatype        := dtFSample;
     DefaultValue    := '0.1';
     Result          := True;
    end;
  pinRelease:
   with Properties^ do
    begin
     Name            := 'Release [ms]';
     VariableAddress := @FReleaseBuffer;
     Direction       := drIn;
     Datatype        := dtFSample;
     DefaultValue    := '1';
     Result          := True;
    end;
 end;
end;

procedure TSEBarkVocoderAutomatableModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 case TSEBarkVocoderPins(CurrentPin.PinID) of
  pinAttack, pinRelease:
    if (Pin[Integer(pinAttack)].Status <> stRun) and
       (Pin[Integer(pinRelease)].Status <> stRun)
     then OnProcess := SubProcess
     else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TSEBarkVocoderAutomatableModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Inp     : PDAVSingleFixedArray;
  Carrier : PDAVSingleFixedArray;
  Outp    : PDAVSingleFixedArray;
  Att     : PDAVSingleFixedArray;
  Rel     : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp     := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp    := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Carrier := PDAVSingleFixedArray(@FCarrierBuffer[BufferOffset]);
 Att     := PDAVSingleFixedArray(@FAttackBuffer[BufferOffset]);
 Rel     := PDAVSingleFixedArray(@FReleaseBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  with FVocoder do
   begin
    Attack  := Att^[Sample];
    Release := Rel^[Sample];
    Outp^[Sample] := ProcessSample(Inp^[Sample], Carrier^[Sample]);
   end;
end;

end.
