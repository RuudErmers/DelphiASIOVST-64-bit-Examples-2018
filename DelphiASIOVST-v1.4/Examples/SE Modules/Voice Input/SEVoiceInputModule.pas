unit SEVoiceInputModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspVoiceInput;

type
  // define some constants to make referencing in/outs clearer
  TSEVoiceInputPins = (pinInput, pinOutput, pinTracking, pinPitch, pinBreath,
    pinVoicedUnvoicedRatio, pinMaximumFrequency);

  TCustomSEVoiceInputModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray;
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FVoiceInput       : TVoiceInput;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TSEVoiceInputStaticModule = class(TCustomSEVoiceInputModule)
  private
    FBreath              : Single; 
    FMaximumFrequency    : Single;
    FPitch               : Single;
    FTracking            : TTrackingType;
    FVoicedUnvoicedRatio : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEVoiceInputControllableModule = class(TSEVoiceInputStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSEVoiceInputModule }

constructor TCustomSEVoiceInputModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FVoiceInput := TVoiceInput.Create
end;

destructor TCustomSEVoiceInputModule.Destroy;
begin
 FreeAndNil(FVoiceInput);
 inherited;
end;

procedure TCustomSEVoiceInputModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEVoiceInputModule.SampleRateChanged;
begin
 inherited;
 FVoiceInput.SampleRate := SampleRate;
end;

procedure TCustomSEVoiceInputModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEVoiceInputModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TCustomSEVoiceInputModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEVoiceInputModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEVoiceInputPins(index) of
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
  pinOutput:
   with Properties^ do
    begin
     Name            := 'Output';
     VariableAddress := @FOutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEVoiceInputModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEVoiceInputPins(CurrentPin.PinID) of
  pinInput: begin
             ChooseProcess;
             Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
            end;
 end;
end;


{ TSEVoiceInputStaticModule }

// describe your module
class procedure TSEVoiceInputStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Voice Input (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Voice Input (static)';
  end;
end;

procedure TSEVoiceInputStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FVoiceInput.ProcessSample(Inp^[Sample]);
end;

// describe the pins (plugs)
function TSEVoiceInputStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSEVoiceInputPins(index) of
  pinTracking:
   with Properties^ do
    begin
     Name            := 'Tracking';
     VariableAddress := @FTracking;
     Direction       := drParameter;
     Datatype        := dtEnum;
     DatatypeExtra   := 'Off, Free, Quantized';
     DefaultValue    := '2';
     Result          := True;
    end;
  pinPitch:
   with Properties^ do
    begin
     Name            := 'Pitch';
     VariableAddress := @FPitch;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '50';
     Result          := True;
    end;
  pinBreath:
   with Properties^ do
    begin
     Name            := 'Breath';
     VariableAddress := @FBreath;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '60';
     Result          := True;
    end;
  pinVoicedUnvoicedRatio:
   with Properties^ do
    begin
     Name            := 'VoicedUnvoicedRatio';
     VariableAddress := @FVoicedUnvoicedRatio;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '70';
     Result          := True;
    end;
  pinMaximumFrequency:
   with Properties^ do
    begin
     Name            := 'MaximumFrequency';
     VariableAddress := @FMaximumFrequency;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '60';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEVoiceInputStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEVoiceInputPins(CurrentPin.PinID) of
             pinTracking : FVoiceInput.Tracking := FTracking;
                pinPitch : FVoiceInput.Pitch := FPitch;
               pinBreath : FVoiceInput.Breath := FBreath;
  pinVoicedUnvoicedRatio : FVoiceInput.VoicedUnvoicedRatio := FVoicedUnvoicedRatio;
     pinMaximumFrequency : FVoiceInput.MaximumFrequency := FMaximumFrequency;
 end;
end;


{ TSEVoiceInputControllableModule }

class procedure TSEVoiceInputControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Voice Input';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Voice Input';
  end;
end;

function TSEVoiceInputControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSEVoiceInputPins(index) in [pinTracking..pinMaximumFrequency]
  then with Properties^ do Direction := drIn;
end;

end.
