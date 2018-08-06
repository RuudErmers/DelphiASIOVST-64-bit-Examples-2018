unit SELightweightAnalogRingModulatorModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspRingModulator;

type
  // define some constants to make referencing in/outs clearer
  TSELightweightAnalogRingModulatorPins = (pinInput, pinOutput, pinFrequency);

  TCustomSELightweightAnalogRingModulatorModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FAnalogRingModulator : TLightweightAnalogAutoRingModulator32;
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

  TSELightweightAnalogRingModulatorStaticModule = class(TCustomSELightweightAnalogRingModulatorModule)
  private
    FFrequency : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSELightweightAnalogRingModulatorControllableModule = class(TSELightweightAnalogRingModulatorStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSELightweightAnalogRingModulatorModule }

constructor TCustomSELightweightAnalogRingModulatorModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FAnalogRingModulator := TLightweightAnalogAutoRingModulator32.Create
end;

destructor TCustomSELightweightAnalogRingModulatorModule.Destroy;
begin
 FreeAndNil(FAnalogRingModulator);
 inherited;
end;

procedure TCustomSELightweightAnalogRingModulatorModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSELightweightAnalogRingModulatorModule.SampleRateChanged;
begin
 inherited;
 FAnalogRingModulator.SampleRate := SampleRate;
end;

procedure TCustomSELightweightAnalogRingModulatorModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSELightweightAnalogRingModulatorModule.ChooseProcess;
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
class procedure TCustomSELightweightAnalogRingModulatorModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSELightweightAnalogRingModulatorModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSELightweightAnalogRingModulatorPins(index) of
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
procedure TCustomSELightweightAnalogRingModulatorModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSELightweightAnalogRingModulatorPins(CurrentPin.PinID) of
  pinInput: begin
             ChooseProcess;
             Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
            end;
 end;
end;


{ TSELightweightAnalogRingModulatorStaticModule }

// describe your module
class procedure TSELightweightAnalogRingModulatorStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Analog Ring Modulator (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Analog Ring Modulator (static)';
  end;
end;

procedure TSELightweightAnalogRingModulatorStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FAnalogRingModulator.ProcessSample32(Inp^[Sample]);
end;

// describe the pins (plugs)
function TSELightweightAnalogRingModulatorStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSELightweightAnalogRingModulatorPins(index) of
  pinFrequency:
   with Properties^ do
    begin
     Name            := 'Frequency [Hz]';
     VariableAddress := @FFrequency;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '100';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSELightweightAnalogRingModulatorStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSELightweightAnalogRingModulatorPins(CurrentPin.PinID) of
  pinFrequency: FAnalogRingModulator.Frequency := FFrequency;
 end;
end;


{ TSELightweightAnalogRingModulatorControllableModule }

class procedure TSELightweightAnalogRingModulatorControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Lightweight Analog Ring Modulator';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Lightweight Analog Ring Modulator';
  end;
end;

function TSELightweightAnalogRingModulatorControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSELightweightAnalogRingModulatorPins(index) in [pinFrequency]
  then with Properties^ do Direction := drIn;
end;

end.
