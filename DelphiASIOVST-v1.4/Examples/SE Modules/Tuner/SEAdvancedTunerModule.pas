unit SEAdvancedTunerModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspTuner;

type
  // define some constants to make referencing in/outs clearer
  TSETunerPins = (pinInput, pinMinimum, pinMaximum, pinSmooth,
    pinOneCrossingOnly, pinDSFilterOrder, pinDSBandwidth, pinAttack,
    pinRelease, pinThreshold, pinFrequency, pinNote, pinDetune);

  TCustomSEAdvancedTunerModule = class(TSEModuleBase)
  private
    FInputBuffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FFrequency   : PDAVSingleFixedArray;
    FNote        : AnsiString;
    FDetune      : Single;
    FStaticCount : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FTuner : TAdvancedTuner;
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

  TSEAdvancedTunerStaticModule = class(TCustomSEAdvancedTunerModule)
  private
    FMinimum         : Single;
    FMaximum         : Single;
    FSmoothFactor    : Single;
    FOneCrossingOnly : Boolean;
    FDSFilterOrder   : Integer;
    FDSBandwidth     : Single;
    FAttack          : Single;
    FRelease         : Single;
    FThreshold       : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEAdvancedTunerControllableModule = class(TSEAdvancedTunerStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSEAdvancedTunerModule }

constructor TCustomSEAdvancedTunerModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FTuner := TAdvancedTuner.Create;
end;

destructor TCustomSEAdvancedTunerModule.Destroy;
begin
 FreeAndNil(FTuner);
 inherited;
end;

procedure TCustomSEAdvancedTunerModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEAdvancedTunerModule.SampleRateChanged;
begin
 inherited;
 if SampleRate > 0
  then FTuner.SampleRate := SampleRate;
end;

procedure TCustomSEAdvancedTunerModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Inp, Freq : PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Freq := PDAVSingleFixedArray(@FFrequency[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FTuner.ProcessSample32(Inp^[Sample]);
   Freq^[Sample] := 0.1 * FTuner.CurrentFrequency;
  end;
 FNote := FTuner.CurrentNote;
 FDetune := FTuner.CurrentDetune;
end;

procedure TCustomSEAdvancedTunerModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEAdvancedTunerModule.ChooseProcess;
begin
 if (Pin[Integer(pinInput)].Status = stRun)
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TCustomSEAdvancedTunerModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEAdvancedTunerModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSETunerPins(index) of
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
  pinFrequency:
   with Properties^ do
    begin
     Name            := 'Frequency';
     VariableAddress := @FFrequency;
     Direction       := drOut;
     Datatype        := dtFSample;
     DefaultValue    := '0';
     Result          := True;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEAdvancedTunerModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSETunerPins(CurrentPin.PinID) of
  pinInput : begin
              Pin[Integer(pinFrequency)].TransmitStatusChange(SampleClock, Pin[0].Status);
              ChooseProcess;
             end;
 end;
end;


{ TSEAdvancedTunerStaticModule }

// describe your module
class procedure TSEAdvancedTunerStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Advanced Tuner (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Advanced Tuner (static)';
  end;
end;

// describe the pins (plugs)
function TSEAdvancedTunerStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 case TSETunerPins(index) of
  pinMinimum:
   with Properties^ do
    begin
     Name            := 'Minimum [Hz]';
     VariableAddress := @FMinimum;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '100';
     Result          := True;
    end;
  pinMaximum:
   with Properties^ do
    begin
     Name            := 'Maximum [Hz]';
     VariableAddress := @FMaximum;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '4000';
     Result          := True;
    end;
  pinSmooth:
   with Properties^ do
    begin
     Name            := 'Smooth [0..1]';
     VariableAddress := @FSmoothFactor;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0.99';
     Result          := True;
    end;
  pinOneCrossingOnly:
   with Properties^ do
    begin
     Name            := 'One Crossing Only';
     VariableAddress := @FOneCrossingOnly;
     Direction       := drParameter;
     Datatype        := dtBoolean;
     DefaultValue    := '1';
     Result          := True;
    end;
  pinDSFilterOrder:
   with Properties^ do
    begin
     Name            := 'Downsampling Filter Order';
     VariableAddress := @FDSFilterOrder;
     Direction       := drParameter;
     Datatype        := dtEnum;
     DefaultValue    := '4';
     DatatypeExtra   := 'range 2,32';
     Result          := True;
    end;
  pinDSBandwidth:
   with Properties^ do
    begin
     Name            := 'Downsampling Bandwidth [0..1]';
     VariableAddress := @FDSBandwidth;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0.8';
     Result          := True;
    end;
  pinAttack:
   with Properties^ do
    begin
     Name            := 'Attack [ms]';
     VariableAddress := @FAttack;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     Result          := True;
    end;
  pinRelease:
   with Properties^ do
    begin
     Name            := 'Release [ms]';
     VariableAddress := @FRelease;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '10';
     Result          := True;
    end;
  pinThreshold:
   with Properties^ do
    begin
     Name            := 'Threshold [-1..1]';
     VariableAddress := @FThreshold;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0';
     Result          := True;
    end;
  pinNote:
   with Properties^ do
    begin
     Name            := 'Note';
     VariableAddress := @FNote;
     Direction       := drOut;
     Datatype        := dtText;
     DefaultValue    := '';
     Result          := True;
    end;
  pinDetune:
   with Properties^ do
    begin
     Name            := 'Detune [Cent]';
     VariableAddress := @FDetune;
     Direction       := drOut;
     Datatype        := dtSingle;
     DefaultValue    := '0';
     Result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEAdvancedTunerStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSETunerPins(CurrentPin.PinID) of
  pinMinimum         : FTuner.MinimumFrequency := FMinimum;
  pinMaximum         : FTuner.MaximumFrequency := FMaximum;
  pinSmooth          : FTuner.SmoothFactor := FSmoothFactor;
  pinOneCrossingOnly : FTuner.OneCrossingOnly := FOneCrossingOnly;
  pinDSFilterOrder   : FTuner.DownSampleFilterOrder := FDSFilterOrder;
  pinDSBandwidth     : FTuner.DownSampleBandwidth := FDSBandwidth;
  pinAttack          : FTuner.Attack := FAttack;
  pinRelease         : FTuner.Release := FRelease;
 end;
end;


{ TSETunerControllableModule }

class procedure TSEAdvancedTunerControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Advanced Tuner';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Advanced Tuner';
  end;
end;

function TSEAdvancedTunerControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if TSETunerPins(index) in [pinMinimum..pinThreshold]
  then with Properties^ do Direction := drIn;
end;

end.
