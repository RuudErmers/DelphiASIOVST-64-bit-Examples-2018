unit SEHRTF3DModule;

interface

{$I DAV_Compiler.inc}

uses
  SysUtils, SyncObjs, DAV_Types, DAV_SECommon, DAV_SEModule, DAV_Complex,
  DAV_HalfFloat, DAV_DspConvolution, DAV_DspHrtf;

type
  // define some constants to make referencing in/outs clearer
  TSEHRTF3DPins = (pinInputL, pinInputR, pinOutputL, pinOutputR, pinFileName,
    pinAzimuth, pinPolar, pinMaxFFTOrder, pinDesiredLatency, pinRealLatency);

  TSEHRTF3DModule = class(TSEModuleBase)
  private
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FInputBuffer         : array [0..1] of PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer        : array [0..1] of PDAVSingleFixedArray;
    FFileName            : PAnsiChar;

    FHrir                : array [0..1] of PDAVSingleFixedArray;
    FConvolver           : array [0..1] of TLowLatencyConvolution32;

    FHRTF                : THrtfs;
    FHRTFLength          : Integer;
    FAzimuth, FPolar     : Single;

    FCriticalSection     : TCriticalSection;
    FStaticCount         : Integer;
    FMaxIRBlockOrder     : Integer;
    FRealLatency         : Integer;
    FDesiredLatencyIndex : Integer;
    procedure SampleRateChanged; override;
    procedure Open; override;
    procedure LoadCurrentHrirs; virtual;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

implementation

uses
  Classes;

resourcestring
  RCStrSynthEditOnly = 'This module is not allowed to be embedded into a VST Plugin';

constructor TSEHRTF3DModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FCriticalSection     := TCriticalSection.Create;
 FHRTF                := THrtfs.Create;
 FHRTFLength          := 512;
 FConvolver[0]        := TLowLatencyConvolution32.Create;
 FConvolver[1]        := TLowLatencyConvolution32.Create;
 FMaxIRBlockOrder     := FConvolver[0].MaximumIRBlockOrder;
 FRealLatency         := FConvolver[0].Latency;
 FDesiredLatencyIndex := 5;
 FFileName            := '';
end;

destructor TSEHRTF3DModule.Destroy;
begin
 FreeAndNil(FCriticalSection);
 FreeAndNil(FConvolver[0]);
 FreeAndNil(FConvolver[1]);
 FreeAndNil(FHRTF);
 inherited;
end;

procedure TSEHRTF3DModule.Open;
var
  RS : TResourceStream;
begin
 inherited Open;

 RS := TResourceStream.Create(HInstance, 'Default', 'HRTF');
 with RS do
  try
   FHRTF.LoadFromStream(RS);
 finally
   Free;
  end;

 GetMem(FHrir[0], FHRTFLength * SizeOf(Single));
 GetMem(FHrir[1], FHRTFLength * SizeOf(Single));
 FillChar(FHrir[0]^, FHRTFLength * SizeOf(Single), 0);
 FillChar(FHrir[1]^, FHRTFLength * SizeOf(Single), 0);
 LoadCurrentHrirs;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutputL)].TransmitStatusChange(SampleClock, stRun);
 Pin[Integer(pinOutputR)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEHRTF3DModule.SampleRateChanged;
begin
 inherited;
 FHRTF.SampleRate := SampleRate;
end;

// The most important part, processing the audio
procedure TSEHRTF3DModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSEHRTF3DModule.SubProcess(const BufferOffset, SampleFrames: Integer);
begin
 // lock processing
 FCriticalSection.Enter;
 try
  FConvolver[0].ProcessBlock(PDAVSingleFixedArray(@FInputBuffer[0][BufferOffset]),
                             PDAVSingleFixedArray(@FOutputBuffer[0][BufferOffset]),
                             SampleFrames);
  FConvolver[1].ProcessBlock(PDAVSingleFixedArray(@FInputBuffer[1][BufferOffset]),
                             PDAVSingleFixedArray(@FOutputBuffer[1][BufferOffset]),
                             SampleFrames);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSEHRTF3DModule.LoadCurrentHrirs;
begin
  FHRTF.InterpolateHrir(FAzimuth, FPolar, FHRTFLength, FHrir[0], FHrir[1]);
  FConvolver[0].LoadImpulseResponse(FHrir[0], FHRTFLength);
  FConvolver[1].LoadImpulseResponse(FHrir[1], FHRTFLength);
end;

procedure TSEHRTF3DModule.ChooseProcess;
begin
 if (Pin[Integer(pinInputL)].Status = stRun) and
    (Pin[Integer(pinInputR)].Status = stRun) 
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize + FHRTFLength + FConvolver[0].Latency;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TSEHRTF3DModule.GetModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   Name       := 'HRTF3D Module';
   ID         := 'DAV HRTF3D Module';
   About      := 'by Christian-W. Budde';
   SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEHRTF3DModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEHRTF3DPins(index) of
  pinInputL:
    with Properties^ do
     begin
      Name            := 'Input (left)';
      VariableAddress := @FInputBuffer[0];
      Flags           := [iofLinearInput];
      Direction       := drIn;
      Datatype        := dtFSample;
      DefaultValue    := '0';
     end;
  pinInputR:
    with Properties^ do
     begin
      Name            := 'Input (right)';
      VariableAddress := @FInputBuffer[1];
      Flags           := [iofLinearInput];
      Direction       := drIn;
      Datatype        := dtFSample;
      DefaultValue    := '0';
     end;
  pinOutputL:
    with Properties^ do
     begin
      Name            := 'Output (left)';
      VariableAddress := @FOutputBuffer[0];
      Direction       := drOut;
      Datatype        := dtFSample;
     end;
  pinOutputR:
    with Properties^ do
     begin
      Name            := 'Output (right)';
      VariableAddress := @FOutputBuffer[1];
      Direction       := drOut;
      Datatype        := dtFSample;
     end;
  pinFileName:
    with Properties^ do
     begin
      Name            := 'FileName';
      VariableAddress := @FFileName;
      Flags           := [iofFilename];
      Direction       := drIn;
      DataType        := dtText;
      DefaultValue    := '';
     end;
  pinAzimuth:
    with Properties^ do
     begin
      Name            := 'Azimuth [0..360°]';
      VariableAddress := @FAzimuth;
      Direction       := drIn;
      Datatype        := dtSingle;
     end;
  pinPolar:
    with Properties^ do
     begin
      Name            := 'Polar [0..180°]';
      VariableAddress := @FPolar;
      Direction       := drIn;
      Datatype        := dtSingle;
     end;
  pinMaxFFTOrder:
    with Properties^ do
     begin
      Name            := 'Maximum FFT Order';
      VariableAddress := @FMaxIRBlockOrder;
      Direction       := drIn;
      DataType        := dtEnum;
      DefaultValue    := '16';
      DatatypeExtra   := 'range 6,20';
     end;
  pinDesiredLatency:
    with Properties^ do
     begin
      Name            := 'Desired Latency';
      VariableAddress := @FDesiredLatencyIndex;
      Direction       := drParameter;
      DataType        := dtEnum;
      DefaultValue    := '2';
      DatatypeExtra   := '64, 128, 256, 512, 1024, 2048, 4096, 8192';
     end;
  pinRealLatency:
    with Properties^ do
     begin
      Name            := 'Real Latency';
      VariableAddress := @FRealLatency;
      Direction       := drOut;
      DataType        := dtInteger;
     end;
  else Result := False; // host will ask for plugs 0, 1, 2, 3 etc. return false to signal when done
 end;;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEHRTF3DModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSEHRTF3DPins(CurrentPin.PinID) of
             pinInputL : begin
                          ChooseProcess;
                          Pin[2].TransmitStatusChange(SampleClock, Pin[0].Status);
                         end;
             pinInputR : begin
                          ChooseProcess;
                          Pin[3].TransmitStatusChange(SampleClock, Pin[1].Status);
                         end;
        pinFileName : begin
                       FCriticalSection.Enter;
                       try
                        if FileExists(FFileName)
                         then FHRTF.LoadFromFile(StrPas(FFileName));
                       finally
                        FCriticalSection.Leave;
                       end;
                      end;
  pinPolar, pinAzimuth : LoadCurrentHrirs;
        pinMaxFFTOrder : begin
                          FCriticalSection.Enter;
                          try
                           if FMaxIRBlockOrder <= FConvolver[0].MinimumIRBlockOrder
                            then FMaxIRBlockOrder := FConvolver[0].MinimumIRBlockOrder + 1;
                           FConvolver[0].MaximumIRBlockOrder := FMaxIRBlockOrder;
                           FConvolver[1].MaximumIRBlockOrder := FMaxIRBlockOrder;
                          finally
                           FCriticalSection.Leave;
                          end;
                         end;
     pinDesiredLatency : begin
                          FCriticalSection.Enter;
                          try
                           FConvolver[0].MinimumIRBlockOrder := 5 + FDesiredLatencyIndex;
                           FConvolver[1].MinimumIRBlockOrder := 5 + FDesiredLatencyIndex;
                           FRealLatency := FConvolver[0].Latency;
                          finally
                           FCriticalSection.Leave;
                          end;
                         end;
 end;
 inherited;
end;


end.
