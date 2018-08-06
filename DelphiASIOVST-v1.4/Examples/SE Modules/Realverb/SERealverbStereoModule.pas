unit SERealverbStereoModule;

interface

{$I DAV_Compiler.inc}

uses
  SysUtils, SyncObjs, DAV_Types, DAV_SECommon, DAV_SEModule, DAV_Complex,
  DAV_HalfFloat, DAV_DspConvolution;

type
  // define some constants to make referencing in/outs clearer
  TSERealverbPins = (pinInputLeft, pinInputRight, pinOutputLeft,
    pinOutputRight, pinMaxFFTOrder, pinDesiredLatency, pinRealLatency);

  TSERealverbStereoModule = class(TSEModuleBase)
  private
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FInputBuffer         : array [0..1] of PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer        : array [0..1] of PDAVSingleFixedArray;
    FConvolver           : array [0..1] of TLowLatencyConvolution32;
    FCriticalSection     : TCriticalSection;
    FStaticCount         : Integer;
    FMaxIRBlockOrder     : Integer;
    FRealLatency         : Integer;
    FDesiredLatencyIndex : Integer;
    procedure SampleRateChanged; override;
    procedure Open; override;
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

constructor TSERealverbStereoModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FCriticalSection     := TCriticalSection.Create;
 FConvolver[0]        := TLowLatencyConvolution32.Create;
 FConvolver[1]        := TLowLatencyConvolution32.Create;
 FMaxIRBlockOrder     := FConvolver[0].MaximumIRBlockOrder;
 FRealLatency         := FConvolver[0].Latency;
 FDesiredLatencyIndex := 5;
end;

destructor TSERealverbStereoModule.Destroy;
begin
 FreeAndNil(FConvolver);
 FreeAndNil(FCriticalSection);
 inherited;
end;

procedure TSERealverbStereoModule.Open;
var
  i       : Integer;
  HFData  : array of THalfFloat;
  IRData  : TDAVSingleDynArray;
begin
 inherited Open;
 with TResourceStream.Create(HInstance, 'Left', 'RAW') do
  try
   SetLength(HFData, Size div SizeOf(THalfFloat));
   SetLength(IRData, Length(HFData));
   Read(HFData[0], Size);
   for i := 0 to Length(HFData) - 1
    do IRData[i] := HalfFloatToSingle(HFData[i]);
   FConvolver[0].LoadImpulseResponse(IRData);
  finally
   Free;
  end;

 with TResourceStream.Create(HInstance, 'Right', 'RAW') do
  try
   SetLength(HFData, Size div SizeOf(THalfFloat));
   SetLength(IRData, Length(HFData));
   Read(HFData[0], Size);
   for i := 0 to Length(HFData) - 1
    do IRData[i] := HalfFloatToSingle(HFData[i]);
   FConvolver[1].LoadImpulseResponse(IRData);
  finally
   Free;
  end;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutputLeft)].TransmitStatusChange(SampleClock, stRun);
 Pin[Integer(pinOutputRight)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSERealverbStereoModule.SampleRateChanged;
begin
 inherited;
 // ignore
end;

// The most important part, processing the audio
procedure TSERealverbStereoModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSERealverbStereoModule.SubProcess(const BufferOffset, SampleFrames: Integer);
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

procedure TSERealverbStereoModule.ChooseProcess;
var
  Status : TSEStateType;
begin
 Status := Pin[Integer(pinInputLeft)].Status;
 if Pin[Integer(pinInputRight)].Status > Status
  then Status := Pin[Integer(pinInputRight)].Status;
 if Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize + FConvolver[0].IRSize + FConvolver[0].Latency;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TSERealverbStereoModule.GetModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   Name       := 'Realverb Stereo Module';
   ID         := 'DAV Realverb Stereo Module';
   About      := 'by Christian-W. Budde';
   SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSERealverbStereoModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSERealverbPins(index) of
  pinInputLeft:
    with Properties^ do
     begin
      Name            := 'Input Left';
      VariableAddress := @FInputBuffer[0];
      Flags           := [iofLinearInput];
      Direction       := drIn;
      Datatype        := dtFSample;
      DefaultValue    := '0';
     end;
  pinInputRight:
    with Properties^ do
     begin
      Name            := 'Input Right';
      VariableAddress := @FInputBuffer[1];
      Flags           := [iofLinearInput];
      Direction       := drIn;
      Datatype        := dtFSample;
      DefaultValue    := '0';
     end;
  pinOutputLeft:
    with Properties^ do
     begin
      Name            := 'Output';
      VariableAddress := @FOutputBuffer[0];
      Direction       := drOut;
      Datatype        := dtFSample;
     end;
  pinOutputRight:
    with Properties^ do
     begin
      Name            := 'Output Right';
      VariableAddress := @FOutputBuffer[1];
      Direction       := drOut;
      Datatype        := dtFSample;
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
procedure TSERealverbStereoModule.PlugStateChange(const CurrentPin: TSEPin);
var
  Status : TSEStateType;
begin
 // has user altered a filter parameter?
 case TSERealverbPins(CurrentPin.PinID) of
      pinInputLeft,
      pinInputRight : begin
                       Status := Pin[0].Status;
                       if Pin[1].Status > Status then Status := Pin[1].Status;
                       ChooseProcess;
                       Pin[2].TransmitStatusChange(SampleClock, Status);
                       Pin[3].TransmitStatusChange(SampleClock, Status);
                      end;
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
 end; inherited;
end;


end.
