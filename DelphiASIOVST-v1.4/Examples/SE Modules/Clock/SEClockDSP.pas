unit SEClockDSP;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

const
  CDefaultTempo = 10; // 100 BPM

type
  // define some constants to make referencing in/outs clearer
  TSEClockPins = (pinClockOut, pinBarResetOut, pinTempoOut, pinTransportOut,
    pinPulseDivideIn);

  TSmartOutput = class
  private
    FOutputBuffer       : PDAVSingleFixedArray;
    FCurrentOutputValue : Single;
    FModule             : TSEModuleBase;
    FPinNumber          : Integer;
    FStaticOutputCount  : Integer;
  public
    constructor Create(AModule : TSEModuleBase; APlugNumber: Integer);
    destructor Destroy; override;
    function PointerAddress: PDAVArrayOfSingleFixedArray;
    procedure Process(BufferPosition, SampleFrames: Integer);
    procedure SetValue(SampleClock: Cardinal; Value: Single);
    function GetValue: Single;
//    procedure SetPin(SEPin *Pin){m_pin=Pin;};
  end;

  TSEClockModule = class(TSEModuleBase)
  private
    FBMPOut            : TSmartOutput;
    FClockOut          : TSmartOutput;
    FBarResetOut       : TSmartOutput;
    FTransportOut      : TSmartOutput;
    FError             : Single;
    FpulseCount        : Single;
    FpulseCountInt     : Integer;
    FMidiClock         : Integer;
    FQuarterNoteCount  : Integer;
    FQuartersPerBar    : Integer;
    FPulsesPerBeat     : ShortInt;
  protected  
    procedure Open; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  Math, DAV_VSTEffect;

constructor TSmartOutput.Create(AModule: TSEModuleBase; APlugNumber: Integer);
begin
 FCurrentOutputValue := 0;
 FOutputBuffer := nil;
 FModule := AModule;
 FPinNumber := APlugNumber;
 FStaticOutputCount := 200;
end;

destructor TSmartOutput.Destroy;
begin
 inherited;
end;

function TSmartOutput.GetValue: Single;
begin
 Result := FCurrentOutputValue;
end;

function TSmartOutput.PointerAddress: PDAVArrayOfSingleFixedArray;
begin
 Result := @FOutputBuffer;
end;

procedure TSmartOutput.process(BufferPosition, SampleFrames: Integer);
var
  sample : Integer;
begin
  if (FStaticOutputCount < 0) then exit;

  if (FStaticOutputCount < SampleFrames) // lazy as posible
   then SampleFrames := FStaticOutputCount;

  for sample := SampleFrames - 1 downto 0 do
   begin
    FOutputBuffer[BufferPosition + Sample] := FCurrentOutputValue;
   end;

 FStaticOutputCount := FStaticOutputCount - SampleFrames;
end;

procedure TSmartOutput.SetValue(SampleClock: Cardinal; Value: Single);
begin
 if FCurrentOutputValue <> Value then
  begin
   FStaticOutputCount := FModule.BlockSize;
   FCurrentOutputValue := Value;
   FModule.Pin[FPinNumber].TransmitStatusChange(SampleClock, stOneOff);
  end;
end;

{ TSEClockModule }

constructor TSEClockModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FpulseCount       := 0;
 FpulseCountInt    := 0;
 FMidiClock        := -1;
 FQuarterNoteCount := -1;
 FQuartersPerBar   := 4;
 FError            := 0;
 FBMPOut           := TSmartOutput.Create(Self, Integer(pinTempoOut));
 FClockOut         := TSmartOutput.Create(Self, Integer(pinClockOut));
 FBarResetOut      := TSmartOutput.Create(Self, Integer(pinBarResetOut));
 FTransportOut     := TSmartOutput.Create(Self, Integer(pinTransportOut));
end;

destructor TSEClockModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 inherited;
end;

procedure TSEClockModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
 FBMPOut.SetValue(SampleClock, CDefaultTempo);
end;

// The most important part, processing the audio
procedure TSEClockModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  SamplesPerBeat          : Single;
  ti                      : PVstTimeInfo;
  SamplesRemain           : Integer;
  BufferPosition          : Integer;
  to_do                   : Integer;
  SamplesAlreadyProcessed : Integer;
  CurSampleClock          : Cardinal;
  NewBarReset             : Single;
  SamplesToBeats          : Double;
  SamplesToClocks         : Double;
  NewTransportRunning     : Single; 
  ExtraSamples            : Double;
  HostTotalBeats          : Double;
  BeatFrac, HostBeatFrac  : Single;
  Correction, NewTempo    : Single;
begin
 ti := nil;
 SamplesPerBeat := 0;
 SamplesRemain := SampleFrames;
 BufferPosition := BufferOffset;

 while True do
  begin
   to_do := SamplesRemain;
   if (FPulseCountInt < SamplesRemain)
    then to_do := FPulseCountInt;

   FBMPOut.Process(BufferPosition, to_do);
   FClockOut.Process(BufferPosition, to_do);
   FBarResetOut.Process(BufferPosition, to_do);
   FTransportOut.Process(BufferPosition, to_do);

    SamplesRemain := SamplesRemain - to_do;
    FPulseCountInt := FPulseCountInt - to_do;

    if (SamplesRemain = 0) then exit;

    BufferPosition := to_do + BufferPosition;

    assert(FPulseCountInt = 0);
    assert(FPulseCount <= 0);

    begin
      SamplesAlreadyProcessed := sampleFrames - SamplesRemain;
      CurSampleClock := SampleClock + SamplesAlreadyProcessed;

      NewBarReset := 0;
      if FClockOut.GetValue = 0 then
       begin
        // keep track of position within beat (24 clock/beat)
        inc(FMidiClock);
        FMidiClock := FMidiClock mod FPulsesPerBeat;

        if (FMidiClock = 0) then // start of quarter note
         begin
          inc(FQuarterNoteCount);
          FQuarterNoteCount := FQuarterNoteCount mod FQuartersPerBar;
          if (FQuarterNoteCount = 0) // start of bar
           then NewBarReset := 0.5;
         end;

        FClockOut.SetValue(CurSampleClock, 0.5);
       end
      else FClockOut.SetValue(CurSampleClock, 0);

      FBarResetOut.SetValue(CurSampleClock, NewBarReset);

      if (ti = nil) then // only do this a max of once per call
       begin
        // add on time since block start
        ti := PVstTimeInfo(CallHost(seaudioMasterGetTime, 0, 1536, nil
        ));
        if (ti <> nil) then
         begin
          // calculate how many samples per beat/clock
          SamplesToBeats := ti.tempo / (sampleRate * 60.0); // converts secs to beats (quarter notes)
          SamplesToClocks := SamplesToBeats * FPulsesPerBeat;

          //int SamplesAlreadyProcessed = sampleFrames - SamplesRemain;//s;
          // half a midi clock (for on/off transition)
          SamplesPerBeat := 0.5 / SamplesToClocks;

          NewTransportRunning := 0;

          // lock to host if transport playing, else just freewheel
          if not (vtiTransportPlaying in ti.Flags) then
           begin
            NewTransportRunning := 1;

            ExtraSamples := BufferOffset + SamplesAlreadyProcessed;
            HostTotalBeats := ti.ppqPos + ExtraSamples * SamplesToBeats;
            FQuarterNoteCount := Round(HostTotalBeats);

            BeatFrac := FMidiClock;

            // this routine called twice per midi clock (leading edge/trailing edge)
            // on trailing edge, we're half way between midi clocks
            if FClockOut.GetValue = 0 //outval == 0.f )
             then BeatFrac := BeatFrac + 0.5;

            // add small fractional ammount due to pulse not always
            // falling on an exact sampleframe
            BeatFrac := BeatFrac - 0.5 * FPulseCount / SamplesPerBeat;

            // convert from MIDI clocks to beats
            BeatFrac := BeatFrac / FPulsesPerBeat;

            HostBeatFrac := (HostTotalBeats - floor(HostTotalBeats));
            
            // calc FError as fraction of beat
            FError := HostBeatFrac - BeatFrac;
            if FError < -0.5 then FError := FError + 1;
            if FError >  0.5 then FError := FError - 1;

            // convert FError back to samples
            Correction := FError * 2 * SamplesPerBeat * FPulsesPerBeat;

            // jump count a fraction to catch up with host
            FPulseCount := FPulseCount - Correction; //SamplesPerBeat * FError * 0.8f;
          end;

          NewTempo := ti.tempo * 0.1;

          // avoid divide-by-zero errors when host don't provide tempo info
          if NewTempo = 0
           then NewTempo := CDefaultTempo;

          // note: these are not sample-accurate. Being Polled, they always lag actual event.
          FBMPOut.SetValue      ( CurSampleClock, NewTempo  );
          FTransportOut.SetValue( CurSampleClock, NewTransportRunning );
        end;
      end;


      // reset pulse count
      FPulseCount := FPulseCount + SamplesPerBeat;

      // can't skip a pulse completely, that would lose sync
      if FPulseCount < 2
       then FPulseCount := 2;

      FPulseCountInt := ceil(FPulseCount);

      // pre-calculate pulse count at next clock
      FPulseCount := FPulseCount - FPulseCountInt;
    end;
  end;
end;

// describe your module
class procedure TSEClockModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'BPM Clock2';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'Synthedit BPM Clock (DAV)';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end
end;

// describe the pins (plugs)
function TSEClockModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEClockPins(index) of
  // typical input plug (inputs are listed first)
  pinClockOut:
   with properties^ do
    begin
     Name            := 'Pulse Out';
     VariableAddress := FClockOut.PointerAddress; // @OutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinBarResetOut:
   with properties^ do
    begin
     Name            := 'Bar Start';
     VariableAddress := FBarResetOut.PointerAddress; //@output4_buffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinTempoOut:
   with properties^ do
    begin
     Name            := 'Tempo Out';
     VariableAddress := FBMPOut.PointerAddress;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinTransportOut:
   with properties^ do
    begin
     Name            := 'Transport Run';
     VariableAddress := FTransportOut.PointerAddress; //&output3_buffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinPulseDivideIn:
   with properties^ do
    begin
     Name            := 'Pulse Divide';
     VariableAddress := @FPulsesPerBeat;
     DatatypeExtra   := '64=64,32=32,24=24,16=16,12=12,8=8,6=6,4=4,3=3,2=2,1=1';
     DefaultValue    := '4';
     Direction       := drIn;
     Datatype        := dtEnum;
    end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

end.
