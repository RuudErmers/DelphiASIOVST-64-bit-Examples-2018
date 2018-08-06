unit SETransposerModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSETransposerPins = (pinMidiIn, pinTranspose, pinMidiOut);

  TSETransposerModule = class(TSEModuleBase)
  private
    FTransposeAmount : ShortInt;
  protected
    procedure Open; override;
    procedure MidiData(AClock, AMidiMsg: Cardinal; PinID: Integer); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

constructor TSETransposerModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
end;

destructor TSETransposerModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 inherited;
end;

procedure TSETransposerModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSETransposerModule.SubProcess(const BufferOffset, SampleFrames: Integer);
begin
 CallHost(seaudioMasterSleepMode);
end;

// describe your module
class procedure TSETransposerModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'MIDI Transposer';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'Synthedit Transposer Example';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SdkVersion := CSeSdkVersion;
  end;
end;

procedure TSETransposerModule.MidiData(AClock, AMidiMsg: Cardinal;
  PinID: Integer);
var
  IsSystemMsg        : Boolean;
  Chan, Stat, b2, b3 : Integer;
begin
 // system messages are for timing info etc
 // they don't have a channel, just pass them right through
 IsSystemMsg := (AMidiMsg and $F0) = $F0;

  if not IsSystemMsg then
   begin
    // The first byte of a midi message contains the status (note on/off etc) and the channel
    chan := AMidiMsg and $0F;
    stat := AMidiMsg and $F0;

    // the next two bytes depend on the type of message
    b2 := (AMidiMsg shr 8) and $FF;
    b3 := (AMidiMsg shr 16) and $FF;

    case stat of
     $80, $90: begin
                b2 := b2 + FTransposeAmount; // byte 2 is the note number
                if (b2 > 127) then b2 := 127;
                if (b2 < 0) then b2 := 0;

                // re-assemble the MIDI message into an 32 bit integer
                AMidiMsg := stat + chan + (b2 shl 8) + (b3 shl 16);
               end;
    end;
  end;

 // send the MIDI
 Pin[Integer(pinMidiOut)].TransmitMIDI(AClock, AMidiMsg);

 inherited;
end;

// describe the pins (plugs)
function TSETransposerModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSETransposerPins(index) of
  // typical input plug (inputs are listed first)
  pinMidiIn: with Properties^ do
              begin
               Name            := 'MIDI In';
               Direction       := drIn;
               Datatype        := dtMIDI2;
              end;
  pinTranspose: with Properties^ do
                 begin
                  Name            := 'Transpose';
                  VariableAddress := @FTransposeAmount;
                  Direction       := drIn;
                  Datatype        := dtEnum;
                  DefaultValue    := '5';
                  DatatypeExtra   := 'range -128,128';
                 end;
  pinMidiOut: with Properties^ do
               begin
                Name            := 'MIDI Out';
                Direction       := drOut;
                Datatype        := dtMIDI2;
               end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

end.
