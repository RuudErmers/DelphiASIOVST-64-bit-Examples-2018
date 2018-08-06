unit SECorrelationMeterModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  TSECorrelationMeterModule = class(TSEModuleBase)
  private
    FDirection   : Integer;
    FCorrelation : Single;
  protected
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    function GetPinProperties(const Index: Integer; Properties : PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;

    procedure SubProcessSleep(const BufferOffset, SampleFrames: Integer);
  end;

implementation

procedure TSECorrelationMeterModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcessSleep;
end;

// The most important part, processing the audio
procedure TSECorrelationMeterModule.SubProcessSleep(const BufferOffset, SampleFrames: Integer);
begin
 CallHost(SEAudioMasterSleepMode);
end;

// describe your module
class procedure TSECorrelationMeterModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Correlation Meter';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'Synthedit Correlation Meter';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   Flags      := [];
   GuiFlags   := [gfControlView, gfStructureView];
   SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSECorrelationMeterModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  0 : with Properties^ do
       begin
        Name            := 'Direction';
        Direction       := drIn;
        Datatype        := dtEnum;
        DatatypeExtra   := 'Horizontal=0, Vertical=1';
        DefaultValue    := '0';
        Flags           := [iofUICommunication, iofPatchStore];
        VariableAddress := @FDirection;
       end;
  1 : with Properties^ do
       begin
        Name            := 'Correlation';
        Direction       := drIn;
        Datatype        := dtSingle;
        DefaultValue    := '0';
        Flags           := [iofUICommunication, iofPatchStore];
        VariableAddress := @FCorrelation;
       end;
  // typical input plug (inputs are listed first)
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TSECorrelationMeterModule.PlugStateChange(const CurrentPin: TSEPin);
begin
(*
 case Pin.PinID of
  0 :
 end;
*)
 inherited;
end;

end.
