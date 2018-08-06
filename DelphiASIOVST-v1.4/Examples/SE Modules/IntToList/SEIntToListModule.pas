unit SEIntToListModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  TSEIntToListModule = class(TSEModuleBase)
  protected
    procedure Open; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    function GetPinProperties(const Index: Integer; Properties : PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;
    procedure SubProcessSleep(const BufferOffset, SampleFrames: Integer);
  end;

implementation

constructor TSEIntToListModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
end;

procedure TSEIntToListModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcessSleep;
end;

procedure TSEIntToListModule.SubProcessSleep(const BufferOffset, SampleFrames: Integer);
begin
 CallHost(SEAudioMasterSleepMode);
end;

// describe the pins (plugs)
class procedure TSEIntToListModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Int To List';

   // return a unique string 32 characters max
   // include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'Synthedit Int To List';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   // only show in structure view
   GuiFlags := [gfStructureView];

   SdkVersion := CSeSdkVersion;
  end;
end;

function TSEIntToListModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       Direction       := drIn;
       Datatype        := dtInteger;
       Flags           := [iofUICommunication];
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       Direction       := drOut;
       Datatype        := dtEnum;
       Flags           := [iofUICommunication];
      end;
  2: with Properties^ do
      begin
       Name          := 'Mode';
       DatatypeExtra := 'Index,Value';
       Direction     := drIn;
       Datatype      := dtEnum;
       Flags         := [iofUICommunication];
      end
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

end.
