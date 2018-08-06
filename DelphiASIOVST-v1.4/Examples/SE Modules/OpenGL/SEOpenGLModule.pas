unit SEOpenGLModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  TSEOpenGLModule = class(TSEModuleBase)
  protected
    FFileName : PAnsiChar;
    procedure Open; override;
  public
    function GetPinProperties(const Index: Integer; Properties : PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;

    procedure SubProcessSleep(const BufferOffset, SampleFrames: Integer);
  end;

implementation

procedure TSEOpenGLModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcessSleep;
end;

// The most important part, processing the audio
procedure TSEOpenGLModule.SubProcessSleep(const BufferOffset, SampleFrames: Integer);
begin
 CallHost(SEAudioMasterSleepMode);
end;

// describe your module
class procedure TSEOpenGLModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 // describe the plugin, this is the name the end-user will see.
 with Properties^ do
  begin
   Name := 'OpenGL Example';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'Synthedit OpenGL Example';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   Flags      := [];
   GuiFlags   := [gfControlView, gfStructureView];
   SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEOpenGLModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'File';
       VariableAddress := @FFileName;
       Direction       := drIn;
       Datatype        := dtText;
       Flags           := [iofFilename, iofUICommunication]; 
       DatatypeExtra   := ''; // file extension
       DefaultValue    := '';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

end.
