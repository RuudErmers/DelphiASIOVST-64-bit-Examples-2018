unit SEStringToolsModule;

interface

uses
  DAV_Types, DAV_Complex, DAV_SECommon, DAV_SEModule, DAV_DspLfo;

type
  TSEStringToolsModule = class(TSEModuleBase)
  protected
    procedure Open; override;
  public
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  end;

  TSEConcatStringModule = class(TSEStringToolsModule)
  protected
    FText : array [0..2] of PAnsiChar;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TSEStringToolsModule }

procedure TSEStringToolsModule.Open;
begin
 inherited Open;

 OnProcess := SubProcessStatic;

 // 'transmit' new output status to next module 'downstream'
 Pin[0].TransmitStatusChange(SampleClock, stStatic);
 Pin[1].TransmitStatusChange(SampleClock, stStatic);
end;

// The most important part, processing the audio
procedure TSEStringToolsModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 // empty
end;


{ TSEConcatStringModule }

// describe your module
class procedure TSEConcatStringModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Concat Text';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Concat Text';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEConcatStringModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 0
  then FText[0] := PAnsiChar(StrPas(FText[1]) + StrPas(FText[2]) + #0);
 inherited;
end;

// describe the pins (plugs)
function TSEConcatStringModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Text 1';
       VariableAddress := @FText[1];
       Direction       := drIn;
       Datatype        := dtText;
       DefaultValue    := '';
      end;
  1: with Properties^ do
      begin
       Name            := 'Text 2';
       VariableAddress := @FText[2];
       Direction       := drIn;
       Datatype        := dtText;
       DefaultValue    := '';
      end;
  2: with Properties^ do
      begin
       Name            := 'Concatted Text';
       VariableAddress := @FText[0];
       Direction       := drOut;
       Datatype        := dtText;
       DefaultValue    := '';
      end;
  else Result := False;
 end;
end;

end.
