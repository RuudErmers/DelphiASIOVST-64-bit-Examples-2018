unit SEConvertModule;

interface

uses
  DAV_Types, DAV_Complex, DAV_SECommon, DAV_SEModule;

type
  TSEConvertModule = class(TSEModuleBase)
  protected
    procedure Open; override;
  public
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  end;

  TSEFloatToIntModule = class(TSEConvertModule)
  protected
    FFloat   : Single;
    FInteger : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEIntToFloatModule = class(TSEConvertModule)
  protected
    FFloat   : Single;
    FInteger : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEDoubleToIntModule = class(TSEConvertModule)
  protected
    FDouble  : Double;
    FInteger : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEIntToDoubleModule = class(TSEConvertModule)
  protected
    FDouble  : Double;
    FInteger : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFloatToTextModule = class(TSEConvertModule)
  protected
    FFloat : Single;
    FText  : PAnsiChar;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEDoubleToTextModule = class(TSEConvertModule)
  protected
    FDouble : Double;
    FText   : PAnsiChar;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEIntToTextModule = class(TSEConvertModule)
  protected
    FInteger : Integer;
    FText    : PAnsiChar;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSETextToGuiTextModule = class(TSEConvertModule)
  protected
    FText    : PAnsiChar;
    FOutText : PAnsiChar;
    FGuiText : PAnsiChar;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEIntToGuiIntModule = class(TSEConvertModule)
  protected
    FInt    : PAnsiChar;
    FGuiInt : PAnsiChar;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFloatToGuiFloatModule = class(TSEConvertModule)
  protected
    FFloat    : PAnsiChar;
    FGuiFloat : PAnsiChar;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEDoubleToGuiDoubleModule = class(TSEConvertModule)
  protected
    FDouble    : PAnsiChar;
    FGuiDouble : PAnsiChar;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEBooleanToGuiBooleanModule = class(TSEConvertModule)
  protected
    FBoolean    : PAnsiChar;
    FGuiBoolean : PAnsiChar;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSESampleToFloatModule = class(TSEModuleBase)
  protected
    FSamples    : PDAVSingleFixedArray;
    FLastSample : Single;
    FFloat      : Single;
    FLower      : Single;
    FUpper      : Single;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure CalculateOutput; virtual;
    procedure Open; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

  TSESampleToDoubleModule = class(TSEModuleBase)
  protected
    FSamples    : PDAVSingleFixedArray;
    FLastSample : Double;
    FDouble     : Double;
    FLower      : Double;
    FUpper      : Double;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure CalculateOutput; virtual;
    procedure Open; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  SysUtils;

{ TSEConvertModule }

procedure TSEConvertModule.Open;
begin
 inherited Open;

 OnProcess := SubProcessStatic;

 // 'transmit' new output status to next module 'downstream'
 if Assigned(Pin[0])
  then Pin[0].TransmitStatusChange(SampleClock, stStatic);
 if Assigned(Pin[1])
  then Pin[1].TransmitStatusChange(SampleClock, stStatic);
end;

// The most important part, processing the audio
procedure TSEConvertModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 CallHost(SEAudioMasterSleepMode);
end;


{ TSEFloatToIntModule }

// describe your module
class procedure TSEFloatToIntModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Float To Integer';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Float To Integer';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEFloatToIntModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 0
  then FInteger := Round(FFloat);
 inherited;
end;

// describe the pins (plugs)
function TSEFloatToIntModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Float';
       VariableAddress := @FFloat;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '100';
      end;
  1: with Properties^ do
      begin
       Name            := 'Integer';
       VariableAddress := @FInteger;
       Direction       := drOut;
       Datatype        := dtInteger;
       DefaultValue    := '100';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TSEIntToFloatModule }

// describe your module
class procedure TSEIntToFloatModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Integer To Float';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Integer To Float';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEIntToFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 0
  then FFloat := FInteger;
 inherited;
end;

// describe the pins (plugs)
function TSEIntToFloatModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Integer';
       VariableAddress := @FInteger;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '100';
      end;
  1: with Properties^ do
      begin
       Name            := 'Float';
       VariableAddress := @FFloat;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '100';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;


{ TSEDoubleToIntModule }

// describe your module
class procedure TSEDoubleToIntModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Double To Integer';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Double To Integer';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEDoubleToIntModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 0
  then FInteger := Round(FDouble);
 inherited;
end;

// describe the pins (plugs)
function TSEDoubleToIntModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Double';
       VariableAddress := @FDouble;
       Direction       := drIn;
       Datatype        := dtDouble;
       DefaultValue    := '100';
      end;
  1: with Properties^ do
      begin
       Name            := 'Integer';
       VariableAddress := @FInteger;
       Direction       := drOut;
       Datatype        := dtInteger;
       DefaultValue    := '100';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TSEIntToDoubleModule }

// describe your module
class procedure TSEIntToDoubleModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Integer To Double';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Integer To Double';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEIntToDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 0
  then FDouble := FInteger;
 inherited;
end;

// describe the pins (plugs)
function TSEIntToDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Integer';
       VariableAddress := @FInteger;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '100';
      end;
  1: with Properties^ do
      begin
       Name            := 'Double';
       VariableAddress := @FDouble;
       Direction       := drOut;
       Datatype        := dtDouble;
       DefaultValue    := '100';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;


{ TSEFloatToTextModule }

// describe your module
class procedure TSEFloatToTextModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Float To Text';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Float To Text';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEFloatToTextModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 0
  then FText := PAnsiChar(AnsiString(FloatToStr(FFloat)) + #0);
 inherited;
end;

// describe the pins (plugs)
function TSEFloatToTextModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Float';
       VariableAddress := @FFloat;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '100';
      end;
  1: with Properties^ do
      begin
       Name            := 'Text';
       VariableAddress := @FText;
       Direction       := drOut;
       Datatype        := dtText;
       DefaultValue    := '100';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TSEFloatToTextModule }

// describe your module
class procedure TSEDoubleToTextModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Double To Text';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Double To Text';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEDoubleToTextModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 0
  then FText := PAnsiChar(AnsiString(FloatToStr(FDouble)) + #0);
 inherited;
end;

// describe the pins (plugs)
function TSEDoubleToTextModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Double';
       VariableAddress := @FDouble;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '100';
      end;
  1: with Properties^ do
      begin
       Name            := 'Text';
       VariableAddress := @FText;
       Direction       := drOut;
       Datatype        := dtText;
       DefaultValue    := '100';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TSEIntToTextModule }

// describe your module
class procedure TSEIntToTextModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Integer To Text';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Integer To Text';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEIntToTextModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 0
  then FText := PAnsiChar(AnsiString(IntToStr(FInteger)) + #0);
 inherited;
end;

// describe the pins (plugs)
function TSEIntToTextModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Integer';
       VariableAddress := @FInteger;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '100';
      end;
  1: with Properties^ do
      begin
       Name            := 'Text';
       VariableAddress := @FText;
       Direction       := drOut;
       Datatype        := dtText;
       DefaultValue    := '100';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TSETextToGuiTextModule }

// describe your module
class procedure TSETextToGuiTextModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Text To GUI Text';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Text To GUI Text';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSETextToGuiTextModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 0 then
  begin
   FOutText := PAnsiChar(FText + #0);
   Pin[1].TransmitStatusChange(SampleClock, stStatic);
  end;
 inherited;
end;

// describe the pins (plugs)
function TSETextToGuiTextModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  0: with Properties^ do
      begin
       Name            := 'Text';
       VariableAddress := @FText;
       Direction       := drIn;
       Datatype        := dtText;
       DefaultValue    := 'Text';
      end;
  1: with Properties^ do
      begin
       Name            := 'Text';
       VariableAddress := @FOutText;
       Flags           := [iofPatchStore, iofHidePin];
       Direction       := drOut;
       Datatype        := dtText;
       DefaultValue    := 'Text';
      end;
  2: with Properties^ do
      begin
       Name            := 'GUI Text';
       Flags           := [iofUICommunication, iofPatchStore, iofHidePin];
       Direction       := drIn;
       Datatype        := dtText;
       DefaultValue    := 'Text';
      end;
  3: with Properties^ do
      begin
       Name            := 'GUI Text';
       VariableAddress := @FGUIText;
       Flags           := [iofUICommunication];
       Direction       := drOut;
       Datatype        := dtText;
       DefaultValue    := 'Text';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TSEIntToGuiIntModule }

// describe your module
class procedure TSEIntToGuiIntModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Integer To GUI Integer';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Integer To GUI Integer';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEIntToGuiIntModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 0
  then FGUIInt := FInt;
 inherited;
end;

// describe the pins (plugs)
function TSEIntToGuiIntModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Integer';
       VariableAddress := @FInt;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '100';
      end;
  1: with Properties^ do
      begin
       Name            := 'GUI Integer';
       VariableAddress := @FGuiInt;
       Flags           := [iofUICommunication];
       Direction       := drOut;
       Datatype        := dtInteger;
       DefaultValue    := '100';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TSEFloatToGuiFloatModule }

// describe your module
class procedure TSEFloatToGuiFloatModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Float To GUI Float';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Float To GUI Float';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEFloatToGuiFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 0
  then FGUIFloat := FFloat;
 inherited;
end;

// describe the pins (plugs)
function TSEFloatToGuiFloatModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Float';
       VariableAddress := @FFloat;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '100';
      end;
  1: with Properties^ do
      begin
       Name            := 'GUI Float';
       VariableAddress := @FGuiFloat;
       Flags           := [iofUICommunication];
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '100';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TSEDoubleToGuiDoubleModule }

// describe your module
class procedure TSEDoubleToGuiDoubleModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Double To GUI Double';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Double To GUI Double';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEDoubleToGuiDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 0
  then FGUIDouble := FDouble;
 inherited;
end;

// describe the pins (plugs)
function TSEDoubleToGuiDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Double';
       VariableAddress := @FDouble;
       Direction       := drIn;
       Datatype        := dtDouble;
       DefaultValue    := '100';
      end;
  1: with Properties^ do
      begin
       Name            := 'GUI Double';
       VariableAddress := @FGuiDouble;
       Flags           := [iofUICommunication];
       Direction       := drOut;
       Datatype        := dtDouble;
       DefaultValue    := '100';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TSEBooleanToGuiBooleanModule }

// describe your module
class procedure TSEBooleanToGuiBooleanModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Boolean To GUI Boolean';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Boolean To GUI Boolean';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEBooleanToGuiBooleanModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 0
  then FGUIBoolean := FBoolean;
 inherited;
end;

// describe the pins (plugs)
function TSEBooleanToGuiBooleanModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Boolean';
       VariableAddress := @FBoolean;
       Direction       := drIn;
       Datatype        := dtBoolean;
      end;
  1: with Properties^ do
      begin
       Name            := 'GUI Boolean';
       VariableAddress := @FGuiBoolean;
       Flags           := [iofUICommunication];
       Direction       := drOut;
       Datatype        := dtBoolean;
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TSESampleToFloatModule }

procedure TSESampleToFloatModule.Open;
begin
 inherited Open;

 OnProcess := SubProcess;

 if Assigned(Pin[1])
  then Pin[1].TransmitStatusChange(SampleClock, stStatic);
end;

class procedure TSESampleToFloatModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Volt To Float';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'Volt To Float';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

function TSESampleToFloatModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Volt';
       VariableAddress := @FSamples;
       Flags           := [iofLinearInput];
       Direction       := drIn;
       Datatype        := dtFSample;
      end;
  1: with Properties^ do
      begin
       Name            := 'Single';
       VariableAddress := @FFloat;
       Direction       := drOut;
       Datatype        := dtSingle;
      end;
  2: with Properties^ do
      begin
       Name            := 'Upper';
       VariableAddress := @FUpper;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  3: with Properties^ do
      begin
       Name            := 'Lower';
       VariableAddress := @FLower;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSESampleToFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  0: begin
      FFloat := FSamples^[0];
      CalculateOutput;
     end;
  2..3: CalculateOutput;
 end;
 inherited;
end;

procedure TSESampleToFloatModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
begin
 if FLastSample <> FSamples^[0] then
  begin
   FLastSample := FSamples^[0];
   CalculateOutput;
  end;
end;

procedure TSESampleToFloatModule.CalculateOutput;
begin
 FFloat := FLower + FLastSample * (FUpper - FLower);
 Pin[1].TransmitStatusChange(SampleClock, stOneOff);
end;


{ TSESampleToDoubleModule }

procedure TSESampleToDoubleModule.Open;
begin
 inherited Open;

 OnProcess := SubProcess;

 if Assigned(Pin[1])
  then Pin[1].TransmitStatusChange(SampleClock, stStatic);
end;

class procedure TSESampleToDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Volt To Double';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'Volt To Double';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

function TSESampleToDoubleModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Volt';
       VariableAddress := @FSamples;
       Flags           := [iofLinearInput];
       Direction       := drIn;
       Datatype        := dtFSample;
      end;
  1: with Properties^ do
      begin
       Name            := 'Single';
       VariableAddress := @FDouble;
       Direction       := drOut;
       Datatype        := dtSingle;
      end;
  2: with Properties^ do
      begin
       Name            := 'Upper';
       VariableAddress := @FUpper;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  3: with Properties^ do
      begin
       Name            := 'Lower';
       VariableAddress := @FLower;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSESampleToDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  0: begin
      FDouble := FSamples^[0];
      CalculateOutput;
     end;
  2..3: CalculateOutput;
 end;
 inherited;
end;

procedure TSESampleToDoubleModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
begin
 if FLastSample <> FSamples^[0] then
  begin
   FLastSample := FSamples^[0];
   CalculateOutput;
  end;
end;

procedure TSESampleToDoubleModule.CalculateOutput;
begin
 FDouble := FLower + FLastSample * (FUpper - FLower);
 Pin[1].TransmitStatusChange(SampleClock, stOneOff);
end;

end.
