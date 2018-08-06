unit SEToolsModule;

interface

uses
  DAV_Types, DAV_Complex, DAV_SECommon, DAV_SEModule, DAV_DspLFO;

type
  TSEToolsModuleClass = class of TSEToolsModule;
  TSEToolsModule = class(TSEModuleBase)
  protected
    procedure Open; override;
  public
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;
  end;

  TSELimitFloatModule = class(TSEToolsModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    FLower    : Single;
    FUpper    : Single;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSELimitDoubleModule = class(TSEToolsModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    FLower     : Double;
    FUpper     : Double;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSELimitIntegerModule = class(TSEToolsModule)
  protected
    FIntIn  : Integer;
    FIntOut : Integer;
    FLower  : Integer;
    FUpper  : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEIsPowerOf2Module = class(TSEToolsModule)
  protected
    FIntIn      : Integer;
    FIsPowerOf2 : Boolean;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSERoundToPowerOf2Module = class(TSEToolsModule)
  protected
    FIntIn    : Integer;
    FPowerOf2 : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSETruncToPowerOf2Module = class(TSEToolsModule)
  protected
    FIntIn    : Integer;
    FPowerOf2 : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEExtendToPowerOf2Module = class(TSEToolsModule)
  protected
    FIntIn    : Integer;
    FPowerOf2 : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFlipIntegerBytesModule = class(TSEToolsModule)
  protected
    FIntIn  : Integer;
    FIntOut : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSESincSingleModule = class(TSEToolsModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSESincDoubleModule = class(TSEToolsModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSESigmoidSingleModule = class(TSEToolsModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSESigmoidDoubleModule = class(TSEToolsModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFractionalSingleModule = class(TSEToolsModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFractionalDoubleModule = class(TSEToolsModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEAbsoluteSingleModule = class(TSEToolsModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEAbsoluteDoubleModule = class(TSEToolsModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEModuloSingleModule = class(TSEToolsModule)
  protected
    FFloatIn  : Single;
    FFloatMod : Single;
    FFloatOut : Single;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEModuloDoubleModule = class(TSEToolsModule)
  protected
    FDoubleIn  : Double;
    FDoubleMod : Double;
    FDoubleOut : Double;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEMinimumIntModule = class(TSEToolsModule)
  protected
    FIntInA : Integer;
    FIntInB : Integer;
    FIntOut : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEMinimumSingleModule = class(TSEToolsModule)
  protected
    FFloatInA : Single;
    FFloatInB : Single;
    FFloatOut : Single;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEMinimumDoubleModule = class(TSEToolsModule)
  protected
    FDoubleInA : Double;
    FDoubleInB : Double;
    FDoubleOut : Double;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEMaximumIntModule = class(TSEToolsModule)
  protected
    FIntInA : Integer;
    FIntInB : Integer;
    FIntOut : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEMaximumSingleModule = class(TSEToolsModule)
  protected
    FFloatInA : Single;
    FFloatInB : Single;
    FFloatOut : Single;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEMaximumDoubleModule = class(TSEToolsModule)
  protected
    FDoubleInA : Double;
    FDoubleInB : Double;
    FDoubleOut : Double;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEGetSinCosSingleModule = class(TSEToolsModule)
  protected
    FFloatFrq : Single;
    FFloatSin : Single;
    FFloatCos : Single;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEGetSinCosDoubleModule = class(TSEToolsModule)
  protected
    FDoubleFrq : Double;
    FDoubleSin : Double;
    FDoubleCos : Double;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  Math, SysUtils, DAV_Common, DAV_Math;

{ TSEToolsModule }

class procedure TSEToolsModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEToolsModule.Open;
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
procedure TSEToolsModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 CallHost(SEAudioMasterSleepMode);
end;


{ TSELimitFloatModule }

// describe your module
class procedure TSELimitFloatModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Limit Float';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Limit Float';
  end;
end;

procedure TSELimitFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID in [0..2]
  then FFloatOut := Limit(FFloatIn, FLower, FUpper);
end;

// describe the pins (plugs)
function TSELimitFloatModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Float';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '10';
      end;
  1: with Properties^ do
      begin
       Name            := 'Lower';
       VariableAddress := @FLower;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  2: with Properties^ do
      begin
       Name            := 'Upper';
       VariableAddress := @FUpper;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '100';
      end;
  3: with Properties^ do
      begin
       Name            := 'Float';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '10';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;


{ TSELimitDoubleModule }

// describe your module
class procedure TSELimitDoubleModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Limit Double';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Limit Double';
  end;
end;

procedure TSELimitDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID in [0..2] then
  if FDoubleIn < FLower then FDoubleOut := FLower else
  if FDoubleIn > FUpper then FDoubleOut := FUpper
   else FDoubleOut := FDoubleIn;
end;

// describe the pins (plugs)
function TSELimitDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Double';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtDouble;
       DefaultValue    := '10';
      end;
  1: with Properties^ do
      begin
       Name            := 'Lower';
       VariableAddress := @FLower;
       Direction       := drIn;
       Datatype        := dtDouble;
       DefaultValue    := '0';
      end;
  2: with Properties^ do
      begin
       Name            := 'Upper';
       VariableAddress := @FUpper;
       Direction       := drIn;
       Datatype        := dtDouble;
       DefaultValue    := '100';
      end;
  3: with Properties^ do
      begin
       Name            := 'Double';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtDouble;
       DefaultValue    := '10';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;


{ TSELimitIntegerModule }

// describe your module
class procedure TSELimitIntegerModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Limit Integer';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Limit Integer';
  end;
end;

procedure TSELimitIntegerModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID in [0..2] then
  if FIntIn < FLower then FIntOut := FLower else
  if FIntIn > FUpper then FIntOut := FUpper
   else FIntOut := FIntIn;
end;

// describe the pins (plugs)
function TSELimitIntegerModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Integer';
       VariableAddress := @FIntIn;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '10';
      end;
  1: with Properties^ do
      begin
       Name            := 'Lower';
       VariableAddress := @FLower;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '0';
      end;
  2: with Properties^ do
      begin
       Name            := 'Upper';
       VariableAddress := @FUpper;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '100';
      end;
  3: with Properties^ do
      begin
       Name            := 'Integer';
       VariableAddress := @FIntOut;
       Direction       := drOut;
       Datatype        := dtInteger;
       DefaultValue    := '10';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TSEIsPowerOf2Module }

class procedure TSEIsPowerOf2Module.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Is Power of 2';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Is Power of 2';
  end;
end;

function TSEIsPowerOf2Module.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Integer';
       VariableAddress := @FIntIn;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '16';
      end;
  1: with Properties^ do
      begin
       Name            := 'IsPowerOf2';
       VariableAddress := @FIsPowerOf2;
       Direction       := drOut;
       Datatype        := dtBoolean;
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEIsPowerOf2Module.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FIsPowerOf2 := IsPowerOf2(FIntIn);
end;


{ TSERoundToPowerOf2Module }

class procedure TSERoundToPowerOf2Module.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Round to Power of 2';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Round to Power of 2';
  end;
end;

function TSERoundToPowerOf2Module.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FIntIn;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '16';
      end;
  1: with Properties^ do
      begin
       Name            := 'Rounded';
       VariableAddress := @FPowerOf2;
       Direction       := drOut;
       Datatype        := dtInteger;
       DefaultValue    := '16';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSERoundToPowerOf2Module.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FPowerOf2 := RoundToPowerOf2(FIntIn);
end;


{ TSETruncToPowerOf2Module }

class procedure TSETruncToPowerOf2Module.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Truncate to Power of 2';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Truncate to Power of 2';
  end;
end;

function TSETruncToPowerOf2Module.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FIntIn;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '16';
      end;
  1: with Properties^ do
      begin
       Name            := 'Truncated';
       VariableAddress := @FPowerOf2;
       Direction       := drOut;
       Datatype        := dtInteger;
       DefaultValue    := '16';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSETruncToPowerOf2Module.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FPowerOf2 := TruncToPowerOf2(FIntIn);
end;


{ TSEExtendToPowerOf2Module }

class procedure TSEExtendToPowerOf2Module.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Extend to Power of 2';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Extend to Power of 2';
  end;
end;

function TSEExtendToPowerOf2Module.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FIntIn;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '16';
      end;
  1: with Properties^ do
      begin
       Name            := 'Extended';
       VariableAddress := @FPowerOf2;
       Direction       := drOut;
       Datatype        := dtInteger;
       DefaultValue    := '16';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEExtendToPowerOf2Module.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FPowerOf2 := ExtendToPowerOf2(FIntIn);
end;

{ TSEFlipIntegerBytesModule }

class procedure TSEFlipIntegerBytesModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Flip Integer Bytes';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Flip Integer Bytes';
  end;
end;

function TSEFlipIntegerBytesModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FIntIn;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FIntOut;
       Direction       := drOut;
       Datatype        := dtInteger;
       DefaultValue    := '0';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFlipIntegerBytesModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FIntOut := Swap32(FIntIn);
end;


{ TSESincSingleModule }

class procedure TSESincSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Sinc(x)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Sinc(x) Single';
  end;
end;

function TSESincSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSESincSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Sinc(FFloatIn);
end;


{ TSESincDoubleModule }

class procedure TSESincDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Sinc(x)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Sinc(x) Double';
  end;
end;

function TSESincDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSESincDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Sinc(FDoubleIn);
end;


{ TSESigmoidSingleModule }

class procedure TSESigmoidSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Sigmoid(x)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Sigmoid(x) Single';
  end;
end;

function TSESigmoidSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSESigmoidSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Sigmoid(FFloatIn);
end;


{ TSESigmoidDoubleModule }

class procedure TSESigmoidDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Sigmoid(x)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Sigmoid(x) Double';
  end;
end;

function TSESigmoidDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSESigmoidDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Sigmoid(FDoubleIn);
end;


{ TSEFractionalSingleModule }

class procedure TSEFractionalSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fractional(x)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fractional(x) Single';
  end;
end;

function TSEFractionalSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFractionalSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Frac(FFloatIn);
end;


{ TSEFractionalDoubleModule }

class procedure TSEFractionalDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fractional(x)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fractional(x) Double';
  end;
end;

function TSEFractionalDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFractionalDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Frac(FDoubleIn);
end;

{ TSEAbsoluteSingleModule }

class procedure TSEAbsoluteSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Absolute(x)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Absolute(x) Single';
  end;
end;

function TSEAbsoluteSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEAbsoluteSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := abs(FFloatIn);
end;


{ TSEAbsoluteDoubleModule }

class procedure TSEAbsoluteDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Absolute(x)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Absolute(x) Double';
  end;
end;

function TSEAbsoluteDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEAbsoluteDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := abs(FDoubleIn);
end;


{ TSEModuloSingleModule }

class procedure TSEModuloSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Modulo(x)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Modulo(x) Single';
  end;
end;

function TSEModuloSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Mod';
       VariableAddress := @FFloatMod;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  2: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEModuloSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := FastMod(FFloatIn, FFloatMod);
end;


{ TSEModuloDoubleModule }

class procedure TSEModuloDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Modulo(x)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Modulo(x) Double';
  end;
end;

function TSEModuloDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Mod';
       VariableAddress := @FDoubleMod;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  2: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEModuloDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := FastMod(FDoubleIn, FDoubleMod);
end;


{ TSEMinimumIntModule }

class procedure TSEMinimumIntModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Minimum';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Minimum Integer';
  end;
end;

function TSEMinimumIntModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In A';
       VariableAddress := @FIntInA;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'In B';
       VariableAddress := @FIntInB;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '0';
      end;
  2: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FIntOut;
       Direction       := drOut;
       Datatype        := dtInteger;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEMinimumIntModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FIntOut := min(FIntInA, FIntInB);
end;


{ TSEMinimum SingleModule }

class procedure TSEMinimumSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Minimum';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Minimum Single';
  end;
end;

function TSEMinimumSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In A';
       VariableAddress := @FFloatInA;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'In B';
       VariableAddress := @FFloatInB;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  2: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEMinimumSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := min(FFloatInA, FFloatInB);
end;


{ TSEMinimumDoubleModule }

class procedure TSEMinimumDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Minimum';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Minimum Double';
  end;
end;

function TSEMinimumDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In A';
       VariableAddress := @FDoubleInA;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'In B';
       VariableAddress := @FDoubleInB;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  2: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEMinimumDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := min(FDoubleInA, FDoubleInB);
end;


{ TSEMaximumIntModule }

class procedure TSEMaximumIntModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Maximum';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Maximum Integer';
  end;
end;

function TSEMaximumIntModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In A';
       VariableAddress := @FIntInA;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'In B';
       VariableAddress := @FIntInB;
       Direction       := drIn;
       Datatype        := dtInteger;
       DefaultValue    := '0';
      end;
  2: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FIntOut;
       Direction       := drOut;
       Datatype        := dtInteger;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEMaximumIntModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FIntOut := max(FIntInA, FIntInB);
end;


{ TSEMaximumSingleModule }

class procedure TSEMaximumSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Maximum';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Maximum Single';
  end;
end;

function TSEMaximumSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In A';
       VariableAddress := @FFloatInA;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'In B';
       VariableAddress := @FFloatInB;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  2: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEMaximumSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := max(FFloatInA, FFloatInB);
end;


{ TSEMaximumDoubleModule }

class procedure TSEMaximumDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Maximum';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Maximum Double';
  end;
end;

function TSEMaximumDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In A';
       VariableAddress := @FDoubleInA;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'In B';
       VariableAddress := @FDoubleInB;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  2: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEMaximumDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := max(FDoubleInA, FDoubleInB);
end;


{ TSEGetSinCosSingleModule }

class procedure TSEGetSinCosSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Calculate Sin/Cos';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Calculate Sin/Cos Single';
  end;
end;

function TSEGetSinCosSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Argument';
       VariableAddress := @FFloatFrq;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Sin';
       VariableAddress := @FFloatSin;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  2: with Properties^ do
      begin
       Name            := 'Cos';
       VariableAddress := @FFloatCos;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEGetSinCosSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then GetSinCos(FFloatFrq,  FFloatSin, FFloatCos);
end;


{ TSEGetSinCosDoubleModule }

class procedure TSEGetSinCosDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Calculate Sin/Cos';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Calculate Sin/Cos Double';
  end;
end;

function TSEGetSinCosDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Argument';
       VariableAddress := @FDoubleFrq;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Sin';
       VariableAddress := @FDoubleSin;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  2: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleCos;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEGetSinCosDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then GetSinCos(FDoubleFrq,  FDoubleSin, FDoubleCos);
end;

end.
