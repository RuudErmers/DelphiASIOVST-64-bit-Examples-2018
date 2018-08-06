unit SEGFXModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  TSEGFXBaseModuleClass = class of TSEGFXBaseModule;
  TSEGFXBaseModule = class(TSEModuleBase)
  protected
    FLock : Boolean;
    procedure Open; override;
    class function GetGfxName: string; virtual; abstract;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

  TSEGFXAmountModule = class(TSEGFXBaseModule)
  protected
    FAmount : Integer;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEGFXAddColorNoiseModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXAddMonoNoiseModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXContrastModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXEmbossModule = class(TSEGFXBaseModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXGrayScaleModule = class(TSEGFXBaseModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXHueModule = class(TSEGFXAmountModule)
  protected
    FDither : Boolean;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXInvertModule = class(TSEGFXBaseModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXFishEyeModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXLightnessModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXDarknessModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXSaturationModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXSplitBlurModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXSplitBlur2Module = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXGaussianBlurModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXMosaicModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXTwistModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXSplitlightModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXTileModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXSpotlightModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXTraceModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXSolarizeModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXPosterizeModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXBrushedMetalModule = class(TSEGFXAmountModule)
  protected
    FGradient : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXBrushedMetal2Module = class(TSEGFXBrushedMetalModule)
  public
    class function GetGfxName: string; override;
  end;

implementation

function TSEGFXBaseModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Lock';
       VariableAddress := @FLock;
       Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
       Direction       := drIn;
       Datatype        := dtBoolean;
       DefaultValue    := '0';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEGFXBaseModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEGFXBaseModule.SubProcess(const BufferOffset, SampleFrames: Integer);
begin
 CallHost(SEAudioMasterSleepMode);
end;

// describe your module
class procedure TSEGFXBaseModule.getModuleProperties(Properties : PSEModuleProperties);
var
  str : AnsiString;
begin
 // describe the plugin, this is the name the end-user will see.
 with Properties^ do
  begin
   str  := 'GFX ' + GetGfxName;
   Name := PAnsiChar(str);
   ID   := PAnsiChar(str);

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   Flags      := [];
   GuiFlags   := [gfControlView, gfStructureView];
   SdkVersion := CSeSdkVersion;
  end;
end;

{ TSEGFXAmountModule }

// describe the pins (plugs)
function TSEGFXAmountModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if Index = 1 then
  with Properties^ do
   begin
    Name            := 'Amount';
    VariableAddress := @FAmount;
    Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
    Direction       := drIn;
    Datatype        := dtEnum;
    DefaultValue    := '0';
    DatatypeExtra   := 'range 0,255';
    Result          := True;
   end;
end;

{ TSEGFXInvertModule }

class function TSEGFXInvertModule.GetGfxName: string;
begin
 Result := 'Invert';
end;

{ TSEGFXGrayScaleModule }

class function TSEGFXGrayScaleModule.GetGfxName: string;
begin
 Result := 'Grayscale';
end;

{ TSEGFXEmbossModule }

class function TSEGFXEmbossModule.GetGfxName: string;
begin
 Result := 'Emboss';
end;

{ TSEGFXAddColorNoiseModule }

class function TSEGFXAddColorNoiseModule.GetGfxName: string;
begin
 Result := 'Add Color Noise';
end;

{ TSEGFXAddMonoNoiseModule }

class function TSEGFXAddMonoNoiseModule.GetGfxName: string;
begin
 Result := 'Add Mono Noise';
end;

{ TSEGFXContrastModule }

class function TSEGFXContrastModule.GetGfxName: string;
begin
 Result := 'Contrast';
end;

{ TSEGFXHueModule }

function TSEGFXHueModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if Index = 2 then
  with Properties^ do
   begin
    Name            := 'Dither';
    VariableAddress := @FDither;
    Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
    Direction       := drIn;
    Datatype        := dtBoolean;
    Result          := True;
   end;
end;

class function TSEGFXHueModule.GetGfxName: string;
begin
 Result := 'Hue';
end;

{ TSEGFXFishEyeModule }

class function TSEGFXFishEyeModule.GetGfxName: string;
begin
 Result := 'Fish Eye';
end;

{ TSEGFXLightnessModule }

class function TSEGFXLightnessModule.GetGfxName: string;
begin
 Result := 'Lightness';
end;

{ TSEGFXDarknessModule }

class function TSEGFXDarknessModule.GetGfxName: string;
begin
 Result := 'Darkness';
end;

{ TSEGFXSaturationModule }

class function TSEGFXSaturationModule.GetGfxName: string;
begin
 Result := 'Saturation';
end;

{ TSEGFXSplitBlurModule }

class function TSEGFXSplitBlurModule.GetGfxName: string;
begin
 Result := 'Split Blur';
end;

{ TSEGFXSplitBlur2Module }

class function TSEGFXSplitBlur2Module.GetGfxName: string;
begin
 Result := 'Split Blur²';
end;

{ TSEGFXGaussianBlurModule }

class function TSEGFXGaussianBlurModule.GetGfxName: string;
begin
 Result := 'Gaussian Blur';
end;

{ TSEGFXMosaicModule }

class function TSEGFXMosaicModule.GetGfxName: string;
begin
 Result := 'Mosaic';
end;

{ TSEGFXTwistModule }

class function TSEGFXTwistModule.GetGfxName: string;
begin
 Result := 'Twist';
end;

{ TSEGFXSplitlightModule }

class function TSEGFXSplitlightModule.GetGfxName: string;
begin
 Result := 'Splitlight';
end;

{ TSEGFXTraceModule }

class function TSEGFXTraceModule.GetGfxName: string;
begin
 Result := 'Trace';
end;

{ TSEGFXSolarizeModule }

class function TSEGFXSolarizeModule.GetGfxName: string;
begin
 Result := 'Solarize';
end;

{ TSEGFXPosterizeModule }

class function TSEGFXPosterizeModule.GetGfxName: string;
begin
 Result := 'Posterize';
end;

{ TSEGFXTileModule }

class function TSEGFXTileModule.GetGfxName: string;
begin
 Result := 'Tile';
end;

{ TSEGFXSpotlightModule }

class function TSEGFXSpotlightModule.GetGfxName: string;
begin
 Result := 'Spotlight';
end;

{ TSEGFXBrushedMetalModule }

function TSEGFXBrushedMetalModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := inherited GetPinProperties(Index, Properties);
 if Index = 2 then
  with Properties^ do
   begin
    Name            := 'Gradient';
    VariableAddress := @FGradient;
    Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
    Direction       := drIn;
    Datatype        := dtSingle;
    Result          := True;
   end;
end;

class function TSEGFXBrushedMetalModule.GetGfxName: string;
begin
 Result := 'Brushed Metal';
end;

{ TSEGFXBrushedMetal2Module }

class function TSEGFXBrushedMetal2Module.GetGfxName: string;
begin
 Result := 'Brushed Metal²';
end;

end.
