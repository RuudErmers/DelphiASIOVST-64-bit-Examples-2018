library SEGFX;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  DAV_SEGUI,
  SEGFXModule in 'SEGFXModule.pas',
  SEGFXGUI in 'SEGFXGUI.pas';

{$E sem}
{$R *.res}

const
  SEGFXBaseModuleClasses : array [0..23] of TSEGFXBaseModuleClass = (
    TSEGFXAddColorNoiseModule, TSEGFXAddMonoNoiseModule,
    TSEGFXContrastModule, TSEGFXEmbossModule, TSEGFXGrayScaleModule,
    TSEGFXHueModule, TSEGFXInvertModule, TSEGFXFishEyeModule,
    TSEGFXLightnessModule, TSEGFXDarknessModule, TSEGFXSaturationModule,
    TSEGFXSplitBlurModule, TSEGFXSplitBlur2Module, TSEGFXGaussianBlurModule,
    TSEGFXMosaicModule, TSEGFXTwistModule, TSEGFXSplitlightModule,
    TSEGFXTileModule, TSEGFXSpotlightModule, TSEGFXTraceModule,
    TSEGFXSolarizeModule, TSEGFXPosterizeModule, TSEGFXBrushedMetalModule,
    TSEGFXBrushedMetal2Module);

  SEGFXBaseGuiClasses : array [0..23] of TSEGFXBaseGuiClass = (
    TSEGFXAddColorNoiseGui, TSEGFXAddMonoNoiseGui, TSEGFXContrastGui,
    TSEGFXEmbossGui, TSEGFXGrayScaleGui, TSEGFXHueGui, TSEGFXInvertGui,
    TSEGFXFishEyeGui, TSEGFXLightnessGui, TSEGFXDarknessGui,
    TSEGFXSaturationGui, TSEGFXSplitBlurGui, TSEGFXSplitBlur2Gui,
    TSEGFXGaussianBlurGui, TSEGFXMosaicGui, TSEGFXTwistGui,
    TSEGFXSplitlightGui, TSEGFXTileGui, TSEGFXSpotlightGui, TSEGFXTraceGui,
    TSEGFXSolarizeGui, TSEGFXPosterizeGui, TSEGFXBrushedMetalGui,
    TSEGFXBrushedMetal2Gui);

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 if (Index >= 0) and (Index < Length(SEGFXBaseModuleClasses))
  then SEGFXBaseModuleClasses[Index].GetModuleProperties(Properties)
  else Result := False;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase : TSEModuleBase;
  GUI          : TSEGUIBase;
begin
 Result := nil;
 if (Index >= 0) and (Index < Length(SEGFXBaseModuleClasses)) then
  begin
   if (ProcessType = 1) then
    begin
     SEModuleBase := SEGFXBaseModuleClasses[Index].Create(SEAudioMaster, Reserved);
     if Assigned(SEModuleBase)
      then Result := SEModuleBase.Effect;
    end;
  end;
 if (Index >= 0) and (Index < Length(SEGFXBaseModuleClasses)) then
  begin
   if (ProcessType = 2) then
    begin
     GUI := SEGFXBaseGuiClasses[Index].Create(TSEGuiCallback(SEAudioMaster), Reserved);    // nasty!
     if Assigned(GUI)
      then Result := GUI.SEGUIStructBase;
    end;
  end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.