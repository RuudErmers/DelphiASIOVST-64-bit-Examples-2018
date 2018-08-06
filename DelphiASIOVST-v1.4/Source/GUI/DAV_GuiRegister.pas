unit DAV_GuiRegister;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, TypInfo,
  {$IFDEF FPC}
  LCLIntf, LazIDEIntf, PropEdits, ComponentEditors
  {$ELSE}
  {$IFDEF COMPILER6_UP}
  DesignIntf
  {$ELSE}
  DsgnIntf
  {$ENDIF}
  {$ENDIF};

procedure Register;

implementation

{$IFNDEF FPC}{$R ..\..\Resources\DAV_GuiRegister.res}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ELSE} DAV_GuiLevelMeter,
  (*DAV_GuiModular*) DAV_GuiBackgrounds, DAV_GuiEdit, {$ENDIF}
  DAV_GuiADSRGraph, DAV_GuiAudioDataDisplay, DAV_GuiButton, DAV_GuiCheckBox,
  DAV_GuiCorrelationMeter, DAV_GuiDial, DAV_GuiDialDesign, DAV_GuiDialRenderer,
  DAV_GuiDynamicWaveform, DAV_GuiEQGraph, DAV_GuiEQSlide, DAV_GuiFader,
  DAV_GuiFont, DAV_GuiFontList, DAV_GuiFontDesign, DAV_GuiGroup,
  DAV_GuiInscription, DAV_GuiImageControl, DAV_GuiImageList, DAV_GuiGraphXY,
  DAV_GuiGraphXYDesign, DAV_GuiLabel, DAV_GuiLED, DAV_GuiMediaButton,
  DAV_GuiMidiKeys, DAV_GuiRadioButton, DAV_GuiPaintBox, DAV_GuiPanel,
  DAV_GuiPixelMap, DAV_GuiPixelMapDesign, DAV_GuiPng, DAV_GuiPngList,
  DAV_GuiPngDesign, DAV_GuiSelectBox, DAV_GuiSlider, DAV_GuiStaticWaveform,
  DAV_GuiStitchedButton, DAV_GuiStitchedControls, DAV_GuiStitchedDial,
  DAV_GuiStitchedDisplay, DAV_GuiStitchedImageList, DAV_GuiStitchedRadioSwitch,
  DAV_GuiStitchedPngList, DAV_GuiStitchedSwitch, DAV_GuiVUMeter;

procedure Register;
begin
  RegisterComponents('ASIO/VST GUI', [
    TGuiADSRGraph, TGuiAudioDataDisplay, TGuiButton, TGuiControlsCheckBox,
    TGuiCorrelationMeter, TGuiDial, TGuiDialEx, TGuiDialImageList,
    TGuiDialImageRenderer, TGuiDialMetal, TGuiDynamicWaveform, TGuiEQGraph,
    TGuiEQSlide, TGuiFader, TGuiFontList, TGuiGraphXY, TGuiGroup, TGuiGroupTop,
    TGuiGroupSide, TGuiGroupSimple, TGuiInscription, TGuiIntegerBox,
    TGuiImageList, TGuiLabel, TGuiLED, TGuiMediaButton, TGuiMidiKeys,
    TGuiControlsRadioButton, TGuiPaintBox, TGuiPanel, TGuiPngList,
    TGuiSelectBox, TGuiSlider, TGuiStaticWaveform, TGuiSwitch, TGuiVUMeter
    {$IFNDEF FPC}, TGuiLevelMeter, TGuiColorLevelMeter, (*TGuiModular*)
    TGuiBackground, TGuiControlsEdit {$ENDIF}]);

  RegisterComponents('ASIO/VST GUI Stitched', [
    TGuiStitchedButton, TGuiStitchedDial, TGuiStitchedDisplay,
    TGuiStitchedImageList, TGuiStitchedPNGList, TGuiStitchedRadioSwitch,
    TGuiStitchedSwitch]);

  RegisterPropertyEditor(TypeInfo(TGuiCustomPixelMap), nil, '', TPixelMapProperty);
  RegisterPropertyEditor(TypeInfo(TPortableNetworkGraphicPixel32), nil, '', TPngProperty);

  RegisterPropertyEditor(TypeInfo(string), TGuiDialLayerCollectionItem, 'PrimitiveClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomGuiDialPrimitive), TGuiDialLayerCollectionItem, 'Primitive', TPrimitiveClassProperty);

  RegisterPropertyEditor(TypeInfo(string), TGuiGraphXYSeriesCollectionItem, 'SeriesClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomGuiGraphXYSeries), TGuiGraphXYSeriesCollectionItem, 'Series', TSeriesClassProperty);

  RegisterPropertyEditor(TypeInfo(string), TGuiCustomFontCollectionItem, 'FontClassName', nil);
  RegisterPropertyEditor(TypeInfo(TGuiCustomFont), TGuiCustomFontCollectionItem, 'Font', TFontClassProperty);
end;

{$IFDEF FPC}
initialization
  {$i ..\..\Resources\DAV_GuiRegister.lrs}
{$ENDIF}

end.
