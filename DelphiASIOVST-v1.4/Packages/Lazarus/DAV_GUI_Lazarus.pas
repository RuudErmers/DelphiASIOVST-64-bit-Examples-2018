{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DAV_GUI_Lazarus; 

interface

uses
    DAV_GuiVUMeter, DAV_GuiCorrelationMeter, DAV_GuiADSRGraph, 
    DAV_GuiAudioDataDisplay, DAV_GuiBaseControl, DAV_GuiButton, 
    DAV_GuiDynamicWaveform, DAV_GuiLabel, DAV_GuiMidiKeys, 
    DAV_GuiMidiKeyZones, DAV_GuiPanel, DAV_GuiRegister, DAV_GuiSelectBox, 
    DAV_GuiStaticWaveform, DAV_GuiPngTypes, DAV_GuiPng, DAV_GuiPngChunks, 
    DAV_GuiPngClasses, DAV_GuiPngCoder, DAV_GuiPngDesign, DAV_GuiPngImageList, 
    DAV_GuiPngResourceStrings, DAV_GuiAudioDataDisplayAxis, 
    DAV_GuiAudioDataDisplayCursor, DAV_GuiBitmap, DAV_GuiBlend, 
    DAV_GuiBlendReference, DAV_GuiByteMap, DAV_GuiCommon, DAV_GuiCustomMap, 
    DAV_GuiDesign, DAV_GuiDial, DAV_GuiDialDesign, DAV_GuiDialRenderer, 
    DAV_GuiEQGraph, DAV_GuiEQSlide, DAV_GuiFileFormatGraphics, 
    DAV_GuiFileFormats, DAV_GuiFilters, DAV_GuiFixedPoint, DAV_GuiFont, 
    DAV_GuiFontList, DAV_GuiGraphXY, DAV_GuiGraphXYDesign, DAV_GuiInscription, 
    DAV_GuiLED, DAV_GuiLevelMeter, DAV_GuiMediaButton, DAV_GuiPaintBox, 
    DAV_GuiPixelMap, DAV_GuiPixelMapDesign, DAV_GuiShadow, DAV_GuiSlider, 
    DAV_GuiStitchedButton, DAV_GuiStitchedControls, DAV_GuiStitchedDial, 
    DAV_GuiStitchedDisplay, DAV_GuiStitchedPngList, DAV_GuiStitchedSwitch, 
    DAV_GuiVector, DAV_GuiVectorPixel, DAV_GuiVectorPixelCircle, 
    DAV_GuiVectorPixelLine, DAV_GuiInterface, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DAV_GuiRegister', @DAV_GuiRegister.Register); 
end; 

initialization
  RegisterPackage('DAV_GUI_Lazarus', @Register); 
end.
