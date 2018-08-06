{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library XRECDS2D;

uses
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  XSynthModule in '..\..\VSTShared\XSynthModule.pas' {VSTSSModule: TVSTModule},
  XSynthVoice in '..\XRESynth\XSynthVoice.pas',
  UXPluginBase in '..\..\VSTShared\UXPluginBase.pas',
  XSynthMainFrame in '..\..\VSTShared\XSynthMainFrame.pas' {SynthMainFrame},
  UKnobEditor in '..\..\Components\RMC\UKnobEditor.pas',
  URMCConstants in '..\..\Components\RMC\URMCConstants.pas',
  URMCControls in '..\..\Components\RMC\URMCControls.pas',
  URMCEmptyPanel in '..\..\Components\RMC\URMCEmptyPanel.pas',
  URMCShape in '..\..\Components\RMC\URMCShape.pas',
  URMCBaseControlPanel in '..\..\Components\RMC\URMCBaseControlPanel.pas',
  UIXSynthModel in '..\XRESynth\UIXSynthModel.pas',
  XPluginRECDS2 in 'XPluginRECDS2.pas',
  URMCBitmaps in '..\..\Components\RMC\URMCBitmaps.pas',
  URMC7Segment in '..\..\Components\RMC\URMC7Segment.pas',
  XOscillator in '..\..\VSTShared\XOscillator.pas',
  XMoogFilter in '..\..\VSTShared\XMoogFilter.pas',
  XEffectsBase in '..\..\VSTShared\XEffectsBase.pas',
  UDataLayer in '..\..\Common\UDataLayer.pas',
  URMCCrumarView in '..\..\RMCShared\RMCCrumarView\URMCCrumarView.pas',
  URMCVSTView in '..\..\RMCShared\URMCVSTView.pas',
  UVirtCC in '..\..\Common\UVirtCC.pas',
  UCrumarViewFrame in '..\..\RMCShared\RMCCrumarView\UCrumarViewFrame.pas' {CrumarViewFrame},
  UFormEffects in '..\..\VSTShared\UFormEffects.pas' {FormEffects},
  XPluginFactory in 'XPluginFactory.pas',
  UIXPlugin in '..\..\VSTShared\UIXPlugin.pas',
  XSynthModelBase in '..\XRESynth\XSynthModelBase.pas',
  UMidiEvent in '..\..\Common\UMidiEvent.pas',
  USampleValue in '..\..\VSTShared\USampleValue.pas',
  XEffects in '..\..\VSTShared\XEffects.pas',
  XEffects64 in '..\..\VSTShared\XEffects64.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TVSTSSModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.

