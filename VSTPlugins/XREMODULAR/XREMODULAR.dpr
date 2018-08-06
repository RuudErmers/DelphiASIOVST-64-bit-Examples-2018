{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library XREMODULAR;



uses
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  XSynthModule in '..\..\VSTShared\XSynthModule.pas' {VSTSSModule: TVSTModule},
  XSynthVoice in '..\XRESynth\XSynthVoice.pas',
  XMoogFilter in '..\..\VSTShared\XMoogFilter.pas',
  UXPluginBase in '..\..\VSTShared\UXPluginBase.pas',
  XOscillator in '..\..\VSTShared\XOscillator.pas',
  XSynthMainFrame in '..\..\VSTShared\XSynthMainFrame.pas' {SynthMainFrame},
  UKnobEditor in '..\..\Components\RMC\UKnobEditor.pas',
  URMCConstants in '..\..\Components\RMC\URMCConstants.pas',
  URMCControls in '..\..\Components\RMC\URMCControls.pas',
  URMCEmptyPanel in '..\..\Components\RMC\URMCEmptyPanel.pas',
  URMCShape in '..\..\Components\RMC\URMCShape.pas',
  URMCBaseControlPanel in '..\..\Components\RMC\URMCBaseControlPanel.pas',
  UVirtCC in '..\..\Common\UVirtCC.pas',
  UIXSynthModel in '..\XRESynth\UIXSynthModel.pas',
  XPluginREModular in 'XPluginREModular.pas',
  URMC7Segment in '..\..\Components\RMC\URMC7Segment.pas',
  URMCBitmaps in '..\..\Components\RMC\URMCBitmaps.pas',
  XEffectsBase in '..\..\VSTShared\XEffectsBase.pas',
  UDataLayer in '..\..\Common\UDataLayer.pas',
  UFormEffects in '..\..\VSTShared\UFormEffects.pas' {FormEffects},
  URMCVSTView in '..\..\RMCShared\URMCVSTView.pas',
  URMCSunriseControlPanel in '..\..\Components\RMC\URMCSunriseControlPanel.pas',
  URMCSunriseFrame in '..\..\RMCShared\RMCModularView\URMCSunriseFrame.pas' {RMCSunriseFrame},
  URMCSunriseLFO in '..\..\RMCShared\RMCModularView\URMCSunriseLFO.pas' {RMCSunriseLFOFrame},
  URMCSunriseOSC in '..\..\RMCShared\RMCModularView\URMCSunriseOSC.pas' {RMCSunriseOSCFrame},
  URMCSunrisePERF in '..\..\RMCShared\RMCModularView\URMCSunrisePERF.pas' {RMCSunrisePERFFrame},
  URMCSunriseVCA in '..\..\RMCShared\RMCModularView\URMCSunriseVCA.pas' {RMCSunriseVCAFrame},
  URMCModularView in '..\..\RMCShared\RMCModularView\URMCModularView.pas',
  XPluginFactory in 'XPluginFactory.pas',
  UIXPlugin in '..\..\VSTShared\UIXPlugin.pas',
  URMCSunriseVCF in '..\..\RMCShared\RMCModularView\URMCSunriseVCF.pas',
  XSynthModelBase in '..\XRESynth\XSynthModelBase.pas',
  UMidiEvent in '..\..\Common\UMidiEvent.pas',
  bassMidi in '..\..\Common\bassMidi.pas',
  USampleValue in '..\..\VSTShared\USampleValue.pas',
  XEffects64 in '..\..\VSTShared\XEffects64.pas',
  XEffects in '..\..\VSTShared\XEffects.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TVSTSSModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.

