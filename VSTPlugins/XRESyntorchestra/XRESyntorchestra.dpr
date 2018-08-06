{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library XRESyntorchestra;

uses
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  UDataLayer in '..\..\Common\UDataLayer.pas',
  UMidiEvent in '..\..\Common\UMidiEvent.pas',
  UIXPlugin in '..\UIXPlugin.pas',
  XPluginFactory in 'XPluginFactory.pas',
  XSynthModule in '..\..\VSTShared\XSynthModule.pas' {VSTSSModule: TVSTModule},
  USyntorchestraVoice in 'USyntorchestraVoice.pas',
  UXPluginBase in '..\..\VSTShared\UXPluginBase.pas',
  URMCVSTView in '..\..\RMCShared\URMCVSTView.pas',
  URMCShape in '..\..\Components\RMC\URMCShape.pas',
  URMCEmptyPanel in '..\..\Components\RMC\URMCEmptyPanel.pas',
  URMCControls in '..\..\Components\RMC\URMCControls.pas',
  URMCConstants in '..\..\Components\RMC\URMCConstants.pas',
  URMCBitmaps in '..\..\Components\RMC\URMCBitmaps.pas',
  URMCBaseControlPanel in '..\..\Components\RMC\URMCBaseControlPanel.pas',
  URMC7Segment in '..\..\Components\RMC\URMC7Segment.pas',
  UKnobEditor in '..\..\Components\RMC\UKnobEditor.pas',
  XMoogFilter in '..\..\VSTShared\XMoogFilter.pas',
  XEffectsBase in '..\..\VSTShared\XEffectsBase.pas',
  UFormEffects in '..\..\VSTShared\UFormEffects.pas' {FormEffects},
  UVirtCC in '..\..\Common\UVirtCC.pas',
  XSynthMainFrame in '..\..\VSTShared\XSynthMainFrame.pas' {SynthMainFrame},
  XPluginRESyntorchestra in 'XPluginRESyntorchestra.pas',
  URMCSyntorchestraView in '..\..\RMCShared\RMCSyntorchestra\URMCSyntorchestraView.pas',
  URMCSyntorchestraFrame in '..\..\RMCShared\RMCSyntorchestra\URMCSyntorchestraFrame.pas' {RMCSyntorchestraFrame},
  XOscillator in '..\..\VSTShared\XOscillator.pas',
  USoundDataWaveFormSyntorchestra in 'USoundDataWaveFormSyntorchestra.pas',
  USoundDataSamplesSyntorchestra in 'USoundDataSamplesSyntorchestra.pas',
  URMCEffects in '..\..\VSTShared\URMCEffects.pas' {RMCEffectsFrame},
  URMCSunriseControlPanel in '..\..\Components\RMC\URMCSunriseControlPanel.pas',
  UWaveformOscillator in '..\..\VSTShared\UWaveformOscillator.pas',
  UWaveformSoundData in '..\..\VSTShared\UWaveformSoundData.pas',
  bassMidi in '..\..\Common\bassMidi.pas',
  UWavePlayer in '..\..\VSTShared\UWavePlayer.pas',
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

