{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library XRERompler;

uses
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  XPluginRERompler in 'XPluginRERompler.pas',
  UDataLayer in '..\..\Common\UDataLayer.pas',
  UMidiEvent in '..\..\Common\UMidiEvent.pas',
  UIXPlugin in '..\UIXPlugin.pas',
  XPluginFactory in 'XPluginFactory.pas',
  XSynthModule in '..\..\VSTShared\XSynthModule.pas' {VSTSSModule: TVSTModule},
  UWaveFormVoice in 'UWaveFormVoice.pas',
  URMCRomplerView in '..\..\RMCShared\RMCRompler\URMCRomplerView.pas',
  URMCRomplerFrame in '..\..\RMCShared\RMCRompler\URMCRomplerFrame.pas' {RMCRomplerFrame},
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
  XEffects in '..\..\VSTShared\XEffects.pas',
  XEffects64 in '..\..\VSTShared\XEffects64.pas',
  UFormEffects in '..\..\VSTShared\UFormEffects.pas' {FormEffects},
  UVirtCC in '..\..\Common\UVirtCC.pas',
  XSynthMainFrame in '..\..\VSTShared\XSynthMainFrame.pas' {SynthMainFrame},
  UWaveformOscillator in '..\..\VSTShared\UWaveformOscillator.pas',
  URMCEffects in '..\..\VSTShared\URMCEffects.pas' {RMCEffectsFrame},
  URMCSunriseControlPanel in '..\..\Components\RMC\URMCSunriseControlPanel.pas',
  USoundDataSamplesMellotron in 'USoundDataSamplesMellotron.pas',
  UWaveformSoundData in '..\..\VSTShared\UWaveformSoundData.pas',
  bassMidi in '..\..\Common\bassMidi.pas',
  UWavePlayer in '..\..\VSTShared\UWavePlayer.pas',
  USampleValue in '..\..\VSTShared\USampleValue.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TVSTSSModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.

