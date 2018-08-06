{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library XREWavePlayer;

uses
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  XPluginREWavePlayer in 'XPluginREWavePlayer.pas',
  UDataLayer in '..\..\Common\UDataLayer.pas',
  UMidiEvent in '..\..\Common\UMidiEvent.pas',
  UIXPlugin in '..\UIXPlugin.pas',
  XPluginFactory in 'XPluginFactory.pas',
  UXPluginBase in '..\..\VSTShared\UXPluginBase.pas',
  URMCVSTView in '..\..\RMCShared\URMCVSTView.pas',
  URMCEmptyPanel in '..\..\Components\RMC\URMCEmptyPanel.pas',
  URMCControls in '..\..\Components\RMC\URMCControls.pas',
  UKnobEditor in '..\..\Components\RMC\UKnobEditor.pas',
  URMCConstants in '..\..\Components\RMC\URMCConstants.pas',
  URMCBaseControlPanel in '..\..\Components\RMC\URMCBaseControlPanel.pas',
  URMCShape in '..\..\Components\RMC\URMCShape.pas',
  URMCBitmaps in '..\..\Components\RMC\URMCBitmaps.pas',
  URMC7Segment in '..\..\Components\RMC\URMC7Segment.pas',
  XEffectsBase in '..\..\VSTShared\XEffectsBase.pas',
  UVirtCC in '..\..\Common\UVirtCC.pas',
  URMCWavePlayerView in '..\..\RMCShared\RMCWavePlayerView\URMCWavePlayerView.pas',
  URMCWavePlayerFrame in '..\..\RMCShared\RMCWavePlayerView\URMCWavePlayerFrame.pas' {RMCWavePlayerFrame},
  XSynthModule in '..\..\VSTShared\XSynthModule.pas' {VSTSSModule: TVSTModule},
  XSynthMainFrame in '..\..\VSTShared\XSynthMainFrame.pas' {SynthMainFrame},
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

