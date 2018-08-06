program prjWavePlayerVSTSA;

uses
  Vcl.Forms,
  UDataLayer in '..\..\Common\UDataLayer.pas',
  UIXPlugin in '..\UIXPlugin.pas',
  UViewASIOForm in '..\..\VSTShared\UViewASIOForm.pas' {ViewASIOForm},
  XPluginFactory in 'XPluginFactory.pas',
  XPluginREWavePlayer in 'XPluginREWavePlayer.pas',
  UWavePlayer in '..\..\VSTShared\UWavePlayer.pas',
  UXPluginBase in '..\..\VSTShared\UXPluginBase.pas',
  URMCVSTView in '..\..\RMCShared\URMCVSTView.pas',
  XEffectsBase in '..\..\VSTShared\XEffectsBase.pas',
  UFormEffects in '..\..\VSTShared\UFormEffects.pas' {FormEffects},
  UVirtCC in '..\..\Common\UVirtCC.pas',
  XSynthMainFrame in '..\..\VSTShared\XSynthMainFrame.pas' {SynthMainFrame},
  XSynthModule in '..\..\VSTShared\XSynthModule.pas' {VSTSSModule: TVSTModule},
  URMCWavePlayerFrame in '..\..\RMCShared\RMCWavePlayerView\URMCWavePlayerFrame.pas' {RMCWavePlayerFrame},
  URMCWavePlayerView in '..\..\RMCShared\RMCWavePlayerView\URMCWavePlayerView.pas',
  UMidiPortsCombo in '..\..\Common\UMidiPortsCombo.pas',
  UMidiEvent in '..\..\Common\UMidiEvent.pas',
  UMidiNrpn in '..\..\Common\UMidiNrpn.pas',
  UMidiPorts in '..\..\Common\UMidiPorts.pas',
  bassMidi in '..\..\Common\bassMidi.pas',
  DAV_AudioFileWAV in '..\..\DelphiASIOVST-v1.4\Source\FileFormats\DAV_AudioFileWAV.pas',
  UWaveformSoundData in '..\..\VSTShared\UWaveformSoundData.pas',
  UWaveformOscillator in '..\..\VSTShared\UWaveformOscillator.pas',
  DAV_ChunkWaveBasic in '..\..\DelphiASIOVST-v1.4\Source\FileFormats\DAV_ChunkWaveBasic.pas',
  USampleValue in '..\..\VSTShared\USampleValue.pas',
  URMCShape in '..\..\Components\RMC\URMCShape.pas',
  URMCEmptyPanel in '..\..\Components\RMC\URMCEmptyPanel.pas',
  URMCControls in '..\..\Components\RMC\URMCControls.pas',
  URMCConstants in '..\..\Components\RMC\URMCConstants.pas',
  URMCBitmaps in '..\..\Components\RMC\URMCBitmaps.pas',
  URMCBaseControlPanel in '..\..\Components\RMC\URMCBaseControlPanel.pas',
  URMC7Segment in '..\..\Components\RMC\URMC7Segment.pas',
  UKnobEditor in '..\..\Components\RMC\UKnobEditor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TViewASIOForm, ViewASIOForm);
  Application.CreateForm(TRMCWavePlayerFrame, RMCWavePlayerFrame);
  Application.Run;
end.
