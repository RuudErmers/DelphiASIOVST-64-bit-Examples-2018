program prjRomplerVSTVA;

uses
  Vcl.Forms,
  UDataLayer in '..\..\Common\UDataLayer.pas',
  UIXPlugin in '..\UIXPlugin.pas',
  UTickCount in '..\..\..\Common\UTickCount.pas',
  UViewASIOForm in '..\..\VSTShared\UViewASIOForm.pas' {ViewASIOForm},
  XPluginFactory in 'XPluginFactory.pas',
  XPluginRERompler in 'XPluginRERompler.pas',
  UWaveFormVoice in 'UWaveFormVoice.pas',
  UXPluginBase in '..\..\VSTShared\UXPluginBase.pas',
  URMCEmptyPanel in '..\..\Components\RMC\URMCEmptyPanel.pas',
  URMCControls in '..\..\Components\RMC\URMCControls.pas',
  UKnobEditor in '..\..\Components\RMC\UKnobEditor.pas',
  URMCBaseControlPanel in '..\..\Components\RMC\URMCBaseControlPanel.pas',
  URMCConstants in '..\..\Components\RMC\URMCConstants.pas',
  URMCShape in '..\..\Components\RMC\URMCShape.pas',
  URMCBitSlicer in '..\..\Components\RMC\URMCBitSlicer.pas',
  URMCBitmaps in '..\..\Components\RMC\URMCBitmaps.pas',
  URMC7Segment in '..\..\Components\RMC\URMC7Segment.pas',
  XEffects in '..\..\VSTShared\XEffects.pas',
  XMoogFilter in '..\..\VSTShared\XMoogFilter.pas',
  UFormEffects in '..\..\VSTShared\UFormEffects.pas' {FormEffects},
  UVirtCC in '..\..\Common\UVirtCC.pas',
  XSynthMainFrame in '..\..\VSTShared\XSynthMainFrame.pas' {SynthMainFrame},
  XSynthModule in '..\..\VSTShared\XSynthModule.pas' {VSTSSModule: TVSTModule},
  URMCVSTView in '..\..\RMCShared\URMCVSTView.pas',
  URMCRomplerView in '..\..\RMCShared\RMCRompler\URMCRomplerView.pas',
  URMCRomplerFrame in '..\..\RMCShared\RMCRompler\URMCRomplerFrame.pas' {RMCRomplerFrame},
  UMidiPortsCombo in '..\..\Common\UMidiPortsCombo.pas',
  UMidiEvent in '..\..\Common\UMidiEvent.pas',
  UMidiPorts in '..\..\Common\UMidiPorts.pas',
  UMidiNrpn in '..\..\Common\UMidiNrpn.pas',
  MidiBase in '..\..\Common\MidiBase.pas',
  UWaveformOscillator in '..\..\VSTShared\UWaveformOscillator.pas',
  URMCSunriseControlPanel in '..\..\Components\RMC\URMCSunriseControlPanel.pas',
  UWaveformSoundData in '..\..\VSTShared\UWaveformSoundData.pas',
  USoundDataSamplesMellotron in 'USoundDataSamplesMellotron.pas',
  URMCEffects in '..\..\VSTShared\URMCEffects.pas' {RMCEffectsFrame},
  bassMidi in '..\..\Common\bassMidi.pas',
  UWavePlayer in '..\..\VSTShared\UWavePlayer.pas',
  DAV_ChunkWaveBasic in '..\..\DelphiASIOVST-v1.4\Source\FileFormats\DAV_ChunkWaveBasic.pas',
  DAV_AudioFileWAV in '..\..\DelphiASIOVST-v1.4\Source\FileFormats\DAV_AudioFileWAV.pas',
  USampleValue in '..\..\VSTShared\USampleValue.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TViewASIOForm, ViewASIOForm);
  Application.CreateForm(TRMCEffectsFrame, RMCEffectsFrame);
  Application.Run;
end.
