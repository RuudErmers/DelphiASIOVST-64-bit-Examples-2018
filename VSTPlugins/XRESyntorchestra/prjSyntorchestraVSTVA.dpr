program prjSyntorchestraVSTVA;

uses
  Vcl.Forms,
  UDataLayer in '..\..\Common\UDataLayer.pas',
  UIXPlugin in '..\UIXPlugin.pas',
  UViewASIOForm in '..\..\VSTShared\UViewASIOForm.pas' {ViewASIOForm},
  XPluginFactory in 'XPluginFactory.pas',
  XPluginRESyntorchestra in 'XPluginRESyntorchestra.pas',
  USyntorchestraVoice in 'USyntorchestraVoice.pas',
  UXPluginBase in '..\..\VSTShared\UXPluginBase.pas',
  URMCEmptyPanel in '..\..\Components\RMC\URMCEmptyPanel.pas',
  URMCControls in '..\..\Components\RMC\URMCControls.pas',
  UKnobEditor in '..\..\Components\RMC\UKnobEditor.pas',
  URMCBaseControlPanel in '..\..\Components\RMC\URMCBaseControlPanel.pas',
  URMCConstants in '..\..\Components\RMC\URMCConstants.pas',
  URMCShape in '..\..\Components\RMC\URMCShape.pas',
  URMCBitmaps in '..\..\Components\RMC\URMCBitmaps.pas',
  URMC7Segment in '..\..\Components\RMC\URMC7Segment.pas',
  XEffectsBase in '..\..\VSTShared\XEffectsBase.pas',
  XMoogFilter in '..\..\VSTShared\XMoogFilter.pas',
  UFormEffects in '..\..\VSTShared\UFormEffects.pas' {FormEffects},
  UVirtCC in '..\..\Common\UVirtCC.pas',
  XSynthMainFrame in '..\..\VSTShared\XSynthMainFrame.pas' {SynthMainFrame},
  XSynthModule in '..\..\VSTShared\XSynthModule.pas' {VSTSSModule: TVSTModule},
  URMCVSTView in '..\..\RMCShared\URMCVSTView.pas',
  UMidiPortsCombo in '..\..\Common\UMidiPortsCombo.pas',
  UMidiEvent in '..\..\Common\UMidiEvent.pas',
  UMidiPorts in '..\..\Common\UMidiPorts.pas',
  UMidiNrpn in '..\..\Common\UMidiNrpn.pas',
  MidiBase in '..\..\Common\MidiBase.pas',
  URMCSyntorchestraFrame in '..\..\RMCShared\RMCSyntorchestra\URMCSyntorchestraFrame.pas' {RMCSyntorchestraFrame},
  URMCSyntorchestraView in '..\..\RMCShared\RMCSyntorchestra\URMCSyntorchestraView.pas',
  XOscillator in '..\..\VSTShared\XOscillator.pas',
  URMCSunriseControlPanel in '..\..\Components\RMC\URMCSunriseControlPanel.pas',
  UWaveformOscillator in '..\..\VSTShared\UWaveformOscillator.pas',
  UWaveformSoundData in '..\..\VSTShared\UWaveformSoundData.pas',
  USoundDataSamplesSyntorchestra in 'USoundDataSamplesSyntorchestra.pas',
  USoundDataWaveFormSyntorchestra in 'USoundDataWaveFormSyntorchestra.pas',
  URMCEffects in '..\..\VSTShared\URMCEffects.pas' {RMCEffectsFrame},
  bassMidi in '..\..\Common\bassMidi.pas',
  UWavePlayer in '..\..\VSTShared\UWavePlayer.pas',
  USampleValue in '..\..\VSTShared\USampleValue.pas',
  XEffects64 in '..\..\VSTShared\XEffects64.pas',
  XEffects in '..\..\VSTShared\XEffects.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TViewASIOForm, ViewASIOForm);
  Application.CreateForm(TRMCEffectsFrame, RMCEffectsFrame);
  Application.Run;
end.
