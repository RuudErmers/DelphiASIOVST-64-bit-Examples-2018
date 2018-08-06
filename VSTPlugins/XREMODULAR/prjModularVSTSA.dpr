program prjModularVSTSA;



uses
  Vcl.Forms,
  URMCEmptyPanel in '..\..\Components\RMC\URMCEmptyPanel.pas',
  URMCControls in '..\..\Components\RMC\URMCControls.pas',
  URMCConstants in '..\..\Components\RMC\URMCConstants.pas',
  URMCBaseControlPanel in '..\..\Components\RMC\URMCBaseControlPanel.pas',
  UKnobEditor in '..\..\Components\RMC\UKnobEditor.pas',
  URMCShape in '..\..\Components\RMC\URMCShape.pas',
  UVirtCC in '..\..\Common\UVirtCC.pas',
  UMidiPorts in '..\..\Common\UMidiPorts.pas',
  UMidiEvent in '..\..\Common\UMidiEvent.pas',
  UMidiNrpn in '..\..\Common\UMidiNrpn.pas',
  MidiBase in '..\..\Common\MidiBase.pas',
  URMCBitmaps in '..\..\Components\RMC\URMCBitmaps.pas',
  URMC7Segment in '..\..\Components\RMC\URMC7Segment.pas',
  DAV_DspPhaser in '..\..\DelphiASIOVST-v1.4\Source\DSP\DAV_DspPhaser.pas',
  UDataLayer in '..\..\Common\UDataLayer.pas',
  DAV_VSTHost in '..\..\DelphiASIOVST-v1.4\Source\VST\DAV_VSTHost.pas',
  URMCModularView in '..\..\RMCShared\RMCModularView\URMCModularView.pas',
  URMCSunriseControlPanel in '..\..\Components\RMC\URMCSunriseControlPanel.pas',
  URMCSunriseFrame in '..\..\RMCShared\RMCModularView\URMCSunriseFrame.pas' {RMCSunriseFrame},
  URMCSunriseLFO in '..\..\RMCShared\RMCModularView\URMCSunriseLFO.pas' {RMCSunriseLFOFrame},
  URMCSunriseOSC in '..\..\RMCShared\RMCModularView\URMCSunriseOSC.pas' {RMCSunriseOSCFrame},
  URMCSunrisePERF in '..\..\RMCShared\RMCModularView\URMCSunrisePERF.pas' {RMCSunrisePERFFrame},
  URMCSunriseVCA in '..\..\RMCShared\RMCModularView\URMCSunriseVCA.pas' {RMCSunriseVCAFrame},
  URMCVSTView in '..\..\RMCShared\URMCVSTView.pas',
  XPluginREModular in 'XPluginREModular.pas',
  UXPluginBase in '..\..\VSTShared\UXPluginBase.pas',
  XOscillator in '..\..\VSTShared\XOscillator.pas',
  XSynthVoice in '..\XRESynth\XSynthVoice.pas',
  XMoogFilter in '..\..\VSTShared\XMoogFilter.pas',
  UIXSynthModel in '..\XRESynth\UIXSynthModel.pas',
  XSynthModule in '..\..\VSTShared\XSynthModule.pas' {VSTSSModule: TVSTModule},
  XEffectsBase in '..\..\VSTShared\XEffectsBase.pas',
  UFormEffects in '..\..\VSTShared\UFormEffects.pas' {FormEffects},
  XSynthMainFrame in '..\..\VSTShared\XSynthMainFrame.pas' {SynthMainFrame},
  UIXPlugin in '..\..\VSTShared\UIXPlugin.pas',
  UViewASIOForm in '..\..\VSTShared\UViewASIOForm.pas' {ViewASIOForm},
  URMCSunriseVCF in '..\..\RMCShared\RMCModularView\URMCSunriseVCF.pas',
  XPluginFactory in 'XPluginFactory.pas',
  XSynthModelBase in '..\XRESynth\XSynthModelBase.pas',
  UMidiPortsCombo in '..\..\Common\UMidiPortsCombo.pas',
  bassMidi in '..\..\Common\bassMidi.pas',
  USampleValue in '..\..\VSTShared\USampleValue.pas',
  XEffects in '..\..\VSTShared\XEffects.pas',
  XEffects64 in '..\..\VSTShared\XEffects64.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TViewASIOForm, ViewASIOForm);
  Application.Run;
end.
