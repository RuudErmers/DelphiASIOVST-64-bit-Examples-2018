program prjCrumarVSTSA;

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
  URMC7Segment in '..\..\Components\RMC\URMC7Segment.pas',
  URMCBitmaps in '..\..\Components\RMC\URMCBitmaps.pas',
  UDataLayer in '..\..\Common\UDataLayer.pas',
  URMCVSTView in '..\..\RMCShared\URMCVSTView.pas',
  XPluginRECDS2 in 'XPluginRECDS2.pas',
  XEffectsBase in '..\..\VSTShared\XEffectsBase.pas',
  XMoogFilter in '..\..\VSTShared\XMoogFilter.pas',
  XOscillator in '..\..\VSTShared\XOscillator.pas',
  XSynthMainFrame in '..\..\VSTShared\XSynthMainFrame.pas' {SynthMainFrame},
  XSynthVoice in '..\XRESynth\XSynthVoice.pas',
  XSynthModelBase in '..\XRESynth\XSynthModelBase.pas',
  UIXSynthModel in '..\XRESynth\UIXSynthModel.pas',
  XSynthModule in '..\..\VSTShared\XSynthModule.pas' {VSTSSModule: TVSTModule},
  UFormEffects in '..\..\VSTShared\UFormEffects.pas' {FormEffects},
  UCrumarViewFrame in '..\..\RMCShared\RMCCrumarView\UCrumarViewFrame.pas' {CrumarViewFrame},
  URMCCrumarView in '..\..\RMCShared\RMCCrumarView\URMCCrumarView.pas',
  UIXPlugin in '..\..\VSTShared\UIXPlugin.pas',
  UViewASIOForm in '..\..\VSTShared\UViewASIOForm.pas' {ViewASIOForm},
  UXPluginBase in '..\..\VSTShared\UXPluginBase.pas',
  UMidiPortsCombo in '..\..\Common\UMidiPortsCombo.pas',
  USampleValue in '..\..\VSTShared\USampleValue.pas',
  XEffects64 in '..\..\VSTShared\XEffects64.pas',
  XEffects in '..\..\VSTShared\XEffects.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TViewASIOForm, ViewASIOForm);
  Application.Run;
end.
