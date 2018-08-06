object MIDIModule: TMIDIModule
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'MIDI Plugin'
  ProductName = 'DAV MIDI Examples'
  VendorName = 'Delphi ASIO & VST Project'
  CanDos = [vcdSendVstEvents, vcdSendVstMidiEvent, vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd1in2out, vcd2in1out, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'MIDI'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Transpose'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 24.000000000000000000
      MaxInteger = 0
      Min = -24.000000000000000000
      ShortLabel = 'Transpo'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = 'semitone'
      VSTModule = Owner
      OnParameterChange = ParamTransposeChange
    end>
  ParameterCategories = <>
  OnEditOpen = VSTModuleEditOpen
  OnProcessMidi = VSTModuleProcessMidi
  Left = 243
  Top = 103
  Height = 150
  Width = 215
end
