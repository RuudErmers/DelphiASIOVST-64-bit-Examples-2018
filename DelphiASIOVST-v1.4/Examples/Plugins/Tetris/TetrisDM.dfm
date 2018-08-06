object TetrisModule: TTetrisModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsHasEditor]
  Version = '1.0'
  EffectName = 'Tetris'
  ProductName = 'DAV Tools Examples'
  VendorName = 'Delphi ASIO & VST Project'
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd1in2out, vcd2in1out, vcd2in2out, vcd2in4out, vcd4in2out, vcd4in4out, vcd4in8out, vcd8in4out, vcd8in8out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  KeysRequired = True
  IORatio = 1.000000000000000000
  UniqueID = 'Tetr'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <>
  ParameterCategories = <>
  OnEditorKeyDown = VSTModuleEditorKeyDown
  OnCheckKey = VSTModuleCheckKey
  Height = 150
  Width = 215
end
