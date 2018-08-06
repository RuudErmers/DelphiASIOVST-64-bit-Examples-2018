object WavedisplayModule: TWavedisplayModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsHasVu, effFlagsCanMono, effFlagsCanReplacing]
  Version = '0.0'
  EffectName = 'Wavedisplay'
  ProductName = 'Wavedisplay'
  VendorName = 'ASIO-VST Delphi Project'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'mwdp'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <>
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  Left = 208
  Top = 110
  Height = 150
  Width = 215
end
