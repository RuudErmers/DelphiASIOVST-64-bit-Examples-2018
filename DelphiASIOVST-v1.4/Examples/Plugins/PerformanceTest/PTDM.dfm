object PerformanceTestModule: TPerformanceTestModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsHasEditor]
  Version = '1.0'
  EffectName = 'Performance Tester'
  ProductName = 'Delphi ASIO & VST Project'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'DVPT'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <>
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcess64Replacing = VSTModuleProcessDoubleReplacing
  OnProcess32Replacing = VSTModuleProcess
  Left = 309
  Top = 202
  Height = 150
  Width = 215
end
