object BarberpoleTunerDataModule: TBarberpoleTunerDataModule
  OldCreateOrder = True
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'BarberpoleTuner'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  numInputs = 1
  numOutputs = 1
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'BaTu'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Guitar String'
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'String'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterGuitarStringChange
      OnCustomParameterDisplay = ParameterNoteDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 424
  Top = 78
  Height = 150
  Width = 215
end
