object TunerDataModule: TTunerDataModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Tuner'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  numInputs = 1
  numOutputs = 0
  CurrentProgramName = 'Classic Tuning'
  IORatio = 1.000000000000000000
  UniqueID = 'Tuna'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Classic Tuning'
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
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
