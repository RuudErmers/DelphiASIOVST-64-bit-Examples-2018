object SoundTouchPitchShifterModule: TSoundTouchPitchShifterModule
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'SoundTouchPitchShifter'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'DDPS'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Semi Tones'
      LargeStepFloat = 2.000000000000000000
      Max = 12.000000000000000000
      MaxInteger = 12
      Min = -12.000000000000000000
      MinInteger = -12
      ShortLabel = 'SemiTon'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterPitchFactorChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnBlockSizeChange = VSTModuleBlockSizeChange
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
