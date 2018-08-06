object SpectralDelayFilterModule: TSpectralDelayFilterModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Version = '1.0'
  EffectName = 'Spectral Delay Filter'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'DSDF'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Tune'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Tune'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterTuneChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Order'
      LargeStepFloat = 2.000000000000000000
      Max = 1024.000000000000000000
      MaxInteger = 1024
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Order'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterOrderChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
