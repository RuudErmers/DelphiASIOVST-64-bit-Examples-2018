object SpectralSelfFilterModule: TSpectralSelfFilterModule
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Version = '1.0'
  EffectName = 'Spectral Self Filtering'
  ProductName = 'DAV FFT Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  BlockSize = 2048
  CurrentProgram = -1
  BlockModeSize = 2048
  InitialDelay = 2048
  IORatio = 1.000000000000000000
  UniqueID = 'Fsng'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'FFT Order'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 14.000000000000000000
      MaxInteger = 14
      Min = 6.000000000000000000
      MinInteger = 6
      ReportVST2Properties = True
      ShortLabel = 'FFT Ord'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFftOrderChange
      OnCustomParameterDisplay = ParameterFftOrderDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Window Function'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Window'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterWindowFunctionChange
      OnCustomParameterDisplay = ParameterWindowFunctionDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  Left = 286
  Top = 77
  Height = 150
  Width = 215
end
