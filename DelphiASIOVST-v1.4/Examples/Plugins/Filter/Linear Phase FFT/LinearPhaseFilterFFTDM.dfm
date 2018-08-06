object LinearPhaseFilterFFTDataModule: TLinearPhaseFilterFFTDataModule
  OldCreateOrder = True
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Version = '1.0'
  EffectName = 'LinearPhaseFilterFFT'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  BlockSize = 8192
  numInputs = 1
  CurrentProgramName = 'Default'
  BlockModeSize = 8192
  InitialDelay = 1024
  IORatio = 1.000000000000000000
  UniqueID = 'Conv'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'Frequency'
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ShortLabel = 'Frequen'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'FFT Order'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 16.000000000000000000
      MaxInteger = 16
      Min = 6.000000000000000000
      MinInteger = 6
      ShortLabel = 'Order'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFftOrderChange
      OnCustomParameterDisplay = ParameterfftOrderDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  Left = 191
  Top = 76
  Height = 150
  Width = 215
end
