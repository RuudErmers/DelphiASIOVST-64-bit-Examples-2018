object SpectralNoiseGateModule: TSpectralNoiseGateModule
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Spectral Noise Gate'
  ProductName = 'DAV FFT Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  BlockSize = 512
  CurrentProgram = -1
  BlockModeSize = 512
  BlockModeOverlap = 256
  InitialDelay = 384
  IORatio = 1.000000000000000000
  UniqueID = 'Fsng'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Threshold'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -90.000000000000000000
      MinInteger = -90
      ShortLabel = 'Thresh.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterThresholdChange
    end
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
      ShortLabel = 'Order'
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
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'Ratio'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Ratio'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = ' : 1'
      VSTModule = Owner
      OnParameterChange = ParameterRatioChange
      OnCustomParameterDisplay = Parameter2DigitDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Knee'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'Knee'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterKneeChange
      OnCustomParameterDisplay = Parameter2DigitDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 10000.000000000000000000
      DisplayName = 'Attack'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 0.009999999776482582
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Attack'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterAttackChange
      OnCustomParameterDisplay = Parameter2DigitDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 600.000000000000000000
      DisplayName = 'Release'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 300.000000000000000000
      MaxInteger = 300
      Min = 0.500000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
      OnCustomParameterDisplay = Parameter2DigitDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 286
  Top = 77
  Height = 150
  Width = 215
end
