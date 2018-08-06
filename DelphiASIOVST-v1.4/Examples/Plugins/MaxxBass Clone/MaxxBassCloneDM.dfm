object HarmonicBassModule: THarmonicBassModule
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Version = '1.0'
  EffectName = 'MaxxBass Clone'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'MaxxBass Full Reset'
  IORatio = 1.000000000000000000
  UniqueID = 'DMBC'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'MaxxBass Full Reset'
      VSTModule = Owner
    end
    item
      DisplayName = 'Ultralow Extender'
      VSTModule = Owner
    end
    item
      DisplayName = 'Light'
      VSTModule = Owner
    end
    item
      DisplayName = 'Medium'
      VSTModule = Owner
    end
    item
      DisplayName = 'Aggressive'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      Category = 'Crossover'
      DisplayName = 'Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 256.000000000000000000
      MaxInteger = 256
      Min = 32.000000000000000000
      MinInteger = 32
      ReportVST2Properties = True
      ShortLabel = 'Freq.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Dynamics'
      DisplayName = 'Ratio'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 4.000000000000000000
      MaxInteger = 4
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Ratio'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = ': 1'
      VSTModule = Owner
      OnParameterChange = ParameterRatioChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Dynamics'
      DisplayName = 'Response'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 30.000000000000000000
      MaxInteger = 30
      Min = 10.000000000000000000
      MinInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'Respons'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterResponseChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Harmonics'
      DisplayName = 'Highpass Select'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 2.000000000000000000
      MaxInteger = 2
      ReportVST2Properties = True
      ShortLabel = 'HP Sel.'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterHighpassSelectChange
      OnCustomParameterDisplay = ParameterHighpassDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Harmonics'
      DisplayName = 'Decay'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = -9.000000000000000000
      MaxInteger = -9
      Min = -24.000000000000000000
      MinInteger = -24
      ReportVST2Properties = True
      ShortLabel = 'Decay'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterDecayChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 101.000000000000000000
      Category = 'Mix'
      DisplayName = 'Input'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Input'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterInputChange
      OnCustomParameterDisplay = ParameterdBDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 101.000000000000000000
      Category = 'Mix'
      DisplayName = 'Original Bass'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 3.990524530410767000
      MaxInteger = 4
      ReportVST2Properties = True
      ShortLabel = 'OrgBass'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterOriginalBassChange
      OnCustomParameterDisplay = ParameterdBDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 101.000000000000000000
      Category = 'Mix'
      DisplayName = 'Maxx Bass'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 3.990524530410767000
      MaxInteger = 4
      ReportVST2Properties = True
      ShortLabel = 'MaxBass'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterMaxxbassChange
      OnCustomParameterDisplay = ParameterdBDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Control'
      DisplayName = 'Listen'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 2.000000000000000000
      MaxInteger = 2
      ReportVST2Properties = True
      ShortLabel = 'Listen'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterListenChange
      OnCustomParameterDisplay = ParameterListenDisplay
    end>
  ParameterCategories = <
    item
      DisplayName = 'Crossover'
      VSTModule = Owner
    end
    item
      DisplayName = 'Dynamics'
      VSTModule = Owner
    end
    item
      DisplayName = 'Harmonics'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mix'
      VSTModule = Owner
    end
    item
      DisplayName = 'Control'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnProcess64Replacing = VSTModuleProcessDoubleReplacing
  OnResume = VSTModuleResume
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
