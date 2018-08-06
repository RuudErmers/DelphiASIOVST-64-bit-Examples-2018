object BassBaronModule: TBassBaronModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'Bass Baron'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Bass Baron Empty'
  IORatio = 1.000000000000000000
  UniqueID = 'DMBC'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Bass Baron Empty'
      VSTModule = Owner
    end
    item
      DisplayName = 'Big Baron'
      VSTModule = Owner
    end
    item
      DisplayName = 'Little Baron'
      VSTModule = Owner
    end
    item
      DisplayName = 'Classic'
      VSTModule = Owner
    end
    item
      DisplayName = 'M'#252'nchhausen'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      Category = 'I/O'
      DisplayName = 'Input Level'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 6
      Min = -24.000000000000000000
      ShortLabel = 'Input'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterInputLevelChange
      OnCustomParameterDisplay = ParameterLevelDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 10.000000000000000000
      Category = 'Crossover'
      DisplayName = 'Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 300.000000000000000000
      MaxInteger = 300
      Min = 30.000000000000000000
      MinInteger = 30
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
      Category = 'Crossover'
      DisplayName = 'FilterType'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 4.000000000000000000
      MaxInteger = 4
      ShortLabel = 'FilterT'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFilterTypeChange
      OnCustomParameterDisplay = ParameterFilterTypeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 75.000000000000000000
      Category = 'Crossover'
      DisplayName = 'Lowcut'
      LargeStepFloat = 2.000000000000000000
      Max = 300.000000000000000000
      MaxInteger = 300
      Min = 4.000000000000000000
      MinInteger = 4
      ShortLabel = 'Lowcut'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterLowcutChange
      OnCustomParameterLabel = ParameterLowcutLabel
      OnCustomParameterDisplay = ParameterLowcutDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Algorithm Type'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Algo'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterAlgorithmChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 999999.937500000000000000
      Category = 'Dynamics'
      DisplayName = 'Response'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 0.001000000047497451
      ReportVST2Properties = True
      ShortLabel = 'AddBass'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterResponseChange
      OnCustomParameterDisplay = ParameterResponseDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Mix'
      DisplayName = 'HF Level'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -50.000000000000000000
      MinInteger = -50
      ReportVST2Properties = True
      ShortLabel = 'HFLevel'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterHfLevelChange
      OnCustomParameterDisplay = ParameterLevelDisplay
    end
    item
      CurveFactor = 101.000000000000000000
      Category = 'Mix'
      DisplayName = 'Bass Mix'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'BassMix'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterBassMixChange
      OnCustomParameterDisplay = ParameterMixDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'I/O'
      DisplayName = 'Output Level'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 6
      Min = -24.000000000000000000
      ShortLabel = 'Output'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterOutputChange
      OnCustomParameterDisplay = ParameterLevelDisplay
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
    end
    item
      DisplayName = 'I/O'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess32
  OnProcess32Replacing = VSTModuleProcess32
  OnProcess64Replacing = VSTModuleProcess64
  OnResume = VSTModuleResume
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
