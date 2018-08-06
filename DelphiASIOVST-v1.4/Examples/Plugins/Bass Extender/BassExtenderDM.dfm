object BassExtenderModule: TBassExtenderModule
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Bass Extender'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'BASS'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Crossover Only'
      VSTModule = Owner
    end
    item
      DisplayName = 'Try this'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mid only'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 10000.000000000000000000
      Category = 'Frequency Splitter'
      DisplayName = 'Split Frequency'
      Flags = [ppfParameterUsesFloatStep]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'Freq'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 10.000000000000000000
      StepInteger = 10
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamFrequencyChange
      OnCustomParameterLabel = ParamFreqLabel
      OnCustomParameterDisplay = ParamFreqDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Frequency Splitter'
      DisplayName = 'Split Order'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep]
      LargeStepFloat = 4.000000000000000000
      LargeStepInteger = 4
      Max = 32.000000000000000000
      MaxInteger = 32
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Order'
      SmallStepFloat = 2.000000000000000000
      StepFloat = 2.000000000000000000
      StepInteger = 2
      VSTModule = Owner
      OnParameterChange = ParamOrderChange
      OnCustomParameterDisplay = ParamSplitOrderDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Frequency Divider'
      DisplayName = 'Divider'
      Flags = [ppfParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Divider'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamDividerChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Frequency Divider'
      DisplayName = 'Shape'
      Flags = [ppfParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Shape'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamShapeChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Compressor'
      DisplayName = 'Threshold'
      Flags = [ppfParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -60.000000000000000000
      MinInteger = -60
      ReportVST2Properties = True
      ShortLabel = 'Thres.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamThresholdChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Compressor'
      DisplayName = 'Ratio'
      Flags = [ppfParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Ratio'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamRatioChange
      OnCustomParameterDisplay = ParamRatioDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000000.000000000000000000
      Category = 'Compressor'
      DisplayName = 'Attack'
      Flags = [ppfParameterUsesFloatStep]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      Max = 1000000.000000000000000000
      MaxInteger = 1000000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Attack'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 10.000000000000000000
      Units = #181's'
      VSTModule = Owner
      OnParameterChange = ParamAttackChange
      OnCustomParameterLabel = ParamAttackLabel
      OnCustomParameterDisplay = ParamAttackDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 10000.000000000000000000
      Category = 'Compressor'
      DisplayName = 'Release'
      Flags = [ppfParameterUsesFloatStep]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      Max = 10000.000000000000000000
      MaxInteger = 10000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Release'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 10.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParamReleaseChange
      OnCustomParameterLabel = ParamReleaseLabel
      OnCustomParameterDisplay = ParamReleaseDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Compressor'
      DisplayName = 'Compression Mix'
      Flags = [ppfParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'CompMix'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamCompressionMixChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Misc.'
      DisplayName = 'Balance'
      Flags = [ppfParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Balance'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamBalanceChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Misc.'
      DisplayName = 'Mode'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 3.000000000000000000
      MaxInteger = 3
      ReportVST2Properties = True
      ShortLabel = 'Mode'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamModeChange
      OnCustomParameterDisplay = ParamModeDisplay
    end>
  ParameterCategories = <
    item
      DisplayName = 'Frequency Splitter'
      VSTModule = Owner
    end
    item
      DisplayName = 'Frequency Divider'
      VSTModule = Owner
    end
    item
      DisplayName = 'Compressor'
      VSTModule = Owner
    end
    item
      DisplayName = 'Misc.'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess32
  OnProcess32Replacing = VSTModuleProcess32
  OnProcess64Replacing = VSTModuleProcess64
  Height = 150
  Width = 215
end
