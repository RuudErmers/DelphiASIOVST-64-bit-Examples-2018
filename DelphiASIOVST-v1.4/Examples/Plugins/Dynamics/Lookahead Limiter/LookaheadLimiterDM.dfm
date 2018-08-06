object LookaheadLimiterDataModule: TLookaheadLimiterDataModule
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0.2'
  EffectName = 'Lookahead Limiter'
  ProductName = 'DAV Dynamic Examples'
  VendorName = 'Delphi ASIO & VST Project'
  VersionRelease = 2
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  InitialDelay = 64
  IORatio = 1.000000000000000000
  UniqueID = 'DVLL'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Soft Compression'
      VSTModule = Owner
    end
    item
      DisplayName = 'Light Compression'
      VSTModule = Owner
    end
    item
      DisplayName = 'Moderate Compression'
      VSTModule = Owner
    end
    item
      DisplayName = 'Independent'
      VSTModule = Owner
    end
    item
      DisplayName = 'Pumpkin'
      VSTModule = Owner
    end
    item
      DisplayName = 'More Compression'
      VSTModule = Owner
    end
    item
      DisplayName = 'Heavy Compression'
      VSTModule = Owner
    end
    item
      DisplayName = 'Percussive'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      Category = 'I/O'
      DisplayName = 'Input'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 24.000000000000000000
      MaxInteger = 24
      Min = -6.000000000000000000
      MinInteger = -6
      ReportVST2Properties = True
      ShortLabel = 'Thresho'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterInputChange
      OnCustomParameterDisplay = ParameterInputDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'I/O'
      DisplayName = 'Output'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -24.000000000000000000
      MinInteger = -24
      ReportVST2Properties = True
      ShortLabel = 'Output'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterOutputChange
      OnCustomParameterDisplay = ParameterOutputDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'I/O'
      DisplayName = 'Processing Mode'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 2.000000000000000000
      MaxInteger = 2
      ReportVST2Properties = True
      ShortLabel = 'Mode'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterProcessingModeChange
      OnCustomParameterDisplay = ParameterProcessingModeDisplay
      OnStringToParameter = StringToModeParameter
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000000.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Release'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 3000.000000000000000000
      MaxInteger = 3000
      Min = 0.003000000026077032
      ReportVST2Properties = True
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
      OnStringToParameter = StringToReleaseParameter
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Attack Shape'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Attack'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterAttackShapeChange
      OnCustomParameterDisplay = ParameterAttackDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Lookahead'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 4.000000000000000000
      LargeStepInteger = 4
      Max = 1024.000000000000000000
      MaxInteger = 1024
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Lookahd'
      SmallStepFloat = 2.000000000000000000
      StepFloat = 2.000000000000000000
      StepInteger = 2
      Units = 'Samples'
      VSTModule = Owner
      OnParameterChange = ParameterLookaheadChange
      OnCustomParameterDisplay = ParameterLookaheadDisplay
    end>
  ParameterCategories = <
    item
      DisplayName = 'I/O'
      VSTModule = Owner
    end
    item
      DisplayName = 'Time Constants'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcessStereo
  OnProcess32Replacing = VSTModuleProcessStereo
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
