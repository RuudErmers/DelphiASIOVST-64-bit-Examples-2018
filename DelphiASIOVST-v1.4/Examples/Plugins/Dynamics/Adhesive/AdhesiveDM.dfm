object AdhesiveDataModule: TAdhesiveDataModule
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Fast Compressor'
  ProductName = 'DAV Dynamic Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  numInputs = 3
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'DVFC'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'Threshold'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = -90.000000000000000000
      MinInteger = -90
      ReportVST2Properties = True
      ShortLabel = 'Thresho'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterThresholdChange
      OnCustomParameterDisplay = ParameterThresholdDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'MakeUp Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 60.000000000000000000
      MaxInteger = 60
      ReportVST2Properties = True
      ShortLabel = 'Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterMakeUpGainChange
      OnCustomParameterDisplay = ParameterMakeUpGainDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 5.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'Ratio'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 2.000000000000000000
      MinInteger = 2
      ReportVST2Properties = True
      ShortLabel = 'Ratio'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterRatioChange
      OnCustomParameterDisplay = ParameterRatioDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'Knee'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
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
      OnCustomParameterDisplay = ParameterKneeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 3000.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Attack'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 30.000000000000000000
      MaxInteger = 30
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'Attack'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterAttackChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 7.500000000000000000
      Category = 'Time Constants'
      DisplayName = 'Release'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1500.000000000000000000
      MaxInteger = 1500
      Min = 200.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Mode'
      DisplayName = 'Mix'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Mix'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterMixChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Mode'
      DisplayName = 'Active'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Active'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameteActiveChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Mode'
      DisplayName = 'PeakClip'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Clip'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterPeakClipChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'Sidechain'
      DisplayName = 'Sidechain Filter'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 2000.000000000000000000
      MaxInteger = 2000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'SC Filt'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterSideChainChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Sidechain'
      DisplayName = 'External Sidechain'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Ext.'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterExtSideChsinChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end>
  ParameterCategories = <
    item
      DisplayName = 'Time Constants'
      VSTModule = Owner
    end
    item
      DisplayName = 'Characteristic'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mode'
      VSTModule = Owner
    end
    item
      DisplayName = 'Sidechain'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
