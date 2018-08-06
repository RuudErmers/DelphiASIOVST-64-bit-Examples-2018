object LightweightGateDataModule: TLightweightGateDataModule
  OldCreateOrder = True
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0.2'
  EffectName = 'Lightweight Gate'
  ProductName = 'DAV Dynamic Examples'
  VendorName = 'Delphi ASIO & VST Project'
  VersionRelease = 2
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'DVLG'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Soft Gate'
      VSTModule = Owner
    end
    item
      DisplayName = 'Light Gate'
      VSTModule = Owner
    end
    item
      DisplayName = 'Moderate Gate'
      VSTModule = Owner
    end
    item
      DisplayName = 'Doorway'
      VSTModule = Owner
    end
    item
      DisplayName = 'Staccato'
      VSTModule = Owner
    end
    item
      DisplayName = 'More Gating'
      VSTModule = Owner
    end
    item
      DisplayName = 'Heavy Gate'
      VSTModule = Owner
    end
    item
      DisplayName = 'Percussive'
      VSTModule = Owner
    end
    item
      DisplayName = 'Difference'
      VSTModule = Owner
    end
    item
      DisplayName = 'Potentials'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 100000.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Attack'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
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
      CurveFactor = 100000.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Release'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 5000.000000000000000000
      MaxInteger = 5000
      Min = 0.050000000745058060
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
      DisplayName = 'Ratio'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 1
      Min = 0.009999999776482582
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
    end>
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
