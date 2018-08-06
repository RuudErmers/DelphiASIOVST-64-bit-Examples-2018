object DjFilterDataModule: TDjFilterDataModule
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'DjFilter'
  ProductName = 'DAV Filter Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Init'
  IORatio = 1.000000000000000000
  UniqueID = 'DjFr'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      Category = 'Gains'
      DisplayName = 'Low'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -96.000000000000000000
      MinInteger = -96
      ReportVST2Properties = True
      ShortLabel = 'Low'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamLowChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Gains'
      DisplayName = 'Mid'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -96.000000000000000000
      MinInteger = -96
      ReportVST2Properties = True
      ShortLabel = 'Mid'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamMidChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Gains'
      DisplayName = 'High'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -96.000000000000000000
      MinInteger = -96
      ReportVST2Properties = True
      ShortLabel = 'High'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamHigh
    end>
  ParameterCategories = <
    item
      DisplayName = 'Gains'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
