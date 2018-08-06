object ChebyshevHPModule: TChebyshevHPModule
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Chebyshev Lowpass Filter'
  ProductName = 'DAV Filter Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'CbcL'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'Freq'
      SmallStepFloat = 10.000000000000000000
      StepFloat = 50.000000000000000000
      StepInteger = 50
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamFrequencyChange
      OnCustomParameterLabel = ParameterFrequencyLabel
      OnCustomParameterDisplay = ParameterFrequencyDisplay
      OnStringToParameter = StringToFrequencyParameter
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Ripple'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.000009999999747379
      ReportVST2Properties = True
      ShortLabel = 'Ripple'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 0.100000001490116100
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamRippleChange
      OnCustomParameterDisplay = ParameterRippleDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Order'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      ReportVST2Properties = True
      ShortLabel = 'Order'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamOrderChange
      OnCustomParameterDisplay = ParameterOrderDisplay
      OnStringToParameter = StringToOrderParameter
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnProcess64Replacing = VSTModuleProcessDoubleReplacing
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 694
  Top = 85
  Height = 150
  Width = 215
end
