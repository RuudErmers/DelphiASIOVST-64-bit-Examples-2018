object LinearPhaseLinkwitzRileyDataModule: TLinearPhaseLinkwitzRileyDataModule
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Version = '1.0'
  EffectName = 'Linear Phase'
  ProductName = 'DAV Filter Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd1in2out]
  SampleRate = 44100.000000000000000000
  numInputs = 1
  numOutputs = 4
  CurrentProgramName = 'Default'
  InitialDelay = 128
  IORatio = 1.000000000000000000
  UniqueID = 'LRCR'
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
      Category = 'Low'
      DisplayName = 'Low Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Low'
      DisplayName = 'Low Filter Order'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 32.000000000000000000
      MaxInteger = 32
      Min = 2.000000000000000000
      MinInteger = 2
      ShortLabel = 'Order'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFilterOrderChange
      OnCustomParameterDisplay = ParameterFilterOrderDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Mid'
      DisplayName = 'Mid Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ShortLabel = 'Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Mid'
      DisplayName = 'Mid Filter Order'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 32.000000000000000000
      MaxInteger = 32
      Min = 2.000000000000000000
      MinInteger = 2
      ShortLabel = 'Order'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFilterOrderChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'High'
      DisplayName = 'High Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ShortLabel = 'Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'High'
      DisplayName = 'High Filter Order'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 32.000000000000000000
      MaxInteger = 32
      Min = 2.000000000000000000
      MinInteger = 2
      ShortLabel = 'Order'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFilterOrderChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Oversampling'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'OS'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterOversamplingChange
    end>
  ParameterCategories = <
    item
      DisplayName = 'Low'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mid'
      VSTModule = Owner
    end
    item
      DisplayName = 'High'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnResume = VSTModuleResume
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
