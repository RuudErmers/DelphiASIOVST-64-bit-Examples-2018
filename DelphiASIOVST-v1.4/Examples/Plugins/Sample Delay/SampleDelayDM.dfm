object SampleDelayDataModule: TSampleDelayDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Sample Delay'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  InitialDelay = 1025
  IORatio = 1.000000000000000000
  UniqueID = 'Spdy'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Samples Left'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 1024.000000000000000000
      MaxInteger = 1024
      Min = -1024.000000000000000000
      MinInteger = -1024
      ReportVST2Properties = True
      ShortLabel = 'Left'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Samples'
      VSTModule = Owner
      OnParameterChange = ParameterSamplesLeftChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Samples Right'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 1024.000000000000000000
      MaxInteger = 1024
      Min = -1024.000000000000000000
      MinInteger = -1024
      ReportVST2Properties = True
      ShortLabel = 'Right'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Samples'
      VSTModule = Owner
      OnParameterChange = ParameterSamplesRightChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  Height = 150
  Width = 215
end
