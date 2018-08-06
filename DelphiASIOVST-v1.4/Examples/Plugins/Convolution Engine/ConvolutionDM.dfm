object ConvolutionDataModule: TConvolutionDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Convolution'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  BlockSize = 8192
  CurrentProgramName = 'Default'
  BlockModeSize = 8192
  InitialDelay = 2048
  IORatio = 1.000000000000000000
  UniqueID = 'Conv'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Latency Order'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 16.000000000000000000
      MaxInteger = 16
      Min = 6.000000000000000000
      MinInteger = 6
      ShortLabel = 'Latency'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterLatencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Maximum IR Order'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 20.000000000000000000
      MaxInteger = 20
      Min = 7.000000000000000000
      MinInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'Maximum'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterMaximumIROrderChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  Height = 150
  Width = 215
end
