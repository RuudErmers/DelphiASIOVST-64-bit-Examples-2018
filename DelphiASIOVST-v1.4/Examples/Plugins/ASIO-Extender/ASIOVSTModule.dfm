object ASIOVSTModule: TASIOVSTModule
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'ASIO Extender'
  ProductName = 'DAV Tools Examples'
  VendorName = 'Delphi ASIO & VST Project'
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Init'
  IORatio = 1.000000000000000000
  UniqueID = 'ASIO'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'ASIO Driver'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 0.500000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'ASIO Dr'
      SmallStepFloat = 0.009999999776482582
      StepFloat = 0.100000001490116100
      VSTModule = Owner
      OnParameterChange = ASIODriverChange
      OnCustomParameterDisplay = ASIODriverDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Use SSE/MMX'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Use SSE'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterUseSSEMMXChange
      OnCustomParameterDisplay = ParameterUseSSEMMXDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Accuracy'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Accurac'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'bit'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterAccuracyChange
      OnCustomParameterDisplay = ParameterAccuracyDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnBlockSizeChange = VSTModuleBlockSizeChange
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  Height = 188
  Width = 282
end
