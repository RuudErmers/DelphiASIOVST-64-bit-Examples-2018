object ProcessingTestModule: TProcessingTestModule
  OldCreateOrder = False
  Version = '1.0'
  EffectName = 'Processing Test'
  ProductName = 'DAV Effect Example'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'Proc'
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
      DisplayName = '32-Bit Freq.'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ShortLabel = '32-Bit'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterFreq32Change
      OnCustomParameterLabel = ParameterFreqLabel
      OnCustomParameterDisplay = ParameterFreqDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = '64-Bit Freq.'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ShortLabel = '64-Bit '
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterFreq64Change
      OnCustomParameterLabel = ParameterFreqLabel
      OnCustomParameterDisplay = ParameterFreqDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess32Replacing
  OnProcess32Replacing = VSTModuleProcess32Replacing
  OnProcess64Replacing = VSTModuleProcess64Replacing
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
