object ThirdOctaveAnalyserModule: TThirdOctaveAnalyserModule
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Third Octave Analyser'
  ProductName = 'DAV Analyser Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  numInputs = 1
  numOutputs = 1
  CurrentProgramName = 'Slow'
  IORatio = 1.000000000000000000
  UniqueID = 'TOAV'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Slow'
      VSTModule = Owner
    end
    item
      DisplayName = 'Medium'
      VSTModule = Owner
    end
    item
      DisplayName = 'Fast'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'Smooth'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Smooth'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterSmoothChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Fullscale Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 120.000000000000000000
      MaxInteger = 120
      Min = -20.000000000000000000
      MinInteger = -20
      ReportVST2Properties = True
      ShortLabel = 'FSGain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterFullscaleGainChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Downsampling'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'DS'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterDownsamplingChange
      OnCustomParameterDisplay = ParameterDownsamplingDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcessReplacing
  OnProcess32Replacing = VSTModuleProcessReplacing
  Height = 150
  Width = 215
end
