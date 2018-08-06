object AnalogueMojoDM: TAnalogueMojoDM
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Version = '1.0'
  EffectName = 'Analogue Mojo'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'AnMo'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'DC Filter Freq.'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'DC Frq.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterFrequencyChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 2.000000000000000000
      DisplayName = 'Damping Freq,'
      LargeStepFloat = 2.000000000000000000
      Max = 16000.000000000000000000
      MaxInteger = 16000
      Min = 10000.000000000000000000
      MinInteger = 10000
      ShortLabel = 'Damping'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'kHz'
      VSTModule = Owner
      OnParameterChange = ParameterDampingChange
      OnCustomParameterDisplay = ParameterDampingDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcessStereo
  OnProcess32Replacing = VSTModuleProcessStereo
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
