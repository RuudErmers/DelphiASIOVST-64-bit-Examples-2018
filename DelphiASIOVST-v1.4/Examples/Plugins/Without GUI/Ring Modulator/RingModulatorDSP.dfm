object RingModulatorDataModule: TRingModulatorDataModule
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Version = '1.0'
  EffectName = 'Ring Modulator'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  numInputs = 1
  numOutputs = 1
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'RiMo'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Very Light'
      VSTModule = Owner
    end
    item
      DisplayName = 'Dirty'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 100000.000000000000000000
      DisplayName = 'Frequency'
      LargeStepFloat = 2.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 0.009999999776482582
      ShortLabel = 'Freq.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterFrequencyChange
      OnCustomParameterLabel = ParameterFrequencyLabel
      OnCustomParameterDisplay = ParameterFrequencyDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcessMono
  OnProcess32Replacing = VSTModuleProcessMono
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
