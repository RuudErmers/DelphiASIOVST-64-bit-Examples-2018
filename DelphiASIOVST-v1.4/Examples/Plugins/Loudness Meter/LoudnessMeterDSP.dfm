object LoudnessMeterModule: TLoudnessMeterModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Loudness Meter'
  ProductName = 'DAV Analyser Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'ACLM'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Scale'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Scale'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterScaleChange
      OnCustomParameterDisplay = ParameterScaleDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Time'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 2.000000000000000000
      MaxInteger = 2
      ShortLabel = 'Time'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterTimeChange
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'State'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'State'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterStateChange
      OnCustomParameterDisplay = ParameterStateDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Loudness'
      LargeStepFloat = 2.000000000000000000
      Max = 9.000000000000000000
      MaxInteger = 9
      Min = -18.000000000000000000
      MinInteger = -18
      ShortLabel = 'Loudnes'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterLoudnessChange
      OnCustomParameterDisplay = ParameterLoudnessDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Peak Momentary'
      LargeStepFloat = 2.000000000000000000
      Max = 9.000000000000000000
      MaxInteger = 9
      Min = -18.000000000000000000
      MinInteger = -18
      ShortLabel = 'PeakMom'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterPeakMomChange
      OnCustomParameterDisplay = ParameterPeakMomDisplay
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
