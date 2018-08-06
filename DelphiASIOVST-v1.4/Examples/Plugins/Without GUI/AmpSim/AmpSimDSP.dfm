object AmpSimModule: TAmpSimModule
  OldCreateOrder = True
  Flags = [effFlagsCanMono, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'AmpSim'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Amp & Speaker Simulator'
  IORatio = 1.000000000000000000
  UniqueID = 'AmpS'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Amp & Speaker Simulator'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mesa-Boogie'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Model'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 6.000000000000000000
      MaxInteger = 6
      ShortLabel = 'select'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamModelChange
      OnCustomParameterDisplay = ParamModelDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Drive'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ShortLabel = 'drive'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'S <> H'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamDriveChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Bias'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ShortLabel = 'bias'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamBiasChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 20.000000000000000000
      MaxInteger = 20
      Min = -20.000000000000000000
      MinInteger = -20
      ShortLabel = 'output'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamOutputChanged
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Process'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'stereo'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamProcessChange
      OnCustomParameterDisplay = ParamProcessDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Highpass Filter'
      DisplayName = 'HPF Frequency'
      LargeStepFloat = 10.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 2.000000000000000000
      MinInteger = 2
      ShortLabel = 'hpfFreq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamHPFFreqChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Highpass Filter'
      DisplayName = 'HPF Resonance'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'hpfReso'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamHPFResonanceChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Noise'
      Flags = [ppfParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = -20.000000000000000000
      MaxInteger = -20
      Min = -120.000000000000000000
      MinInteger = -120
      ShortLabel = 'Noise'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamNoiseChange
    end>
  ParameterCategories = <
    item
      DisplayName = 'Highpass Filter'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnProcess64Replacing = VSTModuleProcessDoubleReplacing
  OnSampleRateChange = VSTModuleSampleRateChange
  OnSuspend = VSTModuleSuspend
  Left = 864
  Top = 84
  Height = 150
  Width = 215
end
