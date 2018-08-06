object SplitHarmonizerModule: TSplitHarmonizerModule
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'SoundTouch Detuner'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'DDPS'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'FaderWear'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mid/Side Adventure'
      VSTModule = Owner
    end
    item
      DisplayName = 'Distance Doppler'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Encoding'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'EncLRMS'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEncodeChange
      OnCustomParameterDisplay = ParameterEncodeDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Stage A'
      DisplayName = 'Semi Tones A'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      Max = 12000.000000000000000000
      MaxInteger = 12000
      Min = -12000.000000000000000000
      MinInteger = -12000
      ReportVST2Properties = True
      ShortLabel = 'SemiL'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 10.000000000000000000
      StepInteger = 10
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterSemiTonesAChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Stage A'
      DisplayName = 'Delay A'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 200.000000000000000000
      MaxInteger = 200
      ReportVST2Properties = True
      ShortLabel = 'DelayL'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterDelayAChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'Lowpass Filter A'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'LPF A'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterLowpassAChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Stage A'
      DisplayName = 'Mix A'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'MixL'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterMixAChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Stage B'
      DisplayName = 'Semi Tones B'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      Max = 12000.000000000000000000
      MaxInteger = 12000
      Min = -12000.000000000000000000
      MinInteger = -12000
      ReportVST2Properties = True
      ShortLabel = 'SemiR'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 10.000000000000000000
      StepInteger = 10
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterSemiTonesBChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Stage B'
      DisplayName = 'Delay B'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 200.000000000000000000
      MaxInteger = 200
      ReportVST2Properties = True
      ShortLabel = 'DelayR'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterDelayBChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'Lowpass Filter B'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'LPF B'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterLowpassBChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Stage B'
      DisplayName = 'Mix B'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'MixR'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterMixRightChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Use AntiAlias Filter'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Use Ant'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterUseAntiAliasChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Use Quick Seek'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Quick'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterUseQuickSeekChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Use Auto Settings'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'UseAuto'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterAutoSettingsChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Sequence'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Sequenc'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterSequenceChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Seek Window'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'SeekWin'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterSeekWindowChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Overlap'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Overlap'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterOverlapChange
    end>
  ParameterCategories = <
    item
      DisplayName = 'Stage A'
      VSTModule = Owner
    end
    item
      DisplayName = 'Stage B'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcessLR
  OnProcess32Replacing = VSTModuleProcessLR
  OnProcess64Replacing = VSTModuleProcessReplacing64LR
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
