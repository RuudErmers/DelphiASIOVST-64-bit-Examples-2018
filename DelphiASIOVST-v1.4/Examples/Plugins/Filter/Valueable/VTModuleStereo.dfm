object VTVSTModule: TVTVSTModule
  OldCreateOrder = True
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Valve Tone '#39'63'
  ProductName = 'DAV Filter Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd2in1out, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'VTST'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      Category = 'Treble'
      DisplayName = 'Treble Gain Left'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 12.000000000000000000
      MaxInteger = 12
      Min = -12.000000000000000000
      MinInteger = -12
      ShortLabel = 'HiGain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamHiGainLeftChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Treble'
      DisplayName = 'Treble Bypass Left'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'HiByps'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamHiBypassLeftChange
      OnCustomParameterDisplay = ParameterBypassDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Bass'
      DisplayName = 'Bass Gain Left'
      LargeStepFloat = 2.000000000000000000
      Max = 12.000000000000000000
      MaxInteger = 12
      Min = -12.000000000000000000
      MinInteger = -12
      ShortLabel = 'LowGain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamLowGainLeftChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Bass'
      DisplayName = 'Bass Bypass Left'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'LowByps'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamLowBypassLeftChange
      OnCustomParameterDisplay = ParameterBypassDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Treble'
      DisplayName = 'Treble Gain Right'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 12.000000000000000000
      MaxInteger = 12
      Min = -12.000000000000000000
      MinInteger = -12
      ShortLabel = 'HiGain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamHiGainRightChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Treble'
      DisplayName = 'Treble Bypass Right'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'HiByps'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamHiBypassRightChange
      OnCustomParameterDisplay = ParameterBypassDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Bass'
      DisplayName = 'Bass Gain Right'
      LargeStepFloat = 2.000000000000000000
      Max = 12.000000000000000000
      MaxInteger = 12
      Min = -12.000000000000000000
      MinInteger = -12
      ShortLabel = 'LowGain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamLowGainRightChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Bass'
      DisplayName = 'Bass Bypass Right'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'LowByps'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamLowBypassRightChange
      OnCustomParameterDisplay = ParameterBypassDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Drive'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 4.000000000000000000
      MaxInteger = 4
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Drive'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamDriveChange
      OnCustomParameterDisplay = ParamDriveDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Channel'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 2.000000000000000000
      MaxInteger = 2
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Channel'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamChannelChange
      OnCustomParameterDisplay = ParamChannelDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output Gain'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 12.000000000000000000
      MaxInteger = 12
      Min = -12.000000000000000000
      MinInteger = -12
      ShortLabel = 'OutGain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParamOutGainChange
    end>
  ParameterCategories = <
    item
      DisplayName = 'Treble'
      VSTModule = Owner
    end
    item
      DisplayName = 'Bass'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcessStereo
  OnProcess32Replacing = VSTModuleProcessStereo
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 248
  Top = 106
  Height = 150
  Width = 215
end
