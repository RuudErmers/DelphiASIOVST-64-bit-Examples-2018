object VSTHRTF3DModule: TVSTHRTF3DModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'HRTF 3D'
  VendorName = 'Christian Budde'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Front'
  InitialDelay = 64
  IORatio = 1.000000000000000000
  UniqueID = 'HR3D'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Front'
      VSTModule = Owner
    end
    item
      DisplayName = 'Left'
      VSTModule = Owner
    end
    item
      DisplayName = 'Right'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Azimuth'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 360.000000000000000000
      MaxInteger = 360
      ReportVST2Properties = True
      ShortLabel = 'Azmth'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = #176
      VSTModule = Owner
      OnParameterChange = ParamAzimuthChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Polar'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 140.000000000000000000
      MaxInteger = 140
      ReportVST2Properties = True
      ShortLabel = 'Polar'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = #176
      VSTModule = Owner
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Radius'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 2.000000000000000000
      MaxInteger = 2
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Rad'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = 'm'
      VSTModule = Owner
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Interpolation'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 3.000000000000000000
      MaxInteger = 3
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Interpl'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterInterpolationChange
      OnCustomParameterDisplay = ParameterInterpolationDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Display HRTFs'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'HRTFs?'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterDisplayHRTFsDisplay
    end>
  ParameterCategories = <>
  OnOpen = VST2ModuleOpen
  OnClose = VST2ModuleClose
  OnParameterChange = VST2ModuleParameterChange
  OnProcess = VST2ModuleProcess
  OnProcess32Replacing = VST2ModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
