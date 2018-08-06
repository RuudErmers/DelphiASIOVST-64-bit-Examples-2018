object HrtfConvolverDataModule: THrtfConvolverDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'HRTF Convolver'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'HRCv'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      Category = 'HRTF'
      DisplayName = 'Azimuth'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 180.000000000000000000
      MaxInteger = 180
      Min = -180.000000000000000000
      MinInteger = -180
      ReportVST2Properties = True
      ShortLabel = 'Azimuth'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = #176
      VSTModule = Owner
      OnParameterChange = ParameterAzimuthChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'HRTF'
      DisplayName = 'Elevation'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 90.000000000000000000
      MaxInteger = 90
      Min = -90.000000000000000000
      MinInteger = -90
      ReportVST2Properties = True
      ShortLabel = 'Elev.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = #176
      VSTModule = Owner
      OnParameterChange = ParameterElevationChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'HRTF'
      DisplayName = 'Radius'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Radius'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'm'
      VSTModule = Owner
      OnParameterChange = ParameterRadiusChange
    end>
  ParameterCategories = <
    item
      DisplayName = 'HRTF'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
