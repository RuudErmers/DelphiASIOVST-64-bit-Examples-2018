object SimpleVibratoModule: TSimpleVibratoModule
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'Vibrato'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'Vibr'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Soft Flange'
      VSTModule = Owner
    end
    item
      DisplayName = 'Hard Flange'
      VSTModule = Owner
    end
    item
      DisplayName = 'WishyWoshy'
      VSTModule = Owner
    end
    item
      DisplayName = 'Full Vibrato'
      VSTModule = Owner
    end
    item
      DisplayName = '2,5 Promille'
      VSTModule = Owner
    end
    item
      DisplayName = 'Thick'
      VSTModule = Owner
    end
    item
      DisplayName = 'Extreme'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 10000.000000000000000000
      DisplayName = 'Speed'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.001000000047497451
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Speed'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamSpeedChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Semitones'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesFloatStep, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 12.000000000000000000
      MaxInteger = 12
      ReportVST2Properties = True
      ShortLabel = 'Semi'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamDepthChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnProcess64Replacing = VSTModuleProcessDoubleReplacing
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
