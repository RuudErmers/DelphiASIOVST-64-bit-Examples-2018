object StkChorusModule: TStkChorusModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Stk Chorus'
  ProductName = 'DAV Stk Examples'
  VendorName = 'Delphi ASIO & VST Projects'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default Network Chorus'
  IORatio = 1.000000000000000000
  UniqueID = 'STKR'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default Network Chorus'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default JC Chorus'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default Blended Chorus'
      VSTModule = Owner
    end
    item
      DisplayName = 'Short'
      VSTModule = Owner
    end
    item
      DisplayName = 'Long'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Depth'
      LargeStepFloat = 2.000000000000000000
      Max = 500.000000000000000000
      ShortLabel = 'ModDpth'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParamModDepthChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Frequency'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'ModFreq'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamModFreqChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Effect Mix'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Mix'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamMixChange
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
