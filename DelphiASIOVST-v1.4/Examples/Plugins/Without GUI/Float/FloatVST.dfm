object FloatModule: TFloatModule
  OldCreateOrder = False
  Version = '1.0'
  EffectName = 'Float'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi VST & ASIO Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'FltX'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = '8-Bit'
      VSTModule = Owner
    end
    item
      DisplayName = '16-Bit'
      VSTModule = Owner
    end
    item
      DisplayName = '32-Bit'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Float Bits'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 4.000000000000000000
      MaxInteger = 4
      ReportVST2Properties = True
      ShortLabel = 'Bits'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFloatBitsChange
      OnCustomParameterDisplay = ParameterFloatBitsDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnProcess = VSTModuleProcess32_8
  OnProcess32Replacing = VSTModuleProcess32_8
  OnProcess64Replacing = VSTModuleProcess64_8
  Height = 150
  Width = 215
end
