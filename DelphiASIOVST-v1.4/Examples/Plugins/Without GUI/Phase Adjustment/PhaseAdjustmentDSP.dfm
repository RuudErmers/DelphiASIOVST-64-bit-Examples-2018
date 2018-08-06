object PhaseAdjustmentModule: TPhaseAdjustmentModule
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Version = '1.0'
  EffectName = 'Phase Adjustment'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'A'
  IORatio = 1.000000000000000000
  UniqueID = 'Phas'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'A'
      VSTModule = Owner
    end
    item
      DisplayName = 'B'
      VSTModule = Owner
    end
    item
      DisplayName = 'C'
      VSTModule = Owner
    end
    item
      DisplayName = 'D'
      VSTModule = Owner
    end
    item
      DisplayName = 'E'
      VSTModule = Owner
    end
    item
      DisplayName = 'F'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Phase'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 360.000000000000000000
      MaxInteger = 360
      ReportVST2Properties = True
      ShortLabel = 'Phase'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = #176
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterPhaseChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Suppress Ringing'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'No Ring'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterSuppressRingingChange
      OnCustomParameterDisplay = ParameterSuppressRingingDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  Left = 299
  Top = 51
  Height = 150
  Width = 215
end
