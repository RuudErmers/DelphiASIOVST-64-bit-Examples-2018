object fReeverbVST: TfReeverbVST
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'fReeverb'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'fRee'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Small Room'
      VSTModule = Owner
    end
    item
      DisplayName = 'Large Hall'
      VSTModule = Owner
    end
    item
      DisplayName = 'Random 23'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      Category = 'Mix'
      DisplayName = 'Dry'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 100.000000000000000000
      MaxInteger = 0
      ReportVST2Properties = True
      ShortLabel = 'Dry'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDryChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Mix'
      DisplayName = 'Wet'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 100.000000000000000000
      MaxInteger = 0
      ReportVST2Properties = True
      ShortLabel = 'Wet'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterWetChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Geometry'
      DisplayName = 'Width'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 100.000000000000000000
      MaxInteger = 0
      ReportVST2Properties = True
      ShortLabel = 'Width'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterWidthChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Geometry'
      DisplayName = 'RoomSize'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 1.000000000000000000
      MaxInteger = 0
      ReportVST2Properties = True
      ShortLabel = 'RoomSiz'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterRoomSizeChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Geometry'
      DisplayName = 'FreeZe'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 1.000000000000000000
      MaxInteger = 0
      ReportVST2Properties = True
      ShortLabel = 'FreeZe'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterFreezeChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Geometry'
      DisplayName = 'Stretch'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 20.000000000000000000
      MaxInteger = 0
      ReportVST2Properties = True
      ShortLabel = 'Stretch'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterStretchChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Geometry'
      DisplayName = 'Damp'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 100.000000000000000000
      MaxInteger = 0
      ReportVST2Properties = True
      ShortLabel = 'Damp'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDampChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Filter Count'
      DisplayName = 'NumAllPasses'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 16.000000000000000000
      MaxInteger = 0
      Min = 1.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'NumAllP'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterNumAllpassesChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Filter Count'
      DisplayName = 'NumCombs'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 16.000000000000000000
      MaxInteger = 0
      Min = 1.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'NumComb'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterNumCombsChange
    end>
  ParameterCategories = <
    item
      DisplayName = 'Mix'
      VSTModule = Owner
    end
    item
      DisplayName = 'Geometry'
      VSTModule = Owner
    end
    item
      DisplayName = 'Filter Count'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcessReplacing
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 248
  Top = 106
  Height = 150
  Width = 215
end
