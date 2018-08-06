object EnhancedGateDataModule: TEnhancedGateDataModule
  OldCreateOrder = True
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Simple Gate'
  ProductName = 'DAV Dynamics Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out, vcdBypass]
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'EAGa'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Power'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Power'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = EAGPowerChange
      OnCustomParameterDisplay = EAGOnOffDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Threshold'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -80.000000000000000000
      MinInteger = -80
      ReportVST2Properties = True
      ShortLabel = 'Thrshld'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = EAGThresholdChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100000.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Attack'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 300
      Min = 0.009999999776482582
      MinInteger = -200
      ReportVST2Properties = True
      ShortLabel = 'Attack'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = EAGAttackChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 250.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Hold'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 2.500000000000000000
      MaxInteger = 24
      Min = 0.009999999776482582
      MinInteger = -200
      ReportVST2Properties = True
      ShortLabel = 'Hold'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = EAGHoldChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Decay'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 100.000000000000000000
      Max = 5000.000000000000000000
      MaxInteger = 370
      Min = 5.000000000000000000
      MinInteger = 70
      ReportVST2Properties = True
      ShortLabel = 'Decay'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 10.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = EAGDecayChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Duck'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Duck'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = EAGDuckChange
      OnCustomParameterDisplay = EAGOnOffDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stereo Link'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Link'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = EAGStereoLinkChange
      OnCustomParameterDisplay = EAGOnOffDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Sidechain'
      DisplayName = 'Side Chain Source'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'SCSrc'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = EAGSideChainSourceChange
      OnCustomParameterDisplay = EAGSideChainSourceDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 200.000000000000000000
      Category = 'Sidechain'
      DisplayName = 'Lo Cut'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 20.000000000000000000
      LargeStepInteger = 20
      Max = 4000.000000000000000000
      MaxInteger = 4000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'LoCut'
      SmallStepFloat = 20.000000000000000000
      StepFloat = 20.000000000000000000
      StepInteger = 20
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = EAGLoCutChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'Sidechain'
      DisplayName = 'Hi Cut'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 20.000000000000000000
      LargeStepInteger = 20
      Max = 20.000000000000000000
      MaxInteger = 20000
      Min = 0.200000002980232200
      MinInteger = 200
      ReportVST2Properties = True
      ShortLabel = 'HiCut'
      SmallStepFloat = 20.000000000000000000
      StepFloat = 20.000000000000000000
      StepInteger = 20
      Units = 'kHz'
      VSTModule = Owner
      OnParameterChange = EAGHiCutChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'Ratio'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Ratio'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 0.500000000000000000
      VSTModule = Owner
      OnParameterChange = EAGRatioChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'Knee'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Knee'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 0.200000002980232200
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = EAGKneeChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'Range'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 10.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -80.000000000000000000
      MinInteger = -80
      ReportVST2Properties = True
      ShortLabel = 'Range'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = EAGRangeChange
    end>
  ParameterCategories = <
    item
      DisplayName = 'Sidechain'
      VSTModule = Owner
    end
    item
      DisplayName = 'Characteristic'
      VSTModule = Owner
    end
    item
      DisplayName = 'Time constants'
      VSTModule = Owner
    end
    item
      DisplayName = 'Time Constants'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcessBypass
  OnProcess32Replacing = VSTModuleProcessBypass
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 552
  Top = 84
  Height = 150
  Width = 215
end
