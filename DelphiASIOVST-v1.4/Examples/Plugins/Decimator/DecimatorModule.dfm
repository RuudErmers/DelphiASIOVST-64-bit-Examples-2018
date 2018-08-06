object VSTDecimator: TVSTDecimator
  Flags = [effFlagsHasEditor, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'Decimator'
  ProductName = 'Decimator'
  VendorName = 'Tobybear & Christian'
  PlugCategory = vpcEffect
  CanDos = [vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd1in2out, vcd2in1out, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Preset 1'
  IORatio = 1.000000000000000000
  UniqueID = 'TBDM'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Preset 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Preset 2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Preset 3'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      Category = 'Decimator'
      DisplayName = 'Samplerate'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1000.000000000000000000
      LargeStepInteger = 1000
      Max = 44100.000000000000000000
      MaxInteger = 44100
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'rate'
      SmallStepFloat = 100.000000000000000000
      StepFloat = 100.000000000000000000
      StepInteger = 100
      Units = 'Hz'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterSampleRateChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Decimator'
      DisplayName = 'Bits'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 24.000000000000000000
      MaxInteger = 24
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'bits'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterBitsChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Post Filter'
      DisplayName = 'Cutoff Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1000.000000000000000000
      LargeStepInteger = 1000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'cut'
      SmallStepFloat = 100.000000000000000000
      StepFloat = 100.000000000000000000
      StepInteger = 100
      Units = 'Hz'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterCutoffChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Post Filter'
      DisplayName = 'Resonance'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 8.000000000000000000
      MaxInteger = 8
      Min = 0.100000001490116100
      ReportVST2Properties = True
      ShortLabel = 'res'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterResonanceChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Post Filter'
      DisplayName = 'FilterType'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Type'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterFilterTypeChange
      OnCustomParameterDisplay = ParameterFilterTypeDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Mix'
      DisplayName = 'Wet/Dry Mix'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 10.000000000000000000
      Max = 100.000000000000000000
      MaxInteger = 0
      ReportVST2Properties = True
      ShortLabel = 'mix'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterWetDryMixChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Mix'
      DisplayName = 'Output Volume'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 3.000000000000000000
      LargeStepInteger = 3
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -24.000000000000000000
      MinInteger = -24
      ReportVST2Properties = True
      ShortLabel = 'vol'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      UseDefaultString2ParameterHandler = True
      VSTModule = Owner
      OnParameterChange = ParameterOutputVolumeChange
    end>
  ParameterCategories = <
    item
      DisplayName = 'Post Filter'
      VSTModule = Owner
    end
    item
      DisplayName = 'Decimator'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mix'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnProcess64Replacing = VSTModuleProcessDoubleReplacing
  OnProcessMidi = VSTModuleProcessMidi
  Height = 152
  Width = 219
end
