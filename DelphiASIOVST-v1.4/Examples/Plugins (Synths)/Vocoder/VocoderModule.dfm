object VSTSSModule: TVSTSSModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth]
  Version = '1.0'
  EffectName = 'Vocoder'
  ProductName = 'DAV Synth Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcSynth
  CanDos = [vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcdMixDryWet, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'Voco'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Input Volume'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 5.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -80.000000000000000000
      MinInteger = -80
      ReportVST2Properties = True
      ShortLabel = 'vol in'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 3
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = VocInputVolumeChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Synth Volume'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 5.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -80.000000000000000000
      MinInteger = -80
      ReportVST2Properties = True
      ShortLabel = 'vol syn'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 3
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = VocSynthVolumeChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Vocoder Volume'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 5.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -80.000000000000000000
      MinInteger = -80
      ReportVST2Properties = True
      ShortLabel = 'vol voc'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 3
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = VocVocoderVolumeChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Attack'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'Attack'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterAttackChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Release'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Bandwidth'
      LargeStepFloat = 2.000000000000000000
      Max = 2.000000000000000000
      Min = 0.019999999552965160
      ShortLabel = 'BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterBandwidthChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess32Replacing = VSTModuleProcess32Replacing
  OnProcess64Replacing = VSTModuleProcess64Replacing
  OnProcessMidi = VSTModuleProcessMidi
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
