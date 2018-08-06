object VSTSSModule: TVSTSSModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsProgramChunks, effFlagsIsSynth]
  Version = '1.0'
  ProductName = 'XRECDS2'
  VendorName = 'ChristianTobyRuud'
  VersionRelease = 1
  PlugCategory = vpcSynth
  CanDos = [vcdSendVstEvents, vcdSendVstMidiEvent, vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Preset 01'
  IORatio = 1.000000000000000000
  UniqueID = 'XWVR'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Preset 01'
      VSTModule = Owner
      OnLoadChunk = VSTSSModulePrograms0LoadChunk
      OnStoreChunk = VSTSSModulePrograms0StoreChunk
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Position'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 127
      ShortLabel = 'Positio'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Length'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 127
      ShortLabel = 'Length'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
    end>
  ParameterCategories = <>
  OnGetChunkParameter = VSTModuleGetChunkParameter
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnEditClose = VSTModuleEditClose
  OnProcess32Replacing = VSTModuleProcess32Replacing
  OnProcessMidi = VSTModuleProcessMidi
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
