object VSTSSModule: TVSTSSModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsProgramChunks, effFlagsIsSynth]
  Version = '1.0'
  ProductName = 'XRECDS3'
  VendorName = 'ChristianTobyRuud'
  VersionRelease = 1
  PlugCategory = vpcSynth
  CanDos = [vcdSendVstEvents, vcdSendVstMidiEvent, vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Preset 01'
  IORatio = 1.000000000000000000
  UniqueID = 'XDS3'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Preset 01'
      VSTModule = Owner
      OnLoadChunk = VSTSSModulePrograms0LoadChunk
      OnStoreChunk = VSTSSModulePrograms0StoreChunk
    end>
  ParameterProperties = <>
  ParameterCategories = <>
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
