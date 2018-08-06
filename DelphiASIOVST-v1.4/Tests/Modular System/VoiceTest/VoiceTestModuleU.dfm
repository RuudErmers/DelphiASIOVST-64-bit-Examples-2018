object VoiceTestModule: TVoiceTestModule
  OldCreateOrder = False
  DspDirectProcessItem = DspVoiceController1
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth]
  Version = '0.0'
  EffectName = 'VoiceTest'
  ProductName = 'VoiceTest'
  VendorName = 'MyCo'
  PlugCategory = vpcSynth
  CanDos = [vcdSendVstEvents, vcdSendVstMidiEvent, vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  ProcessingMode = pmDspQueue
  UniqueID = 'Qxpl'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <>
  OnEditOpen = VSTModuleEditOpen
  Left = 470
  Top = 116
  Height = 152
  Width = 215
  object DspVoiceController1: TDspVoiceController
    SampleRate = 44100.000000000000000000
    MaxVoices = 5
    LimitVoices = lvtKillOldest
    OnCreateVoice = DspVoiceController1CreateVoice
    OnVoiceCountChanged = DspVoiceController1VoiceCountChanged
    Left = 40
    Top = 8
  end
end
