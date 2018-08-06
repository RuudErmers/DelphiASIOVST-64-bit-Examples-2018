object PlugInPlugModule: TPlugInPlugModule
  OnCreate = VST2ModuleCreate
  Flags = [effFlagsHasEditor, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'Load VST Plugin'
  ProductName = 'DAV Tools Examples'
  VendorName = 'Delphi ASIO & VST Examples'
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd1in2out, vcd2in1out, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'Plug'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <>
  ParameterCategories = <>
  OnOpen = VST2ModuleOpen
  OnClose = VST2ModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnEditClose = VSTModuleEditClose
  OnEditIdle = VST2ModuleEditIdle
  OnEditTop = VST2ModuleEditTop
  OnEditGetSize = VSTModuleEditGetSize
  OnEditSleep = VST2ModuleEditSleep
  OnBeforeProgramChange = VST2ModuleBeforeProgramChange
  OnBlockSizeChange = VST2ModuleBlockSizeChange
  OnCanDo = VST2ModuleCanDo
  OnGetVU = VST2ModuleGetVU
  OnParameterChange = VST2ModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess32Replacing
  OnProcess64Replacing = VSTModuleProcess64Replacing
  OnSampleRateChange = VST2ModuleSampleRateChange
  OnStartProcess = VST2ModuleStartProcess
  OnStopProcess = VST2ModuleStopProcess
  Height = 199
  Width = 283
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Programme\Audio\Plugins\VST'
    Tempo = 120.000000000000000000
    VendorVersion = 0
    VstPlugIns = <
      item
        DisplayName = 'TVstPlugIn'
        VstOfflineTasks = <>
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2300
    Left = 32
    Top = 16
  end
end
