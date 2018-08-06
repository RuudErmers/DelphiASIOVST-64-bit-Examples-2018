object SplitTemplateDataModule: TSplitTemplateDataModule
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Split Template'
  ProductName = 'DAV Wrapper Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'Spli'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Simple Split'
      VSTModule = Owner
    end
    item
      DisplayName = 'Linkwitz-Riley'
      VSTModule = Owner
    end
    item
      DisplayName = 'Dynamic Duo'
      VSTModule = Owner
    end
    item
      DisplayName = 'Left/Right'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mid/Side'
      VSTModule = Owner
    end
    item
      DisplayName = 'Serialized'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      Category = 'Split'
      DisplayName = 'Mode'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 12.000000000000000000
      MaxInteger = 12
      ReportVST2Properties = True
      ShortLabel = 'Mode'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamModeChange
      OnCustomParameterDisplay = ParamModeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Split'
      DisplayName = 'Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'Freq'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 10.000000000000000000
      StepInteger = 10
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamFreqChange
      OnCustomParameterLabel = ParamFreqLabel
      OnCustomParameterDisplay = ParamFreqDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Split'
      DisplayName = 'Order'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Order'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamOrderChange
      OnCustomParameterLabel = ParamOrderLabel
      OnCustomParameterDisplay = ParamOrderDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Volume'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -90.000000000000000000
      MinInteger = -90
      ReportVST2Properties = True
      ShortLabel = 'Volume'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamVolumeChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Oversampling'
      DisplayName = 'Oversampling'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Oversmp'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamOversamplingChange
      OnCustomParameterDisplay = ParamOversamplingDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'FontOversampling'
      DisplayName = 'OS Factor'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Factor'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamOSFactorChange
      OnCustomParameterDisplay = ParamOSFactorDisplay
    end>
  ParameterCategories = <
    item
      DisplayName = 'Split'
      VSTModule = Owner
    end
    item
      DisplayName = 'Oversampling'
      VSTModule = Owner
    end
    item
      DisplayName = 'Oversampling'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnEditClose = VSTModuleEditClose
  OnEditIdle = VSTModuleEditIdle
  OnEditTop = VSTModuleEditTop
  OnEditSleep = VSTModuleEditSleep
  OnBlockSizeChange = VSTModuleBlockSizeChange
  OnGetVU = VSTModuleGetVU
  OnInputProperties = VSTModuleInputProperties
  OnOfflineNotify = VSTModuleOfflineNotify
  OnOutputProperties = VSTModuleOutputProperties
  OnProcessEvents = VSTModuleProcessEvents
  OnProcessVarIO = VSTModuleProcessVarIO
  OnResume = VSTModuleResume
  OnSampleRateChange = VSTModuleSampleRateChange
  OnStartProcess = VSTModuleStartProcess
  OnStopProcess = VSTModuleStopProcess
  OnSuspend = VSTModuleSuspend
  OnVendorSpecific = VSTModuleVendorSpecific
  Left = 215
  Top = 113
  Height = 150
  Width = 215
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Program Files\Audio\Plugins\VST'
    Tempo = 120.000000000000000000
    VendorVersion = 0
    VstPlugIns = <
      item
        DisplayName = 'Low'
        VstOfflineTasks = <>
        OnAudioMasterAutomate = ParameterLowAutomate
      end
      item
        DisplayName = 'High'
        VstOfflineTasks = <>
        OnAudioMasterAutomate = ParameterHighAutomate
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2300
    Left = 8
    Top = 8
  end
end
