object FmVSTEditor: TFmVSTEditor
  Left = 399
  Top = 298
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'VST Plugin Editor'
  ClientHeight = 124
  ClientWidth = 289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object PnPlugin: TPanel
    Left = 0
    Top = 0
    Width = 289
    Height = 124
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
  end
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Programme\Audio\Plugins\VST'
    Tempo = 120.000000000000000000
    VendorVersion = 0
    VstPlugIns = <
      item
        DisplayName = 'Plugin'
        VstOfflineTasks = <>
        OnCloseEdit = CloseCustomEdit
        OnShowEdit = ShowCustomEdit
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2400
    Left = 96
    Top = 75
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreventClipping = pcAnalog
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    OnLatencyChanged = ASIOHostReset
    OnReset = ASIOHostReset
    Left = 124
    Top = 75
  end
  object XPManifest: TXPManifest
    Left = 152
    Top = 75
  end
  object OD: TOpenDialog
    DefaultExt = 'fxp'
    Filter = 'VST Preset (*.fxp)|*.fxp|VST Bank (*.fxb)|*.fxb'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 208
    Top = 75
  end
  object SD: TSaveDialog
    DefaultExt = 'fxp'
    Filter = 'VST Preset (*.fxp)|*.fxp|VST Bank (*.fxb)|*.fxb'
    Left = 180
    Top = 75
  end
  object MainMenu: TMainMenu
    Left = 68
    Top = 75
    object MiStandalone: TMenuItem
      Caption = '&Standalone'
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
    object GUI1: TMenuItem
      Caption = 'GUI'
      object MiGuiDefault: TMenuItem
        Caption = 'Default'
        Checked = True
        RadioItem = True
        OnClick = MiGuiDefaultClick
      end
      object MiGuiList: TMenuItem
        Caption = 'List'
        RadioItem = True
        OnClick = MiGuiListClick
      end
      object MiGuiSelector: TMenuItem
        Caption = 'Selector'
        RadioItem = True
        OnClick = MiGuiSelectorClick
      end
      object MiGuiCustom: TMenuItem
        Caption = 'Custom'
        RadioItem = True
        OnClick = MiGuiCustomClick
      end
    end
    object MiProgram: TMenuItem
      Caption = '&Presets'
      object MiLoadPreset: TMenuItem
        Caption = '&Load Preset...'
        OnClick = MILoadPresetClick
      end
      object MiSavePreset: TMenuItem
        Caption = '&Save Preset...'
      end
      object N1: TMenuItem
        Caption = '-'
      end
    end
    object MiAudio: TMenuItem
      Caption = 'Audio'
      object MiSetup: TMenuItem
        Caption = 'Setup'
        OnClick = MiSetupClick
      end
    end
  end
end
