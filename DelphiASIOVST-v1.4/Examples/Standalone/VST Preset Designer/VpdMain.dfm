object FmVstPresetDesigner: TFmVstPresetDesigner
  Left = 218
  Top = 77
  BorderIcons = [biSystemMenu]
  Caption = 'VST Preset Designer'
  ClientHeight = 116
  ClientWidth = 255
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 255
    Height = 29
    Caption = 'ToolBar'
    TabOrder = 0
    Visible = False
  end
  object VSTPanel: TPanel
    Left = 0
    Top = 29
    Width = 255
    Height = 87
    Align = alClient
    BevelOuter = bvNone
    Caption = '(empty)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 6695441
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = True
    ParameterQuantization = 0
    PlugInDir = 'C:\Program Files\Audio\Plugins\VST\=Delphi='
    Tempo = 120.000000000000000000
    VendorVersion = 0
    VstPlugIns = <
      item
        DisplayName = 'Plugin'
        VstOfflineTasks = <>
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2300
    Left = 16
    Top = 8
  end
  object MainMenu: TMainMenu
    Left = 48
    Top = 8
    object MIVstPlugin: TMenuItem
      Caption = '&VST Plugin'
      object MiOpen: TMenuItem
        Caption = 'Open...'
        OnClick = MiOpenClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object N4: TMenuItem
        Caption = '-'
        Visible = False
      end
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
    object MiProgram: TMenuItem
      Caption = '&Program'
      object MiProgramLoad: TMenuItem
        Caption = '&Load...'
        OnClick = MiProgramLoadClick
      end
      object MiProgramSave: TMenuItem
        Caption = '&Save...'
        OnClick = MiProgramSaveClick
      end
      object MiEmbed: TMenuItem
        Caption = 'Embed'
        Visible = False
        OnClick = MiEmbedClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MiProgramRename: TMenuItem
        Caption = 'Rename...'
        OnClick = MiProgramRenameClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MiProgramRandomize: TMenuItem
        Caption = '&Randomize!'
        OnClick = MiProgramRandomizeClick
      end
      object MiProgramShuffle: TMenuItem
        Caption = 'S&huffle!'
        OnClick = MiProgramShuffleClick
      end
      object MiProgramDesign: TMenuItem
        Caption = 'Design...'
        Visible = False
        OnClick = MiProgramDesignClick
      end
      object MiProgramSplitter: TMenuItem
        Caption = '-'
      end
    end
    object MiBank: TMenuItem
      Caption = '&Bank'
      object MiBankLoad: TMenuItem
        Caption = '&Load...'
        OnClick = MiBankLoadClick
      end
      object MiBankSave: TMenuItem
        Caption = '&Save...'
        OnClick = MiBankSaveClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object MiBankRandomize: TMenuItem
        Caption = '&Randomize!'
        OnClick = MiBankRandomizeClick
      end
      object MiBankShuffle: TMenuItem
        Caption = 'S&huffle!'
        OnClick = MiBankShuffleClick
      end
      object MiBankDesign: TMenuItem
        Caption = 'Design...'
        Visible = False
      end
    end
    object MiPreview: TMenuItem
      Caption = 'Pre&view'
      object MiOpenMIDI: TMenuItem
        Caption = 'Open &MIDI...'
        OnClick = MiOpenMIDIClick
      end
      object MiOpenAudio: TMenuItem
        Caption = '&Open Audio...'
        OnClick = MiOpenAudioClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MiASIO: TMenuItem
        Caption = 'ASIO'
        object MiAsioSplitter: TMenuItem
          Caption = '-'
        end
        object MiAsioControlPanel: TMenuItem
          Caption = '&Control Panel'
          OnClick = MiAsioControlPanelClick
        end
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MiPlayPreview: TMenuItem
        Caption = 'Preview!'
        OnClick = MiPlayPreviewClick
      end
    end
  end
  object OpenVSTDialog: TOpenDialog
    DefaultExt = '.dll'
    Filter = 'VST Plugin (*.dll)|*.dll'
    Title = 'Select a VST Plugin'
    Left = 80
    Top = 8
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.fxb'
    Filter = 'VST Bank (*.fxb)|*.fxb|VST Preset (*.fxp)|*.fxp'
    Title = 'Save as preset/bank'
    Left = 48
    Top = 64
  end
  object OpenAudio: TOpenDialog
    DefaultExt = '.mp3'
    Filter = 'MP3 File (*.mp3)|*.mp3'
    Title = 'Select an MP3 File'
    Left = 112
    Top = 8
  end
  object OpenMidi: TOpenDialog
    DefaultExt = '.mid'
    Filter = 'MIDI (*.mid)|*.mid'
    Title = 'Select a MIDI file'
    Left = 144
    Top = 8
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    Left = 176
    Top = 8
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.dll'
    Filter = 'VST Bank (*.fxb)|*.fxb|VST Preset (*.fxp)|*.fxp'
    Title = 'Select a VST Plugin'
    Left = 16
    Top = 64
  end
end
