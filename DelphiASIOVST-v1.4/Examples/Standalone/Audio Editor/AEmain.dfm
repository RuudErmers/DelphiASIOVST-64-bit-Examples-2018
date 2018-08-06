object FmAudioEditor: TFmAudioEditor
  Left = 286
  Top = 92
  Caption = 'Simple Audio Editor'
  ClientHeight = 322
  ClientWidth = 712
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GuiLevelMeter: TGuiLevelMeter
    Left = 650
    Top = 30
    Width = 62
    Height = 292
    Align = alRight
    BarWidthPercentage = 0.800000011920929000
    MaximumPeakLevel = 1.000000000000000000
    PeakLevel = 0.500000000000000000
  end
  object Splitter1: TSplitter
    Left = 647
    Top = 30
    Height = 292
    Align = alRight
  end
  object GuiAudioDataDisplay: TGuiAudioDataDisplay
    Left = 0
    Top = 30
    Width = 647
    Height = 292
    Align = alClient
    AntiAlias = gaaLinear4x
    AudioDataCollection = AudioDataCollection32
    DisplayChannels = <>
    LineColor = clMaroon
    LineWidth = 0
    Normalize = False
    XAxis.SampleUpper = 8191
    XAxis.FractionalLower = -0.500000000000000000
    XAxis.FractionalUpper = 0.500000000000000000
  end
  object ControlBar1: TControlBar
    Left = 0
    Top = 0
    Width = 712
    Height = 30
    Align = alTop
    AutoSize = True
    TabOrder = 0
    object ToolBar1: TToolBar
      Left = 11
      Top = 2
      Width = 150
      Height = 22
      Caption = 'ToolBar1'
      TabOrder = 0
      object SpeedButton1: TSpeedButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Glyph.Data = {
          96010000424D9601000000000000760000002800000018000000180000000100
          04000000000020010000000000000000000010000000000000000080000000FF
          0000FFFFFF000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00222222222222
          2222222222222222222222222222222222222222022222222222222222222222
          0002222222222222222222220110222222222222222222220111002222222222
          2222222201111102222222222222222201111110222222222222222201111111
          0022222222222222011111111102222222222222011111111110022222222222
          0111111111111022222222220111111111111022222222220111111111100222
          2222222201111111110222222222222201111111002222222222222201111110
          2222222222222222011111022222222222222222011100222222222222222222
          0110222222222222222222220002222222222222222222220222222222222222
          2222222222222222222222222222222222222222222222222222}
      end
    end
    object ToolBar2: TToolBar
      Left = 174
      Top = 2
      Width = 150
      Height = 22
      Caption = 'ToolBar2'
      TabOrder = 1
    end
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 32
    object MIFile: TMenuItem
      Caption = '&File'
      object MINew: TMenuItem
        Caption = '&New'
      end
      object MIOpen: TMenuItem
        Caption = 'Open...'
        OnClick = MIOpenClick
      end
      object MISave: TMenuItem
        Caption = 'Save'
      end
      object MISaveAs: TMenuItem
        Caption = 'Save as...'
        OnClick = MISaveAsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MIExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MIExitClick
      end
    end
    object MIEdit: TMenuItem
      Caption = '&Edit'
      object MIUndo: TMenuItem
        Caption = '&Undo'
      end
      object MIRedo: TMenuItem
        Caption = '&Redo'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MIAusschneiden: TMenuItem
        Caption = '&Cut'
      end
      object MIKopieren: TMenuItem
        Caption = '&Copy'
      end
      object MIPaste: TMenuItem
        Caption = '&Paste'
      end
      object MIDelete: TMenuItem
        Caption = 'Delete'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MISelectAll: TMenuItem
        Caption = 'Select &All'
      end
      object MISelectNone: TMenuItem
        Caption = 'Select &None'
      end
    end
    object MIGenerate: TMenuItem
      Caption = '&Generate'
      object MINoise: TMenuItem
        Caption = '&Noise'
        object WhiteNoise1: TMenuItem
          Caption = '&White Noise'
          OnClick = MIWhiteNoiseClick
        end
        object MIPinkNoise: TMenuItem
          Caption = '&Pink Noise'
        end
      end
      object MIWaveform: TMenuItem
        Caption = 'Waveform'
      end
    end
    object MIProcess: TMenuItem
      Caption = '&Process'
      object MINormalize: TMenuItem
        Caption = '&Normalize'
        OnClick = MINormalizeClick
      end
      object MIRectify: TMenuItem
        Caption = '&Rectify'
        OnClick = MIRectifyClick
      end
      object MIRemoveDC: TMenuItem
        Caption = 'Remove &DC'
        OnClick = MIRemoveDCClick
      end
      object MIInvert: TMenuItem
        Caption = '&Invert'
        OnClick = MIInvertClick
      end
    end
    object MIView: TMenuItem
      Caption = 'View'
    end
    object MIOptions: TMenuItem
      Caption = '&Options'
      object MIASIOSetup: TMenuItem
        Bitmap.Data = {
          F6060000424DF606000000000000360000002800000018000000180000000100
          180000000000C006000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA4A0A0FFFFFFFFFFFFFFFFFFFFFFFFA4
          A0A0FFFFFFFFFFFFA4A0A0A4A0A0A4A0A0FFFFFFFFFFFFFFFFFFA4A0A0A4A0A0
          A4A0A0FFFFFFFFFFFFA4A0A0A4A0A0FFFFFFFFFFFF000000A4A0A0FFFFFFFFFF
          FFFFFFFF000000A4A0A0FFFFFF000000000000000000FFFFFFA4A0A0FFFFFF00
          0000000000000000FFFFFFA4A0A0000000000000FFFFFFFFFFFFFFFFFF000000
          A4A0A0FFFFFFFFFFFFFFFFFF000000A4A0A0000000A4A0A0FFFFFFFFFFFF0000
          00A4A0A0000000FFFFFFFFFFFFFFFFFF000000FFFFFF000000A4A0A0FFFFFFFF
          FFFFFFFFFF000000A4A0A0FFFFFFFFFFFFFFFFFF000000A4A0A0000000A4A0A0
          FFFFFFFFFFFF000000A4A0A0FFFFFFFFFFFFA4A0A0000000FFFFFFFFFFFF0000
          00A4A0A0FFFFFFFFFFFFFFFFFF000000A4A0A0A4A0A0A4A0A0A4A0A0000000A4
          A0A0000000A4A0A0FFFFFFFFFFFF000000A4A0A0FFFFFF000000000000FFFFFF
          FFFFFFA4A0A0000000A4A0A0FFFFFFFFFFFFFFFFFF0000000000000000000000
          00000000000000A4A0A0000000FFFFFFA4A0A0A4A0A0000000FFFFFF000000FF
          FFFFA4A0A0A4A0A0000000FFFFFF000000A4A0A0A4A0A0FFFFFFFFFFFF000000
          A4A0A0FFFFFFFFFFFFFFFFFF000000A4A0A0FFFFFF000000000000000000FFFF
          FFFFFFFFFFFFFF000000000000000000FFFFFF000000000000000000FFFFFFFF
          FFFFFFFFFF000000A4A0A0FFFFFFFFFFFFFFFFFF000000A4A0A0FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00A4A0A0FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFF000000FF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFA4A0A0A4A0A0A4A0A0A4A0A0A4A0A0A4A0A0A4A0A0A4A0A0A4A0A0A4A0A0
          A4A0A0A4A0A0A4A0A0A4A0A0A4A0A0A4A0A080A08080808080A080A4A0A080A0
          8080808080808080808080A080A4A0A0C0C0C0C0C0C0C0C0C0A4A0A0C0C0C0C0
          C0C0A4A0A0A4A0A0C0C0C0C0DCC0C0C0C0A4A0A0A4A0A0A4A0A0A4A0A0A4A0A0
          808080806060806060808080808080808080404040404040C0C0C0C0C0C0A4A0
          A0000000404040404040000000404040000000A4A0A0C0C0C0402020402020A4
          A0A0A4A0A0404040000000404040402020000000806060808080808080000000
          A4A0A0C0C0C0806060000000A4A0A0808080C0DCC0FFFFFF404040000000F0FB
          FF402020402020C0C0C0404040000000808080A4A0A0A4A0A040404000000080
          6060A4A0A0402020402020404040000000000000C0DCC0F0FBFFF0FBFFC0C0C0
          000000000000F0FBFF402020402020C0C0C0000000000000A4A0A0A4A0A0A4A0
          A0806060000000402020C0C0C0808080404040C0DCC0000000404040F0FBFFC0
          DCC0C0C0C0808080000000808080F0FBFF402020402020C0C0A0000000000000
          A4A0A0A4A0A0A4A0A0808080000000000000A4A0A0C0C0C04020208060600000
          00808080F0FBFFA4A0A0A4A0A0808080C0C0C0F0FBFFC0C0C0402020402020C0
          C0C0000000000000A4A0A0A4A0A0A4A0A0806080000000402020A4A0A0C0C0C0
          806060000000404040C0C0C0F0FBFF806080806060F0FBFFC0C0C0A4A0A0C0DC
          C0402020402020C0C0C0806060000000806060A4A0A080808040404000000080
          6060A4A0A0C0C0C0A4A0A0000000806060C0DCC0F0FBFFC0C0C0402020806060
          402020806060C0DCC0000000402020C0C0C0A4A0A08060600000004040404020
          20000000404040808080A4A0A0A4A0A0A4A0A0A4A0A0A4A0A0C0C0C0C0C0C0C0
          DCC0C0C0C0808080808080C0C0C0C0C0C0808080808080A4A0A0A4A0A0A4A0A0
          808080806060806060808080808080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        Caption = '&ASIO Setup...'
        OnClick = MIASIOSetupClick
      end
      object MIVstSetup: TMenuItem
        Caption = 'VST Setup...'
        OnClick = MIVstSetupClick
      end
    end
    object MIHelp: TMenuItem
      Caption = '&Help'
      object MIAbout: TMenuItem
        Caption = '&About'
      end
    end
  end
  object AudioDataCollection32: TAudioDataCollection32
    Channels = <
      item
        DisplayName = 'Channel 1'
      end>
    SampleFrames = 8192
    SampleRate = 44100.000000000000000000
    OnDataChanged = DataChangedHandler
    Left = 72
    Top = 32
  end
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Program Files\Audio\Plugins\VST'
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
    Left = 104
    Top = 32
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    SampleRate = 44100.000000000000000000
    Left = 40
    Top = 32
  end
end
