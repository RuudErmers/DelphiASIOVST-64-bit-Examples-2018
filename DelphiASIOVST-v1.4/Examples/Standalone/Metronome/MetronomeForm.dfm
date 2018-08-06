object FmASIO: TFmASIO
  Left = 291
  Top = 266
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Simple Metronome'
  ClientHeight = 81
  ClientWidth = 242
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LbDrivername: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Driver:'
  end
  object LbTempo: TLabel
    Left = 8
    Top = 35
    Width = 36
    Height = 13
    Caption = 'Tempo:'
  end
  object LbBPM: TLabel
    Left = 111
    Top = 36
    Width = 23
    Height = 13
    Caption = 'BPM'
  end
  object LbVolume: TLabel
    Left = 8
    Top = 60
    Width = 38
    Height = 13
    Caption = 'Volume:'
  end
  object DriverCombo: TComboBox
    Left = 45
    Top = 5
    Width = 105
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = DriverComboChange
  end
  object BtControlPanel: TButton
    Left = 156
    Top = 5
    Width = 81
    Height = 21
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = BtControlPanelClick
  end
  object BtPlay: TButton
    Left = 156
    Top = 32
    Width = 81
    Height = 21
    Caption = 'Start Audio'
    Default = True
    Enabled = False
    TabOrder = 2
    OnClick = BtPlayClick
  end
  object SETempo: TSpinEdit
    Left = 50
    Top = 32
    Width = 55
    Height = 22
    MaxValue = 300
    MinValue = 20
    TabOrder = 3
    Value = 120
    OnChange = SETempoChange
  end
  object SBVolume: TScrollBar
    Left = 52
    Top = 59
    Width = 185
    Height = 16
    Max = 0
    Min = -100
    PageSize = 0
    TabOrder = 4
    OnChange = SbVolumeChange
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    ConvertOptimizations = [coSSE]
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 52
    Top = 8
  end
end
