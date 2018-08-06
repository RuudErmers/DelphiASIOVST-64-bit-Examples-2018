object FmAsioBufferdAudioFilePlayer: TFmAsioBufferdAudioFilePlayer
  Left = 459
  Top = 285
  BorderStyle = bsSingle
  Caption = 'Demo application for ASIO-Host & MP3'
  ClientHeight = 134
  ClientWidth = 429
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
    Left = 7
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Driver:'
  end
  object LbChannels: TLabel
    Left = 7
    Top = 36
    Width = 82
    Height = 13
    Caption = 'Output Channels:'
  end
  object LbAudioFile: TLabel
    Left = 8
    Top = 60
    Width = 49
    Height = 13
    Caption = 'Audio File:'
  end
  object LbBuffer: TLabel
    Left = 344
    Top = 36
    Width = 34
    Height = 13
    Caption = 'Buffer: '
    OnClick = LbBufferClick
  end
  object LbBufferValue: TLabel
    Left = 384
    Top = 36
    Width = 29
    Height = 13
    Caption = '100 %'
  end
  object DriverCombo: TComboBox
    Left = 64
    Top = 7
    Width = 273
    Height = 21
    TabOrder = 0
    OnChange = DriverComboChange
  end
  object BtControlPanel: TButton
    Left = 344
    Top = 7
    Width = 81
    Height = 21
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = BtControlPanelClick
  end
  object ChannelBox: TComboBox
    Left = 104
    Top = 32
    Width = 233
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = ChannelBoxChange
  end
  object BtStartStop: TButton
    Left = 2
    Top = 88
    Width = 423
    Height = 41
    Caption = '&Start Audio'
    Enabled = False
    TabOrder = 3
    OnClick = BtStartStopClick
  end
  object EdFile: TEdit
    Left = 83
    Top = 57
    Width = 254
    Height = 21
    TabOrder = 4
    OnChange = EdFileChange
  end
  object BtSelect: TButton
    Left = 344
    Top = 57
    Width = 81
    Height = 21
    Caption = 'Select...'
    TabOrder = 5
    OnClick = BtSelectClick
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 130
    Top = 23
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.wav'
    Filter = 'Wave (*.wav)|*.wav|AIFF (*.aiff)|*.aif*|AU (*.au)|*.au'
    Left = 161
    Top = 24
  end
  object Timer: TTimer
    Interval = 200
    OnTimer = TimerTimer
    Left = 192
    Top = 24
  end
end
