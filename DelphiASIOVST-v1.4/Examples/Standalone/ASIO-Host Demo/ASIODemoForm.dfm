object FmASIO: TFmASIO
  Left = 291
  Top = 266
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Demo application for ASIO-Host'
  ClientHeight = 271
  ClientWidth = 590
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object LbDrivername: TLabel
    Left = 9
    Top = 15
    Width = 39
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Driver:'
  end
  object LbChannels: TLabel
    Left = 9
    Top = 44
    Width = 100
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Output Channels:'
  end
  object LbCopyright: TLabel
    Left = 107
    Top = 249
    Width = 325
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '(C)opyright in 2004-2010 by  Delphi ASIO && VST Project'
  end
  object LbFreq: TLabel
    Left = 10
    Top = 89
    Width = 117
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Frequency: 1000 Hz'
  end
  object LbVolume: TLabel
    Left = 10
    Top = 138
    Width = 150
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Volume: 1,00 equals 0 dB'
  end
  object LbPanorama: TLabel
    Left = 10
    Top = 187
    Width = 78
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Panorama: C'
  end
  object DriverCombo: TComboBox
    Left = 79
    Top = 9
    Width = 336
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
    TabOrder = 0
    OnChange = DriverComboChange
  end
  object BtControlPanel: TButton
    Left = 433
    Top = 9
    Width = 149
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = BtControlPanelClick
  end
  object ChannelBox: TComboBox
    Left = 128
    Top = 39
    Width = 287
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
    TabOrder = 2
    OnChange = ChannelBoxChange
  end
  object BtStartStop: TButton
    Left = 433
    Top = 39
    Width = 149
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Start Audio'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = BtStartStopClick
  end
  object SbFreq: TScrollBar
    Left = 10
    Top = 108
    Width = 568
    Height = 20
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    LargeChange = 1000
    Max = 100000
    PageSize = 0
    Position = 56633
    SmallChange = 10
    TabOrder = 4
    OnChange = SbFreqChange
  end
  object SbVolume: TScrollBar
    Left = 10
    Top = 158
    Width = 568
    Height = 19
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Max = 100000
    PageSize = 0
    Position = 100000
    TabOrder = 5
    OnChange = SbVolumeChange
  end
  object SbPan: TScrollBar
    Left = 10
    Top = 207
    Width = 568
    Height = 19
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    PageSize = 0
    Position = 50
    TabOrder = 6
    OnChange = SbPanChange
  end
  object ASIOHost: TAsioHost
    AsioTime.SamplePos = 0
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreFillOutBuffer = bpfZero
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    OnReset = ASIOHostReset
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 8
    Top = 8
  end
end
