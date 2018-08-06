object FmASIOConfig: TFmASIOConfig
  Left = 569
  Top = 295
  BorderStyle = bsToolWindow
  Caption = 'Config ASIO'
  ClientHeight = 58
  ClientWidth = 197
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    197
    58)
  PixelsPerInch = 96
  TextHeight = 13
  object LbDriver: TLabel
    Left = 7
    Top = 10
    Width = 45
    Height = 13
    Caption = 'Driver:'
  end
  object CBDriver: TComboBox
    Left = 56
    Top = 6
    Width = 136
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 2625542
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ItemHeight = 13
    ParentFont = False
    TabOrder = 0
    OnChange = CBDriverChange
  end
  object BtControlPanel: TButton
    Left = 8
    Top = 32
    Width = 184
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Control Panel'
    TabOrder = 1
    TabStop = False
    OnClick = BtControlPanelClick
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    OnLatencyChanged = ASIOHostLatencyChanged
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 56
    Top = 8
  end
end
