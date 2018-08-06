object ViewASIOForm: TViewASIOForm
  Left = 291
  Top = 266
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Demo application for ASIO-Host'
  ClientHeight = 900
  ClientWidth = 1303
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Lb_Drivername: TLabel
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
  object Lb_Copyright: TLabel
    Left = 2
    Top = 41
    Width = 389
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      '(C)opyright in 2004-2008 by  Christian Budde and Tobias Fleische' +
      'r'
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
    Caption = 'Control Panel'
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
    Caption = 'Start Audio'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = BtStartStopClick
  end
  object XSynthPanel: TPanel
    Left = 0
    Top = 80
    Width = 185
    Height = 41
    TabOrder = 4
  end
  object ScrollBar1: TScrollBar
    Left = 769
    Top = 8
    Width = 121
    Height = 21
    Max = 127
    PageSize = 0
    TabOrder = 5
    OnChange = ScrollBar1Change
  end
  object ScrollBar2: TScrollBar
    Tag = 1
    Left = 769
    Top = 35
    Width = 121
    Height = 21
    Max = 127
    PageSize = 0
    TabOrder = 6
    OnChange = ScrollBar1Change
  end
  object Memo1: TMemo
    Left = 248
    Top = 200
    Width = 521
    Height = 361
    Lines.Strings = (
      'Memo1')
    TabOrder = 7
    Visible = False
    WordWrap = False
  end
  object ComboBoxMidi: TComboBox
    Left = 589
    Top = 8
    Width = 174
    Height = 24
    TabOrder = 8
    Text = 'ComboBoxMidi'
  end
  object ASIOHostAudioData: TAsioHostAudioData
    AsioTime.SamplePos = 0
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    ConvertOptimizations = [coSSE, co3DNow]
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostAudioDataBufferSwitch32
    OnSampleRateChanged = ASIOHostAudioDataSampleRateChanged
    Left = 24
    Top = 16
  end
end
