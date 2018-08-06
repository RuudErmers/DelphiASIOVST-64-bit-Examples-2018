object FmASIO: TFmASIO
  Left = 250
  Top = 188
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Demo application for ASIO-Host'
  ClientHeight = 422
  ClientWidth = 587
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
  object Lb_Drivername: TLabel
    Left = 7
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Driver:'
  end
  object ShBackText: TShape
    Left = 49
    Top = 219
    Width = 524
    Height = 15
    Brush.Color = clBtnShadow
    Pen.Color = clBtnShadow
    Shape = stRoundRect
  end
  object MiddleL: TShape
    Left = 70
    Top = 126
    Width = 475
    Height = 2
    Pen.Color = clBtnShadow
  end
  object Lb20: TLabel
    Left = 53
    Top = 220
    Width = 32
    Height = 13
    Caption = '20 Hz'
    Color = clBtnShadow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Lb40: TLabel
    Left = 103
    Top = 220
    Width = 32
    Height = 13
    Caption = '40 Hz'
    Color = clBtnShadow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Lb80: TLabel
    Left = 151
    Top = 220
    Width = 32
    Height = 13
    Caption = '80 Hz'
    Color = clBtnShadow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Lb160: TLabel
    Left = 196
    Top = 220
    Width = 39
    Height = 13
    Caption = '160 Hz'
    Color = clBtnShadow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Lb320: TLabel
    Left = 244
    Top = 220
    Width = 39
    Height = 13
    Caption = '320 Hz'
    Color = clBtnShadow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Lb640: TLabel
    Left = 293
    Top = 220
    Width = 39
    Height = 13
    Caption = '640 Hz'
    Color = clBtnShadow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Lb1200: TLabel
    Left = 340
    Top = 220
    Width = 39
    Height = 13
    Caption = '1.2kHz'
    Color = clBtnShadow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Lb2500: TLabel
    Left = 386
    Top = 220
    Width = 43
    Height = 13
    Caption = '2.5 kHz'
    Color = clBtnShadow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Lb5k: TLabel
    Left = 439
    Top = 220
    Width = 32
    Height = 13
    Caption = '5 kHz'
    Color = clBtnShadow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Lb10k: TLabel
    Left = 483
    Top = 220
    Width = 39
    Height = 13
    Caption = '10 kHz'
    Color = clBtnShadow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object Lb20k: TLabel
    Left = 530
    Top = 220
    Width = 39
    Height = 13
    Caption = '20 kHz'
    Color = clBtnShadow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object LbLM: TLabel
    Left = 8
    Top = 100
    Width = 27
    Height = 52
    Caption = 'L'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -43
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = False
  end
  object LbRS: TLabel
    Left = 8
    Top = 300
    Width = 34
    Height = 52
    Caption = 'R'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -43
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = False
  end
  object MiddleR: TShape
    Left = 70
    Top = 326
    Width = 475
    Height = 2
    Pen.Color = clBtnShadow
  end
  object DriverCombo: TComboBox
    Left = 56
    Top = 7
    Width = 264
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = DriverComboChange
  end
  object BtPlay: TButton
    Left = 464
    Top = 8
    Width = 96
    Height = 21
    Caption = 'Start Audio'
    Default = True
    Enabled = False
    TabOrder = 1
    OnClick = BtPlayClick
  end
  object SB1200L: TScrollBar
    Tag = 6
    Left = 344
    Top = 38
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 2
    OnChange = SliderChange
  end
  object SB640L: TScrollBar
    Tag = 5
    Left = 296
    Top = 38
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 3
    OnChange = SliderChange
  end
  object SB320L: TScrollBar
    Tag = 4
    Left = 248
    Top = 38
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 4
    OnChange = SliderChange
  end
  object SB160L: TScrollBar
    Tag = 3
    Left = 200
    Top = 38
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 5
    OnChange = SliderChange
  end
  object SB80L: TScrollBar
    Tag = 2
    Left = 152
    Top = 38
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 6
    OnChange = SliderChange
  end
  object SB40L: TScrollBar
    Tag = 1
    Left = 104
    Top = 38
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 7
    OnChange = SliderChange
  end
  object SB20L: TScrollBar
    Left = 56
    Top = 38
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 8
    OnChange = SliderChange
  end
  object SB10kL: TScrollBar
    Tag = 9
    Left = 488
    Top = 38
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 9
    OnChange = SliderChange
  end
  object SB5kL: TScrollBar
    Tag = 8
    Left = 440
    Top = 38
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 10
    OnChange = SliderChange
  end
  object SB2500L: TScrollBar
    Tag = 7
    Left = 392
    Top = 38
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 11
    OnChange = SliderChange
  end
  object SB20kL: TScrollBar
    Tag = 10
    Left = 536
    Top = 38
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 12
    OnChange = SliderChange
  end
  object SB1200R: TScrollBar
    Tag = 17
    Left = 344
    Top = 238
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 13
    OnChange = SliderChange
  end
  object SB640R: TScrollBar
    Tag = 16
    Left = 296
    Top = 238
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 14
    OnChange = SliderChange
  end
  object SB320R: TScrollBar
    Tag = 15
    Left = 248
    Top = 238
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 15
    OnChange = SliderChange
  end
  object SB160R: TScrollBar
    Tag = 14
    Left = 200
    Top = 238
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 16
    OnChange = SliderChange
  end
  object SB80R: TScrollBar
    Tag = 13
    Left = 152
    Top = 238
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 17
    OnChange = SliderChange
  end
  object SB40R: TScrollBar
    Tag = 12
    Left = 104
    Top = 238
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 18
    OnChange = SliderChange
  end
  object SB20R: TScrollBar
    Tag = 11
    Left = 56
    Top = 238
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 19
    OnChange = SliderChange
  end
  object SB10kR: TScrollBar
    Tag = 20
    Left = 488
    Top = 238
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 20
    OnChange = SliderChange
  end
  object SB5kR: TScrollBar
    Tag = 19
    Left = 440
    Top = 238
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 21
    OnChange = SliderChange
  end
  object SB2500R: TScrollBar
    Tag = 18
    Left = 392
    Top = 238
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 22
    OnChange = SliderChange
  end
  object SB20kR: TScrollBar
    Tag = 21
    Left = 536
    Top = 238
    Width = 24
    Height = 177
    Kind = sbVertical
    Max = 150
    Min = -150
    PageSize = 0
    TabOrder = 23
    OnChange = SliderChange
  end
  object BtControlPanel: TButton
    Left = 344
    Top = 8
    Width = 97
    Height = 21
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 24
    OnClick = BtControlPanelClick
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 284
    Top = 32
  end
end
