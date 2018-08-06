object FmASIO: TFmASIO
  Tag = 4
  Left = 279
  Top = 98
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Demo application for ASIO-Host'
  ClientHeight = 503
  ClientWidth = 644
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    644
    503)
  PixelsPerInch = 96
  TextHeight = 13
  object LbDrivername: TLabel
    Left = 8
    Top = 13
    Width = 31
    Height = 13
    Caption = 'Driver:'
  end
  object LbChannels: TLabel
    Left = 8
    Top = 37
    Width = 82
    Height = 13
    Caption = 'Output Channels:'
  end
  object ShBackText: TShape
    Left = 36
    Top = 242
    Width = 579
    Height = 45
    Brush.Color = clBtnShadow
    Pen.Color = clBtnShadow
    Shape = stRoundRect
  end
  object MiddleL: TShape
    Left = 58
    Top = 147
    Width = 534
    Height = 2
    Pen.Color = clBtnShadow
  end
  object Lb50: TLabel
    Tag = 4
    Left = 117
    Top = 245
    Width = 24
    Height = 12
    Caption = '50 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb63: TLabel
    Tag = 5
    Left = 135
    Top = 258
    Width = 25
    Height = 11
    Caption = '63 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb80: TLabel
    Tag = 6
    Left = 150
    Top = 271
    Width = 24
    Height = 12
    Caption = '80 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb100: TLabel
    Tag = 7
    Left = 170
    Top = 245
    Width = 29
    Height = 12
    Caption = '100 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb125: TLabel
    Tag = 8
    Left = 188
    Top = 258
    Width = 30
    Height = 11
    Caption = '125 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb160: TLabel
    Tag = 9
    Left = 204
    Top = 271
    Width = 29
    Height = 12
    Caption = '160 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb200: TLabel
    Tag = 10
    Left = 223
    Top = 245
    Width = 29
    Height = 12
    Caption = '200 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb250: TLabel
    Tag = 11
    Left = 243
    Top = 258
    Width = 30
    Height = 11
    Caption = '250 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb500: TLabel
    Tag = 14
    Left = 294
    Top = 258
    Width = 30
    Height = 11
    Caption = '500 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb630: TLabel
    Tag = 15
    Left = 311
    Top = 271
    Width = 29
    Height = 12
    Caption = '630 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb800: TLabel
    Tag = 16
    Left = 332
    Top = 245
    Width = 29
    Height = 12
    Caption = '800 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object LbLM: TLabel
    Left = 8
    Top = 122
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
    Top = 358
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
    Left = 58
    Top = 382
    Width = 528
    Height = 2
    Pen.Color = clBtnShadow
  end
  object Lb24AL: TLabel
    Left = 607
    Top = 225
    Width = 11
    Height = 10
    Caption = '-24'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -8
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Lb12AL: TLabel
    Left = 607
    Top = 152
    Width = 11
    Height = 10
    Caption = '-12'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -8
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Lb12AR: TLabel
    Left = 607
    Top = 386
    Width = 11
    Height = 10
    Caption = '-12'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -8
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Lb24AR: TLabel
    Left = 607
    Top = 460
    Width = 11
    Height = 10
    Caption = '-24'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -8
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Lb0R: TLabel
    Left = 614
    Top = 313
    Width = 4
    Height = 10
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -8
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Lb1k6: TLabel
    Tag = 19
    Left = 382
    Top = 245
    Width = 32
    Height = 12
    Caption = '1.6 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb1k25: TLabel
    Tag = 18
    Left = 365
    Top = 271
    Width = 37
    Height = 12
    Caption = '1.25 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb1k: TLabel
    Tag = 17
    Left = 351
    Top = 258
    Width = 25
    Height = 11
    Caption = '1 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb2k: TLabel
    Tag = 20
    Left = 405
    Top = 258
    Width = 25
    Height = 11
    Caption = '2 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb2k5: TLabel
    Tag = 21
    Left = 418
    Top = 271
    Width = 32
    Height = 12
    Caption = '2.5 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb3k15: TLabel
    Tag = 22
    Left = 437
    Top = 245
    Width = 37
    Height = 12
    Caption = '3.15 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb4k: TLabel
    Tag = 23
    Left = 455
    Top = 258
    Width = 25
    Height = 11
    Caption = '4 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb315: TLabel
    Tag = 12
    Left = 259
    Top = 271
    Width = 29
    Height = 12
    Caption = '315 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb400: TLabel
    Tag = 13
    Left = 278
    Top = 245
    Width = 29
    Height = 12
    Caption = '400 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb5k: TLabel
    Tag = 24
    Left = 475
    Top = 271
    Width = 24
    Height = 12
    Caption = '5 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object PeakMeterLeft: TGuiColorLevelMeter
    Left = 624
    Top = 80
    Width = 12
    Height = 155
    Anchors = [akLeft, akTop, akRight]
    BorderColor = clWindowFrame
    ContrastLuminance = 0.300000011920929000
    Upper = 1.000000000000000000
  end
  object PeakMeterRight: TGuiColorLevelMeter
    Left = 624
    Top = 315
    Width = 12
    Height = 155
    Anchors = [akLeft, akTop, akRight]
    BorderColor = clWindowFrame
    ContrastLuminance = 0.300000011920929000
    Upper = 1.000000000000000000
  end
  object Lb0L: TLabel
    Left = 614
    Top = 80
    Width = 4
    Height = 10
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -8
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Lb20Hz: TLabel
    Left = 44
    Top = 271
    Width = 24
    Height = 12
    Caption = '20 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb25Hz: TLabel
    Tag = 1
    Left = 63
    Top = 245
    Width = 24
    Height = 12
    Caption = '25 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb31Hz: TLabel
    Tag = 2
    Left = 79
    Top = 258
    Width = 33
    Height = 11
    Caption = '31.5 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb40Hz: TLabel
    Tag = 3
    Left = 99
    Top = 271
    Width = 24
    Height = 12
    Caption = '40 Hz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb6k3Hz: TLabel
    Tag = 25
    Left = 492
    Top = 245
    Width = 32
    Height = 12
    Caption = '6.3 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb8kHz: TLabel
    Tag = 26
    Left = 511
    Top = 258
    Width = 25
    Height = 11
    Caption = '8 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb10kHz: TLabel
    Tag = 27
    Left = 528
    Top = 271
    Width = 29
    Height = 12
    Caption = '10 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb12k5Hz: TLabel
    Tag = 28
    Left = 542
    Top = 245
    Width = 37
    Height = 12
    Caption = '12.5 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb16kHz: TLabel
    Tag = 29
    Left = 564
    Top = 258
    Width = 30
    Height = 11
    Caption = '16 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object Lb20kHz: TLabel
    Tag = 30
    Left = 579
    Top = 271
    Width = 29
    Height = 12
    Caption = '20 kHz'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3684408
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
    OnDblClick = LbFrequencyDblClick
  end
  object LedClipR: TGuiLED
    Left = 621
    Top = 293
    Width = 16
    Height = 16
    BorderStrength_Percent = 50.000000000000000000
    Brightness_Percent = 100.000000000000000000
    LEDColor = clRed
    BorderWidth = 2.000000000000000000
    Uniformity_Percent = 33.000000000000000000
    Transparent = False
    OnClick = LedClipRClick
  end
  object LedClipL: TGuiLED
    Left = 621
    Top = 58
    Width = 16
    Height = 16
    BorderStrength_Percent = 50.000000000000000000
    Brightness_Percent = 100.000000000000000000
    LEDColor = clRed
    BorderWidth = 2.000000000000000000
    Uniformity_Percent = 33.000000000000000000
    Transparent = False
    OnClick = LedClipLClick
  end
  object DriverCombo: TComboBox
    Left = 45
    Top = 8
    Width = 506
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = DriverComboChange
  end
  object BtControlPanel: TButton
    Left = 557
    Top = 8
    Width = 79
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = BtControlPanelClick
  end
  object ChannelBox: TComboBox
    Left = 96
    Top = 33
    Width = 455
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = ChannelBoxChange
  end
  object BtStartStop: TButton
    Left = 557
    Top = 33
    Width = 79
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Start Audio'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = BtStartStopClick
  end
  object SB200L: TScrollBar
    Tag = 10
    Left = 229
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 14
    OnChange = SBVolumeChange
  end
  object SB160L: TScrollBar
    Tag = 9
    Left = 211
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 13
    OnChange = SBVolumeChange
  end
  object SB125L: TScrollBar
    Tag = 8
    Left = 193
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 12
    OnChange = SBVolumeChange
  end
  object SB100L: TScrollBar
    Tag = 7
    Left = 175
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 11
    OnChange = SBVolumeChange
  end
  object SB80L: TScrollBar
    Tag = 6
    Left = 157
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 10
    OnChange = SBVolumeChange
  end
  object SB63L: TScrollBar
    Tag = 5
    Left = 139
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 9
    OnChange = SBVolumeChange
  end
  object SB50L: TScrollBar
    Tag = 4
    Left = 121
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 8
    OnChange = SBVolumeChange
  end
  object SB400L: TScrollBar
    Tag = 13
    Left = 283
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 17
    OnChange = SBVolumeChange
  end
  object SB315L: TScrollBar
    Tag = 12
    Left = 265
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 16
    OnChange = SBVolumeChange
  end
  object SB250L: TScrollBar
    Tag = 11
    Left = 247
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 15
    OnChange = SBVolumeChange
  end
  object SB500L: TScrollBar
    Tag = 14
    Left = 301
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 18
    OnChange = SBVolumeChange
  end
  object SB200R: TScrollBar
    Tag = 41
    Left = 229
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 45
    OnChange = SBVolumeChange
  end
  object SB160R: TScrollBar
    Tag = 40
    Left = 211
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 44
    OnChange = SBVolumeChange
  end
  object SB125R: TScrollBar
    Tag = 39
    Left = 193
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 43
    OnChange = SBVolumeChange
  end
  object SB100R: TScrollBar
    Tag = 38
    Left = 175
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 42
    OnChange = SBVolumeChange
  end
  object SB80R: TScrollBar
    Tag = 37
    Left = 157
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 41
    OnChange = SBVolumeChange
  end
  object SB63R: TScrollBar
    Tag = 36
    Left = 139
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 40
    OnChange = SBVolumeChange
  end
  object SB50R: TScrollBar
    Tag = 35
    Left = 121
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 39
    OnChange = SBVolumeChange
  end
  object SB400R: TScrollBar
    Tag = 44
    Left = 283
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 48
    OnChange = SBVolumeChange
  end
  object SB315R: TScrollBar
    Tag = 43
    Left = 265
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 47
    OnChange = SBVolumeChange
  end
  object SB250R: TScrollBar
    Tag = 42
    Left = 247
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 46
    OnChange = SBVolumeChange
  end
  object SB500R: TScrollBar
    Tag = 45
    Left = 301
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 49
    OnChange = SBVolumeChange
  end
  object SB2kR: TScrollBar
    Tag = 51
    Left = 409
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 55
    OnChange = SBVolumeChange
  end
  object SB1k6R: TScrollBar
    Tag = 50
    Left = 391
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 54
    OnChange = SBVolumeChange
  end
  object SB1k25R: TScrollBar
    Tag = 49
    Left = 373
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 53
    OnChange = SBVolumeChange
  end
  object SB1kR: TScrollBar
    Tag = 48
    Left = 355
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 52
    OnChange = SBVolumeChange
  end
  object SB800R: TScrollBar
    Tag = 47
    Left = 337
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 51
    OnChange = SBVolumeChange
  end
  object SB630R: TScrollBar
    Tag = 46
    Left = 319
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 50
    OnChange = SBVolumeChange
  end
  object SB4kR: TScrollBar
    Tag = 54
    Left = 463
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 58
    OnChange = SBVolumeChange
  end
  object SB3k15R: TScrollBar
    Tag = 53
    Left = 445
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 57
    OnChange = SBVolumeChange
  end
  object SB2k5R: TScrollBar
    Tag = 52
    Left = 427
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 56
    OnChange = SBVolumeChange
  end
  object SB5kR: TScrollBar
    Tag = 55
    Left = 481
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 59
    OnChange = SBVolumeChange
  end
  object SB2kL: TScrollBar
    Tag = 20
    Left = 409
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 24
    OnChange = SBVolumeChange
  end
  object SB1k6L: TScrollBar
    Tag = 19
    Left = 391
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 23
    OnChange = SBVolumeChange
  end
  object SB1k25L: TScrollBar
    Tag = 18
    Left = 373
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 22
    OnChange = SBVolumeChange
  end
  object SB1kL: TScrollBar
    Tag = 17
    Left = 355
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 21
    OnChange = SBVolumeChange
  end
  object SB800L: TScrollBar
    Tag = 16
    Left = 337
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 20
    OnChange = SBVolumeChange
  end
  object SB630L: TScrollBar
    Tag = 15
    Left = 319
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 19
    OnChange = SBVolumeChange
  end
  object SB4kL: TScrollBar
    Tag = 23
    Left = 463
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 27
    OnChange = SBVolumeChange
  end
  object SB3k15L: TScrollBar
    Tag = 22
    Left = 445
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 26
    OnChange = SBVolumeChange
  end
  object SB2k5L: TScrollBar
    Tag = 21
    Left = 427
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 25
    OnChange = SBVolumeChange
  end
  object SB5kL: TScrollBar
    Tag = 24
    Left = 481
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 28
    OnChange = SBVolumeChange
  end
  object SB40L: TScrollBar
    Tag = 3
    Left = 103
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 7
    OnChange = SBVolumeChange
  end
  object SB31L: TScrollBar
    Tag = 2
    Left = 85
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 6
    OnChange = SBVolumeChange
  end
  object SB25L: TScrollBar
    Tag = 1
    Left = 67
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 5
    OnChange = SBVolumeChange
  end
  object SB20L: TScrollBar
    Left = 49
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 4
    OnChange = SBVolumeChange
  end
  object SB6k3L: TScrollBar
    Tag = 25
    Left = 499
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 29
    OnChange = SBVolumeChange
  end
  object SB12k5L: TScrollBar
    Tag = 28
    Left = 553
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 32
    OnChange = SBVolumeChange
  end
  object SB10kL: TScrollBar
    Tag = 27
    Left = 535
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 31
    OnChange = SBVolumeChange
  end
  object SB8kL: TScrollBar
    Tag = 26
    Left = 517
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 30
    OnChange = SBVolumeChange
  end
  object SB16kL: TScrollBar
    Tag = 29
    Left = 571
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 33
    OnChange = SBVolumeChange
  end
  object SB20kL: TScrollBar
    Tag = 30
    Left = 589
    Top = 58
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 34
    OnChange = SBVolumeChange
  end
  object SB40R: TScrollBar
    Tag = 34
    Left = 103
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 38
    OnChange = SBVolumeChange
  end
  object SB31R: TScrollBar
    Tag = 33
    Left = 85
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 37
    OnChange = SBVolumeChange
  end
  object SB25R: TScrollBar
    Tag = 32
    Left = 67
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 36
    OnChange = SBVolumeChange
  end
  object SB20R: TScrollBar
    Tag = 31
    Left = 49
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 35
    OnChange = SBVolumeChange
  end
  object SB6k3R: TScrollBar
    Tag = 56
    Left = 499
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 60
    OnChange = SBVolumeChange
  end
  object SB12k5R: TScrollBar
    Tag = 59
    Left = 553
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 63
    OnChange = SBVolumeChange
  end
  object SB10kR: TScrollBar
    Tag = 58
    Left = 535
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 62
    OnChange = SBVolumeChange
  end
  object SB8kR: TScrollBar
    Tag = 57
    Left = 517
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 61
    OnChange = SBVolumeChange
  end
  object SB16kR: TScrollBar
    Tag = 60
    Left = 571
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 64
    OnChange = SBVolumeChange
  end
  object SB20kR: TScrollBar
    Tag = 61
    Left = 589
    Top = 293
    Width = 14
    Height = 177
    Kind = sbVertical
    PageSize = 0
    Position = 100
    TabOrder = 65
    OnChange = SBVolumeChange
  end
  object BtMute: TButton
    Left = 48
    Top = 476
    Width = 67
    Height = 21
    Caption = '&Mute'
    TabOrder = 66
    OnClick = BtMuteClick
  end
  object BtAllThirdOctaves: TButton
    Left = 121
    Top = 476
    Width = 156
    Height = 21
    Caption = '&All Third Octaves (equal level)'
    TabOrder = 67
    OnClick = BtAllThirdOctavesClick
  end
  object BtAllOctaves: TButton
    Left = 285
    Top = 476
    Width = 136
    Height = 21
    Caption = '&All Octaves (equal level)'
    TabOrder = 68
    OnClick = BtAllOctavesClick
  end
  object CbLinkChannels: TCheckBox
    Left = 427
    Top = 478
    Width = 86
    Height = 17
    Caption = 'Link Channels'
    Enabled = False
    TabOrder = 69
  end
  object BtExport: TButton
    Left = 573
    Top = 476
    Width = 63
    Height = 21
    Caption = 'E&xport...'
    TabOrder = 70
    OnClick = BtExportClick
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreFillOutBuffer = bpfZero
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    OnReset = ASIOHostReset
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 24
    Top = 40
  end
  object MeterTimer: TTimer
    Interval = 30
    OnTimer = MeterTimerTimer
    Left = 610
    Top = 247
  end
end
