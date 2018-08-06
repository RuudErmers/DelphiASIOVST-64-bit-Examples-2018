object FmAnalyser: TFmAnalyser
  Left = 291
  Top = 266
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Simple ASIO Third Octave Analyser'
  ClientHeight = 326
  ClientWidth = 446
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
    446
    326)
  PixelsPerInch = 96
  TextHeight = 13
  object Lb_Drivername: TLabel
    Left = 7
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Driver:'
  end
  object Lb_Channels: TLabel
    Left = 7
    Top = 36
    Width = 77
    Height = 13
    Caption = 'Output Channel:'
  end
  object LbSpeed: TLabel
    Left = 7
    Top = 64
    Width = 34
    Height = 13
    Caption = 'Speed:'
  end
  object LbFullscale: TLabel
    Left = 205
    Top = 64
    Width = 53
    Height = 13
    Caption = 'Fullscale = '
  end
  object Lb_dB: TLabel
    Left = 322
    Top = 64
    Width = 13
    Height = 13
    Caption = 'dB'
  end
  object DriverCombo: TComboBox
    Left = 64
    Top = 7
    Width = 273
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = DriverComboChange
  end
  object Bt_CP: TButton
    Left = 350
    Top = 8
    Width = 91
    Height = 21
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = Bt_CPClick
  end
  object ChannelBox: TComboBox
    Left = 104
    Top = 32
    Width = 233
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
  object Bt_Analyse: TButton
    Left = 350
    Top = 32
    Width = 91
    Height = 50
    Caption = 'Analyse'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = Bt_AnalyseClick
  end
  object AnalyserChart: TChart
    Left = 7
    Top = 88
    Width = 434
    Height = 231
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    LeftAxis.Automatic = False
    LeftAxis.AutomaticMaximum = False
    LeftAxis.AutomaticMinimum = False
    LeftAxis.Maximum = 140.000000000000000000
    LeftAxis.Title.Caption = 'Magnitude [dB]'
    View3D = False
    View3DWalls = False
    TabOrder = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnDblClick = AnalyserChartDblClick
    object BarSeries: TBarSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Visible = False
      ShowInLegend = False
      Gradient.Direction = gdTopBottom
      MultiBar = mbNone
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Balken'
      YValues.Order = loNone
    end
  end
  object RB_Fast: TRadioButton
    Left = 49
    Top = 63
    Width = 40
    Height = 17
    Caption = '&Fast'
    TabOrder = 5
    OnClick = RB_FastClick
  end
  object RB_Medium: TRadioButton
    Left = 91
    Top = 63
    Width = 55
    Height = 17
    Caption = '&Medium'
    Checked = True
    TabOrder = 6
    TabStop = True
    OnClick = RB_MediumClick
  end
  object RB_Slow: TRadioButton
    Left = 150
    Top = 63
    Width = 46
    Height = 17
    Caption = '&Slow'
    TabOrder = 7
    OnClick = RB_SlowClick
  end
  object SEFullscaleGain: TSpinEdit
    Left = 262
    Top = 60
    Width = 56
    Height = 22
    MaxValue = 200
    MinValue = 0
    TabOrder = 8
    Value = 120
    OnChange = SEFullscaleGainChange
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreFillOutBuffer = bpfZero
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    Left = 252
    Top = 24
  end
end
