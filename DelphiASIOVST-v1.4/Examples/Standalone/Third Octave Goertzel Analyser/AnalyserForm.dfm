object FmAnalyser: TFmAnalyser
  Left = 287
  Top = 277
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
  object LbDriverName: TLabel
    Left = 7
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Driver:'
  end
  object LbChannels: TLabel
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
  object LbFullScaleUnit: TLabel
    Left = 322
    Top = 64
    Width = 13
    Height = 13
    Caption = 'dB'
  end
  object CbDriver: TComboBox
    Left = 64
    Top = 7
    Width = 273
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = CbDriverChange
  end
  object BtControlPanel: TButton
    Left = 350
    Top = 8
    Width = 91
    Height = 21
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = BtControlPanelClick
  end
  object CbChannel: TComboBox
    Left = 104
    Top = 32
    Width = 233
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
  object BtAnalyse: TButton
    Left = 350
    Top = 32
    Width = 91
    Height = 50
    Caption = 'Analyse'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = BtAnalyseClick
  end
  object RbFast: TRadioButton
    Left = 49
    Top = 63
    Width = 40
    Height = 17
    Caption = '&Fast'
    TabOrder = 4
    OnClick = RbFastClick
  end
  object RbMedium: TRadioButton
    Left = 91
    Top = 63
    Width = 55
    Height = 17
    Caption = '&Medium'
    Checked = True
    TabOrder = 5
    TabStop = True
    OnClick = RbMediumClick
  end
  object RbSlow: TRadioButton
    Left = 150
    Top = 63
    Width = 46
    Height = 17
    Caption = '&Slow'
    TabOrder = 6
    OnClick = RbSlowClick
  end
  object SEFullscaleGain: TSpinEdit
    Left = 262
    Top = 60
    Width = 56
    Height = 22
    MaxValue = 200
    MinValue = 0
    TabOrder = 7
    Value = 0
    OnChange = SEFullscaleGainChange
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
    TabOrder = 8
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
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    ConvertOptimizations = [coSSE]
    PreFillOutBuffer = bpfZero
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = BSNormal
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 252
    Top = 24
  end
  object Timer: TTimer
    Enabled = False
    Interval = 30
    OnTimer = TimerTimer
    Left = 280
    Top = 24
  end
end
