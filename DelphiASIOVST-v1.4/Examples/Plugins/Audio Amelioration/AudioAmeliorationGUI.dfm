object FmAudioAmelioration: TFmAudioAmelioration
  Left = 286
  Top = 77
  BorderStyle = bsNone
  Caption = 'Audio Amelioration'
  ClientHeight = 302
  ClientWidth = 291
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  DesignSize = (
    291
    302)
  PixelsPerInch = 96
  TextHeight = 13
  object LbSetup: TLabel
    Left = 95
    Top = 9
    Width = 32
    Height = 13
    Caption = 'Setup:'
  end
  object CbExciter: TCheckBox
    Left = 8
    Top = 31
    Width = 57
    Height = 17
    Caption = 'Exciter'
    TabOrder = 0
    OnClick = CbExciterClick
  end
  object CbAmbience: TCheckBox
    Left = 8
    Top = 54
    Width = 66
    Height = 17
    Caption = 'Ambience'
    TabOrder = 1
    OnClick = CbAmbienceClick
  end
  object Cb3DSound: TCheckBox
    Left = 8
    Top = 77
    Width = 81
    Height = 17
    Caption = '3D Surround'
    Enabled = False
    TabOrder = 2
    OnClick = Cb3DSoundClick
  end
  object CbPower: TCheckBox
    Left = 8
    Top = 8
    Width = 66
    Height = 17
    Caption = 'Power'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = CbPowerClick
  end
  object CbCompressor: TCheckBox
    Left = 8
    Top = 100
    Width = 81
    Height = 17
    Caption = 'Compressor'
    TabOrder = 4
    OnClick = CbCompressorClick
  end
  object CbExtrabass: TCheckBox
    Left = 8
    Top = 123
    Width = 81
    Height = 17
    Caption = 'Extra Bass'
    TabOrder = 5
    OnClick = CbExtrabassClick
  end
  object TbExciter: TTrackBar
    Left = 95
    Top = 31
    Width = 188
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    ThumbLength = 12
    OnChange = TbExciterChange
  end
  object TbAmbience: TTrackBar
    Left = 95
    Top = 54
    Width = 188
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    ThumbLength = 12
    OnChange = TbAmbienceChange
  end
  object Tb3DSurround: TTrackBar
    Left = 95
    Top = 77
    Width = 188
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 8
    ThumbLength = 12
    OnChange = Tb3DSurroundChange
  end
  object TbCompressor: TTrackBar
    Left = 95
    Top = 100
    Width = 188
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 9
    ThumbLength = 12
    OnChange = TbCompressorChange
  end
  object TbExtraBass: TTrackBar
    Left = 95
    Top = 123
    Width = 188
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 10
    ThumbLength = 12
    OnChange = TbExtraBassChange
  end
  object RbSpeaker: TRadioButton
    Left = 133
    Top = 8
    Width = 58
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Speaker'
    Checked = True
    TabOrder = 11
    TabStop = True
    OnClick = RbSpeakerHeadphoneClick
  end
  object RbHeadphones: TRadioButton
    Left = 197
    Top = 8
    Width = 86
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Headphones'
    TabOrder = 12
    OnClick = RbSpeakerHeadphoneClick
  end
  object SpectrumAnalyser: TChart
    Left = 5
    Top = 146
    Width = 278
    Height = 148
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      'Spectrum Analyser')
    LeftAxis.Automatic = False
    LeftAxis.AutomaticMaximum = False
    LeftAxis.AutomaticMinimum = False
    LeftAxis.Minimum = -100.000000000000000000
    View3D = False
    View3DWalls = False
    BevelInner = bvLowered
    TabOrder = 13
    Anchors = [akLeft, akTop, akRight]
    ColorPaletteIndex = 13
    object SpectrumBars: TBarSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Visible = False
      SeriesColor = clRed
      ShowInLegend = False
      AutoMarkPosition = False
      Dark3D = False
      YOrigin = -100.000000000000000000
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Balken'
      YValues.Order = loNone
    end
  end
  object Timer: TTimer
    Interval = 40
    OnTimer = TimerTimer
    Left = 136
    Top = 152
  end
end
