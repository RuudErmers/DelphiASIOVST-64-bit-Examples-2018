object FmSpinBugLite: TFmSpinBugLite
  Left = 527
  Top = 329
  BorderStyle = bsNone
  Caption = 'SpinBug Lite'
  ClientHeight = 190
  ClientWidth = 317
  Color = 197995
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 216826
  Font.Height = -13
  Font.Name = 'Comic Sans MS'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    317
    190)
  PixelsPerInch = 96
  TextHeight = 18
  object LbTitle: TGuiLabel
    Left = 7
    Top = 103
    Width = 299
    Height = 78
    Anchors = [akLeft, akBottom]
    Caption = 'SPINBUG'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 216826
    Font.Height = -64
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 3.000000000000000000
    Shadow.Color = 13491454
    Shadow.OffsetX = 2
    Shadow.OffsetY = 2
    Shadow.Saturation = 4.000000000000000000
    Shadow.Visible = True
    Transparent = True
  end
  object DialLFO: TGuiDial
    Left = 213
    Top = 8
    Width = 96
    Height = 96
    CircleColor = 216826
    CurveMapping = -0.920000016689300500
    DialImageIndex = -1
    LineColor = clWhite
    LineWidth = 2
    Max = 13.000000000000000000
    OnChange = DialLFOChange
    PointerAngles.Start = 215
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 4.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbType: TGuiLabel
    Left = 8
    Top = 12
    Width = 31
    Height = 18
    Margins.Bottom = 0
    AutoSize = True
    Caption = 'type:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 1.000000000000000000
    Shadow.Color = 216826
    Shadow.Saturation = 1.000000000000000000
    Shadow.Visible = True
  end
  object LbColour: TGuiLabel
    Left = 8
    Top = 43
    Width = 44
    Height = 18
    Margins.Bottom = 0
    AutoSize = True
    Caption = 'colour:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 1.000000000000000000
    Shadow.Color = 216826
    Shadow.Saturation = 1.000000000000000000
    Shadow.Visible = True
  end
  object LbLFOSpeed: TGuiLabel
    Left = 84
    Top = 88
    Width = 66
    Height = 18
    Margins.Bottom = 0
    AutoSize = True
    Caption = 'lfo speed: '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 1.000000000000000000
    Shadow.Color = 216826
    Shadow.Saturation = 1.000000000000000000
    Shadow.Visible = True
  end
  object LbLFOSpeedValue: TGuiLabel
    Left = 156
    Top = 88
    Width = 51
    Height = 18
    Margins.Bottom = 0
    AutoSize = True
    Caption = '10 Hz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 1.000000000000000000
    Shadow.Color = 216826
    Shadow.Saturation = 1.000000000000000000
    Shadow.Visible = True
  end
  object SelectType: TGuiSelectBox
    Left = 45
    Top = 8
    Width = 89
    Height = 25
    Items.Strings = (
      'stereo a'
      'stereo b'
      'stereo c'
      'stereo d'
      'mono'
      'mono l'
      'mono r'
      'm+s'
      'special'
      'old one')
    ArrowColor = clWhite
    BorderRadius = 4.000000000000000000
    BorderWidth = 2.000000000000000000
    ButtonColor = 271837
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    SelectBoxColor = 271837
    OnChange = SelectTypeChange
  end
  object SelectColour: TGuiSelectBox
    Left = 58
    Top = 39
    Width = 108
    Height = 25
    Items.Strings = (
      'rough'
      'firm'
      'medium'
      'soft'
      'smooth')
    ArrowColor = clWhite
    BorderRadius = 4.000000000000000000
    BorderWidth = 2.000000000000000000
    ButtonColor = 271837
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    SelectBoxColor = 271837
    OnChange = SelectColourChange
  end
end
