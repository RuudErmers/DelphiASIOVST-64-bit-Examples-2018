object FmButton: TFmButton
  Left = 218
  Top = 77
  Caption = 'Button'
  ClientHeight = 146
  ClientWidth = 150
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object LbLineWidth: TLabel
    Left = 8
    Top = 70
    Width = 54
    Height = 13
    Caption = 'Line Width:'
    Transparent = True
  end
  object LbRadius: TLabel
    Left = 8
    Top = 98
    Width = 36
    Height = 13
    Caption = 'Radius:'
    Transparent = True
  end
  object ButtonA: TGuiButton
    Left = 8
    Top = 8
    Width = 64
    Height = 24
    Alignment = taCenter
    BorderColor = clBlue
    BorderWidth = 1.000000000000000000
    BorderRadius = 2.000000000000000000
    ButtonColor = clBtnShadow
    Caption = 'Button A'
    Shadow.Blur = 4.000000000000000000
    Transparent = False
    OnClick = ButtonAClick
  end
  object ButtonB: TGuiButton
    Left = 78
    Top = 8
    Width = 64
    Height = 24
    Alignment = taCenter
    BorderColor = clRed
    BorderWidth = 1.000000000000000000
    BorderRadius = 2.000000000000000000
    ButtonColor = clBtnShadow
    Caption = 'Button B'
    Shadow.Blur = 4.000000000000000000
    Transparent = False
  end
  object ButtonC: TGuiButton
    Left = 8
    Top = 38
    Width = 64
    Height = 24
    Alignment = taCenter
    BorderColor = clGreen
    BorderWidth = 1.000000000000000000
    BorderRadius = 2.000000000000000000
    ButtonColor = clBtnShadow
    Caption = 'Button C'
    Shadow.Blur = 4.000000000000000000
    Transparent = False
  end
  object ButtonD: TGuiButton
    Left = 78
    Top = 38
    Width = 64
    Height = 24
    Alignment = taCenter
    BorderColor = clYellow
    BorderWidth = 1.000000000000000000
    BorderRadius = 2.000000000000000000
    ButtonColor = clBtnShadow
    Caption = 'Button D'
    Shadow.Blur = 4.000000000000000000
    Transparent = False
  end
  object CbTransparent: TCheckBox
    Left = 32
    Top = 124
    Width = 81
    Height = 17
    Caption = 'Transparent'
    TabOrder = 0
    OnClick = CbTransparentClick
  end
  object TbBorderWidth: TTrackBar
    Left = 62
    Top = 68
    Width = 80
    Height = 22
    Max = 16
    Min = 1
    Position = 1
    TabOrder = 1
    ThumbLength = 12
    OnChange = TbBorderWidthChange
  end
  object TbRadius: TTrackBar
    Left = 62
    Top = 96
    Width = 80
    Height = 22
    Max = 16
    Min = 1
    Position = 1
    TabOrder = 2
    ThumbLength = 12
    OnChange = TbRadiusChange
  end
end
