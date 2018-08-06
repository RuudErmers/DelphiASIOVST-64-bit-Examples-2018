object FmGroupBoxTest: TFmGroupBoxTest
  Left = 218
  Top = 77
  Caption = 'GroupBox Test'
  ClientHeight = 220
  ClientWidth = 278
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
  object ShGroupColor: TShape
    Left = 129
    Top = 196
    Width = 17
    Height = 17
    Brush.Color = clBtnShadow
    OnMouseDown = ShGroupColorMouseDown
  end
  object LbOutlineWidth: TGuiLabel
    Left = 8
    Top = 149
    Width = 69
    Height = 13
    Margins.Bottom = 0
    AutoSize = True
    Caption = 'Outline Width:'
    Transparent = True
  end
  object LbRoundRadius: TGuiLabel
    Left = 8
    Top = 173
    Width = 70
    Height = 13
    Margins.Bottom = 0
    AutoSize = True
    Caption = 'Round Radius:'
    Transparent = True
  end
  object LbColor: TGuiLabel
    Left = 94
    Top = 197
    Width = 29
    Height = 13
    Margins.Bottom = 0
    AutoSize = True
    Caption = 'Color:'
    Transparent = True
  end
  object GroupA: TGuiGroup
    Left = 8
    Top = 8
    Width = 128
    Height = 64
    Alpha = 128
    BorderColor = 4210752
    BorderWidth = 2.000000000000000000
    Caption = 'Group A'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    GroupColor = clRed
    Native = False
    ParentColor = False
    ParentFont = False
    BorderRadius = 4.000000000000000000
    TabOrder = 0
    Transparent = True
  end
  object GroupB: TGuiGroup
    Left = 142
    Top = 8
    Width = 128
    Height = 64
    Alpha = 128
    BorderColor = 4210752
    BorderWidth = 2.000000000000000000
    Caption = 'Group B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    GroupColor = clLime
    Native = False
    ParentFont = False
    BorderRadius = 4.000000000000000000
    TabOrder = 1
    Transparent = True
  end
  object GroupC: TGuiGroup
    Left = 8
    Top = 78
    Width = 128
    Height = 64
    Alpha = 128
    BorderColor = 4210752
    BorderWidth = 2.000000000000000000
    Caption = 'Group C'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    GroupColor = clYellow
    Native = False
    ParentColor = False
    ParentFont = False
    BorderRadius = 4.000000000000000000
    TabOrder = 2
    Transparent = True
  end
  object GroupD: TGuiGroup
    Left = 142
    Top = 78
    Width = 128
    Height = 64
    Alpha = 128
    BorderColor = 4210752
    BorderWidth = 2.000000000000000000
    Caption = 'Group D'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    GroupColor = clBlue
    Native = False
    ParentFont = False
    BorderRadius = 4.000000000000000000
    TabOrder = 3
    Transparent = True
  end
  object CbTransparent: TGuiControlsCheckBox
    Left = 8
    Top = 196
    Width = 80
    Height = 17
    Caption = 'Transparent'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CbTransparentClick
    Transparent = True
    Native = False
  end
  object SlBorderWidth: TGuiSlider
    Left = 84
    Top = 148
    Width = 186
    Height = 18
    BorderRadius = 4.000000000000000000
    BorderWidth = 2.000000000000000000
    Color = clBtnFace
    DefaultValue = 10.000000000000000000
    Max = 10.000000000000000000
    ReadOnly = False
    ParentColor = False
    Value = 2.000000000000000000
    ShowText = True
    SlideColor = 6316128
    Transparent = True
    OnChange = SlBorderWidthChange
  end
  object SlRoundRadius: TGuiSlider
    Left = 84
    Top = 172
    Width = 186
    Height = 18
    BorderRadius = 4.000000000000000000
    BorderWidth = 2.000000000000000000
    Color = clBtnFace
    DefaultValue = 10.000000000000000000
    Max = 10.000000000000000000
    ReadOnly = False
    ParentColor = False
    Value = 4.000000000000000000
    ShowText = True
    SlideColor = 6316128
    Transparent = True
    OnChange = SlRoundRadiusChange
  end
  object ColorDialog: TColorDialog
    Left = 168
    Top = 184
  end
end
