object FmSelectBox: TFmSelectBox
  Left = 218
  Top = 77
  Caption = 'Test Select Box'
  ClientHeight = 140
  ClientWidth = 219
  Color = 7373965
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
  object LbRoundRadius: TLabel
    Left = 8
    Top = 72
    Width = 70
    Height = 13
    Caption = 'Round Radius:'
    Transparent = True
  end
  object LbArrowWidth: TLabel
    Left = 8
    Top = 95
    Width = 64
    Height = 13
    Caption = 'Arrow Width:'
    Transparent = True
  end
  object SelectBoxA: TGuiSelectBox
    Left = 8
    Top = 8
    Width = 100
    Height = 22
    BorderRadius = 7.000000000000000000
    BorderWidth = 1.500000000000000000
    ButtonColor = 5530731
    ItemIndex = -1
    Items.Strings = (
      'Test 1'
      'Test 2'
      'Test 3')
    SelectBoxColor = 7373965
  end
  object SelectBoxB: TGuiSelectBox
    Left = 114
    Top = 8
    Width = 100
    Height = 23
    BorderRadius = 7.000000000000000000
    BorderWidth = 1.500000000000000000
    ButtonColor = 5530731
    ItemIndex = -1
    Items.Strings = (
      'Test 1'
      'Test 2'
      'Test 3')
    SelectBoxColor = 7373965
  end
  object SelectBoxC: TGuiSelectBox
    Left = 8
    Top = 39
    Width = 100
    Height = 24
    BorderRadius = 7.000000000000000000
    BorderWidth = 1.500000000000000000
    ButtonColor = 5530731
    ItemIndex = -1
    Items.Strings = (
      'Test A'
      'Test B'
      'Test C'
      'Test D')
    SelectBoxColor = 7373965
  end
  object SelectBoxD: TGuiSelectBox
    Left = 114
    Top = 39
    Width = 100
    Height = 25
    BorderRadius = 7.000000000000000000
    BorderWidth = 1.500000000000000000
    ButtonColor = 5530731
    ItemIndex = -1
    Items.Strings = (
      'Test 1'
      'Test 2'
      'Test 3'
      'Test 4')
    SelectBoxColor = 7373965
  end
  object CbTransparent: TCheckBox
    Left = 8
    Top = 118
    Width = 97
    Height = 17
    Caption = 'Transparent'
    Color = 7373965
    ParentColor = False
    TabOrder = 0
    OnClick = CbTransparentClick
  end
  object TbRoundRadius: TGuiSlider
    Left = 84
    Top = 70
    Width = 130
    Height = 19
    BorderColor = clBtnHighlight
    BorderRadius = 7.000000000000000000
    BorderWidth = 1.500000000000000000
    Color = 7373965
    DefaultValue = 19.000000000000000000
    Digits = 2
    FontShadow.Blur = 1.000000000000000000
    FontShadow.Color = 7373965
    FontShadow.OffsetX = 0
    FontShadow.OffsetY = 0
    FontShadow.Saturation = 4.000000000000000000
    FontShadow.Visible = True
    Max = 19.000000000000000000
    Min = 1.000000000000000000
    ParentColor = False
    Value = 1.000000000000000000
    ShowText = True
    SlideColor = 4872286
    Transparent = True
    OnChange = TbRoundRadiusChange
  end
  object TbArrowWidth: TGuiSlider
    Left = 84
    Top = 93
    Width = 130
    Height = 19
    BorderColor = clBtnHighlight
    BorderRadius = 7.000000000000000000
    BorderWidth = 1.500000000000000000
    Color = 7373965
    DefaultValue = 5.000000000000000000
    Digits = 2
    FontShadow.Blur = 1.000000000000000000
    FontShadow.Color = 7373965
    FontShadow.OffsetX = 0
    FontShadow.OffsetY = 0
    FontShadow.Saturation = 4.000000000000000000
    FontShadow.Visible = True
    Max = 5.000000000000000000
    Min = 1.000000000000000000
    ParentColor = False
    Value = 1.000000000000000000
    ShowText = True
    SlideColor = 4872286
    Transparent = True
    OnChange = TbArrowWidthChange
  end
end
