object FmSelectBoxTest: TFmSelectBoxTest
  Left = 393
  Top = 163
  Caption = 'Select Box Test Application'
  ClientHeight = 107
  ClientWidth = 168
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbAntiAlias: TLabel
    Left = 8
    Top = 9
    Width = 44
    Height = 13
    Caption = 'Antialias:'
  end
  object LbLineWidth: TLabel
    Left = 8
    Top = 34
    Width = 54
    Height = 13
    Caption = 'Line Width:'
  end
  object LbRadius: TLabel
    Left = 8
    Top = 61
    Width = 72
    Height = 13
    Caption = 'Corner Radius:'
  end
  object LbArrowWidth: TLabel
    Left = 8
    Top = 87
    Width = 64
    Height = 13
    Caption = 'Arrow Width:'
  end
  object SbAntiAlias: TGuiSelectBox
    Left = 58
    Top = 5
    Width = 105
    Height = 20
    Items.Strings = (
      'gaaNone'
      'gaa2x'
      'gaa3x'
      'gaa4x'
      'gaa8x'
      'gaa16x')
    BorderColor = clBtnShadow
    BorderRadius = 5.000000000000000000
    BorderWidth = 1.500000000000000000
    ButtonColor = clBtnFace
  end
  object SbLineWidth: TGuiSelectBox
    Left = 68
    Top = 31
    Width = 95
    Height = 20
    Items.Strings = (
      '1'
      '2'
      '3'
      '4')
    BorderColor = clBtnShadow
    BorderRadius = 5.000000000000000000
    BorderWidth = 1.500000000000000000
    ButtonColor = clBtnFace
    OnChange = SbLineWidthChange
  end
  object SbCornerRadius: TGuiSelectBox
    Left = 86
    Top = 57
    Width = 77
    Height = 20
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9'
      '10')
    BorderColor = clBtnShadow
    BorderRadius = 5.000000000000000000
    BorderWidth = 1.500000000000000000
    ButtonColor = clBtnFace
    OnChange = SbCornerRadiusChange
  end
  object SbArrowWidth: TGuiSelectBox
    Left = 76
    Top = 83
    Width = 87
    Height = 20
    Items.Strings = (
      '1'
      '2'
      '3'
      '4')
    BorderColor = clBtnShadow
    BorderRadius = 5.000000000000000000
    BorderWidth = 1.500000000000000000
    ButtonColor = clBtnFace
    OnChange = SbArrowWidthChange
  end
end
