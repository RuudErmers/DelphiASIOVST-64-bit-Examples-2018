object FmMegaDemo: TFmMegaDemo
  Left = 276
  Top = 159
  Caption = 'Mega Demo'
  ClientHeight = 243
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PC: TPageControl
    Left = 0
    Top = 0
    Width = 409
    Height = 243
    ActivePage = TsGroupBox
    Align = alClient
    TabOrder = 0
    TabPosition = tpBottom
    object TsCheckBox: TTabSheet
      Caption = 'CheckBox'
      object CbItem1: TGuiControlsCheckBox
        Left = 3
        Top = 3
        Width = 54
        Height = 17
        Caption = 'Item 1'
        TabOrder = 0
        ColorFocused = 10874110
        Native = False
        Shadow.Blur = 2.000000000000000000
        Shadow.Opacity = 128
        Shadow.Saturation = 0.100000001490116100
      end
      object CbItem2: TGuiControlsCheckBox
        Left = 3
        Top = 26
        Width = 54
        Height = 17
        Caption = 'Item 2'
        TabOrder = 1
        ColorFocused = 10874110
        Native = False
        Shadow.Blur = 2.000000000000000000
        Shadow.Opacity = 128
        Shadow.Saturation = 0.100000001490116100
      end
      object CbItem3: TGuiControlsCheckBox
        Left = 3
        Top = 49
        Width = 54
        Height = 17
        Caption = 'Item 3'
        TabOrder = 2
        ColorFocused = 10874110
        Native = False
        Shadow.Blur = 2.000000000000000000
        Shadow.Opacity = 128
        Shadow.Saturation = 0.100000001490116100
      end
      object CbItem4: TGuiControlsCheckBox
        Left = 75
        Top = 3
        Width = 118
        Height = 25
        Caption = 'Item 4'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -27
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        ColorFocused = 10874110
        Native = False
        Shadow.Blur = 2.000000000000000000
        Shadow.Opacity = 128
        Shadow.Saturation = 0.100000001490116100
      end
      object CbItem5: TGuiControlsCheckBox
        Left = 75
        Top = 34
        Width = 132
        Height = 31
        Caption = 'Item 5'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -32
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        ColorFocused = 10874110
        Native = False
        Shadow.Blur = 2.000000000000000000
        Shadow.Opacity = 128
        Shadow.Saturation = 0.100000001490116100
      end
    end
    object TsRadioButton: TTabSheet
      Caption = 'Radio Button'
      ImageIndex = 1
      object RbItem1: TGuiControlsRadioButton
        Left = 3
        Top = 3
        Width = 54
        Height = 17
        Caption = 'Item 1'
        TabOrder = 0
        ColorFocused = 10874110
        Native = False
        Shadow.Blur = 2.000000000000000000
        Shadow.Opacity = 128
        Shadow.Saturation = 0.100000001490116100
      end
      object RbItem2: TGuiControlsRadioButton
        Left = 3
        Top = 26
        Width = 54
        Height = 17
        Caption = 'Item 2'
        TabOrder = 1
        ColorFocused = 10874110
        Native = False
        Shadow.Blur = 2.000000000000000000
        Shadow.Opacity = 128
        Shadow.Saturation = 0.100000001490116100
      end
      object RbItem3: TGuiControlsRadioButton
        Left = 3
        Top = 49
        Width = 54
        Height = 17
        Caption = 'Item 3'
        TabOrder = 2
        ColorFocused = 10874110
        Native = False
        Shadow.Blur = 2.000000000000000000
        Shadow.Opacity = 128
        Shadow.Saturation = 0.100000001490116100
      end
      object RbItem4: TGuiControlsRadioButton
        Left = 63
        Top = 3
        Width = 186
        Height = 25
        Caption = 'Item 1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        ColorFocused = 10874110
        Native = False
        Shadow.Blur = 2.000000000000000000
        Shadow.Opacity = 128
        Shadow.Saturation = 0.100000001490116100
        GroupIndex = 1
      end
      object RbItem5: TGuiControlsRadioButton
        Left = 63
        Top = 34
        Width = 186
        Height = 32
        Caption = 'Item 2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -37
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        ColorFocused = 10874110
        Native = False
        Shadow.Blur = 2.000000000000000000
        Shadow.Opacity = 128
        Shadow.Saturation = 0.100000001490116100
        GroupIndex = 1
      end
    end
    object TsGroupBox: TTabSheet
      Caption = 'Group Box'
      ImageIndex = 2
      object GbTypical: TGuiGroup
        Left = 3
        Top = 3
        Width = 128
        Height = 64
        BorderWidth = 1.500000000000000000
        Caption = 'Typical Group'
        Native = False
        BorderRadius = 5.000000000000000000
        Shadow.Blur = 2.000000000000000000
        Shadow.Opacity = 128
        Shadow.Saturation = 0.100000001490116100
        TabOrder = 0
        object CbGroupNative: TGuiControlsCheckBox
          Left = 12
          Top = 23
          Width = 77
          Height = 17
          Caption = 'Group Item'
          TabOrder = 0
          Native = False
          Shadow.Blur = 2.000000000000000000
          Shadow.Opacity = 128
          Shadow.Saturation = 0.100000001490116100
        end
      end
      object GbTop: TGuiGroupTop
        Left = 137
        Top = 3
        Width = 128
        Height = 64
        BorderWidth = 1.500000000000000000
        Caption = 'Top Group'
        Native = False
        BorderRadius = 5.000000000000000000
        Shadow.Blur = 2.000000000000000000
        Shadow.Opacity = 128
        Shadow.Saturation = 0.100000001490116100
        TabOrder = 1
        object CbGroupShadow: TGuiControlsCheckBox
          Left = 11
          Top = 23
          Width = 86
          Height = 17
          Caption = 'Another Item'
          TabOrder = 0
          Native = False
          Shadow.Blur = 2.000000000000000000
          Shadow.Opacity = 128
          Shadow.Saturation = 0.100000001490116100
        end
      end
      object GbSide: TGuiGroupSide
        Left = 3
        Top = 73
        Width = 128
        Height = 104
        BorderWidth = 1.500000000000000000
        Caption = 'Side Group'
        Native = False
        BorderRadius = 5.000000000000000000
        Shadow.Blur = 2.000000000000000000
        Shadow.Opacity = 128
        Shadow.Saturation = 0.100000001490116100
        TabOrder = 2
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 192
    Top = 120
    object MiDemo: TMenuItem
      Caption = '&Demo'
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
    object MiLayout: TMenuItem
      Caption = '&Layout'
      object MiFontOversampling: TMenuItem
        Caption = 'Font Oversampling'
        object MiFontOversamplingNone: TMenuItem
          Caption = '&None'
          Checked = True
          RadioItem = True
          OnClick = MiFontOversamplingClick
        end
        object MiFontOversampling2x: TMenuItem
          Caption = '&2x'
          RadioItem = True
          OnClick = MiFontOversamplingClick
        end
        object MiFontOversampling3x: TMenuItem
          Caption = '&3x'
          RadioItem = True
          OnClick = MiFontOversamplingClick
        end
        object MiFontOversampling4x: TMenuItem
          Caption = '&4x'
          RadioItem = True
          OnClick = MiFontOversamplingClick
        end
      end
      object MiFontShadow: TMenuItem
        Caption = 'Font Shadow'
        object MiFontShadowEnabled: TMenuItem
          Caption = '&Enabled'
          OnClick = MiFontShadowEnabledClick
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MiNative: TMenuItem
        Caption = '&Native'
        OnClick = MiNativeClick
      end
    end
  end
end
