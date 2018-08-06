object FmESTP: TFmESTP
  Left = 394
  Top = 68
  Caption = 'Equal Spaced Thick Polyline'
  ClientHeight = 290
  ClientWidth = 411
  Color = clBtnFace
  Constraints.MinHeight = 128
  Constraints.MinWidth = 128
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    411
    290)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 8
    Top = 32
    Width = 395
    Height = 233
    Anchors = [akLeft, akTop, akRight, akBottom]
    PopupMenu = PuScenario
    OnClick = PaintBoxClick
    OnPaint = PaintBoxPaint
  end
  object SlLineWidth: TGuiSlider
    Left = 8
    Top = 8
    Width = 395
    Height = 18
    Anchors = [akLeft, akTop, akRight]
    BorderRadius = 4.000000000000000000
    BorderWidth = 1.500000000000000000
    Caption = 'Linewidth'
    Color = clBtnFace
    CurveMapping = 0.500000000000000000
    DefaultValue = 2.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    FontOversampling = fo4x
    FontShadow.Blur = 2.000000000000000000
    FontShadow.Color = clBtnFace
    FontShadow.OffsetX = 0
    FontShadow.OffsetY = 0
    FontShadow.Saturation = 3.000000000000000000
    FontShadow.Visible = True
    Max = 20.000000000000000000
    Min = 1.000000000000000000
    ParentColor = False
    PopupMenu = PuLinePreset
    Value = 2.000000000000000000
    ShowText = True
    SlideColor = 6316128
    OnChange = SlLineWidthChange
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 271
    Width = 411
    Height = 19
    Panels = <
      item
        Text = 'X Pixel:'
        Width = 60
      end
      item
        Width = 50
      end>
  end
  object PuLinePreset: TPopupMenu
    Left = 40
    Top = 32
    object MiWidthA: TMenuItem
      Caption = 'Width A (2.0)'
      OnClick = MiWidthAClick
    end
    object MiWidthB: TMenuItem
      Caption = 'Width B (2.99999)'
      OnClick = MiWidthBClick
    end
    object MiWidthC: TMenuItem
      Caption = 'Width C (3.0)'
      OnClick = MiWidthCClick
    end
    object MiWidthD: TMenuItem
      Caption = 'Width D (3.5)'
      OnClick = MiWidthDClick
    end
    object MiWidthE: TMenuItem
      Caption = 'Width E (4.5)'
      OnClick = MiWidthEClick
    end
    object MiWidthF: TMenuItem
      Caption = 'Width F (7.0)'
      OnClick = MiWidthFClick
    end
    object MiWidthG: TMenuItem
      Caption = 'Width G (7.076)'
      OnClick = MiWidthGClick
    end
    object MiWidthH: TMenuItem
      Caption = 'Width H (9.0)'
      OnClick = MiWidthHClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MiAddTinyValue: TMenuItem
      Action = ACAddTinyValue
    end
    object MiSubtractTinyValue: TMenuItem
      Action = ACSubtractTinyValue
    end
  end
  object PuScenario: TPopupMenu
    Left = 40
    Top = 80
    object MiScenarioStandard: TMenuItem
      Caption = '&Standard'
      Checked = True
      RadioItem = True
      OnClick = MiScenarioStandardClick
    end
    object MiScenarioPeakLine1: TMenuItem
      Caption = '&Peak Line I'
      RadioItem = True
      OnClick = MiScenarioPeakLine1Click
    end
    object MiScenarioPeakLine2: TMenuItem
      Caption = 'Peak Line II'
      RadioItem = True
      OnClick = MiScenarioPeakLine2Click
    end
    object MiScenarioPeakLine3: TMenuItem
      Caption = 'Peak Line III'
      RadioItem = True
      OnClick = MiScenarioPeakLine3Click
    end
    object MiScenarioRandom: TMenuItem
      Caption = '&Random'
      RadioItem = True
      OnClick = MiScenarioRandomClick
    end
    object MiScenarioSmallIncrease: TMenuItem
      Caption = '&Small Increase'
      RadioItem = True
      OnClick = MiScenarioSmallIncreaseClick
    end
    object MiScenarioExceedBorders: TMenuItem
      Caption = 'E&xceed Borders'
      RadioItem = True
      OnClick = MiScenarioExceedBordersClick
    end
    object MiUpDown: TMenuItem
      Caption = '&Up && Down'
      RadioItem = True
      OnClick = MiUpDownClick
    end
    object MiScenarioLargeUpDown: TMenuItem
      Caption = '&Large Up && Down'
      RadioItem = True
      OnClick = MiScenarioLargeUpDownClick
    end
  end
  object ActionList1: TActionList
    Left = 128
    Top = 64
    object ACAddTinyValue: TAction
      Caption = 'Add tiny value'
      ShortCut = 187
      OnExecute = ACAddTinyValueExecute
    end
    object ACSubtractTinyValue: TAction
      Caption = 'Subtract Tiny Value'
      ShortCut = 111
      OnExecute = ACSubtractTinyValueExecute
    end
  end
end
