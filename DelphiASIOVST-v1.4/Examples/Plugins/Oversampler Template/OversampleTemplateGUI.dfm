object FmOversampler: TFmOversampler
  Left = 277
  Top = 185
  BorderStyle = bsNone
  Caption = 'Frequency Oversampler'
  ClientHeight = 54
  ClientWidth = 187
  Color = 2830643
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    187
    54)
  PixelsPerInch = 96
  TextHeight = 13
  object ShBorder: TShape
    Left = 77
    Top = 30
    Width = 33
    Height = 16
    Anchors = []
    Brush.Style = bsClear
  end
  object PnControl: TGuiPanel
    Left = 0
    Top = 0
    Width = 187
    Height = 24
    Align = alTop
    Anchors = []
    BorderColor = clBtnShadow
    BorderVisible = False
    BorderWidth = 2.000000000000000000
    Color = 7701642
    PanelColor = 7701642
    PopupMenu = PUSettings
    TabOrder = 0
    UseDockManager = True
    object LedOversampling: TGuiLED
      Left = 4
      Top = 4
      Width = 16
      Height = 16
      BorderStrength_Percent = 100.000000000000000000
      Brightness_Percent = 20.000000000000000000
      LEDColor = clLime
      BorderWidth = 2.099999904632568000
      Uniformity_Percent = 36.754447937011720000
      Transparent = False
      OnClick = LedOversamplingClick
    end
    object LblOversampling: TGuiLabel
      Left = 26
      Top = 4
      Width = 80
      Height = 16
      Caption = 'Oversampling:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      FontOversampling = fo4x
      ParentFont = False
      OnClick = LedOversamplingClick
    end
    object LblOversamplingFactor: TGuiLabel
      Left = 134
      Top = 4
      Width = 27
      Height = 16
      Caption = '4x'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      FontOversampling = fo2x
      ParentFont = False
      Visible = False
    end
    object DialOversampling: TGuiDial
      Left = 115
      Top = 4
      Width = 16
      Height = 16
      AntiAlias = gaaLinear4x
      CircleColor = 6450289
      CurveMapping = -1.250000000000000000
      DefaultPosition = 4.000000000000000000
      DialImageIndex = -1
      LineColor = 2830643
      LineWidth = 2
      Max = 16.000000000000000000
      Min = 1.000000000000000000
      OnChange = DialOversamplingChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 4.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      Visible = False
      WheelStep = 1.000000000000000000
    end
  end
  object PnGui: TPanel
    Left = 78
    Top = 31
    Width = 31
    Height = 14
    BevelOuter = bvNone
    Color = 2830643
    TabOrder = 1
  end
  object PUSettings: TPopupMenu
    Left = 144
    Top = 8
    object MiAllowResizing: TMenuItem
      Caption = 'Allow &Resizing'
      OnClick = MiAllowResizingClick
    end
    object MiManualIdle: TMenuItem
      Caption = 'Manual &Idle'
      OnClick = MiManualIdleClick
    end
  end
end
