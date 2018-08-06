object FmSplitter: TFmSplitter
  Left = 277
  Top = 185
  BorderStyle = bsNone
  Caption = 'Frequency Splitter'
  ClientHeight = 65
  ClientWidth = 588
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
  PixelsPerInch = 96
  TextHeight = 13
  object ShBorder: TShape
    Left = 261
    Top = 42
    Width = 33
    Height = 16
    Brush.Style = bsClear
  end
  object PnControl: TGuiPanel
    Left = 0
    Top = 0
    Width = 588
    Height = 37
    Align = alTop
    BorderColor = clBtnShadow
    BorderRadius = 2.000000000000000000
    BorderVisible = False
    BorderWidth = 2.000000000000000000
    Color = 7701642
    PanelColor = 7701642
    TabOrder = 0
    UseDockManager = True
    object DialSplitFrequency: TGuiDial
      Left = 105
      Top = 9
      Width = 20
      Height = 20
      AntiAlias = gaaLinear2x
      CircleColor = 6450289
      CurveMapping = -2.099999904632568000
      DefaultPosition = 100.000000000000000000
      DialImageIndex = -1
      LineWidth = 2
      Max = 20000.000000000000000000
      Min = 20.000000000000000000
      OnChange = DialSplitFrequencyChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 1000.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbSplitFrequency: TGuiLabel
      Left = 129
      Top = 9
      Width = 59
      Height = 20
      Caption = '1kHz'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      FontOversampling = fo2x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
    end
    object DialSplitOrder: TGuiDial
      Left = 194
      Top = 9
      Width = 20
      Height = 20
      AntiAlias = gaaLinear2x
      CircleColor = 6450289
      CurveMapping = -1.250000000000000000
      DefaultPosition = 8.000000000000000000
      DialImageIndex = -1
      LineWidth = 2
      Max = 16.000000000000000000
      Min = 1.000000000000000000
      OnChange = DialSplitOrderChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 8.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbSplitOrder: TGuiLabel
      Left = 219
      Top = 9
      Width = 39
      Height = 20
      Caption = '4x'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      FontOversampling = fo2x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
    end
    object GuiLEDOversampling: TGuiLED
      Left = 257
      Top = 9
      Width = 20
      Height = 20
      BorderStrength_Percent = 50.000000000000000000
      Brightness_Percent = 20.000000000000000000
      LEDColor = clLime
      BorderWidth = 2.400000095367432000
      Uniformity_Percent = 36.754447937011720000
      Transparent = False
      OnClick = GuiLEDOversamplingClick
    end
    object LbOversampling: TGuiLabel
      Left = 279
      Top = 9
      Width = 97
      Height = 20
      Caption = 'Oversampling:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      OnClick = GuiLEDOversamplingClick
    end
    object LbOversamplingFactor: TGuiLabel
      Left = 411
      Top = 9
      Width = 33
      Height = 20
      Caption = '4x'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      FontOversampling = fo2x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Visible = False
    end
    object DialOversampling: TGuiDial
      Left = 386
      Top = 9
      Width = 20
      Height = 20
      AntiAlias = gaaLinear2x
      CircleColor = 6450289
      CurveMapping = -1.250000000000000000
      DefaultPosition = 4.000000000000000000
      DialImageIndex = -1
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
    object SBMode: TGuiSelectBox
      Left = 8
      Top = 6
      Width = 89
      Height = 26
      Items.Strings = (
        'Split A'
        'Split B'
        'Dyn'
        'L/R'
        'M/S'
        'Serial'
        'Trans.'
        'LFO'
        'Spin'
        'Single'
        'Bypass')
      ArrowColor = clBlack
      BackColor = 6450289
      BorderColor = clBlack
      BorderRadius = 4.000000000000000000
      BorderWidth = 1.500000000000000000
      ButtonColor = 6450289
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      FontOversampling = fo4x
      ParentFont = False
      SelectBoxColor = 7701642
      OnChange = SBModeChange
    end
    object BtLow: TGuiButton
      Left = 447
      Top = 6
      Width = 65
      Height = 26
      Alignment = taCenter
      BorderColor = clBlack
      BorderWidth = 1.500000000000000000
      BorderRadius = 4.000000000000000000
      ButtonColor = 1625885
      Caption = 'Low'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      FontOversampling = fo4x
      Shadow.Blur = 4.000000000000000000
      Transparent = False
      OnClick = BtLowClick
    end
    object BtHigh: TGuiButton
      Left = 518
      Top = 6
      Width = 65
      Height = 26
      Alignment = taCenter
      BorderColor = clBlack
      BorderWidth = 1.500000000000000000
      BorderRadius = 4.000000000000000000
      ButtonColor = 6450289
      Caption = 'High'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      FontOversampling = fo4x
      Shadow.Blur = 4.000000000000000000
      Transparent = False
      OnClick = BtHighClick
    end
  end
  object PnGui: TPanel
    Left = 262
    Top = 43
    Width = 31
    Height = 14
    BevelOuter = bvNone
    Color = 2830643
    TabOrder = 1
  end
end
