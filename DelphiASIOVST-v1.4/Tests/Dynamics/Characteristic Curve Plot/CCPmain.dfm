object FmDynamicCurvePlot: TFmDynamicCurvePlot
  Left = 339
  Top = 86
  Caption = 'Dynamic Characteristic Curve Plot'
  ClientHeight = 302
  ClientWidth = 204
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    204
    302)
  PixelsPerInch = 96
  TextHeight = 13
  object LbThreshold: TLabel
    Left = 8
    Top = 217
    Width = 47
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = 'Threshold'
    ExplicitTop = 311
  end
  object LbRatio: TLabel
    Left = 80
    Top = 217
    Width = 48
    Height = 16
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = 'Ratio'
    ExplicitTop = 195
  end
  object LbKnee: TLabel
    Left = 146
    Top = 217
    Width = 48
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = 'Knee'
    ExplicitTop = 195
  end
  object DialThreshold: TGuiDial
    Left = 8
    Top = 233
    Width = 48
    Height = 48
    Anchors = [akLeft, akBottom]
    LineColor = clBlack
    CircleColor = clBtnFace
    AntiAlias = gaaLinear2x
    Min = -90.000000000000000000
    GlyphCount = 31
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 225.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    OnChange = SEThresholdChange
    ExplicitTop = 327
  end
  object DialRatio: TGuiDial
    Left = 80
    Top = 233
    Width = 48
    Height = 48
    Anchors = [akLeft, akBottom]
    LineColor = clBlack
    CircleColor = clBtnFace
    AntiAlias = gaaLinear2x
    DefaultPosition = 8.000000000000000000
    Max = 100.000000000000000000
    Min = 1.000000000000000000
    GlyphCount = 31
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 225.000000000000000000
    Position = 1.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    OnChange = SERatioChange
    ExplicitTop = 211
  end
  object DialKnee: TGuiDial
    Left = 146
    Top = 233
    Width = 48
    Height = 48
    Anchors = [akLeft, akBottom]
    LineColor = clBlack
    CircleColor = clBtnFace
    AntiAlias = gaaLinear2x
    DefaultPosition = 1.000000000000000000
    Max = 10.000000000000000000
    Min = 0.009999999776482582
    GlyphCount = 31
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 225.000000000000000000
    Position = 1.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    OnChange = SEKneeChange
    ExplicitTop = 211
  end
  object LbThresholdValue: TLabel
    Left = 8
    Top = 284
    Width = 47
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = '0 dB'
    ExplicitTop = 378
  end
  object LbRatioValue: TLabel
    Left = 80
    Top = 284
    Width = 48
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = '1 : 1'
    ExplicitTop = 262
  end
  object LbKneeValue: TLabel
    Left = 146
    Top = 284
    Width = 48
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = '1 dB'
    ExplicitTop = 262
  end
  object DynamicsChart: TGuiGraphXY
    Left = 0
    Top = 0
    Width = 204
    Height = 184
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    FrameColor = clBlack
    SeriesCollection = <>
    XAxis.Granularity = 20.000000000000000000
    XAxis.Minimum = -90.000000000000000000
    XAxis.Maximum = 5.000000000000000000
    XAxis.Lower = -90.000000000000000000
    XAxis.Upper = 5.000000000000000000
    YAxis.Granularity = 20.000000000000000000
    YAxis.Minimum = -90.000000000000000000
    YAxis.Maximum = 5.000000000000000000
    YAxis.Lower = -90.000000000000000000
    YAxis.Upper = 5.000000000000000000
    LineColor = clGray
    LineWidth = 2
    ExplicitWidth = 212
  end
  object CBType: TComboBox
    Left = 8
    Top = 190
    Width = 186
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 7
    TabOrder = 0
    Text = 'TSimpleSoftKneeLimiter'
    OnChange = CBTypeChange
    Items.Strings = (
      'TSimpleDirectGate'
      'TSoftDirectGate'
      'TBrickwallLimiter'
      'TSoftBrickwallLimiter'
      'TSimpleSoftBrickwallLimiter'
      'TLimiter'
      'TSoftKneeLimiter'
      'TSimpleSoftKneeLimiter'
      'TClassicGate'
      'TClassicSoftRangeGate'
      'TClassicSoftKneeGate'
      'TAdvancedGate'
      'TSimpleCompressor'
      'TSoftKneeCompressor'
      'TSimpleRMSCompressor'
      'TCompressor')
  end
end
