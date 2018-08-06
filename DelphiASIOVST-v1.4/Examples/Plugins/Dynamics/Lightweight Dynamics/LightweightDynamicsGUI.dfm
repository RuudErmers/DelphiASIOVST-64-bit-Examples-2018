object FmLightweightDynamics: TFmLightweightDynamics
  Left = 334
  Top = 58
  BorderStyle = bsNone
  Caption = 'Lightweight Dynamics'
  ClientHeight = 422
  ClientWidth = 473
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 14277598
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 23
  object GbGate: TGuiGroup
    Left = 8
    Top = 8
    Width = 457
    Height = 93
    FontOversampling = fo4x
    Caption = 'Gate'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = 14277598
    PanelColor = clBlack
    ParentFont = False
    BorderRadius = 5
    TabOrder = 0
    object LbGateAttack: TGuiLabel
      Left = 58
      Top = 8
      Width = 52
      Height = 19
      Alignment = taCenter
      Caption = 'Attack'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbGateAttackValue: TGuiLabel
      Left = 48
      Top = 66
      Width = 72
      Height = 20
      Alignment = taCenter
      Caption = '15 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbGateRelease: TGuiLabel
      Left = 132
      Top = 8
      Width = 68
      Height = 19
      Alignment = taCenter
      Caption = 'Release'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbGateReleaseValue: TGuiLabel
      Left = 126
      Top = 66
      Width = 76
      Height = 20
      Alignment = taCenter
      Caption = '75 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbGateThreshold: TGuiLabel
      Left = 203
      Top = 8
      Width = 90
      Height = 25
      Alignment = taCenter
      Caption = 'Threshold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbGateThresholdValue: TGuiLabel
      Left = 208
      Top = 66
      Width = 72
      Height = 20
      Alignment = taCenter
      Caption = '-60 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbGateRatio: TGuiLabel
      Left = 299
      Top = 8
      Width = 50
      Height = 25
      Alignment = taCenter
      Caption = 'Ratio'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbGateRatioValue: TGuiLabel
      Left = 286
      Top = 66
      Width = 76
      Height = 20
      Alignment = taCenter
      Caption = '1 : 10'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbGateKnee: TGuiLabel
      Left = 363
      Top = 8
      Width = 82
      Height = 25
      Alignment = taCenter
      Caption = 'Soft Knee'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbGateKneeValue: TGuiLabel
      Left = 368
      Top = 66
      Width = 74
      Height = 20
      Alignment = taCenter
      Caption = '2 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LmLeft: TGuiColorLevelMeter
      Left = 9
      Top = 32
      Width = 13
      Height = 36
      Visible = False
      BorderColor = 14277598
      ContrastLuminance = 0.219999998807907100
      Upper = 1.100000023841858000
    end
    object LmRight: TGuiColorLevelMeter
      Left = 28
      Top = 32
      Width = 13
      Height = 36
      Visible = False
      BorderColor = 14277598
      ContrastLuminance = 0.219999998807907100
      Upper = 1.100000023841858000
    end
    object LbInputGainLeft: TGuiLabel
      Left = 9
      Top = 70
      Width = 13
      Height = 17
      Alignment = taCenter
      Caption = 'L'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
      Visible = False
    end
    object LbInputGainRight: TGuiLabel
      Left = 28
      Top = 70
      Width = 13
      Height = 17
      Alignment = taCenter
      Caption = 'R'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
      Visible = False
    end
    object DialGateAttack: TGuiStitchedDial
      Left = 67
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      Max = 1000.000000000000000000
      Min = 0.009999999776482582
      OnChange = DialGateAttackChange
      ImageList = GSPL
      ImageIndex = 0
      Value = 15.000000000000000000
      WheelStep = 1.000000000000000000
    end
    object DialGateRelease: TGuiStitchedDial
      Left = 147
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      Max = 5000.000000000000000000
      Min = 0.100000001490116100
      OnChange = DialGateReleaseChange
      ImageList = GSPL
      ImageIndex = 0
      Value = 75.000000000000000000
      WheelStep = 1.000000000000000000
    end
    object DialGateThreshold: TGuiStitchedDial
      Left = 227
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.799999952316284000
      Max = 10.000000000000000000
      Min = -90.000000000000000000
      OnChange = DialGateThresholdChange
      ImageList = GSPL
      ImageIndex = 0
      Value = -60.000000000000000000
      WheelStep = 1.000000000000000000
    end
    object DialGateRatio: TGuiStitchedDial
      Left = 307
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.799999952316284000
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      OnChange = DialGateRatioChange
      ImageList = GSPL
      ImageIndex = 0
      Value = 10.000000000000000000
      WheelStep = 1.000000000000000000
    end
    object DialGateKnee: TGuiStitchedDial
      Left = 387
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      Max = 10.000000000000000000
      OnChange = DialGateKneeChange
      ImageList = GSPL
      ImageIndex = 0
      Value = 2.000000000000000000
      WheelStep = 1.000000000000000000
    end
  end
  object GbCompressor: TGuiGroup
    Left = 8
    Top = 107
    Width = 273
    Height = 208
    FontOversampling = fo4x
    Caption = 'Compressor'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = 14277598
    PanelColor = clBlack
    ParentFont = False
    BorderRadius = 5
    TabOrder = 1
    object LbCompressorThreshold: TGuiLabel
      Left = 166
      Top = 36
      Width = 90
      Height = 25
      Alignment = taCenter
      Caption = 'Threshold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbCompressorThresholdValue: TGuiLabel
      Left = 171
      Top = 94
      Width = 72
      Height = 20
      Alignment = taCenter
      Caption = '-10 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbCompressorAttack: TGuiLabel
      Left = 18
      Top = 36
      Width = 52
      Height = 19
      Alignment = taCenter
      Caption = 'Attack'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbCompressorAttackValue: TGuiLabel
      Left = 8
      Top = 94
      Width = 72
      Height = 20
      Alignment = taCenter
      Caption = '15 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbCompressorRelease: TGuiLabel
      Left = 92
      Top = 35
      Width = 68
      Height = 19
      Alignment = taCenter
      Caption = 'Release'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbCompressorReleaseValue: TGuiLabel
      Left = 86
      Top = 94
      Width = 76
      Height = 20
      Alignment = taCenter
      Caption = '75 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbCompressorRatio: TGuiLabel
      Left = 19
      Top = 122
      Width = 50
      Height = 25
      Alignment = taCenter
      Caption = 'Ratio'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbCompressorRatioValue: TGuiLabel
      Left = 6
      Top = 180
      Width = 76
      Height = 20
      Alignment = taCenter
      Caption = '1 : 5'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbCompressorKnee: TGuiLabel
      Left = 83
      Top = 122
      Width = 82
      Height = 25
      Alignment = taCenter
      Caption = 'Soft Knee'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbCompressorKneeValue: TGuiLabel
      Left = 88
      Top = 180
      Width = 74
      Height = 20
      Alignment = taCenter
      Caption = '2 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbCompressorMakeUpGain: TGuiLabel
      Left = 173
      Top = 122
      Width = 72
      Height = 25
      Alignment = taCenter
      Caption = 'Make Up'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbCompressorMakeUpGainValue: TGuiLabel
      Left = 171
      Top = 180
      Width = 74
      Height = 20
      Alignment = taCenter
      Caption = '6 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LEDAutoGain: TGuiLED
      Left = 110
      Top = 7
      Width = 15
      Height = 15
      BorderStrength_Percent = 10.000001907348630000
      Brightness_Percent = 10.000000000000000000
      LEDColor = 14277598
      BorderWidth = 2.000000000000000000
      Uniformity_Percent = 40.000000000000000000
      Transparent = False
      OnClick = LEDAutoGainClick
    end
    object LbAutomaticMakeupGain: TGuiLabel
      Left = 127
      Top = 8
      Width = 136
      Height = 13
      Caption = 'Automatic Make-Up Gain'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
      OnClick = LEDAutoGainClick
    end
    object DialCompressorThreshold: TGuiStitchedDial
      Left = 190
      Top = 60
      Width = 36
      Height = 36
      CurveMapping = -1.799999952316284000
      Max = 10.000000000000000000
      Min = -90.000000000000000000
      OnChange = DialCompressorThresholdChange
      ImageList = GSPL
      ImageIndex = 0
      Value = -10.000000000000000000
      WheelStep = 1.000000000000000000
    end
    object DialCompressorAttack: TGuiStitchedDial
      Left = 27
      Top = 60
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      Max = 1000.000000000000000000
      Min = 0.009999999776482582
      OnChange = DialCompressorAttackChange
      ImageList = GSPL
      ImageIndex = 0
      Value = 15.000000000000000000
      WheelStep = 1.000000000000000000
    end
    object DialCompressorRelease: TGuiStitchedDial
      Left = 107
      Top = 60
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      Max = 5000.000000000000000000
      Min = 0.100000001490116100
      OnChange = DialCompressorReleaseChange
      ImageList = GSPL
      ImageIndex = 0
      Value = 75.000000000000000000
      WheelStep = 1.000000000000000000
    end
    object DialCompressorRatio: TGuiStitchedDial
      Left = 27
      Top = 146
      Width = 36
      Height = 36
      CurveMapping = -1.799999952316284000
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      OnChange = DialCompressorRatioChange
      ImageList = GSPL
      ImageIndex = 0
      Value = 5.000000000000000000
      WheelStep = 1.000000000000000000
    end
    object DialCompressorKnee: TGuiStitchedDial
      Left = 107
      Top = 146
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      Max = 10.000000000000000000
      OnChange = DialCompressorKneeChange
      ImageList = GSPL
      ImageIndex = 0
      Value = 2.000000000000000000
      WheelStep = 1.000000000000000000
    end
    object DialCompressorMakeUpGain: TGuiStitchedDial
      Left = 190
      Top = 146
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      Max = 60.000000000000000000
      OnChange = DialCompressorMakeUpGainChange
      ImageList = GSPL
      ImageIndex = 0
      Value = 6.000000000000000000
      WheelStep = 1.000000000000000000
    end
  end
  object GbCharacteristics: TGuiGroup
    Left = 287
    Top = 107
    Width = 178
    Height = 208
    FontOversampling = fo4x
    Caption = 'Characteristic Plot'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = 14277598
    PanelColor = clBlack
    ParentFont = False
    BorderRadius = 5
    TabOrder = 2
    object GuiGraphXY: TGuiGraphXY
      Left = 16
      Top = 35
      Width = 145
      Height = 160
      BorderColor = 11383224
      FrameColor = 11383224
      SeriesCollection = <
        item
          DisplayName = 'TGuiGraphXYSeriesCollectionItem'
          SeriesClassName = 'TGuiGraphXYFunctionSeries'
          Series.Color = clWhite
        end>
      XAxis.Flags = []
      XAxis.Granularity = 20.000000000000000000
      XAxis.Minimum = -90.000000000000000000
      XAxis.Maximum = 10.000000000000000000
      XAxis.Lower = -90.000000000000000000
      XAxis.Upper = 10.000000000000000000
      YAxis.Flags = []
      YAxis.Granularity = 20.000000000000000000
      YAxis.Minimum = -90.000000000000000000
      YAxis.Maximum = 10.000000000000000000
      YAxis.Lower = -90.000000000000000000
      YAxis.Upper = 10.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo2x
      LineColor = 5922921
      LineWidth = 2
    end
  end
  object GbLimiter: TGuiGroup
    Left = 8
    Top = 321
    Width = 457
    Height = 93
    FontOversampling = fo4x
    Caption = 'Limiter'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = 14277598
    PanelColor = clBlack
    ParentFont = False
    BorderRadius = 5
    TabOrder = 3
    object LbLimiterAttack: TGuiLabel
      Left = 103
      Top = 8
      Width = 52
      Height = 19
      Alignment = taCenter
      Caption = 'Attack'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbLimiterAttackValue: TGuiLabel
      Left = 93
      Top = 66
      Width = 72
      Height = 20
      Alignment = taCenter
      Caption = '15 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbLimiterRelease: TGuiLabel
      Left = 185
      Top = 8
      Width = 68
      Height = 19
      Alignment = taCenter
      Caption = 'Release'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbLimiterReleaseValue: TGuiLabel
      Left = 179
      Top = 66
      Width = 76
      Height = 20
      Alignment = taCenter
      Caption = '75 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbLimiterThreshold: TGuiLabel
      Left = 264
      Top = 8
      Width = 90
      Height = 25
      Alignment = taCenter
      Caption = 'Threshold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbLimiterThresholdValue: TGuiLabel
      Left = 269
      Top = 66
      Width = 72
      Height = 20
      Alignment = taCenter
      Caption = '-0,02 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbLimiterKnee: TGuiLabel
      Left = 363
      Top = 8
      Width = 82
      Height = 25
      Alignment = taCenter
      Caption = 'Soft Knee'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbLimiterKneeValue: TGuiLabel
      Left = 368
      Top = 66
      Width = 74
      Height = 20
      Alignment = taCenter
      Caption = '5 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
    end
    object LbSoftClip: TGuiLabel
      Left = 26
      Top = 35
      Width = 49
      Height = 14
      Caption = 'Soft Clip'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = clBlack
      OnClick = LbSoftClipClick
    end
    object LEDSoftClip: TGuiLED
      Left = 9
      Top = 34
      Width = 15
      Height = 15
      BorderStrength_Percent = 10.000001907348630000
      Brightness_Percent = 10.000000000000000000
      LEDColor = 14277598
      BorderWidth = 2.000000000000000000
      Uniformity_Percent = 40.000000000000000000
      Transparent = False
      OnClick = LbSoftClipClick
    end
    object DialLimiterAttack: TGuiStitchedDial
      Left = 112
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      Max = 1000.000000000000000000
      Min = 0.009999999776482582
      OnChange = DialLimiterAttackChange
      ImageList = GSPL
      ImageIndex = 0
      Value = 15.000000000000000000
      WheelStep = 1.000000000000000000
    end
    object DialLimiterRelease: TGuiStitchedDial
      Left = 200
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      Max = 5000.000000000000000000
      Min = 0.100000001490116100
      OnChange = DialLimiterReleaseChange
      ImageList = GSPL
      ImageIndex = 0
      Value = 75.000000000000000000
      WheelStep = 1.000000000000000000
    end
    object DialLimiterThreshold: TGuiStitchedDial
      Left = 288
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.799999952316284000
      Max = 10.000000000000000000
      Min = -90.000000000000000000
      OnChange = DialLimiterThresholdChange
      ImageList = GSPL
      ImageIndex = 0
      Value = -0.019999999552965160
      WheelStep = 1.000000000000000000
    end
    object DialLimiterKnee: TGuiStitchedDial
      Left = 387
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      Max = 10.000000000000000000
      OnChange = DialLimiterKneeChange
      ImageList = GSPL
      ImageIndex = 0
      Value = 5.000000000000000000
      WheelStep = 1.000000000000000000
    end
  end
  object GSPL: TGuiStitchedPNGList
    StitchedPNGs = <
      item
        PortableNetworkGraphic.Data = {
          89504E470D0A1A0A0000000D49484452000009240000002408060000009A4DA6
          1D0000000467414D410000B18F0BFC6105000000206348524D00007A26000080
          840000FA00000080E8000075300000EA6000003A98000017709CBA513C000000
          017352474200AECE1CE9000086F249444154785EED9D77589CE799AF37BBB693
          93FD7FCFC64EB269EB96DEEC249BBA9BDD759C6C9C6AC78E1DCBB62259B2244B
          56B57A170281901042F4327418FAD07B19DA0002248484911132B6229B601312
          0507E73BF7F3C2906128A2CD0C67F59EEB9ACBF659973BF7FBCE577FF37BDE67
          18C6DFE9FFA70D6803DA8036A00D6803DA8036A00D6803DA8036A00D6803DA80
          36A00D6803DA8036A00D6803DA8036A00D6803DA8036A00D6803DA8036A00D68
          03DAC0A2189040D27C3E917E5E7746F81EF9F7F063879785F91CDA10EA7D7043
          C8D103CB82BDF6FFFB9923FBEE9CCFBF7321FF8CE699791DB51FED477FBFE677
          AC9B8D37FDFDD2DFAFD9EC93E9FE1EBD7FF4FED1FB471F9FF5F5F3E81ED0C743
          7D3CD4C7C35BE378F8C057BF725B90CFE14F2EA5E709C7BD0E3D600A3EF5DC52
          79BE11E27DE8C381DE871ECF4C883D1171DCFB654F3F6F89F2F7BE2BCCCFEBE1
          D013C776A5C54547C6079FF2F2E4F31FE18909F07D28EAA4EF9A8853C7FD93A3
          C3C35263228E7AEA7C6AE7893B7D62755CD0895DA1C77D7CE2C382BDA3830256
          9F3CB8E7630B39B6CDE79F75E6893DEDBF2BD4D7EBC59387F73F7EECE0BEAF3C
          F0C0037F3F9F7FEF7CFF99A9786202FC5607791DF8E1DEAD1BEFFFD94F7EF281
          F9FEBBE7F3CF4DC713EE77F421F6F55D1FFDE847FF613EFFDEF9FE3337E399EF
          BF77BEFF9CE699F95CACFD683FF3FD6EC93FA7F78FDE3F7AFFB8EE7E477FBFF4
          F74B7FBFF4F7CB7E3DBF90BD309F7F561F7FF4F1673EFBC6FECFDC2AFB67D661
          A498133EEF8BF6F7BE3BEAF8D16DBC2CB0F1F0F0CF3CAC330823193C1C33B869
          3778F863104632820EEFFDF3E9437B6C8107776F3B7560D7DD01FB77BE6F218B
          31D53FAB7966FE826B3FDACF42BE737AFFE8FDA3F78F3E7FE9F3FBE871401F0F
          F5F1501F0FF5F1501F0F97E6F170C5334FDFB6D4EE4FE3C2CEFC77FCE9137B96
          CAFDB2DFBE5D5F21A4119593687A6529DCBFC7061EBF3FFAF48980A2EC8CD76B
          4A0ADF83C9A3CF138427392CE8707A5CF4959AD2C2F75A6DF586E9D4718F3DDF
          B0F3649822BBF2D353DE6BAEAB313ADACE1A99F1D11E79DE62E7C98C8BBA946F
          4E1A29CDCB365A6D0D4647FB59A32C37DBEDCF7F1C790AD292FF529293615416
          E51B671BEB95A786EA0AB73E8F72E629B5641A1505B9467549214C75C6B9B3CD
          464B7DAD300DBBE3F9D8543C9585B9465571BE51519867D8AC554673BD553CFD
          35FCF8D176573FAF9B8EA71A1E6192FDC471C0A8292BFE6B6561DEF5B0E3C74E
          B8F2F9E1CD782A0A2C464186D9B0A426BE97111F73D56C8A8A0B0B3CF988AB9E
          67CE86A7383BDDC84E8C1D890F397D39EC84EF9920DFA3FFFDAB5FFEE21F1772
          6D3ADDF3D5D9F25892E3FF9212197229C2DFC7CB6BC7D62F3CFCFDFF58F4F096
          DC7FCD95273AC0EFF099A307EFF7F47AD9FD689E4C438E3FF6EFBB7CBF643F6B
          3F5197E4FC25E70BEDE76FF717FAFB3EFD33177D3CBCF9F328BD7FF4FE99EF75
          91FE7EE9EFD77CF78EFD7D813EFEE8E3CF7CF7D0AD7AFCB96920C974F2D8EDC8
          F93C0F7B030823BDC1C355431E1CCE10463278D862F070C3E06182C1CDE01B27
          F7ED0838B177FBE7FDF7BC7CFB7C17C8FECF699E994F14DA8FF6B390EF98DE3F
          7AFFE8FDA3CF5FFAFC3E7A1CD0C7437D3CD4C7437D3CD4C7C3D1E3406AF8993B
          92C283BF971074326429DC0F0ACF996347BF9F97965C4C50E2F79EBE3F557E42
          4F3F9814199ADD5853F9E7DAB2628FDE2FDB79CCD161092596CC3F9E6F6D5661
          8DD840FFE9C23F2EBD7FB7F3649822E2F3CC89831288B874E1BCF1CAA54E2335
          3264AAF08FDB780A3352DEA922D022C111E1E9EEBA68E4A5263A877FDCCA2301
          8D268248E248782EBF7289D04D9E63F8C7ED3C2DAC59477BEB388F3035126E19
          FB319A47782E289E4EE5C7FE696FB1C98FE33CC833BA5E8E1F5947573E1F73FC
          7EC97E96FD23EB35EA67328FFCFFC95E0FE7B99E2B9ED7CD8747982EE229292C
          68D19F1FCE87A7EB628771A9B34305EFB212630717F379E69C7958AB0BE7DA8C
          F36D2D465B73A3515F553E6C8E8D2AF4DEBEF92B8BF17C753E3CE2C5565B6DD4
          579619A5B9D9377252122A234F073CE1499E2678CAF3730CC2A57F880C389E71
          CAE7C88F6974BB6321D7F2E3D73F9CDFE5FC35ABEF17EB257E8447023884C9DE
          319D3E9170C6FBD0D7960A4F6C50403C6D690F6A9E4B46F714EBA5FDFCED1CA6
          FDDCE47CAAF7CFCCD71BDA8FF6E3309567BED71B8EE7537D7CD6C7E7855C8FE9
          FDA3F78FDE3F53BFE771F7F179DA40526C80EFDFF332EE9F08233D4718E91C0F
          9F8D793CEC35B87937082319DC9C9E3BBE7BDB737EBBB6FE93EFCE2D73AE87D6
          3C33BF18D47EB41FFDFD9AF4F0591F7F2636D7393F0CD77EB41F7D7E9F626CAD
          3E9FEAF3A93E9FDE5AE7D3C4E0800F99027C572E99FB9D40FFBB32E2A2F7A547
          87752F85FB2F823677C60507EEAFAB2819A8A4F9C3D3F783C24388656B6156DA
          9B1DBCA0BD70AED5480E3D3D971FCB2CEAF58FF0F06063736E6A629F34A2D843
          2D59F131B30DFFB884875FDFBF56535A645CEC38371E4AA0E16636E11F97F1D4
          969718ED2D4D134212D2B2E3D4B4ECD2EB55FB7A891FE13977B6C978E5E28509
          01129A92660AFFB8D4CF543C126E9117FFD3847F3CC2630F244D11FEF1188F30
          B534D47A7CFF380792644D1D7EACE7513F7636392E8C859196048F70492829F4
          D89125C363674A890C5DB4E799B339FE38EF1FC7BF16475D1CAF38DF2CCAF3D5
          C5E091739E84158BB2D3DFF4D9B1E5F9853CEF5D288FB0C8DE6E6BB619759565
          7F608C6380277964ADDA08494A735B7E86F9AD84889080BD5B5EFAD47C9F872F
          D48FE221D456416B514A74F81B41C7BCF6EEDDBCE1939EE691C06D7A6CD46B31
          81FE9B69B8BA53F38CBE30B4AF97F63331E46B3F266A3F537BD17E66F6A2FD68
          3FF2BC77B1CEA7FAF8AC8FCFD33D4F98E97A5E9FBFF4F94B9E1FCEF43CEA7FFB
          FE993290C4C1F91F0823DD4D18C99F87F36F2FC2C37083973B0637836F7373E1
          7F6CC7E6BB7DB66F9A75ADAFE6B9E9CB41BD5E53BC54B6FF1A4AEF1FBD7FF4F1
          67C697CBFAF83CF997D48E0FC3B51FED67AE4D177AFFCC3EECA7BF5FB7D8F72B
          3522F8B389C1A7CE2C95FB8BF013C7BE949B929059989EF2A7A570BF237E52A2
          C23268B718696B6A34B851F5E8F147783262A31278D0F217698B90977D322269
          0ECD518B7A3C149EECF8E8F8E29C8C610922D9DB3F244032CD58BD99C2090B3E
          FED879CAF3B2FFDC321646B23F3C905F734E31D6DC6D3CAD8CB0EA62CD1C1F66
          48606A0EE19F45F533158FB0C9883487B1EF1EF56377259E6611FE718B1F3B53
          739D75AAE61F977CBF643F4FB75E8EFB8936B025B15E8E4CB4C74C17FE71EB7A
          39334D11FEF1188F0A93B17634997B74FF383F68956381C38F193DEAC7CE76E9
          C2397B186949F0D8435226DAEE16FA7CC3F1FC359BEFFB4C0FC6E5DAC09292B0
          A0E7BD8BC9A3825B9CFF082EBF4B93D4A9F93C7F5E6C1EB986E23AE1BDD498C8
          827D2FADBD7FAECFC31793475E84A91197359523D92989957E87F6FFBB2779A4
          45AE89731CA1AD77A5DDEAE8EEED5FF73C4F8D8C6E1C8EA1DDEAF491FD9F5B0A
          3CB482FE59DA1D827D0E7F56F3FCED65E6E8FEA931B49FA95FF06A3F33BFF8D6
          7EB41FE7FBE599AE77A66A41D5C79FE9F790FE7EE9EF97FE7EFDED9DBCE41316
          F37AFEFFE7EFD7A440D25833808491227959602CE2C379833092C1CDA0C1C573
          A4F7CB1BEF3EBAEDA59B3625699E9B8649A4C94AAFD7348124BD7FF4FE197B58
          A78F3F7F1BA339D5C35EED47FB59C8CB5CBD7FF4FED1FB67B45974495D8FC9CD
          8E39322475A95CCF4BF887514865D5C5F92A68B3C0A61DFBCBB8791F7FC44F5A
          6C54858CF8E9641C8A953699398EA55ED4F3A9F064C547E7CAAFACEC8D2DC2B6
          C0F0CF82FCE4249A2C55FC725D1A07EC0FE0644492E994DF7CC3360BE691FD73
          8E1164CE0F04634FFBCF276CB3283CE75B5B26F1085F4C80EF5CC3362EE511A6
          B06387E712B671398FBC189D43F8C7E53CE2A8F37CDB6CC33F6EE1112669B570
          1AFBB5E8C71FF9BECBF76BBAFDECFC9D9350CB34CD3F8B727C9E2B8FF055327E
          6F86F08FDBD6CBD15561A679BAF08F4778842D2739CEE3FBC7793F31DACA39FC
          E3313F76B6828C546922F2D87E9EEAA5580D234117FA7C43AE37E6F3FD9AEE25
          5DE7F97623807BA1F93EEF5D6C1E7B702B9991A0F379FEEC0A1E615221A9CC34
          C37BFB26D35C9E87BB8A47C671C9B85253F0A9D23DEB5FB87FB6CFE75DC5237E
          5A09E3E7A6A534FB1ED8F73D4FF3C8B9B0BC30CF880B3B53EDB5EBE5AF7B9A47
          9ABF08D919D181FE797EFB777EC1D33C728F52949D61C49E09B09CF13EF859CD
          33F1C5B3F633F38B78ED47FB994BF0C7F9EFD5FB47EF1FBD7F7CFFDE55D763FA
          FBA5BF5FAEFE7E4D0A248D8D699366245785910C6EBE0C2E56FDBDB66EF8A79B
          CDF4D63C33074AB41FED87E631C7873FFAFBF5B73191533DACD37EB49F85BC1C
          D4FB47EF1FBD7FF4F5E194EDA263B5CFD109412797C4F573F031AF8FE424C565
          CBCBE54518B3B5E0F3A9F8498E0C354BEBCF451A07E42588392A7431C336733A
          3E0B8F34ED48F8470211F61BAEC28C94C50AD7CD8BA78A97E9D2FCE378035851
          60598CF0CFBC7864FFC85834E71B52E19C47D3CEBCAF57EDEB35CA7376CAF04F
          45BEC56DC7E7D9F088B3425E66CF336CB380F59ADA8FFD05AD393A6C3E611B97
          F148B346D48963730DDBB8860796D1913FED4B82478E4DF28256028A2DF5B573
          09FFB8C48FF048584B028AC225630A6619FE7119CF05C2A4E73926D8992C29F1
          B309FFB8944746B4C94B7EF9D5A2AC9DE9F4899B857F5CCCD3AC026D1718AB25
          6B284C61BE47660AFFB89C471E74CBCB75F9AEC9F5807CEFCED0AE394DF8C72D
          3CEDF09CE77C278EA461473EC1470F2ECAF58F5C6FCC74FE526115FEBBB29F65
          EF881FE191BDE4387653FEBE488E970B79FE33DBF3D7743CB28F64BD1CAF0B52
          A3C35D7A7E9FC98F5C33755D9CD806287F7F052366C7C2486EDB3FB25EA33C13
          C780DA5D89B7505FAF25C3331AC06D3708E71B3C9B3F319BE7F30BDE3F33F8B1
          5FA7484B61E8711FF3EE75ABFEF966EF0B5CCD333A56C546482AB966C7FA173E
          E6691E59AFBAAA3223E6CCA9BC83DB367DD4D33C3286B780D02DEB95127070CF
          9D1EE7E13B5894952EA1AD78CD33F9856ABBF633EDF1598E3FDACFF4E72FED67
          E6F3BBF6A3FDD8AFF5A6BD7EBEC9F58F3EFEE8E3CFCDEF77A6BEBFF8FFF1F833
          E1A50AE196DB69467A6E0E6314DEE1977ADDFC32AE8D87516DD43E7753B3FCCE
          89BDDB6FF630416E78DE3EB265FD738737BF78FB7417AD9A47FB99E6618BDE3F
          53FF52CFFE7043FBD17EE465933E3EF3B07D866609ED47FB19BF06D2D71BFF7F
          5D6FA4869FB943662EA745850ECDB2C9D3A5DF77E1216C73803137C3F2D28F96
          9B9B35EDB89C27292CE8E586AA72C66C9D572F8DEA2A4A3D763CB4AF978CD992
          17B4F69B7579A89E107C6AAAB08DCBFDC8FE91B144F2F2D1396C234129A7B08D
          FB78A669DA4985C961AC95C779E48576FC99007BD8C6E33CF2E2585E924AFB8F
          ABAF7FECFB59ED9F69D64B5E3ACA5E97C0DB52E091F088BCD49697DC34A3DC70
          E5FDFB6CFC881B79E87696B1721CA746183FF49AAB9E27CC8A87E3A4B8A9E538
          595D52F897B2BC9CC1A0C3FBCEB9E2F9C65C78E4B85D9099F66E714EE63B8C43
          BAE6699EFACA32696718B6A4260D945832DF4A8E08B9B2D8CF7FE6E247788AB3
          D387B31263DFCA888F7903A6FE503FAFAB8BF93C6A3E3C96E4F837D34C91AF65
          27C7FF0EA6411C0D2ED6F3B1F9F264C6C7F49A4D917DC5F0C0F4EE52E0498F8D
          BA9A6B4E7ABBAEB26CA488D68F9B849166F57C63217E32E2A27B33E34D6F5515
          E7FF458E078519E699C2486EE1C94E8A7B333F23F5DDD6A60643467E493BCA42
          9E8F2DD4CF18CF70A3B54A9DE3E4DC2BC1448766A4393D8F5A0C1E1AC886A535
          4AAE29ED413209B4F8EDDE26CDFC1EE191737F1B7BC839B8151F1CE8391E7E10
          30D5D80C2BACC7766C71DB7E96F5527EB85F72BC27B05F8BCB1A4AC825DCDF67
          E866EF0B166BFFCCC4630F495515178CF8ECD9BE63A6F717EEE2917DC588BBF7
          5262224396028F7CF72A8B0BFE1272C2D7E7D4A1BD774CF77EC75D7E24409699
          14371C78F4E0CB4B85272725E1CF61C7BD376B9EC92129592FED67EAE3A11C7F
          B49FE9CF17DACFCCE753ED47FB91FB19FBFDB2DC9FCAFDA0DCEFC8F5FCCDAEC7
          F4FED1FBC795FB6742208930D2E709239D9BC5CB9D2B3C5C4DE361E64E1E1E3E
          C9C3B11FF1A0E5473CFC7992871B3B799890468DF095196E960D6E2E0C2E9ECF
          1DDAB4EEF3D35DB06A1EED679A871B7AFFE8EF973EFE4C1F26D1C7673FAF99C2
          00DA8FF633A9E1465F6FCC7CBDC1AF4F1F24C0F16A3CBFF2BFC9982DB77CBFC2
          4FF87CA728C3FC7A2B2FB3EB2B4B8D58C6474D133E740B8FF8E125D19B8E63AD
          A41925CCE79073D8C66D3CB9A9897DCE611B5EFCC373C8316CE3369E82B4E4D7
          5A1AEA26857FE42627CADFC71EB6712B8FEC9FA9AA68E5C55F38E7589A6D9604
          8FBC78E005A9045B7ADD71FF25FB59D66B3A3FD24420CD0DE9B1916F2D051E79
          11222F1F09910C459FF42D70F5FDE9CDFC48304A5EF01565A70FA6448535041F
          3DB0D795F7CB37E5E1D778F2B28D074F03891121F5E1C7BD8F799A478236B929
          09BF4F083D5D19E97FCC1747CB5CF53C61367EEC3CC911C1E5EC219FD0638797
          C3F33FAE78BE31579E9853C7BD8527E8C8FE9F2C259E335EFB7F1A7060D78F17
          FBF9CF7CFD084FD0917D3F85E7A9C57C1EB5509ED347F63DBB9478CE1C3DB89C
          D15A0717EB79DD42FD084FF0B123C74FECDF65598CE7878BC143838DAF2938B0
          FEE4FE9D5717FA3C73317822FC7DBC13C2836D1CC3DF8669BAF0CFAC9E8F2D16
          8FE9CCA9BA124BD650390D8EA70EEE716E2292708B5B7962834E56E4A5A70C4A
          C845AEE1A485C8298CE4569EB8E053E56971516F9FB5D5AB5092FC5A3E8560F9
          D8A400B7FB119EF4B8E8DFD757578C8F0496E04F0DA38ABD5FDEE4111E5E8AFD
          BEAAB85005ED9CAFC5A5B16936EF0B166B3F8B1FE1A92E2D343AB88673E61157
          0DB83BB66BDBA599DE5FB884679A56D08B04020907BFB97DEDF3DF9AEE7D8A3B
          7924ECC6B5EF1F424EFAFD7A29F0C83AE667A6FDDEEFD0BE1F2F059EC6DA6A23
          3E2CE88DE37B777C6D49F0102A4D8A0879EDC481DD0F6A9EC9212909DD6A3F1C
          0FA739FE683F63E70BED67B9FDFE4BAEE7E57A75C2F954FBD17E0855CA3595B4
          873A3E6FB13FDFD0FBE7D6F533FE528E9771EF238C14709330D2300FE72B79F8
          FC3C0F7BEFE661E66DCE172F3CDCB88D87097773F3FE3C61924A6A7287A7B919
          34B898370E6E5C1B70E0A535EF73FEF7689E17B51FBD7FF4F76BE2CB5C7DFC39
          7E74A63080F6A3FDE8F3E9C45FC2DA1F66FE7F773EA5F927268DB13A33847FDC
          FA7DCF888D4C9217DAD2B653CCAFB3A708FFB895272134C82C6311ECBF3C9611
          32D24A14EA7DD01EB6712B0F61AD7869D69007D68E0FB12B0B733DC7C3AFC5A7
          1A19612D2B12A677DD793DAFFCC868B4295E3AC8AFA3F3CD4923F054B9EBFE62
          261E61ACAF2AFF6B7CF0A996A5C0237B9B17467FA501E495705FAF75EEB8FF9A
          C98F8C46E205E47BA9D1611778E1B7CDD33CF2A0B43C3FE7BDA4F0E0F361C78F
          6E61CDEE71F5FDE94C7E8487D171EF3116AD2D3AC077230F59EED53C7F7BD0AF
          FDF8BC4FEF9F999FFF683FDACF429E1FFE6FDD3F27F6EDBC9767AA6B16FA7C75
          B1FC08CFC9837BB61CDBB9B56629AC97F09C3AB26F5B7CD8998B84DBDE9DEFF3
          E7C5F413E47560735A5CF4ABFC60E1BD187ED0314518E9A6F7A78BC913E27378
          635CC8E90B4D7535C639DA1D094D1B47B7BDE41846723B4FCCE993E76925FCAB
          DCCFA9E016D7E026465F8F8591DCCE131F12D8566CC9FAABF3FD537343AD7186
          1F74DCECFDC562AF97F0C8FE91E0BD73484AC63BD29E66EC5DBFFACC74EF535C
          C1C3BE79EF5C6BF394A12D1B7BEBCC31AFF4A5C223E32FB392E26B1EFDC5CFFF
          71AAF74DEEF423FB5B028AE101C773FFF33BDFFA074FF34810B02437DB38E575
          306529AC973C0F288527C4D72B5EF34CFE7E693F8737CAF170BAE38FF6A3FD4C
          F7BC45AE0FE5FA47EF9FA9F31BDACFCCCFEBB41FD7F9190F241146BA9B30D21B
          3779F995C583DEEFF3F0F9839D3579B7F5B555DFC3E7A1ABCD958F7637943EDA
          5953F0506B59F63DB57929B771B3FC416E4EBFCFCD6016375FC3D3DCEC185C6C
          BCB17FC30B773B5F90CD95E7424DBEE279ADAD66128F352FD5933C3FE86DAE7A
          EC9586B2C7F0F383B36539F7788A0737E26729F1DC0BCFC3F8791C3F8F5FA829
          7C183FF77AC8CFEDB008CF0FE179029E27E0F9E118CFED1ED8CF4B91E73EFCFC
          083F4FE2E749FCFC083FF7B15E9EF273FFD536EB8FE1790A9EA7E0F9313CF77B
          82A7A3A640D64BF15C69A95E4A3C8FC0F3745763F9D3F879C4937E70237EC679
          3AAC458FB4945BEEAFC933BB7DFFC87AC1F2693E3FC1CF32FC2C83E727F07CDA
          833C9F81E7A7F03C03CF33F0FC149ECF689E3D1F1C5B2FF1F333FC3C879FE7F0
          F3B3C5F2630AF0BB8F00507FC29993D3356B49B865FCFA678CE7B3F0FC029EE5
          F02C87E717F07C7631D62BEAE4B1CF1466A4BC23811BF9956F4E62AC73D3CE94
          3CBD6DB5BFE869A959749EC8133E9FAE28CCFB9363FB4F1B5CE9A608C7B0CDB8
          9FF33585F2FDFA1C3CBF8467C5A5C68A15F8F9257E3E579D97B6E0EF7B6CE0F1
          FB69FF19ECE441ACF3C3D9929C0C69DA99E047786099C4D35C9EBB683CB25E32
          5EC099E76247BBB4808C047B1DC8B15F3F3BF03C8A9F95F85979DE5AFCE862F3
          C8FE71E691315B0483FECAA8AD32279ECFE3E8317856C1B30A9EC7E0F9FC62AD
          977D3F3BF35CBA704E856D1877D718EA73E821B9BF18F3E3111EF9E58E846D12
          C3831B228E1F7DD881E70BF8F9157E56E367357E7E859F2FB8DA8FF0D0FAF31E
          E3F7AA1D78EE8045789E80670D3C6BE079628CE78E855EAFCAF76BBAF5121E09
          DBD0225365E769AF29523C57DAEA9612CF17E1F9F5AB2DD6B5F8598B9F5FE3E7
          8BAC9747FCC032CE73B1B152F13495E77992E74B303D899F75F0AC83E74978BE
          E4413FC2F3143CEBE159DF6E2D794A78AAF2D33DB55E4B8DE7CBF8F90D7E36E0
          67037E7E839F2F7BD0CF57E05906CF467836C2B30C9EAF7892A7A7AD7E49F0B4
          D514DF819BAFC0F3CCE596DA4DF8D9849F673CE547786099C4632BCFF7C87A8D
          F17C15A667F1B3A5B3B16A0B7E9E85E7AB9ED83F0E3CCFC1B3159EAD6DD6D2E7
          34CFE8F5CF989F0758AFE5F8D9869F6DF8598E9F072AF333DC7E7C1EE379109E
          15F06C87673B3C2BE079D0133CADD612F97E3DF86A7BC38AEEB37553F29CD8B7
          E321EFED9BF3E6F33C7CAED7630E3C2BE1D9819F1DF859E9E8E7D4A13D0F871E
          F7AE0B38B8E72F737D3E3F1F1EDC889F197968236B9197CC129082CB398C34ED
          FB8279F27C0D9EE7F1B30B3FBBF0F33C7EBE66DF3FE227FAF4C946096D49DBA5
          FC1087B19232D9C11E465A4C9EF7C3F2753EABE0D90DCF6E78563596177C1D9E
          F7CBF5BCF0449EF4ADAE1A0B6D39FEF085115784C95E7219CF85C6EADDADD6B2
          493CD1A78E578D86B626365B099BB4A684F81D1D98EAFDCE3CD6CBEE67357EF6
          C2B3179ED5E2A7223F73DC8FF014649ADF737C4E61BFDF949002CD697FDAB961
          ED379CDF37CD93E71BACD71A78F6C1B30F9E35F07CC399A7302BFD3DF90187F3
          7DAF04DD182F391C72F2F8CF168BE7727BE3AC78A66ABA116739A9897FDAB379
          C3B717CA73D65A2AEBF56FF0AC7DE56CFD01FC1CC0CF5AFCFCDB547EA6E29167
          29F1E1C17FD8BF71DD671683079605F148808C6714EF78BDBCE9FE45E459879F
          83F839889F7573F1E3029E6FE2E845780EC173089E17E1F9E66CD7CB953C1D8D
          358AA7A1BCD0933CDFC2CF7AFC1C81E7C8596BF97A78BEE5413FC2F3123C5EF0
          78C1F392A779BADB6D9378CAF3B3261C9FE57838D5F7DD05FBE7DBF06CC48F37
          7EBCF1B3113FDFF6044F8BB5ECFDB0289EAEB30D4B85E73BF06C82C7073F3EF8
          D9849FEF78D08FF06C86C7171E5F78362F359EFA8A224FFAF92E7EB6E0C7EFBC
          CDEAD7525BB1059EEF7A68BD3E00CBF7F86C83C71F1E7F78B6C1F33D783E60BF
          5E95EBB1B97CDFC703498491B6DD640C8734234918E98EDF9DB7FEF3EFCED73D
          7ABDA376DFB5F69A53AFB556055DB19506BD525B74EA7C65EEBEA6A2F447AB73
          12FF9930D21DDC7C7D9F9B9D4A87DA5EC78B79838B5563DFFAD5DB9C4FF073E1
          B976BE76461E5B51C6A3553949EEE4F9107E1EFB5D47DD8137DAADA7AFB65687
          F4D8CA42BA6A8B4FB757E61D80E731783EE4463F4B8EE7DAF9FAC7F173083FC1
          F809C34F187E82F173083F8FBBDB0F3C4FC0E3054F283C91F044C2130A8F173C
          4FDCE23C77E2E749FC78E327023FD1F889C64F047EBCF1F3247EEE74D77E7EE3
          7C9DF03C05CF3178A2E0898527169E28788EC1F394A7795EB5958FF3341667BA
          9BE72EFC3C8D1FBFD7DB6B4DBDAD35F1F0C45FAA2B31E1C70F9EA7F173971BD7
          EBAE373A1A9EBED6513F2D4F654EB2BB7996C1E38F9F38FC24E127093F716D55
          F9FEF859E6019E67E039094F023CA9F0A4C29300CF49789EF9DFC6931074726F
          4AF89999C6EA8D5F6FF07DFF30FBE739FC04E227193F69F849C34F327E02F1F3
          1C7E3EBC90FD9C1C1674B8A6A45055D83754971B19D4C43B34EDC8D826679EE5
          F004C1930A4F263C99F0A4C21304CFF285F2449D3A7EBCA9B666BCFD471E66B5
          F24044B808DB4CE079FD7CBDF859CE27A8AFBD2EF54AAB35F3B2AD625179941F
          2AFD9DDB7FE4216D9E39D108F13E586DBF3E84E723B0ACE0130C4F3A3CD9F064
          5FAC2B4DC74F704371D60AFC7C64A1EB555D52303E72C0FEB04F7E79D8585369
          24879F91669BFF94EBD5D9F054E4A42C98C7BE7F1C1F3C0A8F04CA32E2632E85
          FA1CFE819DE7F58EC695F809C54F267E72F1938B9FCCD6AA8250FCAC74158FFC
          EA58C60D503F7E612C4CA2FCC0B30A9E707872E0C987271F9E1C78C2E159E52A
          1E79482C63AD922342CE2D091EF6B3D4084BB38D03CF47F1B31A3F91F8C9C54F
          117E8AF0938B9F48FCACC6CF4717BA9FA7DA3FF2FD72E6E9EB68109E35F0C4BC
          76BEBE009E92EEA6CA12780AE08981678D27785EBFD0A8787A5A6B3DCDF32FF8
          590B4F2C3C45F094E3A71C3F45F889C5CF5AFCFC8B1BD76B4A9ECEBA32C5535F
          9CED769EBE0EDB3AFCC4E3A7143F95F8A984A7149E7878D6B9DB0F3CEBE14984
          A71C9E1A786AE0293F5B559808CFFA724BAA5BD76B89F17C0C9E0DF849C64F15
          7E6A5F69AAAAC54F157E92F1B3013F1F73E37E169E8DF0A4C253034F033C0DF0
          D4C0930ACF4677F2BCD6D1A878FA2ED852AF9E6F580A3C1F8767133C69F0D4BD
          DA5A67C38FED425D791D7ED2F0B3093F1F77D77AE1E7E3AF75342D359E2DF8C9
          C44F037E5AF0D3829F06FC64D615E76C71B39F4FE0672B3CD9F034C1D30A4F2B
          3C4D2D5545D9F06C85E7136E5CAF9BF29459CCEEE6D9861F0B7E5AF0730E3FE7
          F0D3821F0B7EB6B993E76A874DFC6C7FED42535EEFF9C656782E7435555F80A7
          159E3C78B6DFE23C9FC4CF0EFC14E0E7DCE5B6FA8BF8B9889F73F829C0CF0EFC
          7CD25DFB99F5FAE4D50BCD93783AEA2B26F1F8ECD8F2DF47B66EB0CEF57D81E3
          FDBBE3FDD754D7CF633CBBF053849F0EFCBC829F57E0E9C04F516DB16597DDCF
          C903BB1E8E090AE84C0C3B432BD241E730D2B4EF2FE6C8F329FCEC86A7149E8B
          F05C86E7323C179BAB8A4BE1D90DCFA764BD84C76C8A7C95208BD1546F35CAF2
          2DC6E923FB65D28484913CC2C3C8C68BADB606F5ACC2FE7C40425C61C7BD1793
          672F7ECAAF9CB775E1E7CAA5A69A2BF8E9C24F397EF63AFA890EF43F2F2D56CE
          CF2AA4598A5185C68E352BF739BF6F9ACB7AF57634C97AED83A7129ECBF05C85
          E72A3C97E1A984679F230F2D5F6D728F6B6F72B6EF4FE16BAAAB9696A498C5E0
          81695A9E524BDAF8FE119E8AA2FC494DC5C22321204281710BE4F957580EF0A9
          C64F4F775BC3EBF8791D3F3DF8A9C6CF0178FED5BE9F85A7AAB8C090B1838EDF
          5DE1A9AB2C334E1F3B12B6509EDE0B2D9378CED757CE894702640559E9C6B13D
          DB8F2E02CF41FC58F173153FD7F0C323FBCAABF8B1E2E7E06CFC480890C63483
          317B8717C873377E0EC153074F1F3C6FC2F3263C7DF0D4598B730FC173F7CDD6
          CB1D3C4D55256EE7B9D2D12C7E8EF069C4CF35FCF4E3A71F3FD7E069C4CF1177
          FA19E3F182A7A9E77CD37578DEBED8647D1B9EEBF034C1E355624977DB7A4DC7
          D35E5FE5299E7B70E3CDA7053FFDF819C4CF203CFDF869C18F377EEE71D77EC6
          CF3D572E9C1DE779A5AD7129F0F8E0A7153F03F0FC113F7FC4CF007E5AF1E3E3
          663FF7E2C7179E767806E1B901CF0D7806E16987C7171E698555D73FD39D2F16
          EBF8C37A4DCB63AB2E6DAF29C9732B4F4F478BF0F8F139FFEAF9E621FCBCDBD9
          54FB2E7E86E0390F8F9F3BFDC0731F2CFE7C3AE1B901CF7BF0BC07CF0D783AE1
          F187E7BEF9ACD778208930922DC2F7C8546338E4E5D795B1B1051F2484F4CF6F
          75D62FE713C89F0713480A7EADA52AB8A7A92C9840523081A4E0E6E28C406B4E
          F2F2E2D4A87F268CF4416E2E9EA776F58AD32F0BEC17ABC6DE1757D99C4FA80B
          E579B5A9DC233C84003EF46667C30A3E41FC79A80AB8B45487C2133A1670096D
          2ACE0CC2CF0AFC7CC8D57E66CB539393B2A22835DA5D3CCFE32704B608157069
          A98EC04FC458C025023F21F03CEF469E55F084C113A5022E2DD551F0448D055C
          A2E0098367D52DCA73276E5EE013891F137E4CF831E1C7841F09B898F013899F
          17F073A71BF6B3F0ACE5130D4F1C3C71F0C4C113074F1C3C71F044C3B3D6933C
          979B2A3CC573D7F5CEC6757C62F193486022B1F76C4D223C89042612F193889F
          58FCACC3CF5DAE5E2F421BE33CFCF9943CB692ACD86A4BAA3B79D6E3271E9E64
          1570395B938C9FE4B1804B323CF1F0AC2F4C8D71979F97E04982C70C8F191E33
          3C6678CC0438CCF024C1F3D24278E2834EB4C59DF6BFE9F97D6CBD5CCE931917
          75A985076A1290B0F2708DBF5601A0B1B0CDF8F5063C1FC6CD463E29FC793A7E
          D2F1938E9F74FC48C0251D3F29F8D9889F0FCF773F6750A7DFDE6C9BF04044DA
          657292E2A401E8AAFDFA678C67333C69FC79163C59F064C193054F163C59F0A4
          C1B379213C69A688AEB6E6C649BFAA6B6FB14928E94D02406BE497CB8412C48F
          E2E1CFB3084C645D396BCDE285EEA2F2C87A35F39071AAF69FC24CF3DB51278E
          6D1CE3F9C8EF3A6D5BF964C06381C7028F051E0B81000B7E2CF8C9C0CF56FC7C
          64BEEB65DF3F93ABEB5BF91564EA60F871EF2D4B8147D62B3B296E40C63639F0
          6CC34F367EF2F093879F3CFCE4E1278F40405E634976367EB615A49A16DD8FF0
          D0D6F47B279EEDF058E02984A7109E42780AE12984A7101E4B95C5BCFD16E5F9
          286E76F2C9C74F317E8AF1538C9F62FC14E3A7183FF9F8D9899F8F2EF67E9E62
          BD8467379F4278CA080494C153064F193C65F094C15308CF6E77F0104AF8E8B5
          8B4DBBF914F2E78AA7E76C6D192F2C3DC5F32FB0ECE153024F053C15F054C053
          819F0AFC54E0A7043F7BF0F32FAE5E2F18A6E521C0E1299E7DF82983AD1A3FD5
          F8A9C64F353CD5F8A96E28C929C3CF3E37FA39004F253C5678ACF058E1B1C263
          25A06085A7129E03F9E65877ADD752E2F9186E0EF2A9C64F3D7EEAF1538F9F7A
          FCD4E3A71E3FD59596B483F8F9981BF6B3F01CE6530B8F0D1E1B3C2A70038F0D
          1E1B3CB5F01C76070F21898FBD71B1F9309F5AFEDC4660C2D33C1F87C58B4F3D
          3CCDF034BF7AB6AE9917CCCD04029AF1D38C9F7AFC78E1E7E3AE5E2F189622CF
          51FCD8606BC54F2B7E5AF12301A056FCB4D697586CF839EA263F9F80C5874F33
          3CEDF0B4C3D30E4F3B3CED0414DAE16986C7079E4FB861BD96228F2F7E5AF1D3
          819F0EFC74E0A7033F12E0E8C04F6B8525DDD71D7E08017CE2F58B2D7E7CDAF9
          F34E021C9DF074C2D3094F273C9DF0B4C3E3E7499E4BCD359378F2CC712EDF3F
          38F9246EFCF974F0E75DF8E9BADC5ADF054F177EBAF0D3859F0EFCF8C3F34957
          EFE799782450E2219E93F8E984AD1B3FDDF8E9C64F373CDDF074E3A7133F27DD
          E4E753B004F0E982A7079E1E787AE0E9190B70F4D495E476C11300CFA764BD8E
          6CDBB8DE77CFF637FCF7ED9CEAC7DD53BE4F99EE7ED9F9FE028639F31CDFB7EB
          65425203A57939867C224EF83A86A3168327103FDDB0F51208E8C54F2F7E7AF1
          43397E712F7EBAF11368F7E3BF7FD7D6C2ECF4A1F36D2DE32129795969494D34
          8E6EDFBC609EBE8B6783F8F4C0D327010E78FAE0E98347021C7DF0F4C01364E7
          093CBC6F63AE39F90FF2E39B093F5622E052CD8FBC8EBCBCE982F3FBAFD9AE17
          21924FD979F8F36979CA733326F0244785BD3DD5E8BF0E9AC0524D11AF3DF0C0
          03B73932CD81E75FE139C3A7179E6B2AC0D1DA70ED62B395C76515D7F0730D3F
          BDF09CC935C7FFABEC67F193121DFEFB369E4D4C6A4FE607391673D2EF7EF693
          9F7C60B1792450321D4F6A4CF8EFA5B1C599E73CE32D234F9FE8FDE8473F3A61
          CCDE1CFD84E0A70F3FD7F1731D3FD7F1735D0225F05CC74F1F7E421CFD8CF234
          4CE2A9ABAA3082FD8E762D60FFDC0D4B189F37E0E987A71F9E7E782470D30F4F
          3F3C6FC01306CFDDF6F59A9EA7DC08F3F7B9E42A1E020AFDB525796EE3212421
          7EC2F95CE7CF07F033809F01FC0CE047021C03F05CC74FB83BFC08CF6B175B23
          F9BCC59F0F4A80039E417806E19100C7203C6FC113E9699EB14089E229CBCD74
          17CF3DB889E633809F21FC0CE167A8B3B97648021CF8198267009E688B39E11E
          57EF671826F1BCD2DAE8699E18FCBC03DB0DFCDC80E7067E240074033F37F0F3
          0E7E62DCE4E75E5862F90CC1330CCF303CC3F00CC3330CCF303C43F0C4C273AF
          1BD66B461E0237C3D6D27CB7F110FAB9F7EAC5B6783E7FE2CF4708008DE06704
          3F23F8198167049E3FE127DE1D7E60B80F96443EC3FCB9018F018F018F018F01
          8F01CF303C89F0DC37D7F5528124C23F771246FA73F8B1C3CE6341248C64F032
          2E8D972977CB983669469230D29B17EA82694A0AEE3B5B1D7CB5B93CF8D586D2
          E04BB5050492728269480AB6E624069698A31F2D488ABC8D30D2DD8491D2C666
          2E3B5FAC1A7BD63DFFE7DD6B57DE693F89CD9647C6B4D979AE5FA80FA629699C
          E7724359F0C5DA421A772CC134A604D7E42405169B63DCC1F3988491E0098527
          143FA1BDCD15A1F084C243E38E25149E507882E079CC0D7E26F0BC76B6C6A33C
          D28C246124FC44E027029E08FC44E027023F34EE5822F013819F10FC3C9E9F14
          E5CAFD73BB34234918099E2878A2E08982270A9E287868DCB144C113054F183C
          4FC073BB0BF7F352E47952C248F831E1C7841F137E4CF831E1874092C5841F13
          7E22F1F3A41BFC3C25612478E2684E8983270E9EB8EEC6F23878E2DA2A73E3E0
          8983271A9EA75CC923639BA4A94985A32E348CF35C69AE543C9DB5458A87C614
          77F23CADC248171A12F193889F447812E1498427119E447812AB2DC9B1F879DA
          D57EA419C991E76AABD5D33CCB248C849F64FC24C3938C9F64FC24E327193FC9
          F849C64F7C91D9B46CBE7E4C01BEC312F00DF339E4DCB4337E3E95FD839F6724
          8C048F191E333C6678CCF098E131C36386C70C4F123CCFE42545CFF9F813E5EF
          7D57415AF25FA4765CAA526B4A0B55D0C674CA6FC2F9DD5D3C54B17FB4282B6D
          C479DC965446E7A72519E17E5E3972BD31C6F39C8491F0938E9F74FCA4E3271D
          3FE9F849C74F3A7ED2F193829FE7E6EBC7C2482D094739B7DBD45795D3DE125C
          CE35D03D633CCB25FC034F163C59F064C193054FD685DAE22C78B2E0C982270D
          9EE5F3E591F5928743CEBF189396A4E4C810EB19AFFDF73AF25CBBD09845D38D
          E2E1675F59AF3456289ED6CA3CC553654959308FEC1F673F0DB411C58504D608
          8F8CD99266240923C16381C7028F051E0B3C16782CF058682821E09292819F15
          0BF1E3CC23BF60ABA5DD46D66B3A9EDED65AB7F2D4F30B3F471E69469230127E
          F2F093074F1E7EF2F093879F3CFCE4E1270F3FD985E6D8958BED670A9E551246
          82A7109E42780AE12984A7109E42780AE121E092628167D52DC8B35AC248F829
          C64F317E8AE56795F829C64F317E8AF1538C9F7CFCACCE4D8E59D0F1D9F1FB25
          FBD969BDEE9066240923C153465352193C65F094C153D6515B52064F193C044A
          520AE15903CF1D73BD5E753C5FCCC42363DA8447C2486F74DAC6795E6DA92EEB
          6AAC543C672BF3DDCDB356C248F054E0A7023F15F054C053014F053C15F8A9C0
          4F097ED6BAC1CF049E2BAD759378684CA9A8B4A4BA85479A91248C849F6AFC54
          C3538D9F6AFC54E3A71A3FD5F01070492DC3CF3A57FB9126220923C16385C70A
          8F151E2B3C5678ACF058E1B1C253599016B7FE16E4D9206124FCD4E3A71E3FF5
          975B6AEAF1538F9F7AFCD4E387804B6A357E36B8C1CF461546EA6CB2C16383C7
          068F0D1E1B3C36786CF0102849AD8567A33B78248C243C34018DF35C6AACB29D
          AF2D9DC4634936B9EC782863A4A41949C248F034C3D38C9F66FC34C3D30C4F73
          4B6541337E9A2B2CE67AFC6C72358F342339F2F4B4D67B9A678B8491F0D38A9F
          56785AF1D38A9F56FCB4E2A7950617022E661B7EB6B8C1CF560923C1D30E4F3B
          3CEDF0B4C3D30E4F3B3C44907308B8989BE1D9BA9478F2D3E2DDC1B34DC248F8
          E9C04F077E3AF0D3819F0EFC74E0A7033F045CCCADF06C73839FED1246EAEB6C
          EEA4E9A6139ECEEE166B273C9DF074C2D3090F8112733B3CDB5DC92363D1A4A9
          692A9E8BB6EACEF6BAB24E2EA5DDCDB343C248F8E9C24F177EBAF0D3054F173C
          34CA1476E187808BB9033F3B7292635D763C143FD28CE4C8F36A5B83A7797649
          18093FDDF8E986A71B3FDDF8E9C64F377EBAB94D25E062EEC4CF2E17FB79BF34
          234918099E1E787AE0E981A7079E1E78E456AC079E9E724B5A173CBBE179BFAB
          AEE759AF79F31CD8B836EB0823E39C9A9A24FC33E9FDCE6CEF2FC678F6AA3052
          674B2F7E7AF1D38B9F5EFCF4E2874052612F7E7AF1D39D9796B057FC786D7BE9
          1EFF03BB2B084D1899092623E6F40983D62D99C4B1183CFB5418A9B3A58FA6A4
          3E78FA5E69A9ED8387DFAA9411482AEC83A70F9E1E78F6098FF7F64DF7460705
          D8A4D54A9EE9480BB71A69575305DB4963D7DA95C38EEFBFE6E8675A9EB6BAF2
          BEA6AAA22979A2024F344A73B3638B94FC794B439DC118C7BFAC5DF6EB8FD9DF
          C7CD9647C6B44933928491F0730D3FD7F0730D3FD73A6D35D7E0A151A6E81A7E
          AEE1A7173F07EC7EC2FC8F595BA668B59256294602BEB77DC3DACFCD87479A91
          1C792EB735CE8A27E2A46FB9956749DDFCF79D9F799517E6FD75C74BEB3E3F4F
          9E831246C2CF75FC5C87E73A7EAEE3E73A7E689429BA8E9FEB6596F43EFC1CCC
          4E8E53FB47782A8B69B562EF4C688023E4663127BFB7FED9A73E3E4F9E431246
          82A71F9E7E78FAE1E987A71F1E1A658AFA6928E987E70D780E39F254D150EEFC
          CCF41C81ADB8D0D3237CD7EE5A123C679B8D84F0E0BF2C80E7888491AE769E1D
          A03965003F035D2D7503F819C00F81A4A201FC0CE0E73A7E8EDCD4CFC279BC24
          8C04CF203C83F00CC23308CF203C04928A06E11984E72D78BC3CC9C3A30E4FF0
          784B18093F43F819C2CF107E862ED8AC43F010482A1AC2CF107E0672D312BD5D
          E947C6B4493392234F779B6D028FADAAD8DD3C3E1246C2CF0DFCDC80E7067E6E
          E0E7067E6EC073033F37F0F30E7E7CDCE0C757C248F00CC3330CCF303CC3F00C
          C3330CCF303CC3F00CC1E3BB147868001A2ECDCD70178F9F84917A3B5B476826
          1AC1CF08BFE41FC1CF087E46F03302CF083C7FC28F9F8BFD7C409A91248C048F
          018F018F018F018F018F018F018F01CF303CFEF07CC07EFE9AF27CE1743C5481
          245E9CFEFB0C61A4777839B833F0E0EEDBFADAAAEF91316DD28C2461A437DAAA
          8319D7167CC5561EDCDD501C7CA13A3FB8B52C3BB891401223DB828B52A2F765
          9A82EE218C741B61A49D5CACBEE3502B6ABF3834B8183376AD59F1EFF613D87C
          78248C34130F238A820B5362F66598CEB89447C6B449339284915E6FAB91F15F
          A13DB68AD0571A4A423BAA0B42CF96E5843610488227149E03AEE479ADADE69E
          25C673AF8C699366240923E147C67F45E027023F11F889C04F047E22F013819F
          43F8B9D755FB073FC2E325CD4812468247C67F45C113054F143C51F044C11305
          4F143C5EB718CF7D32A64D9A91248C841F137E4CF831E1C7841F137E4CF831E1
          C7841F6FFCDCE7C2F5BA5FC6B4A96624C248F0C4C113074F1C3C71F0C4C11307
          4F1C3C71F01C83E77E77F3BCDA54E9491E3F69469230525F9B3591715289F024
          76359426E227113F89F849C44F62418AC9CF957EAEB659EF97316DD28C34150F
          0186C496728BE2618493E2493705BB6CBDE0F9B48C699366240923E147C67F25
          E327193FC9F024C3935C5F94990C4F323CFEF07C7AAEFB2726C0F7A1524BA6BA
          D9975FA0D552D74C00C7488D0A35C28E1DFE83FD7C0ACF67644C9B3423491809
          1E333C6678CCF098E131C36386C70C8F199E93F07C66AE3C91FE3E0FDB791C6F
          662F9C6B230094FCE770BFA387E4FCEE2E1EE5272F7B52A5B604924A73B3DE8D
          3975DCDB8127509A91248C849F74FCA4E3271D3FE9F849C74F3A7ED2F1938E9F
          C0F9F8119E929C8C493C72B35D5198F717DA7F8E8DF17C56C6B44933928491E0
          C982270B9E2C78B2E0C982270B9E2C78B2E00982E7B3735DAFBFED9F890F433A
          CFB719859969C30E7E3E2B63DAA41949C248AFB5D56631BEC9853C17273C0C91
          FD539C9DEEC8F33919D326CD48124682C7028FE5725395E5524399053F16FC58
          F063C18F053FC1F8F9DCFCFDCCCCD3DB563B230F810A4B7379AEE2614491BB78
          42A51949C248F8C9C34F1E7EF2F093074F1E3C79F0E4C193979F1A1BEA623F9F
          97316DD28C246124780AE12984A7109E42780AE12984A7109E4278C2E1F9BC0B
          D76B563C754559E33C69A61057F27C41C6B44933928491F0538C9F62FC14E3A7
          183FC5F82986A7183FC5F88984E70B2EF4233C31D28C24612478CAE02983A70C
          9E3278CAE02983A70C9E3278625CCD2363DAA419C9CEC3F826DA9AAA3DC5F345
          19D326CD481246BADA5E57014F053C15171BCA2BF053819F0AFC54E0A7023FB1
          F8F9A2ABD6EB4A5BDD4D799ACAF3DCC9F32519D326CD481246C24F357EAAF153
          8D9F6AFC54C3538D9F6AFC54E3271E3F5F72A11FE1499466240923C16385C70A
          8F151E2B010F2B3C5678AC8C4CB2C293788BF17C59C6B44933928491F0538F9F
          7AFCD4E3A71E3FF5F8A9AF2590849F7AFC24E3E7CB2E5CAFAFC8983669469230
          123C36786CF0D8E0B1C16383C7068F0D1E1B3CA9F07CC5953C32A64D9A91248C
          D4DB4E5B133CAF34D54CC993971A976A3685BA9A274D9A91248C044F33E3A49A
          E169C64F337E9AF1D38C9F66FC34C393E64A9E9EB6FAAFC8D8B899786CE5F9EE
          E4F9AA8C699366240923E147C66D311EADA6B5B3A1A2B5B5A6A4151E22BFD9AD
          F869C54F267EBEEAAAFD831FE1C99666240923C1D30E4F3B3CEDF0B4C3D30E4F
          3B3CEDF0B4C393BD54781809E40E9E07644C9B34234918093F1DF8E9C04F077E
          3AF0D3819F0EFC74C0D3811F0B7E1E70E17A3D2863DAA41949C248F074C2D309
          4F273C9DF074C2D3692590044F273C79F03CE82E9E2BED0D4B81A7409A91248C
          048F8CB7EAEA6AB676E1A70B3F5DF8E9C24F177EBA7253E30B5CE9E7D5F60659
          AF29792E34564EC9936A0A73D97AC1F33519D326CD481246C24F377EBAF1D30D
          4F377EBAF1D38D9F6EFC74E3A7089EAFB96AFFC0F37519D326CD48124682A707
          9E1E787AE0E9395B53DAD3585ED0034F0F3C3DF094C2F3F55B8CA75C9A91248C
          849F5EFCF4E2A7173F145397F6E2A7173FBDF8E9C54FB9A31FDE350DCEE67D93
          E3F33AE7E7638ECF3758AF6FC8983669469230123C7DF0F4C1D3074F1F3C7DF0
          F4C1D3074F1F3C95F07C43D66BDF8617F6067A1DF86302A3FF089718C7766D9B
          F6FDD75C78644C9B3423094F4F7BA3E2B92465914E3C8C949AC0736CD7CB3E19
          09A66119DF7E89E75CF2AC52C25296D42463FFC67513DEC7CD81E7DF644C9B34
          234918091E196F750D1E7E3F53790D3FD7F073AD8640123CD7F0539D620AFF37
          F173E2E01E3F025B2312B8710C4909531A63F6B6BEB0E2E7F6F783B3E5B9DCDE
          386F9E9307F71C2B2FC81D996ACC5E3DCEFC0FEF5F394F1EAB34234918093F32
          6EEB3A7EAE7734565DC7CF75FC5CC7CF75FC5CB7A42658ED7E028FECF3CE4D4B
          7ED73900A4C6DAD192E4BD77C733F3E0F9A68C699366240923C123E3ADFAE1E9
          87A71F9E7E78FAE1E987A71F9E3A78BE29EB253C594971C3CE2D5BC2539C9B65
          EC7A71F58F5DC1D3505E382D4F7672FCF0797E703BE1479C04CAD2E2628C439B
          D73F340F9E6FC99836D58C4418093F03F819C0CF007E065A6ACA06E019C0CF00
          7E06F0D3889F6FD9FD088FFC00D839D09611BF209E2669469230D2ABEDB64119
          4706CF203C83F00CC23308CF20239306E1695A0A3CD504A4DCC4F36D19D326CD
          481246C2CF107E862E36D70DE167083F43F8198267089E21FCB4E0E7DBAE5AAF
          EE76DB241EC65B299EF38DD59EE0F98E8C699366240923E147C66DDD80E7063C
          37F073033F37F073033F37F0D38A9FEFB8D08FF0B44B33928491E0198667189E
          617886E1198667189E61788673CC89EDC9A6088FF2D45714B993E7BB32A64D9A
          91248C74B9BD69043F23F819C1CF48B3B57C049E11FC8CE067043FE7F1F35D17
          AED7F7644C9B34234918091E031E031E031E031E031E031E031E039E4E78BE37
          171E1548228CB46C9A26078397A7DDBC8C7B52FE3E02490F31A2ED948C699366
          241546A21DE9727D49F0A59A82607E4624E3DA82EBF253832BB3E28369FF3995
          11735A9D0808233DC9C561F7FE0D2F3826D5ED612463E70BBF5D663F61CC9687
          40C9943CDDF5A5C1176B0A83DB2AD4F83877F2FC801152A7654C9B3423A93012
          ED48F084C2130A8F8C6B0BADCD3787E227143FA7F1F30317FA19E79166A425C0
          F3306E82654C9B3423A93012ED48F889C04F047E645C5B047E22F013413B4970
          7A4CD0C32EF4F343592B19D326CD482A8C443B123C51F044C123E3DAA2E08982
          270A9E50787EB814782AB212DCC1F323592B19D326CD482A8C443B127E4CF831
          E147C6B599F06382C7849F08FCFCC8557E084CFC58D64AC6B44933920A23D18E
          F44A43591C3C71F0C8B8B63878E2E08983270A1E7561EC8AE3CF543CAFD28E24
          3C9D56DA91DCCFF30823A44C32A64D9A9154188976247812E149A43D41C6B525
          E227113F89F831E1E71117FA19E79166A4A97818DFB4609EB8D327565716E61A
          179911DFC6AF7E1A696DA96626799E39C93005FACBB8ADDFC8FF46D6EB27B889
          93316DD28CA4C248B423E127193FC9F891716DC9F849C64F327EE2F0F393B9FA
          B1F338D7FD9E230D4CF1D29BA1C70E3F37C6F3535812644C9B3423A93012ED48
          F098E131C323E3DACCD6FC34737956A299769204787E3A571EC657ADABA11ADA
          9947022E7969C9BF8767B93B79C44F79BE654A9EECA478479E9FC95AC9983669
          46526124DA91F0938E9F74FCC8B8B674FCA4E3271D3FC96931677E36573FD3AF
          57938CDB7ACBC1CF2F604995316DD244A4C248B423C19375C13ADA46044F163C
          59F064C1930ACF2F168F677CFFA8F52270F30B7E96972A63DA5453938491E0E9
          6AA0ADC9333CBF84255DC6B44933920A23D18E048F051E0BED0932AECD821F0B
          7E2CF849C7CF2F5DE8679C479A919600CFA3B8C994316DD28CA4C248B423E127
          0F3F79F891716D79F8C9C34F1E7E32F1F3A80BFD3C064B8E8C69936624154692
          9F507317084F213C32AEAD109E42780AE1C981E7314FF3D4E4A717966525291E
          734CB02B797E859B5C19D326CD482A8C443B127E8AF1538C1F19D7560C4F313C
          C5B481E4C2F32B17FA798211490532164D9A9154188976A44B0DB4235947DB88
          E02983A70C9E32780AE079C2553C046E26F0A83052B384913CC6F36BDC14C998
          3669465261A4E6EA0A782AF0A3DA91F053819F0AFC54E0A7083FBF76A19F711E
          69465A023C4FE2A654C6B44933920A23355757E3A71A3FD5B44BC8B8B66AFC54
          E3A71A3FA5F851F7FE73B97E9EC5F58F3A7FB17F9E82A55CC6B44933920A2335
          575BE1B1C2638547C6B559E1B1C26385A71C9EA76E219EDFE0A64AC6B4493392
          0A233557D7E3A71E3FF5F891716DF5F8A9C74F3D7EAAF0A3AE755DB45ECB60A9
          91316DD28CA4C248CDD536786CF0D8E091716D36786CF0D8E0A981473DD77105
          0F8192658C90AA91316D769E579A258C54693B6F2D1DE7A92EC8B0956627DB68
          4BA9498D097125CF33B0D4C9983669465261A46609235536C3A3DA91F0D30C4F
          333CCDF0D4C1A35E24B9C8CF388F34232D019E6771D32063DAA4194985919A6B
          5AF1D38A9FD6E64A35AEAD153FADF869C54F037E9E75A19FE7606992316DD28C
          A4C248CD35EDF0B4C3D30E8F8C6B6B87A71D9E76789AE051F76E2E5AAFA5C6B3
          1C372D32A64D9A915418A9B9A6033F1DF8E9C08F8C6BE3774E191DF8E9C04F0B
          7ED4B1DD457E5630D2AA55C6B44933920A23311AAD53DA91ACA59DF0C8B8B6CE
          AA82CCCE92EC944E785AE159E12A1E020AD3F2B4D7D28EE47E9E95B8392763DA
          A4194985916847C24F173C5DF0C8B8B62EFC74E1A78B769273F8512FD65DB15E
          F819E79166A4D9F0A4C484BA92E779DC74C898366946526124DA91F0D38D1FA6
          1EAB716DDDF8E9C64F377E3AE079DE857E56C17251C6B44933920A23D18E044F
          0F3C3DF0C8B8B61E787AE0E981E7223CAB6E219ED58C48EA92316DD28CA4C248
          B423E1A7173FBDF891716DBDF8E9C54F2F7EBAF0B3DA857ED6C07259C6B44933
          920A23B558FBE0E983A70F1E19D7D6074F1F3C7DF05C86678DAB7808DC4CE2B9
          445B93F0B4D5D28E34C653599035250FC1A32B41DE0765C496717CEF0E83D0D4
          94EFE3667B3D0FCF5ADCF4C898366946526124DA91F8FDC33578AEC123E3DAAE
          C1730D3FD7F0D3839FB5E2E7C8960D4F87F879FF4E5AA4E4B9A4046FE4076F55
          2585862F6C5B563CA3CE2BF2990F8F3423CDC4539C9D3A81E7D8AEADCB1915F7
          8EB4703B07A4F2D2538D035B5F7A791E3CEB707355C6B44933920A23D18E849F
          EBF8B98E1F19D7761D3FD7E1B94EBBC455FCAC93FF8EF0E4A6A50C3A3724C933
          5DF9D16B74D029FF79F0BC287B59C6B44933920A23D18E044F3F3CFDF0C8B8B6
          7E78FAE1E987A70F9E17ED3CE9F1316FCB8F489D9F2BB734D61927BD0E1E5E6C
          1E5B65B18C6B9BC0931C1336CE931819F2968C6974E6A161CBD8BF69FDFA79F0
          AC97BD2C63DAA4194985915A6A07F033809F0178645CDB007E06F033809F6BF0
          A8FF8EAC176311DF6C6D6A9CC493A346476E52C7A939EEE7971869755DC6B449
          33920A23B5D40EC23308CF203C321E6D109E417806E1B90ECF4B8E3C6DCD9379
          D8E7F3E22170332D0F8F5E3CC1B31137FD32A64D9A9154188976A48EC69A2178
          86F023E3DA86F033849F21FCF4E367A30BFD8CF34833920A234DC15351903D54
          946D7607CF26DC0CC898366946526124DA91F073033F37F023E3DA6EC073039E
          1BF819C0CF2617FAD92C7B59C6B44933920A23D18E04CF303CC3F0C8B8B66178
          86E11986673029267CB3A7781A191F27E3C8DCC8B38591684332A64D9A915418
          A9A56E043F23F8198147C6B58DC033829F11FC0CE1678B0BFD6C83E5868C6993
          662415466AA933E031E031E091716D063C063C063C37E0D936171E7540248CB4
          21940B9390A3078C60AFFDF6312E1246320823B59D3AB04B850CAE36573E4A08
          29E8B5962A35A64D9A91248CD4652D64545B6EF0D9D2ACE0C60273708D2529B8
          CC6C0ACE4B080F328707A8172A84917E4418A9CDA136D3F1E2C7D8B17AF906FB
          017A213C1246BA642D1AE76928481BE7C94D88084A0D3FE5321EFA431F232411
          72B565744C9B34234918091E46B5E585E227149E50FC84E227149E1078D40B15
          57F8990B4FA939D61D3C8FE3270C3F6A4C9B34234918093F8C6ACB8B6829CD8E
          C04F047E22E089C04F187E1E77A19F27E08984478D699366240923C1C3A8B6BC
          2878A2E08982270A9E287822E1512F785CB45EB3E2611C90BB789EC44F347ED4
          983669469230127E18D59667C28F093F26784CF831E1271A3FEA05868BFC3C05
          4F2C3C6A4CDB683352591C3C71F0C4C113074F1C3C71F0C4C1130B8F7A81E10A
          9E2B2DD5E33C32A66DB4198970546DF1384F7D61BAAB79DAEDC767789E262411
          CF4F87D4583469469230123C89F849C44F223C32AE2D113F899684C878FC3C3D
          573FB181FE3B69B231CEB53411006A34CE7203D25C6F5521A0A4B03397838EEC
          7B44FE9DF3E149090F9C334F5CD0895DD5D4D93ADF88C88D5A667C4C2FE39B54
          88079E65F849C28F1AD326CD481246C24F725B557E327E92F123E3DA92F1938C
          9F2478D40B8CB9EC9FC893C7F6355A2B27F15C38D76A64269A5E77E079069E54
          78D4983669469230123C6678CCCD65396678CC8C9731C3638627151EF502632E
          3CE2A7B2306F128F04A4D24C91AFCD87A7242D6E413C53AD97DC2C39ADD773F8
          49C38F1AD326CD481246C24F3A7ED2F1938E9F74FCA4C3938E9F34FCA8170673
          F533CBFDB31C9E4C78D4983669469230123C59F064C193058F8C47CB82270B9E
          4C78D4831457F0F4B4D42C27949079E5ECE898B6D1A6A6F2ACCEDA124FF1AC80
          271B1E35164D9A91248C048F053F16FC58EA0A33645C9B053F16FC64E347BD30
          70919F39F324479C7625CF4AFCE4E2478D699366240923E127AFB5AA200F3F79
          F891716D79F8C9CB498CCA85473DA077919F55F0E4C3A3C6B44933928491E029
          84A7B0A9CC52088F8C6B2B84A7109E7C78D403714FF314A7C5BB8367357E8AF0
          A3C6B44933928491F0538C9F62FC14E347C6B515C3538C9F22FCA8074D2EF2B3
          069E1278D498B6D166A48A3278CAE02983A70C9E3219B7050F5D885125F0A807
          E2AEE2219450D273B6B64CC6B48D362355945DA82DF508CFAB2DD6B5F094C3A3
          C6A24933928491E0A9C04F057E2AF023E3D12AF053819F72FCA807E2AEF0331B
          9EDAC24C77F2ACC30F3FBDAF5563DAA41949C248F8A9C64F357EAAE191716DD5
          F8A1E438AA123FEA81B88BFCAC87A7061E35A64D9A91248C048FF56C55A1D556
          966B8547C6B559E1610A7C540D3CEA81EF2DC2B3013FB5F85163DAA41949C248
          F8A9C74F3D7EEAF1535F914B89665A427D7662746D5244907A8EE2223F1BE169
          80478D699366240923C16383C7068F0D1E1B3C36786CF034C0A31EF8BA8367B4
          19A9D2633C975B6A371192B0BD7A76744CDB6833526573476D59337E9AF1D36C
          2DCC6AC64F337E9AF163C38F7AE0EB0A3F4B90670B7E5AF0A3C6B44933928491
          F0D38A9F56FC1089CE6AC54F2B7E5AF1D3821FF580D5457EB64A280A1E35A64D
          9A91248C044F7B4B55513B3CEDF0B4C3D30E4F3B3CADF06CF5144F23259A6EE6
          D9869F73F85163DAA41949C248F8E9C04F073C1DF074E0A7A3302DB1033FE7F0
          A31E40BB68BDB6C373011E35A64D3523114682A7139E4E783AE1E984A7139E4E
          782EC0B3DD553CDD67EB66C5539E9BA678B212632E24469C7125CF0E4212172F
          8F8D6953CD488491CED79577E1A70B3F3CAACFEA82A70B9E2E782EC2B3C3857E
          6ECA535398ED4E9E5DF879053F6A4C9B34234918093FDDF8E9C64F373CDDF8E9
          C64F377E5EC1CF2E17FAD90DCF6578D4983669469230123C72BBD1D35096DF03
          4F0F3C3DF0F4C073199EDDB710CF5EFC5CC18F1AD326CD481246C20FAF368A7B
          F1D38B9F5EFCF4E2A7173F57F0B3D7857EF6C173151E35A64D9A9124FC030FA3
          DA8AFBE0E983A70F9E3E78FAE0B90ACF3E77F14833920A47D5554CE029CB4D1F
          E74988087619CF2B67EB0F100278BDBB75744CDB683352F53578AEE1E71A7EAE
          E1E71A3CD70AD293AEC9B350780ED8FDEC7971D5B923DB361A0187F71AFEFB77
          195E2F6F94717F525630E1FDE06C9FF72E94E7E88E2D5D059966E35C6BB37189
          D16812906AE0C7AEB1C181C6A6DF3EADCE73F29903CF410945E1478D69936624
          0923E1E73A7EAEE3E73A7EAEE3E73A7EAEE3E71A7E0ECA7FE3E0A6758FA4C644
          BCD15467351C5B9B24A05444BBBBD7CEADC7E6C173089E37E15163DAA41949C2
          48F0F4C3D30F4F7F75614E3F3CFDF0F4C3F3263C87E4BFE3B56DE34F696CEA17
          2F53FDF0362A28206A31799AAA4AFAEBCB0A66E4498A0CBB3665408A5180BEFB
          F79C9807CF110969E1478D699366240923E187516D2503F00CE067003F03F819
          C04F3F7E8ED8FD988203FBA4E1CBD98F84EC0E6D7D491DA7E6B87FBCE0791B1E
          35A64D9A91248C040FA3D14A06E1198467109E417806E1791B1E2F3B4F6C48E0
          D5B3B6FA493C12B063AFABF3DC62F0B4D555CE8A273EE474EFD9C6C93CF919E6
          F9F2784B480B3F6A2CDA683352CD103C8C6A2B19C2CF107E86F033849F21FC0C
          E2C7DBEE67B179BACE367813DA187CA575746CDC743C32FE2B3F3DD91D3C3EF0
          FC111E35A64D9A91248C849F1BF8B9819F1BF8B901CF0D786EE0E78FF8F171A1
          1F5F0945C1A3C6B44933928491E0198667189E6178643CDA303CC31949B137E2
          23437C3DC163AB2E553C55451677F2F8E1E75DFCA8316DD28C246124FC8CC033
          02CF083C32AE6D043F23F879173F7E2EF4E30FCF7BF0A8316DD28C246124780C
          780C780C78645C9B018F01CF7BF0A8A0AC9C2F66F3FD520720C2481BA6092319
          BCEC6E0BD8BF530592BA1B4A1FBD622B0DEA692A0B7EB5A1548D699366247B18
          C95698165C4B18A922338E716D91C1D9A6E0A0C4107F1500228CF423C2486D63
          33859DC348C6F655CF8D0792E6CAF36A13C1A88632C523CD48CE3CE599F18A27
          CB1412941072C2653CBC9C7CACC75616024F283C6A4C9B3423D9C3488D85E9A1
          564B72283CA1F084C213028F0A24B9C2CF5C780A53A2DCC1F3387EC2F013811F
          35A64D9A91EC6124FC44E027023F8C6B8B8AC04F187E5420C9457E9E8027129E
          2878D498366946B28791E08982270A1EC6B54545C113098F0A24DD223C4FE227
          1A3F26FCA8316DD28C640F23E1C7841F137E18D71665C24F347E5420C9457E9E
          8227169EB8EEC67235164D9A91EC612478E2E0898387716D5171F0C4C2A30249
          0BE561D496111DE06784FB7975061CD8A55A97F87E3DF5AAAD3CF67253C5388F
          342339F39465268CF3C4879C9C334FF489632F335ACB68A9AF3524B4719E39CD
          173BDA0D99019E1117DD4B0048056EBA1ACB9F86271E9E44FCA8316DD28C640F
          23E127113F89F024E227113FF1F03C3D573FB1A7FD77554D15006AB61919F1D1
          5767CB23A3DA1C79324DA1F3E2615DF65AF9658DF385BFDC40A6C745F739F02C
          C34F127E92F1A3C6B44933923D8C044F724D6E4A327E92F1930C4F127E542069
          2EFB47FC30EA6B8A805493911E1BE9E8E7197852E131C3A3C6B44933923D8C04
          8F191E333C6678CCF0A4C2A3024973E5996ABD642F39ED1F8FF2B4CBFE99B89F
          9FC34F1A7ED2F1A3C6B44933923D8C849F74FCA4E387716DD1E9F849C38F0A24
          2D869F297896C393094F163C6A4C9B3411D9C348F064C193050FE3DAA2B3E0C9
          844705925CC573D95691C94F3BB35E69ACF038CFA5C68A15F064C36381478D69
          9366247B18093F16FC58F0C378B4684B466C58367E5400C8157E66C3539D9BEA
          4E9E95F8C9C54F1E7ED498366946B2879164541B3C79A5998979F8C9C34F2E7E
          5420C9457E56C1930F4F213C6A4C9B3423D9C34832AA0D9E42780AE12984271F
          1E15485A2A3C712101AEE4598D9F22FC14E3478D699366247B18093FC5F829C6
          4F317E8AF153048F0A24B9C8CF1A584AF894C1A3C6A24933923D8C044F193C65
          F094E5A7C494C153028F0A24B992E71582515D8D954B81672D6ECAE1A980478D
          699366247B18093F15F8A9C04F057E2AF0538E1F154872859F8B8D954B8D671D
          7E2AF1538D1F35A64D9A91EC6124FC54E3A7BA847624FC54E3A7123F2A90E422
          3FEBE1A981C70A8F1AD326CD48F630928C6AABCA355BE1B1C2634D8F0DAF8147
          05926E119E0DB8A9E5538F1F35A64D9A91EC6124FCD4E3A71E3FF5F8A9C74F2D
          7ED4731417F9D9084B031F1B3C6A2C9A3423D9C348F0D8E0A10431C9068F0D9E
          06785420C9D53C971AAB148F34234DC593976A7207CF26E5A6A9BA191E35A64D
          9A91EC6124FC34E3A7193F8C6B3335E3C7161B724A05925CE46749F1F0B2740B
          7E5AF0D38A1F35A64D9A91EC6124FCB4E2A7153F8C6B33B5E2A7053F2A90E40A
          3FF06C85A7159E7678D498366946B28791E06987A71D1EC6A399DAE169854705
          92DCCD2361A4DAA2ECF6CADC3477F26CC3CF39FC74E0478D699366247B18099E
          0E783A8AB39219D766EAC0CF39FCA817B52EF2B31D960B7C3AE15163DAA419C9
          1E4682A7139E4E7818D766EA4C8B8DB8008F0A00B992E7122D4D176DD58A479A
          91A6E2C94D8D7507CF0EDC5C84A70B1E35A64D9A91EC6124FC74E1A70B3F8C6B
          8BEDC2CF45FCA840928BFC4CE091316D37E3318506BA9267177E5EC14F377ED4
          98366946B28791F0D38D9F6EFC30AE2DB61B3FAFC0A35ED4BAC8CF6E782EC3D3
          038F1AD326CD48F630123C3D15B9E93DF030AE2DB6079ECBF0A840922B7878B9
          BDD478F65E6AAAB9829F5EFCA8316DD28C640F23598B727AF1D35B9495C2B8B6
          D85EFC5CC18F7AD1EF223FFBE0B90A4F1F3C6A2C9A3423D9C348F0F4C1D3070F
          E3D162FBE0B90A8F0A00B98B479A913CC873003FAF5F6CB65EEBB4D5A8316DD2
          8C640F23E1E71A7EAEE1E79A2535EE9A3936F275FCA840928BFC2C88877795ED
          FB37AC317C77BF6CF8D1D2E4B767BB8CFE92779513DE57CEF83CDCE17926DFAF
          83F8B9869FEBF85163DAA419C91E46C2CF75FC5CC70FE3DAE2AEE3E71A7E5420
          49F979696D67764A8221A192F1805475A511E4739880D4B29DF2F7C9670E3C87
          E079139E7E78D498366946B28791E0E987A71F1EC6B5C5F5C3F3263C2A908497
          FF39ED7DE88AB5BCC4706C6D923F67D49D716CEFCEF100D06C7978B93D258F3D
          8C3413CFE12D1B7E42D3CF5BD305A422034F8E07A4E6C073043FFDF819C08F1A
          D326CD48F630123C03E5B91903F8615C5BDC007EFAF1A3024947B6BEF4533A0C
          7E375540CA56576378EDDAA6821573592FFC785D6CB2BE0DCF203C6A4C9B3423
          D9C3483545964178060BB35219D7163708CFDB31A1A755204978E2C382DF9077
          22CEEF494A99564010509D5716CA23CD48B3E589090A78ED2CEFB09C7972D353
          686CDA3C1E909AC37A79E367B0B3B976E882CDAAC6B44933923D8C849F21FC0C
          E1672827357E083F83F8518124F1131712D8DB3245402A2F83C0D622F0C89836
          0FF3F8E0E78FF8B9811F35A64D9A91EC6124FCDCC0CF0DFCDCC0CF0DFCFC113F
          2A90E4223FBEF0DC8067181E35A64D9A91EC61247886E1198667189EE1D4B8A8
          1BF0A8EF8D3B79EC6124191D579697E9369EF336AB5F6753EDBBF819C18F1AD3
          26CD48F630928C6A8367043F23F819C1CFBBF851812457F881C71F9EF7E031E0
          5163DAA419C91E4692516DF018F018F018F0BC078F0A24CD96471D8008232D9B
          A21949C2480661A4EE93FB76A89041674DC143AFD4169DE2137CA9B620F84275
          BE1AD326CD481246AACB4D0EAECA4C082E324707E772F44B8B3875CA14E8A346
          B611467A92137CB7249E77AF5D29336AEDC96775827FF9F967C747B6CD87E762
          6DA1E291316DCE3C85F4865BE2C383CD1181AEE6F941576DF1693EA1F084D265
          ACC6B449339284916A735342F1130A4F283CA1F09CC68F1AD9E6223F37E5A9CC
          4C741BCF859AC2877113CC27023F11F85163DAA41949C248F8898027023F11F8
          89C04F704CE03135B2CD157EE0F9A1AC159F2878A2E05163DAA41949C248F044
          C113054F143C51F084C2A346B6FD2FE751A119FCFC48D68A8F093F26FCA8316D
          D28C246124FC98F063C28F093F26FC44E047851717E2E7CC917D465CD049DA5B
          228C8490C0D74F1FD9A76ADBE1F9B1AC151F5A880AE3E05163D1A41949C248F0
          C4C113074F1C3C71A911A7A3E051E1A1B9F084FB7AFD362336CAA8AF2A570D40
          32635B2E9ACE9D55239CDE3C73F4A00A1938F274D6164DE2B1E6A52E0A4F4C80
          DFEA8A82C923AEE4971B3929F1FD0E3C8F5CAA2B31F1498427113F6A4C9B3423
          A930525E6A227E120BCCA6C49CF88844FC98F0F3C85CFD084F55D1E4C08DCC93
          CE4C308DF374588B26F09CAF291CE7516124782AB2921685A72C2F678A5F6A34
          194C127CCBEE079E9FE0268E4F327E92E15163DAA41949C248F030AA2D29193F
          C9F849C64F1C7ED4C8B6B9EC9F69D7CB69FFC0F3535812F898E131C3A3C6B449
          33928491E031C36386C70C8F199E047854F8EC16E1F999AC159F74FCA4E3478D
          699366240923E1271D3FE9F849C74F3A7E92F1A346B6B9C8CF2F6049E5937581
          562478D4983669469230123C59F064C193054F163CA9F0A8916D4B85273AD0D7
          953CBFBC58579ACEC7821F0B7ED4983669469230127E2CF8B1E0C7821F0B7ED2
          E15123DB5CE467024F3B77388E3C3579E6093C2991412EE5396F2D7E1437997C
          F2F093078F1AD326CD48124682270F3F79F9E6D8BCEC84C83C7832F1A302F4AE
          F003CF63B0E4F02984A7101E35A64D9A91248C044F213C85F014C253084F0E3C
          2A40EF299EF2AC6477F2FC0A37B97C8AF1538C1F35A64D9A91248C849F62788A
          F1538C9F62FCE4E2478D6C73919F276029E053D6412B123C6A4C9B3423491809
          9E3278CAE02983A70C9E02785480FE16E1F9356E8AF854E0A7023F6A4C9B3423
          A930525E5A057E2AF053819F0AFC144505FAA9916D2EF2F3EBCEBAB2223E4B85
          E749584AF954E3A71A3F6A4C9B3423A930525E5A357EAAF1538D9F6AFC94E247
          DDFBBBC24FBBB5E42958CAF958E1B1B6D514AB316DD28C2461247818D5966285
          C70A8F159E7278D4BDC92DC2F31BDC54F1A9C74F3D7ED4983669469230127E18
          D596528F9F7A2678D727479EA9C28F1AD9E6223FCB60A9E16383C7068F1A8B26
          CD4812468287516D2936786CF0D8E0A981473DD77135CF795A9AA6E3C933C7B9
          8BE7990B75E5757C9AE16986478D699366240923E1A7193FCDF034E3A7193F75
          F8513F3070919FA5C6F32C6E1AF8B4E2A7B5B5A6448D699366241546CA4B6FC5
          4F2B7E5AF1D38A9F06FCA87B7F57F869B3963E074B139F7678DAE15163DAA419
          498591F2D2DBE16987A71D9E76789AE0513F7870278F3D8CE4CC931419EC6A9E
          E5B869E1D3819F0EFCA8316DD28C246124783AF0D3819F0EFC74C0D31219785C
          3D1B71919F15B277F874C2D3098F1A8B26CD48124682A7B3342BB5139E4E783A
          E1698547FDE0C1D53CED756533F2642644BB8367256ECEF1E982A70B3F6A4C9B
          34234918093F5DF8E9CA35C777C1D3859F73F8513F7870919F951DF515E7F82C
          159EE761E9E0D38D9F6EFCA8316DD28C246124FC74E3A71B3FDDF8E9C64F077E
          D4C83617F95905CB453E3DF0F49CAD295563DAA41949C2489579193DF0F4C0D3
          034F0F3C17E1513FC070050F13A0971ACF6AD93B7C7AF1433175A91AD326CD48
          1246C24F2F7E7AF1D38B9F5EFC74459CF6573F0871919F35B05CE6430B51591F
          3C6A2C9A34234918099E3E78FAE0E983A70F9ECBF0A81F84B89AA78D96A6A978
          4AB2CDE33C8CB47235CF5AD9CB7CAEC1730D1E35A64D9A91248C841F46B599AF
          E1E75A4642CC35787AF0A37E10E2223F6BCFD757F6F0592A3CEB60B9CAE73A7E
          AEE3478D699366241546CACBBC8E9FEB1673C275FC5CC7CF55FCA81F8438FA61
          DC97717CDF4ED524E5B36B9BB17DF5F209EF4F67FBFC99EFFB8BB0F4F1E987A7
          1F1E35A64D9A9154F8272FB31F9E7E78FAE1E987A70F9E17ED3CBBD7ADEA2174
          C384847AD51E259FFAEA0AC6C79D34362E7F7A7C24D94278644C9B3423CD86C7
          FFE09ED765E49F63404A0241954C6E08F439AC8239F2992DCF596BF97AD93B7C
          06F033D05253A6C6B44933928491F033809F01FC0CE067003FD7F0A37E307378
          CBFA65E97131039DFC70DC3970D3C60F6F43FC8FA9E0C01C795E92BDC367109E
          4178D4583469469230123C83F00CC23308CF203CD7E15123DB08D42CA7798891
          7FA3EFB21C3F8DD62A63DFE6F52A683F579EF6FAAAEB7C0699483F679E8CA4B8
          77A60A6CE5D1907478EB4B73DE3FACD74658FAF90CC133841F35A64D9A91248C
          849F21FC0CE167083F43F8E9C78FFAC18CF849080F7E4B7EECEFEC272725D165
          3CC5D96933F09C794B4A0726F130F26F9E7E36E16680CF0DFCDCC08F1AD326CD
          481246C2CF0D786EE0E7067E6EE06720FCF409F58319F193141132ED08C279F2
          6C96BDC367189E6178D4983669229230123CC3F00CE7981387D3134DC30951A1
          83F0A8916DEEE291316DAAA9893052797E965B795A6A2BB6C85EE633829F9166
          6BB91AD326CD4812468267043F23F819C1CF087E86F0A37EC0E30A3FF06C93BD
          C3C780C780478D699366240923C163C063C063C063C073031EF50395D9F2A803
          1061A47F9710401027B8B1316DF630924118E99D137BB7EFF4DFF3F26DAD65D9
          F7D03EB44F1A88CE57E604F3D7C1CDC5196A4C9B34234918A938354646B50567
          46070527041FDF17E67FF81E4EA6B71146DA4918E99D69C248C6B695CFFCBBFD
          80381F9EF64ACB388F8C6973E46174537046F499E0F8607F97F29C2DCBB98736
          9203D288044F287F1DDA549CA9C6B44933928491F023A3D142E10985E7802BFD
          DC8C47C2486EE6B9173787A411093F11F045E0478D699366240923C123A3DA22
          F013819F43F8B9D755FB87FFBEF0784923123C51FC75143C6A4C9B3423491809
          1E19D516054F143C5EA1FE47169587F6963F041DD97F40BE5FB3E1294A352D3A
          4F08E31ACDD1611C4CD20D4B4AC29F23FC7DBCC778EEC38DB73422E1C7049F09
          3F6A4C9B34234918091E19D566C28F093FDEF8B96F1EEBF51F09C1A70C192BE5
          58E12823AE0A33CDC30E3CF7C3724C1A88DA2A73E3E08983478D699366240923
          C123A3DAE2E08983E7183CF7CF9527DCEFE843E2A2FB52E7848B01996D5D9C93
          F9EECD78644C9B4B78BA2E4EE091A0547E46AAB31F3F6944C24F227E12F1A3C6
          B44913918491F023A3DA12F193881FBFF9FA912A5BC7D9DA72D1E4BC5E2DE516
          59AF711EFE3A919F4F8FF34818C9CE931E1DBC209EA9D66B0A9E4FD3D6E22F8D
          48F8498627191E35A64D9A91248C048F8C6A4B8627193FFEF8F9F462ED9F2978
          3E03CB49694482C70C8FFCBC5C8D699366240923C123A3D1CCF098E3824F9C84
          E733B7184FA03422E1271D3FE9F85163DAA41949C24885A9B132AA2D1D3FE9F8
          0974B19FCFC212248D48F064C193058F1AD326CD481246824746B565C193054F
          103C9F75E17A8DF348306A09F07C0E37C1D288048F051E0B7ED4983669469230
          127E64549B053F16FC04E3E773AEF2D35C9E3B8187BF1EE79166244FF0D04612
          2A8D48F8C98327AFB1245B8D699366240923E14746B5E5E1270F3FA121FE5EAE
          F4F37958C2A511099E42780AE15163DAA41949C248F0C868B442780AE10987E7
          F32E5CAF1979248C64E7498B09298C0D3EE96A9E2FE026521A91F0538C9F62FC
          A8316DD28C2461247864545B313CC5F05050EBF50517FA119E18694482A70C9E
          3278D4983669469230123C32AAAD0C9E327862DCC523C1A8A9780ACC71EEE4F9
          226E62A511498248F054E0478D699366240923C123A3DA2AF053819F58FC7CD1
          55EBD5549E378187BF563C32A6CD433C5FC24DBC3422E1A71A9EEA86921C35A6
          4D9A91248C841F19D5568D9F6AFCC4E3E74B2EF4F325DA5112A511091E2B3C56
          78D4983669469230123C32AACD0A8F151E0A9797164FB0FF5157FAF9326E92A5
          11093FF5F8A9C78F1AD326CD481246C28F8C6AABC74F3D7E92E1F9B20BD7EB2B
          B0A44A03113C36786CF0A8316DD28C2461247864549B0D1E1B3CA9F07C6529F0
          F01B3077F1A44923920491F0D38C1F35A64D9A91248C946F8E97516DCDF0349B
          8203D25CE9C7569E2FEB35CEC35F7B9AE7ABF0644A23127E5AE169AD2FB1A831
          6DD28C246124FCC8A8B656FCB4E227133F5F75D5FEE1BFFF55DA51B2A511099E
          76FEBA1D1E35164D9A91248C048F8C6A6B87A71D9E6C4FF148339207781EC08D
          451A91F0D3819F0EFCA8316DD28C246124FCC8A8B60EFC74E0C7829F075CB85E
          0FC292270D44F074C2D3098F1AD326CD481246824746B575C2D3094FDE197FEF
          07DDC1C3C4E3697918DDE44E9E02694482A70B3F5DF85163DAA41949C248F891
          516D5DF8E9C24F811BFC4CE291316D769E3C7382E2498D09EB8A0939E56A9EAF
          E1A6481A91F0D38D9F6EFCA8316DD28C2461247864545B373CDDF014E1E76BAE
          DA3F8DE5055FA78DA4541A91E0E9E1AF7BEA4A72D5983669469230123C32AAAD
          079E1E784AE1F9FA2DC6532E8D48F8E9C54F2F7ED4983669469230127E64545B
          2F7E7AF153EE623FDF80A5521A88E0E983A70F1E35A64D9A91248C048F8C6AEB
          83A70F9E4A78BEE1C2F51AE769AA2A9AC4236124679E207F1F57F2FC1B6EAAA5
          11099E6BF8B9861F35A64D9A91248C048F8C6ABB9612137E0D3FD5F0FC9B0BFD
          2C451EAB3422E1E73A7EAEE3478D699366240923E14746B55DC7CF75FC585DEC
          E79BB0D44923123CFDF0F4C3A3C6A24933928491E091516DFDF0F4C35307CF37
          9DD78BF08D71E2C06E23C4EFA8E14B93D4D695CF4C789F3BDDFB14E7E7E10DE5
          85DFA41DA54E1A918487BFEEAF2DC99B35CFCE352B07CDA648A399116D128EEA
          683F6B54971619114CB6D8B4F2D99FDBDF2FCF81E75BB0344A23123C03F00CC0
          A3C6B4493392849172CD8932AA6D003F03F869C4CFB7EC7E4E1EDAFBA7F2C25C
          E3155AA3ECA1922EDED994E6590CDF037B55505A3E73E4699206227806E11984
          478D699366240923C123A3DA06E1198C0E096CB2F3EC7971F5FEB4B8E861F9D1
          B863C045DEE134D4541947766E533F8C70178FF7AEADBE55A585238EE307854B
          78A4B169DB0B2BE6B35EDFC64D8B3422E167083F43F85163DAA41949C248F891
          D16843F819C24F0B7EBE2DEBC5BEF5E3BF3BE21CD8121E5EB91A7B37BCA08A55
          E6E8679CC756553C8947C248D3F11CDFB7C3272735F1DD8B4E2311C5978400E7
          C9F31DDCB44A23123C37F073033F6A4C9B34234918091E19D576033F37F0D38A
          9FEF889F13077679A7279886CFB79D9DB07F1681A75D1A91E0198667181E35A6
          4D9A91248C048F8C6A1B4E36450CC3D3EEC8939918BBA83CF51545DFA1EDA75D
          1A918487BF1EB696E6AB316DD28CE4019EEFC2725E1A91E0198167041E35A64D
          9A91248C841F19D536829F11FC9CC7CF77EDEBE5023FDF83A5531A91E031E031
          E05163DAA41949C248F0C8A836031E039E4E78BE37171EF505238C742761A43F
          4F1146320823198413D28EEFDE76776D5ECA6D4D45E98F12420AE48FC18D7CEA
          F253836BEC63DA6846B2879192424F04469DF27E945FE84A18E96EC248693384
          91FEBC75C5B23BED5FF8D9F2F08BFBDB6C45198A873F4EE091316DD28C640F23
          25869E0C8C3CE5F328BF707235CF63840082E009A51520B436DF1C8A1F35A64D
          9A91EC61247882E079EC56E0610F65F8EFD97ECFD87A3D8E9F10FC44E027023F
          11F8191DD34633923D8C849F10FC3CBED87E4CA74F1851277D8B4FECDB792F3C
          B7C3F1043C61FC310A9E2878A2E0191DD34633923D8C044F183C4FC073FB42F7
          B30480686E510D3CB5545EC6059F2A9F89A7DA3E366E1179183D369C4BFD6753
          6DCDDF2E96B870129ED83301950E3C4FE227123F26FC98F0638267744C1BCD48
          F630127E22F1F3E47CFCD08876972539FE2F7201E978B124497771141B74B2C2
          81E72978A2E18983270E9E387846C7B4D18C640F23C1130DCF538BC923275F49
          E03BAE576371A6E2E18F1378D49836079E84D08005F170721991316D13FD744C
          DA3F703C0D4F2C7F4C941622FC24E2478D699366247B18099E58FC3CBD203F4E
          1727B25E8EFB99C686DB858750C28C3C12465A149E29F6CF143CCBE089872B99
          568064FC24E3478D699366247B18099E78FC2C5B909FD9F13C23332EE031C363
          B6E6A799AB2CA363DAA419C91E468227099E676E419EE7F093829F74FCA4E327
          1D3FA363DA6846B28791F0931271EAD8736EF0B31C9E3478B2E0C982270B9ED1
          316D3423D9C348F05010796CF952E3E11796733E7FCD747C76FC7ED100707B43
          71D60AFC64F0470B7E2CF8B1E067742C1ACD48F630127E32F0B3E216E4594928
          211B3FAA15093F79F85163DAA419C91E46C24F367E56BAC1CF2A782CF0A85624
          780AE15163DAA419C91E4682C702CF2A77F2D0525058939F3E8947C248F1A1A7
          DCC5B31A3FF9F82986A7189E62FCA8316DD28C640F23C1938F9FD52EF673071C
          6B2434C61FCBE02983A7ACD23236A68D66247B18099E4278D6C073C75CAF57E7
          F07D9F134FF8295F77F0ACC54F097E2AF053819F0AFC8C8E69A319C91E46C24F
          093C6B5DEDA7BE385BF1F0C759F1F08B3D97AE171CEB084994F1C76AFC54E3A7
          1A3FA363DA6846B28791F053869F75AEE421677C071CEBE1A9E48FAA15091E2B
          3CA363DA6846B28791E0A98467FD52E1890B0D7417CF06098DE1A7BE960F7EEA
          2BEC63DA6846B28791E0A9C6CF0637F8D9084F2D3C36786CF03084746C4C1BCD
          48F630123CB5F06C74374F7541C6388F3423098F8491DCC8B3494263F869C64F
          333CCDF8191DD34633923D8C044F7DD829BF4D6EF0B3A478EA8A73B61002B0F1
          C756FCB4E2A7153FA363DA6846B28791F063C3CF1657FB81632B3CCDFCB11D9E
          7678DAE1191D8B4633923D8C044F333C5B6F259ECAFC8C3BF0B24D4263FCB103
          3F942E6774E067744C1BCD48F630127E5AF1B3CD957EC678B64B680C9E4E2B9F
          AA82CC4E7846C7B4D18C640F23C1D30ECF764FF094DBC7C64953D35818C98D3C
          3B2434869F2EFC74E1A70B9ED1316D0E6124783AF0B3C30D7E6ECA2361A4D8B0
          D38A875F54BBECFA47F60FC5BDBB24C4C61FBBF1D38D9F6EFC8C8E69A319C91E
          4682A7139E5D2EE6793F1CBB090150FA6EE981A7079E1E7846C7B4D18C640F23
          C1D305CF6E78DEEFAAEB79FC2C299E8AFC4CE1D98B9F6EFED88B9F5EFCF4E267
          744C1BCD48F630127EBAF1B3D7957EC678F649680C9E3E78FAE0E98367744C1B
          CD48F630123C3DF0EC73374F6541565F997D6C9C134FE8A9E3EEE03920A131FC
          5CABE103CF3578D498366946B28791F0D30BCF0137F8596A3C0725C4869FEBF8
          B98E9FEBF8191DD3E61046C24F1F7E0EBADA8FB538F7103C6FF0C77E78FAE1E9
          8767744C9B4318099E37E039E40E1E4209B3E6093B7D72D2F1F0C0C6B52A2015
          4353533021A9CDBF5D36ECF87E79B6CF37E4FB8E9723F05CE78F03F819C0CF00
          7E46C7B4D18C640F23E1E73A7E8E38F2EC7C61859114196A34D6561B12043A77
          B6D9A82A2934FCF6EDFACBDA677F739FFD7DF71C79BCE0790B9E417806E11984
          67744C1BCD48F63092292CE82D78BCEC3C4C244A0F38BC8FF051E578404AC236
          C2559095FEDECE4D1BBE3C571E1A51C4CF380F2D29B3E6D9BB614D7E4662ACE1
          DC90244C65F996BFEED9BAE98179F2784B680CAE217886F033841F35A64D9A91
          EC6124FC0CE0C7DBEE27C0EBA0B5A9DE3AE947F61220CB4A49788F3DF4E1C5E0
          A928C81E2A1D1B1BE7CC1312E8AF78F66D78E19E8853FE36191F37F947FF6D34
          90058C6C5FB5FCAE79F2F8E0E71DFCDCC0CF0D786EC0A3C6B44933923D8C849F
          77E0F1111EBE4FF786FA1FAB577BC7A9A4A1BDA5C9300507FE65013CBE121A83
          47B510C1330CCFE858348730123C43F0F8DA79CEF81CAE282FCC37647D1CDF89
          B6338564213C352579BE846C86F8E39C78827DBDCA2B28D4B04FD3B1332D028F
          1F3C7F8247B522E167043FA363DA1CC248F8F9137EFCEC7E5CC4F30138FC25A4
          C51F552B123C063CA363DA1CC248F00CC3E30FCF0764FFCC96470592E44318C9
          167870B76333923D8C641046BAE2B76BEBF3BE3BB77CB03A2BFE9FAD39C9CB79
          5218C8D3CBE0CAACF8E032B329B82825528D69936624092345071C5D1EE4B3EF
          9FB978FE2061A4E709235D99624C9B24690D4E16363BC77C796A729202AB7292
          26F0C8983669469230525480F7F2D33EFB5DC9B35AFC5465257C083F2BE00982
          27143FA1F809C58F1AD326CD4812468267053C1F72859F20AFFD7DB4EE6CB0F3
          D4E4A44CE02935C78616A644B99C87762D23232ECA488F8BEA0FF139BCD181E7
          79FC84E027023F11F044C0A3C6B44933928491F0F3FC62F9394DF3574E729CD1
          DE6233DA9A1B8DCCF898DF3BF1AC82270C9E2878A2E08982478D699366240923
          C1B36AA13C89A1A70D6B79F18403A8E249300D4CC75341F7B42B785222432E35
          71A1E45C0728958569B1518E3C77B27F5EC04F247E4CF098E031E1478D699366
          240923E1E705FCDC39DFFD3C1D8F8C4C735A2FE1590B4F343C71F0C4C113078F
          1AD326CD4812468267ED427812C3CFBCE2D8D664F734C5FEB9298F0A2305F82C
          8847FCD8AC93D76B0A9EBBF0B38E904D2C7E18899690889F44FCA8316DD28CA4
          C23F013EEBF073D7A2AFD7E4EFD75DD59654C55399933C89673C8C044FA0CF01
          77F1AC87271E1E5A911292F1938C1F35A64D9A91541829C067BD3B782AB313C5
          CF4BF024C1632ECF4A34C36386478D6993662415460AF079C9533C2569719EE4
          F9307E36E227053FE9F8498727BD20255A8D699366240923E167237E3EECEAFD
          CC7A09CF6678D2E0C982270B9E2C78D4983669469230123C9BDDC9434862A9F0
          7C043F5BE1C9C08F053F16FC58F0A3C6B44933928491F0B3153F1F71C37A8DF3
          54E4A4789CA7223B4978B6E1271B9E3CFCE4E1270F3F6A4C9B34234918093FDB
          DCE14778AA2CE6ED121A83A7109E42780AE15163DAA41949C248F06CF7044F19
          2D4DC569F1E33CF630921B793E8A9F9DF8C9C74F313CC5F014E3478D69936624
          15460A38B6133F1F75F57E66BD8467B784C6E02983A70C9EB2FC941835A64D9A
          91541829E0D8EE5B94E75FF0B3073F25F8A9C04F057E2AF0A3C6B44933928491
          F0B3073FFFE286F5523C846C6ECA73CAE7A0BB78F649880D3FD5F8A9C64F357E
          D4983669469230127EF6B991E7003C95E50491E0B1C26385478D699366240923
          C173C01D3CE5D9C9B25E33F24818C98D3C1FABB4A41D94D0187E68454AAA2F4A
          4BA8C78F1AD326CD48124682E7207E3EE6EAFD8C1FE1390C4F2D3CB42225D9E0
          B1C1A3C6A2493392847FE039EC091EA62F2B9EBC5493E2190F2305F8BA8BE7E3
          F8F1AAB098EBF1D30C4F333CCDF0A8316DD28C2461A4F0005F2FFC7CDC0DEBB5
          14798EE2C7869F56FCB4E2A7153F6A4C9B34234918093F47DDE1A72C3BE513AC
          970F3CCDF0B4C3D30E4F3B3C6A4C9B34234918091E1F783EE1EAF572E429B398
          67E409F039E4169E0A4BBA2F7E5AE1E9C04F47615A62077ED498366946923012
          7E7CDDC8E3074F3B3C9D25D9299DF074C2A3C6B4493392847FE0F1F3244F6E6A
          6CA78C69B3F3104E7017CF27592F7FFC74E0A70B3F5DF8E982478D699366A4B1
          30923F7E3EE986FD3C238F4318C99D3C27F1D3899F6EFC74E3A71B3F6A4C9B34
          238D85914EBAC9CFA758AF80724B5A173C3DF0F4C0D3038F1AD326CD486361A4
          00783EE586F55A523CA5D9A9C213889F6EFCF4E2A7173FBDF85163DAA419692C
          8C14E80E3F633C41F0F4C0D3074F1F3C7DF0A8316DD28C3416460A72170F6D12
          8AA7D4923623CF499FC32EDF3FF8F95778CEC0D30B0FAD4829D70AD293AE5952
          E3D498366946923012C7C333F0FCABABF7B3334F7176EA52E00929B3D0A86549
          BB0ECF75FC5CC78F1AD326CD481246C24F889BFCDCCD7A85C1F3063CFDF0F4C3
          D30F8F1AD326CD48124682270C9EBB5DBD5EB4D82C459E70FC5CC7CF007E06F0
          33801F35A64D9A91C6C248E1D3F9A1CDC608F23E643038C8083872807176BF69
          757EDF3DEDFB2FA7F729637E22E179ABC4923E08CF203C83F0A8316DD28C3416
          468A9CC4B376256D3FA30D521244923049B125D338B263CBC53BEFBCF37D8E4C
          73E129CBCD9C33CF8E177EDB6B3A73CA700CDD48F846C6D965A724FCEEE73FFB
          E907E7C9730F3CD1F819C0CF107E86F03394931AAFC6B44933D25818291A3FF7
          D8F7F3C12D1BDE28CAC99C14B891C0547274C4EB0FFFE0A1FFB3183C45B434E5
          A7274FC973E2D811C543906D75D4E913BF3FD7DA3CE9FD6C072500C1FE3E5716
          B07FC44F0C7EDEC1CF0D786EC073033F6A4C9B3423491889B0568C9DE7C0A617
          3732466FC8B9AD49DE89CA88C6535E07BA16C0732F3CB1F00CC1330CCF303CC3
          F0A8316DD28C2461247862E1B957FC78EDD8FC725672C21F9CDBBE84C74AA1C6
          E9A3872E2D94474263B3E539BA63CB16F6C83BAD7C579DDFA75BCB4B1783271E
          9E3FC133829F11FC8CE0478D699366240923E127DEEEC777F7CB1BE34283065A
          1AEB5CC1731FEB9528A131780C780C780C78D4983669469230123C89F0DC27EB
          253C09E1677E3F1B9EF1401261A46DA70EEC3202F6EF94316D8E6124833092E1
          B5F5A5D6633B367FCF67FBA6F715A746FD738939FAD1A294E87D05F436D08A14
          946D0A0EA212E0948C699366A4B130D2FB08237DEFE5E79F6B9D218C646CF9ED
          D36ACE9CE3E7663C7EBB5F3E7FF2C0AEFFB6F3149B631E2D4C89513CB4A40465
          994282CC1181A7644C9B34238D8591169547BC9C3CB8C78838E96B849FF07D35
          F0D05E3BCF87E0790C9E03F09C8627049E10784ECB983669461A0BB72C1A0F2D
          5686DFEE6D24D5D2550A37E2A45F8F9DA72835DAED3C84488CF36D2DC6252AE7
          A6E1791C3F87F293A2A4C52A0C3F61F891B17A87A4196931FC84F81C328AB2D2
          26A42885475A6EC24F4CF2F3043C5EF0488B55243C91F0C8583D2F69465A280F
          AD4C872A0A2C13E6DBCAC14B5A6564C6ED14EBE5529EE800BFC353F148C2738A
          F5BA93FDFC247EBCF1232D56D1F889C68F8CD5F39666A4B1F0CFBCF7732447AF
          2A3C382770A7D93FC2F3143CC7E09116AB58786253234ECB58BD63D28CB4501E
          F1533EC57A799267CAF59AFAFB75177E9E2E4831F9E1C7442B493C7EE2F12363
          F5FCA419692C8C34EFF59A76FFCC9227D3143A81672C6CE3169EC2D498BB8ACC
          A665F8F1C74F1C7E92E049C28F8CF9F39766240FF03C03CFC9BCA4E8047852E1
          49852741C6B44933D22DCEF361D6EB39FC04E2475AACD2F093869F6419D326CD
          4863E11F77ED1FE1590E4F103CA9F064C293094FAA8C699366A45B9CE723F859
          819F60FC488B5576466C58367E64AC5EB034238D855BDCB55E53F2A444067984
          A720D5F4914273ECCAFCD4D850FC64D202928B9F5C7832654C9B3423B9D3CF18
          CF2A78C2E1C981271F9E7C7872644C9B34232D151E19D3E6019E8FB25EABF113
          999B1C938B9F22FCF01B83A05C19D326CD4863E11FB7EC67D64B78D6C013034F
          013C25F094C0532063DAA4196929F14833929B79FE053F6BF1138B9F22FC94E3
          A71C3F4532A64D9A91C6C248EE5AAF9BF28C856DDCC9B30E3FF1F829C54F257E
          2AF1532A63DAA419C99D3C3421FD0B5397D7C393084F393C35E9B1E135F094CB
          98366946BAC5793E869F0DF849C64F152D20B5F8A94D8E3C532563DAA419692C
          FCE396FDC37A09CF467852E1A981A7019E06786A644C9B34237982272F352E95
          F2DEA5C0F371FC6C82270D9E3A6969C28F0D3F7532A64D9A91C6C236EE5AAFA5
          C8B3053F99F869C04F0B7E5AF0D32063DAA419C9CD7E3EC17A6D85271B9E2669
          6982A7159E2619D326CD4863612477ADD727F2D3E227F12445068FF38C856DDC
          C9B30D3F16FCB4E0E71C7ECEC1D32263DAA419C9033CDBE1C983475AB52EA4C5
          465C8047C6FCE54933923B79686692F51AE7A125C5D33C9F8467476E6A7C414E
          72EC39782EE2E7227ECEC9983669221A0BDBB865FFE0675A1E19D3E6219E5DF8
          29C24F077E5EC1CF2BF8E990316DD28CE4663F9F62BD76C3530ACF45782EC373
          199E8B32A64D9A91C6C22DEE5AAF25C7939796B0173FE5F891117F57F073053F
          32E6AF5C9A91DCED079E7DF0F09BABD8CBF05C85E72A3C97654C9B34232D059E
          C4C810C523CD48636124B7EC1F9A90FE153F07F0538D9F1E5A375E37C746BE0E
          8F8C1DAC9666A4B1B0CDADCC73D0929A60CD4E8EBB2AAD51F8B9869FAB32A64D
          9A91DCECE76ED6EB103C75F0F4C1F3263C6FC2236307EBA419692CDCE2AEF55A
          8A3C47F0D3881F69F9EAC74F3F7E64EC60A3342379C08F173C4DF0C888BFB7E1
          791B9EEB32A64D9A9166E239C4883DDE371A293111C6D19D5B8DF5CFFC7AB7F3
          FBF7B9BCDFE1FB2EEB35679E3D2FAE32526168E487F612B491C00263C18CA3BB
          B7072D8487A69F7B72D312BDF1D3821F193938889F41FCF4CB983669461A0B23
          8DEFE7EDAB971BB969292AA4651F1D27ED3B12DE8A3C7D32DE953CD28C3416FE
          513C5B563CD37EF2D03EA3B6B26C42439204B65A9B1A8C409F23318BC0E3839F
          56FC48CBD71FF1F347FC0CC89836694672E4D9B66A794706EBE2DC46243C3504
          800E6E7DE9E80279EE65BD7C69D869876790915F3752E3A26E244485CAD8C176
          69461A0BDB283FECD96E5B5DCDA4F62879779C634E32BCB66F3EEC2E1EDAACBE
          4FD4E48DE686BA49EFAEC5576E5AF262F1F8E1E73C7E86F0F32E7EDEC58F8C1D
          3C2FCD48763F47B76FFAEF107F9FAB754C16926CC184893A8BC7731FEBE50F4F
          273C37E0790F9EF7E091317F9DD28C3416467A9FF09C3A7AA847C6303A07DAA6
          F2331E02228C743761A437A60A23D16C63F8ECD83CBCF6992793ED0B4DD0E6B6
          4C53D03D1931A71F3287073C9A18E2FFA829D0E7A130FFC3F7C89836FBDFB7EC
          973F4DDEB966C5F0CE177E6BECE04BBF7DD573C6CBCF3F6B6F469230D21B9B97
          FFE66EE70D34134F625810C9ACF4F7FCBD0E5622FCEFE59F159E0CD319C5C313
          B14713424E8CF3C8D8AFC5E491F08FFFFE5D4609894A697011B1B515A57F3DE5
          7BB4610A9E1FC0F3183C8FE1E707E267B17968B03292A3C2C6EBEF144F65D95F
          03FDBCEBDDCD93147EC668B5354C1ABD65C58F230F410059AF7BD363821EC6CF
          E3F8793C26F0D8C3F8B97731FCD08A746F7662EC3BD220E39C5AB4AF9713CFED
          633C3F84E709789E80E787A1FE4784E7F685EE9F33470FDEBFD478B2126307CF
          4F91C29DC1CF7DACD78FF0F3247E9EC4CF8FF073DF62F9A1296A50BE4F7358AF
          FBE1F9313C4FC1F3143C3F86E7FEC5E299C77A8DF3C4879C5C2A3C8FE0E76978
          9EC6CF239EF4936E0A163F8FA484072E159E4FC3F3137896E167197E7E829F4F
          7B62FF1004B81D3F9F81E7A7F03C03CF33F0FC149ECF681EE3EFEC7ED262CEFC
          0C3FCFE1E739FCFCCCC37E3E0BCF2FE0590ECF72787E01CF673DB85E1378B80E
          533C3246CADDE7AFB1F5FA1C7E7E899F15F85901CF2FE1F99CA77992234E2F25
          9E47E159899F95F87934C4DFCB937E3ECF7A3D06CF2A7856C1F3183C9FF7D47A
          A599423ECFD465C5131712E0511E5EBCDF0ECF17E0F9153CABE1598D9F5FE1E7
          0B9EF003CF1D633C4FC0B3069E35F03C31C67387BBBFEF4B94E78BACD7AFF1B3
          163F6B392EFF1A3F5F94316D1EF233258F8C4DF110CF97F0F3247ED6E1671D7E
          9EC4CF973CCCF3143CEBE1590FCF534B8D27D8FFA827FD7C99F5FA4D5244D006
          FC6CC0CF6FE0F9B207D7EB2BF02C8367233C1BE15906CF573CC14330E10E8AA8
          BF921A13B2D4789EC1CFA6D890539BF0F3CC12F0B39478BECA7A3D8B9F2DF8D9
          829F67F1F3550FEE1FE1790E9EADF06C85E739CD33FA7C75ECFBF5007E96E367
          1B7EB6711E5D8E9F073CB85E0FC2B3029EEDF06C8767C5197FEF073DCD931871
          C6E33C0401E478287E56C2B3033F3BF0B3D2537E8427D514F6604A4CA8E23185
          068EF3C85834775FFF8CF17C0D9EE7E1D905CF2EFC3C8F9FAF7988E7FDF8F93A
          3CABE0D90DCF6E7856C1F3751993E4013F4B9567357EF6E2672FDFF3D51EF6F3
          0DD66B0D3CFBE0D907CF1A78BEE1C1F5523C0911C1E33C41FE3E1EE3493185FF
          1B3C6BE139809F03F8590BCFBF79CACF52E2E145EEFBC778D6E1E7207E0EE267
          9DA7FC8CF17C93F57A119E43F01C82E74578BEE989F5B2F324C7844DE291B144
          EE3E1E8EF17C0B9EF5F839829F23F8598F9F6F7998E72578BC62424F7BC1F3D2
          5C7878F73EEDFBF7B9BEAF74F033679E63BB5F36E2424E4BA987B17BC39AB7D6
          FEE657E3E3ECECEB3C4F9E6FB35E1BF1E38D1F6FFC6CC4CFB7A75BAF7D2FAD31
          4AF3720C5B6D0DC19F46A3AEAAC2883875FC9D833BB67EDF399FE06A9EFD1BD7
          1925B959AAD0C31E90EA38D76A5414E70F9FF43EF29345E2F90E7E36E1C7073F
          3E7CCF37E1E73BCE7E5E5EBDFC5D0B419F0BE7DA26BDD72FB664FD79E3F3CBBF
          B6183CB4EB7C2729267C737C64882F3CBEF06C9E8AE7E8AE6D7FA92570E33CCE
          4E022FF1E12143BBD63D7FBFBB789E7DFC97598C887BEFE2857393DE5BCB88C6
          A8D327071791E7BBF8D9821F3FFCF8E1670B7EBEEBB85E5BD7AD2EA97718CBE8
          F82E5DC275316702DE59249E0FB05EDF83671B3CFEF0F8C3B30D9EEFC99836F1
          2FD913BF2307EBAB4A0BFFEA3CCA4EB8A6E2190F2411467A1F61A480137BB71B
          12B891908B3400A930D2F64DA39F9D5BBAFEEB3BDFFCB6F3624FF7D7DFFEDA03
          DFA6AEAE6BE7BAE70D2ADAA60A2319849102363DF7D484BA38F9F74DC7939D14
          A716FE025FCEAC94F8DFFFF0BFFEE387EEE639B66B1B2D3B4513126892CECB4C
          8A1B70378F8C239383A873884378DCED87546D7465117314C70EA28E4C9EE089
          0D0A885F6A3C55C593E75E8A274FF9A928CCD5EB35CDF16729EE9FA5B69F35CF
          F4E72FBD7F9E9EF1FCAEFD683F0BB93ED4FB47EF1FBD7F2636CB3ADE0B2DF4FE
          4B7FBFF4F74B7FBFF4F7CB93F7A7FAFE42DF5FCCF7F9A13E7FE9F3973E7FE9F3
          973E7F8DBE4FD1C7437D3CD4C7437D3CD4C7437D3CDCBA6299849196D4F379B2
          009EE7A13025D0EB80114E8394D78E2DC6A615CF444D974F70F5F9F404D39764
          7ACE595BBD0A47A5C79B0CBF037BD21FFEEFFF1C0FE4D99FF7B9F2FCBE83517F
          8C90534D5652562141200998C8B8BDD4D8A81A77F3F85206535D5A34B93D8AF2
          9140EF23B9EE5CAF9DEB565D667C9CD1D1DE3A2183218E6AABCA0CEFDD2FA7B8
          9307377DB24ECE612D694BCACF4C338EEFDF15EF2E9EFFFCEE777E688E8FFEFD
          2B4E4D4D72FE91518853F14C18934618E9F30491CE4F19461A0B25FDE8FBDF7D
          6AB601A0EF7FEB1B4FEDA296CDFE915AB46D2B9F31EC0743C49CE7F3F9E9FE7D
          CE3CB4228D2FBA08AE6634D92FFEE7E197DCC9E3BB6B6B470E1B70AA059760D0
          2F1FF9D17A77F19C3AB8FB4B29D1617D92E69CD42A73B1C3909153EEE409F5F5
          7AD06C8A9C86E782C778642EE9643F9EE149890E7F5D6AF7960ACF525CAFA9F7
          B367D64BFB99FEF8BC54BFEF7AFF4C7DD3ADD76BE6EB0DED47FB59C8F5AADE3F
          7AFFE8FD33FD03DF85DE0FEAEF97FE7EE9EF97FE7ED9479DEBE71B4BF3798BBE
          FFD2F75F729C9EEBF35E7D7ED7E7777D7ED7E7777D7E1FDD03FA78A88F87FA78
          A88F87FA78E8FEE3A184A35E7AE6D71D33E513DC717CDEBEFAB7C6C9C3FBD467
          D7FA172EAF79EA575F992E5FE04A1EF951487CD819A38ACC05D38E8C22A643C5
          87075FF3DAB3F317EEE6D9C9BD455A7C8C21EFF5A5C843B2201208AA2C29F883
          DFE1FDBF7227CF76C25A3266D03990A4C691A5A7BEBD63DDAA6FBA8BE73FBEF9
          F5DF9CF13B6AB4106073CEA7085F4C70E09B0736AF7FD05D3C3F79F8A1F54596
          CC49A3FEE478D6545F6B44041CEF73E6991048A219E9F6633B36FF96CF5F2634
          23D91B92F8E3CF7FF09F2B671BB8F99FFFFC8F958E2754FB4DAA2433F9A2FF65
          C333BFFEEDC6679F1C1F27E2FCEF75E01929CEC9180F71483D966CC49AB26263
          DDCAE547DCC9137078EF4B65CCC3739ECF273CF26BBE179FFFEDA4F985D3F12D
          D44F989FD71DA1C7BD5F9E92872F696551BEC7789C5371F2D79EE3C999BC5E1E
          E59962FF689EBF5BCAFB79D2F75DAF975EAF9B9CBFF47E9EF9FCAEFD683F0BB9
          3ED4FB47EF1FBD7FA67F60F7BFF9FE425F8F4D5E777D3CD4C7437D3CD4C743F5
          0B787D7FAAEF4FF5FDE9829E3FEBF3A93E9FEAF3A93E9FEAF3E9E81ED0C7437D
          3CD4C7437D3CD4C743F71F0F5F7AF6C9919BE5255C7D7C9650D2812D1B0CDFBD
          3B2999593DB2E6E927D6CF94DF70250FA3FD9848156F300ECC2826F4921415F6
          57EFBD3B8F7A82C767EF0E388AD4483B990E75BEADC5A8AD2833A2CF9C0A39B8
          65FDF83862E71C882BFC30CECEA82617631F9126C1A46EC9CA94971847766E3D
          E04E9E352B961F2EA1A4C7F959AD84B5F233D2A481EC65679E09812411C698B6
          7FDABBFE05DBF898368730D2D1ED9BDE7CE08B9F9BF5C8B62F7CE6BE6F73027B
          73D2498CF16DAB7EFDA88DCDF34F370B13098FFF81DDE7A41DE95C6BB371EE6C
          93122C8B5F909DFEE76F7EED81598F6C5B0C1E6647DE19ECE773B9BD050E59EC
          B18FCC0B2CCEC9BCB12478F023AE348FF17753AE97F633FEBDD77EA6BFC09463
          93F6A3FD2CE4FCA5F78FDE3F7AFF4CBD075C763DA6CFEFFAFC3ECBFB0B7D7CD6
          C7677D7CD6C7677DBF3CBA07F4F1501F0FF5F1501F0FF5F1501F0F67F37C5E9F
          2FF4F9429F2FF4F9429F2FF4F9429F2F16FEBE5B9F4FF5F974A99F4F5F66D2D5
          6CF31BAEDCCF3B084879EFD96EECDBBCDE58FD9BC7DB56FEEAE71FBA599EC415
          3C12D492317296B414430238293111C6BE2D2F5D5AFBEC531F73370F4EDE8A0D
          0D326CB535AAB5495AA464845BF499C0BE2DAB967FD49D3C5FFEC267FF332FC3
          7CA39DCC8E14F6D8B332AD8CD6F3DEB7EBF2BE8DEBEE74E6991448A219E97DFF
          F18D073FB97FE3DA734EA1A491277FF6E31337FB1FE4FC7F7FE4BFFEE3045FB0
          11C72FD9A6E79F3DF785FBEFFD249568EFBBD9BF4F7896FDEA97F707FB7A5D2E
          B164194D7535EA53929B6DACFDEDB389FFF7FFFEDFBFBFD9BFC3F1FFBE183CCF
          3FF7CCB7CC71D16FD633D7D1565B6D34D4541A65F9397F5D0A3CE2A651F38CEF
          6BD93F8EEBA5FD4C3CE16B3F335F00693FDACF42CF5FFAF833FD1ED2DF2FFDFD
          D2DFAFE9F7C0625FAFEAEB1F7DFD33D7FB2F7DFED2E7AF85DCBFEBFDA3F78FDE
          3F53EF017D7E9FF9FA57FBD17EF4F3CCBFED017DBFACEF97F5FDB2BE5FD6EF9B
          46F7803E1EEAE3A13E1EEAE3A13E1EEAE3E16CF324AE7A1E2501A99DEB5F30D6
          FF7659E7E7EFBBE7539EE281E3BDD3C78E18A9B151862924D0D8BF6DE3E59F3F
          F23F5FF704CF334F3C16579893F1D7DAAA729593A961DC5F4254D89B4FFFFAF1
          6F4DC5332990640FEF7CE69E7FBD6BD9A33FF379E9B7CB32D62C7BD2F483EF7E
          EBD9B9047F1CFFDEEF7EE36BCF3EF3D8CF4DCF3FF5AB8C477FF490CFC73F72D7
          5D73FD77FDE8A1FFBA7FC3AA15D1678E1F6B0A38E655F5F31FFD60F3A73EF189
          7F98EBBF47FEFEC5E0F9C54F1FF9A2F004FB1F6B3A75EC68A5E6997852D47E66
          BE50D47EB49F851C0FF5FED1FB47EF9FA9F7803EBFCFFCDDD07EB41F7DBDAAAF
          57E772EFA4AF37F4F586BEDED0D71B733966D8FF5E7DBDA1AF37F4F586BEDE98
          CBB1435F6FE8EB0D7DBDA1AF37E672CCD0D71B331F33B41FED47BFAF9C7A0FE8
          EB0D7DBDA1AF37F4F5C6FFA6EB8DE54FFC32E7E70FFFD7898F7DF8CE0FCFF57F
          D7621D0F3FF5A94FBDEF7F1EFAAFCDC7BD0E5506F87835AD7EF6E9E81FFFF0E1
          2F4EC7336D20C9FE0FDC76DB6D7FFF0FFFF00F376D32BAD9FFE0BFFBBBBF7B1F
          9F39B5194DF5EFFCC77FFCC7DBFECFFFF93F0BFEF7689E994F40DA8FF673B3EF
          F44CFF77BD7FF4FED1FB67F21ED0E7AF99BF17DA8FF6B390E386DE3F7AFFE8FD
          33790FE8EB317D3DB690EF85DE3F7AFFE8FDA3AFE7E7BA07F4F598BE1E9BEB9E
          71FCFBF5FED1FB47EF1F7D3D3FD73DA0AF57F5F5EA5CF78CE3DFAFF78FDE3F7A
          FFE8FB9DB9EE017DBDAAAF57E7BA676E85FB1DC9EC7CF0831FBCED666E6E1A48
          BAD9BF40FFDF6797BAD69EB427BD07F41ED07B40EF01BD07F41ED07B40EF01BD
          07F41ED07B40EF01BD07F41ED07B40EF01BD07F41ED07B40EF01BD07F41ED07B
          40EF01BD07F41ED07B40EF01BD07F41ED07B40EF815B610FE8401223DC6E8585
          D6FF1BF53AEB3DA0F780DE037A0FE83DA0F780DE037A0FE83DA0F780DE037A0F
          E83DA0F780DE037A0FE83DA0F780DE037A0FE83DA0F780DE037A0FE83DA0F780
          DE037A0FE83DA0F780DE037A0FB8630FE8308E0E24E93DA0F780DE037A0FE83D
          A0F780DE037A0FE83DA0F780DE037A0FE83DA0F780DE037A0FE83DA0F780DE03
          7A0FE83DA0F780DE037A0FE83DA0F780DE037A0FE83DA0F780DE037A0FE83DA0
          F7C0A2ED8145FB17B9233DA5FF1B3AA5A7F780DE037A0FE83DA0F780DE037A0F
          E83DA0F780DE037A0FE83DA0F780DE037A0FE83DA0F780DE037A0FE83DA0F780
          DE037A0FE83DA0F780DE037A0FE83DA0F780DE037A0FE83DA0F7C0D2DE03FF0F
          9BA0C66898C64E7D0000000049454E44AE426082}
        DisplayName = 'Knob'
        GlyphCount = 65
        StitchKind = skHorizontal
        Height = 36
        Width = 2340
      end>
    Left = 112
    Top = 32
  end
end
