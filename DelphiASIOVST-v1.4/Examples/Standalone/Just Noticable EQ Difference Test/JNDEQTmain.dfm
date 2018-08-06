object FmJNDEQT: TFmJNDEQT
  Left = 329
  Top = 72
  Caption = 'Just Noticable EQ Difference Test'
  ClientHeight = 312
  ClientWidth = 328
  Color = 8620693
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    328
    312)
  PixelsPerInch = 96
  TextHeight = 13
  object LbAudioFile: TGuiLabel
    Left = 34
    Top = 8
    Width = 73
    Height = 15
    Margins.Bottom = 0
    AutoSize = True
    Caption = 'Audio File:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 4.000000000000000000
    Shadow.Color = 10333885
    Shadow.Visible = True
    Transparent = True
  end
  object LbAudioFileValue: TGuiLabel
    Left = 113
    Top = 8
    Width = 207
    Height = 15
    Margins.Bottom = 0
    Anchors = [akLeft, akTop, akRight]
    AutoSize = True
    Caption = 'Pink Noise (double click to change)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsItalic]
    FontOversampling = fo4x
    ParentFont = False
    PopupMenu = PuAudioFile
    Shadow.Blur = 4.000000000000000000
    Shadow.Color = 10333885
    Shadow.Visible = True
    Transparent = True
    OnDblClick = LbAudioFileValueDblClick
  end
  object LbVolume: TGuiLabel
    Left = 8
    Top = 295
    Width = 42
    Height = 11
    Margins.Bottom = 0
    AutoSize = True
    Caption = 'Volume:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 4.000000000000000000
    Shadow.Color = 10333885
    Shadow.Visible = True
    Transparent = True
  end
  object LbVolumeValue: TGuiLabel
    Left = 146
    Top = 295
    Width = 56
    Height = 11
    Margins.Bottom = 0
    Alignment = taCenter
    Anchors = [akTop, akRight]
    AutoSize = True
    Caption = '0 dB'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 4.000000000000000000
    Shadow.Color = 10333885
    Shadow.Visible = True
    Transparent = True
  end
  object ClipLED: TGuiLED
    Left = 299
    Top = 293
    Width = 16
    Height = 16
    BorderStrength_Percent = 50.000000000000000000
    Brightness_Percent = 100.000000000000000000
    LEDColor = clRed
    BorderWidth = 2.200000047683716000
    Uniformity_Percent = 30.000001907348630000
    Transparent = False
    OnClick = ClipLEDClick
  end
  object LbClipIndicator: TGuiLabel
    Left = 229
    Top = 295
    Width = 70
    Height = 11
    Margins.Bottom = 0
    AutoSize = True
    Caption = 'Clip Indicator:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 4.000000000000000000
    Shadow.Color = 10333885
    Shadow.Visible = True
    Transparent = True
  end
  object LbInformation: TGuiLabel
    Left = 8
    Top = 191
    Width = 312
    Height = 13
    Margins.Bottom = 0
    Alignment = taCenter
    AutoSize = True
    Caption = 'Information'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 4.000000000000000000
    Shadow.Color = 10333885
    Shadow.Visible = True
    Transparent = True
  end
  object SliderVolume: TGuiSlider
    Left = 56
    Top = 297
    Width = 84
    Height = 8
    Anchors = [akLeft, akTop, akRight]
    BorderColor = clBlack
    BorderRadius = 3.000000000000000000
    BorderWidth = 1.000000000000000000
    Color = 8620693
    DefaultValue = 3.000000000000000000
    Max = 3.000000000000000000
    Min = -30.000000000000000000
    ParentColor = False
    Value = 3.000000000000000000
    SlideColor = clBlack
    Transparent = True
    OnChange = SliderVolumeChange
  end
  object BtMedia: TGuiMediaButton
    Left = 8
    Top = 4
    Width = 20
    Height = 20
    Transparent = True
    BorderRadius = 3.000000000000000000
    BorderWidth = 2.000000000000000000
    ButtonColor = 10333885
    OutlineWidth = 1.000000000000000000
    Color = clBtnFace
    Enabled = False
    OnClick = BtMediaClick
    ParentColor = False
  end
  object GbEQFilter: TGuiGroup
    Left = 8
    Top = 209
    Width = 312
    Height = 82
    Anchors = [akLeft, akTop, akRight]
    BorderColor = clBlack
    BorderWidth = 2.000000000000000000
    Caption = 'EQ Peak Filter'
    Color = 8620693
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 10333885
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo3x
    GroupColor = 8620693
    Native = False
    ParentColor = False
    ParentFont = False
    BorderRadius = 5.000000000000000000
    TabOrder = 2
    DesignSize = (
      312
      82)
    object LbBandwidth: TGuiLabel
      Left = 7
      Top = 60
      Width = 55
      Height = 11
      Margins.Bottom = 0
      AutoSize = True
      Caption = 'Bandwidth:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 10333885
      Shadow.Visible = True
      Transparent = True
    end
    object LbBandwidthValue: TGuiLabel
      Left = 253
      Top = 60
      Width = 56
      Height = 11
      Margins.Bottom = 0
      Alignment = taCenter
      Anchors = [akTop, akRight]
      AutoSize = True
      Caption = '1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 10333885
      Shadow.Visible = True
      Transparent = True
    end
    object LbFrequency: TGuiLabel
      Left = 7
      Top = 44
      Width = 55
      Height = 11
      Margins.Bottom = 0
      AutoSize = True
      Caption = 'Frequency:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 10333885
      Shadow.Visible = True
      Transparent = True
    end
    object LbFrequencyValue: TGuiLabel
      Left = 253
      Top = 44
      Width = 56
      Height = 11
      Margins.Bottom = 0
      Alignment = taCenter
      Anchors = [akTop, akRight]
      AutoSize = True
      Caption = '1 kHz'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 10333885
      Shadow.Visible = True
      Transparent = True
    end
    object LbGain: TGuiLabel
      Left = 7
      Top = 28
      Width = 26
      Height = 11
      Margins.Bottom = 0
      AutoSize = True
      Caption = 'Gain:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 10333885
      Shadow.Visible = True
      Transparent = True
    end
    object LbGainValue: TGuiLabel
      Left = 253
      Top = 28
      Width = 56
      Height = 11
      Margins.Bottom = 0
      Alignment = taCenter
      Anchors = [akTop, akRight]
      AutoSize = True
      Caption = '0 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 10333885
      Shadow.Visible = True
      Transparent = True
    end
    object LbAutoVolumeAdj: TGuiLabel
      Left = 115
      Top = 6
      Width = 149
      Height = 11
      Margins.Bottom = 0
      AutoSize = True
      Caption = 'Automatic Volume Adjustment:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 10333885
      Shadow.Visible = True
      Transparent = True
      OnClick = LbAutoVolumeAdjustmentClick
    end
    object LbAutoVolumeAdjValue: TGuiLabel
      Left = 270
      Top = 6
      Width = 18
      Height = 11
      Margins.Bottom = 0
      AutoSize = True
      Caption = 'Off'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 10333885
      Shadow.Visible = True
      Transparent = True
      OnClick = LbAutoVolumeAdjustmentClick
    end
    object SliderBandwidth: TGuiSlider
      Left = 68
      Top = 62
      Width = 179
      Height = 8
      Anchors = [akLeft, akTop, akRight]
      BorderRadius = 3.000000000000000000
      BorderWidth = 1.000000000000000000
      Color = 8620693
      CurveMapping = 1.000000000000000000
      DefaultValue = 3.000000000000000000
      Max = 10.000000000000000000
      Min = 0.100000001490116100
      ParentColor = False
      Value = 3.000000000000000000
      SlideColor = clBlack
      Transparent = True
      OnChange = SliderBandwidthChange
    end
    object SliderFrequency: TGuiSlider
      Left = 68
      Top = 46
      Width = 179
      Height = 8
      Anchors = [akLeft, akTop, akRight]
      BorderRadius = 3.000000000000000000
      BorderWidth = 1.000000000000000000
      Color = 8620693
      CurveMapping = 2.000000000000000000
      DefaultValue = 1000.000000000000000000
      Max = 20000.000000000000000000
      Min = 20.000000000000000000
      ParentColor = False
      Value = 1000.000000000000000000
      SlideColor = clBlack
      Transparent = True
      OnChange = SliderFrequencyChange
    end
    object SliderGain: TGuiSlider
      Left = 68
      Top = 30
      Width = 179
      Height = 8
      Anchors = [akLeft, akTop, akRight]
      BorderRadius = 3.000000000000000000
      BorderWidth = 1.000000000000000000
      Color = 8620693
      DefaultValue = 15.000000000000000000
      Max = 15.000000000000000000
      Min = -15.000000000000000000
      ParentColor = False
      Value = 15.000000000000000000
      SlideColor = clBlack
      Transparent = True
      OnChange = SliderGainChange
    end
  end
  object PnSelectorA: TGuiPanel
    Tag = 1
    Left = 8
    Top = 31
    Width = 100
    Height = 100
    BorderColor = 2105376
    BorderRadius = 5.000000000000000000
    BorderWidth = 2.000000000000000000
    PanelColor = 10333885
    ParentColor = True
    TabOrder = 3
    UseDockManager = True
    OnMouseDown = LbSelectionMouseDown
    OnMouseUp = LbSelectionMouseUp
    object LbSelectionA: TGuiLabel
      Tag = 1
      Left = 25
      Top = 3
      Width = 48
      Height = 80
      Alignment = taCenter
      Caption = 'A'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -75
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnMouseDown = LbSelectionMouseDown
      OnMouseUp = LbSelectionMouseUp
    end
  end
  object PnSelectorB: TGuiPanel
    Tag = 2
    Left = 220
    Top = 31
    Width = 100
    Height = 100
    BorderColor = 2105376
    BorderRadius = 5.000000000000000000
    BorderWidth = 2.000000000000000000
    PanelColor = 10333885
    ParentColor = True
    TabOrder = 4
    UseDockManager = True
    OnMouseDown = LbSelectionMouseDown
    OnMouseUp = LbSelectionMouseUp
    object LbSelectionB: TGuiLabel
      Tag = 2
      Left = 27
      Top = 3
      Width = 44
      Height = 80
      Alignment = taCenter
      Caption = 'B'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -75
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnMouseDown = LbSelectionMouseDown
      OnMouseUp = LbSelectionMouseUp
    end
  end
  object PnSelectorX: TGuiPanel
    Left = 114
    Top = 31
    Width = 100
    Height = 100
    BorderColor = 2105376
    BorderRadius = 5.000000000000000000
    BorderWidth = 2.000000000000000000
    PanelColor = 10333885
    ParentColor = True
    TabOrder = 5
    UseDockManager = True
    OnMouseDown = LbSelectionMouseDown
    object LbSelectionX: TGuiLabel
      Left = 25
      Top = 3
      Width = 46
      Height = 80
      Alignment = taCenter
      Caption = 'X'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 27903
      Font.Height = -75
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnMouseDown = LbSelectionMouseDown
    end
  end
  object PnSelectorXisA: TGuiPanel
    Left = 8
    Top = 137
    Width = 100
    Height = 48
    BorderColor = 2105376
    BorderRadius = 5.000000000000000000
    BorderWidth = 2.000000000000000000
    PanelColor = 10333885
    ParentColor = True
    TabOrder = 6
    UseDockManager = True
    OnClick = LbXisAClick
    DesignSize = (
      100
      48)
    object LbXisA: TGuiLabel
      Left = 7
      Top = 5
      Width = 85
      Height = 38
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      Caption = 'X is A'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -32
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnClick = LbXisAClick
    end
  end
  object PnSelectorXisB: TGuiPanel
    Left = 220
    Top = 137
    Width = 100
    Height = 48
    BorderColor = 2105376
    BorderRadius = 5.000000000000000000
    BorderWidth = 2.000000000000000000
    PanelColor = 10333885
    ParentColor = True
    TabOrder = 7
    UseDockManager = True
    OnClick = LbXisBClick
    DesignSize = (
      100
      48)
    object LbXisB: TGuiLabel
      Left = 8
      Top = 5
      Width = 85
      Height = 38
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      Caption = 'X is B'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -32
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnClick = LbXisBClick
    end
  end
  object PnSkip: TGuiPanel
    Left = 114
    Top = 137
    Width = 100
    Height = 48
    BorderColor = 2105376
    BorderRadius = 5.000000000000000000
    BorderWidth = 2.000000000000000000
    PanelColor = 10333885
    ParentColor = True
    TabOrder = 8
    UseDockManager = True
    OnClick = LbXisAClick
    DesignSize = (
      100
      48)
    object LbSkip: TGuiLabel
      Left = 7
      Top = 5
      Width = 85
      Height = 38
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Skip'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -32
      Font.Name = 'Tahoma'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 4.000000000000000000
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnClick = LbSkipClick
    end
  end
  object MainMenu: TMainMenu
    Left = 224
    Top = 4
    object MiTest: TMenuItem
      Caption = '&Test'
      object MiTestStart: TMenuItem
        Caption = 'Start'
        OnClick = MiTestFullGainReferenceClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MiTestTraining: TMenuItem
        Caption = '&Training'
        RadioItem = True
        object MiTestTrainingGain: TMenuItem
          Caption = '&Gain'
          OnClick = MiTestTrainingGainClick
        end
        object MiTestTrainingFrequency: TMenuItem
          Caption = '&Frequency'
          Enabled = False
          OnClick = MiTestTrainingFrequencyClick
        end
        object MiTestTrainingBandwidth: TMenuItem
          Caption = '&Bandwidth'
          Enabled = False
          OnClick = MiTestTrainingBandwidthClick
        end
      end
      object MiTestFull: TMenuItem
        Caption = '&Full'
        RadioItem = True
        object MiTestFullGain: TMenuItem
          Caption = '&Gain'
          object MiTestFullGainReference: TMenuItem
            Caption = '&Reference (1 kHz, 1 Oct.)'
            OnClick = MiTestFullGainReferenceClick
          end
          object MiTestFullGainNarrow: TMenuItem
            Caption = '&Narrow (1 kHz, 1/3 Oct.)'
            OnClick = MiTestFullGainNarrowClick
          end
          object MiTestFullGainWide: TMenuItem
            Caption = 'Wide (1 kHz, 3 Oct.)'
            OnClick = MiTestFullGainWideClick
          end
        end
        object MiTestFullFrequency: TMenuItem
          Caption = '&Frequency'
          Enabled = False
          object MiTestFullFrequencyReference: TMenuItem
            Caption = 'Reference'
          end
        end
        object MiTestFullBandwidth: TMenuItem
          Caption = '&Bandwidth'
          Enabled = False
          object MiTestFullBandwidthReference: TMenuItem
            Caption = 'Reference'
          end
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MiDecryptJNDfile: TMenuItem
        Caption = 'Decrypt JND file...'
        OnClick = MiDecryptJNDfileClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
    object MiSettings: TMenuItem
      Caption = '&Settings'
      object MiAudioSettings: TMenuItem
        Caption = '&Audio'
        OnClick = MiAudioSettingsClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MiLatchButtons: TMenuItem
        Caption = 'Latch Buttons'
        OnClick = MiLatchButtonsClick
      end
    end
  end
  object OD: TOpenDialog
    DefaultExt = '.mp3'
    Filter = 'MP3-File (*.mp3)|*.mp3'
    Title = 'Select an MP3 file'
    Left = 256
    Top = 4
  end
  object AsioHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBuffersCreate = AsioHostBuffersCreate
    OnBufferSwitch32 = AsioHostBufferSwitch32
    OnSampleRateChanged = AsioHostSampleRateChanged
    Left = 288
    Top = 4
  end
  object PeakCheck: TTimer
    Interval = 50
    OnTimer = PeakCheckTimer
    Left = 272
    Top = 152
  end
  object PuAudioFile: TPopupMenu
    Left = 192
    Top = 4
    object MiPinkNoise: TMenuItem
      Caption = '&Pink Noise'
      OnClick = MiPinkNoiseClick
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.JND'
    Filter = 'Just Noticable Difference Files (*.JND)|*.jnd'
    Left = 24
    Top = 160
  end
  object ResultButtonEnabler: TTimer
    Enabled = False
    Interval = 500
    OnTimer = ResultButtonEnablerTimer
    Left = 240
    Top = 152
  end
end
