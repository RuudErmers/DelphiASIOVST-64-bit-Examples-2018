object FmAudioDataDisplay: TFmAudioDataDisplay
  Left = 218
  Top = 77
  Caption = 'Audio Data Display'
  ClientHeight = 283
  ClientWidth = 335
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
  object PC: TPageControl
    Left = 0
    Top = 0
    Width = 335
    Height = 283
    ActivePage = Axis
    Align = alClient
    TabOrder = 0
    object TsBasic: TTabSheet
      Caption = 'Basic Drawing'
      object LbLineWidth: TLabel
        Left = 16
        Top = 168
        Width = 54
        Height = 13
        Caption = 'Line Width:'
        Transparent = True
      end
      object ADD1: TGuiAudioDataDisplay
        Left = 16
        Top = 0
        Width = 128
        Height = 64
        AudioDataCollection = ADC
        DisplayChannels = <>
        LineColor = clBlue
        LineWidth = 0
        Normalize = False
        XAxis.SampleUpper = 511
        XAxis.FractionalLower = -0.500000000000000000
        XAxis.FractionalUpper = 0.500000000000000000
      end
      object TbLineWidth: TTrackBar
        Left = 76
        Top = 168
        Width = 202
        Height = 19
        Max = 5
        Min = 1
        Position = 1
        TabOrder = 1
        ThumbLength = 12
        OnChange = TbLineWidthChange
      end
      object CbTransparent: TCheckBox
        Left = 16
        Top = 140
        Width = 97
        Height = 17
        Caption = 'Transparent'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = CbTransparentClick
      end
      object ADD4: TGuiAudioDataDisplay
        Left = 150
        Top = 70
        Width = 128
        Height = 64
        AntiAlias = gaaLinear4x
        AudioDataCollection = ADC
        DisplayChannels = <>
        LineColor = clYellow
        LineWidth = 0
        Normalize = False
        XAxis.SampleUpper = 511
        XAxis.FractionalLower = -0.500000000000000000
        XAxis.FractionalUpper = 0.500000000000000000
      end
      object ADD3: TGuiAudioDataDisplay
        Left = 16
        Top = 70
        Width = 128
        Height = 64
        AntiAlias = gaaLinear3x
        AudioDataCollection = ADC
        DisplayChannels = <>
        LineColor = clRed
        LineWidth = 0
        Normalize = False
        XAxis.SampleUpper = 511
        XAxis.FractionalLower = -0.500000000000000000
        XAxis.FractionalUpper = 0.500000000000000000
      end
      object ADD2: TGuiAudioDataDisplay
        Left = 150
        Top = 0
        Width = 128
        Height = 64
        AntiAlias = gaaLinear2x
        AudioDataCollection = ADC
        DisplayChannels = <>
        LineColor = clLime
        LineWidth = 0
        Normalize = False
        XAxis.SampleUpper = 511
        XAxis.FractionalLower = -0.500000000000000000
        XAxis.FractionalUpper = 0.500000000000000000
      end
    end
    object Axis: TTabSheet
      Caption = 'Axis'
      ImageIndex = 1
      object GuiAudioDataDisplay1: TGuiAudioDataDisplay
        Left = 16
        Top = 16
        Width = 297
        Height = 153
        AudioDataCollection = ADC
        DisplayChannels = <>
        LineColor = clBlue
        LineWidth = 0
        Normalize = False
        XAxis.SampleUpper = 511
        XAxis.FractionalLower = -0.500000000000000000
        XAxis.FractionalUpper = 0.500000000000000000
      end
    end
  end
  object ADC: TAudioDataCollection32
    Channels = <
      item
        DisplayName = 'Channel 1'
      end>
    SampleFrames = 512
    SampleRate = 44100.000000000000000000
    Left = 72
    Top = 152
  end
end
