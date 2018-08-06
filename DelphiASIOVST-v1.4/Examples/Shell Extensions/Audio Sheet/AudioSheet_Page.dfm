object FmPage: TFmPage
  Left = 372
  Top = 223
  BorderStyle = bsNone
  Caption = 'Audio Sheet'
  ClientHeight = 405
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PCInfo: TPageControl
    Left = 8
    Top = 8
    Width = 324
    Height = 134
    ActivePage = TSBasicInformation
    TabOrder = 0
    object TSBasicInformation: TTabSheet
      Caption = 'Basic Information'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      object LbSamplerate: TLabel
        Left = 22
        Top = 9
        Width = 110
        Height = 17
        AutoSize = False
        Caption = 'Samplerate:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object LbSamplerateValue: TLabel
        Left = 173
        Top = 9
        Width = 135
        Height = 16
        AutoSize = False
        Caption = '44100 Hz'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object LbSampleLength: TLabel
        Left = 22
        Top = 30
        Width = 110
        Height = 17
        AutoSize = False
        Caption = 'Signal Length:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object LbSampleLengthValue: TLabel
        Left = 173
        Top = 30
        Width = 135
        Height = 16
        AutoSize = False
        Caption = '16384 Sample'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object LbChannels: TLabel
        Left = 22
        Top = 51
        Width = 110
        Height = 17
        AutoSize = False
        Caption = 'Channels:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object LbChannelsValue: TLabel
        Left = 174
        Top = 51
        Width = 135
        Height = 16
        AutoSize = False
        Caption = '2 Channels'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object LbBitsPerSampleValue: TLabel
        Left = 174
        Top = 72
        Width = 135
        Height = 16
        AutoSize = False
        Caption = '32 Bit Float'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object LbBitsPerSample: TLabel
        Left = 22
        Top = 72
        Width = 111
        Height = 17
        AutoSize = False
        Caption = 'Bits Per Sample:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
    end
    object TSSignalProperties: TTabSheet
      Caption = 'Signal Properties'
      ImageIndex = 2
      object SignalMemo: TMemo
        Left = 0
        Top = 0
        Width = 316
        Height = 106
        Align = alClient
        BorderStyle = bsNone
        Color = clBtnFace
        Lines.Strings = (
          'Memo')
        ReadOnly = True
        TabOrder = 0
      end
    end
  end
  object ADD: TGuiAudioDataDisplay
    Left = 8
    Top = 148
    Width = 324
    Height = 249
    AudioDataCollection = ADC
    DisplayChannels = <>
    LineWidth = 0
    XAxis.SampleUpper = 127
    XAxis.FractionalLower = -0.500000000000000000
    XAxis.FractionalUpper = 0.500000000000000000
  end
  object ADC: TAudioDataCollection32
    Channels = <
      item
        DisplayName = 'Channel 1'
      end>
    SampleFrames = 128
    SampleRate = 44100.000000000000000000
    Left = 88
    Top = 184
  end
end
