object FmContinousExcitationSignalBuilder: TFmContinousExcitationSignalBuilder
  Left = 218
  Top = 77
  Caption = 'Continous Excitation Signal Builder'
  ClientHeight = 184
  ClientWidth = 416
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GbSignalType: TGroupBox
    Left = 8
    Top = 8
    Width = 137
    Height = 137
    Caption = 'Signal Type'
    TabOrder = 0
    object RbPureTone: TRadioButton
      Left = 8
      Top = 22
      Width = 67
      Height = 17
      Caption = 'Pure Tone'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RbPureToneClick
    end
    object RbThirdOctaveNoise: TRadioButton
      Left = 8
      Top = 45
      Width = 113
      Height = 17
      Caption = 'Third-Octave-Noise'
      TabOrder = 1
      OnClick = RbThirdOctaveNoiseClick
    end
    object RbOctaveNoise: TRadioButton
      Left = 8
      Top = 68
      Width = 86
      Height = 17
      Caption = 'Octave-Noise'
      TabOrder = 2
      OnClick = RbOctaveNoiseClick
    end
    object RbWhiteNoise: TRadioButton
      Left = 8
      Top = 91
      Width = 81
      Height = 17
      Caption = 'White Noise'
      TabOrder = 3
      OnClick = RbWhiteNoiseClick
    end
    object RbPinkNoise: TRadioButton
      Left = 8
      Top = 114
      Width = 67
      Height = 17
      Caption = 'Pink Noise'
      TabOrder = 4
      OnClick = RbPinkNoiseClick
    end
  end
  object GbFrequency: TGroupBox
    Left = 151
    Top = 8
    Width = 258
    Height = 72
    Caption = 'Frequency'
    TabOrder = 1
    DesignSize = (
      258
      72)
    object LbFrequencyUnit: TLabel
      Left = 161
      Top = 44
      Width = 12
      Height = 13
      Caption = 'Hz'
    end
    object TbFrequency: TTrackBar
      Left = 7
      Top = 18
      Width = 242
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Max = 10000000
      PopupMenu = PuFrequency
      Frequency = 1000000
      TabOrder = 0
      ThumbLength = 14
      OnChange = TbFrequencyChange
    end
    object EdFrequency: TEdit
      Left = 88
      Top = 41
      Width = 67
      Height = 21
      BiDiMode = bdLeftToRight
      ParentBiDiMode = False
      PopupMenu = PuFrequency
      TabOrder = 1
      Text = '1000'
      OnChange = EdFrequencyChange
      OnKeyUp = EdFrequencyKeyUp
      OnMouseLeave = EdFrequencyMouseLeave
    end
  end
  object GbOutput: TGroupBox
    Left = 151
    Top = 86
    Width = 258
    Height = 90
    Caption = 'Output'
    TabOrder = 2
    object LbFilename: TLabel
      Left = 7
      Top = 26
      Width = 46
      Height = 13
      Caption = 'Filename:'
    end
    object LbChannels: TLabel
      Left = 7
      Top = 59
      Width = 48
      Height = 13
      Caption = 'Channels:'
    end
    object LbLength: TLabel
      Left = 122
      Top = 59
      Width = 37
      Height = 13
      Caption = 'Length:'
    end
    object LbLengthUnit: TLabel
      Left = 236
      Top = 59
      Width = 13
      Height = 13
      Caption = 'ms'
    end
    object EdFileName: TEdit
      Left = 59
      Top = 23
      Width = 190
      Height = 21
      TabOrder = 0
      Text = 'EdFileName'
    end
    object SeChannels: TSpinEdit
      Left = 59
      Top = 56
      Width = 49
      Height = 22
      MaxValue = 16
      MinValue = 1
      TabOrder = 1
      Value = 2
      OnChange = SeChannelsChange
    end
    object SeLength: TSpinEdit
      Left = 165
      Top = 56
      Width = 65
      Height = 22
      MaxValue = 99999
      MinValue = 10
      TabOrder = 2
      Value = 15000
    end
  end
  object BtExit: TButton
    Left = 88
    Top = 151
    Width = 57
    Height = 25
    Caption = 'E&xit'
    TabOrder = 3
    OnClick = BtExitClick
  end
  object BtBuild: TButton
    Left = 8
    Top = 151
    Width = 74
    Height = 25
    Caption = '&Build'
    Default = True
    TabOrder = 4
    OnClick = BtBuildClick
  end
  object PuFrequency: TPopupMenu
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    Left = 192
    Top = 48
    object Mi20Hz: TMenuItem
      Tag = 20
      Caption = '20 Hz'
      OnClick = Mi20HzClick
    end
    object Mi25Hz: TMenuItem
      Tag = 25
      Caption = '25 Hz'
      OnClick = Mi25HzClick
    end
    object Mi31Hz5: TMenuItem
      Tag = 32
      Caption = '31.5 Hz'
      OnClick = Mi31Hz5Click
    end
    object Mi40Hz: TMenuItem
      Tag = 40
      Caption = '40 Hz'
      OnClick = Mi40HzClick
    end
    object Mi50Hz: TMenuItem
      Tag = 50
      Caption = '50 Hz'
      OnClick = Mi50HzClick
    end
    object Mi63Hz: TMenuItem
      Tag = 63
      Caption = '63 Hz'
      OnClick = Mi63HzClick
    end
    object Mi80Hz: TMenuItem
      Tag = 80
      Caption = '80 Hz'
      OnClick = Mi80HzClick
    end
    object Mi100Hz: TMenuItem
      Tag = 100
      Caption = '100 Hz'
      OnClick = Mi100HzClick
    end
    object Mi125Hz: TMenuItem
      Tag = 125
      Caption = '125 Hz'
      OnClick = Mi125HzClick
    end
    object Mi160Hz: TMenuItem
      Tag = 160
      Caption = '160 Hz'
      OnClick = Mi160HzClick
    end
    object Mi200Hz: TMenuItem
      Tag = 200
      Caption = '200 Hz'
      OnClick = Mi200HzClick
    end
    object Mi250Hz: TMenuItem
      Tag = 250
      Caption = '250 Hz'
      OnClick = Mi250HzClick
    end
    object Mi315Hz: TMenuItem
      Tag = 315
      Caption = '315 Hz'
      OnClick = Mi315HzClick
    end
    object Mi400Hz: TMenuItem
      Tag = 400
      Caption = '400 Hz'
      OnClick = Mi400HzClick
    end
    object Mi500Hz: TMenuItem
      Tag = 500
      Caption = '500 Hz'
      OnClick = Mi500HzClick
    end
    object Mi630Hz: TMenuItem
      Tag = 630
      Caption = '630 Hz'
      OnClick = Mi630HzClick
    end
    object Mi800Hz: TMenuItem
      Tag = 800
      Caption = '800 Hz'
      OnClick = Mi800HzClick
    end
    object Mi1kHz: TMenuItem
      Tag = 1000
      Caption = '1 kHz'
      OnClick = Mi1kHzClick
    end
    object Mi1k25Hz: TMenuItem
      Tag = 1250
      Caption = '1.25 kHz'
      OnClick = Mi1k25HzClick
    end
    object Mi1k6Hz: TMenuItem
      Tag = 1600
      Caption = '1.6 kHz'
      OnClick = Mi1k6HzClick
    end
    object Mi2kHz: TMenuItem
      Tag = 2000
      Caption = '2 kHz'
      OnClick = Mi2kHzClick
    end
    object Mi2k5Hz: TMenuItem
      Tag = 2500
      Caption = '2.5 kHz'
      OnClick = Mi2k5HzClick
    end
    object Mi31k5Hz: TMenuItem
      Tag = 3150
      Caption = '3.15 kHz'
      OnClick = Mi31k5HzClick
    end
    object Mi4kHz: TMenuItem
      Tag = 4000
      Caption = '4 kHz'
      OnClick = Mi4kHzClick
    end
    object Mi5kHz: TMenuItem
      Tag = 5000
      Caption = '5 kHz'
      OnClick = Mi5kHzClick
    end
    object Mi6k3Hz: TMenuItem
      Tag = 6300
      Caption = '6.3 kHz'
      OnClick = Mi6k3HzClick
    end
    object Mi8kHz: TMenuItem
      Tag = 8000
      Caption = '8 kHz'
      OnClick = Mi8kHzClick
    end
    object Mi10kHz: TMenuItem
      Tag = 10000
      Caption = '10 kHz'
      OnClick = Mi10kHzClick
    end
    object Mi12k5Hz: TMenuItem
      Tag = 12500
      Caption = '12.5 kHz'
      OnClick = Mi12k5HzClick
    end
    object Mi16kHz: TMenuItem
      Tag = 16000
      Caption = '16 kHz'
      OnClick = Mi16kHzClick
    end
    object Mi20kHz: TMenuItem
      Tag = 20000
      Caption = '20 kHz'
      OnClick = Mi20kHzClick
    end
  end
  object AudioDataCollection: TAudioDataCollection32
    Channels = <>
    SampleRate = 44100.000000000000000000
    Left = 160
    Top = 112
  end
end
