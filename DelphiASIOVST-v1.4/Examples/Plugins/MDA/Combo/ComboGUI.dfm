object FmCombo: TFmCombo
  Left = 397
  Top = 347
  BorderStyle = bsNone
  Caption = 'Combo'
  ClientHeight = 142
  ClientWidth = 291
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LbModel: TLabel
    Left = 8
    Top = 8
    Width = 32
    Height = 13
    Caption = 'Model:'
  end
  object LbDrive: TLabel
    Left = 8
    Top = 33
    Width = 29
    Height = 13
    Caption = 'Drive:'
  end
  object LbBias: TLabel
    Left = 8
    Top = 56
    Width = 23
    Height = 13
    Caption = 'Bias:'
  end
  object LbOutput: TLabel
    Left = 8
    Top = 78
    Width = 38
    Height = 13
    Caption = 'Output:'
  end
  object LbFreq: TLabel
    Left = 8
    Top = 100
    Width = 55
    Height = 13
    Caption = 'Frequency:'
  end
  object LbReso: TLabel
    Left = 8
    Top = 122
    Width = 57
    Height = 13
    Caption = 'Resonance:'
  end
  object LbResonanceValue: TLabel
    Left = 234
    Top = 122
    Width = 16
    Height = 13
    Caption = '0.0'
  end
  object LbFrequencyValue: TLabel
    Left = 234
    Top = 100
    Width = 16
    Height = 13
    Caption = '0.0'
  end
  object LbOutputValue: TLabel
    Left = 234
    Top = 78
    Width = 16
    Height = 13
    Caption = '0.0'
  end
  object LbBiasValue: TLabel
    Left = 234
    Top = 56
    Width = 16
    Height = 13
    Caption = '0.0'
  end
  object LbDriveValue: TLabel
    Left = 234
    Top = 33
    Width = 16
    Height = 13
    Caption = '0.0'
  end
  object CBModel: TComboBox
    Left = 52
    Top = 5
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 1
    TabOrder = 0
    Text = 'Speaker Sim'
    OnChange = CBModelChange
    Items.Strings = (
      'D.I.'
      'Speaker Sim'
      'Radio'
      'MB 1"'
      'MB 8"'
      '4x12 ^'
      '4x12 >')
  end
  object SBDrive: TScrollBar
    Left = 69
    Top = 32
    Width = 159
    Height = 16
    Max = 1000
    Min = -1000
    PageSize = 0
    TabOrder = 1
    OnChange = SBDriveChange
  end
  object SBBias: TScrollBar
    Left = 69
    Top = 54
    Width = 159
    Height = 16
    Max = 1000
    Min = -1000
    PageSize = 0
    TabOrder = 2
    OnChange = SBBiasChange
  end
  object RBMono: TRadioButton
    Left = 179
    Top = 8
    Width = 49
    Height = 17
    Caption = 'Mono'
    Checked = True
    TabOrder = 3
    TabStop = True
    OnClick = RBMonoClick
  end
  object RBStereo: TRadioButton
    Left = 234
    Top = 8
    Width = 58
    Height = 17
    Caption = 'Stereo'
    TabOrder = 4
    OnClick = RBStereoClick
  end
  object SBOutput: TScrollBar
    Left = 69
    Top = 76
    Width = 159
    Height = 16
    Max = 200
    Min = -200
    PageSize = 0
    TabOrder = 5
    OnChange = SBOutputChange
  end
  object SBFreq: TScrollBar
    Left = 69
    Top = 98
    Width = 159
    Height = 16
    Max = 1000
    PageSize = 0
    TabOrder = 6
    OnChange = SBFreqChange
  end
  object SBReso: TScrollBar
    Left = 69
    Top = 120
    Width = 159
    Height = 16
    Max = 1000
    PageSize = 0
    TabOrder = 7
    OnChange = SBResoChange
  end
end
