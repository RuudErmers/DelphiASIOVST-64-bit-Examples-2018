object FrAnalysisThirdOctave: TFrAnalysisThirdOctave
  Left = 0
  Top = 0
  Width = 343
  Height = 252
  TabOrder = 0
  DesignSize = (
    343
    252)
  object GbBandSeparation: TGroupBox
    Left = 3
    Top = 3
    Width = 337
    Height = 246
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Band Separation'
    TabOrder = 0
    object LbType: TLabel
      Left = 11
      Top = 22
      Width = 28
      Height = 13
      Caption = 'Type:'
    end
    object RbFilter: TRadioButton
      Left = 45
      Top = 21
      Width = 44
      Height = 17
      Caption = 'Filter'
      TabOrder = 0
    end
    object RbFFT: TRadioButton
      Left = 95
      Top = 21
      Width = 44
      Height = 17
      Caption = 'FFT'
      TabOrder = 1
    end
  end
end
