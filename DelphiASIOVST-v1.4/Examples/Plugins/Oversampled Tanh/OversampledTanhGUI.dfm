object FmOversampledTanh: TFmOversampledTanh
  Left = 310
  Top = 168
  BorderStyle = bsNone
  Caption = 'Oversampled Tanh'
  ClientHeight = 36
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbNbrOfCoeff: TLabel
    Left = 8
    Top = 12
    Width = 33
    Height = 13
    Caption = 'Coeffs:'
  end
  object LbTransition: TLabel
    Left = 103
    Top = 12
    Width = 49
    Height = 13
    Caption = 'Transition:'
  end
  object CBCoeffs: TComboBox
    Left = 48
    Top = 8
    Width = 49
    Height = 21
    TabOrder = 0
    Text = '1'
    OnChange = CBCoeffsChange
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '12'
      '16'
      '24'
      '32')
  end
  object TBTransition: TTrackBar
    Left = 155
    Top = 8
    Width = 143
    Height = 21
    Max = 100
    Frequency = 10
    TabOrder = 1
    ThumbLength = 16
    OnChange = TBTransitionChange
  end
end
