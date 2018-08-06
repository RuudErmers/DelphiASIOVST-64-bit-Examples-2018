object FmStaticWaveformTest: TFmStaticWaveformTest
  Left = 218
  Top = 77
  Caption = 'Static Waveform Test'
  ClientHeight = 242
  ClientWidth = 215
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
  object StaticWaveformA: TGuiStaticWaveform
    Left = 8
    Top = 8
    Width = 96
    Height = 96
    DisplayChannels = 1
    LineColor = clBlue
    MedianColor = clBlack
  end
  object StaticWaveformB: TGuiStaticWaveform
    Left = 110
    Top = 8
    Width = 96
    Height = 96
    DisplayChannels = 1
    LineColor = clYellow
    MedianColor = clBlack
  end
  object StaticWaveformC: TGuiStaticWaveform
    Left = 8
    Top = 110
    Width = 96
    Height = 96
    DisplayChannels = 1
    LineColor = clLime
    MedianColor = clBlack
  end
  object StaticWaveformD: TGuiStaticWaveform
    Left = 110
    Top = 110
    Width = 96
    Height = 96
    DisplayChannels = 1
    LineColor = clRed
    MedianColor = clBlack
  end
  object CbTransparent: TCheckBox
    Left = 7
    Top = 212
    Width = 97
    Height = 17
    Caption = 'Transparent'
    TabOrder = 0
    OnClick = CbTransparentClick
  end
end
