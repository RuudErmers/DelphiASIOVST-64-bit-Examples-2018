object FmPerformanceTest: TFmPerformanceTest
  Left = 299
  Top = 51
  Caption = 'Performance Test'
  ClientHeight = 291
  ClientWidth = 463
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object MeResults: TMemo
    Left = 0
    Top = 39
    Width = 463
    Height = 252
    Align = alBottom
    TabOrder = 0
  end
  object BtStartTest: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start Tests'
    TabOrder = 1
    OnClick = BtStartTestClick
  end
end
