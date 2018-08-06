object FmAdsrTest: TFmAdsrTest
  Left = 218
  Top = 77
  Caption = 'ADSR Test'
  ClientHeight = 168
  ClientWidth = 188
  Color = 7373965
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClick = FormClick
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object ADSRGraph: TGuiADSRGraph
    Left = 8
    Top = 16
    Width = 169
    Height = 89
    ADSRSettings.Attack = 0.500000000000000000
    ADSRSettings.Decay = 0.500000000000000000
    ADSRSettings.Sustain = 0.500000000000000000
    ADSRSettings.Release = 0.500000000000000000
    GridWidth = 3
  end
end
