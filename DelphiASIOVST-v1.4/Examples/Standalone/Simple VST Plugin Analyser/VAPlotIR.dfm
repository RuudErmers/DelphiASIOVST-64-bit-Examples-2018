object FmPlotIR: TFmPlotIR
  Left = 554
  Top = 554
  Caption = 'Impulse Resonse Plot'
  ClientHeight = 154
  ClientWidth = 387
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Waveform: TGuiStaticWaveform
    Left = 0
    Top = 0
    Width = 387
    Height = 154
    Align = alClient
    DisplayChannels = 1
    MedianColor = clLime
    MedianLineWidth = 3
    NormalizationType = ntOverallChannels
    PopupMenu = PUDisplay
  end
  object PUDisplay: TPopupMenu
    Left = 192
    Top = 24
    object MIWaveform: TMenuItem
      Caption = 'Waveform'
      Checked = True
      RadioItem = True
    end
  end
end
