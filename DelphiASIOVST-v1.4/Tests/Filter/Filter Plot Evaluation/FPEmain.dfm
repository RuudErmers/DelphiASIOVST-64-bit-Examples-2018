object Form1: TForm1
  Left = 218
  Top = 77
  Caption = 'Form1'
  ClientHeight = 330
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GuiEQGraph: TGuiEQGraph
    Left = 0
    Top = 0
    Width = 472
    Height = 330
    ColorChart = clBtnFace
    FilterSeries = <
      item
        DisplayName = 'TGuiEQGraphSeriesCollectionItem'
        OnGetFilterGain = GetFilterGain
      end>
    XAxis.UpperFrequency = 20000.000000000000000000
    XAxis.LowerFrequency = 20.000000000000000000
    YAxis.LowerLevel = -36.000000000000000000
    YAxis.UpperLevel = 6.000000000000000000
    YAxis.Granularity = 5.000000000000000000
    Align = alClient
    Color = clBtnFace
    ParentColor = False
  end
end
