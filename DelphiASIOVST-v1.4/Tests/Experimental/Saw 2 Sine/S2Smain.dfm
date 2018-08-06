object FmMain: TFmMain
  Left = 218
  Top = 77
  Caption = 'Saw 2 Sine'
  ClientHeight = 436
  ClientWidth = 320
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
  object GuiGraphXY: TGuiGraphXY
    Left = 0
    Top = 0
    Width = 320
    Height = 320
    BorderColor = clBlack
    BorderRadius = 4
    FrameColor = clGray
    SeriesCollection = <
      item
        DisplayName = 'TGuiGraphXYSeriesCollectionItem'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
      end
      item
        DisplayName = 'TGuiGraphXYSeriesCollectionItem'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
        Series.Color = clBlue
      end
      item
        DisplayName = 'TGuiGraphXYSeriesCollectionItem'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
        Series.Color = clLime
      end>
    XAxis.Granularity = 0.500000000000000000
    XAxis.Minimum = -1.000000000000000000
    XAxis.Maximum = 1.000000000000000000
    XAxis.Lower = -1.000000000000000000
    XAxis.Upper = 1.000000000000000000
    YAxis.Granularity = 0.500000000000000000
    YAxis.Minimum = -1.000000000000000000
    YAxis.Maximum = 1.000000000000000000
    YAxis.Lower = -1.000000000000000000
    YAxis.Upper = 1.000000000000000000
    Align = alTop
    LineColor = clGray
    LineWidth = 2
  end
  object BtOptimize: TButton
    Left = 8
    Top = 326
    Width = 75
    Height = 25
    Caption = '&Optimize'
    Default = True
    TabOrder = 0
    OnClick = BtOptimizeClick
  end
  object Memo1: TMemo
    Left = 89
    Top = 326
    Width = 223
    Height = 102
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
end
