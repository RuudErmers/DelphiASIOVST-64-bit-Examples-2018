object FmFunctionPlot: TFmFunctionPlot
  Left = 218
  Top = 80
  Caption = 'Simple Function Plotter'
  ClientHeight = 342
  ClientWidth = 410
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
  object GuiGraphXY: TGuiGraphXY
    Left = 0
    Top = 0
    Width = 410
    Height = 342
    FrameColor = clBlack
    SeriesCollection = <
      item
        DisplayName = 'Y = X'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
      end>
    XAxis.Granularity = 1.000000000000000000
    XAxis.Minimum = -5.000000000000000000
    XAxis.Maximum = 5.000000000000000000
    XAxis.Lower = -5.000000000000000000
    XAxis.Upper = 5.000000000000000000
    YAxis.Granularity = 1.000000000000000000
    YAxis.Minimum = -5.000000000000000000
    YAxis.Maximum = 5.000000000000000000
    YAxis.Lower = -5.000000000000000000
    YAxis.Upper = 5.000000000000000000
    Align = alClient
    LineColor = clGray
    LineWidth = 2
  end
end
