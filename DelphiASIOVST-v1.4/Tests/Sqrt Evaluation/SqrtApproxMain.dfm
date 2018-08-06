object FmSqrtApproximation: TFmSqrtApproximation
  Left = 219
  Top = 63
  Caption = 'Evaluation of 2^x'
  ClientHeight = 325
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GuiGraphXY: TGuiGraphXY
    Left = 0
    Top = 0
    Width = 436
    Height = 325
    SeriesCollection = <
      item
        DisplayName = 'Exp'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
      end
      item
        DisplayName = 'FastExp'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
        Series.Color = clBlue
      end>
    XAxis.Granularity = 10.000000000000000000
    XAxis.Maximum = 100.000000000000000000
    XAxis.Upper = 100.000000000000000000
    YAxis.Granularity = 2.000000000000000000
    YAxis.Minimum = -10.000000000000000000
    YAxis.Maximum = 10.000000000000000000
    YAxis.Lower = -10.000000000000000000
    YAxis.Upper = 10.000000000000000000
    Align = alClient
    AntiAlias = gaaLinear3x
    LineColor = clMaroon
    LineWidth = 2
    ExplicitWidth = 433
    ExplicitHeight = 257
  end
end
