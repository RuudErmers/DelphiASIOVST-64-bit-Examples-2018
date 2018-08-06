object FmApproximationBenchmark: TFmApproximationBenchmark
  Left = 286
  Top = 77
  Caption = 'Approximation Benchmark'
  ClientHeight = 304
  ClientWidth = 670
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 281
    Top = 0
    Height = 304
  end
  object GuiGraphXY: TGuiGraphXY
    Left = 284
    Top = 0
    Width = 386
    Height = 304
    BorderColor = clMaroon
    BorderWidth = 1.500000000000000000
    GridColor = clBlack
    SeriesCollection = <
      item
        DisplayName = 'TGuiGraphXYSeriesCollectionItem'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
        Series.ShadeAlpha = 0
        Series.Width = 1.000000000000000000
      end
      item
        DisplayName = 'TGuiGraphXYSeriesCollectionItem'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
        Series.Color = clBlue
        Series.ShadeAlpha = 0
        Series.Width = 1.000000000000000000
      end>
    XAxis.Granularity = 1.000000000000000000
    XAxis.Minimum = -5.000000000000000000
    XAxis.Maximum = 5.000000000000000000
    XAxis.Lower = -5.000000000000000000
    XAxis.Upper = 5.000000000000000000
    YAxis.Granularity = 0.050000000000000000
    YAxis.Minimum = -0.100000000000000000
    YAxis.Maximum = 0.100000000000000000
    YAxis.Lower = -0.100000000000000000
    YAxis.Upper = 0.100000000000000000
    Align = alClient
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 281
    Height = 304
    Align = alLeft
    TabOrder = 0
    OnClick = MemoClick
  end
  object MainMenu: TMainMenu
    Left = 16
    Top = 8
    object MIFile: TMenuItem
      Caption = '&File'
      object MISaveLog: TMenuItem
        Caption = 'Save Log...'
        OnClick = MISaveLogClick
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object MIExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MIExitClick
      end
    end
    object MIBenchmark: TMenuItem
      Caption = '&Benchmark'
      object MICos: TMenuItem
        Caption = 'Cos(x)'
        object MiCosAll: TMenuItem
          Caption = '&All'
          OnClick = MiCosAllClick
        end
        object N11: TMenuItem
          Caption = '-'
        end
        object MIFastCos3Term: TMenuItem
          Caption = 'Complete &3 Term'
          OnClick = MIFastCos3TermClick
        end
        object MIFastCos4Term: TMenuItem
          Caption = 'Complete &4 Term'
          OnClick = MIFastCos4TermClick
        end
        object MIFastCos5Term: TMenuItem
          Caption = 'Complete &5 Term'
          OnClick = MIFastCos5TermClick
        end
        object MIFastCos6Term: TMenuItem
          Caption = 'Complete &6 Term'
          OnClick = MIFastCos6TermClick
        end
        object N1: TMenuItem
          Caption = '-'
        end
        object MIInBounds3Term: TMenuItem
          Caption = 'In bounds &3 Term'
          OnClick = MICosInBounds3TermClick
        end
        object MICosInBounds4Term: TMenuItem
          Caption = 'In bounds &4 Term'
          OnClick = MICosInBounds4TermClick
        end
        object MICosInBounds5Term: TMenuItem
          Caption = 'In bounds &5 Term'
          OnClick = MICosInBounds5TermClick
        end
        object MICosInBounds6Term: TMenuItem
          Caption = 'In bounds &6 Term'
          OnClick = MICosInBounds6TermClick
        end
      end
      object MISin: TMenuItem
        Caption = 'Sin(x)'
        object MiSinAll: TMenuItem
          Caption = '&All'
          OnClick = MiSinAllClick
        end
        object N12: TMenuItem
          Caption = '-'
        end
        object MISinComplete3Term: TMenuItem
          Caption = 'Complete &3 Term'
          OnClick = MISinComplete3TermClick
        end
        object MISinComplete4Term: TMenuItem
          Caption = 'Complete &4 Term'
          OnClick = MISinComplete4TermClick
        end
        object MISinComplete5Term: TMenuItem
          Caption = 'Complete &5 Term'
          OnClick = MISinComplete5TermClick
        end
        object MISinComplete6Term: TMenuItem
          Caption = 'Complete &6 Term'
          OnClick = MISinComplete6TermClick
        end
        object N3: TMenuItem
          Caption = '-'
        end
        object MISinInbounds3Term: TMenuItem
          Caption = 'In bounds &3 Term'
          OnClick = MISinInbounds3TermClick
        end
        object MISinInbounds4Term: TMenuItem
          Caption = 'In bounds &4 Term'
          OnClick = MISinInbounds4TermClick
        end
        object MISinInbounds5Term: TMenuItem
          Caption = 'In bounds &5 Term'
          OnClick = MISinInbounds5TermClick
        end
        object MISinInbounds6Term: TMenuItem
          Caption = 'In bounds &6 Term'
          OnClick = MISinInbounds6TermClick
        end
      end
      object MITan: TMenuItem
        Caption = 'Tan(x)'
        object MiTanAll: TMenuItem
          Caption = 'All'
          OnClick = MiTanAllClick
        end
        object N13: TMenuItem
          Caption = '-'
        end
        object MITanComplete2Term: TMenuItem
          Caption = 'Complete &2 Term'
          OnClick = MITanComplete2TermClick
        end
        object MITanComplete3Term: TMenuItem
          Caption = 'Complete &3 Term'
          OnClick = MITanComplete3TermClick
        end
        object MITanComplete4Term: TMenuItem
          Caption = 'Complete &4 Term'
          OnClick = MITanComplete4TermClick
        end
        object MITanComplete6Term: TMenuItem
          Caption = 'Complete &6 Term'
          OnClick = MITanComplete6TermClick
        end
        object N2: TMenuItem
          Caption = '-'
        end
        object MITanInbounds2Term: TMenuItem
          Caption = 'In bounds &2 Term'
          OnClick = MITanInbounds2TermClick
        end
        object MITanInbounds3Term: TMenuItem
          Caption = 'In bounds &3 Term'
          OnClick = MITanInbounds3TermClick
        end
        object MITanInbounds4Term: TMenuItem
          Caption = 'In bounds &4 Term'
          OnClick = MITanInbounds4TermClick
        end
        object MITanInbounds6Term: TMenuItem
          Caption = 'In bounds &6 Term'
          OnClick = MITanInbounds6TermClick
        end
      end
      object MITanh: TMenuItem
        Caption = 'Tanh(x)'
        object MiTanhAll: TMenuItem
          Caption = 'All'
          OnClick = MiTanhAllClick
        end
        object N14: TMenuItem
          Caption = '-'
        end
        object MITanhComplete3Term: TMenuItem
          Caption = 'Rational Polynom &3 Term'
          OnClick = MITanhComplete3TermClick
        end
        object MITanhComplete4Term: TMenuItem
          Caption = 'Rational Polynom &4 Term'
          OnClick = MITanhComplete4TermClick
        end
        object MITanhComplete5Term: TMenuItem
          Caption = 'Rational Polynom &5 Term'
          OnClick = MITanhComplete5TermClick
        end
        object MITanhComplete6Term: TMenuItem
          Caption = 'Rational Polynom &6 Term'
          OnClick = MITanhComplete6TermClick
        end
        object MITanhComplete7Term: TMenuItem
          Caption = 'Rational Polynom &7 Term'
          OnClick = MITanhComplete7TermClick
        end
        object N4: TMenuItem
          Caption = '-'
        end
        object MITanhRationalPolynom3TermFPU: TMenuItem
          Caption = 'Rational Polynom &3 Term (FPU)'
          OnClick = MITanhRationalPolynom3TermFPUClick
        end
        object MITanhRationalPolynom4TermFPU: TMenuItem
          Caption = 'Rational Polynom &4 Term (FPU)'
          OnClick = MITanhRationalPolynom4TermFPUClick
        end
        object MITanhRationalPolynom5TermFPU: TMenuItem
          Caption = 'Rational Polynom &5 Term (FPU)'
          OnClick = MITanhRationalPolynom5TermFPUClick
        end
        object MITanhRationalPolynom6TermFPU: TMenuItem
          Caption = 'Rational Polynom &6 Term (FPU)'
          OnClick = MITanhRationalPolynom6TermFPUClick
        end
        object MITanhRationalPolynom7TermFPU: TMenuItem
          Caption = 'Rational Polynom &7 Term (FPU)'
          OnClick = MITanhRationalPolynom7TermFPUClick
        end
        object N5: TMenuItem
          Caption = '-'
        end
        object MITanhExp2TermMinError: TMenuItem
          Caption = 'Exp. &2 Term (min. error)'
          OnClick = MITanhExp2TermMinErrorClick
        end
        object MITanhExp2TermContError: TMenuItem
          Caption = 'Exp. &2 Term (cont. error)'
          OnClick = MITanhExp2TermContErrorClick
        end
        object MITanhExp3TermMinError: TMenuItem
          Caption = 'Exp. &3 Term (min. error)'
          OnClick = MITanhExp3TermMinErrorClick
        end
        object MITanhExp3TermContError: TMenuItem
          Caption = 'Exp. &3 Term (cont. error)'
          OnClick = MITanhExp3TermContErrorClick
        end
        object MITanhExp4TermMinError: TMenuItem
          Caption = 'Exp. &4 Term (min. error)'
          OnClick = MITanhExp4TermMinErrorClick
        end
        object MITanhExp4TermContError: TMenuItem
          Caption = 'Exp. &4 Term (cont. error)'
          OnClick = MITanhExp4TermContErrorClick
        end
        object MITanhExp5TermMinError: TMenuItem
          Caption = 'Exp. &5 Term (min. error)'
          OnClick = MITanhExp5TermMinErrorClick
        end
        object MITanhExp5TermContError: TMenuItem
          Caption = 'Exp. &5 Term (cont. error)'
          OnClick = MITanhExp5TermContErrorClick
        end
      end
      object MILog2: TMenuItem
        Caption = 'Log2(x)'
        OnClick = MILog2Click
        object MiLog2All: TMenuItem
          Caption = 'All'
          OnClick = MiLog2AllClick
        end
        object N15: TMenuItem
          Caption = '-'
        end
        object MILog2MinError2Term: TMenuItem
          Caption = 'min. Error &2 Term'
          OnClick = MILog2MinError2TermClick
        end
        object MILog2MinError3Term: TMenuItem
          Caption = 'min. Error &3 Term'
          OnClick = MILog2MinError3TermClick
        end
        object MILog2MinError4Term: TMenuItem
          Caption = 'min. Error &4 Term'
          OnClick = MILog2MinError4TermClick
        end
        object MILog2MinError5Term: TMenuItem
          Caption = 'min. Error &5 Term'
          OnClick = MILog2MinError5TermClick
        end
        object N6: TMenuItem
          Caption = '-'
        end
        object MILog2ContError2Term: TMenuItem
          Caption = 'cont. Error &2 Term'
          OnClick = MILog2ContError2TermClick
        end
        object MILog2ContError3Term: TMenuItem
          Caption = 'cont. Error &3 Term'
          OnClick = MILog2ContError3TermClick
        end
        object MILog2ContError4Term: TMenuItem
          Caption = 'cont. Error &4 Term'
          OnClick = MILog2ContError4TermClick
        end
        object MILog2ContError5Term: TMenuItem
          Caption = 'cont. Error &5 Term'
          OnClick = MILog2ContError5TermClick
        end
      end
      object MILn: TMenuItem
        Caption = 'Ln(x)'
        object MiLnAll: TMenuItem
          Caption = '&All'
          OnClick = MiLnAllClick
        end
        object N16: TMenuItem
          Caption = '-'
        end
        object MILnMinError2Term: TMenuItem
          Caption = 'min. Error &2 Term'
          OnClick = MILnMinError2TermClick
        end
        object MILnMinError3Term: TMenuItem
          Caption = 'min. Error &3 Term'
          OnClick = MILnMinError3TermClick
        end
        object MILnMinError4Term: TMenuItem
          Caption = 'min. Error &4 Term'
          OnClick = MILnMinError4TermClick
        end
        object MILnMinError5Term: TMenuItem
          Caption = 'min. Error &5 Term'
          OnClick = MILnMinError5TermClick
        end
        object N10: TMenuItem
          Caption = '-'
        end
        object MILnContError2Term: TMenuItem
          Caption = 'cont. Error &2 Term'
          OnClick = MILnContError2TermClick
        end
        object MILnContError3Term: TMenuItem
          Caption = 'cont. Error &3 Term'
          OnClick = MILnContError3TermClick
        end
        object MILnContError4Term: TMenuItem
          Caption = 'cont. Error &4 Term'
          OnClick = MILnContError4TermClick
        end
        object MILnContError5Term: TMenuItem
          Caption = 'cont. Error &5 Term'
          OnClick = MILnContError5TermClick
        end
      end
      object MIPower2: TMenuItem
        Caption = '2^(x)'
        object MiPower2All: TMenuItem
          Caption = '&All'
          OnClick = MiPower2AllClick
        end
        object N17: TMenuItem
          Caption = '-'
        end
        object MIPower2MinError2Term: TMenuItem
          Caption = 'min. Error &2 Term'
          OnClick = MIPower2MinError2TermClick
        end
        object MIPower2MinError3Term: TMenuItem
          Caption = 'min. Error &3 Term'
          OnClick = MIPower2MinError3TermClick
        end
        object MIPower2MinError4Term: TMenuItem
          Caption = 'min. Error &4 Term'
          OnClick = MIPower2MinError4TermClick
        end
        object MIPower2MinError5Term: TMenuItem
          Caption = 'min. Error &5 Term'
          OnClick = MIPower2MinError5TermClick
        end
        object N7: TMenuItem
          Caption = '-'
        end
        object MIPower2ContError2Term: TMenuItem
          Caption = 'cont. Error &2 Term'
          OnClick = MIPower2ContError2TermClick
        end
        object MIPower2ContError3Term: TMenuItem
          Caption = 'cont. Error &3 Term'
          OnClick = MIPower2ContError3TermClick
        end
        object MIPower2ContError4Term: TMenuItem
          Caption = 'cont. Error &4 Term'
          OnClick = MIPower2ContError4TermClick
        end
        object MIPower2ContError5Term: TMenuItem
          Caption = 'cont. Error &5 Term'
          OnClick = MIPower2ContError5TermClick
        end
      end
      object MIExp: TMenuItem
        Caption = 'Exp(x)'
        object MiExpAll: TMenuItem
          Caption = '&All'
          OnClick = MiExpAllClick
        end
        object N18: TMenuItem
          Caption = '-'
        end
        object MIExpMinError2Term: TMenuItem
          Caption = 'min. Error &2 Term'
          OnClick = MIExpMinError2TermClick
        end
        object MIExpMinError3Term: TMenuItem
          Caption = 'min. Error &3 Term'
          OnClick = MIExpMinError3TermClick
        end
        object MIExpMinError4Term: TMenuItem
          Caption = 'min. Error &4 Term'
          OnClick = MIExpMinError4TermClick
        end
        object MIExpMinError5Term: TMenuItem
          Caption = 'min. Error &5 Term'
          OnClick = MIExpMinError5TermClick
        end
        object N9: TMenuItem
          Caption = '-'
        end
        object MIExpContError2Term: TMenuItem
          Caption = 'cont. Error &2 Term'
          OnClick = MIExpContError2TermClick
        end
        object MIExpContError3Term: TMenuItem
          Caption = 'cont. Error &3 Term'
          OnClick = MIExpContError3TermClick
        end
        object MIExpContError4Term: TMenuItem
          Caption = 'cont. Error &4 Term'
          OnClick = MIExpContError4TermClick
        end
        object MIExpContError5Term: TMenuItem
          Caption = 'cont. Error &5 Term'
          OnClick = MIExpContError5TermClick
        end
      end
    end
    object MiAccuracy: TMenuItem
      Caption = 'Accuracy'
      object MiAccuracyLog2: TMenuItem
        Caption = 'Log2'
        object MiAccuracyLog2MinError2: TMenuItem
          Caption = 'min. Error &2 Term'
          OnClick = MiAccuracyLog2Click
        end
        object MiAccuracyLog2MinError3: TMenuItem
          Tag = 1
          Caption = 'min. Error &3 Term'
          OnClick = MiAccuracyLog2Click
        end
        object MiAccuracyLog2MinError4: TMenuItem
          Tag = 2
          Caption = 'min. Error &4 Term'
          OnClick = MiAccuracyLog2Click
        end
        object MiAccuracyLog2MinError5: TMenuItem
          Tag = 3
          Caption = 'min. Error &5 Term'
          OnClick = MiAccuracyLog2Click
        end
        object N19: TMenuItem
          Caption = '-'
        end
        object MiAccuracyLog2ContError2: TMenuItem
          Tag = 4
          Caption = 'cont. Error &2 Term'
          OnClick = MiAccuracyLog2Click
        end
        object MiAccuracyLog2ContError3: TMenuItem
          Tag = 5
          Caption = 'cont. Error &3 Term'
          OnClick = MiAccuracyLog2Click
        end
        object MiAccuracyLog2ContError4: TMenuItem
          Tag = 6
          Caption = 'cont. Error &4 Term'
          OnClick = MiAccuracyLog2Click
        end
        object MiAccuracyLog2ContError5: TMenuItem
          Tag = 7
          Caption = 'cont. Error &5 Term'
          OnClick = MiAccuracyLog2Click
        end
      end
      object N2x1: TMenuItem
        Caption = '2^(x)'
        object MiAccuracyPower2MinError2: TMenuItem
          Caption = 'min. Error &2 Term'
          OnClick = MiAccuracyPower2MinErrorClick
        end
        object MiAccuracyPower2MinError3: TMenuItem
          Caption = 'min. Error &3 Term'
        end
        object MiAccuracyPower2MinError4: TMenuItem
          Caption = 'min. Error &4 Term'
        end
        object MiAccuracyPower2MinError5: TMenuItem
          Caption = 'min. Error &5 Term'
        end
        object N20: TMenuItem
          Caption = '-'
        end
        object MiAccuracyPower2ContError2: TMenuItem
          Caption = 'cont. Error &2 Term'
        end
        object MiAccuracyPower2ContError3: TMenuItem
          Caption = 'cont. Error &3 Term'
        end
        object MiAccuracyPower2ContError4: TMenuItem
          Caption = 'cont. Error &4 Term'
        end
        object MiAccuracyPower2ContError5: TMenuItem
          Caption = 'cont. Error &5 Term'
        end
      end
    end
  end
end
