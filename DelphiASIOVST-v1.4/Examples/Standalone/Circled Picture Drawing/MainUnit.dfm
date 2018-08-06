object FmPrimitivePictureEvolution: TFmPrimitivePictureEvolution
  Left = 300
  Top = 56
  Caption = 'Primitive Picture Evolution'
  ClientHeight = 264
  ClientWidth = 431
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 245
    Width = 431
    Height = 19
    Panels = <
      item
        Text = 'No Reference'
        Width = 72
      end
      item
        Text = 'Circles:'
        Width = 64
      end
      item
        Text = 'Trials:'
        Width = 72
      end
      item
        Text = 'Cost:'
        Width = 80
      end
      item
        Text = 'Global Costs:'
        Width = 120
      end
      item
        Text = 'Circles per Second:'
        Width = 150
      end
      item
        Text = 'Misc. Status'
        Width = 100
      end>
  end
  object PcMain: TPageControl
    Left = 0
    Top = 0
    Width = 431
    Height = 245
    ActivePage = TsDrawing
    Align = alClient
    TabOrder = 1
    object TsDrawing: TTabSheet
      Caption = 'Drawing'
      object PaintBoxDraw: TPaintBox
        Left = 215
        Top = 8
        Width = 201
        Height = 201
        OnPaint = PaintBoxDrawPaint
      end
      object PaintBoxRef: TPaintBox
        Left = 8
        Top = 8
        Width = 201
        Height = 201
        OnPaint = PaintBoxRefPaint
      end
    end
    object TsOptimizationHistory: TTabSheet
      Caption = 'Optimization History'
      ImageIndex = 1
      object CtOptimizationHistory: TChart
        Left = 0
        Top = 0
        Width = 423
        Height = 217
        Legend.Visible = False
        Title.Text.Strings = (
          'TChart')
        Title.Visible = False
        Shadow.Visible = False
        View3D = False
        View3DWalls = False
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ColorPaletteIndex = 13
        object SeriesHistory: TFastLineSeries
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Visible = False
          LinePen.Color = 10708548
          LinePen.Width = 2
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
          Data = {
            001900000000000000008690400000000000A48F400000000000E08F40000000
            0000B08D400000000000348C4000000000006088400000000000208C40000000
            0000788E4000000000003690400000000000408F400000000000B88F40000000
            0000788E400000000000648E4000000000000891400000000000309140000000
            0000909040000000000058914000000000000C924000000000005C9240000000
            0000B69240000000000028944000000000001296400000000000969440000000
            00003096400000000000349740}
        end
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 16
    object MiFile: TMenuItem
      Caption = '&File'
      object MiOpenReference: TMenuItem
        Caption = '&Open Reference...'
        OnClick = MiOpenReferenceClick
      end
      object MiSaveResult: TMenuItem
        Caption = '&Save Result...'
        OnClick = MiSaveResultClick
      end
      object MiSaveHighResolution: TMenuItem
        Caption = 'Save &High Resolution...'
        OnClick = MiSaveHighResolutionClick
      end
      object MiSaveFramed: TMenuItem
        Caption = 'Save &Framed...'
        OnClick = MiSaveFramedClick
      end
      object MiSaveAnimation: TMenuItem
        Caption = 'Save &Animation...'
        OnClick = MiSaveAnimationClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MiOpenDrawing: TMenuItem
        Caption = 'Open &Drawing...'
        OnClick = MiOpenDrawingClick
      end
      object MiOpenBest: TMenuItem
        Caption = 'Open &Best...'
        OnClick = MiOpenBestClick
      end
      object MiCopyReference: TMenuItem
        Caption = '&Copy Reference'
        Visible = False
        OnClick = MiCopyReferenceClick
      end
      object MiSaveDrawing: TMenuItem
        Caption = 'Sa&ve Drawing...'
        Enabled = False
        OnClick = MiSaveDrawingClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
    object MiEvolve: TMenuItem
      Caption = '&Optimizer'
      object MiStart: TMenuItem
        Action = AcStart
      end
      object MiStopContinue: TMenuItem
        Caption = 'St&op'
        Enabled = False
        OnClick = MiStopContinueClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MiSavePopulation: TMenuItem
        Caption = 'Save &Population'
        Enabled = False
        OnClick = MiSavePopulationClick
      end
      object MiLoadPopulation: TMenuItem
        Caption = 'Load Population...'
        OnClick = MiLoadPopulationClick
      end
      object MiBackupPopulation: TMenuItem
        Caption = 'Backup Population'
        OnClick = MiBackupPopulationClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MiNext: TMenuItem
        Action = AcNext
        Enabled = False
      end
      object MiBack: TMenuItem
        Action = AcBack
        Enabled = False
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MiLog: TMenuItem
        Caption = 'Log'
        ShortCut = 123
        OnClick = MiLogClick
      end
      object MiStoreLog: TMenuItem
        Caption = 'Store Log'
        OnClick = MiStoreLogClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MiSettings: TMenuItem
        Action = AcSettings
      end
    end
    object MiDisplay: TMenuItem
      Caption = '&Display'
      object MiCurrentBestCost: TMenuItem
        Caption = 'Current Best'
        Checked = True
        RadioItem = True
        OnClick = MiCurrentBestCostClick
      end
      object MiPreviousBest: TMenuItem
        Caption = 'Previous Best'
        RadioItem = True
        OnClick = MiPreviousBestClick
      end
      object MiCostMap: TMenuItem
        Caption = 'Cost Map'
        RadioItem = True
        OnClick = MiCostMapClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object MiCrosshair: TMenuItem
        Caption = 'Crosshair'
        Checked = True
        OnClick = MiCrosshairClick
      end
    end
    object MiDrawing: TMenuItem
      Caption = 'Drawing'
      object MiScale2x: TMenuItem
        Caption = 'Scale by Factor 2'
        OnClick = MiScale2xClick
      end
      object MiScaleHalf: TMenuItem
        Caption = 'Scale by Factor 0.5'
        OnClick = MiScaleHalfClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmap (*.bmp)|*.bmp|Portable Network Graphics (*.png)|*.png'
    Left = 112
    Top = 16
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmap (*.bmp)|*.bmp|Portable Network Graphics (*.png)|*.png'
    Left = 200
    Top = 16
  end
  object OpenDialogPrimitives: TOpenDialog
    Filter = 
      'Circles (*.Circles)|*.Circles|Rounded Rectangles (*.RoundRects)|' +
      '*.RoundRects'
    Left = 112
    Top = 72
  end
  object SaveDialogPrimitives: TSaveDialog
    Filter = 
      'Circles (*.Circles)|*.Circles|Rounded Rectangles (*.RoundRects)|' +
      '*.RoundRects'
    Left = 200
    Top = 72
  end
  object ActionList: TActionList
    Left = 24
    Top = 72
    object AcStart: TAction
      Category = 'Optimizer'
      Caption = '&Start'
      ShortCut = 120
      OnExecute = AcStartExecute
    end
    object AcNext: TAction
      Category = 'Optimizer'
      Caption = '&Next'
      ShortCut = 119
      OnExecute = AcNextExecute
    end
    object AcBack: TAction
      Category = 'Optimizer'
      Caption = '&Back'
      OnExecute = AcBackExecute
    end
    object AcSettings: TAction
      Category = 'Optimizer'
      Caption = 'S&ettings'
      ShortCut = 121
      OnExecute = AcSettingsExecute
    end
  end
end
