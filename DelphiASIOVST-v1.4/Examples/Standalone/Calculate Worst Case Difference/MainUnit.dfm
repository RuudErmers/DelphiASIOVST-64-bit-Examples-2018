object FmCalculateWorstCaseDifference: TFmCalculateWorstCaseDifference
  Left = 0
  Top = 0
  Caption = 'Calculate Worst Case Difference'
  ClientHeight = 337
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    377
    337)
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 318
    Width = 377
    Height = 19
    Panels = <
      item
        Text = 'Trial:'
        Width = 70
      end
      item
        Text = 'Average:'
        Width = 124
      end
      item
        Text = 'Maximum:'
        Width = 50
      end>
  end
  object PCMain: TPageControl
    Left = 8
    Top = 8
    Width = 361
    Height = 304
    ActivePage = TsMain
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    TabPosition = tpBottom
    object TsMain: TTabSheet
      Caption = 'Main'
      DesignSize = (
        353
        278)
      object ListBox: TListBox
        Left = 0
        Top = 34
        Width = 353
        Height = 239
        ItemHeight = 13
        TabOrder = 0
      end
      object BtAddFiles: TButton
        Left = 0
        Top = 3
        Width = 89
        Height = 25
        Caption = 'Add Files...'
        Default = True
        TabOrder = 1
        OnClick = BtAddFilesClick
      end
      object BtClear: TButton
        Left = 95
        Top = 3
        Width = 42
        Height = 25
        Caption = 'Clear'
        TabOrder = 2
        OnClick = BtClearClick
      end
      object BtStartCalculation: TButton
        Left = 143
        Top = 3
        Width = 106
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Start Calculation'
        Enabled = False
        TabOrder = 3
        OnClick = BtStartCalculationClick
      end
      object BtBuildDifference: TButton
        Left = 255
        Top = 3
        Width = 98
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Build Difference...'
        Enabled = False
        TabOrder = 4
        OnClick = BtBuildDifferenceClick
      end
    end
    object TsHistory: TTabSheet
      Caption = 'History'
      Enabled = False
      ImageIndex = 1
      object CtHistory: TChart
        Left = 0
        Top = 0
        Width = 353
        Height = 278
        Legend.Visible = False
        Title.Text.Strings = (
          'History')
        Title.Visible = False
        View3D = False
        View3DOptions.Orthogonal = False
        Align = alClient
        TabOrder = 0
        ColorPaletteIndex = 13
        object SsWorst: TLineSeries
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Visible = False
          Dark3D = False
          LinePen.Width = 2
          Pointer.InflateMargins = True
          Pointer.Style = psRectangle
          Pointer.Visible = False
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object SsAverage: TLineSeries
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Visible = False
          Dark3D = False
          LinePen.Width = 2
          Pointer.Brush.Gradient.EndColor = 3513587
          Pointer.Gradient.EndColor = 3513587
          Pointer.InflateMargins = True
          Pointer.Style = psRectangle
          Pointer.Visible = False
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.wav'
    Filter = 'WAV (*.wav)|*.wav'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 32
    Top = 56
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.wav'
    Filter = 'WAV (*.wav)|*.wav'
    Left = 104
    Top = 56
  end
end
