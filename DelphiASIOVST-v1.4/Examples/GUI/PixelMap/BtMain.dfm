object FmPixelMapTest: TFmPixelMapTest
  Left = 504
  Top = 147
  Caption = 'Pixelmap Test'
  ClientHeight = 244
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    384
    244)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 8
    Top = 8
    Width = 368
    Height = 228
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnPaint = PaintBoxPaint
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 16
    object MiFile: TMenuItem
      Caption = '&File'
      object MiSave: TMenuItem
        Caption = '&Save as Test.bmp'
        OnClick = MiSaveClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
    object MiRender: TMenuItem
      Caption = '&Render'
      object MiClear: TMenuItem
        Caption = '&Clear'
        OnClick = MiClearClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MiLineCircle: TMenuItem
        Caption = '&Line Circle'
        OnClick = MiLineCircleClick
      end
      object MiFillRect: TMenuItem
        Caption = 'Filled &Rectangles'
        OnClick = MiFillRectClick
      end
      object MiFrameRectangles: TMenuItem
        Caption = '&Framed Rectangles'
        OnClick = MiFrameRectanglesClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MiBasicMix: TMenuItem
        Caption = '&Basic Mix'
        OnClick = MiBasicMixClick
      end
    end
    object MiEdit: TMenuItem
      Caption = '&Edit'
      object MiTurnClockwise: TMenuItem
        Caption = '&Turn Clockwise'
        OnClick = MiTurnClockwiseClick
      end
      object MiTurnCounterclockwise: TMenuItem
        Caption = 'Turn &Counterclockwise'
        OnClick = MiTurnCounterclockwiseClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MiResize: TMenuItem
        Caption = '&Resize'
        OnClick = MiResizeClick
      end
    end
    object MiTests: TMenuItem
      Caption = '&Tests'
      object MiCountTest: TMenuItem
        Caption = '&Count Test'
        OnClick = MiCountTestClick
      end
    end
    object MiFilter: TMenuItem
      Caption = '&Filter'
      object MiBoxBlur: TMenuItem
        Caption = 'Stack &Blur'
        OnClick = MiBoxBlurClick
      end
      object MiSaturation: TMenuItem
        Caption = 'Saturation'
        OnClick = MiSaturationClick
      end
    end
  end
end
