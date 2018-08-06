object FmPixelMapDialog: TFmPixelMapDialog
  Left = 299
  Top = 55
  BorderIcons = [biSystemMenu]
  Caption = 'Manage Pixelmap'
  ClientHeight = 168
  ClientWidth = 275
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    275
    168)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 8
    Top = 8
    Width = 259
    Height = 152
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnPaint = PaintBoxPaint
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmap (*.bmp)|*.bmp|Portable Network Graphics (*.png)|*.png'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Open Pixelmap'
    Left = 24
    Top = 8
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmap (*.bmp)|*.bmp|Portable Network Graphics (*.png)|*.png'
    Title = 'Save Pixelmap'
    Left = 88
    Top = 8
  end
  object MainMenu: TMainMenu
    Left = 152
    Top = 8
    object MiFile: TMenuItem
      Caption = '&File'
      object MiLoad: TMenuItem
        Caption = 'Load Image...'
        OnClick = MiLoadClick
      end
      object MiSaveImage: TMenuItem
        Caption = 'Save Image...'
        OnClick = MiSaveImageClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
    object MiGenerate: TMenuItem
      Caption = '&Generate'
      object MiBrushedMetal: TMenuItem
        Caption = 'Brushed Metal'
        OnClick = MiBrushedMetalClick
      end
    end
  end
end
