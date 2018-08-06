object FmPngDialog: TFmPngDialog
  Left = 301
  Top = 68
  Caption = 'Manage PNG'
  ClientHeight = 145
  ClientWidth = 218
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
    218
    145)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 8
    Top = 8
    Width = 202
    Height = 129
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnPaint = PaintBoxPaint
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.png'
    Filter = 'Portable Network Graphics (*.png)|*.png'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Open Pixelmap'
    Left = 24
    Top = 8
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.png'
    Filter = 'Portable Network Graphics (*.png)|*.png'
    Title = 'Save Pixelmap'
    Left = 88
    Top = 8
  end
  object MainMenu: TMainMenu
    Left = 152
    Top = 8
    object MiFile: TMenuItem
      Caption = '&File'
      object MiLoadPng: TMenuItem
        Caption = 'Load PNG...'
        OnClick = MiLoadPngClick
      end
      object MiSavePng: TMenuItem
        Caption = 'Save PNG...'
        OnClick = MiSavePngClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
  end
end
