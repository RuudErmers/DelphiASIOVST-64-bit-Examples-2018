object FmCustomGui: TFmCustomGui
  Left = 327
  Top = 117
  BorderStyle = bsNone
  Caption = 'Graphics32 GUI'
  ClientHeight = 178
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 120
    Top = 8
    object MiDesignMode: TMenuItem
      Caption = '&Design Mode'
      OnClick = MiDesignModeClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object MiParameter: TMenuItem
      Caption = 'Parameter'
    end
    object MiProperties: TMenuItem
      Caption = 'Properties'
      Visible = False
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MiAddBitmap: TMenuItem
      Caption = 'Add Bitmap...'
      Visible = False
      OnClick = MiAddBitmapClick
    end
    object MiAddDial: TMenuItem
      Caption = 'Add Dial...'
      OnClick = MiAddDialClick
    end
    object MiAddText: TMenuItem
      Caption = 'Add Text'
      Visible = False
      OnClick = MiAddTextClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MiSetBackground: TMenuItem
      Caption = 'Set Background...'
      OnClick = MiSetBackgroundClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object MiLoad: TMenuItem
      Caption = 'Load...'
      OnClick = MiLoadClick
    end
  end
  object OpenPictureDialog: TOpenDialog
    Filter = 
      'All (*.png;*.bmp)|*.png;*.bmp|Portable Network Graphics (*.png)|' +
      '*.png|Bitmaps (*.bmp)|*.bmp'
    Left = 32
    Top = 8
  end
end
