object FmFileFormatTest: TFmFileFormatTest
  Left = 299
  Top = 55
  Caption = 'File Format Test'
  ClientHeight = 208
  ClientWidth = 223
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    223
    208)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 8
    Top = 8
    Width = 207
    Height = 192
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnClick = PaintBoxClick
    OnPaint = PaintBoxPaint
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmap (*.bmp)|*.bmp|Portable Network Graphics (*.png)|*.png'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Open Pixelmap'
    Left = 24
    Top = 16
  end
end
