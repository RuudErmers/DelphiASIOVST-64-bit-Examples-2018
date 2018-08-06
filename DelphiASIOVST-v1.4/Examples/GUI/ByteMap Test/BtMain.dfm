object FmBytemapTest: TFmBytemapTest
  Left = 504
  Top = 147
  Caption = 'Bitmap Test'
  ClientHeight = 244
  ClientWidth = 384
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
    384
    244)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 8
    Top = 39
    Width = 368
    Height = 197
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnPaint = PaintBoxPaint
  end
  object BtCountTest: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Count Test'
    TabOrder = 0
    OnClick = BtCountTestClick
  end
  object BtSimpleTest: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Simple Test'
    TabOrder = 1
  end
  object BtPaintTest: TButton
    Left = 170
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Paint Test'
    TabOrder = 2
    OnClick = BtPaintTestClick
  end
  object BtSave: TButton
    Left = 251
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Save Test'
    TabOrder = 3
    OnClick = BtSaveClick
  end
end
