object FmPaintBoxTest: TFmPaintBoxTest
  Left = 320
  Top = 87
  Caption = 'Paint Box Test'
  ClientHeight = 336
  ClientWidth = 412
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GuiPaintBox: TGuiPaintBox
    Left = 0
    Top = 0
    Width = 412
    Height = 336
    Align = alClient
  end
end
