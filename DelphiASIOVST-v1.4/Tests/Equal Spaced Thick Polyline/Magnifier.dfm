object FmMagnifier: TFmMagnifier
  Left = 653
  Top = 24
  Caption = 'Magnifier'
  ClientHeight = 964
  ClientWidth = 1013
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    1013
    964)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 8
    Top = 8
    Width = 997
    Height = 948
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnPaint = PaintBoxPaint
  end
end
