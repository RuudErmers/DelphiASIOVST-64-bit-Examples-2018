object FmBindingTest: TFmBindingTest
  Left = 299
  Top = 55
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Binding Test'
  ClientHeight = 29
  ClientWidth = 143
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LbFunctionResult: TLabel
    Left = 8
    Top = 8
    Width = 78
    Height = 13
    Caption = 'Function Result:'
  end
  object LbResult: TLabel
    Left = 92
    Top = 8
    Width = 6
    Height = 13
    Caption = '0'
  end
end
