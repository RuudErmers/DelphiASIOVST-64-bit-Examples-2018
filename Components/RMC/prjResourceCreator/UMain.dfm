object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 528
  ClientWidth = 921
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 129
    Height = 25
    Caption = 'Create Everything'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 47
    Width = 129
    Height = 25
    Caption = 'Exec RH'
    TabOrder = 1
    Visible = False
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 208
    Top = 16
    Width = 577
    Height = 449
    MaxLength = 5
    ScrollBars = ssVertical
    TabOrder = 2
    WordWrap = False
  end
end
