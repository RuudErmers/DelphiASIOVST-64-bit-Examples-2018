object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 658
  ClientWidth = 874
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Label2: TLabel
    Left = 39
    Top = 19
    Width = 47
    Height = 16
    Caption = 'Midi Out'
  end
  object ComboBoxOut: TComboBox
    Left = 39
    Top = 49
    Width = 145
    Height = 24
    TabOrder = 0
    Text = 'ComboBoxOut'
    OnChange = ComboBoxOutChange
  end
  object Memo1: TMemo
    Left = 231
    Top = 336
    Width = 409
    Height = 281
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Button1: TButton
    Left = 39
    Top = 88
    Width = 98
    Height = 25
    Caption = 'Set Wavefile'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 39
    Top = 119
    Width = 98
    Height = 25
    Caption = 'Play'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 39
    Top = 150
    Width = 98
    Height = 25
    Caption = 'Stop'
    TabOrder = 4
    OnClick = Button3Click
  end
end
