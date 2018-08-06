object DriverTestCP: TDriverTestCP
  Left = 0
  Top = 0
  Caption = 'DriverTestCP'
  ClientHeight = 110
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbStupid: TLabel
    Left = 72
    Top = 24
    Width = 233
    Height = 37
    Caption = 'MyCo was here'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnDone: TButton
    Left = 280
    Top = 77
    Width = 75
    Height = 25
    Caption = '&Done'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button1: TButton
    Left = 16
    Top = 77
    Width = 75
    Height = 25
    Caption = 'Reset'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
end
