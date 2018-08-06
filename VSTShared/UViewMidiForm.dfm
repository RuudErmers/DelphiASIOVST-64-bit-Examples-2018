object ViewMidiForm: TViewMidiForm
  Left = 291
  Top = 266
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Demo application for ASIO-Host'
  ClientHeight = 900
  ClientWidth = 1303
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 19
    Width = 38
    Height = 16
    Caption = 'Midi In'
  end
  object Label2: TLabel
    Left = 184
    Top = 19
    Width = 48
    Height = 16
    Caption = 'Midi Out'
  end
  object XSynthPanel: TPanel
    Left = 0
    Top = 80
    Width = 185
    Height = 41
    TabOrder = 0
  end
  object ComboBoxOut: TComboBox
    Left = 184
    Top = 41
    Width = 145
    Height = 24
    TabOrder = 1
    Text = 'ComboBoxOut'
    OnChange = ComboBoxOutChange
  end
  object ComboBoxIn: TComboBox
    Left = 8
    Top = 41
    Width = 145
    Height = 24
    TabOrder = 2
    Text = 'ComboBoxOut'
    OnChange = ComboBoxInChange
  end
  object Button1: TButton
    Left = 376
    Top = 41
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 457
    Top = 41
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 4
    OnClick = Button2Click
  end
end
