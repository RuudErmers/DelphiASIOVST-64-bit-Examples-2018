object FmSetup: TFmSetup
  Left = 653
  Top = 675
  BorderStyle = bsToolWindow
  Caption = 'Setup'
  ClientHeight = 86
  ClientWidth = 226
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LbPreset: TLabel
    Left = 4
    Top = 2
    Width = 81
    Height = 21
    AutoSize = False
    Caption = 'ASIO Driver:'
    Layout = tlCenter
  end
  object LbInput: TLabel
    Left = 4
    Top = 34
    Width = 37
    Height = 21
    AutoSize = False
    Caption = 'Input:'
    Layout = tlCenter
  end
  object LbOutput: TLabel
    Left = 4
    Top = 58
    Width = 47
    Height = 21
    AutoSize = False
    Caption = 'Output:'
    Layout = tlCenter
  end
  object CBDrivers: TComboBox
    Left = 88
    Top = 2
    Width = 132
    Height = 21
    Style = csDropDownList
    Color = clBtnFace
    ItemHeight = 13
    TabOrder = 0
    OnChange = CBDriversChange
  end
  object CBInput: TComboBox
    Left = 56
    Top = 34
    Width = 164
    Height = 21
    Style = csDropDownList
    Color = clBtnFace
    ItemHeight = 13
    TabOrder = 1
    OnChange = CBInputChange
  end
  object CBOutput: TComboBox
    Left = 56
    Top = 58
    Width = 164
    Height = 21
    Style = csDropDownList
    Color = clBtnFace
    ItemHeight = 13
    TabOrder = 2
    OnChange = CBOutputChange
  end
end
