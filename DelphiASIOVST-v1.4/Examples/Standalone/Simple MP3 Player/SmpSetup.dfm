object FmSetup: TFmSetup
  Left = 511
  Top = 239
  BorderStyle = bsDialog
  Caption = 'Setup'
  ClientHeight = 94
  ClientWidth = 226
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LbAsioDriver: TLabel
    Left = 8
    Top = 11
    Width = 61
    Height = 13
    Caption = 'ASIO Driver:'
    Layout = tlCenter
  end
  object LbOutput: TLabel
    Left = 8
    Top = 38
    Width = 38
    Height = 13
    Caption = 'Output:'
    Layout = tlCenter
  end
  object CBDrivers: TComboBox
    Left = 75
    Top = 8
    Width = 143
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = CBDriversChange
  end
  object CBOutput: TComboBox
    Left = 52
    Top = 35
    Width = 166
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = CBOutputChange
  end
  object BtControlPanel: TButton
    Left = 8
    Top = 62
    Width = 210
    Height = 25
    Caption = 'Control Panel'
    TabOrder = 2
    OnClick = BtControlPanelClick
  end
end
