object FmSetup: TFmSetup
  Left = 412
  Top = 277
  BorderStyle = bsDialog
  Caption = 'Setup'
  ClientHeight = 92
  ClientWidth = 235
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    235
    92)
  PixelsPerInch = 96
  TextHeight = 13
  object LbAsioDriver: TLabel
    Left = 8
    Top = 7
    Width = 81
    Height = 21
    AutoSize = False
    Caption = 'ASIO Driver:'
    Layout = tlCenter
    OnClick = LbAsioDriverClick
  end
  object LbInput: TLabel
    Left = 8
    Top = 39
    Width = 46
    Height = 21
    AutoSize = False
    Caption = 'Input:'
    Layout = tlCenter
  end
  object LbOutput: TLabel
    Left = 8
    Top = 63
    Width = 48
    Height = 21
    AutoSize = False
    Caption = 'Output:'
    Layout = tlCenter
  end
  object CBDrivers: TComboBox
    Left = 95
    Top = 8
    Width = 132
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    TabOrder = 0
    OnChange = CBDriversChange
  end
  object CBInput: TComboBox
    Left = 60
    Top = 39
    Width = 167
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    TabOrder = 1
    OnChange = CBInputChange
  end
  object CBOutput: TComboBox
    Left = 60
    Top = 63
    Width = 167
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    TabOrder = 2
    OnChange = CBOutputChange
  end
end
