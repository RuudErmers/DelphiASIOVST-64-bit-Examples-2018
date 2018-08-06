object FmAudioSettings: TFmAudioSettings
  Left = 421
  Top = 193
  BorderStyle = bsDialog
  Caption = 'Audio Settings'
  ClientHeight = 35
  ClientWidth = 255
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    255
    35)
  PixelsPerInch = 96
  TextHeight = 13
  object LbPreset: TLabel
    Left = 8
    Top = 8
    Width = 81
    Height = 21
    AutoSize = False
    Caption = 'ASIO Driver:'
    Layout = tlCenter
  end
  object CBDrivers: TComboBox
    Left = 89
    Top = 8
    Width = 158
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
  end
end
