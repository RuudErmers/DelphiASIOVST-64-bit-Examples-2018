object FmAudioSettings: TFmAudioSettings
  Left = 421
  Top = 193
  BorderStyle = bsDialog
  Caption = 'Audio Settings'
  ClientHeight = 147
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
    147)
  PixelsPerInch = 96
  TextHeight = 13
  object LbPreset: TLabel
    Left = 8
    Top = 8
    Width = 81
    Height = 19
    AutoSize = False
    Caption = 'ASIO Driver:'
    Layout = tlCenter
  end
  object LbSettings: TLabel
    Left = 8
    Top = 69
    Width = 41
    Height = 21
    AutoSize = False
    Caption = 'Settings:'
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
    OnChange = CBDriversChange
  end
  object MemoSettings: TMemo
    Left = 55
    Top = 74
    Width = 192
    Height = 70
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 1
  end
  object BtControlPanel: TButton
    Left = 8
    Top = 38
    Width = 239
    Height = 25
    Caption = '&Control Panel'
    TabOrder = 2
  end
end
