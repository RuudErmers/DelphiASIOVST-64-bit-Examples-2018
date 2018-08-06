object FmAudioSettings: TFmAudioSettings
  Left = 421
  Top = 193
  BorderStyle = bsDialog
  Caption = 'Audio Settings'
  ClientHeight = 89
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
    89)
  PixelsPerInch = 96
  TextHeight = 13
  object LbPreset: TLabel
    Left = 8
    Top = 8
    Width = 65
    Height = 21
    AutoSize = False
    Caption = 'ASIO Driver:'
    Layout = tlCenter
  end
  object LbChannels: TLabel
    Left = 8
    Top = 32
    Width = 41
    Height = 21
    AutoSize = False
    Caption = 'Channel:'
    Layout = tlCenter
  end
  object CBDrivers: TComboBox
    Left = 79
    Top = 8
    Width = 168
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
  end
  object CBChannels: TComboBox
    Left = 79
    Top = 32
    Width = 168
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
    OnChange = CBChannelsChange
  end
  object BtControlPanel: TButton
    Left = 8
    Top = 59
    Width = 239
    Height = 25
    Caption = '&Control Panel'
    TabOrder = 2
    OnClick = BtControlPanelClick
  end
end
