object FmAsioDriverControlPanel: TFmAsioDriverControlPanel
  Left = 0
  Top = 0
  Caption = 'ASIO VST Host Driver - Control Panel'
  ClientHeight = 35
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    306
    35)
  PixelsPerInch = 96
  TextHeight = 13
  object LbDriver: TLabel
    Left = 8
    Top = 11
    Width = 33
    Height = 13
    Caption = 'Driver:'
  end
  object CbDriver: TComboBox
    Left = 47
    Top = 8
    Width = 166
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = CbDriverChange
  end
  object BtControlPanel: TButton
    Left = 219
    Top = 8
    Width = 81
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '&Control Panel'
    TabOrder = 1
    OnClick = BtControlPanelClick
  end
end
