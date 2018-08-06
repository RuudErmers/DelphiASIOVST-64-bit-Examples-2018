object FmASIOVST: TFmASIOVST
  Left = 336
  Top = 123
  BorderStyle = bsNone
  Caption = 'FmASIOVST'
  ClientHeight = 236
  ClientWidth = 332
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnShow = FormShow
  DesignSize = (
    332
    236)
  PixelsPerInch = 96
  TextHeight = 13
  object LbAsioOutput: TLabel
    Left = 2
    Top = 7
    Width = 59
    Height = 13
    Caption = 'ASIO Driver:'
    Transparent = True
    OnClick = LbAsioOutputClick
  end
  object CbASIO: TComboBox
    Left = 67
    Top = 4
    Width = 236
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = CbASIOChange
  end
  object Memo: TMemo
    Left = 2
    Top = 31
    Width = 324
    Height = 197
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 1
  end
  object CBShortCircuit: TCheckBox
    Left = 309
    Top = 6
    Width = 15
    Height = 17
    Hint = 'Short Circuit'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnClick = CBShortCircuitClick
  end
end
