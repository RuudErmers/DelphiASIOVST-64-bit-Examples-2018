object FmProjectSetup: TFmProjectSetup
  Left = 218
  Top = 77
  BorderStyle = bsDialog
  Caption = 'Project Setup'
  ClientHeight = 119
  ClientWidth = 266
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    266
    119)
  PixelsPerInch = 96
  TextHeight = 13
  object LbSelectA: TLabel
    Left = 8
    Top = 11
    Width = 56
    Height = 13
    Caption = 'Filename A:'
  end
  object LbSelectB: TLabel
    Left = 8
    Top = 38
    Width = 55
    Height = 13
    Caption = 'Filename B:'
  end
  object EdFilenameA: TEdit
    Left = 73
    Top = 8
    Width = 120
    Height = 21
    TabOrder = 0
    OnChange = EdFilenameChange
  end
  object EdFilenameB: TEdit
    Left = 73
    Top = 35
    Width = 120
    Height = 21
    TabOrder = 1
    OnChange = EdFilenameChange
  end
  object BtSelectA: TButton
    Left = 199
    Top = 8
    Width = 59
    Height = 21
    Caption = 'Select...'
    TabOrder = 2
    OnClick = BtSelectAClick
  end
  object BtSelectB: TButton
    Left = 199
    Top = 35
    Width = 59
    Height = 21
    Caption = 'Select...'
    TabOrder = 3
    OnClick = BtSelectBClick
  end
  object BtOk: TButton
    Left = 8
    Top = 87
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 4
    OnClick = BtOkClick
  end
  object BtCancel: TButton
    Left = 89
    Top = 87
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object CBProtectSettings: TCheckBox
    Left = 8
    Top = 62
    Width = 225
    Height = 17
    Caption = 'Protect Settings (store audio into project)'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 6
  end
end
