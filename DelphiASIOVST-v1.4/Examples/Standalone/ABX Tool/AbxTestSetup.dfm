object FmTestSetup: TFmTestSetup
  Left = 218
  Top = 77
  BorderStyle = bsDialog
  Caption = 'Test Setup'
  ClientHeight = 87
  ClientWidth = 248
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
  DesignSize = (
    248
    87)
  PixelsPerInch = 96
  TextHeight = 13
  object LbNumberOfTrials: TLabel
    Left = 8
    Top = 11
    Width = 82
    Height = 13
    Caption = 'Number of Trials:'
  end
  object LbNameID: TLabel
    Left = 8
    Top = 40
    Width = 46
    Height = 13
    Caption = 'Name/ID:'
  end
  object BtGo: TButton
    Left = 170
    Top = 8
    Width = 70
    Height = 44
    Anchors = [akTop, akRight]
    Caption = 'Go!'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = BtGoClick
  end
  object BtCancel: TButton
    Left = 170
    Top = 58
    Width = 70
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object SENumberOfTrials: TSpinEdit
    Left = 96
    Top = 8
    Width = 68
    Height = 22
    MaxValue = 9999
    MinValue = 1
    TabOrder = 2
    Value = 15
  end
  object CBAllowNavigation: TCheckBox
    Left = 8
    Top = 62
    Width = 97
    Height = 17
    Caption = 'Allow Navigation'
    Enabled = False
    TabOrder = 3
  end
  object EdNameID: TEdit
    Left = 60
    Top = 35
    Width = 104
    Height = 21
    TabOrder = 4
  end
end
