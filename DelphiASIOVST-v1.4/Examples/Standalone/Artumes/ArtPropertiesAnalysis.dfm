object FmAnalysisProperties: TFmAnalysisProperties
  Left = 300
  Top = 53
  BorderStyle = bsDialog
  Caption = 'Analysis Properties'
  ClientHeight = 465
  ClientWidth = 507
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PnProperties: TPanel
    Left = 8
    Top = 8
    Width = 491
    Height = 418
    TabOrder = 0
  end
  object BtOK: TButton
    Left = 262
    Top = 432
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = BtOKClick
  end
  object BtCancel: TButton
    Left = 343
    Top = 432
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = BtCancelClick
  end
  object BtApply: TButton
    Left = 424
    Top = 432
    Width = 75
    Height = 25
    Caption = '&Apply'
    Enabled = False
    TabOrder = 3
  end
end
