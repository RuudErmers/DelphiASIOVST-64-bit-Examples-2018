object FmSetFrequency: TFmSetFrequency
  Left = 406
  Top = 321
  BorderStyle = bsDialog
  Caption = 'Set Frequency'
  ClientHeight = 72
  ClientWidth = 210
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    210
    72)
  PixelsPerInch = 96
  TextHeight = 13
  object LbNewFrequency: TLabel
    Left = 8
    Top = 13
    Width = 79
    Height = 13
    Caption = 'New Frequency:'
  end
  object LbFrequencyUnit: TLabel
    Left = 93
    Top = 44
    Width = 12
    Height = 13
    Caption = 'Hz'
  end
  object BtOK: TButton
    Left = 127
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BtCancel: TButton
    Left = 127
    Top = 39
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object EdFrequency: TEdit
    Left = 24
    Top = 41
    Width = 63
    Height = 21
    TabOrder = 2
    Text = '1000'
  end
end
