object FmBlowfish: TFmBlowfish
  Left = 240
  Top = 77
  BorderStyle = bsDialog
  Caption = 'Blowfish De-/Encryption'
  ClientHeight = 91
  ClientWidth = 544
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    544
    91)
  PixelsPerInch = 96
  TextHeight = 13
  object LbInputFile: TLabel
    Left = 8
    Top = 11
    Width = 49
    Height = 13
    Caption = 'Input File:'
    OnClick = LbInputFileClick
  end
  object LbOutputFile: TLabel
    Left = 8
    Top = 38
    Width = 57
    Height = 13
    Caption = 'Output File:'
  end
  object LbDirection: TLabel
    Left = 8
    Top = 66
    Width = 46
    Height = 13
    Caption = 'Direction:'
  end
  object LbPassword: TLabel
    Left = 229
    Top = 66
    Width = 50
    Height = 13
    Caption = 'Password:'
  end
  object EdInputFile: TEdit
    Left = 71
    Top = 8
    Width = 465
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object RbDecryption: TRadioButton
    Left = 150
    Top = 65
    Width = 73
    Height = 17
    Caption = '&Decryption'
    TabOrder = 1
  end
  object EdOutputFile: TEdit
    Left = 71
    Top = 35
    Width = 465
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object RbEncryption: TRadioButton
    Left = 71
    Top = 65
    Width = 73
    Height = 17
    Caption = '&Encryption'
    Checked = True
    TabOrder = 3
    TabStop = True
  end
  object BtInputFile: TButton
    Left = 517
    Top = 10
    Width = 18
    Height = 18
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 4
    OnClick = BtInputFileClick
  end
  object BtOutputFile: TButton
    Left = 517
    Top = 37
    Width = 18
    Height = 18
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 5
    OnClick = BtOutputFileClick
  end
  object BtExecute: TButton
    Left = 480
    Top = 62
    Width = 56
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'E&xecute'
    Enabled = False
    TabOrder = 6
    OnClick = BtExecuteClick
  end
  object EdPassword: TEdit
    Left = 285
    Top = 62
    Width = 189
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    Text = 'Password'
  end
  object OD: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    Left = 248
    Top = 16
  end
end
