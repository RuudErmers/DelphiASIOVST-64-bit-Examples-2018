object EditorForm: TEditorForm
  Left = 388
  Top = 144
  BorderStyle = bsNone
  Caption = 'EditorForm'
  ClientHeight = 126
  ClientWidth = 252
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbThreshold: TLabel
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Threshold:'
  end
  object LbThresholdValue: TLabel
    Left = 193
    Top = 8
    Width = 51
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'dB'
  end
  object LbAttack: TLabel
    Left = 8
    Top = 46
    Width = 35
    Height = 13
    Caption = 'Attack:'
  end
  object LbAttackValue: TLabel
    Left = 193
    Top = 46
    Width = 51
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'dB'
  end
  object LbRelease: TLabel
    Left = 8
    Top = 84
    Width = 42
    Height = 13
    Caption = 'Release:'
  end
  object LbReleaseValue: TLabel
    Left = 193
    Top = 84
    Width = 51
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'dB'
  end
  object SBThreshold: TScrollBar
    Left = 8
    Top = 24
    Width = 236
    Height = 16
    Max = 0
    Min = -96
    PageSize = 0
    Position = -80
    TabOrder = 0
    OnChange = SBThresholdChange
  end
  object SBAttack: TScrollBar
    Left = 8
    Top = 62
    Width = 236
    Height = 16
    Max = 20
    Min = 1
    PageSize = 0
    Position = 10
    TabOrder = 1
    OnChange = SBAttackChange
  end
  object SBRelease: TScrollBar
    Left = 8
    Top = 100
    Width = 236
    Height = 16
    Max = 200
    Min = 10
    PageSize = 0
    Position = 10
    TabOrder = 2
    OnChange = SBReleaseChange
  end
end
