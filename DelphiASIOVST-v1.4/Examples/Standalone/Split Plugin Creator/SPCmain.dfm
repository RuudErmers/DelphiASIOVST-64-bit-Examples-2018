object FmSplitPluginCreator: TFmSplitPluginCreator
  Left = 291
  Top = 238
  BorderStyle = bsDialog
  Caption = 'Split Plugin Creator'
  ClientHeight = 93
  ClientWidth = 336
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LbPluginA: TLabel
    Left = 8
    Top = 11
    Width = 42
    Height = 13
    Caption = 'Plugin A:'
  end
  object LbPluginB: TLabel
    Left = 8
    Top = 38
    Width = 41
    Height = 13
    Caption = 'Plugin B:'
  end
  object EdPluginA: TEdit
    Left = 56
    Top = 8
    Width = 230
    Height = 21
    TabOrder = 0
    OnChange = EdPluginChange
  end
  object EdPluginB: TEdit
    Left = 55
    Top = 35
    Width = 231
    Height = 21
    Enabled = False
    TabOrder = 1
    Text = 'please donate to unlock this feature'
    OnChange = EdPluginChange
  end
  object BtCreate: TButton
    Left = 120
    Top = 62
    Width = 75
    Height = 25
    Caption = 'Create...'
    Enabled = False
    TabOrder = 2
    OnClick = BtCreateClick
  end
  object BtOpenA: TButton
    Left = 286
    Top = 8
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 3
    OnClick = BtOpenAClick
  end
  object BtClearA: TButton
    Left = 307
    Top = 8
    Width = 21
    Height = 21
    Caption = 'C'
    TabOrder = 4
    OnClick = BtClearAClick
  end
  object BtOpenB: TButton
    Left = 286
    Top = 35
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 5
    OnClick = BtOpenBClick
  end
  object BtClearB: TButton
    Left = 307
    Top = 35
    Width = 21
    Height = 21
    Caption = 'C'
    TabOrder = 6
    OnClick = BtClearBClick
  end
end
