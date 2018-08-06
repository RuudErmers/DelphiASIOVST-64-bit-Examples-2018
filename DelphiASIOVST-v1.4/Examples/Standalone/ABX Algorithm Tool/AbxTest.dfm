object FmAbxTest: TFmAbxTest
  Left = 643
  Top = 73
  Caption = 'ABX Test'
  ClientHeight = 388
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    396
    388)
  PixelsPerInch = 96
  TextHeight = 13
  object LbB: TLabel
    Left = 268
    Top = 8
    Width = 120
    Height = 161
    Alignment = taCenter
    AutoSize = False
    Caption = 'B'
    Color = clGrayText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -133
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
    OnClick = LbBClick
  end
  object LbX: TLabel
    Left = 138
    Top = 8
    Width = 120
    Height = 161
    Alignment = taCenter
    AutoSize = False
    Caption = 'X'
    Color = clGrayText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -133
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
    OnClick = LbXClick
  end
  object LbA: TLabel
    Left = 8
    Top = 8
    Width = 120
    Height = 161
    Alignment = taCenter
    AutoSize = False
    Caption = 'A'
    Color = clGrayText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -133
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
    OnClick = LbAClick
  end
  object BtXisA: TButton
    Tag = 1
    Left = 103
    Top = 184
    Width = 75
    Height = 34
    Caption = 'X is A'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = BtXisAClick
  end
  object BtXisB: TButton
    Tag = 2
    Left = 215
    Top = 184
    Width = 75
    Height = 34
    Caption = 'X is B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = BtXisBClick
  end
  object BtAudioStop: TButton
    Left = 199
    Top = 224
    Width = 51
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 2
    OnClick = BtAudioStopClick
  end
  object BtAudioPlay: TButton
    Left = 143
    Top = 224
    Width = 51
    Height = 25
    Caption = 'Play'
    TabOrder = 3
    OnClick = BtAudioPlayClick
  end
  object NotesBox: TGroupBox
    Left = 8
    Top = 264
    Width = 380
    Height = 99
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Comments '
    TabOrder = 4
    object Notes: TMemo
      Left = 2
      Top = 15
      Width = 376
      Height = 82
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 0
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 369
    Width = 396
    Height = 19
    Panels = <>
  end
end
