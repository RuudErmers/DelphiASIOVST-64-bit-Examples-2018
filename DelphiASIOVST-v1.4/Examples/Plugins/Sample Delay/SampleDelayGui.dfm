object FmSampleDelay: TFmSampleDelay
  Left = 218
  Top = 77
  BorderStyle = bsNone
  Caption = 'Sample Delay'
  ClientHeight = 71
  ClientWidth = 272
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbSamplesLeft: TLabel
    Left = 8
    Top = 9
    Width = 23
    Height = 13
    Caption = 'Left:'
  end
  object LbSamplesLeftValue: TLabel
    Left = 207
    Top = 9
    Width = 6
    Height = 13
    Caption = '0'
  end
  object LbSamplesRight: TLabel
    Left = 8
    Top = 30
    Width = 29
    Height = 13
    Caption = 'Right:'
  end
  object LbSamplesRightValue: TLabel
    Left = 207
    Top = 30
    Width = 6
    Height = 13
    Caption = '0'
  end
  object SbSamplesLeft: TScrollBar
    Left = 43
    Top = 8
    Width = 158
    Height = 16
    Max = 1024
    Min = -1024
    PageSize = 0
    TabOrder = 0
    OnChange = SbSamplesLeftChange
  end
  object SbSamplesRight: TScrollBar
    Left = 43
    Top = 29
    Width = 158
    Height = 16
    Max = 1024
    Min = -1024
    PageSize = 0
    TabOrder = 1
    OnChange = SbSamplesRightChange
  end
  object CbLink: TCheckBox
    Left = 8
    Top = 51
    Width = 41
    Height = 17
    Caption = 'Link'
    TabOrder = 2
    OnClick = CbLinkClick
  end
  object CbMilliseconds: TCheckBox
    Left = 55
    Top = 51
    Width = 122
    Height = 17
    Caption = 'Display Samples in ms'
    TabOrder = 3
  end
  object BrushedMetal: TGuiBackground
    Active = False
    Color = 6782354
    Left = 88
    Top = 24
  end
end
