object FmStkChorus: TFmStkChorus
  Left = 286
  Top = 77
  BorderStyle = bsNone
  Caption = 'Stk Chorus'
  ClientHeight = 78
  ClientWidth = 289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  DesignSize = (
    289
    78)
  PixelsPerInch = 96
  TextHeight = 13
  object LbModDepth: TLabel
    Left = 8
    Top = 8
    Width = 56
    Height = 13
    Caption = 'Mod Depth:'
  end
  object LbModDepthValue: TLabel
    Left = 247
    Top = 8
    Width = 26
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Value'
  end
  object LbModFreq: TLabel
    Left = 8
    Top = 32
    Width = 53
    Height = 13
    Caption = 'Mod Freq.:'
  end
  object LbModFreqValue: TLabel
    Left = 247
    Top = 32
    Width = 26
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Value'
  end
  object LbEffectMix: TLabel
    Left = 8
    Top = 56
    Width = 52
    Height = 13
    Caption = 'Effect Mix:'
  end
  object LbEffectMixValue: TLabel
    Left = 247
    Top = 56
    Width = 26
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Value'
  end
  object SBModDepth: TScrollBar
    Left = 70
    Top = 6
    Width = 171
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Max = 5000
    Min = 1
    PageSize = 0
    Position = 20
    TabOrder = 0
    OnChange = SBModDepthChange
  end
  object SBModFreq: TScrollBar
    Left = 70
    Top = 30
    Width = 171
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Max = 1000
    PageSize = 0
    Position = 20
    TabOrder = 1
    OnChange = SBModFreqChange
  end
  object SbEffectMix: TScrollBar
    Left = 70
    Top = 54
    Width = 171
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Max = 1000
    PageSize = 0
    Position = 20
    TabOrder = 2
    OnChange = SbEffectMixChange
  end
end
