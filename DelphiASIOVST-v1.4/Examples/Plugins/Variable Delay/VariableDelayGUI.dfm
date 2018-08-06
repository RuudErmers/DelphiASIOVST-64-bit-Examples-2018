object VSTGUI: TVSTGUI
  Left = 300
  Top = 179
  BorderStyle = bsNone
  Caption = 'VariableDelay'
  ClientHeight = 146
  ClientWidth = 301
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  DesignSize = (
    301
    146)
  PixelsPerInch = 96
  TextHeight = 13
  object LbSamples: TLabel
    Left = 8
    Top = 32
    Width = 285
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Delay: 441 samples (= 10 ms)'
  end
  object LbDryMixValue: TLabel
    Left = 8
    Top = 77
    Width = 285
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Dry Mix: 0 %'
  end
  object LbWetMixValue: TLabel
    Left = 8
    Top = 125
    Width = 285
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Wet Mix: 100 %'
  end
  object SampleBar: TScrollBar
    Left = 8
    Top = 8
    Width = 285
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Max = 100000
    PageSize = 0
    Position = 441
    TabOrder = 0
    OnChange = SampleBarChange
  end
  object SBDryMix: TScrollBar
    Left = 8
    Top = 53
    Width = 285
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Max = 1000
    PageSize = 0
    TabOrder = 1
    OnChange = SBDryMixChange
  end
  object SBWetMix: TScrollBar
    Left = 8
    Top = 101
    Width = 285
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Max = 1000
    PageSize = 0
    TabOrder = 2
    OnChange = SBWetMixChange
  end
end
