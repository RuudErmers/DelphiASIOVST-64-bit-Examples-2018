object FmStkPitchShift: TFmStkPitchShift
  Left = 286
  Top = 77
  BorderStyle = bsNone
  Caption = 'Stk PitchShift'
  ClientHeight = 47
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
    47)
  PixelsPerInch = 96
  TextHeight = 13
  object LbSemitones: TLabel
    Left = 8
    Top = 8
    Width = 53
    Height = 13
    Caption = 'Semitones:'
  end
  object LbSemitonesValue: TLabel
    Left = 247
    Top = 8
    Width = 26
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Value'
  end
  object LbEffectMix: TLabel
    Left = 8
    Top = 28
    Width = 52
    Height = 13
    Caption = 'Effect Mix:'
  end
  object LbEffectMixValue: TLabel
    Left = 247
    Top = 28
    Width = 26
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Value'
  end
  object SBSemitones: TScrollBar
    Left = 70
    Top = 6
    Width = 171
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Max = 240
    Min = -240
    PageSize = 0
    Position = 20
    TabOrder = 0
    OnChange = SBSemitonesChange
  end
  object SbEffectMix: TScrollBar
    Left = 70
    Top = 26
    Width = 171
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Max = 1000
    PageSize = 0
    Position = 20
    TabOrder = 1
    OnChange = SbEffectMixChange
  end
end
