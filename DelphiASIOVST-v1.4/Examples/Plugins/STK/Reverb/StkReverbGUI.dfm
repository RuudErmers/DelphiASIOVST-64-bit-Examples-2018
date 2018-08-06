object FmStkReverb: TFmStkReverb
  Left = 286
  Top = 77
  BorderStyle = bsNone
  Caption = 'Stk Reverb'
  ClientHeight = 27
  ClientWidth = 289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  DesignSize = (
    289
    27)
  PixelsPerInch = 96
  TextHeight = 13
  object LbT60: TLabel
    Left = 8
    Top = 8
    Width = 22
    Height = 13
    Anchors = [akLeft]
    Caption = 'T60:'
  end
  object LbT60Value: TLabel
    Left = 247
    Top = 8
    Width = 35
    Height = 13
    Anchors = [akRight]
    Caption = 'T60:'
  end
  object SBT60: TScrollBar
    Left = 36
    Top = 6
    Width = 205
    Height = 16
    Anchors = [akLeft, akRight]
    Max = 5000
    Min = 20
    PageSize = 0
    Position = 20
    TabOrder = 0
    OnChange = SBT60Change
  end
end
