object VoiceTestForm: TVoiceTestForm
  Left = 243
  Top = 114
  BorderStyle = bsNone
  Caption = 'VoiceTestForm'
  ClientHeight = 73
  ClientWidth = 175
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 67
    Height = 13
    Caption = 'Active voices:'
  end
  object Label2: TLabel
    Left = 16
    Top = 40
    Width = 74
    Height = 13
    Caption = 'Existing Voices:'
  end
  object lblActiveV: TLabel
    Left = 104
    Top = 16
    Width = 6
    Height = 13
    Caption = '0'
  end
  object lblAllV: TLabel
    Left = 104
    Top = 40
    Width = 6
    Height = 13
    Caption = '0'
  end
end
