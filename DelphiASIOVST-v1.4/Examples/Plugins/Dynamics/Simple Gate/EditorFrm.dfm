object EditorForm: TEditorForm
  Left = 218
  Top = 81
  BorderStyle = bsNone
  Caption = 'EditorForm'
  ClientHeight = 46
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
  object LbdB: TLabel
    Left = 193
    Top = 8
    Width = 51
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'dB'
  end
  object ScrollBar: TScrollBar
    Left = 8
    Top = 24
    Width = 236
    Height = 16
    Max = 0
    Min = -96
    PageSize = 0
    Position = -80
    TabOrder = 0
    OnChange = ScrollBarChange
  end
end
