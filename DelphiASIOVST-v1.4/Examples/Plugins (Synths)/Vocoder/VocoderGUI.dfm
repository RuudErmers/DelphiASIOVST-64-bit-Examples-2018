object VSTGUI: TVSTGUI
  Left = 317
  Top = 185
  BorderStyle = bsNone
  Caption = 'Vocoder'
  ClientHeight = 142
  ClientWidth = 262
  Color = clBtnFace
  TransparentColorValue = 12948623
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MidiKeys: TGuiMidiKeys
    Left = 0
    Top = 69
    Width = 262
    Height = 73
    Align = alBottom
    KeyDownMode = kdmFlat
    Height3d = 0.200000002980232200
    BlackKeyHeight = 0.629999995231628400
    BaseOctave = 4
    NumOctaves = 2
    KeyZones = <>
    ShowKeyZones = False
    OnNoteOn = MidiKeysNoteOn
    OnNoteOff = MidiKeysNoteOff
  end
  object LbInput: TLabel
    Left = 8
    Top = 10
    Width = 69
    Height = 13
    Caption = 'Input Level:'
  end
  object LbSynthLevel: TLabel
    Left = 8
    Top = 29
    Width = 72
    Height = 13
    Caption = 'Synth Level:'
  end
  object LbVocoderLevel: TLabel
    Left = 8
    Top = 48
    Width = 86
    Height = 13
    Caption = 'Vocoder Level:'
  end
  object SBInputLevel: TScrollBar
    Left = 104
    Top = 8
    Width = 150
    Height = 15
    Max = 0
    Min = -80
    PageSize = 0
    TabOrder = 0
    OnChange = SBInputLevelChange
  end
  object SBSynthLevel: TScrollBar
    Left = 104
    Top = 27
    Width = 150
    Height = 15
    Max = 0
    Min = -80
    PageSize = 0
    TabOrder = 1
    OnChange = SBSynthLevelChange
  end
  object SBVocoderLevel: TScrollBar
    Left = 104
    Top = 46
    Width = 150
    Height = 15
    Max = 0
    Min = -80
    PageSize = 0
    TabOrder = 2
    OnChange = SBVocoderLevelChange
  end
end
