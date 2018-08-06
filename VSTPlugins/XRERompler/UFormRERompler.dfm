object FormRERompler: TFormRERompler
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'FormRERompler'
  ClientHeight = 373
  ClientWidth = 673
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 120
  TextHeight = 16
  object MidiKeys: TGuiMidiKeys
    Left = 0
    Top = 300
    Width = 673
    Height = 73
    Align = alBottom
    BlackKeyHeight = 0.629999995231628400
    Height3d = 0.200000002980232200
    KeyDownMode = kdmFlat
    KeyZones = <>
    NumOctaves = 2
    ShowKeyZones = False
    ExplicitLeft = -881
    ExplicitWidth = 1554
  end
  object ButtonLoadWave: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Load Wave'
    TabOrder = 0
  end
  object MemoDebug: TMemo
    Left = 136
    Top = 8
    Width = 489
    Height = 273
    Lines.Strings = (
      'MemoDebug')
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object ButtonEasy: TButton
    Left = 8
    Top = 39
    Width = 113
    Height = 25
    Caption = 'ButtonEasy'
    TabOrder = 2
  end
  object SESampleRate: TSpinEdit
    Left = 9
    Top = 88
    Width = 121
    Height = 26
    MaxValue = 20000
    MinValue = 0
    TabOrder = 3
    Value = 800
  end
end
