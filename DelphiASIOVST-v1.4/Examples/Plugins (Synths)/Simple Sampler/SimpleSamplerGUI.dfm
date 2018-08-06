object VSTGUI: TVSTGUI
  Left = 317
  Top = 185
  BorderStyle = bsNone
  Caption = 'SimpleSampler'
  ClientHeight = 231
  ClientWidth = 365
  Color = clBtnFace
  TransparentColorValue = 12948623
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object MidiKeys: TGuiMidiKeys
    Left = 0
    Top = 158
    Width = 365
    Height = 73
    Align = alBottom
    BaseOctave = 4
    BlackKeyHeight = 0.629999995231628400
    Height3d = 0.200000002980232200
    KeyDownMode = kdmFlat
    KeyZones = <>
    ShowKeyZones = False
    OnNoteOff = MidiKeysNoteOff
    OnNoteOn = MidiKeysNoteOn
  end
  object LbSample: TLabel
    Left = 8
    Top = 8
    Width = 48
    Height = 13
    Caption = 'Sample:'
  end
  object Waveform: TGuiStaticWaveform
    Left = 8
    Top = 32
    Width = 349
    Height = 121
    DisplayChannels = 1
    NormalizationType = ntOverallChannels
    WaveDrawMode = wdmOutline
    Transparent = True
  end
  object EditSample: TEdit
    Left = 58
    Top = 5
    Width = 200
    Height = 21
    TabOrder = 0
    OnChange = EditSampleChange
  end
  object BtSampleSelect: TButton
    Left = 264
    Top = 5
    Width = 93
    Height = 21
    Caption = 'Select...'
    TabOrder = 1
    OnClick = BtSampleSelectClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'wav'
    Filter = 
      'All known files|*.wav;*.aif*;*.au;*.snd|Wave Files (*.wav)|*.wav' +
      '|AIFF Files (*.aiff)|*.aif*|AU files (*.au)|*.au;*.snd'
    Title = 'Select a Sample...'
    Left = 8
    Top = 40
  end
end
