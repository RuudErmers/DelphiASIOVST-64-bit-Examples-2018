object Player: TPlayer
  Left = 248
  Top = 103
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'MIDI/WAV Player/Recorder'
  ClientHeight = 529
  ClientWidth = 424
  Color = clGray
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object GbMidiFilePlayer: TGroupBox
    Left = 7
    Top = 0
    Width = 201
    Height = 417
    Caption = ' MIDI File Player '
    TabOrder = 0
    object LbMidiCurrentFile: TLabel
      Left = 8
      Top = 294
      Width = 55
      Height = 14
      Caption = 'current file:'
    end
    object LbMidiFile: TLabel
      Left = 8
      Top = 308
      Width = 36
      Height = 14
      Caption = '<none>'
      Transparent = True
    end
    object LbMidiTempo: TLabel
      Left = 8
      Top = 328
      Width = 76
      Height = 14
      Caption = 'tempo: 120 bpm'
    end
    object LbMidiPosition: TLabel
      Left = 8
      Top = 366
      Width = 62
      Height = 14
      Caption = 'position: 0 %'
    end
    object LbMidiPlayMode: TLabel
      Left = 8
      Top = 245
      Width = 141
      Height = 14
      Caption = 'action when finished playing:'
    end
    object MidiBox: TListBox
      Left = 8
      Top = 24
      Width = 185
      Height = 161
      AutoComplete = False
      BevelInner = bvNone
      BevelOuter = bvNone
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 14
      MultiSelect = True
      ParentFont = False
      TabOrder = 0
      OnDblClick = MidiBoxDblClick
    end
    object BtMidiAdd: TButton
      Left = 8
      Top = 192
      Width = 49
      Height = 17
      Caption = 'add'
      TabOrder = 1
      OnClick = BtMidiAddClick
    end
    object BtMidiRemove: TButton
      Left = 64
      Top = 192
      Width = 49
      Height = 17
      Caption = 'remove'
      TabOrder = 2
      OnClick = BtMidiRemoveClick
    end
    object BtMidiStop: TButton
      Left = 64
      Top = 216
      Width = 49
      Height = 17
      Caption = 'stop'
      TabOrder = 3
      OnClick = BtMidiStopClick
    end
    object BtMidiPlay: TButton
      Left = 8
      Top = 216
      Width = 49
      Height = 17
      Caption = 'play'
      TabOrder = 4
      OnClick = BtMidiPlayClick
    end
    object CbOnlyChannel1: TCheckBox
      Left = 124
      Top = 192
      Width = 69
      Height = 17
      Caption = 'only CH1'
      TabOrder = 5
    end
    object CBMidiPlayMode: TComboBox
      Left = 8
      Top = 261
      Width = 140
      Height = 22
      BevelInner = bvNone
      BevelOuter = bvNone
      Style = csDropDownList
      Color = clBlack
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 14
      ItemIndex = 1
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 6
      Text = 'play same file again'
      Items.Strings = (
        'stop playback'
        'play same file again'
        'play next file in list'
        'play random file in list')
    end
    object SbTempo: TScrollBar
      Left = 8
      Top = 344
      Width = 185
      Height = 16
      Max = 240
      Min = 20
      PageSize = 0
      Position = 120
      TabOrder = 7
      OnChange = SbTempoChange
    end
    object SbMidiPosition: TScrollBar
      Left = 8
      Top = 384
      Width = 185
      Height = 16
      PageSize = 0
      TabOrder = 8
      OnChange = SbMidiPositionChange
    end
  end
  object GbWavFilePlayer: TGroupBox
    Left = 215
    Top = 0
    Width = 201
    Height = 417
    Caption = ' WAV File Player '
    TabOrder = 1
    object LbWavCurrentFile: TLabel
      Left = 8
      Top = 294
      Width = 55
      Height = 14
      Caption = 'current file:'
    end
    object LbWaveFile: TLabel
      Left = 8
      Top = 308
      Width = 36
      Height = 14
      Caption = '<none>'
      Transparent = True
    end
    object LbWavPitch: TLabel
      Left = 8
      Top = 328
      Width = 60
      Height = 14
      Caption = 'pitch: 100 %'
    end
    object LbWavPosition: TLabel
      Left = 8
      Top = 368
      Width = 62
      Height = 14
      Caption = 'position: 0 %'
    end
    object LbWavPlayMode: TLabel
      Left = 8
      Top = 245
      Width = 141
      Height = 14
      Caption = 'action when finished playing:'
    end
    object WavBox: TListBox
      Left = 8
      Top = 24
      Width = 185
      Height = 161
      AutoComplete = False
      BevelInner = bvNone
      BevelOuter = bvNone
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 14
      MultiSelect = True
      ParentFont = False
      TabOrder = 0
      OnDblClick = WavBoxDblClick
    end
    object BtWavAdd: TButton
      Left = 8
      Top = 192
      Width = 49
      Height = 17
      Caption = 'add'
      TabOrder = 1
      OnClick = BtWavAddClick
    end
    object BtWavRemove: TButton
      Left = 64
      Top = 192
      Width = 49
      Height = 17
      Caption = 'remove'
      TabOrder = 2
      OnClick = BtWavRemoveClick
    end
    object BtWavStop: TButton
      Left = 64
      Top = 216
      Width = 49
      Height = 17
      Caption = 'stop'
      TabOrder = 3
      OnClick = BtWavStopClick
    end
    object BtWavPlay: TButton
      Left = 8
      Top = 216
      Width = 49
      Height = 17
      Caption = 'play'
      TabOrder = 4
      OnClick = BtWavPlayClick
    end
    object CBWavPlayMode: TComboBox
      Left = 8
      Top = 261
      Width = 140
      Height = 22
      BevelInner = bvNone
      BevelOuter = bvNone
      Style = csDropDownList
      Color = clBlack
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 14
      ItemIndex = 1
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 5
      Text = 'play same file again'
      OnChange = CBWavPlayModeChange
      Items.Strings = (
        'stop playback'
        'play same file again'
        'play next file in list'
        'play random file in list')
    end
    object SbPitch: TScrollBar
      Left = 8
      Top = 344
      Width = 185
      Height = 16
      Max = 341
      Min = 1
      PageSize = 0
      Position = 170
      TabOrder = 6
      OnChange = SbPitchChange
    end
    object SbWavPosition: TScrollBar
      Left = 8
      Top = 384
      Width = 185
      Height = 16
      PageSize = 0
      TabOrder = 7
      OnChange = SbWavPositionChange
    end
  end
  object GbWavRecorder: TGroupBox
    Left = 7
    Top = 418
    Width = 410
    Height = 105
    Caption = ' WAV Recorder '
    TabOrder = 2
    object LbCurrentRecordFile: TLabel
      Left = 8
      Top = 48
      Width = 138
      Height = 14
      Caption = 'current file (click to change):'
    end
    object LbRecordFile: TLabel
      Left = 8
      Top = 62
      Width = 36
      Height = 14
      Cursor = crHandPoint
      Caption = '<none>'
      Transparent = True
      OnClick = LbRecordFileClick
    end
    object LbStatus: TLabel
      Left = 8
      Top = 80
      Width = 75
      Height = 14
      Caption = 'status: stopped'
    end
    object BtWavPause: TButton
      Left = 64
      Top = 24
      Width = 49
      Height = 17
      Caption = 'pause'
      TabOrder = 0
      OnClick = BtWavPauseClick
    end
    object BtWavStopRec: TButton
      Left = 120
      Top = 24
      Width = 49
      Height = 17
      Caption = 'stop'
      TabOrder = 1
      OnClick = BtWavStopRecClick
    end
    object BtWavRecord: TButton
      Left = 8
      Top = 24
      Width = 49
      Height = 17
      Caption = 'record'
      TabOrder = 2
      OnClick = BtWavRecordClick
    end
    object CbRecInMono: TCheckBox
      Left = 185
      Top = 24
      Width = 96
      Height = 17
      Caption = 'record in mono'
      TabOrder = 3
    end
    object CbRecordFormat: TComboBox
      Left = 304
      Top = 22
      Width = 89
      Height = 22
      BevelInner = bvNone
      BevelOuter = bvNone
      Style = csDropDownList
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 14
      ItemIndex = 0
      ParentFont = False
      TabOrder = 4
      Text = '16 bit integer'
      Items.Strings = (
        '16 bit integer'
        '32 bit float')
    end
  end
end
