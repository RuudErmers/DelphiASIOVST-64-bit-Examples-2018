object FmOptions: TFmOptions
  Left = 213
  Top = 173
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Settings'
  ClientHeight = 459
  ClientWidth = 321
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object GbASIO: TGroupBox
    Left = 8
    Top = 9
    Width = 305
    Height = 169
    Caption = ' ASIO '
    TabOrder = 0
    object LbASIODriver: TLabel
      Left = 8
      Top = 16
      Width = 60
      Height = 14
      Caption = 'ASIO Driver:'
    end
    object LbOutputs: TLabel
      Left = 8
      Top = 40
      Width = 41
      Height = 14
      Caption = 'Outputs:'
    end
    object LbBufferSize: TLabel
      Left = 8
      Top = 88
      Width = 54
      Height = 14
      Caption = 'Buffersize:'
    end
    object LbSampleRate: TLabel
      Left = 8
      Top = 112
      Width = 57
      Height = 14
      Caption = 'Samplerate:'
    end
    object LbInputs: TLabel
      Left = 8
      Top = 64
      Width = 32
      Height = 14
      Caption = 'Inputs:'
    end
    object LbFormat: TLabel
      Left = 8
      Top = 136
      Width = 39
      Height = 14
      Caption = 'Format: '
    end
    object MemoInfo: TMemo
      Left = 280
      Top = 39
      Width = 289
      Height = 121
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 1
      Visible = False
    end
    object BtInfo: TButton
      Left = 256
      Top = 16
      Width = 41
      Height = 17
      Caption = 'Info'
      TabOrder = 0
      OnClick = BtInfoClick
    end
  end
  object GbGlobalSetting: TGroupBox
    Left = 8
    Top = 184
    Width = 305
    Height = 265
    Caption = ' Global Settings '
    TabOrder = 1
    object LbOverallVolume: TLabel
      Left = 8
      Top = 216
      Width = 76
      Height = 14
      Caption = 'Overall Volume:'
    end
    object LbWavVolume: TLabel
      Left = 8
      Top = 72
      Width = 68
      Height = 14
      Caption = 'WAV Volume:'
    end
    object LbTempo: TLabel
      Left = 8
      Top = 24
      Width = 35
      Height = 14
      Caption = 'Tempo:'
    end
    object LbInputVolume: TLabel
      Left = 8
      Top = 120
      Width = 65
      Height = 14
      Caption = 'Input Volume:'
    end
    object LbVSTVolume: TLabel
      Left = 8
      Top = 168
      Width = 63
      Height = 14
      Caption = 'VST Volume:'
    end
    object SbTempo: TScrollBar
      Left = 8
      Top = 40
      Width = 289
      Height = 16
      Max = 250
      Min = 10
      PageSize = 0
      Position = 10
      TabOrder = 0
      OnChange = SbTempoChange
    end
    object SbWavVolume: TScrollBar
      Left = 8
      Top = 88
      Width = 289
      Height = 16
      PageSize = 0
      Position = 100
      TabOrder = 1
      OnChange = SbTempoChange
    end
    object SbInputVolume: TScrollBar
      Left = 8
      Top = 136
      Width = 289
      Height = 16
      PageSize = 0
      Position = 100
      TabOrder = 2
      OnChange = SbTempoChange
    end
    object SbVSTVolume: TScrollBar
      Left = 8
      Top = 184
      Width = 289
      Height = 16
      PageSize = 0
      Position = 100
      TabOrder = 3
      OnChange = SbTempoChange
    end
    object SbOverallVolume: TScrollBar
      Left = 8
      Top = 232
      Width = 289
      Height = 16
      PageSize = 0
      Position = 100
      TabOrder = 4
      OnChange = SbTempoChange
    end
  end
end
