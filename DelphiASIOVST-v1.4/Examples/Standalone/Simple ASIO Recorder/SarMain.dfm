object FmRecordAudio: TFmRecordAudio
  Left = 416
  Top = 156
  Caption = 'Record Audio'
  ClientHeight = 138
  ClientWidth = 432
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LbDrivername: TLabel
    Left = 7
    Top = 12
    Width = 33
    Height = 13
    Caption = 'Driver:'
  end
  object LbChannels: TLabel
    Left = 7
    Top = 36
    Width = 72
    Height = 13
    Caption = 'Input Channel:'
  end
  object LbRecordedFile: TLabel
    Left = 8
    Top = 60
    Width = 20
    Height = 13
    Caption = 'File:'
  end
  object LbBuffer: TLabel
    Left = 344
    Top = 36
    Width = 37
    Height = 13
    Caption = 'Buffer: '
    OnClick = LbBufferClick
  end
  object LbBufferValue: TLabel
    Left = 384
    Top = 36
    Width = 32
    Height = 13
    Caption = '100 %'
  end
  object DriverCombo: TComboBox
    Left = 64
    Top = 7
    Width = 273
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    OnChange = DriverComboChange
  end
  object BtControlPanel: TButton
    Left = 344
    Top = 7
    Width = 81
    Height = 21
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = BtControlPanelClick
  end
  object ChannelBox: TComboBox
    Left = 104
    Top = 32
    Width = 233
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = ChannelBoxChange
  end
  object BtStartStop: TButton
    Left = 2
    Top = 88
    Width = 423
    Height = 41
    Caption = '&Record Audio'
    Enabled = False
    TabOrder = 3
    OnClick = BtStartStopClick
  end
  object EdFile: TEdit
    Left = 34
    Top = 57
    Width = 303
    Height = 21
    TabOrder = 4
    Text = 'Test.wav'
    OnChange = EdFileChange
  end
  object BtSelect: TButton
    Left = 344
    Top = 57
    Width = 81
    Height = 21
    Caption = 'Select...'
    TabOrder = 5
    OnClick = BtSelectClick
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    Left = 208
    Top = 16
  end
  object Timer: TTimer
    Interval = 200
    OnTimer = TimerTimer
    Left = 176
    Top = 16
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'mp3'
    Filter = 'WAV File (*.wav)|*.wav'
    Title = 'Select a WAV file'
    Left = 144
    Top = 16
  end
end
