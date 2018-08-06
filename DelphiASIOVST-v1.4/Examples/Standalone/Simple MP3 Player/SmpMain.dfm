object FmSimpleMp3Player: TFmSimpleMp3Player
  Left = 459
  Top = 285
  BorderStyle = bsSingle
  Caption = 'Simple MP3 Player'
  ClientHeight = 310
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    322
    310)
  PixelsPerInch = 96
  TextHeight = 13
  object LbBuffer: TLabel
    Left = 123
    Top = 286
    Width = 37
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Buffer: '
  end
  object LbBufferValue: TLabel
    Left = 163
    Top = 286
    Width = 32
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '100 %'
  end
  object BtRewind: TButton
    Left = 8
    Top = 111
    Width = 25
    Height = 25
    Caption = '<<'
    TabOrder = 0
    OnClick = BtRewindClick
  end
  object PnInformation: TPanel
    Left = 8
    Top = 8
    Width = 305
    Height = 65
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      305
      65)
    object LbInformation: TLabel
      Left = 9
      Top = 44
      Width = 288
      Height = 13
      Alignment = taCenter
      Anchors = [akLeft, akBottom]
      AutoSize = False
      Caption = 'Artist Name - Album Name - Track No - Track Name'
    end
    object LbTimeInfo: TLabel
      Left = 9
      Top = 12
      Width = 50
      Height = 21
      Alignment = taCenter
      Anchors = [akLeft, akBottom]
      Caption = '00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LbBitrateInfo: TLabel
      Left = 240
      Top = 20
      Width = 57
      Height = 13
      Alignment = taRightJustify
      Anchors = [akLeft, akBottom]
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object BtPlay: TButton
    Left = 39
    Top = 111
    Width = 25
    Height = 25
    Caption = '>'
    TabOrder = 2
    OnClick = BtPlayClick
  end
  object BtPause: TButton
    Left = 70
    Top = 111
    Width = 25
    Height = 25
    Caption = '||'
    TabOrder = 3
    OnClick = BtPauseClick
  end
  object BtStop: TButton
    Left = 101
    Top = 111
    Width = 25
    Height = 25
    Caption = '#'
    TabOrder = 4
    OnClick = BtStopClick
  end
  object BtForward: TButton
    Left = 132
    Top = 111
    Width = 25
    Height = 25
    Caption = '>>'
    TabOrder = 5
    OnClick = BtForwardClick
  end
  object TbVolume: TTrackBar
    Left = 163
    Top = 111
    Width = 150
    Height = 25
    Max = 100
    Frequency = 10
    Position = 100
    TabOrder = 6
    ThumbLength = 15
    OnChange = TbVolumeChange
  end
  object TbPosition: TTrackBar
    Left = 7
    Top = 79
    Width = 306
    Height = 26
    Max = 1000
    Frequency = 25
    TabOrder = 7
    ThumbLength = 16
  end
  object PlayList: TListBox
    Left = 8
    Top = 142
    Width = 305
    Height = 132
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 8
    OnClick = PlayListClick
  end
  object BtAddFile: TButton
    Left = 8
    Top = 280
    Width = 25
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '+'
    TabOrder = 9
    OnClick = BtAddFileClick
  end
  object BtDeleteItem: TButton
    Left = 39
    Top = 280
    Width = 25
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '-'
    TabOrder = 10
    OnClick = BtDeleteItemClick
  end
  object BtSetup: TButton
    Left = 258
    Top = 280
    Width = 55
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Setup'
    TabOrder = 11
    OnClick = BtSetupClick
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 58
    Top = 35
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'mp3'
    Filter = 'MP3 File (*.mp3)|*.mp3'
    Left = 90
    Top = 35
  end
  object Timer: TTimer
    Interval = 200
    OnTimer = TimerTimer
    Left = 26
    Top = 35
  end
end
