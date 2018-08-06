object FmAbxAlgorithmTest: TFmAbxAlgorithmTest
  Left = 273
  Top = 97
  BorderStyle = bsDialog
  Caption = 'ABX Algorithm Test'
  ClientHeight = 206
  ClientWidth = 376
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    376
    206)
  PixelsPerInch = 96
  TextHeight = 13
  object LbChooseTest: TLabel
    Left = 8
    Top = 11
    Width = 64
    Height = 13
    Caption = 'Choose Test:'
  end
  object Label1: TLabel
    Left = 8
    Top = 180
    Width = 50
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Audio File:'
  end
  object BtGo: TButton
    Left = 8
    Top = 40
    Width = 64
    Height = 33
    Caption = '&Go'
    Default = True
    Enabled = False
    TabOrder = 0
    OnClick = BtGoClick
  end
  object TVTestSelect: TTreeView
    Left = 78
    Top = 8
    Width = 290
    Height = 163
    Anchors = [akLeft, akTop, akRight, akBottom]
    Indent = 19
    TabOrder = 1
    OnChange = TVTestSelectChange
    Items.NodeData = {
      01020000001D0000000000000000000000FFFFFFFFFFFFFFFF00000000010000
      0002450051002F0000000000000000000000FFFFFFFFFFFFFFFF000000000200
      00000B5000650061006B002000460069006C0074006500720031000000000000
      0000000000FFFFFFFFFFFFFFFF00000000000000000C4700610069006E002000
      28004E006F006900730065002900310000000000000000000000FFFFFFFFFFFF
      FFFF00000000000000000C4700610069006E00200028004D0075007300690063
      002900290000000000000000000000FFFFFFFFFFFFFFFF000000000100000008
      440079006E0061006D00690063007300270000000000000000000000FFFFFFFF
      FFFFFFFF0000000002000000074C0069006D0069007400650072002B00000000
      00000000000000FFFFFFFFFFFFFFFF0000000000000000095400680072006500
      730068006F006C006400210000000000000000000000FFFFFFFFFFFFFFFF0000
      000000000000044B006E0065006500}
  end
  object BtASIOSetup: TButton
    Left = 8
    Top = 145
    Width = 64
    Height = 26
    Anchors = [akLeft, akBottom]
    Caption = '&ASIO Setup'
    TabOrder = 2
    OnClick = BtASIOSetupClick
  end
  object EdAudioFile: TEdit
    Left = 78
    Top = 177
    Width = 228
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
  end
  object BtSelectAudioFile: TButton
    Left = 312
    Top = 177
    Width = 56
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = '&Select...'
    TabOrder = 4
    OnClick = BtSelectAudioFileClick
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    SampleRate = 44100.000000000000000000
    Left = 8
    Top = 80
  end
  object Adc: TAudioDataCollection32
    Channels = <>
    SampleRate = 44100.000000000000000000
    Left = 40
    Top = 80
  end
end
