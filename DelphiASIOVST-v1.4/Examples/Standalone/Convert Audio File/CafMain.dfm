object FmConvertAudioFile: TFmConvertAudioFile
  Left = 286
  Top = 77
  Caption = 'Convert Audio File'
  ClientHeight = 199
  ClientWidth = 310
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  DesignSize = (
    310
    199)
  PixelsPerInch = 96
  TextHeight = 13
  object LbNoFileLoaded: TLabel
    Left = 118
    Top = 78
    Width = 70
    Height = 13
    Anchors = []
    Caption = 'No File Loaded'
  end
  object PnAudioDetails: TPanel
    Left = 0
    Top = 0
    Width = 310
    Height = 199
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    Visible = False
    object LbChannel: TLabel
      Left = 16
      Top = 16
      Width = 43
      Height = 13
      Caption = 'Channel:'
    end
    object LbSampleFrames: TLabel
      Left = 93
      Top = 16
      Width = 71
      Height = 13
      Caption = 'Sampleframes:'
    end
    object LbTotalTime: TLabel
      Left = 93
      Top = 35
      Width = 53
      Height = 13
      Caption = 'Total Time:'
    end
    object LbSampleRate: TLabel
      Left = 16
      Top = 89
      Width = 58
      Height = 13
      Caption = 'Samplerate:'
    end
    object LbBitsPerSample: TLabel
      Left = 16
      Top = 118
      Width = 65
      Height = 13
      Caption = 'Bits / Sample:'
    end
    object LbEncoding: TLabel
      Left = 16
      Top = 148
      Width = 47
      Height = 13
      Caption = 'Encoding:'
    end
    object LbOld: TLabel
      Left = 125
      Top = 64
      Width = 22
      Height = 13
      Caption = 'OLD'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LbNew: TLabel
      Left = 213
      Top = 64
      Width = 24
      Height = 13
      Caption = 'NEW'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object EdChannel: TEdit
      Left = 65
      Top = 16
      Width = 22
      Height = 13
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
    object EdSampleFrames: TEdit
      Left = 170
      Top = 16
      Width = 64
      Height = 13
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
    object EdTotalTime: TEdit
      Left = 170
      Top = 35
      Width = 64
      Height = 13
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
    end
    object EdSampleRate: TEdit
      Left = 125
      Top = 89
      Width = 64
      Height = 13
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 3
    end
    object EdBitsPerSample: TEdit
      Left = 125
      Top = 118
      Width = 64
      Height = 13
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 4
    end
    object EdEncoding: TEdit
      Left = 125
      Top = 148
      Width = 64
      Height = 13
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 5
    end
    object CbEncoding: TComboBox
      Left = 213
      Top = 145
      Width = 84
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 6
      Text = 'Integer'
      OnChange = CbEncodingChange
      Items.Strings = (
        'Integer'
        'Float')
    end
    object SEBitsPerSample: TSpinEdit
      Left = 213
      Top = 115
      Width = 52
      Height = 22
      MaxValue = 64
      MinValue = 1
      TabOrder = 7
      Value = 16
    end
    object SeSampleRate: TSpinEdit
      Left = 213
      Top = 86
      Width = 68
      Height = 22
      MaxValue = 9999999
      MinValue = 1
      TabOrder = 8
      Value = 44100
    end
    object CbFloatBits: TComboBox
      Left = 213
      Top = 114
      Width = 68
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 1
      TabOrder = 9
      Text = '32 bit'
      Visible = False
      Items.Strings = (
        '16 bit'
        '32 bit'
        '64 bit')
    end
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
    object MiFile: TMenuItem
      Caption = '&File'
      object MiOpen: TMenuItem
        Caption = 'Open...'
        OnClick = MiOpenClick
      end
      object MiSaveAs: TMenuItem
        Caption = 'Save As...'
        Enabled = False
        OnClick = MiSaveAsClick
      end
      object MiSave: TMenuItem
        Caption = '&Save'
        Enabled = False
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.wav'
    Filter = 'WAVE File (*.wav)|*.wav|AIFF (*.aiff)|*.aif*|AU (*.au)|*.au'
    Title = 'Select any supported audio file'
    Left = 40
    Top = 8
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.wav'
    Filter = 'WAVE File (*.wav)|*.wav|AIFF (*.aiff)|*.aif*|AU (*.au)|*.au'
    Title = 'Select any supported audio file'
    Left = 72
    Top = 8
  end
end
