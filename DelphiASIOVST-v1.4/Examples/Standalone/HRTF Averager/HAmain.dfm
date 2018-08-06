object FmHrtfAverager: TFmHrtfAverager
  Left = 218
  Top = 77
  Caption = 'HRTF Averager'
  ClientHeight = 276
  ClientWidth = 597
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    597
    276)
  PixelsPerInch = 96
  TextHeight = 13
  object LbPolarUnit: TLabel
    Left = 358
    Top = 6
    Width = 5
    Height = 13
    Caption = #176
    Enabled = False
  end
  object LbPolar: TLabel
    Left = 262
    Top = 6
    Width = 28
    Height = 13
    Caption = 'Polar:'
    Enabled = False
  end
  object LbAzimuthUnit: TLabel
    Left = 244
    Top = 6
    Width = 5
    Height = 13
    Caption = #176
    Enabled = False
  end
  object LbAzimuth: TLabel
    Left = 135
    Top = 6
    Width = 42
    Height = 13
    Caption = 'Azimuth:'
    Enabled = False
  end
  object LbHrtfIndex: TLabel
    Left = 3
    Top = 6
    Width = 61
    Height = 13
    Caption = 'HRTF Index:'
    Enabled = False
  end
  object AudioDataDisplayLeft: TGuiAudioDataDisplay
    Left = 3
    Top = 30
    Width = 586
    Height = 110
    Anchors = [akLeft, akTop, akRight]
    AudioDataCollection = ADHRIR
    DisplayedChannel = 0
    DisplayChannels = <>
    LineWidth = 0
    Normalize = False
    XAxis.SampleUpper = 511
    XAxis.FractionalLower = -0.500000000000000000
    XAxis.FractionalUpper = 0.500000000000000000
  end
  object AudioDataDisplayRight: TGuiAudioDataDisplay
    Left = 3
    Top = 146
    Width = 586
    Height = 110
    Anchors = [akLeft, akTop, akRight]
    AudioDataCollection = ADHRIR
    DisplayedChannel = 1
    DisplayChannels = <>
    LineWidth = 0
    Normalize = False
    XAxis.SampleUpper = 511
    XAxis.FractionalLower = -0.500000000000000000
    XAxis.FractionalUpper = 0.500000000000000000
  end
  object SEPolar: TSpinEdit
    Left = 296
    Top = 3
    Width = 59
    Height = 22
    Enabled = False
    MaxValue = 180
    MinValue = -180
    TabOrder = 2
    Value = 0
    OnChange = SEPolarChange
  end
  object SEAzimuth: TSpinEdit
    Left = 183
    Top = 3
    Width = 59
    Height = 22
    Enabled = False
    MaxValue = 360
    MinValue = -360
    TabOrder = 3
    Value = 0
    OnChange = SEAzimuthChange
  end
  object SEHrtfIndex: TSpinEdit
    Left = 70
    Top = 3
    Width = 59
    Height = 22
    Enabled = False
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 0
    OnChange = SEHrtfIndexChange
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 257
    Width = 597
    Height = 19
    Panels = <>
  end
  object MainMenu: TMainMenu
    Left = 16
    Top = 24
    object MIFile: TMenuItem
      Caption = '&File'
      object MISelectDirectory: TMenuItem
        Caption = 'Select Directory...'
        OnClick = MISelectDirectoryClick
      end
      object MISaveAs: TMenuItem
        Caption = '&Save As...'
        OnClick = MISaveAsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MIExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MIExitClick
      end
    end
  end
  object ADHRIR: TAudioDataCollection32
    Channels = <
      item
        DisplayName = 'Channel 1'
      end
      item
        DisplayName = 'Channel 2'
      end>
    SampleFrames = 512
    SampleRate = 44100.000000000000000000
    Left = 48
    Top = 24
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'hrtf'
    Filter = 'HRTF files (*.HRTF)|*.hrtf'
    Title = 'Select an HRTF file'
    Left = 80
    Top = 24
  end
end
