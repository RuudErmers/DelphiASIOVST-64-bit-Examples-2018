object FmAnalyser: TFmAnalyser
  Left = 328
  Top = 278
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Simple ASIO Third Octave Analyser'
  ClientHeight = 326
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbDrivername: TLabel
    Left = 8
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Driver:'
    OnClick = LbDrivernameClick
  end
  object LbChannels: TLabel
    Left = 8
    Top = 36
    Width = 69
    Height = 13
    Caption = 'Input Channel:'
  end
  object LbSpeed: TLabel
    Left = 8
    Top = 64
    Width = 34
    Height = 13
    Caption = 'Speed:'
  end
  object LbFullscale: TLabel
    Left = 206
    Top = 64
    Width = 53
    Height = 13
    Caption = 'Fullscale = '
  end
  object LbFullScaleUnit: TLabel
    Left = 323
    Top = 64
    Width = 13
    Height = 13
    Caption = 'dB'
  end
  object CbDriver: TComboBox
    Left = 50
    Top = 7
    Width = 288
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = CbDriverChange
  end
  object BtControlPanel: TButton
    Left = 347
    Top = 8
    Width = 91
    Height = 21
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = BtControlPanelClick
  end
  object CbChannel: TComboBox
    Left = 92
    Top = 32
    Width = 246
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
  object BtAnalyse: TButton
    Left = 347
    Top = 32
    Width = 91
    Height = 50
    Caption = 'Analyse'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = BtAnalyseClick
  end
  object RbFast: TRadioButton
    Left = 50
    Top = 63
    Width = 40
    Height = 17
    Caption = '&Fast'
    TabOrder = 4
    OnClick = RbFastClick
  end
  object RbMedium: TRadioButton
    Left = 92
    Top = 63
    Width = 55
    Height = 17
    Caption = '&Medium'
    Checked = True
    TabOrder = 5
    TabStop = True
    OnClick = RbMediumClick
  end
  object RbSlow: TRadioButton
    Left = 151
    Top = 63
    Width = 46
    Height = 17
    Caption = '&Slow'
    TabOrder = 6
    OnClick = RbSlowClick
  end
  object SeFullscaleGain: TSpinEdit
    Left = 263
    Top = 60
    Width = 56
    Height = 22
    MaxValue = 200
    MinValue = 0
    TabOrder = 7
    Value = 0
    OnChange = SeFullscaleGainChange
  end
  object GLSceneViewer: TGLSceneViewer
    Left = 8
    Top = 88
    Width = 430
    Height = 230
    Camera = GLCamera
    Buffer.BackgroundColor = 789774
    FieldOfView = 101.991035461425800000
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
    OnMouseWheel = GLSceneViewerMouseWheel
    TabOrder = 8
  end
  object ASIOHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreFillOutBuffer = bpfZero
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = BSDownSampled
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 253
    Top = 24
  end
  object Timer: TTimer
    Enabled = False
    Interval = 50
    OnTimer = TimerTimer
    Left = 281
    Top = 24
  end
  object BarGraphScene: TGLScene
    Left = 216
    Top = 160
    object GLDummyCube: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1.000000000000000000
    end
    object GLLight: TGLLightSource
      Ambient.Color = {8716793F08AC3C3F1B2FDD3C0000803F}
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {54E3453F9A99193FAAF1D23E0000803F}
      Position.Coordinates = {000000000000803F0000A0400000803F}
      LightStyle = lsOmni
      Specular.Color = {0000803F0000003F000000000000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 93.140060424804680000
      Position.Coordinates = {000000000000803F0000A0400000803F}
    end
  end
end
