object FmLoudnessMeter: TFmLoudnessMeter
  Left = 299
  Top = 51
  BorderStyle = bsNone
  Caption = 'Loudness Meter'
  ClientHeight = 345
  ClientWidth = 400
  Color = 7373965
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 14410211
  Font.Height = -16
  Font.Name = 'Trebuchet MS'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 22
  object GbLoudness: TGuiGroupSimple
    Left = 8
    Top = 8
    Width = 217
    Height = 137
    Alpha = 128
    BorderColor = 14410211
    BorderWidth = 4.000000000000000000
    Caption = 'Loudness'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14410211
    Font.Height = -24
    Font.Name = 'Trebuchet MS'
    Font.Style = []
    FontOversampling = fo4x
    Native = False
    ParentFont = False
    BorderRadius = 10.000000000000000000
    Shadow.Blur = 3.000000000000000000
    Shadow.Opacity = 64
    Shadow.Saturation = 1.000000000000000000
    Shadow.Visible = True
    TabOrder = 0
    Transparent = True
    object LbLoudness: TGuiLabel
      Left = 7
      Top = 35
      Width = 201
      Height = 80
      Alignment = taCenter
      Caption = '-23,2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14410211
      Font.Height = -64
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 3.000000000000000000
      Shadow.Opacity = 64
      Shadow.Saturation = 1.000000000000000000
      Shadow.Visible = True
      Transparent = True
    end
  end
  object GbMomentaryPeak: TGuiGroupSimple
    Left = 8
    Top = 151
    Width = 385
    Height = 90
    Alpha = 128
    BorderColor = 14410211
    BorderWidth = 4.000000000000000000
    Caption = 'Peak of Momentary Loudness'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14410211
    Font.Height = -20
    Font.Name = 'Trebuchet MS'
    Font.Style = []
    FontOversampling = fo4x
    Native = False
    ParentFont = False
    BorderRadius = 10.000000000000000000
    Shadow.Blur = 3.000000000000000000
    Shadow.Opacity = 64
    Shadow.Saturation = 1.000000000000000000
    Shadow.Visible = True
    TabOrder = 1
    Transparent = True
    object LbMomentaryPeak: TGuiLabel
      Left = 93
      Top = 30
      Width = 204
      Height = 48
      Alignment = taCenter
      Caption = '-23,2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14410211
      Font.Height = -37
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 3.000000000000000000
      Shadow.Opacity = 64
      Shadow.Saturation = 1.000000000000000000
      Shadow.Visible = True
      Transparent = True
      OnClick = LbMomentaryPeakClick
    end
  end
  object GbUnit: TGuiGroupSimple
    Left = 231
    Top = 8
    Width = 162
    Height = 65
    Alpha = 128
    BorderColor = 14410211
    BorderWidth = 4.000000000000000000
    Caption = 'Unit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14410211
    Font.Height = -16
    Font.Name = 'Trebuchet MS'
    Font.Style = []
    FontOversampling = fo4x
    Native = False
    ParentFont = False
    BorderRadius = 10.000000000000000000
    Shadow.Blur = 3.000000000000000000
    Shadow.Opacity = 64
    Shadow.Saturation = 1.000000000000000000
    Shadow.Visible = True
    TabOrder = 2
    Transparent = True
    object LbUnit: TGuiLabel
      Left = 13
      Top = 24
      Width = 137
      Height = 30
      Alignment = taCenter
      Caption = 'LUFS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14410211
      Font.Height = -24
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 3.000000000000000000
      Shadow.Opacity = 64
      Shadow.Saturation = 1.000000000000000000
      Shadow.Visible = True
      Transparent = True
      OnClick = LbUnitClick
    end
  end
  object GbIntegrationTime: TGuiGroupSimple
    Left = 231
    Top = 79
    Width = 162
    Height = 65
    Alpha = 128
    BorderColor = 14410211
    BorderWidth = 4.000000000000000000
    Caption = 'Integration Time'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14410211
    Font.Height = -16
    Font.Name = 'Trebuchet MS'
    Font.Style = []
    FontOversampling = fo4x
    Native = False
    ParentFont = False
    BorderRadius = 10.000000000000000000
    Shadow.Blur = 3.000000000000000000
    Shadow.Opacity = 64
    Shadow.Saturation = 1.000000000000000000
    Shadow.Visible = True
    TabOrder = 3
    Transparent = True
    object LbIntegrationTime: TGuiLabel
      Left = 13
      Top = 24
      Width = 137
      Height = 30
      Alignment = taCenter
      Caption = 'Momentary'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14410211
      Font.Height = -24
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      PopupMenu = PmIntegrationTime
      Shadow.Blur = 3.000000000000000000
      Shadow.Opacity = 64
      Shadow.Saturation = 1.000000000000000000
      Shadow.Visible = True
      Transparent = True
      OnClick = LbIntegrationTimeClick
    end
  end
  object GbTimeDisplay: TGuiGroupSimple
    Left = 8
    Top = 247
    Width = 385
    Height = 90
    Alpha = 128
    BorderColor = 14410211
    BorderWidth = 4.000000000000000000
    Caption = 'LEQ Time Display'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14410211
    Font.Height = -20
    Font.Name = 'Trebuchet MS'
    Font.Style = []
    FontOversampling = fo4x
    Native = False
    ParentFont = False
    BorderRadius = 10.000000000000000000
    Shadow.Blur = 3.000000000000000000
    Shadow.Opacity = 64
    Shadow.Saturation = 1.000000000000000000
    Shadow.Visible = True
    TabOrder = 4
    Transparent = True
    object LbTimeDisplay: TGuiLabel
      Left = 93
      Top = 30
      Width = 204
      Height = 48
      Alignment = taCenter
      Caption = '00:00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14410211
      Font.Height = -37
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      FontOversampling = fo4x
      ParentFont = False
      Shadow.Blur = 3.000000000000000000
      Shadow.Opacity = 64
      Shadow.Saturation = 1.000000000000000000
      Shadow.Visible = True
      Transparent = True
      OnClick = LbTimeDisplayClick
    end
  end
  object TrScreenUpdate: TTimer
    Interval = 100
    OnTimer = TrScreenUpdateTimer
    Left = 312
    Top = 176
  end
  object PmIntegrationTime: TPopupMenu
    Left = 328
    Top = 120
    object MiIntegrationMomentary: TMenuItem
      Caption = '&Momentary'
      RadioItem = True
      OnClick = MiIntegrationMomentaryClick
    end
    object MiIntegrationShort: TMenuItem
      Caption = '&Short'
      Checked = True
      RadioItem = True
      OnClick = MiIntegrationShortClick
    end
    object MiIntegrationLongTerm: TMenuItem
      Caption = '&Integration'
      RadioItem = True
      OnClick = MiIntegrationLongTermClick
    end
  end
end
