object FmModDelay: TFmModDelay
  Left = 218
  Top = 77
  BorderStyle = bsNone
  Caption = 'Mod Delay'
  ClientHeight = 173
  ClientWidth = 320
  Color = 7373965
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbGain: TGuiLabel
    Left = 8
    Top = 8
    Width = 41
    Height = 17
    Caption = 'Gain:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object LbMix: TGuiLabel
    Left = 8
    Top = 31
    Width = 41
    Height = 17
    Caption = 'Mix:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object LbLpf: TGuiLabel
    Left = 8
    Top = 54
    Width = 41
    Height = 17
    Caption = 'LPF:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object LbDelay: TGuiLabel
    Left = 8
    Top = 77
    Width = 41
    Height = 17
    Caption = 'Delay:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object LbDepth: TGuiLabel
    Left = 8
    Top = 100
    Width = 46
    Height = 17
    Caption = 'Depth:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object LbRate: TGuiLabel
    Left = 8
    Top = 123
    Width = 41
    Height = 17
    Caption = 'Rate:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object LbFeedback: TGuiLabel
    Left = 8
    Top = 146
    Width = 69
    Height = 17
    Caption = 'Feedback:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object LbGainValue: TGuiLabel
    Left = 213
    Top = 8
    Width = 100
    Height = 17
    Alignment = taCenter
    Caption = '-'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object LbMixValue: TGuiLabel
    Left = 213
    Top = 31
    Width = 100
    Height = 17
    Alignment = taCenter
    Caption = '-'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object LbLpfValue: TGuiLabel
    Left = 213
    Top = 54
    Width = 100
    Height = 17
    Alignment = taCenter
    Caption = '-'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object LbDelayValue: TGuiLabel
    Left = 213
    Top = 77
    Width = 100
    Height = 17
    Alignment = taCenter
    Caption = '-'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object LbDepthValue: TGuiLabel
    Left = 213
    Top = 100
    Width = 100
    Height = 17
    Alignment = taCenter
    Caption = '-'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object LbRateValue: TGuiLabel
    Left = 213
    Top = 123
    Width = 100
    Height = 17
    Alignment = taCenter
    Caption = '-'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object LbFeedbackValue: TGuiLabel
    Left = 213
    Top = 146
    Width = 100
    Height = 17
    Alignment = taCenter
    Caption = '-'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
  end
  object SbGain: TScrollBar
    Left = 83
    Top = 8
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 0
    OnChange = SbGainChange
  end
  object ScrollBar2: TScrollBar
    Left = 83
    Top = 31
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 1
  end
  object ScrollBar3: TScrollBar
    Left = 83
    Top = 54
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 2
  end
  object ScrollBar4: TScrollBar
    Left = 83
    Top = 77
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 3
  end
  object ScrollBar5: TScrollBar
    Left = 83
    Top = 100
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 4
  end
  object ScrollBar6: TScrollBar
    Left = 83
    Top = 123
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 5
  end
  object ScrollBar7: TScrollBar
    Left = 83
    Top = 146
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 6
  end
end
