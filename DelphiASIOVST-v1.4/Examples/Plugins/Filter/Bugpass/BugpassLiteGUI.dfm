object FmBugpassLite: TFmBugpassLite
  Left = 425
  Top = 147
  BorderStyle = bsNone
  Caption = 'Bugpass Lite'
  ClientHeight = 173
  ClientWidth = 325
  Color = 14733494
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
  DesignSize = (
    325
    173)
  PixelsPerInch = 96
  TextHeight = 13
  object LbTitle: TGuiLabel
    Left = 8
    Top = 8
    Width = 309
    Height = 78
    Alignment = taCenter
    Caption = 'BUGPASS'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 12693147
    Font.Height = -64
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 2.000000000000000000
    Shadow.Color = 3616034
    Shadow.OffsetX = 2
    Shadow.OffsetY = 2
    Shadow.Opacity = 220
    Shadow.Saturation = 3.000000000000000000
    Shadow.Visible = True
    Transparent = True
  end
  object LbFreqLowValue: TGuiLabel
    Left = 8
    Top = 144
    Width = 81
    Height = 21
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    Caption = '1 kHz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 12693147
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    Transparent = True
  end
  object FrequencyBar: TPaintBox
    Left = 8
    Top = 127
    Width = 309
    Height = 16
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseDown = FrequencyBarMouseDown
    OnMouseMove = FrequencyBarMouseMove
    OnMouseUp = FrequencyBarMouseUp
    OnPaint = PaintBoxPaint
  end
  object LbSubTitle: TGuiLabel
    Left = 257
    Top = 86
    Width = 59
    Height = 34
    Alignment = taCenter
    Caption = 'Lite'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 12693147
    Font.Height = -27
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 2.000000000000000000
    Shadow.Color = 3616034
    Shadow.Opacity = 220
    Shadow.Saturation = 3.000000000000000000
    Shadow.Visible = True
    Transparent = True
  end
  object LbFreqHighValue: TGuiLabel
    Left = 236
    Top = 144
    Width = 81
    Height = 21
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    Caption = '1 kHz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 12693147
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    Transparent = True
  end
  object LbVST: TGuiLabel
    Left = 7
    Top = 86
    Width = 61
    Height = 34
    Alignment = taCenter
    Caption = 'VST'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 12693147
    Font.Height = -27
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 2.000000000000000000
    Shadow.Color = 3616034
    Shadow.Opacity = 220
    Shadow.Saturation = 3.000000000000000000
    Shadow.Visible = True
    Transparent = True
  end
end
