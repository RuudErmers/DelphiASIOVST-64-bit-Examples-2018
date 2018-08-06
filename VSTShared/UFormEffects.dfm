object FormEffects: TFormEffects
  Left = 0
  Top = 0
  Caption = 'Effects'
  ClientHeight = 601
  ClientWidth = 1046
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 120
  TextHeight = 16
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1046
    Height = 560
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 560
    Width = 1046
    Height = 41
    Align = alBottom
    TabOrder = 1
    object ButtonPluginEditor: TButton
      Left = 24
      Top = 6
      Width = 89
      Height = 25
      Caption = 'Plugin Editor'
      TabOrder = 0
    end
    object ButtonListEditor: TButton
      Left = 136
      Top = 6
      Width = 89
      Height = 25
      Caption = 'List Editor'
      TabOrder = 1
    end
  end
end
