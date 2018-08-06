object FmAbout: TFmAbout
  Left = 384
  Top = 373
  BorderStyle = bsNone
  Caption = 'About'
  ClientHeight = 99
  ClientWidth = 317
  Color = clMedGray
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClick = FormClick
  PixelsPerInch = 96
  TextHeight = 13
  object LbTitle: TLabel
    Left = 24
    Top = 17
    Width = 258
    Height = 32
    Caption = 'Simple Lunchbox'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = FormClick
  end
  object LbCopyright: TLabel
    Left = 88
    Top = 55
    Width = 141
    Height = 13
    Caption = 'originally created by BramBos'
    OnClick = FormClick
  end
  object LbInfo: TLabel
    Left = 86
    Top = 69
    Width = 155
    Height = 13
    Caption = 'completely rewritten as example'
    OnClick = FormClick
  end
end
