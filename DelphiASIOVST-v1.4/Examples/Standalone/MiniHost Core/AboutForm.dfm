object FmAbout: TFmAbout
  Left = 370
  Top = 241
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 314
  ClientWidth = 225
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    225
    314)
  PixelsPerInch = 96
  TextHeight = 14
  object LbTitle: TLabel
    Left = 46
    Top = 16
    Width = 129
    Height = 14
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Tobybear MiniHost 1.0.1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LbCopyright: TLabel
    Left = 4
    Top = 184
    Width = 216
    Height = 42
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      '(C)opyright in 2004-2009 by Tobias Fleischer'#13#10'based on the Delph' +
      'i ASIO && VST '#13#10'host code by Christian Budde'
  end
  object LbMail: TLabel
    Left = 40
    Top = 160
    Width = 101
    Height = 14
    Cursor = crHandPoint
    Anchors = [akLeft, akTop, akRight]
    Caption = 'tobybear@web.de'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = LbMailClick
  end
  object LbWeb: TLabel
    Left = 40
    Top = 144
    Width = 129
    Height = 14
    Cursor = crHandPoint
    Anchors = [akLeft, akTop, akRight]
    Caption = 'http://www.tobybear.de'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = LbWebClick
  end
  object LbDonate: TLabel
    Left = 24
    Top = 120
    Width = 175
    Height = 14
    Cursor = crHandPoint
    Anchors = [akLeft, akTop, akRight]
    Caption = '[Click here to donate via PayPal]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = LbDonateClick
  end
  object Lbml: TLabel
    Left = 8
    Top = 160
    Width = 27
    Height = 14
    Anchors = [akLeft, akTop, akRight]
    Caption = 'email:'
  end
  object LbTrademarks: TLabel
    Left = 40
    Top = 240
    Width = 150
    Height = 28
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    Caption = 'VST and ASIO are registered '#13#10'trademarks by Steinberg GmbH'
  end
  object LbHours: TLabel
    Left = 8
    Top = 64
    Width = 203
    Height = 42
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Countless hours of coding were needed'#13#10'to make this little appli' +
      'cation, so please '#13#10'donate some money if you use it regularly!'
  end
  object LbReadManual: TLabel
    Left = 8
    Top = 36
    Width = 203
    Height = 14
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Read the included manual for detailed help'
  end
  object LbWb: TLabel
    Left = 8
    Top = 144
    Width = 25
    Height = 14
    Anchors = [akLeft, akTop, akRight]
    Caption = 'web:'
  end
  object BtOK: TButton
    Left = 72
    Top = 280
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = BtOKClick
  end
end
