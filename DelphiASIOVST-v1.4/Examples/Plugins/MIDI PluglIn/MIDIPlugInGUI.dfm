object VSTGUI: TVSTGUI
  Left = 321
  Top = 145
  BorderStyle = bsNone
  Caption = 'VSTGUI'
  ClientHeight = 174
  ClientWidth = 442
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  DesignSize = (
    442
    174)
  PixelsPerInch = 96
  TextHeight = 13
  object LbInstructions: TLabel
    Left = 48
    Top = 56
    Width = 321
    Height = 65
    Alignment = taCenter
    Caption = 
      'Use the slider to adjust the transpose value for incoming MIDI n' +
      'otes!'#13#10' '#13#10'This is only a demonstration plugin!'#13#10' '#13#10'(C)opyright i' +
      'n 2004/2005 by Tobias Fleischer (www.tobybear.de)'
  end
  object LbVSTTrademark: TLabel
    Left = 64
    Top = 152
    Width = 282
    Height = 13
    Caption = 'VST is a trademark of Steinberg Media Technologies GmbH'
  end
  object LbTranspose: TLabel
    Left = 8
    Top = 8
    Width = 58
    Height = 13
    Caption = 'transpose: 0'
  end
  object par0: TScrollBar
    Left = 8
    Top = 24
    Width = 425
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Max = 24
    Min = -24
    PageSize = 0
    TabOrder = 0
    OnChange = par0Change
  end
end
