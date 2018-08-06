object VSTGUI: TVSTGUI
  Left = 372
  Top = 170
  BorderStyle = bsNone
  Caption = 'Simple OpAmp Simulation'
  ClientHeight = 48
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object LbGain: TLabel
    Left = 152
    Top = 32
    Width = 72
    Height = 13
    Caption = 'OpAmp Gain'
    OnClick = LbGainClick
  end
  object SBGain: TScrollBar
    Left = 8
    Top = 8
    Width = 369
    Height = 17
    Max = 200
    Min = -400
    PageSize = 0
    Position = 20
    TabOrder = 0
    OnChange = SBGainChange
  end
end
