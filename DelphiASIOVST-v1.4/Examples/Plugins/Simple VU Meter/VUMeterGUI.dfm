object VSTVUMeterGUI: TVSTVUMeterGUI
  Left = 243
  Top = 108
  BorderStyle = bsNone
  Caption = 'Simple VU Meter'
  ClientHeight = 208
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object LbAbout: TLabel
    Left = 80
    Top = 104
    Width = 302
    Height = 65
    Alignment = taCenter
    Caption = 
      'The first 2 sliders adjust the volume of the left and right chan' +
      'nels'#13#10' '#13#10'This is only a demonstration plugin!'#13#10' '#13#10'(C)opyright in' +
      ' 2004-2006 by Tobias Fleischer (www.tobybear.de)'
  end
  object LbTrademark: TLabel
    Left = 88
    Top = 192
    Width = 282
    Height = 13
    Caption = 'VST is a trademark of Steinberg Media Technologies GmbH'
  end
  object ShVULeft: TShape
    Left = 120
    Top = 62
    Width = 17
    Height = 12
  end
  object ShVURight: TShape
    Left = 120
    Top = 78
    Width = 17
    Height = 12
  end
  object LbGainLeft: TLabel
    Left = 8
    Top = 62
    Width = 78
    Height = 13
    Caption = 'left gain: 0 db(fs)'
  end
  object LbGainRight: TLabel
    Left = 8
    Top = 78
    Width = 84
    Height = 13
    Caption = 'right gain: 0 db(fs)'
  end
  object SBLeft: TScrollBar
    Left = 8
    Top = 8
    Width = 425
    Height = 16
    Max = 0
    Min = -90
    PageSize = 0
    TabOrder = 0
    OnChange = ParameterChange
  end
  object SBRight: TScrollBar
    Tag = 1
    Left = 8
    Top = 40
    Width = 425
    Height = 16
    Max = 0
    Min = -90
    PageSize = 0
    TabOrder = 1
    OnChange = ParameterChange
  end
  object Timer: TTimer
    Interval = 25
    OnTimer = TimerTimer
    Left = 16
    Top = 112
  end
end
