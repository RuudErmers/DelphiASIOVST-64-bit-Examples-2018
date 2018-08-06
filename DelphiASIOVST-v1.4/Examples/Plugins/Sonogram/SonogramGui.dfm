object FmSonogram: TFmSonogram
  Left = 351
  Top = 167
  BorderStyle = bsNone
  Caption = 'Sonogram'
  ClientHeight = 287
  ClientWidth = 272
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbFftOrder: TLabel
    Left = 8
    Top = 269
    Width = 62
    Height = 13
    Caption = 'FFT Order: 8'
    PopupMenu = PuFftOrder
    Transparent = True
  end
  object LbOverlapFactor: TLabel
    Left = 182
    Top = 269
    Width = 82
    Height = 13
    Caption = 'Overlap Order: 4'
    PopupMenu = PuOverlapFactor
    Transparent = True
  end
  object Timer: TTimer
    Interval = 25
    OnTimer = TimerTimer
    Left = 16
    Top = 16
  end
  object PuFftOrder: TPopupMenu
    Left = 48
    Top = 16
    object MiOrder6: TMenuItem
      Tag = 6
      Caption = '6 (= 64 Samples)'
      RadioItem = True
      OnClick = MiOrderClick
    end
    object MiOrder7: TMenuItem
      Tag = 7
      Caption = '7 (= 128 Samples)'
      RadioItem = True
      OnClick = MiOrderClick
    end
    object MiOrder8: TMenuItem
      Tag = 8
      Caption = '8 (= 256 Samples)'
      RadioItem = True
      OnClick = MiOrderClick
    end
    object MiOrder9: TMenuItem
      Tag = 9
      Caption = '9 (= 512 Samples)'
      RadioItem = True
      OnClick = MiOrderClick
    end
    object MiOrder10: TMenuItem
      Tag = 10
      Caption = '10 (= 1024 Samples)'
      RadioItem = True
      OnClick = MiOrderClick
    end
    object MiOrder11: TMenuItem
      Tag = 11
      Caption = '11 (= 2048 Samples)'
      RadioItem = True
      OnClick = MiOrderClick
    end
    object MiOrder12: TMenuItem
      Tag = 12
      Caption = '12 (= 4096 Samples)'
      RadioItem = True
      OnClick = MiOrderClick
    end
    object MiOrder13: TMenuItem
      Tag = 13
      Caption = '13 (= 8192 Samples)'
      RadioItem = True
      OnClick = MiOrderClick
    end
    object MiOrder14: TMenuItem
      Tag = 14
      Caption = '14 (= 16384 Samples)'
      RadioItem = True
      Visible = False
      OnClick = MiOrderClick
    end
  end
  object PuOverlapFactor: TPopupMenu
    Left = 80
    Top = 16
    object MiOverlapOrder1: TMenuItem
      Tag = 1
      Caption = '1 (no 0verlap)'
      RadioItem = True
      OnClick = MiOverlapOrderClick
    end
    object MiOverlapOrder2: TMenuItem
      Tag = 2
      Caption = '2 (half)'
      RadioItem = True
      OnClick = MiOverlapOrderClick
    end
    object MiOverlapOrder3: TMenuItem
      Tag = 3
      Caption = '3 (quarter)'
      RadioItem = True
      OnClick = MiOverlapOrderClick
    end
    object MiOverlapOrder4: TMenuItem
      Tag = 4
      Caption = '4 (1/8th)'
      RadioItem = True
      OnClick = MiOverlapOrderClick
    end
    object MiOverlapOrder5: TMenuItem
      Tag = 5
      Caption = '5 (1/16th)'
      RadioItem = True
      OnClick = MiOverlapOrderClick
    end
    object MiOverlapOrder6: TMenuItem
      Tag = 6
      Caption = '6 (1/32th)'
      RadioItem = True
      OnClick = MiOverlapOrderClick
    end
    object MiOverlapOrder7: TMenuItem
      Tag = 7
      Caption = '7 (1/64th)'
      RadioItem = True
      OnClick = MiOverlapOrderClick
    end
  end
end
