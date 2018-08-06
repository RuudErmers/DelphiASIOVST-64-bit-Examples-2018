object FmPerformanceTest: TFmPerformanceTest
  Left = 325
  Top = 157
  BorderStyle = bsNone
  Caption = 'Performance Test'
  ClientHeight = 38
  ClientWidth = 388
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  DesignSize = (
    388
    38)
  PixelsPerInch = 96
  TextHeight = 13
  object LbPerformance: TLabel
    Left = 143
    Top = 13
    Width = 94
    Height = 13
    Caption = 'Cycles per Sample: '
  end
  object LbCycles: TLabel
    Left = 243
    Top = 13
    Width = 138
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '0'
  end
  object BtPatchFunctionCalls: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = '&Patch Function Calls'
    TabOrder = 0
    OnClick = BtPatchFunctionCallsClick
  end
  object Timer: TTimer
    Interval = 50
    OnTimer = TimerTimer
    Left = 8
    Top = 8
  end
end
