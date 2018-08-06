object WavedisplayGUI: TWavedisplayGUI
  Left = 208
  Top = 110
  BorderStyle = bsNone
  Caption = 'WavedisplayGUI'
  ClientHeight = 230
  ClientWidth = 532
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  DesignSize = (
    532
    230)
  PixelsPerInch = 96
  TextHeight = 13
  object Display: TGuiDynamicWaveform
    Left = 0
    Top = 33
    Width = 491
    Height = 197
    RedrawInterval = 50
    InternalBufferSize = 1024
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clWhite
  end
  object LbDrawMode: TLabel
    Left = 8
    Top = 12
    Width = 57
    Height = 13
    Caption = 'Draw mode:'
  end
  object LbWaveSize: TLabel
    Left = 143
    Top = 12
    Width = 53
    Height = 13
    Caption = 'Wave size:'
  end
  object LbProcessingMode: TLabel
    Left = 271
    Top = 12
    Width = 84
    Height = 13
    Caption = 'Processing mode:'
  end
  object LevelMeter: TGuiLevelMeter
    Left = 494
    Top = 33
    Width = 34
    Height = 197
    Anchors = [akTop, akRight, akBottom]
    BarWidthPercentage = 0.800000011920929000
    LevelRelease = 1.000000000000000000
    MaximumTimeFactor = 3.000000000000000000
    RedrawInterval = 15
    ShowClipping = scBottomRight
  end
  object ddDrawMode: TComboBox
    Left = 68
    Top = 8
    Width = 61
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = 'Solid'
    OnChange = ddDrawModeChange
    Items.Strings = (
      'Solid'
      'Outline'
      'Points'
      'Simple')
  end
  object ddWaveSize: TComboBox
    Left = 198
    Top = 8
    Width = 61
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 2
    TabOrder = 1
    Text = '1024'
    OnChange = ddWaveSizeChange
    Items.Strings = (
      '256'
      '512'
      '1024'
      '2048'
      '4096'
      '8192')
  end
  object ddProcessing: TComboBox
    Left = 358
    Top = 8
    Width = 91
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 2
    Text = 'Scrolling'
    OnChange = ddProcessingChange
    Items.Strings = (
      'Scrolling'
      'Replacing'
      'Stretching')
  end
end
