object FmPluginMerger: TFmPluginMerger
  Left = 469
  Top = 222
  Caption = 'Plugin Merger'
  ClientHeight = 177
  ClientWidth = 269
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 269
    Height = 177
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TSMergedPlugins: TTabSheet
      Caption = 'Merged Plugins'
      DesignSize = (
        261
        149)
      object LBPlugins: TListBox
        Left = -25
        Top = 3
        Width = 286
        Height = 174
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnClick = LBPluginsClick
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'GUI Elements'
      ImageIndex = 1
      DesignSize = (
        261
        149)
      object LbKnob: TLabel
        Left = 8
        Top = 8
        Width = 63
        Height = 13
        Caption = 'Knob Bitmap:'
      end
      object LbBackgroundColor: TLabel
        Left = 8
        Top = 60
        Width = 88
        Height = 13
        Caption = 'Background Color:'
      end
      object ShBackgroundColor: TShape
        Left = 102
        Top = 60
        Width = 13
        Height = 13
        Brush.Color = clSilver
        OnMouseDown = ShBackgroundColorMouseDown
      end
      object LbKnobsPerRow: TLabel
        Left = 8
        Top = 35
        Width = 73
        Height = 13
        Caption = 'Knobs per row:'
      end
      object LBFont: TLabel
        Left = 8
        Top = 79
        Width = 54
        Height = 13
        Caption = 'Font Color:'
      end
      object ShFontColor: TShape
        Left = 102
        Top = 79
        Width = 13
        Height = 13
        Brush.Color = clBlack
        OnMouseDown = ShBackgroundColorMouseDown
      end
      object LbFontSize: TLabel
        Left = 8
        Top = 127
        Width = 48
        Height = 13
        Caption = 'Font Size:'
      end
      object LbFontAA: TLabel
        Left = 8
        Top = 103
        Width = 43
        Height = 13
        Caption = 'Font AA:'
      end
      object EdKnob: TEdit
        Left = 72
        Top = 5
        Width = 186
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = EdKnobChange
        OnClick = EdKnobClick
      end
      object SEKnobsPerRow: TSpinEdit
        Left = 87
        Top = 32
        Width = 42
        Height = 22
        MaxValue = 10
        MinValue = 1
        TabOrder = 1
        Value = 5
      end
      object GBPreview: TGuiGroup
        Left = 135
        Top = 32
        Width = 122
        Height = 60
        Caption = 'Preview'
        Color = clSilver
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        PanelColor = clSilver
        ParentColor = False
        ParentFont = False
        BorderRadius = 3
        TabOrder = 2
        Transparent = True
        object DialPreview: TGuiDial
          Left = 64
          Top = 6
          Width = 48
          Height = 48
          DialImageIndex = -1
          LineColor = clBtnShadow
          LineWidth = 2
          Max = 100.000000000000000000
          PointerAngles.Start = 225
          PointerAngles.Range = 270
          PointerAngles.Resolution = 270.000000000000000000
          ScrollRange_Pixel = 400.000000000000000000
          StitchKind = skHorizontal
          WheelStep = 1.000000000000000000
        end
        object LbTest: TGuiLabel
          Left = 6
          Top = 21
          Width = 56
          Height = 33
          Caption = 'Test'
          Shadow.Color = clBlack
        end
      end
      object SEFontSize: TSpinEdit
        Left = 77
        Top = 124
        Width = 52
        Height = 22
        MaxValue = 96
        MinValue = 4
        TabOrder = 3
        Value = 8
        OnChange = SEFontSizeChange
      end
      object RBAAnone: TRadioButton
        Left = 64
        Top = 102
        Width = 45
        Height = 17
        Caption = 'none'
        Checked = True
        TabOrder = 4
        TabStop = True
        OnClick = RBAAnoneClick
      end
      object RBAA4: TRadioButton
        Left = 152
        Top = 102
        Width = 34
        Height = 17
        Caption = '4x'
        TabOrder = 5
        OnClick = RBAA4Click
      end
      object RBAA2: TRadioButton
        Left = 112
        Top = 102
        Width = 34
        Height = 17
        Caption = '2x'
        TabOrder = 6
        OnClick = RBAA2Click
      end
      object RBAA8: TRadioButton
        Left = 192
        Top = 102
        Width = 34
        Height = 17
        Caption = '8x'
        TabOrder = 7
        OnClick = RBAA8Click
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 1
    object MIFile: TMenuItem
      Caption = 'File'
      object MISaveasVST: TMenuItem
        Caption = 'Save as VST...'
        Enabled = False
        OnClick = MISaveasVSTClick
      end
      object MIExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MIExitClick
      end
    end
    object MIPlugin: TMenuItem
      Caption = 'Plugin'
      object MIAdd: TMenuItem
        Caption = 'Add...'
        OnClick = MIAddClick
      end
      object MIClear: TMenuItem
        Caption = 'Clear'
        OnClick = MIClearClick
      end
    end
  end
end
