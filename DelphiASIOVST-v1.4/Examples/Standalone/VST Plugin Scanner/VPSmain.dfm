object FmVSTPluginScanner: TFmVSTPluginScanner
  Left = 218
  Top = 81
  Caption = 'VST Plugin Scanner'
  ClientHeight = 584
  ClientWidth = 831
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    831
    584)
  PixelsPerInch = 96
  TextHeight = 13
  object BtScan: TButton
    Left = 786
    Top = 0
    Width = 45
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Scan!'
    Enabled = False
    TabOrder = 3
    OnClick = BtScanClick
  end
  object EdDirectory: TEdit
    Left = 0
    Top = 0
    Width = 710
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = EdDirectoryChange
    OnClick = EdDirectoryClick
  end
  object BtDirectorySelect: TButton
    Left = 711
    Top = 0
    Width = 75
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Select...'
    TabOrder = 1
    OnClick = BtDirectorySelectClick
  end
  object ListView: TListView
    Left = 0
    Top = 21
    Width = 831
    Height = 544
    Align = alBottom
    AllocBy = 1024
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Filename'
        Width = 80
      end
      item
        Caption = 'Unique ID'
        Width = 60
      end
      item
        Caption = 'Programs'
      end
      item
        Caption = 'Parameters'
      end
      item
        Caption = 'Inputs'
        Width = 44
      end
      item
        Caption = 'Outputs'
        Width = 51
      end
      item
        Caption = 'Effect Flags'
      end
      item
        Caption = 'Initial Delay'
      end
      item
        Caption = 'Realtime Qualities'
      end
      item
        Caption = 'Offline Qualities'
      end
      item
        Caption = 'I/O Ratio'
      end
      item
        Caption = 'Version'
      end
      item
        AutoSize = True
        Caption = 'Effect Name'
      end
      item
        AutoSize = True
        Caption = 'Product Name'
      end
      item
        AutoSize = True
        Caption = 'Vendor Name'
      end
      item
        Caption = 'Load Time [ms]'
      end
      item
        Caption = 'Open Time [ms]'
      end>
    GridLines = True
    ReadOnly = True
    TabOrder = 2
    ViewStyle = vsReport
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 565
    Width = 831
    Height = 19
    Panels = <>
  end
end
