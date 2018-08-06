object FmVSTSetup: TFmVSTSetup
  Left = 551
  Top = 293
  Caption = 'VST Plugin Setup'
  ClientHeight = 330
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListView: TListView
    Left = 0
    Top = 21
    Width = 800
    Height = 290
    Align = alClient
    AllocBy = 1024
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
      end>
    GridLines = True
    ReadOnly = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 311
    Width = 800
    Height = 19
    Panels = <>
  end
  object PnSelect: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 21
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      800
      21)
    object BtDirectorySelect: TButton
      Left = 681
      Top = 0
      Width = 75
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Select...'
      TabOrder = 0
      OnClick = BtDirectorySelectClick
    end
    object BtScan: TButton
      Left = 756
      Top = 0
      Width = 44
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Scan!'
      Enabled = False
      TabOrder = 1
      OnClick = BtScanClick
    end
    object EdDirectory: TEdit
      Left = 0
      Top = 0
      Width = 684
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = EdDirectoryChange
    end
  end
end
