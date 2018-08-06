object FmChunkParser: TFmChunkParser
  Left = 286
  Top = 106
  Caption = 'Generic Chunk Parser'
  ClientHeight = 266
  ClientWidth = 410
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 145
    Top = 0
    Height = 266
  end
  object TreeView: TTreeView
    Left = 0
    Top = 0
    Width = 145
    Height = 266
    Align = alLeft
    Indent = 19
    TabOrder = 0
    OnChange = TreeViewChange
  end
  object Memo: TMemo
    Left = 148
    Top = 0
    Width = 262
    Height = 266
    Align = alClient
    TabOrder = 1
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
    object MIFile: TMenuItem
      Caption = '&File'
      object MIOpen: TMenuItem
        Caption = '&Open...'
        OnClick = MIOpenClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MIExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MIExitClick
      end
    end
  end
  object OD: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Select any file'
    Left = 40
    Top = 8
  end
end
