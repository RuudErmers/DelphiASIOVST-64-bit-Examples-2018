object FmProject: TFmProject
  Left = 326
  Top = 190
  Caption = 'Project'
  ClientHeight = 300
  ClientWidth = 632
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 3
    Top = 0
    Height = 300
    OnCanResize = SplitterCanResize
  end
  object Splitter2: TSplitter
    Tag = 1
    Left = 0
    Top = 0
    Height = 300
    OnCanResize = SplitterCanResize
  end
  object Splitter3: TSplitter
    Tag = 2
    Left = 6
    Top = 0
    Height = 300
    OnCanResize = SplitterCanResize
  end
  object Splitter4: TSplitter
    Tag = 3
    Left = 9
    Top = 0
    Height = 300
    OnCanResize = SplitterCanResize
  end
  object TvSource: TTreeView
    Left = 8
    Top = 0
    Width = 121
    Height = 300
    Align = alCustom
    Indent = 19
    PopupMenu = PuSource
    TabOrder = 0
    Items.NodeData = {
      03010000002A0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      0000000000010653006F007500720063006500}
  end
  object TvFilter: TTreeView
    Left = 136
    Top = 0
    Width = 121
    Height = 300
    Align = alCustom
    Indent = 19
    TabOrder = 1
    Items.NodeData = {
      03010000002A0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      00000000000106460069006C00740065007200}
  end
  object TvAnalysis: TTreeView
    Left = 263
    Top = 0
    Width = 121
    Height = 300
    Align = alCustom
    Indent = 19
    PopupMenu = PuAnalysis
    TabOrder = 2
    OnChange = TvAnalysisChange
    Items.NodeData = {
      03010000002E0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      0000000000010841006E0061006C007900730069007300}
  end
  object TvStatistic: TTreeView
    Left = 390
    Top = 0
    Width = 121
    Height = 300
    Align = alCustom
    Indent = 19
    TabOrder = 3
    Items.NodeData = {
      0301000000300000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      00000000000109530074006100740069007300740069006300}
  end
  object TvDestination: TTreeView
    Left = 517
    Top = 0
    Width = 112
    Height = 300
    Align = alCustom
    Indent = 19
    PopupMenu = PuDestination
    TabOrder = 4
    Items.NodeData = {
      0301000000340000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      0000000000010B440065007300740069006E006100740069006F006E00}
  end
  object PuSource: TPopupMenu
    Left = 48
    Top = 32
    object MiSourceAddFile: TMenuItem
      Caption = '&Add File...'
      ShortCut = 187
      OnClick = MiSourceAddFileClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MiSourceAddFolder: TMenuItem
      Caption = 'Add &Folder'
      OnClick = MiSourceAddFolderClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MiSourceRename: TMenuItem
      Caption = '&Rename'
      ShortCut = 113
      OnClick = MiSourceRenameClick
    end
    object MiSourceDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 8238
      OnClick = MiSourceDeleteClick
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object MiSourceProperties: TMenuItem
      Caption = '&Properties'
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.wav'
    Filter = 'Wave (*.wav)|*.wav|AIFF (*.aiff)|*.aiff|AU (*.au)|*.au'
    Title = 'Add Source File...'
    Left = 48
    Top = 88
  end
  object PuAnalysis: TPopupMenu
    Left = 296
    Top = 32
    object MiThirdOctave: TMenuItem
      Caption = 'Add &Third Octave '
      SubMenuImages = FmArtumes.ImageList
      ShortCut = 16435
      OnClick = MiThirdOctaveClick
    end
    object MiOctave: TMenuItem
      Caption = 'Add &Octave'
      ShortCut = 16440
      OnClick = MiOctaveClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object MiAnalysisAddFolder: TMenuItem
      Caption = 'Add &Folder'
      OnClick = MiAnalysisAddFolderClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object MiAnalysisRename: TMenuItem
      Caption = '&Rename'
      ShortCut = 113
      OnClick = MiAnalysisRenameClick
    end
    object MiAnalysisDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 8238
      OnClick = MiAnalysisDeleteClick
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object MiAnalysisProperties: TMenuItem
      Caption = '&Properties'
      OnClick = MiAnalysisPropertiesClick
    end
  end
  object PuDestination: TPopupMenu
    Left = 552
    Top = 32
    object MiDestinationCSV: TMenuItem
      Caption = '&CSV'
      OnClick = MiDestinationCSVClick
    end
    object MiDestinationExcel: TMenuItem
      Caption = '&Excel'
      OnClick = MiDestinationExcelClick
    end
    object MiDestinationWAV: TMenuItem
      Caption = '&WAV'
      OnClick = MiDestinationWAVClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object MiDestinationAddFolder: TMenuItem
      Caption = 'Add &Folder'
      OnClick = MiDestinationAddFolderClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object MiDestinationRename: TMenuItem
      Caption = '&Rename'
      ShortCut = 113
      OnClick = MiDestinationRenameClick
    end
    object MiDestinationDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 8238
      OnClick = MiDestinationDeleteClick
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object MiDestinationProperties: TMenuItem
      Caption = '&Properties'
    end
  end
end
