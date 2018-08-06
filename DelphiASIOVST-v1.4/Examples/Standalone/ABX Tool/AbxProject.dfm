object FmProject: TFmProject
  Left = 434
  Top = 317
  Caption = 'Project'
  ClientHeight = 209
  ClientWidth = 385
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 350
  ParentFont = True
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 385
    Height = 209
    ActivePage = TSMeasurementDatabase
    Align = alClient
    TabOrder = 0
    object TSTestInformation: TTabSheet
      Caption = 'Test Information'
      DesignSize = (
        377
        181)
      object LbMaterialTitle: TLabel
        Left = 3
        Top = 6
        Width = 93
        Height = 13
        Caption = 'Listening Test Title:'
      end
      object LbMaterialInfo: TLabel
        Left = 3
        Top = 34
        Width = 93
        Height = 13
        Caption = 'Listening Test Info:'
      end
      object EdTestTitle: TEdit
        Left = 102
        Top = 3
        Width = 272
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = EdTestTitleChange
      end
      object MemoTestInfo: TMemo
        Left = 102
        Top = 30
        Width = 272
        Height = 148
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 1
        OnChange = MemoTestInfoChange
      end
    end
    object TSMeasurementDatabase: TTabSheet
      Caption = 'Test Evaluation Database'
      ImageIndex = 1
      object DBGridPro: TDBGridPro
        Left = 0
        Top = 0
        Width = 377
        Height = 181
        Align = alClient
        ParentColor = False
        PopupMenu = PUDatabase
        TabOrder = 0
        TabStop = True
        DataSource = DataSource
        ReadOnly = True
        StripStep = 2
        Designer = LiteDesigner
        Columns = <
          item
            FieldName = 'Date'
            Width = 91
          end
          item
            FieldName = 'Name/ID'
            Width = 112
          end
          item
            FieldName = 'Rating'
          end
          item
            FieldName = 'Trials'
          end>
        OnDblClick = DBGridProDblClick
        OnGetRowAttrs = DBGridProGetRowAttrs
        OnTitleStateChange = DBGridProTitleStateChange
      end
    end
  end
  object DataSource: TDataSource
    DataSet = Database
    Left = 16
    Top = 72
  end
  object Database: TkbmMemTable
    Active = True
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <
      item
        Name = 'Date'
        DataType = ftDate
      end
      item
        Name = 'Name/ID'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Rating'
        DataType = ftFloat
      end
      item
        Name = 'Trials'
        DataType = ftInteger
      end>
    IndexDefs = <>
    SortOptions = []
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    LoadedCompletely = False
    SavedCompletely = False
    FilterOptions = []
    Version = '5.52'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    DefaultFormat = kbmBinaryStreamFormat
    CommaTextFormat = kbmCSVStreamFormat
    PersistentFormat = kbmBinaryStreamFormat
    AllDataFormat = kbmBinaryStreamFormat
    FormFormat = kbmBinaryStreamFormat
    AfterPost = DatabaseAfterPost
    Left = 80
    Top = 72
    object DatabaseDate: TDateField
      FieldName = 'Date'
    end
    object DatabaseNameID: TStringField
      FieldName = 'Name/ID'
    end
    object DatabaseFieldRating: TFloatField
      FieldName = 'Rating'
    end
    object DatabaseTrials: TIntegerField
      FieldName = 'Trials'
    end
  end
  object LiteDesigner: TLiteDesigner
    VertLines = clSilver
    HorizLines = clSilver
    Left = 48
    Top = 72
  end
  object AdcA: TAudioDataCollection32
    Channels = <>
    SampleRate = 44100.000000000000000000
    Left = 176
    Top = 72
  end
  object AdcB: TAudioDataCollection32
    Channels = <>
    SampleRate = 44100.000000000000000000
    Left = 208
    Top = 72
  end
  object kbmBinaryStreamFormat: TkbmBinaryStreamFormat
    Version = '3.00'
    sfUsingIndex = [sfSaveUsingIndex]
    sfData = [sfSaveData, sfLoadData]
    sfCalculated = []
    sfLookup = []
    sfNonVisible = [sfSaveNonVisible, sfLoadNonVisible]
    sfBlobs = [sfSaveBlobs, sfLoadBlobs]
    sfDef = [sfSaveDef, sfLoadDef]
    sfIndexDef = [sfSaveIndexDef, sfLoadIndexDef]
    sfFiltered = [sfSaveFiltered]
    sfIgnoreRange = [sfSaveIgnoreRange]
    sfIgnoreMasterDetail = [sfSaveIgnoreMasterDetail]
    sfDeltas = []
    sfDontFilterDeltas = []
    sfAppend = []
    sfFieldKind = [sfSaveFieldKind]
    sfFromStart = [sfLoadFromStart]
    sfDataTypeHeader = [sfSaveDataTypeHeader, sfLoadDataTypeHeader]
    BufferSize = 16384
    Left = 112
    Top = 72
  end
  object kbmCSVStreamFormat: TkbmCSVStreamFormat
    CSVQuote = '"'
    CSVFieldDelimiter = ','
    CSVRecordDelimiter = ','
    CSVTrueString = 'True'
    CSVFalseString = 'False'
    sfLocalFormat = []
    sfQuoteOnlyStrings = []
    sfNoHeader = []
    Version = '3.00'
    sfData = [sfSaveData, sfLoadData]
    sfCalculated = []
    sfLookup = []
    sfNonVisible = [sfSaveNonVisible, sfLoadNonVisible]
    sfBlobs = [sfSaveBlobs, sfLoadBlobs]
    sfDef = [sfSaveDef, sfLoadDef]
    sfIndexDef = [sfSaveIndexDef, sfLoadIndexDef]
    sfPlaceHolders = []
    sfFiltered = [sfSaveFiltered]
    sfIgnoreRange = [sfSaveIgnoreRange]
    sfIgnoreMasterDetail = [sfSaveIgnoreMasterDetail]
    sfDeltas = []
    sfDontFilterDeltas = []
    sfAppend = []
    sfFieldKind = [sfSaveFieldKind]
    sfFromStart = [sfLoadFromStart]
    Left = 144
    Top = 72
  end
  object PUDatabase: TPopupMenu
    Left = 240
    Top = 72
    object MIDeleteRecord: TMenuItem
      Caption = '&Delete Record'
      OnClick = MIDeleteRecordClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MIFilterDate: TMenuItem
      Caption = 'Filter &Date'
      Visible = False
      OnClick = MIFilterDateClick
    end
    object N2: TMenuItem
      Caption = '-'
      Visible = False
    end
    object MIAllowEdit: TMenuItem
      Caption = 'Allow Edit'
      OnClick = MIAllowEditClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object MIUndo: TMenuItem
      Caption = '&Undo'
      OnClick = MIUndoClick
    end
  end
end
