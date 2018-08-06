object FmHRTFEditor: TFmHRTFEditor
  Left = 337
  Top = 90
  BorderStyle = bsSingle
  Caption = 'HRTF editor'
  ClientHeight = 386
  ClientWidth = 600
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 600
    Height = 386
    ActivePage = TSHrtfData
    Align = alClient
    MultiLine = True
    TabOrder = 0
    object TSGeneralInfo: TTabSheet
      Caption = 'General Information'
      ImageIndex = 1
      DesignSize = (
        592
        340)
      object LbTitle: TLabel
        Left = 8
        Top = 8
        Width = 24
        Height = 13
        Caption = 'Title:'
      end
      object LbContext: TLabel
        Left = 8
        Top = 35
        Width = 43
        Height = 13
        Caption = 'Context:'
      end
      object LbCopyright: TLabel
        Left = 8
        Top = 62
        Width = 51
        Height = 13
        Caption = 'Copyright:'
      end
      object LbAuthor: TLabel
        Left = 8
        Top = 89
        Width = 37
        Height = 13
        Caption = 'Author:'
      end
      object LbNotes: TLabel
        Left = 8
        Top = 116
        Width = 32
        Height = 13
        Caption = 'Notes:'
      end
      object LbDate: TLabel
        Left = 8
        Top = 142
        Width = 27
        Height = 13
        Caption = 'Date:'
      end
      object LbYear: TLabel
        Left = 65
        Top = 143
        Width = 26
        Height = 13
        Caption = 'Year:'
      end
      object LbMonth: TLabel
        Left = 65
        Top = 171
        Width = 34
        Height = 13
        Caption = 'Month:'
      end
      object LbMonthName: TLabel
        Left = 151
        Top = 171
        Width = 31
        Height = 13
        Caption = '(Date)'
        Visible = False
      end
      object LbFullDate: TLabel
        Left = 65
        Top = 203
        Width = 62
        Height = 19
        Caption = 'Full Date'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object EdTitle: TEdit
        Left = 65
        Top = 5
        Width = 507
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = EdTitleChange
      end
      object EdContext: TEdit
        Left = 65
        Top = 32
        Width = 507
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = EdContextChange
      end
      object EdCopyright: TEdit
        Left = 65
        Top = 59
        Width = 507
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = EdCopyrightChange
      end
      object EdAuthor: TEdit
        Left = 65
        Top = 86
        Width = 507
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnChange = EdAuthorChange
      end
      object EdNotes: TEdit
        Left = 65
        Top = 113
        Width = 507
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnChange = EdNotesChange
      end
      object Calendar: TCalendar
        Left = 201
        Top = 140
        Width = 320
        Height = 120
        StartOfWeek = 0
        TabOrder = 5
        UseCurrentDate = False
        OnChange = CalendarChange
      end
      object SEMonth: TSpinEdit
        Left = 105
        Top = 168
        Width = 40
        Height = 22
        MaxValue = 12
        MinValue = 1
        TabOrder = 6
        Value = 4
        OnChange = SEMonthChange
      end
      object SEYear: TSpinEdit
        Left = 97
        Top = 140
        Width = 90
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 7
        Value = 2009
        OnChange = SEYearChange
      end
    end
    object TSHrtfData: TTabSheet
      Caption = 'HRTF Data'
      OnResize = TSHrtfDataResize
      DesignSize = (
        592
        340)
      object LbHrtfIndex: TLabel
        Left = 3
        Top = 6
        Width = 61
        Height = 13
        Caption = 'HRTF Index:'
        Enabled = False
      end
      object LbAzimuth: TLabel
        Left = 135
        Top = 6
        Width = 42
        Height = 13
        Caption = 'Azimuth:'
        Enabled = False
      end
      object LbPolar: TLabel
        Left = 262
        Top = 6
        Width = 28
        Height = 13
        Caption = 'Polar:'
        Enabled = False
      end
      object LbAzimuthUnit: TLabel
        Left = 244
        Top = 6
        Width = 5
        Height = 13
        Caption = #176
        Enabled = False
      end
      object LbPolarUnit: TLabel
        Left = 358
        Top = 6
        Width = 5
        Height = 13
        Caption = #176
        Enabled = False
      end
      object AudioDataDisplayLeft: TGuiAudioDataDisplay
        Left = 3
        Top = 30
        Width = 586
        Height = 110
        Anchors = [akLeft, akTop, akRight]
        AudioDataCollection = ADHRIR
        DisplayedChannel = 0
        DisplayChannels = <>
        LineWidth = 0
        XAxis.SampleUpper = 511
        XAxis.FractionalLower = -0.500000000000000000
        XAxis.FractionalUpper = 0.500000000000000000
      end
      object AudioDataDisplayRight: TGuiAudioDataDisplay
        Left = 3
        Top = 228
        Width = 586
        Height = 110
        Anchors = [akLeft, akRight, akBottom]
        AudioDataCollection = ADHRIR
        DisplayedChannel = 1
        DisplayChannels = <>
        LineWidth = 0
        XAxis.SampleUpper = 511
        XAxis.FractionalLower = -0.500000000000000000
        XAxis.FractionalUpper = 0.500000000000000000
      end
      object SEHrtfIndex: TSpinEdit
        Left = 70
        Top = 3
        Width = 59
        Height = 22
        Enabled = False
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnChange = SEHrtfIndexChange
      end
      object SEAzimuth: TSpinEdit
        Left = 183
        Top = 3
        Width = 59
        Height = 22
        Enabled = False
        MaxValue = 360
        MinValue = -360
        TabOrder = 3
        Value = 0
        OnChange = SEHrirPosChange
      end
      object SEPolar: TSpinEdit
        Left = 296
        Top = 3
        Width = 59
        Height = 22
        Enabled = False
        MaxValue = 180
        MinValue = -180
        TabOrder = 4
        Value = 0
        OnChange = SEHrirPosChange
      end
      object BtExport: TButton
        Left = 458
        Top = 3
        Width = 75
        Height = 22
        Caption = 'Export...'
        TabOrder = 5
        OnClick = BtExportClick
      end
      object BtImport: TButton
        Left = 377
        Top = 3
        Width = 75
        Height = 22
        Caption = 'Import...'
        TabOrder = 6
        OnClick = BtImportClick
      end
    end
    object TSSubjectInfo: TTabSheet
      Caption = 'Subject Information'
      ImageIndex = 2
      DesignSize = (
        592
        340)
      object LbNameID: TLabel
        Left = 8
        Top = 8
        Width = 46
        Height = 13
        Caption = 'Name/ID:'
      end
      object LbDescription: TLabel
        Left = 8
        Top = 64
        Width = 57
        Height = 13
        Caption = 'Description:'
      end
      object LbSex: TLabel
        Left = 8
        Top = 37
        Width = 22
        Height = 13
        Caption = 'Sex:'
      end
      object EdNameID: TEdit
        Left = 65
        Top = 5
        Width = 507
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = EdNameIDChange
      end
      object EdDescription: TEdit
        Left = 65
        Top = 61
        Width = 507
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = EdDescriptionChange
      end
      object RbGeneric: TRadioButton
        Left = 135
        Top = 36
        Width = 58
        Height = 17
        Caption = 'Generic'
        TabOrder = 2
        OnClick = RbGenericClick
      end
      object RbUnknown: TRadioButton
        Left = 65
        Top = 36
        Width = 64
        Height = 17
        Caption = 'Unknown'
        Checked = True
        TabOrder = 3
        TabStop = True
        OnClick = RbUnknownClick
      end
      object RbFemale: TRadioButton
        Left = 239
        Top = 36
        Width = 58
        Height = 17
        Caption = 'Female'
        TabOrder = 4
        OnClick = RbFemaleClick
      end
      object RbMale: TRadioButton
        Left = 193
        Top = 36
        Width = 40
        Height = 17
        Caption = 'Male'
        TabOrder = 5
        OnClick = RbMaleClick
      end
    end
    object TSRoomInfo: TTabSheet
      Caption = 'Room Information'
      ImageIndex = 3
      DesignSize = (
        592
        340)
      object LbRoomType: TLabel
        Left = 8
        Top = 39
        Width = 58
        Height = 13
        Caption = 'Room Type:'
      end
      object LbLength: TLabel
        Left = 8
        Top = 11
        Width = 37
        Height = 13
        Caption = 'Length:'
      end
      object LbLengthUnit: TLabel
        Left = 115
        Top = 11
        Width = 13
        Height = 13
        Caption = 'cm'
      end
      object LbWidthUnit: TLabel
        Left = 244
        Top = 11
        Width = 13
        Height = 13
        Caption = 'cm'
      end
      object LbWidth: TLabel
        Left = 142
        Top = 11
        Width = 32
        Height = 13
        Caption = 'Width:'
      end
      object LbHeightUnit: TLabel
        Left = 372
        Top = 11
        Width = 13
        Height = 13
        Caption = 'cm'
      end
      object LbHeight: TLabel
        Left = 270
        Top = 11
        Width = 35
        Height = 13
        Caption = 'Height:'
      end
      object EdRoomType: TEdit
        Left = 71
        Top = 36
        Width = 501
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = EdRoomTypeChange
      end
      object SELength: TSpinEdit
        Left = 51
        Top = 8
        Width = 58
        Height = 22
        MaxValue = 99999999
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnChange = SELengthChange
      end
      object SEWidth: TSpinEdit
        Left = 180
        Top = 8
        Width = 58
        Height = 22
        MaxValue = 99999999
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnChange = SEWidthChange
      end
      object SEHeight: TSpinEdit
        Left = 308
        Top = 8
        Width = 58
        Height = 22
        MaxValue = 99999999
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnChange = SEHeightChange
      end
    end
    object TSMicrophoneInfo: TTabSheet
      Caption = 'Microphone Information'
      ImageIndex = 4
      DesignSize = (
        592
        340)
      object LbMicType: TLabel
        Left = 8
        Top = 8
        Width = 86
        Height = 13
        Caption = 'Microphone Type:'
      end
      object LbManufacturer: TLabel
        Left = 8
        Top = 35
        Width = 69
        Height = 13
        Caption = 'Manufacturer:'
      end
      object LbMicNotes: TLabel
        Left = 8
        Top = 62
        Width = 32
        Height = 13
        Caption = 'Notes:'
      end
      object EdMicType: TEdit
        Left = 100
        Top = 5
        Width = 472
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = EdMicTypeChange
      end
      object EdMicNotes: TEdit
        Left = 100
        Top = 59
        Width = 472
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = EdMicNotesChange
      end
      object EdManufacturer: TEdit
        Left = 100
        Top = 32
        Width = 472
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = EdManufacturerChange
      end
    end
    object TSOutboardInfo: TTabSheet
      Caption = 'Outboard Information'
      ImageIndex = 5
      DesignSize = (
        592
        340)
      object LbAmplifier: TLabel
        Left = 8
        Top = 62
        Width = 45
        Height = 13
        Caption = 'Amplifier:'
      end
      object LbDAConverter: TLabel
        Left = 8
        Top = 35
        Width = 70
        Height = 13
        Caption = 'DA Converter:'
      end
      object LbADConverter: TLabel
        Left = 8
        Top = 8
        Width = 70
        Height = 13
        Caption = 'AD Converter:'
      end
      object LbLoudSpeaker: TLabel
        Left = 8
        Top = 89
        Width = 65
        Height = 13
        Caption = 'Loudspeaker:'
      end
      object EdADConverter: TEdit
        Left = 94
        Top = 5
        Width = 478
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = EdADConverterChange
      end
      object EdDAConverter: TEdit
        Left = 94
        Top = 32
        Width = 478
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = EdDAConverterChange
      end
      object EdAmplifier: TEdit
        Left = 94
        Top = 59
        Width = 478
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = EdAmplifierChange
      end
      object EdLoudspeaker: TEdit
        Left = 94
        Top = 86
        Width = 478
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnChange = EdLoudspeakerChange
      end
    end
    object TSMeasurementInfo: TTabSheet
      Caption = 'Measurement Information'
      ImageIndex = 6
      DesignSize = (
        592
        340)
      object LbMicrophoneType: TLabel
        Left = 8
        Top = 8
        Width = 96
        Height = 13
        Caption = 'Measurement Type:'
      end
      object LbDistance: TLabel
        Left = 8
        Top = 62
        Width = 45
        Height = 13
        Caption = 'Distance:'
      end
      object LbDistanceUnit: TLabel
        Left = 123
        Top = 62
        Width = 13
        Height = 13
        Caption = 'cm'
      end
      object LbMicrophoneDepth: TLabel
        Left = 160
        Top = 62
        Width = 91
        Height = 13
        Caption = 'Microphone Depth:'
      end
      object LbMicrophoneDepthUnit: TLabel
        Left = 323
        Top = 62
        Width = 13
        Height = 13
        Caption = 'cm'
      end
      object LbSignalType: TLabel
        Left = 8
        Top = 35
        Width = 59
        Height = 13
        Caption = 'Signal Type:'
      end
      object EdMeasurementType: TEdit
        Left = 110
        Top = 5
        Width = 462
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = EdMeasurementTypeChange
      end
      object SEDistance: TSpinEdit
        Left = 59
        Top = 59
        Width = 58
        Height = 22
        MaxValue = 99999999
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnChange = SEDistanceChange
      end
      object SEMicDepth: TSpinEdit
        Left = 259
        Top = 59
        Width = 58
        Height = 22
        MaxValue = 99999999
        MinValue = 0
        TabOrder = 2
        Value = 0
      end
      object EdSignalType: TEdit
        Left = 110
        Top = 32
        Width = 462
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnChange = EdSignalTypeChange
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 152
    object MIFile: TMenuItem
      Caption = '&File'
      object MINew: TMenuItem
        Caption = '&New'
        OnClick = MINewClick
      end
      object MIOpen: TMenuItem
        Caption = '&Open...'
        OnClick = MIOpenClick
      end
      object MISave: TMenuItem
        Caption = '&Save'
        Enabled = False
        OnClick = MISaveClick
      end
      object MISaveAs: TMenuItem
        Caption = 'Save &As...'
        OnClick = MISaveAsClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MIAutoconvertOldHRTFfiles: TMenuItem
        Caption = 'Autoconvert old HRTF files...'
        OnClick = MIAutoconvertOldHRTFfilesClick
      end
      object MIImportIRCAM: TMenuItem
        Caption = 'Import IRCAM...'
        OnClick = MIImportIRCAMClick
      end
      object MIImportDiffuse: TMenuItem
        Caption = 'Import Diffuse...'
        OnClick = MIImportDiffuseClick
      end
      object MIImportETI: TMenuItem
        Caption = 'Import ETI...'
        OnClick = MIImportETIClick
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
  object OpenDialog: TOpenDialog
    DefaultExt = 'hrtf'
    Filter = 'HRTF files (*.HRTF)|*.hrtf'
    Title = 'Select an HRTF file'
    Left = 56
    Top = 152
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'hrtf'
    Filter = 'HRTF files (*.HRTF)|*.hrtf|VST Plugin (*.DLL)|*.DLL'
    Title = 'Select an HRTF file'
    Left = 88
    Top = 152
  end
  object ADHRIR: TAudioDataCollection32
    Channels = <
      item
        DisplayName = 'Left'
      end
      item
        DisplayName = 'Right'
      end>
    SampleFrames = 512
    SampleRate = 44100.000000000000000000
    Left = 120
    Top = 152
  end
  object SaveDialogWAV: TSaveDialog
    DefaultExt = 'wav'
    Filter = 'Wav Files (*.WAV)|*.wav|AIFF Files (*.AIFF)|*.aiff'
    Title = 'Select an HRTF file'
    Left = 152
    Top = 152
  end
  object OpenDialogWAV: TOpenDialog
    DefaultExt = 'wav'
    Filter = 
      'All known files|*.aiff;*.wav|Wav Files (*.WAV)|*.wav|AIFF Files ' +
      '(*.AIFF)|*.aiff'
    Title = 'Select an HRTF file'
    Left = 184
    Top = 152
  end
end
