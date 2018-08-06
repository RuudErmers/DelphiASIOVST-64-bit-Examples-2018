object FmAsioDriverControlPanel: TFmAsioDriverControlPanel
  Left = 533
  Top = 126
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DAV MultiChannel Driver'
  ClientHeight = 309
  ClientWidth = 408
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    408
    309)
  PixelsPerInch = 96
  TextHeight = 13
  object LbDriver: TLabel
    Left = 8
    Top = 12
    Width = 33
    Height = 13
    Caption = 'Driver:'
  end
  object CbDriver: TComboBox
    Left = 47
    Top = 8
    Width = 258
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = CbDriverChange
  end
  object BtControlPanel: TButton
    Left = 311
    Top = 6
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Control &Panel'
    TabOrder = 1
    OnClick = BtControlPanelClick
  end
  object pcAssignments: TPageControl
    Left = 8
    Top = 39
    Width = 392
    Height = 236
    ActivePage = pageInputs
    Anchors = [akLeft, akTop, akRight]
    MultiLine = True
    TabOrder = 2
    object pageInputs: TTabSheet
      Caption = '&Inputs'
      object lbIn01: TLabel
        Left = 12
        Top = 17
        Width = 29
        Height = 13
        Caption = 'In 01:'
      end
      object lbIn02: TLabel
        Left = 12
        Top = 41
        Width = 29
        Height = 13
        Caption = 'In 02:'
      end
      object lbIn03: TLabel
        Left = 12
        Top = 65
        Width = 29
        Height = 13
        Caption = 'In 03:'
      end
      object lbIn04: TLabel
        Left = 12
        Top = 89
        Width = 29
        Height = 13
        Caption = 'In 04:'
      end
      object lbIn05: TLabel
        Left = 12
        Top = 113
        Width = 29
        Height = 13
        Caption = 'In 05:'
      end
      object lbIn06: TLabel
        Left = 12
        Top = 137
        Width = 29
        Height = 13
        Caption = 'In 06:'
      end
      object lbIn07: TLabel
        Left = 12
        Top = 161
        Width = 29
        Height = 13
        Caption = 'In 07:'
      end
      object lbIn08: TLabel
        Left = 12
        Top = 185
        Width = 29
        Height = 13
        Caption = 'In 08:'
      end
      object lbIn09: TLabel
        Left = 212
        Top = 17
        Width = 29
        Height = 13
        Caption = 'In 09:'
      end
      object lbIn10: TLabel
        Left = 212
        Top = 41
        Width = 29
        Height = 13
        Caption = 'In 10:'
      end
      object lbIn11: TLabel
        Left = 212
        Top = 65
        Width = 29
        Height = 13
        Caption = 'In 11:'
      end
      object lbIn12: TLabel
        Left = 212
        Top = 89
        Width = 29
        Height = 13
        Caption = 'In 12:'
      end
      object lbIn13: TLabel
        Left = 212
        Top = 113
        Width = 29
        Height = 13
        Caption = 'In 13:'
      end
      object lbIn14: TLabel
        Left = 212
        Top = 137
        Width = 29
        Height = 13
        Caption = 'In 14:'
      end
      object lbIn15: TLabel
        Left = 212
        Top = 161
        Width = 29
        Height = 13
        Caption = 'In 15:'
      end
      object lbIn16: TLabel
        Left = 212
        Top = 185
        Width = 29
        Height = 13
        Caption = 'In 16:'
      end
      object cbIn01: TComboBox
        Tag = 1
        Left = 46
        Top = 14
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = InputSettingsChanged
      end
      object cbIn02: TComboBox
        Tag = 2
        Left = 46
        Top = 38
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = InputSettingsChanged
      end
      object cbIn03: TComboBox
        Tag = 3
        Left = 46
        Top = 62
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        OnChange = InputSettingsChanged
      end
      object cbIn04: TComboBox
        Tag = 4
        Left = 46
        Top = 86
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
        OnChange = InputSettingsChanged
      end
      object cbIn05: TComboBox
        Tag = 5
        Left = 46
        Top = 110
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        OnChange = InputSettingsChanged
      end
      object cbIn06: TComboBox
        Tag = 6
        Left = 46
        Top = 134
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 5
        OnChange = InputSettingsChanged
      end
      object cbIn07: TComboBox
        Tag = 7
        Left = 46
        Top = 158
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 6
        OnChange = InputSettingsChanged
      end
      object cbIn08: TComboBox
        Tag = 8
        Left = 46
        Top = 182
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 7
        OnChange = InputSettingsChanged
      end
      object cbIn09: TComboBox
        Tag = 9
        Left = 246
        Top = 14
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 8
        OnChange = InputSettingsChanged
      end
      object cbIn10: TComboBox
        Tag = 10
        Left = 246
        Top = 38
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 9
        OnChange = InputSettingsChanged
      end
      object cbIn11: TComboBox
        Tag = 11
        Left = 246
        Top = 62
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 10
        OnChange = InputSettingsChanged
      end
      object cbIn12: TComboBox
        Tag = 12
        Left = 246
        Top = 86
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 11
        OnChange = InputSettingsChanged
      end
      object cbIn13: TComboBox
        Tag = 13
        Left = 246
        Top = 110
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 12
        OnChange = InputSettingsChanged
      end
      object cbIn14: TComboBox
        Tag = 14
        Left = 246
        Top = 134
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 13
        OnChange = InputSettingsChanged
      end
      object cbIn15: TComboBox
        Tag = 15
        Left = 246
        Top = 158
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 14
        OnChange = InputSettingsChanged
      end
      object cbIn16: TComboBox
        Tag = 16
        Left = 246
        Top = 182
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 15
        OnChange = InputSettingsChanged
      end
    end
    object pageOutputs: TTabSheet
      Caption = '&Outputs'
      ImageIndex = 1
      object lbOut01: TLabel
        Left = 4
        Top = 17
        Width = 37
        Height = 13
        Caption = 'Out 01:'
      end
      object lbOut02: TLabel
        Left = 4
        Top = 41
        Width = 37
        Height = 13
        Caption = 'Out 02:'
      end
      object lbOut03: TLabel
        Left = 4
        Top = 65
        Width = 37
        Height = 13
        Caption = 'Out 03:'
      end
      object lbOut04: TLabel
        Left = 4
        Top = 89
        Width = 37
        Height = 13
        Caption = 'Out 04:'
      end
      object lbOut05: TLabel
        Left = 4
        Top = 113
        Width = 37
        Height = 13
        Caption = 'Out 05:'
      end
      object lbOut06: TLabel
        Left = 4
        Top = 137
        Width = 37
        Height = 13
        Caption = 'Out 06:'
      end
      object lbOut07: TLabel
        Left = 4
        Top = 161
        Width = 37
        Height = 13
        Caption = 'Out 07:'
      end
      object lbOut08: TLabel
        Left = 4
        Top = 185
        Width = 37
        Height = 13
        Caption = 'Out 08:'
      end
      object lbOut09: TLabel
        Left = 204
        Top = 17
        Width = 37
        Height = 13
        Caption = 'Out 09:'
      end
      object lbOut10: TLabel
        Left = 204
        Top = 41
        Width = 37
        Height = 13
        Caption = 'Out 10:'
      end
      object lbOut11: TLabel
        Left = 204
        Top = 65
        Width = 37
        Height = 13
        Caption = 'Out 11:'
      end
      object lbOut12: TLabel
        Left = 204
        Top = 89
        Width = 37
        Height = 13
        Caption = 'Out 12:'
      end
      object lbOut13: TLabel
        Left = 204
        Top = 113
        Width = 37
        Height = 13
        Caption = 'Out 13:'
      end
      object lbOut14: TLabel
        Left = 204
        Top = 137
        Width = 37
        Height = 13
        Caption = 'Out 14:'
      end
      object lbOut15: TLabel
        Left = 204
        Top = 161
        Width = 37
        Height = 13
        Caption = 'Out 15:'
      end
      object lbOut16: TLabel
        Left = 204
        Top = 185
        Width = 37
        Height = 13
        Caption = 'Out 16:'
      end
      object cbOut01: TComboBox
        Tag = 1
        Left = 46
        Top = 14
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
        OnChange = OutputSettingsChanged
      end
      object cbOut02: TComboBox
        Tag = 2
        Left = 46
        Top = 38
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 1
        OnChange = OutputSettingsChanged
      end
      object cbOut03: TComboBox
        Tag = 3
        Left = 46
        Top = 62
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 2
        OnChange = OutputSettingsChanged
      end
      object cbOut04: TComboBox
        Tag = 4
        Left = 46
        Top = 86
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 3
        OnChange = OutputSettingsChanged
      end
      object cbOut05: TComboBox
        Tag = 5
        Left = 46
        Top = 110
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 4
        OnChange = OutputSettingsChanged
      end
      object cbOut06: TComboBox
        Tag = 6
        Left = 46
        Top = 134
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 5
        OnChange = OutputSettingsChanged
      end
      object cbOut07: TComboBox
        Tag = 7
        Left = 46
        Top = 158
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 6
        OnChange = OutputSettingsChanged
      end
      object cbOut08: TComboBox
        Tag = 8
        Left = 46
        Top = 182
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 7
        OnChange = OutputSettingsChanged
      end
      object cbOut09: TComboBox
        Tag = 9
        Left = 246
        Top = 14
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 8
        OnChange = OutputSettingsChanged
      end
      object cbOut10: TComboBox
        Tag = 10
        Left = 246
        Top = 38
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 9
        OnChange = OutputSettingsChanged
      end
      object cbOut11: TComboBox
        Tag = 11
        Left = 246
        Top = 62
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 10
        OnChange = OutputSettingsChanged
      end
      object cbOut12: TComboBox
        Tag = 12
        Left = 246
        Top = 86
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 11
        OnChange = OutputSettingsChanged
      end
      object cbOut13: TComboBox
        Tag = 13
        Left = 246
        Top = 110
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 12
        OnChange = OutputSettingsChanged
      end
      object cbOut14: TComboBox
        Tag = 14
        Left = 246
        Top = 134
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 13
        OnChange = OutputSettingsChanged
      end
      object cbOut15: TComboBox
        Tag = 15
        Left = 246
        Top = 158
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 14
        OnChange = OutputSettingsChanged
      end
      object cbOut16: TComboBox
        Tag = 16
        Left = 246
        Top = 182
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 15
        OnChange = OutputSettingsChanged
      end
    end
  end
  object btnClose: TButton
    Left = 325
    Top = 280
    Width = 75
    Height = 25
    Caption = '&Close'
    TabOrder = 3
    OnClick = btnCloseClick
  end
  object btnApply: TButton
    Left = 244
    Top = 280
    Width = 75
    Height = 25
    Caption = '&Apply'
    TabOrder = 4
    OnClick = btnApplyClick
  end
end
