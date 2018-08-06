object KbDemoForm: TKbDemoForm
  Left = 227
  Top = 126
  Caption = 'TGuiMidiKeys Demonstration'
  ClientHeight = 301
  ClientWidth = 786
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object MainKb: TGuiMidiKeys
    Left = 0
    Top = 199
    Width = 786
    Height = 102
    Align = alBottom
    KeyDownMode = kdmFlat
    Height3d = 0.200000002980232200
    BlackKeyHeight = 0.629999995231628400
    KeyZones = <
      item
        DisplayName = 'TestZone'
        Tag = 0
      end>
    OnZoneSelectionChanged = MainKbZoneSelectionChanged
    OnStartZoneBarDragging = MainKbStartZoneBarDragging
    OnMoveZoneBarDragging = MainKbMoveZoneBarDragging
  end
  object PnRemoteControl: TPanel
    Left = 8
    Top = 8
    Width = 353
    Height = 105
    TabOrder = 0
    DesignSize = (
      353
      105)
    object RemoteKeyboard: TGuiMidiKeys
      Left = 8
      Top = 24
      Width = 336
      Height = 41
      Anchors = [akLeft, akTop, akRight, akBottom]
      KeyDownMode = kdmFlat
      Height3d = 0.200000002980232200
      BlackKeyHeight = 0.629999995231628400
      KeyZones = <>
      ShowKeyZones = False
      OnNoteOn = RemoteKeyboardNoteOn
      OnNoteOff = RemoteKeyboardNoteOff
    end
    object LbRemoteCOntrol: TLabel
      Left = 8
      Top = 8
      Width = 337
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Remote control:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LbRemoteControlInfo: TLabel
      Left = 8
      Top = 72
      Width = 337
      Height = 26
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Caption = 
        'Each note event  of this keyboard is sent to the big keyboard at' +
        ' the bottom of this form'
      WordWrap = True
    end
  end
  object PnColorizeKeys: TPanel
    Left = 368
    Top = 8
    Width = 129
    Height = 177
    TabOrder = 1
    object Bevel: TBevel
      Left = 0
      Top = 128
      Width = 129
      Height = 10
      Shape = bsBottomLine
    end
    object ColorizeAllBtn: TButton
      Left = 8
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Colorize all keys'
      TabOrder = 0
      OnClick = ColorizeAllBtnClick
    end
    object ColorizeWhiteBtn: TButton
      Left = 8
      Top = 72
      Width = 113
      Height = 25
      Caption = 'Colorize white keys'
      TabOrder = 1
      OnClick = ColorizeWhiteBtnClick
    end
    object ResetColorBtn: TButton
      Left = 8
      Top = 144
      Width = 113
      Height = 25
      Caption = 'Reset colors'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = ResetColorBtnClick
    end
    object ColorizeCBtn: TButton
      Left = 8
      Top = 40
      Width = 113
      Height = 25
      Caption = 'Colorize C-keys'
      TabOrder = 3
      OnClick = ColorizeCBtnClick
    end
    object ColorizeBlackBtn: TButton
      Left = 8
      Top = 104
      Width = 113
      Height = 25
      Caption = 'Colorize black keys'
      TabOrder = 4
      OnClick = ColorizeBlackBtnClick
    end
  end
  object PnZoneOptions: TPanel
    Left = 8
    Top = 120
    Width = 353
    Height = 65
    TabOrder = 2
    object LbZoneOptions: TLabel
      Left = 8
      Top = 8
      Width = 81
      Height = 13
      Caption = 'Zone Options:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object DeleteZoneBtn: TButton
      Left = 8
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Delete zone'
      Enabled = False
      TabOrder = 0
      OnClick = DeleteZoneBtnClick
    end
    object ZoneNameEdit: TEdit
      Left = 240
      Top = 7
      Width = 105
      Height = 21
      Enabled = False
      TabOrder = 1
      Text = 'ZoneNameEdit'
    end
    object ZoneNameBtn: TButton
      Left = 240
      Top = 32
      Width = 107
      Height = 25
      Caption = 'Change zone name'
      Enabled = False
      TabOrder = 2
      OnClick = ZoneNameBtnClick
    end
    object ClipZoneBtn: TBitBtn
      Left = 88
      Top = 32
      Width = 145
      Height = 25
      Caption = 'Clip overlaying zones'
      TabOrder = 3
      OnClick = ClipZoneBtnClick
    end
  end
  object LogMemo: TMemo
    Left = 504
    Top = 8
    Width = 273
    Height = 177
    ScrollBars = ssVertical
    TabOrder = 3
  end
end
