object FmVSTEditor: TFmVSTEditor
  Left = 281
  Top = 224
  BorderStyle = bsDialog
  Caption = 'VST Plugin Editor'
  ClientHeight = 112
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 16
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 321
    Height = 27
    ButtonHeight = 24
    Color = clBtnFace
    EdgeInner = esNone
    EdgeOuter = esNone
    ParentColor = False
    TabOrder = 0
    object LblPreset: TLabel
      Left = 0
      Top = 0
      Width = 53
      Height = 24
      AutoSize = False
      Caption = 'Preset: '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object BtnSeparator1: TToolButton
      Left = 53
      Top = 0
      Width = 8
      Caption = 'BtnSeparator1'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object CbxPreset: TComboBox
      Left = 61
      Top = 0
      Width = 144
      Height = 24
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      Style = csDropDownList
      Color = clBtnFace
      PopupMenu = PUPreset
      TabOrder = 0
      OnChange = CbxPresetChange
    end
    object BtnSeparator2: TToolButton
      Left = 205
      Top = 0
      Width = 8
      Caption = 'BtnSeparator2'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object BtnSetup: TButton
      Left = 213
      Top = 0
      Width = 51
      Height = 24
      Caption = '&Setup'
      TabOrder = 2
      TabStop = False
      OnClick = BtnSetupClick
    end
    object BtnSeparator3: TToolButton
      Left = 264
      Top = 0
      Width = 8
      Caption = 'BtnSeparator3'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object BtnExit: TButton
      Left = 272
      Top = 0
      Width = 37
      Height = 24
      Caption = 'E&xit'
      TabOrder = 1
      TabStop = False
      OnClick = BtnExitClick
    end
  end
  object VSTPanel: TPanel
    Left = 0
    Top = 27
    Width = 321
    Height = 85
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Programme\Audio\Plugins\VST'
    Tempo = 120.000000000000000000
    VendorVersion = 0
    VstPlugIns = <
      item
        DisplayName = 'Plugin'
        VstOfflineTasks = <>
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2400
    Left = 8
    Top = 35
  end
  object AsioHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreventClipping = pcAnalog
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = AsioHostBufferSwitch32
    OnLatencyChanged = AsioHostReset
    OnReset = AsioHostReset
    Left = 52
    Top = 35
  end
  object PUPreset: TPopupMenu
    Left = 108
    Top = 35
    object MnuLoadPreset: TMenuItem
      Caption = '&Load Preset...'
      OnClick = MnuLoadPresetClick
    end
    object MnuSavePreset: TMenuItem
      Caption = '&Save Preset...'
      OnClick = MnuSavePresetClick
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'fxp'
    Filter = 'VST Preset (*.fxp)|*.fxp|VST Bank (*.fxb)|*.fxb'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 160
    Top = 35
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'fxp'
    Filter = 'VST Preset (*.fxp)|*.fxp|VST Bank (*.fxb)|*.fxb'
    Left = 220
    Top = 35
  end
end
