object FmSonogram: TFmSonogram
  Left = 347
  Top = 139
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Sonogram'
  ClientHeight = 299
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    361
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object LbVstPlugin: TLabel
    Left = 8
    Top = 11
    Width = 53
    Height = 13
    Caption = 'VST Plugin:'
  end
  object PbSonogram: TPaintBox
    Left = 8
    Top = 35
    Width = 345
    Height = 256
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnPaint = PbSonogramPaint
  end
  object EdVSTPlugin: TEdit
    Left = 72
    Top = 8
    Width = 281
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = EdVSTPluginChange
  end
  object BtSelect: TButton
    Left = 334
    Top = 10
    Width = 17
    Height = 17
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = BtSelectClick
  end
  object OD: TOpenDialog
    DefaultExt = '.DLL'
    Filter = 'VST Plugin (*.DLL)|*.DLL'
    Title = 'Select a VST Plugin'
    Left = 176
    Top = 16
  end
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Program Files\Audio\Plugins\VST\=Delphi='
    Tempo = 120.000000000000000000
    VendorVersion = 0
    VstPlugIns = <
      item
        DisplayName = 'PUT'
        VstOfflineTasks = <>
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2300
    Left = 208
    Top = 16
  end
end
