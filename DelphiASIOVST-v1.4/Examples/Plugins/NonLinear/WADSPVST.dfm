object FmWinAmpVST: TFmWinAmpVST
  Left = 342
  Top = 353
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'VST Host DSP Plugin v1.0 for Winamp'
  ClientHeight = 208
  ClientWidth = 426
  Color = clGray
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000003000008F000000001BB008FFF000000007BB38FFFF000000007
    3BB3FFFF0000000783BB38FFF000008F88B3BB3FFF0008F733FB3BB38FF00FF7
    BB38BB3B3F8000F83BFFBBB31800000FF73BBBB300000000FFF3B8BB00000000
    0FFF838BB000000000FFF8033B000000000F800003300000000700000010BE7F
    00008C3F0000801F0000800F0000C0070000C003000080010000000000000000
    000080010000C0030000E0070000F0030000F8010000FC210000FE790000}
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PnGUI: TPanel
    Left = 0
    Top = 0
    Width = 426
    Height = 208
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3945766
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    TabOrder = 0
  end
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdEditFile, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Programme\Audio\Plugins\VST'
    Tempo = 120.000000000000000000
    VendorVersion = 0
    VstPlugIns = <
      item
        DisplayName = 'TVstPlugIn'
        VstOfflineTasks = <>
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2400
    Left = 32
    Top = 48
  end
end
