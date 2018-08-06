object FmVSTAnalyser: TFmVSTAnalyser
  Left = 281
  Top = 224
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Simple VST Plugin Analyser'
  ClientHeight = 35
  ClientWidth = 245
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  Menu = MainMenu
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 16
  object VSTPanel: TPanel
    Left = 0
    Top = 0
    Width = 245
    Height = 35
    Align = alClient
    BevelOuter = bvNone
    Caption = '(empty)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 6695441
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = VSTPanelClick
  end
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Programme\Audio\Plugins\VST'
    ProductString = 'Simple VST Plugin Analyser'
    Tempo = 120.000000000000000000
    VendorString = 'Delphi ASIO&VST Project'
    VendorVersion = 0
    VstPlugIns = <
      item
        DisplayName = 'Plugin'
        VstOfflineTasks = <>
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2300
    Left = 4
    Top = 3
  end
  object XPManifest: TXPManifest
    Left = 32
    Top = 3
  end
  object MainMenu: TMainMenu
    Left = 60
    Top = 3
    object MIFile: TMenuItem
      Caption = '&VST Plugin'
      object MIOpen: TMenuItem
        Caption = '&Open'
        OnClick = MIOpenClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MIQuit: TMenuItem
        Caption = '&Quit'
        OnClick = MIQuitClick
      end
    end
    object MIPrograms: TMenuItem
      Caption = 'Programs'
      object MILoad: TMenuItem
        Caption = '&Load...'
        OnClick = MILoadClick
      end
      object MISave: TMenuItem
        Caption = '&Save...'
        OnClick = MISaveClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
    end
    object MIIR: TMenuItem
      Caption = 'Impulse Response'
      object MIPlotIR: TMenuItem
        Caption = 'Plot Impulse Response...'
        OnClick = MIPlotIRClick
      end
      object MIRenderIR: TMenuItem
        Caption = 'Render Impulse Response...'
        Enabled = False
      end
    end
  end
  object OD: TOpenDialog
    DefaultExt = 'DLL'
    Filter = 'VST Plugin (*.dll)|*.dll'
    Title = 'Load VST Plugin'
    Left = 88
    Top = 3
  end
end
