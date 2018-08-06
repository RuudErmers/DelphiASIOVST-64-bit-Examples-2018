object FmVST2SEM: TFmVST2SEM
  Left = 218
  Top = 81
  BorderStyle = bsSingle
  Caption = 'VST to SEM converter'
  ClientHeight = 234
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PC: TPageControl
    Left = 0
    Top = 0
    Width = 321
    Height = 234
    ActivePage = TSVSTPlugin
    Align = alClient
    TabOrder = 0
    object TSVSTPlugin: TTabSheet
      Caption = 'VST Plugin Properties'
      ImageIndex = 1
      DesignSize = (
        313
        206)
      object LbVSTName: TLabel
        Left = 3
        Top = 8
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object VstName: TEdit
        Left = 40
        Top = 5
        Width = 270
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 0
      end
      object MemoInfo: TMemo
        Left = 3
        Top = 32
        Width = 307
        Height = 171
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        TabOrder = 1
      end
    end
    object TSSEMProperties: TTabSheet
      Caption = 'SEM Properties'
      DesignSize = (
        313
        206)
      object LbName: TLabel
        Left = 3
        Top = 8
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object LbSemId: TLabel
        Left = 3
        Top = 62
        Width = 15
        Height = 13
        Caption = 'ID:'
      end
      object LbSemAbout: TLabel
        Left = 3
        Top = 116
        Width = 33
        Height = 13
        Caption = 'About:'
      end
      object MemoName: TMemo
        Left = 40
        Top = 5
        Width = 270
        Height = 48
        Anchors = [akLeft, akTop, akRight]
        Lines.Strings = (
          'DAV VST-Wrapper')
        ReadOnly = True
        TabOrder = 0
      end
      object MemoID: TMemo
        Left = 40
        Top = 59
        Width = 270
        Height = 48
        Anchors = [akLeft, akTop, akRight]
        Lines.Strings = (
          'VST2SEM')
        ReadOnly = True
        TabOrder = 1
      end
      object MemoAbout: TMemo
        Left = 40
        Top = 113
        Width = 270
        Height = 72
        Anchors = [akLeft, akTop, akRight]
        Lines.Strings = (
          'Wrapper created by Christian-W. Budde')
        ReadOnly = True
        TabOrder = 2
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 216
    object MIFile: TMenuItem
      Caption = '&File'
      object MIOpen: TMenuItem
        Caption = '&Open...'
        OnClick = MIOpenClick
      end
      object MISaveAs: TMenuItem
        Caption = 'Save as...'
        Enabled = False
        OnClick = MISaveAsClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MIBatchConvert: TMenuItem
        Caption = 'Batch Convert...'
        OnClick = MIBatchConvertClick
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
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Program Files\Audio\Plugins\VST'
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
    VstVersion = 2300
    Left = 248
  end
  object XPManifest: TXPManifest
    Left = 280
  end
end
