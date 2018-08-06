object FmSEModuleExplorer: TFmSEModuleExplorer
  Left = 188
  Top = 77
  Caption = 'SE Module Explorer'
  ClientHeight = 439
  ClientWidth = 469
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
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 469
    Height = 439
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
    object MIFile: TMenuItem
      Caption = '&File'
      object MIOpen: TMenuItem
        Caption = '&Open...'
        OnClick = MIOpenClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MIExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MIExitClick
      end
    end
    object MISettings: TMenuItem
      Caption = '&Settings'
      object MIEnableWrapper: TMenuItem
        Caption = 'Enable fixing SEM'
        OnClick = MIEnableWrapperClick
      end
    end
    object MIHelp: TMenuItem
      Caption = '&Help'
      object MIAbout: TMenuItem
        Caption = '&About'
        OnClick = MIAboutClick
      end
    end
  end
end
