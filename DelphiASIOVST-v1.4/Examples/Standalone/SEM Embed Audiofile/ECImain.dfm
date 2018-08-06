object FmSemEmbedAudioFile: TFmSemEmbedAudioFile
  Left = 286
  Top = 77
  Caption = 'SEM Embed Audio File'
  ClientHeight = 169
  ClientWidth = 307
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
  object ListBox: TListBox
    Left = 0
    Top = 0
    Width = 307
    Height = 169
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
    object MIFile: TMenuItem
      Caption = '&File'
      object MIOpenSEM: TMenuItem
        Caption = 'Open SEM...'
        OnClick = MIOpenSEMClick
      end
      object MISaveAs: TMenuItem
        Caption = 'Save As...'
        OnClick = MISaveAsClick
      end
      object MISave: TMenuItem
        Caption = 'Save'
        Enabled = False
        OnClick = MISaveClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MIAddWAV: TMenuItem
        Caption = 'Add Audio File...'
        OnClick = MIAddWAVClick
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
  object OpenDialogSEM: TOpenDialog
    DefaultExt = '.SEM'
    Filter = 'SE Module (*.sem)|*.sem'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Please select an existing Audio File Oscillator Module'
    Left = 40
    Top = 8
  end
  object OpenDialogWAV: TOpenDialog
    DefaultExt = '.WAV'
    Filter = 'Wave Files (*.wav)|*.wav'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 72
    Top = 8
  end
  object SaveDialogSEM: TSaveDialog
    DefaultExt = '.SEM'
    Filter = 'SE Module (*.sem)|*.sem'
    Left = 104
    Top = 8
  end
end
