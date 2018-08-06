object PluginForm: TPluginForm
  Left = 680
  Top = 391
  AlphaBlendValue = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Tobybear MiniHost (www.tobybear.de)'
  ClientHeight = 378
  ClientWidth = 560
  Color = 11053224
  TransparentColorValue = clMaroon
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnDeactivate = FormDeactivate
  PixelsPerInch = 120
  TextHeight = 14
  object PnStatus: TPanel
    Left = 0
    Top = 0
    Width = 560
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    Ctl3D = False
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 0
    object PresetBox: TComboBox
      Left = 101
      Top = 5
      Width = 193
      Height = 22
      AutoComplete = False
      AutoDropDown = True
      BevelInner = bvNone
      BevelKind = bkFlat
      BevelOuter = bvRaised
      Style = csOwnerDrawFixed
      Color = 5789784
      Ctl3D = False
      DropDownCount = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ItemIndex = 0
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
      Text = 'presets'
      OnChange = PresetBoxChange
      OnClick = PresetBoxClick
      OnDrawItem = PresetBoxDrawItem
      OnKeyPress = PresetBoxKeyPress
      Items.Strings = (
        'presets')
    end
  end
  object PanelVST: TPanel
    Left = 296
    Top = 232
    Width = 185
    Height = 41
    Caption = 'PanelVST'
    TabOrder = 1
  end
  object MainMenu: TMainMenu
    AutoHotkeys = maManual
    Left = 152
    Top = 72
    object MIPreset: TMenuItem
      Caption = 'Presets'
      object MILoadPreset: TMenuItem
        Caption = 'Load Preset (*.fxp) ...'
        Enabled = False
        OnClick = MILoadPresetClick
      end
      object MILoadBank: TMenuItem
        Caption = 'Load Bank (*.fxb) ...'
        Enabled = False
        OnClick = MILoadBankClick
      end
      object MISavePreset: TMenuItem
        Caption = 'Save Preset (*.fxp) ...'
        Enabled = False
        OnClick = MISavePresetClick
      end
      object MISaveBank: TMenuItem
        Caption = 'Save Bank (*.fxb) ...'
        Enabled = False
        OnClick = MISaveBankClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MIRenamePreset: TMenuItem
        Caption = 'Rename Preset'
        Enabled = False
        OnClick = MIRenamePresetClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MIShowPreset: TMenuItem
        Caption = 'Show Preset in Title Bar'
        Checked = True
        OnClick = MIShowPresetClick
      end
      object MIUseMouseWheel: TMenuItem
        Caption = 'Use Mouse Wheel'
        Checked = True
      end
    end
  end
end
