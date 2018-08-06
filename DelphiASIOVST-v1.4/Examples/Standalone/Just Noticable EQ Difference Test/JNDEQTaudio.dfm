object FmSetup: TFmSetup
  Left = 454
  Top = 379
  BorderStyle = bsDialog
  Caption = 'Setup'
  ClientHeight = 66
  ClientWidth = 398
  Color = 8620693
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  DesignSize = (
    398
    66)
  PixelsPerInch = 96
  TextHeight = 14
  object LbPreset: TGuiLabel
    Left = 8
    Top = 13
    Width = 67
    Height = 13
    Margins.Bottom = 0
    Caption = 'ASIO Driver:'
    FontOversampling = fo4x
    Shadow.Blur = 4.000000000000000000
    Transparent = True
  end
  object LbOutputChannels: TGuiLabel
    Left = 8
    Top = 41
    Width = 92
    Height = 13
    Margins.Bottom = 0
    Caption = 'Output Channels:'
    FontOversampling = fo4x
    Shadow.Blur = 4.000000000000000000
    Transparent = True
  end
  object SbDrivers: TGuiSelectBox
    Left = 81
    Top = 8
    Width = 217
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    ArrowColor = clBlack
    BorderColor = clBlack
    BorderRadius = 5.000000000000000000
    BorderWidth = 2.000000000000000000
    ButtonColor = 8620693
    FontOversampling = fo4x
    ItemIndex = -1
    SelectBoxColor = 10333885
    OnChange = SbDriversChange
  end
  object SbChannels: TGuiSelectBox
    Left = 106
    Top = 36
    Width = 284
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    ArrowColor = clBlack
    BorderColor = clBlack
    BorderRadius = 5.000000000000000000
    BorderWidth = 2.000000000000000000
    ButtonColor = 8620693
    FontOversampling = fo4x
    ItemIndex = -1
    SelectBoxColor = 10333885
    OnChange = SbChannelsChange
  end
  object BtControlPanel: TGuiButton
    Left = 304
    Top = 8
    Width = 86
    Height = 22
    Alignment = taCenter
    BorderColor = clBlack
    BorderWidth = 2.000000000000000000
    BorderRadius = 5.000000000000000000
    ButtonColor = 10333885
    Caption = 'Control Panel'
    FontOversampling = fo4x
    Shadow.Blur = 4.000000000000000000
    Shadow.Color = 5663873
    Shadow.Visible = True
    Transparent = False
    OnClick = BtControlPanelClick
  end
end
