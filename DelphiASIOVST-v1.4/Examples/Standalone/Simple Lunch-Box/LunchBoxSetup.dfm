object FmSetup: TFmSetup
  Left = 333
  Top = 221
  BorderStyle = bsDialog
  Caption = 'Setup'
  ClientHeight = 109
  ClientWidth = 228
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    228
    109)
  PixelsPerInch = 96
  TextHeight = 13
  object LbPreset: TLabel
    Left = 4
    Top = 2
    Width = 81
    Height = 21
    AutoSize = False
    Caption = 'ASIO Driver:'
    Layout = tlCenter
  end
  object LbOutput: TLabel
    Left = 4
    Top = 29
    Width = 50
    Height = 21
    AutoSize = False
    Caption = 'Output:'
    Layout = tlCenter
  end
  object LbPlaybackSampleRate: TLabel
    Left = 4
    Top = 88
    Width = 138
    Height = 13
    Caption = 'Playback Samplerate'
    Layout = tlCenter
  end
  object CBDrivers: TComboBox
    Left = 88
    Top = 2
    Width = 136
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = CBDriversChange
  end
  object CBOutput: TComboBox
    Left = 64
    Top = 29
    Width = 160
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = CBOutputChange
  end
  object BtControlPanel: TButton
    Left = 4
    Top = 55
    Width = 220
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Control Panel'
    TabOrder = 2
    OnClick = BtControlPanelClick
  end
  object SESampleRate: TSpinEdit
    Left = 148
    Top = 85
    Width = 76
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 44100
    OnChange = SESampleRateChange
  end
end
