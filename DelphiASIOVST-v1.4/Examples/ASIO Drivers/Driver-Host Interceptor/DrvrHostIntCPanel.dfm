object InterceptorTestCP: TInterceptorTestCP
  Left = 0
  Top = 0
  Caption = 'DriverTestCP'
  ClientHeight = 69
  ClientWidth = 194
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnDone: TButton
    Left = 111
    Top = 35
    Width = 75
    Height = 25
    Caption = '&Done'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object cbDrivers: TComboBox
    Left = 8
    Top = 8
    Width = 178
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = cbDriversChange
  end
  object btnControlPanel: TButton
    Left = 8
    Top = 35
    Width = 97
    Height = 25
    Caption = '&Control panel'
    Default = True
    TabOrder = 2
    OnClick = btnControlPanelClick
  end
end
