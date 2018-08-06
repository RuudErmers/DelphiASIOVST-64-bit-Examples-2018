object FmVOL: TFmVOL
  Left = 317
  Top = 85
  BorderStyle = bsNone
  Caption = 'VST Opcode Logger'
  ClientHeight = 277
  ClientWidth = 374
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  DesignSize = (
    374
    277)
  PixelsPerInch = 96
  TextHeight = 13
  object LbParameter: TLabel
    Left = 8
    Top = 30
    Width = 63
    Height = 13
    Caption = 'Parameter 1:'
    Color = clBtnFace
    ParentColor = False
  end
  object LbParameterValue: TLabel
    Left = 287
    Top = 30
    Width = 79
    Height = 13
    Caption = 'Parameter Value'
    Color = clBtnFace
    ParentColor = False
  end
  object BtClear: TButton
    Left = 299
    Top = 0
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Clear'
    TabOrder = 1
    OnClick = BtClearClick
  end
  object MOpcodeLog: TMemo
    Left = 0
    Top = 48
    Width = 374
    Height = 229
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'MOpcodeLog')
    ReadOnly = True
    TabOrder = 0
  end
  object BtUpdate: TButton
    Left = 224
    Top = 0
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Update'
    TabOrder = 2
    OnClick = BtUpdateClick
  end
  object CBAutoUpdates: TCheckBox
    Left = 4
    Top = 3
    Width = 111
    Height = 17
    Caption = 'Automatic Updates'
    TabOrder = 3
  end
  object BtSaveAs: TButton
    Left = 149
    Top = 0
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save &As...'
    TabOrder = 4
    OnClick = BtSaveAsClick
  end
  object Sb1: TScrollBar
    Left = 77
    Top = 29
    Width = 204
    Height = 15
    Max = 2000
    Min = 1000
    PageSize = 0
    Position = 1000
    TabOrder = 5
    OnChange = Sb1Change
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.log'
    Filter = 'Logfile (*.log)|*.log'
    Title = 'Save As...'
    Left = 128
    Top = 40
  end
end
