object FmResultTableSetup: TFmResultTableSetup
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Result Table Options'
  ClientHeight = 129
  ClientWidth = 345
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    345
    129)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel
    Left = 8
    Top = 8
    Width = 244
    Height = 111
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object LbColumnColor: TLabel
    Left = 24
    Top = 20
    Width = 67
    Height = 13
    Caption = 'Column Color:'
  end
  object LbRatingThreshold: TLabel
    Left = 24
    Top = 92
    Width = 83
    Height = 13
    Caption = 'Rating threshold:'
  end
  object BtOK: TButton
    Left = 263
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BtCancel: TButton
    Left = 263
    Top = 38
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 1
  end
  object RBColorEverySecond: TRadioButton
    Left = 98
    Top = 19
    Width = 127
    Height = 17
    Caption = 'color every second'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object RBColorAboveThreshold: TRadioButton
    Left = 98
    Top = 42
    Width = 127
    Height = 17
    Caption = 'color above threshold'
    TabOrder = 3
  end
  object RBColorBelowThreshold: TRadioButton
    Left = 98
    Top = 65
    Width = 127
    Height = 17
    Caption = 'color below threshold'
    TabOrder = 4
  end
  object MERatingThreshold: TMaskEdit
    Left = 113
    Top = 88
    Width = 118
    Height = 21
    EditMask = '!9,00;1;_'
    MaxLength = 4
    TabOrder = 5
    Text = '0,25'
    OnChange = MERatingThresholdChange
  end
end
