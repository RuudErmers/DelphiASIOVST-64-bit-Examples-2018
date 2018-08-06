object FmFilterExplorer: TFmFilterExplorer
  Left = 228
  Top = 81
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 
    'FilterExplorer 2.0 (C)opyright 2005 by Christian Budde & Tobybea' +
    'r'
  ClientHeight = 660
  ClientWidth = 881
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 881
    Height = 29
    TabOrder = 0
    object BtNew: TButton
      Left = 0
      Top = 0
      Width = 81
      Height = 22
      Caption = '&New'
      TabOrder = 0
    end
    object BtLoad: TButton
      Left = 81
      Top = 0
      Width = 80
      Height = 22
      Caption = '&Load'
      TabOrder = 1
      OnClick = BtLoadClick
    end
    object BtSave: TButton
      Left = 161
      Top = 0
      Width = 80
      Height = 22
      Caption = '&Save'
      TabOrder = 2
      OnClick = BtSaveClick
    end
    object BtExport: TButton
      Left = 241
      Top = 0
      Width = 80
      Height = 22
      Caption = '&Export'
      TabOrder = 3
      OnClick = BtExportClick
    end
    object BtReset: TButton
      Left = 321
      Top = 0
      Width = 80
      Height = 22
      Caption = '&Reset'
      TabOrder = 4
      OnClick = BtResetClick
    end
    object BtInfo: TButton
      Left = 401
      Top = 0
      Width = 77
      Height = 22
      Caption = '&Info'
      TabOrder = 5
    end
    object BtErrors: TButton
      Left = 478
      Top = 0
      Width = 83
      Height = 22
      Caption = '&Errors'
      TabOrder = 6
    end
    object BtFunct: TButton
      Left = 561
      Top = 0
      Width = 80
      Height = 22
      Caption = '&Funct'
      TabOrder = 7
    end
    object BtConfig: TButton
      Left = 641
      Top = 0
      Width = 80
      Height = 22
      Caption = '&Config'
      TabOrder = 8
    end
    object BtHelp: TButton
      Left = 721
      Top = 0
      Width = 80
      Height = 22
      Caption = '&Help'
      TabOrder = 9
    end
    object BtExit: TButton
      Left = 801
      Top = 0
      Width = 80
      Height = 22
      Caption = 'E&xit'
      TabOrder = 10
      OnClick = BtExitClick
    end
  end
  object GBCoefficients: TGroupBox
    Left = 4
    Top = 30
    Width = 205
    Height = 499
    Caption = ' coefficients '
    TabOrder = 1
    object Lbt0: TLabel
      Left = 8
      Top = 40
      Width = 15
      Height = 13
      Caption = 't0='
    end
    object Lbt1: TLabel
      Left = 8
      Top = 64
      Width = 15
      Height = 13
      Caption = 't1='
    end
    object Lbt2: TLabel
      Left = 8
      Top = 88
      Width = 15
      Height = 13
      Caption = 't2='
    end
    object Lbt3: TLabel
      Left = 8
      Top = 112
      Width = 15
      Height = 13
      Caption = 't3='
    end
    object Lbt4: TLabel
      Left = 8
      Top = 136
      Width = 15
      Height = 13
      Caption = 't4='
    end
    object Lbt5: TLabel
      Left = 8
      Top = 160
      Width = 15
      Height = 13
      Caption = 't5='
    end
    object Lba0: TLabel
      Left = 8
      Top = 208
      Width = 18
      Height = 13
      Caption = 'a0='
    end
    object Lba1: TLabel
      Left = 8
      Top = 232
      Width = 18
      Height = 13
      Caption = 'a1='
    end
    object Lba2: TLabel
      Left = 8
      Top = 256
      Width = 18
      Height = 13
      Caption = 'a2='
    end
    object Lba3: TLabel
      Left = 8
      Top = 280
      Width = 18
      Height = 13
      Caption = 'a3='
    end
    object Lba4: TLabel
      Left = 8
      Top = 304
      Width = 18
      Height = 13
      Caption = 'a4='
    end
    object Lba5: TLabel
      Left = 8
      Top = 328
      Width = 18
      Height = 13
      Caption = 'a5='
    end
    object Lbb1: TLabel
      Left = 8
      Top = 376
      Width = 18
      Height = 13
      Caption = 'b1='
    end
    object Lbb2: TLabel
      Left = 8
      Top = 400
      Width = 18
      Height = 13
      Caption = 'b2='
    end
    object Lbb3: TLabel
      Left = 8
      Top = 424
      Width = 18
      Height = 13
      Caption = 'b3='
    end
    object Lbb4: TLabel
      Left = 8
      Top = 448
      Width = 18
      Height = 13
      Caption = 'b4='
    end
    object Lbb5: TLabel
      Left = 8
      Top = 472
      Width = 18
      Height = 13
      Caption = 'b5='
    end
    object LbTemp: TLabel
      Left = 64
      Top = 16
      Width = 94
      Height = 13
      Caption = 'temporary variables:'
    end
    object Label1: TLabel
      Left = 56
      Top = 184
      Width = 100
      Height = 13
      Caption = 'a-coefficients (zeros):'
    end
    object Label2: TLabel
      Left = 56
      Top = 352
      Width = 100
      Height = 13
      Caption = 'b-coefficients (poles):'
    end
    object Edt0: TEdit
      Left = 32
      Top = 37
      Width = 161
      Height = 21
      TabOrder = 0
    end
    object Edt1: TEdit
      Left = 32
      Top = 61
      Width = 161
      Height = 21
      TabOrder = 1
    end
    object Edt2: TEdit
      Left = 32
      Top = 85
      Width = 161
      Height = 21
      TabOrder = 2
    end
    object Edt3: TEdit
      Left = 32
      Top = 109
      Width = 161
      Height = 21
      TabOrder = 3
    end
    object Edt4: TEdit
      Left = 32
      Top = 133
      Width = 161
      Height = 21
      TabOrder = 4
    end
    object Edt5: TEdit
      Left = 32
      Top = 157
      Width = 161
      Height = 21
      TabOrder = 5
    end
    object Eda5: TEdit
      Left = 32
      Top = 325
      Width = 161
      Height = 21
      TabOrder = 6
    end
    object Eda4: TEdit
      Left = 32
      Top = 301
      Width = 161
      Height = 21
      TabOrder = 7
    end
    object Eda3: TEdit
      Left = 32
      Top = 277
      Width = 161
      Height = 21
      TabOrder = 8
    end
    object Eda2: TEdit
      Left = 32
      Top = 253
      Width = 161
      Height = 21
      TabOrder = 9
    end
    object Eda1: TEdit
      Left = 32
      Top = 229
      Width = 161
      Height = 21
      TabOrder = 10
    end
    object Eda0: TEdit
      Left = 32
      Top = 205
      Width = 161
      Height = 21
      TabOrder = 11
    end
    object Edb5: TEdit
      Left = 32
      Top = 469
      Width = 161
      Height = 21
      TabOrder = 12
    end
    object Edb4: TEdit
      Left = 32
      Top = 445
      Width = 161
      Height = 21
      TabOrder = 13
    end
    object Edb3: TEdit
      Left = 32
      Top = 421
      Width = 161
      Height = 21
      TabOrder = 14
    end
    object Edb2: TEdit
      Left = 32
      Top = 397
      Width = 161
      Height = 21
      TabOrder = 15
    end
    object Edb1: TEdit
      Left = 32
      Top = 373
      Width = 161
      Height = 21
      TabOrder = 16
    end
  end
  object GBControls: TGroupBox
    Left = 216
    Top = 32
    Width = 185
    Height = 257
    Caption = ' controls '
    TabOrder = 2
    object Lbc0: TLabel
      Left = 10
      Top = 16
      Width = 18
      Height = 13
      Caption = 'c0: '
    end
    object Lbc1: TLabel
      Left = 10
      Top = 56
      Width = 18
      Height = 13
      Caption = 'c1: '
    end
    object Lbc2: TLabel
      Left = 10
      Top = 96
      Width = 18
      Height = 13
      Caption = 'c2: '
    end
    object Lb3: TLabel
      Left = 10
      Top = 136
      Width = 18
      Height = 13
      Caption = 'c3: '
    end
    object Lbc4: TLabel
      Left = 10
      Top = 176
      Width = 18
      Height = 13
      Caption = 'c4: '
    end
    object Lbc5: TLabel
      Left = 10
      Top = 216
      Width = 18
      Height = 13
      Caption = 'c5: '
    end
    object SBc0: TScrollBar
      Left = 8
      Top = 32
      Width = 169
      Height = 16
      PageSize = 0
      TabOrder = 0
    end
    object SBc1: TScrollBar
      Left = 8
      Top = 72
      Width = 169
      Height = 16
      PageSize = 0
      TabOrder = 1
    end
    object SBc2: TScrollBar
      Left = 8
      Top = 112
      Width = 169
      Height = 16
      PageSize = 0
      TabOrder = 2
    end
    object SBc3: TScrollBar
      Left = 8
      Top = 152
      Width = 169
      Height = 16
      PageSize = 0
      TabOrder = 3
    end
    object SBc4: TScrollBar
      Left = 8
      Top = 192
      Width = 169
      Height = 16
      PageSize = 0
      TabOrder = 4
    end
    object SBc5: TScrollBar
      Left = 8
      Top = 232
      Width = 169
      Height = 16
      PageSize = 0
      TabOrder = 5
    end
  end
  object GBPlaneView: TGroupBox
    Left = 216
    Top = 296
    Width = 185
    Height = 233
    Caption = ' plane view '
    TabOrder = 3
    object PBPlaneView: TPaintBox
      Left = 8
      Top = 16
      Width = 169
      Height = 169
    end
    object LbNumberOfZeros: TLabel
      Left = 8
      Top = 192
      Width = 90
      Height = 13
      Caption = 'number of zeros (): '
    end
    object LbNumberOfPoles: TLabel
      Left = 8
      Top = 208
      Width = 95
      Height = 13
      Caption = 'number of poles (x): '
    end
  end
  object GBPlotWindow: TGroupBox
    Left = 408
    Top = 32
    Width = 470
    Height = 497
    Caption = ' plot window '
    TabOrder = 4
  end
  object GBInfo: TGroupBox
    Left = 4
    Top = 534
    Width = 874
    Height = 123
    Caption = ' info window '
    TabOrder = 5
    object InfoWindow: TMemo
      Left = 8
      Top = 23
      Width = 857
      Height = 89
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
    end
  end
end
