object FmPascalScript: TFmPascalScript
  Left = 389
  Top = 249
  BorderStyle = bsNone
  Caption = 'Pascal Script VST Effect Plugin'
  ClientHeight = 354
  ClientWidth = 568
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 0
    Top = 301
    Width = 568
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object SynEdit: TSynEdit
    Left = 0
    Top = 26
    Width = 568
    Height = 275
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    OnKeyDown = SynEditKeyDown
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Highlighter = SynPasSyn
    Lines.Strings = (
      'var'
      '  Temp : Array [0..1] of Double;'
      ''
      
        'procedure VSTProcessSample(Channel : Integer; var Data : Double)' +
        ';'
      'begin'
      ' // simple lowpass filter'
      ' Data := 0.1 * Data + 0.9 * Temp[Channel];'
      ' Temp[Channel] := Data;'
      'end;')
  end
  object DebugBox: TListBox
    Left = 0
    Top = 304
    Width = 568
    Height = 50
    Align = alBottom
    ItemHeight = 13
    TabOrder = 1
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 568
    Height = 26
    Caption = 'ToolBar'
    EdgeInner = esNone
    EdgeOuter = esNone
    Flat = False
    TabOrder = 2
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Width = 4
      Caption = 'ToolButton1'
      Style = tbsSeparator
    end
    object BtRun: TButton
      Left = 4
      Top = 0
      Width = 75
      Height = 22
      Action = ACCompile
      TabOrder = 2
    end
    object ToolButton2: TToolButton
      Left = 79
      Top = 0
      Width = 25
      Caption = 'ToolButton2'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object BtLoadScript: TButton
      Left = 104
      Top = 0
      Width = 94
      Height = 22
      Caption = '&Load Script...'
      TabOrder = 0
      OnClick = BtLoadScriptClick
    end
    object ToolButton3: TToolButton
      Left = 198
      Top = 0
      Width = 4
      Caption = 'ToolButton3'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object BtSaveScript: TButton
      Left = 202
      Top = 0
      Width = 96
      Height = 22
      Caption = '&Save Script...'
      TabOrder = 1
      OnClick = BtSaveScriptClick
    end
  end
  object SynPasSyn: TSynPasSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clHighlight
    FloatAttri.Foreground = clOlive
    StringAttri.Foreground = clTeal
    PackageSource = False
    Left = 8
    Top = 96
  end
  object ActionList: TActionList
    Left = 40
    Top = 96
    object ACCompile: TAction
      Caption = '&Compile'
      ShortCut = 120
      OnExecute = ACCompileExecute
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.vps'
    Filter = 'VST Pascal Script (*.vps)|*.vps'
    Title = 'Load VST Pascal Script'
    Left = 72
    Top = 96
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.vps'
    Filter = 'VST Pascal Script (*.vps)|*.vps'
    Title = 'Save VST Pascal Script'
    Left = 104
    Top = 96
  end
end
