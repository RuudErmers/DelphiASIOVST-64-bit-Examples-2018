object FmVST: TFmVST
  Left = 343
  Top = 356
  Caption = 'VST Setup'
  ClientHeight = 128
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    299
    128)
  PixelsPerInch = 96
  TextHeight = 13
  object GbOutputVST: TGroupBox
    Left = 8
    Top = 67
    Width = 284
    Height = 54
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Output VST '
    TabOrder = 0
    object LbOutputVST: TLabel
      Left = 11
      Top = 25
      Width = 22
      Height = 13
      Caption = 'VST:'
    end
    object EdOutputVST: TEdit
      Left = 39
      Top = 22
      Width = 185
      Height = 21
      TabOrder = 0
      OnChange = EdOutputVSTChange
    end
    object BtOutputVST: TButton
      Left = 204
      Top = 24
      Width = 18
      Height = 18
      Caption = '...'
      TabOrder = 1
      OnClick = BtOutputVSTClick
    end
    object BtOutputEditor: TButton
      Left = 230
      Top = 22
      Width = 43
      Height = 21
      Caption = 'Editor'
      Enabled = False
      TabOrder = 2
      OnClick = BtOutputEditorClick
    end
  end
  object GbRealtimeVST: TGroupBox
    Left = 8
    Top = 4
    Width = 284
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Realtime FX VST '
    TabOrder = 1
    object LbRealtimeVST: TLabel
      Left = 11
      Top = 25
      Width = 22
      Height = 13
      Caption = 'VST:'
    end
    object EdRealtimeVST: TEdit
      Left = 39
      Top = 22
      Width = 185
      Height = 21
      TabOrder = 0
      OnChange = EdRealtimeVSTChange
    end
    object BtRealtimeVST: TButton
      Left = 204
      Top = 24
      Width = 18
      Height = 18
      Caption = '...'
      TabOrder = 1
      OnClick = BtRealtimeVSTClick
    end
    object BtRealtimeEditor: TButton
      Left = 230
      Top = 22
      Width = 43
      Height = 21
      Caption = 'Editor'
      Enabled = False
      TabOrder = 2
      OnClick = BtRealtimeEditorClick
    end
  end
end
