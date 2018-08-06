object FmVectorGraphicTest: TFmVectorGraphicTest
  Left = 392
  Top = 67
  Caption = 'Vector Graphic Test'
  ClientHeight = 304
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClick = FormClick
  OnClose = FormClose
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    421
    304)
  PixelsPerInch = 96
  TextHeight = 13
  object LbTestType: TLabel
    Left = 8
    Top = 8
    Width = 25
    Height = 13
    Caption = 'Test:'
  end
  object PaintBox: TPaintBox
    Left = 8
    Top = 32
    Width = 405
    Height = 264
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnClick = PaintBoxClick
    OnDblClick = FormDblClick
    OnPaint = FormPaint
  end
  object CbTestType: TComboBox
    Left = 39
    Top = 5
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 0
    Text = 'Filled Rectangle'
    OnChange = CbTestTypeChange
    Items.Strings = (
      'Filled Rectangle'
      'Filled Rounded Rectangle'
      'Filled Circle'
      'Filled Circle Sector'
      'Filled Ellipse'
      'Framed Rectangle'
      'Framed Rounded Rectangle'
      'Framed Circle'
      'Framed Circle Sector'
      'Framed Ellipse'
      'Thin Line'
      'Line')
  end
  object CbDraft: TCheckBox
    Left = 190
    Top = 8
    Width = 49
    Height = 16
    Caption = 'Draft'
    TabOrder = 1
    OnClick = CbDraftClick
  end
  object ApplicationEvents: TApplicationEvents
    Left = 32
    Top = 41
  end
end
