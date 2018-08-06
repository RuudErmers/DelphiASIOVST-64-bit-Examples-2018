object FmCustomGr32Gui: TFmCustomGr32Gui
  Left = 327
  Top = 117
  BorderStyle = bsNone
  Caption = 'Graphics32 GUI'
  ClientHeight = 133
  ClientWidth = 262
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnKeyDown = Gr32GuiKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Gr32Gui: TImage32
    Left = 0
    Top = 0
    Width = 262
    Height = 133
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    PopupMenu = PopupMenu
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnKeyDown = Gr32GuiKeyDown
    OnMouseDown = Gr32GuiMouseDown
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 120
    Top = 8
    object MiDesignMode: TMenuItem
      Caption = '&Design Mode'
      OnClick = MiDesignModeClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object MiParameter: TMenuItem
      Caption = 'Parameter'
    end
    object MiProperties: TMenuItem
      Caption = 'Properties'
      Visible = False
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MiAddBitmap: TMenuItem
      Caption = 'Add Bitmap...'
      OnClick = MiAddBitmapClick
    end
    object MiAddDial: TMenuItem
      Caption = 'Add Dial...'
      OnClick = MiAddDialClick
    end
    object MiAddText: TMenuItem
      Caption = 'Add Text'
      OnClick = MiAddTextClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MiSetBackground: TMenuItem
      Caption = 'Set Background...'
      OnClick = MiSetBackgroundClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object MiLoad: TMenuItem
      Caption = 'Load...'
      OnClick = MiLoadClick
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 32
    Top = 8
  end
end
