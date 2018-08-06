object FmEqSlideTest: TFmEqSlideTest
  Left = 623
  Top = 235
  Caption = 'EQ-Slide Test'
  ClientHeight = 56
  ClientWidth = 308
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GuiEQSlide1: TGuiEQSlide
    Left = 8
    Top = 8
    Width = 145
    Height = 17
    BorderRadius = 5
    XAxis.UpperFrequency = 20000.000000000000000000
    XAxis.LowerFrequency = 20.000000000000000000
    OnGetColor = GuiEQSlideGetColor
    Color = clBtnFace
    ParentColor = False
  end
  object GuiEQSlide2: TGuiEQSlide
    Left = 8
    Top = 31
    Width = 145
    Height = 17
    BorderRadius = 5
    XAxis.UpperFrequency = 20000.000000000000000000
    XAxis.LowerFrequency = 20.000000000000000000
    OnGetColor = GuiEQSlideGetColorBW
    Color = clBtnFace
    ParentColor = False
  end
  object GuiEQSlide3: TGuiEQSlide
    Left = 159
    Top = 8
    Width = 145
    Height = 17
    BorderRadius = 5
    XAxis.UpperFrequency = 20000.000000000000000000
    XAxis.LowerFrequency = 20.000000000000000000
    OnGetColor = GuiEQSlideGetColor
    Color = clBtnFace
    ParentColor = False
  end
  object GuiEQSlide4: TGuiEQSlide
    Left = 159
    Top = 31
    Width = 145
    Height = 17
    BorderRadius = 5
    XAxis.UpperFrequency = 20000.000000000000000000
    XAxis.LowerFrequency = 20.000000000000000000
    OnGetColor = GuiEQSlideGetColorBW
    Color = clBtnFace
    ParentColor = False
  end
end
