object FmHarmonicBassClone: TFmHarmonicBassClone
  Left = 501
  Top = 225
  Caption = 'MaxxBass Clone'
  ClientHeight = 300
  ClientWidth = 505
  Color = 10592673
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object OutputMeterLeft: TGuiColorLevelMeter
    Left = 433
    Top = 78
    Width = 17
    Height = 135
    BorderColor = clWindowFrame
    ContrastLuminance = 0.300000011920929000
    Upper = 1.000000000000000000
  end
  object OutputMeterRight: TGuiColorLevelMeter
    Left = 480
    Top = 78
    Width = 17
    Height = 131
    BorderColor = clWindowFrame
    ContrastLuminance = 0.300000011920929000
    Upper = 1.000000000000000000
  end
  object LbOutput: TGuiLabel
    Left = 440
    Top = 38
    Width = 48
    Height = 14
    Alignment = taCenter
    Caption = 'Output'
    Shadow.Blur = 1.000000000000000000
    Shadow.Color = 16777203
    Shadow.Opacity = 192
    Shadow.Visible = True
  end
  object GuiLabel2: TGuiLabel
    Left = 14
    Top = 179
    Width = 57
    Height = 14
    Caption = 'Frequency:'
    Shadow.Blur = 1.000000000000000000
    Shadow.Color = 16777203
    Shadow.Opacity = 192
    Shadow.Visible = True
  end
  object GuiLabel1: TGuiLabel
    Left = 295
    Top = 38
    Width = 32
    Height = 14
    Alignment = taCenter
    Caption = 'Input'
    Shadow.Blur = 1.000000000000000000
    Shadow.Color = 16777203
    Shadow.Opacity = 192
    Shadow.Visible = True
  end
  object GuiLabel3: TGuiLabel
    Left = 330
    Top = 38
    Width = 42
    Height = 14
    Alignment = taCenter
    Caption = 'Original'
    Shadow.Blur = 1.000000000000000000
    Shadow.Color = 16777203
    Shadow.Opacity = 192
    Shadow.Visible = True
  end
  object GuiLabel4: TGuiLabel
    Left = 376
    Top = 38
    Width = 32
    Height = 14
    Alignment = taCenter
    Caption = 'Maxx'
    Shadow.Blur = 1.000000000000000000
    Shadow.Color = 16777203
    Shadow.Opacity = 192
    Shadow.Visible = True
  end
  object EqGraph: TGuiEQGraph
    Left = 14
    Top = 41
    Width = 263
    Height = 132
    BorderRadius = 2
    ColorChart = 2293760
    FilterSeries = <
      item
        DisplayName = 'Original Bass'
        Color = 16744448
      end
      item
        DisplayName = 'MaxxBass'
        Color = clOlive
      end>
    GraphColorDark = 5779742
    GraphColorLight = 3152656
    XAxis.UpperFrequency = 100.000000000000000000
    XAxis.LowerFrequency = 16.000000000000000000
    YAxis.LowerLevel = -24.000000000000000000
    YAxis.UpperLevel = 6.000000000000000000
    YAxis.Granularity = 4.000000000000000000
    Color = 10592673
    ParentColor = False
  end
  object LbAudio: TGuiButton
    Left = 433
    Top = 239
    Width = 64
    Height = 14
    Alignment = taCenter
    BorderColor = clBlack
    BorderWidth = 1.250000000000000000
    ButtonColor = 9351376
    Caption = 'Audio'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    BorderRadius = 2.000000000000000000
    Transparent = False
  end
  object LbMaxxBass: TGuiButton
    Left = 433
    Top = 254
    Width = 64
    Height = 14
    Alignment = taCenter
    BorderColor = clBlack
    BorderWidth = 1.250000000000000000
    ButtonColor = 10592673
    Caption = 'MaxxBass'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    BorderRadius = 2.000000000000000000
    Transparent = False
  end
  object LbOriginalBass: TGuiButton
    Left = 433
    Top = 269
    Width = 64
    Height = 14
    Alignment = taCenter
    BorderColor = clBlack
    BorderWidth = 1.250000000000000000
    ButtonColor = 10592673
    Caption = 'Original Bass'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    BorderRadius = 2.000000000000000000
    Transparent = False
  end
  object LbClipIndicator: TGuiButton
    Left = 432
    Top = 58
    Width = 65
    Height = 14
    Alignment = taCenter
    BorderColor = 6700041
    BorderWidth = 1.250000000000000000
    ButtonColor = 2293760
    Caption = 'No Clip'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 16777203
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    BorderRadius = 2.000000000000000000
    Transparent = False
  end
  object SBFrequency: TGuiSlider
    Left = 77
    Top = 179
    Width = 141
    Height = 16
    BorderRadius = 4.000000000000000000
    BorderWidth = 1.000000000000000000
    Color = 10592673
    DefaultValue = 50.000000000000000000
    Max = 100.000000000000000000
    ParentColor = False
    Value = 50.000000000000000000
    SlideColor = 6447714
  end
  object GuiButton1: TGuiButton
    Left = 224
    Top = 179
    Width = 53
    Height = 15
    Alignment = taCenter
    BorderColor = 6700041
    BorderWidth = 1.250000000000000000
    ButtonColor = 2293760
    Caption = '85 Hz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 16777203
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    BorderRadius = 2.000000000000000000
    Transparent = False
  end
  object GuiButton2: TGuiButton
    Left = 8
    Top = 8
    Width = 41
    Height = 16
    Alignment = taCenter
    BorderColor = clBlack
    BorderWidth = 1.250000000000000000
    ButtonColor = 10592673
    Caption = 'Undo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    BorderRadius = 2.000000000000000000
    Transparent = False
  end
  object GuiButton3: TGuiButton
    Left = 55
    Top = 8
    Width = 163
    Height = 16
    Alignment = taCenter
    BorderColor = clBlack
    BorderWidth = 1.250000000000000000
    ButtonColor = 10592673
    Caption = 'Preset: Medium'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    BorderRadius = 2.000000000000000000
    Transparent = False
  end
  object GuiButton4: TGuiButton
    Left = 224
    Top = 8
    Width = 53
    Height = 16
    Alignment = taCenter
    BorderColor = clBlack
    BorderWidth = 1.250000000000000000
    ButtonColor = 10592673
    Caption = 'A -> B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    BorderRadius = 2.000000000000000000
    Transparent = False
  end
  object GuiButton5: TGuiButton
    Left = 283
    Top = 8
    Width = 53
    Height = 16
    Alignment = taCenter
    BorderColor = clBlack
    BorderWidth = 1.250000000000000000
    ButtonColor = 10592673
    Caption = 'Load'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    BorderRadius = 2.000000000000000000
    Transparent = False
  end
  object GuiButton6: TGuiButton
    Left = 342
    Top = 8
    Width = 53
    Height = 16
    Alignment = taCenter
    BorderColor = clBlack
    BorderWidth = 1.250000000000000000
    ButtonColor = 10592673
    Caption = 'Save'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    BorderRadius = 2.000000000000000000
    Transparent = False
  end
  object GuiButton7: TGuiButton
    Left = 401
    Top = 8
    Width = 16
    Height = 16
    Alignment = taCenter
    BorderColor = clBlack
    BorderWidth = 1.250000000000000000
    ButtonColor = 10592673
    Caption = '?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    BorderRadius = 2.000000000000000000
    Transparent = False
  end
  object GuiButton8: TGuiButton
    Left = 433
    Top = 219
    Width = 30
    Height = 14
    Alignment = taCenter
    BorderColor = 6700041
    BorderWidth = 1.250000000000000000
    ButtonColor = 2293760
    Caption = '-4.0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 6078697
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    BorderRadius = 2.000000000000000000
    Transparent = False
  end
  object GuiButton9: TGuiButton
    Left = 467
    Top = 219
    Width = 30
    Height = 14
    Alignment = taCenter
    BorderColor = 6700041
    BorderWidth = 1.250000000000000000
    ButtonColor = 2293760
    Caption = '-4.0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 6078697
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    BorderRadius = 2.000000000000000000
    Transparent = False
  end
  object GuiFader1: TGuiFader
    Left = 295
    Top = 78
    Width = 32
    Height = 175
    ImageIndex = 0
    ImageList = GuiPNGList
    Maximum = 100.000000000000000000
    DefaultValue = 0.000000000000000000
    Value = 0.000000000000000000
  end
  object GuiFader2: TGuiFader
    Left = 335
    Top = 78
    Width = 32
    Height = 175
    ImageIndex = 0
    ImageList = GuiPNGList
    Maximum = 100.000000000000000000
    DefaultValue = 0.000000000000000000
    Value = 0.000000000000000000
  end
  object GuiFader3: TGuiFader
    Left = 376
    Top = 78
    Width = 32
    Height = 175
    ImageIndex = 0
    ImageList = GuiPNGList
    Maximum = 100.000000000000000000
    DefaultValue = 0.000000000000000000
    Value = 0.000000000000000000
  end
  object GuiPNGList: TGuiPNGList
    PNGs = <
      item
        PortableNetworkGraphic.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC300000EC301C76FA8640000000467414D410000
          B18F0BFC6105000000017352474200AECE1CE900000543494441545847C5575B
          4856591875901966C2C1174B5314CDCB83E3A5874C2DCDACD050311CC52709F4
          640A9A687929ADCCD2F252965A9AB763792B2F999952880F225E827C097FA8C8
          CC18A7140CA4CC7434BFD96B33FB70E61F7FE6FCD3C33C2C44F8397BEDF5AD6F
          7DDF36212293FF13860EDFC4481D669036C2D7AF5FA5B5B535697575555A5E5E
          96BE7CF9222D2D2D498B8B8BD2A74F9FA48F1F3F4A0B0B0B1CF3F3F3D2E0E0E0
          E11B376EE09BFF38CF1081ACF5F5F5950F1F3ED0DCDC1CBD7FFF9EDEBD7B4733
          3333F4F6ED5B9A9E9EA6376FDED0EBD7AF697272925EBD7A452F5FBEA4172F5E
          D0F3E7CF49A7D3D1C4C404C7B367CFA8B7B777252525A5D0C4C4E4077D128608
          E8F0B19E9E1EEAEAEAA27BF7EE51676727B5B7B7535B5B1BDDB973875A5A5AA8
          B9B9999A9A9AE8F6EDDB74EBD62D6A6868A0FAFA7AAAABABA3DADA5AAAAEAEA6
          9B376F527171311D3870E03746C09DE17B35098304868787E9F1E3C7F4E8D123
          8EBEBE3EDC841E3E7CC8893D78F080EEDFBFAF1004C98E8E0E4E12B87BF72E07
          C8565555D1AE5DBB7E67878733FCA489C0C0C0003D79F284C6C6C636C4E8E828
          19C2C8C808A9D1DFDF4FDBB76F07815F19FEE605830AE0E0F1F1F10DF1F4E953
          32064343434201ED042037EA0E0869517F40C8DBDADA4A00FCA0F6047CD1D8D8
          A878A3BCBC9CDCDCDC8C5300044A4A4AA8B0B0902E5DBAA4E0E2C58B24505050
          406AE4E7E71370E1C20505E7CF9FA7ACAC2CF2F2F2328E006E999D9D4D274E9C
          F8261C3F7E9C9292928C57002EC7EDC58D7093BCBC3C8E73E7CE516E6E2EC7D9
          B36739CE9C3943A74F9FE6C8C9C9E1E44F9D3A45274F9E249030DA84683B5147
          F4B72CCBBCC7D1DF3535351CE8F1CACA4A622947151515845A979595D1D5AB57
          A9B4B4942E5FBECCCB0852EEEEEEC69500FDAE2564D0E36A02D7AE5DE304AE5C
          B9A210801AAEAEAEC61140F8088723F1D4AE166A20F1841282C8F5EBD7152540
          064AA0441E1E1EDA09ACACACE8D07EFA912A24C721901C728B43C48D11BB4545
          45BC6BD021F0507A7A3A3939396927C0A25287C3F7EDDBC7111818C8B177EF5E
          8E808000DAB3678F027F7F7FF2F3F3E3D8BD7B37078B5E053B76EC204F4F4FED
          04D858D5A18EE2A31B7DD8D7D7977C7C7C14787B7BD3CE9D3B3958CF2BC0E14C
          7EB2B7B7D74E000A0813A2F69874FA5D804987BAA30BD4250171FD2E405B1A65
          C2CF9F3FEB10B9EA430C391D6DA6AE3B5212B5471A8AEC484D4D25070707ED0A
          C08418AFC284869C2E5A4EDDF32023E21B6484099D9D9DB513602B960E335D2C
          14226CD42DA69619876EE47EA1405A5A9A710AB0BD4E2716897F0B19B49CB8B1
          683BC82FE21A1970ECD831B2B3B3334E018C601134FA5ED0CF01FDE4D3278581
          B66DDB36ED04D832AAC37CEFEEEEE66B97FEEA2576049034B42B404100698A6E
          D9BA75AB7602F000A6A1FE6AF55FFFC77AE7E8E8A89DC0ECECAC0ECE479CAA17
          10438B88A8BD5848D47FE1078CEDCD9B376B27001322F7939393292121E19B11
          1B1B6B9C09A100860DDC9B9898C8A12672F4E85102E2E3E3151C397284244952
          1017174702313131C629303535C587910815B5F4EA84131B91D88610B9D882B0
          03666666524646065FE7222222C8DCDC5C7B09580865B1F0F8233A3A9AA2A2A2
          38222323F9870E1D3A44E1E1E114161646A1A1A114121242070F1EA4E0E0600A
          0A0AC20B88F6EFDFCFA72826270699A5A5E52A7B0F74687E1730F937B183B25D
          5C5CAA997B65D6C332CB72994D340E162A1CB6B6B6B28D8D8D6C6D6DCDC15A4D
          B6B2B292D981F2962D5B64663CD9C2C242363333AB3535358D6704BCF5DF8706
          9FE6EC873F32B8FCF59EC39BEE5BF10BFBC6CF0CDFA99F667F02BAE51E3970FC
          4B550000000049454E44AE426082}
        DisplayName = 'Fader'
        Height = 32
        Width = 32
      end>
    Left = 320
    Top = 248
  end
end
