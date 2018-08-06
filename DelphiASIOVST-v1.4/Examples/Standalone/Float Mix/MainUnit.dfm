object FmFloatMix: TFmFloatMix
  Left = 299
  Top = 55
  Caption = 'Float Mix'
  ClientHeight = 148
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    378
    148)
  PixelsPerInch = 96
  TextHeight = 13
  object LbOutput: TLabel
    Left = 8
    Top = 11
    Width = 57
    Height = 13
    Caption = 'Output File:'
  end
  object LbInputFiles: TLabel
    Left = 8
    Top = 36
    Width = 54
    Height = 13
    Caption = 'Input Files:'
  end
  object LbAccuracy: TLabel
    Left = 320
    Top = 36
    Width = 48
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Accuracy:'
  end
  object EdOutputFile: TEdit
    Left = 71
    Top = 8
    Width = 243
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object BtMix: TButton
    Left = 320
    Top = 8
    Width = 50
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '&Mix!'
    Default = True
    Enabled = False
    TabOrder = 1
    OnClick = BtMixClick
  end
  object ListInputFiles: TListBox
    Left = 71
    Top = 36
    Width = 243
    Height = 104
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
    OnClick = ListInputFilesClick
    OnEnter = ListInputFilesEnter
    OnExit = ListInputFilesExit
  end
  object Rb8Bit: TRadioButton
    Left = 320
    Top = 52
    Width = 49
    Height = 17
    Anchors = [akTop, akRight]
    Caption = '8-Bit'
    TabOrder = 3
    OnClick = Rb8BitClick
  end
  object Rb16Bit: TRadioButton
    Left = 320
    Top = 70
    Width = 49
    Height = 17
    Anchors = [akTop, akRight]
    Caption = '16-Bit'
    TabOrder = 4
    OnClick = Rb16BitClick
  end
  object Rb32Bit: TRadioButton
    Left = 320
    Top = 87
    Width = 49
    Height = 17
    Anchors = [akTop, akRight]
    Caption = '32-Bit'
    TabOrder = 5
    OnClick = Rb32BitClick
  end
  object Rb64Bit: TRadioButton
    Left = 320
    Top = 105
    Width = 49
    Height = 17
    Anchors = [akTop, akRight]
    Caption = '64-Bit'
    TabOrder = 6
    OnClick = Rb64BitClick
  end
  object Rb80Bit: TRadioButton
    Left = 320
    Top = 123
    Width = 49
    Height = 17
    Anchors = [akTop, akRight]
    Caption = '80-Bit'
    TabOrder = 7
    OnClick = Rb80BitClick
  end
  object BtAdd: TButton
    Left = 8
    Top = 52
    Width = 57
    Height = 22
    Caption = 'Add...'
    TabOrder = 8
    OnClick = BtAddClick
  end
  object BtOutputfile: TButton
    Left = 292
    Top = 10
    Width = 20
    Height = 17
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 9
    OnClick = BtOutputfileClick
  end
  object FdLevel: TGuiFader
    Left = 17
    Top = 80
    Width = 33
    Height = 33
    Anchors = [akLeft, akTop, akBottom]
    ImageIndex = 0
    ImageList = GuiPNGList
    CurveMapping = 1.000000000000000000
    Minimum = -96.000000000000000000
    Maximum = 6.000000000000000000
    Visible = False
    OnChange = FdLevelChange
    DefaultValue = 0.000000000000000000
    Value = 0.000000000000000000
  end
  object EdLevel: TEdit
    Left = 8
    Top = 119
    Width = 57
    Height = 21
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    ReadOnly = True
    TabOrder = 11
    Visible = False
    OnChange = EdLevelChange
    OnEnter = EdLevelEnter
    OnExit = EdLevelExit
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.wav'
    Left = 176
    Top = 73
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 240
    Top = 72
  end
  object GuiPNGList: TGuiPNGList
    PNGs = <
      item
        PortableNetworkGraphic.Data = {
          89504E470D0A1A0A0000000D4948445200000016000000160806000000C4B46C
          3B000000097048597300000EC300000EC301C76FA8640000000467414D410000
          B18F0BFC6105000000017352474200AECE1CE90000031949444154484BB55437
          6B5C41183C306E6C63702197FE01328B242424810A77EECF9D108A2873CA19E5
          533CA553CE3987E204CA395492FE8D5FF1DAF1370B2BDED9F7CEB8F083691ECB
          CC7CB3B39F0780E77F208C34140A7D3C3838F0EFEFEFFFDCDDDDB5B6B7B7AD8D
          8D0D6B7D7DDD5A5959B1969696ACF9F9796B6E6ECE9A9E9EB6A6A6A6AC898909
          ABBFBF3F909595F5D969308CF8FCFCFCEBD5D5152E2E2E70767686E3E3631C1D
          1DE1F0F050434421A2D8D9D981884244B1B6B6061185D7EBFDE1F178DE1AF230
          6239A84870737383EBEB6B50E4F2F252836222AC0589D3D3539C9C9C68719914
          A9A9A93E21FE1091786F6F4F91E4E5E5E50F3C3F3F23129E9E9EF0F8F888B4B4
          347762C95271BCC5C5454896902C313B3B8B99991948A6904C35262727313E3E
          AE313A3A8AC1C1C1E88E252FB5B0B080E6E666343434BCA2BEBE1EBFA3AEAE0E
          446D6D2DAAABABFF4EBCB5B5A55D3A9DD1553018C4F0F0308686863030308040
          2080BEBE3EF4F6F6A2A7A707292929EE51C8E83A8AD5D5552C2F2FEB48380185
          18858980422323235A88225237242525B9130BA1920BD475A27323C23A397367
          E69C88198F8D8D6991A88EE5A0A283D2D2528DE2E262141515A1A0A000F9F9F9
          C8CBCB436E6E2E727272909D9D8DCCCC4C64646468242424B83B96F195BC24D4
          D4D4A0AAAA0A959595282F2F475959197C3E9F162B2929D1828585856182C9C9
          C9EEC4929D62C54CB54C9D4C9EAC15F3E4A5757777C3EFF7A3A3A3036D6D6D88
          8F8F77271642C53C6527BCF6D8E4C94998A76907454C332892989818DD71A4AA
          D1B1A9191DB3625D5D5DDA717B7B3B5A5B5BA33B66142466C5F8F29C35A36367
          9F8D630AD171D428C499DADCDCC4C3C3C32BEEEFEFE1C4DDDD1D0C6E6F6F4170
          214525964BD1AD302F8CE3D39913ACA313CC99D128A5DC33A6631E64ADD85F27
          58AF48301D8F8B8B7327EEECEC5454677DB8889A9A9AD0D8D8A81710170E970D
          515151A1FBEDEC766C6CAC3BB1DCEE275928D2A8A02D71D812852D13D87241B6
          FCB7A509B6F45643CEDA2D2D2DB688DBE9E9E9A1989818AF2CFA7711173D7FCA
          F75EA004DFFF01DFE4EC17C11B43FC0BE02EAFE1479CA49F0000000049454E44
          AE426082}
        DisplayName = 'Fader'
        Height = 22
        Width = 22
      end>
    Left = 104
    Top = 80
  end
end
