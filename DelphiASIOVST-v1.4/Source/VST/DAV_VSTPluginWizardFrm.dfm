object VSTPluginWizardForm: TVSTPluginWizardForm
  Left = 311
  Top = 246
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderStyle = bsDialog
  Caption = 'VST Plugin Project Wizard'
  ClientHeight = 345
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LbEffectName: TLabel
    Left = 190
    Top = 264
    Width = 82
    Height = 13
    Caption = 'Plugin unit name:'
  end
  object BlSeparator: TBevel
    Left = 0
    Top = 42
    Width = 452
    Height = 2
    Align = alTop
    Shape = bsBottomLine
  end
  object PnHeader: TPanel
    Left = 0
    Top = 0
    Width = 452
    Height = 42
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    object ImageVST: TImage
      Left = 5
      Top = 5
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        055449636F6E0000010003001010000001000800680500003600000018180000
        01000800C80600009E0500002020000001000800A8080000660C000028000000
        1000000020000000010008000000000040010000000000000000000000000000
        00000000FFFFFF006C6C6C00CFCFCF00C5C5C500C4C4C400D0D0D000B9B9B900
        AAAAAA00B8B8B800C0C0C000C8C8C800C6C6C600CACACA00ADADAD0081818100
        FBFBFB0099999900C7C7C700E1E1E1009F9F9F00F1F1F100EFEFEF00EEEFEF00
        D8D8D8008585850072727200DCDCDC00C6C7C700B7B7B700D3D3D300ECECEC00
        F6F6F600E6E6E600A9A9A900E9EAEA00A9969600DEDFDF007E7E7E00D4D4D400
        CDCDCD00E8D4D400F7F8F800C1C1C100A6A6A60082828200ABABAB00E2E2E200
        33333300D2D2D200F9F9F900A8A8A800AEAEAE00E1E3E300EBEBEB00F3F3F300
        0D0D0D0086868600A3A3A300EDEDEC0011110E00CCCCC900F6F5F600C4C1C400
        4D4B4D004C4C4C00E8E8E800FFFFF200D4CFEF00E6EEE800D7F0DD00F9F4F900
        F4F4F40075757500EEEEEE00E3E3E300E9E9E400DCDCF3000000FF000000F800
        738FB0006EB536003A8A000029960000D9EFDE00EAE7EA00E5E5E500BABABA00
        FCFCFC00BEBEBE00C3C3BD00B5B5E0000000FD008188B60048A224001C940000
        238A00009EC9A400C5C0C500EFF2F2009872BC007676C700F5FBF1008CA29200
        C4C8CA0000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000062636465666700
        00000000575658595A4D5B5C5D5E5F606158581148494A4B4C4D4E4F50515253
        545541562500000000420043444500460F47002625000018393A3B3C3D3E3F2A
        4041003025003132333435003632373837000030250026270028292A2B2C2D2E
        2F000030180B191A001B1C1D1E1F1220212223240E00000F0000101100121314
        0015161701020303040305060708090A030B0C0D000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000002800000018000000300000000100080000000000A002000000000000
        000000000000000000000000FFFFFF00C7C7C700BDBDBD00C8C8C800C6C6C600
        CCCCCC00D3D3D300CBCBCB00CACACA00CDCDCD00C5C5C50063636300ECECEC00
        E8E8E800E7E7E700EFEFEF00EBEBEB00C3C3C300B9B9B900C1C1C100E0E0E000
        DCDCDC00EAEAEA00E9E9E900EDEDED00B7B7B70071717100DDDDDD00C2C2C200
        84848400F2F2F20092929200FEFEFE00FAFAFA00FDFDFD00F7FAFA00BBBBBB00
        73737300B5B5B500B1B1B100C4C4C40090909000F1F1F100CFCFCF00E2E2E200
        CFD1D100BA9F9F00DEE1E10081818100A8A8A800F3F3F300F8F8F800C5C9C900
        BEBEBE00D6D6D600EEEEEE00DFDFDF009D9D9D00FAFCFC00CFC3C300C5C6C600
        70707000DEDEDE00D2D2D200B8B8B800E6EAEA00FCD2D200F3F7F700A4A4A400
        8D8D8D0048484800FEFFFF00BABABA00F9F9F900E9EDED00FCFCFC00F5F5F500
        B0B0B0008B8B8B00757575004A4A4A008C8C8C00A3A3A3002B2B2B008A8A8A00
        A0A0A000A5A5A500B4B4B4001D1D1D00434343001C1C1C006A6A6A001F1F1F00
        D7D7D7002F2F2F00AAAAAA005959590099999900CECECB007C7C770095959000
        B1B1AC00CDCCCD00C9C4C900918C9100C8C4C800E4E4E4009C9C9C00D9D9D900
        EFEFF800EBEBFF00ECECFF00D7D3FF00FCFFFA00D6FFDD00DCFFE200FAFFFD00
        FBFBFB0065656500D5D5D500FAFAF900FEFEF9009797FF000000FF001515FF00
        2725E0004F5DBC00ADE88D0047860A00528C150026921400B4DF9400FCFCFF00
        FAF9FA00BCBCBC009B9B9B009F9F9F009E9E9E00A6A6A0009191B6000202FF00
        0000F2007B74CB0091D958002D8A00002B9900002E9900002D960000209C0700
        8FA59300A6A3A600A1A1A100E6E6E600E9E9E700EDEDEC009090FF002021FF00
        6163C30096BE82001F9400002B9A00002E93000077BC5B00E0EDE300EAE8EA00
        AEB2DD00592299003F26A0003737A200FFFFFE00668D6A0053775900728D7800
        FFF6FF0000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000A5A6A7A8A9AAABACAD00000000000000000C980E
        0E999A9B7B7B9C9D9E9FA0A1A2A3A40E0E0E0E1828878889898A8B8C7B8D8E8F
        90919293949596898989975476774B333378797A7B7C7D7E7F80818283848549
        493320863D3E0000002000006D6E6F707172737400004B00750000483D1B0000
        00313F0062636465666768696A126B246C0000483D1B0000185B25223F5C311E
        214B5D5E565F6061210000483D1B00001355563F15350000204C575859305A51
        000000483D1B0049050849054A4B4C2C4D4E4F5051525354000000483D3E003F
        403E00414243050A35444526221C4639004720481A1504303132003334350909
        36373836001539033A3B3C0825032627281400170829022A000021082B062C09
        2D2E2F021A1B000000000000001C1D011E17131F13200021222300240B010C0D
        0D0D0E0E0D0F10111213061415160D0D0D171819010203010101010101040506
        06060708090401010101010A0000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000002800000020000000400000000100080000000000
        8004000000000000000000000000000000000000000000003300000066000000
        99000000CC000000FF00000000330000333300006633000099330000CC330000
        FF33000000660000336600006666000099660000CC660000FF66000000990000
        339900006699000099990000CC990000FF99000000CC000033CC000066CC0000
        99CC0000CCCC0000FFCC000000FF000033FF000066FF000099FF0000CCFF0000
        FFFF000000003300330033006600330099003300CC003300FF00330000333300
        333333006633330099333300CC333300FF333300006633003366330066663300
        99663300CC663300FF66330000993300339933006699330099993300CC993300
        FF99330000CC330033CC330066CC330099CC3300CCCC3300FFCC330000FF3300
        33FF330066FF330099FF3300CCFF3300FFFF3300000066003300660066006600
        99006600CC006600FF00660000336600333366006633660099336600CC336600
        FF33660000666600336666006666660099666600CC666600FF66660000996600
        339966006699660099996600CC996600FF99660000CC660033CC660066CC6600
        99CC6600CCCC6600FFCC660000FF660033FF660066FF660099FF6600CCFF6600
        FFFF660000009900330099006600990099009900CC009900FF00990000339900
        333399006633990099339900CC339900FF339900006699003366990066669900
        99669900CC669900FF66990000999900339999006699990099999900CC999900
        FF99990000CC990033CC990066CC990099CC9900CCCC9900FFCC990000FF9900
        33FF990066FF990099FF9900CCFF9900FFFF99000000CC003300CC006600CC00
        9900CC00CC00CC00FF00CC000033CC003333CC006633CC009933CC00CC33CC00
        FF33CC000066CC003366CC006666CC009966CC00CC66CC00FF66CC000099CC00
        3399CC006699CC009999CC00CC99CC00FF99CC0000CCCC0033CCCC0066CCCC00
        99CCCC00CCCCCC00FFCCCC0000FFCC0033FFCC0066FFCC0099FFCC00CCFFCC00
        FFFFCC000000FF003300FF006600FF009900FF00CC00FF00FF00FF000033FF00
        3333FF006633FF009933FF00CC33FF00FF33FF000066FF003366FF006666FF00
        9966FF00CC66FF00FF66FF000099FF003399FF006699FF009999FF00CC99FF00
        FF99FF0000CCFF0033CCFF0066CCFF0099CCFF00CCCCFF00FFCCFF0000FFFF00
        33FFFF0066FFFF0099FFFF00CCFFFF00FFFFFF00000000000D0D0D001A1A1A00
        282828003535350043434300505050005D5D5D006B6B6B007878780086868600
        93939300A1A1A100AEAEAE00BBBBBB00C9C9C900D6D6D600E4E4E400F1F1F100
        FFFFFF0000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000D7D7D7D7D7D7D7D7D7D7D7D7
        D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
        D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
        D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
        D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
        D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
        D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
        D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D79E
        6D6D6C73D75C313131ACD7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7C2B4
        B4B4BB9EAC3713131338B2D7D7D7D7D7D7D7D7D7D7E6E3E3E3E3E38181BBB4B4
        B490AC38131313131313378181E3E3E3E3E3E3E4E3E3E7E7E7E7E7ACACC2B4B4
        B490AC6238131313131362ACACE7E7E7E7E7E7E2DFE5D7D7D7D7D7D7D7D7C2B4
        BBBB979EB23838383763D7D7D7D7D7D7D7D7D7E6E0E5D7D7D7D7D7D7D7D7D7D0
        D0D0D0D0D7B2B2B2B2D7D7D7D7D7D7D7D7D7D7E5E0E5D7D7D7D7D7E8EAD7D7AC
        ACD7ACACACACACACD7D7E8E9ACE9E9E9D7D7D7E5E0E5D7D7D7D7EAE0E3D7D7E5
        DCDDE0E6E8E8E656E0E6E8E2E1E1E6E9D7D7D7E5E0E5D7D7D7D7E6E056ACD7E6
        DDE3D7D7D7D7D7E8DDDDE8E356DFE9D7D7D7D7E5E0E5D7D7D7EA818181E5D7E7
        81D7D7D7D7D7D7E5DCDBE1E1DCDDE9D7D7D7D7E5E0E5D7D7D7E8ACACE9E6ACE9
        E9D7D7D7E9E6E2DFDE2BE3E2DCDCE8D7D7D7D7E5E0E5D7D7D7ACACACD7D7E7D7
        D7EAE8E68181E28156E2E9E3DEDEE9D7D7D7D7E5E0E5D7D7ACE6E6EAD7D7E8AD
        EAACACACACAC8181E6EAD7E55656E9D7D7E9D7E5E0E5D7E781E4E7D7D7D7EAAC
        ACACACACACACEAACACD7D7AC8181EAD7EA81D7E6E0E5E7E2E2E3E6E9D7EAE8E5
        81ACACE9D7D7D7E9ACACE9E7ACACACAC82E4D7E5E0E5E9E9E9EAE9EAD7D7E9E9
        E4E3E7D7D7D7D7EAACACACE9EAE9ACE9ACACD7E5E0E5D7D7D7D7D7D7D7D7D7D7
        E981E3E7E9E9E8E58181EAD7D7D7D7D7D7D7D7E5DFE5D7D7D7D7D7D7D7D7D7D7
        D7D7E9E7E7E7E8E9EAE8D7D7D7D7D7D7D7D7D7E6E3E1E4E4E4E4E4E4E4E4E4E4
        E4E4E5E5E5E5E5E4E4E5E4E4E4E4E4E4E4E4E4E3D7D7D7D7D7D7D7D7D7D7D7D7
        D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
        D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
        D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
        D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
        D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
        D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000}
    end
    object LbHeading: TLabel
      Left = 45
      Top = 5
      Width = 170
      Height = 13
      Caption = 'New VST Plugin Project Wizard'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LbSubHeading: TLabel
      Left = 45
      Top = 21
      Width = 235
      Height = 13
      Caption = 'Create a new project for developing a VST plugin'
    end
  end
  object PnControl: TPanel
    Left = 0
    Top = 306
    Width = 452
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      452
      39)
    object Bevel2: TBevel
      Left = 0
      Top = 0
      Width = 452
      Height = 2
      Align = alTop
      Shape = bsBottomLine
    end
    object btnFinish: TButton
      Left = 372
      Top = 9
      Width = 75
      Height = 25
      Hint = 'This is Finish, but not the End'
      Anchors = [akTop, akRight]
      Caption = '&Finish'
      Default = True
      ModalResult = 1
      TabOrder = 0
      Visible = False
    end
    object btnCancel: TButton
      Left = 5
      Top = 9
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object btnNext: TButton
      Left = 372
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Next'
      Default = True
      TabOrder = 1
      OnClick = btnNextClick
    end
    object btnPrev: TButton
      Left = 292
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Back'
      Enabled = False
      TabOrder = 3
      OnClick = btnPrevClick
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 44
    Width = 452
    Height = 262
    ActivePage = TSDestination
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 1
    object TSWelcome: TTabSheet
      Caption = 'Welcome'
      object LbWelcomeTitle: TLabel
        Left = 24
        Top = 13
        Width = 236
        Height = 13
        Caption = 'Welcome to the VST Plugin Project Wizard'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbWelcomeInstructions4: TLabel
        Left = 24
        Top = 136
        Width = 217
        Height = 13
        Caption = 'Click "Next" to start creating your VST plugin.'
      end
      object LbWelcomeInstructions3: TLabel
        Left = 24
        Top = 104
        Width = 387
        Height = 26
        Caption = 
          'Once compiled, your VST plugin can be loaded by a suitable VST H' +
          'ost application such as Steinberg Cubase.'
        WordWrap = True
      end
      object LbWelcomeInstructions2: TLabel
        Left = 24
        Top = 72
        Width = 386
        Height = 26
        Caption = 
          'A VST plugin is a .DLL project that contains a TVSTModule descen' +
          'dant class (the plugin code istelf) and, optionally, a GUI edito' +
          'r form.'
        WordWrap = True
      end
      object LbWelcomeInstructions1: TLabel
        Left = 24
        Top = 40
        Width = 362
        Height = 26
        Caption = 
          'This wizard will guide you through the process of creating a new' +
          ' VST plugin project.'
        WordWrap = True
      end
    end
    object TSDestination: TTabSheet
      Caption = 'Dest'
      ImageIndex = 4
      object LbDestinationTitle: TLabel
        Left = 24
        Top = 13
        Width = 254
        Height = 13
        Caption = 'Select a Destination Folder and Project Name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LBDesinationSelect: TLabel
        Left = 24
        Top = 40
        Width = 310
        Height = 13
        Caption = 
          'Select the folder where you would like the project to be created' +
          ':'
      end
      object LBProjectName: TLabel
        Left = 24
        Top = 93
        Width = 141
        Height = 13
        Caption = 'Enter a name for the project:'
      end
      object LbDpr: TLabel
        Left = 229
        Top = 115
        Width = 20
        Height = 13
        Caption = '.dpr'
      end
      object edtProjectPath: TEdit
        Left = 24
        Top = 59
        Width = 321
        Height = 21
        TabOrder = 0
      end
      object edtProjectName: TEdit
        Left = 24
        Top = 112
        Width = 201
        Height = 21
        TabOrder = 2
      end
      object btnBrowse: TButton
        Left = 351
        Top = 59
        Width = 65
        Height = 21
        Caption = 'Browse'
        TabOrder = 1
        OnClick = btnBrowseClick
      end
      object chkSaveWhenFinished: TCheckBox
        Left = 24
        Top = 152
        Width = 113
        Height = 17
        Caption = 'Save when finished'
        Checked = True
        State = cbChecked
        TabOrder = 3
        Visible = False
      end
    end
    object TSPluginType: TTabSheet
      Caption = 'Plugin Type'
      ImageIndex = 2
      object LbPluginType: TLabel
        Left = 24
        Top = 13
        Width = 65
        Height = 13
        Caption = 'Plugin Type'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbSelectVSTTypeInstruction: TLabel
        Left = 24
        Top = 40
        Width = 259
        Height = 13
        Caption = 'Select the type of VST plugin you would like to create.'
        WordWrap = True
      end
      object optPluginTypeSynth: TRadioButton
        Left = 24
        Top = 96
        Width = 216
        Height = 17
        Caption = 'Synth (a VST Instrument, or "VSTi")'
        TabOrder = 1
      end
      object optPluginTypeEffect: TRadioButton
        Left = 24
        Top = 73
        Width = 217
        Height = 17
        Caption = 'Audio effect (such as a delay or reverb)'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
    end
    object TSModule: TTabSheet
      Caption = 'Module'
      ImageIndex = 5
      object LbModuleTitle: TLabel
        Left = 24
        Top = 13
        Width = 167
        Height = 13
        Caption = 'Add a VSTModule Descendant'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbModuleInstructions: TLabel
        Left = 24
        Top = 40
        Width = 362
        Height = 26
        Caption = 
          'Each VST plugin needs to contain a VSTModule descendant class. T' +
          'his class provides the audio/MIDI processing code for your VST p' +
          'lugin.'
        WordWrap = True
      end
      object LbModuleName: TLabel
        Left = 24
        Top = 85
        Width = 284
        Height = 13
        Caption = 'Please enter a name for your VSTModule descendant class:'
      end
      object LbModuleUnit: TLabel
        Left = 24
        Top = 139
        Width = 257
        Height = 13
        Caption = 'Please enter a name for the unit containing this class:'
      end
      object LbPas: TLabel
        Left = 303
        Top = 161
        Width = 21
        Height = 13
        Caption = '.pas'
      end
      object edtPluginFormName: TEdit
        Left = 24
        Top = 104
        Width = 321
        Height = 21
        TabOrder = 0
      end
      object edtPluginUnitName: TEdit
        Left = 24
        Top = 158
        Width = 273
        Height = 21
        TabOrder = 1
      end
    end
    object TSEditor: TTabSheet
      Caption = 'Editor'
      ImageIndex = 3
      object LbGUIFormTitle: TLabel
        Left = 24
        Top = 13
        Width = 124
        Height = 13
        Caption = 'Add a GUI Editor Form'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbGUIFormInstructions: TLabel
        Left = 24
        Top = 40
        Width = 382
        Height = 26
        Caption = 
          'A VST plugin may optionally include a GUI editor form to enable ' +
          'the end-user to manipulate the parameters of the plugin.'
        WordWrap = True
      end
      object pnlEditorDetails: TPanel
        Left = 0
        Top = 106
        Width = 433
        Height = 105
        BevelOuter = bvNone
        TabOrder = 1
        object LbPasDfm: TLabel
          Left = 302
          Top = 81
          Width = 47
          Height = 13
          Caption = '.pas/.dfm'
        end
        object LbGUIFormUnit: TLabel
          Left = 24
          Top = 58
          Width = 255
          Height = 13
          Caption = 'Please enter a name for the unit containing the form:'
        end
        object lblEditorFormName: TLabel
          Left = 24
          Top = 5
          Width = 194
          Height = 13
          Caption = 'Please enter a name for the editor form:'
        end
        object edtEditorUnitName: TEdit
          Left = 24
          Top = 77
          Width = 273
          Height = 21
          TabOrder = 1
        end
        object edtEditorFormName: TEdit
          Left = 24
          Top = 24
          Width = 321
          Height = 21
          TabOrder = 0
        end
      end
      object chkUseEditor: TCheckBox
        Left = 24
        Top = 83
        Width = 209
        Height = 17
        Caption = 'Include a GUI editor form in the project'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chkUseEditorClick
      end
    end
    object TSNames: TTabSheet
      Caption = 'Names'
      ImageIndex = 6
      object LbNameTitle: TLabel
        Left = 24
        Top = 13
        Width = 108
        Height = 13
        Caption = 'Naming Your Plugin'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbNameInstructions: TLabel
        Left = 24
        Top = 40
        Width = 393
        Height = 26
        Caption = 
          'Choose a name for your VST Plugin. Most host applications will g' +
          'enerally make this name visible to the end-user.'
        WordWrap = True
      end
      object LbVSTPluginName: TLabel
        Left = 24
        Top = 79
        Width = 190
        Height = 13
        Caption = 'Please enter a name for the VST plugin:'
      end
      object LbProductName: TLabel
        Left = 24
        Top = 127
        Width = 315
        Height = 13
        Caption = 
          'Please enter a name for the overall product your plugin is part ' +
          'of:'
      end
      object LBCompanyName: TLabel
        Left = 24
        Top = 175
        Width = 164
        Height = 13
        Caption = 'Please enter your company name:'
      end
      object edtEffectName: TEdit
        Left = 24
        Top = 98
        Width = 321
        Height = 21
        TabOrder = 0
      end
      object edtProductName: TEdit
        Left = 24
        Top = 146
        Width = 321
        Height = 21
        TabOrder = 1
      end
      object edtVendorName: TEdit
        Left = 24
        Top = 194
        Width = 321
        Height = 21
        TabOrder = 2
      end
    end
    object TSVersionID: TTabSheet
      Caption = 'VersionAndID'
      ImageIndex = 7
      object LbVersionID: TLabel
        Left = 24
        Top = 13
        Width = 119
        Height = 13
        Caption = 'Plugin Version and ID'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbVersionIDInstructions: TLabel
        Left = 24
        Top = 40
        Width = 350
        Height = 26
        Caption = 
          'You need to provide a version number for your plugin, and also a' +
          ' unique 4-character ID.'
        WordWrap = True
      end
      object LbMajorVersion: TLabel
        Left = 24
        Top = 81
        Width = 69
        Height = 13
        Caption = 'Major version:'
      end
      object LbUniqueID: TLabel
        Left = 24
        Top = 137
        Width = 51
        Height = 13
        Caption = 'Unique ID:'
      end
      object LbMinorVersion: TLabel
        Left = 104
        Top = 81
        Width = 68
        Height = 13
        Caption = 'Minor version:'
      end
      object LbRelease: TLabel
        Left = 186
        Top = 81
        Width = 42
        Height = 13
        Caption = 'Release:'
      end
      object edtVersionMajor: TEdit
        Left = 24
        Top = 100
        Width = 65
        Height = 21
        MaxLength = 1
        TabOrder = 0
        OnKeyPress = edtVersionMajorKeyPress
      end
      object edtUniqueID: TEdit
        Left = 24
        Top = 156
        Width = 65
        Height = 21
        MaxLength = 4
        TabOrder = 3
      end
      object edtVersionMinor: TEdit
        Left = 104
        Top = 100
        Width = 65
        Height = 21
        MaxLength = 1
        TabOrder = 1
        OnKeyPress = edtVersionMajorKeyPress
      end
      object edtVersionRelease: TEdit
        Left = 186
        Top = 100
        Width = 65
        Height = 21
        MaxLength = 1
        TabOrder = 2
        OnKeyPress = edtVersionMajorKeyPress
      end
    end
    object TSFinish: TTabSheet
      Caption = 'Finish'
      ImageIndex = 2
      object LbDone: TLabel
        Left = 24
        Top = 13
        Width = 32
        Height = 13
        Caption = 'Done!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LbDoneInstruction: TLabel
        Left = 24
        Top = 40
        Width = 383
        Height = 26
        Caption = 
          'The Wizard is now ready to create your VST plugin project with t' +
          'he options you have selected.'
        WordWrap = True
      end
      object LbClickFinish: TLabel
        Left = 24
        Top = 95
        Width = 278
        Height = 13
        Caption = 'Click the "Finish" button to create your VST plugin project.'
      end
    end
  end
end
