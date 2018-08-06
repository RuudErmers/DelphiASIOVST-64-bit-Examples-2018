object FrFilterProperties: TFrFilterProperties
  Left = 0
  Top = 0
  Width = 183
  Height = 148
  TabOrder = 0
  object Filter: TGroupBox
    Left = 3
    Top = 3
    Width = 174
    Height = 110
    Caption = 'Filter'
    TabOrder = 0
    object LbFilterOrder: TLabel
      Left = 11
      Top = 19
      Width = 32
      Height = 13
      Caption = 'Order:'
    end
  end
  object CbFilterOrder: TComboBox
    Left = 52
    Top = 19
    Width = 53
    Height = 21
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 1
    Text = '8'
    Items.Strings = (
      '4'
      '6'
      '8'
      '10')
  end
end
