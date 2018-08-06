object FrChannelStrip: TFrChannelStrip
  Left = 0
  Top = 0
  Width = 108
  Height = 292
  TabOrder = 0
  object TrackBar: TTrackBar
    Left = 2
    Top = 8
    Width = 40
    Height = 257
    Orientation = trVertical
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    TickMarks = tmBoth
    OnChange = TrackBarChange
  end
  object CBMute: TCheckBox
    Left = 15
    Top = 271
    Width = 13
    Height = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = CBMuteClick
  end
end
