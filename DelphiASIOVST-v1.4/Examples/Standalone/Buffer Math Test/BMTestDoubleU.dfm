object BufferMathForm: TBufferMathForm
  Left = 158
  Top = 277
  Caption = 'Buffer-Math Test'
  ClientHeight = 339
  ClientWidth = 973
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object TestCopyBtn: TButton
    Left = 784
    Top = 8
    Width = 177
    Height = 25
    Caption = 'Test ppsingle to array of dyn array'
    TabOrder = 0
    OnClick = TestCopyBtnClick
  end
  object TestAddBtn: TButton
    Left = 8
    Top = 40
    Width = 144
    Height = 25
    Caption = 'Test add buffers'
    TabOrder = 1
    OnClick = TestAddBtnClick
  end
  object ResultMemo: TMemo
    Left = 160
    Top = 40
    Width = 801
    Height = 281
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object TestSubBtn: TButton
    Left = 8
    Top = 72
    Width = 144
    Height = 25
    Caption = 'Test sub buffers'
    TabOrder = 3
    OnClick = TestSubBtnClick
  end
  object TestMulBtn: TButton
    Left = 8
    Top = 104
    Width = 144
    Height = 25
    Caption = 'Test mul buffers'
    TabOrder = 4
    OnClick = TestMulBtnClick
  end
  object TestClearBtn: TButton
    Left = 8
    Top = 136
    Width = 144
    Height = 25
    Caption = 'Test clear buffers'
    TabOrder = 5
    OnClick = TestClearBtnClick
  end
  object TestCopyBufBtn: TButton
    Left = 8
    Top = 168
    Width = 144
    Height = 25
    Caption = 'Test copy buffers'
    TabOrder = 6
    OnClick = TestCopyBufBtnClick
  end
  object TestMulAddBtn: TButton
    Left = 8
    Top = 200
    Width = 144
    Height = 25
    Caption = 'Test mul add buffers'
    TabOrder = 7
    OnClick = TestMulAddBtnClick
  end
  object TestAddMulBtn: TButton
    Left = 8
    Top = 232
    Width = 144
    Height = 25
    Caption = 'Test add mul buffers'
    TabOrder = 8
    OnClick = TestAddMulBtnClick
  end
  object TestAddScaledBtn: TButton
    Left = 8
    Top = 264
    Width = 144
    Height = 25
    Caption = 'Test add scaled buffers'
    TabOrder = 9
    OnClick = TestAddScaledBtnClick
  end
  object TestAddModulatedBtn: TButton
    Left = 8
    Top = 296
    Width = 144
    Height = 25
    Caption = 'Test add modulated buffers'
    TabOrder = 10
    OnClick = TestAddModulatedBtnClick
  end
  object TestBufferSumsBtn: TButton
    Left = 272
    Top = 8
    Width = 105
    Height = 25
    Caption = 'buffer sums'
    TabOrder = 11
    OnClick = TestBufferSumsBtnClick
  end
  object TestFindPeaksBtn: TButton
    Left = 160
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Find buffer peaks'
    TabOrder = 12
    OnClick = TestFindPeaksBtnClick
  end
end
