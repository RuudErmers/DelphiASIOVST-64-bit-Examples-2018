object DeessDataModule: TDeessDataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'mda De-ess'
  ProductName = 'DAV mda'
  VendorName = 'Delphi ASIO & VST Project / mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'De-esser'
  IORatio = 1.000000000000000000
  UniqueID = 'mdaS'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'De-esser'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Threshold'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -60.000000000000000000
      MinInteger = -60
      ShortLabel = 'Thres'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamEnvelopeChange
    end
    item
      Curve = ctFrequencyScale
      CurveFactor = 12.000000000000000000
      DisplayName = 'Frequency'
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      Max = 12000.000000000000000000
      MaxInteger = 12000
      Min = 1000.000000000000000000
      MinInteger = 1000
      ShortLabel = 'Freq'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 10.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamFilterChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'HF Drive'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 20.000000000000000000
      MaxInteger = 20
      Min = -20.000000000000000000
      MinInteger = -20
      ShortLabel = 'HFDrive'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamHFDriveChange
    end>
  OnOpen = VSTModuleOpen
  OnProcess = VSTModuleProcess
  OnProcess64Replacing = VSTModuleProcessDoubleReplacing
  OnProcess32Replacing = VSTModuleProcess
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
