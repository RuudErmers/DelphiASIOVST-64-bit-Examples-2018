object DX10DataModule: TDX10DataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth]
  Version = '1.0'
  EffectName = 'mda DX10'
  ProductName = 'DAV mda'
  VendorName = 'Delphi ASIO & VST Project / mda'
  PlugCategory = vpcSynth
  CanDos = [vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend]
  SampleRate = 44100.000000000000000000
  numInputs = 0
  CurrentProgram = 0
  CurrentProgramName = 'Bright E.Piano'
  IORatio = 1.000000000000000000
  UniqueID = 'MDAx'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Bright E.Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'Jazz E.Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'E.Piano Pad'
      VSTModule = Owner
    end
    item
      DisplayName = 'Fuzzy E.Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'Soft Chime'
      VSTModule = Owner
    end
    item
      DisplayName = 'Harpsichord'
      VSTModule = Owner
    end
    item
      DisplayName = 'Funk Clav'
      VSTModule = Owner
    end
    item
      DisplayName = 'Sitar'
      VSTModule = Owner
    end
    item
      DisplayName = 'Chiff Organ'
      VSTModule = Owner
    end
    item
      DisplayName = 'Tinkle'
      VSTModule = Owner
    end
    item
      DisplayName = 'Space Pad'
      VSTModule = Owner
    end
    item
      DisplayName = 'Koto'
      VSTModule = Owner
    end
    item
      DisplayName = 'Harp'
      VSTModule = Owner
    end
    item
      DisplayName = 'Jazz Guitar'
      VSTModule = Owner
    end
    item
      DisplayName = 'Steel Drum'
      VSTModule = Owner
    end
    item
      DisplayName = 'Log Drum'
      VSTModule = Owner
    end
    item
      DisplayName = 'Trumpet'
      VSTModule = Owner
    end
    item
      DisplayName = 'Horn'
      VSTModule = Owner
    end
    item
      DisplayName = 'Reed 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Reed 2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Violin'
      VSTModule = Owner
    end
    item
      DisplayName = 'Chunky Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'E.Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Clunk Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Thick Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Sine Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Square Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Upright Bass 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Upright Bass 2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Harmonics'
      VSTModule = Owner
    end
    item
      DisplayName = 'Scratch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Syn Tom'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Attack'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Attack'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Decay'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Decay'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Release'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Coarse'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Coarse'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ratio'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Fine'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Fine'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ratio'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Init'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Mod Ini'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Dec'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Mod Dec'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Sus'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Mod Sus'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Rel'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Mod Rel'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Vel'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Mod Vel'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Vibrato'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Vibrato'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Octave'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Octave'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'FineTune'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'FineTun'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'cents'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Waveform'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Wavefrm'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Thru'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'ModThru'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'LFO Rate'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'LFORate'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnOutputProperties = VSTModuleOutputProperties
  OnProcess = VSTModuleProcess
  OnProcessMidi = VSTModuleProcessMidi
  OnProcess32Replacing = VSTModuleProcess
  OnResume = VSTModuleResume
  Left = 218
  Top = 81
  Height = 150
  Width = 194
end
