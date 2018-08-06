object VoiceTestVoice: TVoiceTestVoice
  OldCreateOrder = False
  OnCreate = DspVoiceCreate
  DspDirectProcessItem = DspOscSine1
  VoiceProcessingMode = pmDspQueue
  TrailingType = vttManually
  TrailingSamples = 44100
  Left = 249
  Top = 116
  Height = 150
  Width = 215
  object DspOscSine1: TDspOscSine
    SampleRate = 44100.000000000000000000
    NextDspQueueItem = DspEnvelope1
    Amplitude = 1.000000000000000000
    Frequency = 440.000000000000000000
    Left = 32
    Top = 48
  end
  object DspEnvelope1: TDspEnvelope
    SampleRate = 44100.000000000000000000
    Decay = 0.200000002980232200
    Sustain = 0.699999988079071100
    Release = 1.000000000000000000
    MaxAmplitude = 1.000000000000000000
    Left = 128
    Top = 48
  end
end
