{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DAV_DSP_Lazarus; 

interface

uses
  DAV_DspBesselFilter, DAV_DspCepstrum, DAV_DspConvolution, 
  DAV_DspCorrelation, DAV_DspCrosstalkCancellation, DAV_DspCrosstalkSimulator, 
  DAV_DspDFT, DAV_DspDitherNoiseShaper, DAV_DspDownsampleScheduler, 
  DAV_DspDynamics, DAV_DspFDNReverb, DAV_DspFeedbackDelayNetwork, DAV_DspFFT, 
  DAV_DspFftReal2Complex, DAV_DspFilter, DAV_DspFilterAllpasses, 
  DAV_DspFilterBasics, DAV_DspFilterBasicsAutomatable, 
  DAV_DspFilterButterworth, DAV_DspFilterChebyshev, 
  DAV_DspFilterChebyshevType1, DAV_DspFilterChebyshevType2, 
  DAV_DspFilterLinearPhase, DAV_DspFilterLinearPhaseCrossover, 
  DAV_DspFilterLinkwitzRiley, DAV_DspFilterSimple, DAV_DspFilterSpectralDelay, 
  DAV_DspFreeverb, DAV_DspFreeverbFilter, DAV_DspFrequencyDivider, 
  DAV_DspInterpolation, DAV_DspLevelingAmplifier, DAV_DspLFO, 
  DAV_DspLightweightDynamics, DAV_DspMetronome, DAV_DspMinBlep, DAV_DspPhaser, 
  DAV_DspPlateReverb, DAV_DspPolyphaseDownsampler, DAV_DspPolyphaseFilter, 
  DAV_DspPolyphaseIirDesigner, DAV_DspPolyphaseUpsampler, 
  DAV_DspPsychoacousticBassEnhancer, DAV_DspRegister, DAV_DspRemez, 
  DAV_DspStateVariableFilter, DAV_DspUpDownsampling, DAV_DspWaveshaper, 
  DAV_DspWindowing, DAV_DspR128, DAV_DspSpectralEffects, 
  DAV_DspSpectralFilters, DAV_DspSpectralNoiseReduction, DAV_DspChorus, 
  DAV_DspDelayLines, DAV_DspDynamicLimiters, DAV_DspDynamicLookaheadLimiter, 
  DAV_DspExciter, DAV_DspFilterTransform, DAV_DspFrequencyShifter, 
  DAV_DspHumRemoval, DAV_DspLeslie, DAV_DspLorenzOscilator, DAV_DspModDelay, 
  DAV_DspNoiseShapingFilterDesigner, DAV_DspParametricEQ, 
  DAV_DspPinkNoiseGenerator, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DAV_DspRegister', @DAV_DspRegister.Register); 
end; 

initialization
  RegisterPackage('DAV_DSP_Lazarus', @Register); 
end.
