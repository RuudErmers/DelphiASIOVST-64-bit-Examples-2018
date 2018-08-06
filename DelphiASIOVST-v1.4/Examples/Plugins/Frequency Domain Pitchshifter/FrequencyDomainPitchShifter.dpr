{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library FrequencyDomainPitchShifter;

{$I DAV_Compiler.inc}

{$R 'FrequencyDomainKnob.res' 'FrequencyDomainKnob.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules, 
  {$ENDIF}
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  FrequencyDomainPitchShifterDM in 'FrequencyDomainPitchShifterDM.pas' {FrequencyDomainPitchShifterModule: TVSTModule},
  FrequencyDomainPitchShifterGUI in 'FrequencyDomainPitchShifterGUI.pas' {FmFrequencyDomainPitchShifter},
  DAV_DspFrequencyDomainPitchshifter in '..\..\..\Source\DSP\DAV_DspFrequencyDomainPitchshifter.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TFrequencyDomainPitchShifterModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TFrequencyDomainPitchShifterModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
