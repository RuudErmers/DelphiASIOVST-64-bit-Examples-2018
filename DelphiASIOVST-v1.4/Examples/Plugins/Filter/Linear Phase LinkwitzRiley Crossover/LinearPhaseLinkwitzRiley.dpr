{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LinearPhaseLinkwitzRiley;

{$I DAV_Compiler.inc}

{$IFDEF CPU64}
{$IFDEF Use_IPPS}
  Error: 'IPPS may not be used in 64-Bit'
{$ENDIF}
{$IFDEF Use_CUDA}
  Error: 'CUDA may not be used in 64-Bit'
{$ENDIF}
{$ENDIF}

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
  LinearPhaseLinkwitzRileyDM in 'LinearPhaseLinkwitzRileyDM.pas' {LinearPhaseLinkwitzRileyDataModule: TVSTModule},
  LinearPhaseLinkwitzRileyGUI in 'LinearPhaseLinkwitzRileyGUI.pas' {FmLinearPhaseLinkwitzRiley};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TLinearPhaseLinkwitzRileyDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TLinearPhaseLinkwitzRileyDataModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

end.
