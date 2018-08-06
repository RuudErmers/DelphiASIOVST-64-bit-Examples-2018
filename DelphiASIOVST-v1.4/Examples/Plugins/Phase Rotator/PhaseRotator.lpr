{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library PhaseRotator;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  PhaseRotatorDSP in 'PhaseRotatorDSP.pas' {PhaseRotatorModule: TVSTModule},
  PhaseRotatorGUI in 'PhaseRotatorGUI.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TPhaseRotatorModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

end.
