{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaEPiano;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  EPianoDM in 'EPianoDM.pas' {EPianoDataModule: TVSTModule},
  EPianoData in 'EPianoData.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TEPianoDataModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.