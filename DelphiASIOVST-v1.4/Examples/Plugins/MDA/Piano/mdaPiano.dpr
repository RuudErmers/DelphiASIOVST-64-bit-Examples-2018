{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaPiano;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  PianoDM in 'PianoDM.pas' {PianoDataModule: TVSTModule},
  PianoData in 'PianoData.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TPianoDataModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.