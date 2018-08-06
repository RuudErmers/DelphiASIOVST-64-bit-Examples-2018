{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaOverdrive;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  OverdriveDM in 'OverdriveDM.pas' {OverdriveDataModule: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TOverdriveDataModule);
end;

exports 
  VstModuleMain name 'main',
  VstModuleMain name 'VSTPluginMain';

begin
end.