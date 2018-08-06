{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaCombo;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  ComboDM in 'ComboDM.pas' {ComboDataModule: TVSTModule},
  ComboGUI in 'ComboGUI.pas' {FmCombo};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TComboDataModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.