{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ButterworthSplitterStereo;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  ButterworthSplitterDM in 'ButterworthSplitterDM.pas' {ButterworthSplitterModule: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TButterworthSplitterModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.
