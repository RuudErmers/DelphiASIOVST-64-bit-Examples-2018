{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library OscTestPrj;

uses
  Forms,
  DVSTEffect,
  DAV_VSTBasicModule,
  OscTestModuleU in 'OscTestModuleU.pas' {OscTestModule: TVSTModule},
  OscTestFormU in 'OscTestFormU.pas' {OscTestForm};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TOscTestModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.