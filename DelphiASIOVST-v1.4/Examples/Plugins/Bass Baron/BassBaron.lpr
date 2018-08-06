{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BassBaron;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  BassBaronDM in 'BassBaronDM.pas' {ResurrectionBassCloneModule: TVSTModule},
  BassBaronGUI in 'BassBaronGUI.pas' {FmBassBaron};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TResurrectionBassCloneModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

{$R *.res}

begin
end.
