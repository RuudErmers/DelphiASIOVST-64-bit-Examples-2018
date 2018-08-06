{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library RenaissanceBassClone;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  RenaissanceBassCloneDM in 'RenaissanceBassCloneDM.pas' {ResurrectionBassCloneModule: TVSTModule},
  RenaissanceBassCloneGUI in 'RenaissanceBassCloneGUI.pas' {FmRenaissanceBassClone};

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
