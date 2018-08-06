{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Adhesive;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  {$IFDEF MSWINDOWS}
  DAV_WinAmp,
  {$ENDIF}
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  AdhesiveDM in 'AdhesiveDM.pas' {AdhesiveDataModule: TVSTModule},
  AdhesiveGUI in 'AdhesiveGUI.pas' {FmAdhesive};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TAdhesiveDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TAdhesiveDataModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
 Application.Initialize;
end.
