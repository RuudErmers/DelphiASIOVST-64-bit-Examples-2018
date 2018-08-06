{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LinearPhaseCrossover;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  {$IFDEF MSWINDOWS}
  DAV_WinAmp,
  {$ENDIF}
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  LinearPhaseCrossoverDM in 'LinearPhaseCrossoverDM.pas' {LinearPhaseCrossoverModule: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TLinearPhaseCrossoverModule);
end;

{$IFDEF MSWINDOWS}
function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TLinearPhaseCrossoverModule);
end;
{$ENDIF}

exports
{$IFDEF DARWIN}  {OS X entry points}
  VSTPluginMain name '_main',
  VSTPluginMain name '_main_macho',
  VSTPluginMain name '_VSTPluginMain';
{$ELSE}
  VSTPluginMain name 'main',
  VSTPluginMain name 'main_plugin',
  VSTPluginMain name 'VSTPluginMain',
{$IFDEF MSWINDOWS}
  WinampDSPGetHeader name 'winampDSPGetHeader2';
{$ENDIF}
{$ENDIF}

begin
 Application.Initialize;
end.