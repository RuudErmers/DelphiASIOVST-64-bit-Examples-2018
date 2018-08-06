{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library NoiseReduction;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  {$IFDEF MSWINDOWS}
  DAV_WinAmp,
  {$ENDIF}
  NoiseReductionDM in 'NoiseReductionDM.pas' {NoiseReductionModule: TVSTModule},
  NoiseReductionGui in 'NoiseReductionGui.pas' {FmNoiseReduction};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TNoiseReductionModule);
end;

{$IFDEF MSWINDOWS}
function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TNoiseReductionModule);
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

{$R *.res}

begin
 Application.Initialize;
end.
