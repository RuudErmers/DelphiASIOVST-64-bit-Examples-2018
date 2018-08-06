{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library TwoBandDistortion;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  DAV_Common,
  DAV_VSTBasicModule,
  DAV_VSTEffect,
  ImagesForLazarus,
  {$IFDEF MSWINDOWS}
  DAV_WinAmp,
  {$ENDIF}
  TwoBandDistortionDM in 'TwoBandDistortionDM.pas' {TwoBandDistortionDataModule: TVSTModule},
  TwoBandDistortionGUI in 'TwoBandDistortionGUI.pas' {FmTwoBandDistortion};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TTwoBandDistortionDataModule);
end;

{$IFDEF MSWINDOWS}
function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TTwoBandDistortionDataModule);
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
