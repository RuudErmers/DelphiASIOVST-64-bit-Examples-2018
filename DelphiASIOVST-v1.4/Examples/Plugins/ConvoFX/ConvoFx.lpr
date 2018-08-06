{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ConvoFx;

{$I DAV_Compiler.inc}

// if the file below is missing please execute the batch file in this
// directory first to compile the resource file

{$R 'Resources\IRs.res' 'Resources\IRs.rc'}

uses
  Interfaces,
  Forms,
  {$IFDEF MSWINDOWS}
  DAV_WinAmp,
  {$ENDIF}
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  ConvoFxDM in 'ConvoFxDM.pas' {ConvoFxDataModule: TVSTModule},
  ConvoFxGUI in 'ConvoFxGUI.pas' {FmConvoFx};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TConvoFxDataModule);
end;

{$IFDEF MSWINDOWS}
function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TConvoFxDataModule);
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
