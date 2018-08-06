library Audio2MidiTrigger;

uses
  Interfaces,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  Audio2MidiTriggerDM in 'Audio2MidiTriggerDM.pas' {Audio2MidiTriggerModule: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TAudio2MidiTriggerModule);
end;

exports
{$IFDEF DARWIN}  {OS X entry points}
  VSTPluginMain name '_main',
  VSTPluginMain name '_main_macho',
  VSTPluginMain name '_VSTPluginMain';
{$ELSE}
  VSTPluginMain name 'main',
  VSTPluginMain name 'main_plugin',
  VSTPluginMain name 'VSTPluginMain';
{$ENDIF}

begin
end.
