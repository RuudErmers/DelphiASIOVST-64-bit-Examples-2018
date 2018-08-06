{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SineSynth;

{$I DAV_Compiler.INC}

uses
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  SineSynthModule in 'SineSynthModule.pas' {VSTSSModule: TVSTModule},
  SineSynthGUI in 'SineSynthGUI.pas' {VSTGUI},
  SineSynthVoice in 'SineSynthVoice.pas',
  VoiceList in 'VoiceList.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TVSTSSModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';

begin
end.

