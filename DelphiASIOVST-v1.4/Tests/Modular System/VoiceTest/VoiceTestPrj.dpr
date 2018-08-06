{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library VoiceTestPrj;

uses
  Forms,
  DVSTEffect,
  DVSTBasicModule,
  VoiceTestModuleU in 'VoiceTestModuleU.pas' {VoiceTestModule: TVSTModule},
  VoiceTestFormU in 'VoiceTestFormU.pas' {VoiceTestForm},
  VoiceTestVoiceU in 'VoiceTestVoiceU.pas' {VoiceTestVoice: TDataModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TVoiceTestModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.