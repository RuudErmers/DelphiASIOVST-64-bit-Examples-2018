{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BaxxpanderStereo;

{$IFNDEF Wrapper}

uses
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  BaxxpanderModule in 'BaxxpanderModule.pas' {BaxxpanderModule: TVSTModule};

{$ELSE}

uses
  DAV_VSTEffect;

function BaxxpanderMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; external 'Baxxpander.dll' name 'VSTPluginMain';

{$ENDIF}

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 {$IFNDEF Wrapper}
 Result := VstModuleMain(AudioMasterCallback, TBaxxpanderModule);
 {$ELSE}
 Result := BaxxpanderMain(AudioMasterCallback);
 {$ENDIF}
 Result^.numInputs := 2;
 Result^.numOutputs := 2;
 Result^.UniqueID[0] := '2';
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

end.
