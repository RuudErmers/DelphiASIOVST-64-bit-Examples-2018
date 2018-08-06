{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BodeFrequencyShifterStereo;

{$IFNDEF Wrapper}

uses
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  BodeFrequencyShifterDM in 'BodeFrequencyShifterDM.pas' {BodeFrequencyShifterDataModule: TVSTModule},
  BodeFrequencyShifterGUI in 'BodeFrequencyShifterGUI.pas' {FmBodeFrequencyShifter};

{$ELSE}

uses
  DAV_VSTEffect;

function BodeFrequencyShifterMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; external 'BodeFrequencyShifter.dll' name 'VSTPluginMain';

{$ENDIF}

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 {$IFNDEF Wrapper}
 Result := VstModuleMain(AudioMasterCallback, TBodeFrequencyShifterDataModule);
 {$ELSE}
 Result := BodeFrequencyShifterMain(AudioMasterCallback);
 {$ENDIF}
 Result^.numInputs := 2;
 Result^.numOutputs := 2;
 Result^.UniqueID[0] := '2';
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

end.
