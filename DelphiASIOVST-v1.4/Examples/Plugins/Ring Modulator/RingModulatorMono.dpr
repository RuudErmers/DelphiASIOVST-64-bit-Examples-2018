{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library RingModulatorStereo;

{$IFNDEF Wrapper}
{$R 'RingModulator.res' 'RingModulator.rc'}

uses
  Forms,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  RingModulatorDM in 'RingModulatorDM.pas' {RingModulatorDataModule: TVSTModule},
  RingModulatorGUI in 'RingModulatorGUI.pas' {FmRingModulator};

{$ELSE}

uses
  DAV_VSTEffect;

function RingModulatorMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; external 'RingModulator.dll' name 'VSTPluginMain';

{$ENDIF}

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 {$IFNDEF Wrapper}
 Result := VstModuleMain(AudioMasterCallback, TRingModulatorDataModule);
 {$ELSE}
 Result := RingModulatorMain(AudioMasterCallback);
 {$ENDIF}
 Result^.numInputs := 1;
 Result^.numOutputs := 1;
 Result^.UniqueID[0] := '1';
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

end.
