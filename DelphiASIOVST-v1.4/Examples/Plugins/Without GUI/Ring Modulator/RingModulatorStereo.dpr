{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library RingModulatorStereo;

{$IFNDEF Wrapper}

{$I DAV_Compiler.inc}

{$R 'RingModulator.res' 'RingModulator.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  {$ENDIF}
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  RingModulatorDSP in 'RingModulatorDSP.pas' {RingModulatorDataModule: TVSTModule};

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
 Result^.numInputs := 2;
 Result^.numOutputs := 2;
 Result^.UniqueID[0] := '2';
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

end.
