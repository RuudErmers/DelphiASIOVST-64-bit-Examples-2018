{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BarberpoleShifterStereo;

{$IFNDEF Wrapper}

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  {$ENDIF}
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  BarberpoleShifterDM in 'BarberpoleShifterDM.pas' {BarberpoleShifterDataModule: TVSTModule},
  BarberpoleShifterGUI in 'BarberpoleShifterGUI.pas' {FmBarberpoleShifter};

{$ELSE}

uses
  DAV_VSTEffect;

function BarberpoleShifterMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; external 'BarberpoleShifter.dll' name 'VSTPluginMain';

{$ENDIF}

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 {$IFNDEF Wrapper}
 Result := VstModuleMain(AudioMasterCallback, TBarberpoleShifterDataModule);
 {$ELSE}
 Result := BarberpoleShifterMain(AudioMasterCallback);
 {$ENDIF}
 Result^.numInputs := 2;
 Result^.numOutputs := 2;
 Result^.UniqueID[0] := '2';
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

end.
