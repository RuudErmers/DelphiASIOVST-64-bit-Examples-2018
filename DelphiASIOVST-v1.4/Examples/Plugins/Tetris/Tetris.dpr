{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Tetris;

{$I DAV_Compiler.inc}

uses
  FastMM4,  // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF}
  Forms,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  TetrisDM in 'TetrisDM.pas' {TetrisModule: TVSTModule},
  TetrisEditor in 'TetrisEditor.pas' {FmTetris},
  TetrisUnit in 'TetrisUnit.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TTetrisModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.
