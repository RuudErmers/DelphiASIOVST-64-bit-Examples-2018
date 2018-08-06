library dsp_vst;

{.$R 'EmbeddedVSTPlugin.res' 'EmbeddedVSTPlugin.rc'}

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF}
  WinAmpDspVst in 'WinAmpDspVst.pas',
  WinAmpDspVstGui in 'WinAmpDspVstGui.pas' {FmWinAmpVST};

exports
  winampDSPGetHeader2 name 'winampDSPGetHeader2',
  winampDSPGetHeader2;

end.
