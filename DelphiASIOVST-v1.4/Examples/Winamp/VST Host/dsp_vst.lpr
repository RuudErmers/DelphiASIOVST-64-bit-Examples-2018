library dsp_vst;

{$MODE Delphi}

uses
  Interfaces,
  WinAmpDspVst in 'WinAmpDspVst.pas',
  WinAmpDspVstGui in 'WinAmpDspVstGui.pas' {FmWinAmpVST};

exports winampDSPGetHeader2 name 'winampDSPGetHeader2';
exports winampDSPGetHeader2;

end.
