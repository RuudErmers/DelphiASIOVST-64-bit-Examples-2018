library dsp_GraphicEQ;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  {$ENDIF}
  FastCode,
  WADSPVST in 'WADSPVST.pas' {FmWinAmpVST},
  PluginDM in 'PluginDM.pas' {PluginDataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {EditorForm};

exports winampDSPGetHeader2;

end.

