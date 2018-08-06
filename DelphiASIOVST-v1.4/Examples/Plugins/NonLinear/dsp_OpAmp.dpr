library dsp_OpAmp;

uses
  WADSPVST in 'WADSPVST.pas' {FmWinAmpVST},
  OpAmpModule in 'OpAmpModule.pas' {OpAmpDecimator: TVSTModule},
  OpAmpGUI in 'OpAmpGUI.pas' {VSTGUI};

exports winampDSPGetHeader2;

end.

