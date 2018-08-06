program VST2SEM;

{$R 'SEVST2SEM.res' 'SEVST2SEM.rc'}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  madListHardware,
  {$ENDIF}
  Forms,
  V2Smain in 'V2Smain.pas' {FmVST2SEM};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmVST2SEM, FmVST2SEM);
  Application.Run;
end.
