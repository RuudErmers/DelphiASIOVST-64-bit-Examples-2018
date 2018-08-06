program VST2Sonogram;

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
  {$ENDIF}
  Forms,
  VSmain in 'VSmain.pas' {FmSonogram};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSonogram, FmSonogram);
  Application.Run;
end.
