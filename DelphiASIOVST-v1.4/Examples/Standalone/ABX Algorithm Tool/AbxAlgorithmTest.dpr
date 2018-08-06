program AbxAlgorithmTest;

{$I DAV_Compiler.inc}

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
  AbxMain in 'AbxMain.pas' {FmAbxAlgorithmTest},
  AbxTest in 'AbxTest.pas' {FmAbxTest},
  AbxAudio in 'AbxAudio.pas' {FmAudioSettings},
  AbxTestSetup in 'AbxTestSetup.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ABX Algorithm Test';
  Application.CreateForm(TFmAbxAlgorithmTest, FmAbxAlgorithmTest);
  Application.CreateForm(TFmAudioSettings, FmAudioSettings);
  Application.Run;
end.
