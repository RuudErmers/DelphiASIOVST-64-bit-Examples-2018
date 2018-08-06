program AbxStandalone;

{$I DAV_Compiler.inc}

uses
  FastMM4,  // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madListHardware,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  {$ENDIF}
  Forms,
  AbxStandaloneTest in 'AbxStandaloneTest.pas' {FmABXStandaloneTest},
  AbxStandaloneAudioSetup in 'AbxStandaloneAudioSetup.pas' {FmAudioSettings};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ABX Test (Standalone)';
  Application.CreateForm(TFmABXStandaloneTest, FmABXStandaloneTest);
  Application.CreateForm(TFmAudioSettings, FmAudioSettings);
  Application.Run;
end.
