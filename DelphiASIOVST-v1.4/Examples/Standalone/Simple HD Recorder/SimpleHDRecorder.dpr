program SimpleHDRecorder;

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  SHRmain in 'SHRmain.pas' {FmSimpleHDRecorder},
  SHRSetup in 'SHRSetup.pas' {FmSetup};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Simple HD Recorder';
  Application.CreateForm(TFmSimpleHDRecorder, FmSimpleHDRecorder);
  Application.CreateForm(TFmSetup, FmSetup);
  Application.Run;
end.
