program ASIODemo;

{$I DAV_Compiler.inc}

uses
{$IFDEF CPU32}
(*
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
*)
{$ENDIF}
  Forms,
  AsioDemoForm in 'ASIODemoForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for ASIO-Host';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
