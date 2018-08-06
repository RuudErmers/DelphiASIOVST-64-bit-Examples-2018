program SimpleSignalGenerator;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms,
  SsgMain in 'SsgMain.pas' {FmASIO}, HostASIOLaz;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
