program MultiSineGenerator;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms,
  HostASIOLaz,
  MultiSineGeneratorMain in 'MultiSineGeneratorMain.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
