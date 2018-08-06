program Analyser;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  TAChartLazarusPkg, 
  HostASIOLaz,
  AnalyserForm in 'AnalyserForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFmAnalyser, FmAnalyser);
  Application.Run;
end.
