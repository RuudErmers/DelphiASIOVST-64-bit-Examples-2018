program BlendPerformance;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FmPerformanceTest};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmPerformanceTest, FmPerformanceTest);
  Application.Run;
end.

