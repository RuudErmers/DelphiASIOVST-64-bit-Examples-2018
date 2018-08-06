program GraphXyTest;

uses
  Forms,
  GraphXYtestMain in 'GraphXYtestMain.pas' {FmGraphXY};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmGraphXY, FmGraphXY);
  Application.Run;
end.
