program LedTest;

uses
  Forms,
  LedTestMain in 'LedTestMain.pas' {FmLEDTest};

begin
  Application.Initialize;
  Application.CreateForm(TFmLEDTest, FmLEDTest);
  Application.Run;
end.
