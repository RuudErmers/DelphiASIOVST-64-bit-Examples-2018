program PanelTest;

uses
  Forms,
  PanelTestMain in 'PanelTestMain.pas' {FmPanelTest};

begin
  Application.Initialize;
  Application.CreateForm(TFmPanelTest, FmPanelTest);
  Application.Run;
end.
