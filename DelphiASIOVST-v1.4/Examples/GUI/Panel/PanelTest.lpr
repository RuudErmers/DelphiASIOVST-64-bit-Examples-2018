program PanelTest;

{$MODE Delphi}

uses
  Forms, Interfaces,
  PanelTestMain in 'PanelTestMain.pas' {FmPanelTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPanelTest, FmPanelTest);
  Application.Run;
end.
