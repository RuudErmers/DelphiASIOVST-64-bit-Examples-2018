program FilterExplorer;

uses
  Forms,
  FEXMain in 'FEXMain.pas' {FmFilterExplorer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmFilterExplorer, FmFilterExplorer);
  Application.Run;
end.
