program BindingTest;

uses
  FastMM4,
  Forms,
  MainUnit in 'MainUnit.pas' {FmBindingTest};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmBindingTest, FmBindingTest);
  Application.Run;
end.
