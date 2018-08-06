program StitchedButtonTest;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FmStitchedButtonTest};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmStitchedButtonTest, FmStitchedButtonTest);
  Application.Run;
end.

