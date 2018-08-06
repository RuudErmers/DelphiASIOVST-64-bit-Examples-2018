program MegaDemo;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FmMegaDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmMegaDemo, FmMegaDemo);
  Application.Run;
end.

