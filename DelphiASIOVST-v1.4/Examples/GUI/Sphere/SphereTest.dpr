program SphereTest;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FmSphereTest};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmSphereTest, FmSphereTest);
  Application.Run;
end.

