program FaderTest;

uses
  FastMM4,
  Forms,
  MainUnit in 'MainUnit.pas' {FmFaderDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmFaderDemo, FmFaderDemo);
  Application.Run;
end.

