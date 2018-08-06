program PaintBoxTest;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmPaintBoxTest, FmPaintBoxTest);
  Application.Run;
end.

