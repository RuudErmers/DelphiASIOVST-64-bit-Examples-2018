program PaintBoxTest;

{$MODE Delphi}

uses
  Forms, Interfaces,
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPaintBoxTest, FmPaintBoxTest);
  Application.Run;
end.

