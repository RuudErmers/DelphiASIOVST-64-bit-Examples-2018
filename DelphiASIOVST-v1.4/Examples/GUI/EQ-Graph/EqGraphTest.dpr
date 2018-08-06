program EqGraphTest;

uses
  Forms,
  EqGraphTestMain in 'EqGraphTestMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmEqGraphTest, FmEqGraphTest);
  Application.Run;
end.
