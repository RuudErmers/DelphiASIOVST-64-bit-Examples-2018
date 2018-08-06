program EqSlideTest;

uses
  Forms,
  EqSlideTestMain in 'EqSlideTestMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmEqSlideTest, FmEqSlideTest);
  Application.Run;
end.
