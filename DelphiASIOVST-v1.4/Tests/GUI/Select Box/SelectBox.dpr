program SelectBox;

uses
  Forms,
  SBmain in 'SBmain.pas' {FmSelectBoxTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSelectBoxTest, FmSelectBoxTest);
  Application.Run;
end.
