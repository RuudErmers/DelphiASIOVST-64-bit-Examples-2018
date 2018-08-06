program Saw2Sine;

uses
  Forms,
  S2Smain in 'S2Smain.pas' {FmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmMain, FmMain);
  Application.Run;
end.
