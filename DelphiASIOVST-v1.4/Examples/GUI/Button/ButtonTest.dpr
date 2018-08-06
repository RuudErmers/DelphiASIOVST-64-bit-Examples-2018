program ButtonTest;

uses
  Forms,
  ButtonTestMain in 'ButtonTestMain.pas' {FmButton};

begin
  Application.Initialize;
  Application.CreateForm(TFmButton, FmButton);
  Application.Run;
end.
