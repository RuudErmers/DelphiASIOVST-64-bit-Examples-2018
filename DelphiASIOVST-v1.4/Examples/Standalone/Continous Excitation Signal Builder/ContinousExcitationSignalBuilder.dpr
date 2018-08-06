program ContinousExcitationSignalBuilder;

uses
  Forms,
  CESBmain in 'CESBmain.pas' {FmContinousExcitationSignalBuilder};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmContinousExcitationSignalBuilder, FmContinousExcitationSignalBuilder);
  Application.Run;
end.
