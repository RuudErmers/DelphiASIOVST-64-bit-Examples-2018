program KnobGrabber;

uses
  Forms,
  KGmain in 'KGmain.pas' {FmKnobGrabber};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmKnobGrabber, FmKnobGrabber);
  Application.Run;
end.
