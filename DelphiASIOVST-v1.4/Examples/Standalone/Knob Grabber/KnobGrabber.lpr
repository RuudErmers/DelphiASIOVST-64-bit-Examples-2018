program KnobGrabber;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  KGmain in 'KGmain.pas' {FmKnobGrabber};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmKnobGrabber, FmKnobGrabber);
  Application.Run;
end.
