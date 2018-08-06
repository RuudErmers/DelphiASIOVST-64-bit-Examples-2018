program Blowfish;

uses
  Forms,
  BlowfishMain in 'BlowfishMain.pas' {FmBlowfish};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmBlowfish, FmBlowfish);
  Application.Run;
end.
