program BarberpoleTuner;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove,
  Forms,
  BTmain in 'BTmain.pas' {FmBarberpoleTuner};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmBarberpoleTuner, FmBarberpoleTuner);
  Application.Run;
end.
