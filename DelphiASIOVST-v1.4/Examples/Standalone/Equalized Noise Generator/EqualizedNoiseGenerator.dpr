program EqualizedNoiseGenerator;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove,
  Forms,
  EqualizedNoiseGeneratorForm in 'EqualizedNoiseGeneratorForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ASIO Equalized Noise Generator';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
