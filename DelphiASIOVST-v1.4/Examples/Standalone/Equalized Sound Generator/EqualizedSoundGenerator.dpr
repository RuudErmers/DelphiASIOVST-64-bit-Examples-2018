program EqualizedSoundGenerator;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove,
  Forms,
  EqualizedSoundGeneratorForm in 'EqualizedSoundGeneratorForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ASIO Equalized Sound Generator';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
