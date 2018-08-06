program Metronome;

uses
  Forms,
  MetronomeForm in 'MetronomeForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Simple ASIO Metronome';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
