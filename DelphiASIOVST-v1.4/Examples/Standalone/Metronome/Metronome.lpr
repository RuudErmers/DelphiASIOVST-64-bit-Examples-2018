program Metronome;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  MetronomeForm in 'MetronomeForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title:='ASIO Metronome';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
