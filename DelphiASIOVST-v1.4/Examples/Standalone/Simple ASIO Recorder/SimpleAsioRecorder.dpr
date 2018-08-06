program SimpleAsioRecorder;

uses
  Forms,
  SarMain in 'SarMain.pas' {FmRecordAudio};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmRecordAudio, FmRecordAudio);
  Application.Run;
end.
