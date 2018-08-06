program AudioDataDisplayTest;

uses
  Forms,
  AudioDataDisplayTestMain in 'AudioDataDisplayTestMain.pas' {FmAudioDataDisplay};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmAudioDataDisplay, FmAudioDataDisplay);
  Application.Run;
end.
