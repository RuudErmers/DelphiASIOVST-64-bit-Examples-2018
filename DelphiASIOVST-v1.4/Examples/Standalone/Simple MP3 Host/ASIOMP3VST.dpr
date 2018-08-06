program ASIOMP3VST;

uses
  FastMM4,
  FastMove,
  Forms,
  ASIOMP3VSTGUI in 'ASIOMP3VSTGUI.pas' {FmASIOMP3VST},
  ASIOMP3VSTSetup in 'ASIOMP3VSTSetup.pas' {FmSetup};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Simple ASIO MP3 Player';
  Application.CreateForm(TFmASIOMP3VST, FmASIOMP3VST);
  Application.CreateForm(TFmSetup, FmSetup);
  Application.Run;
end.
