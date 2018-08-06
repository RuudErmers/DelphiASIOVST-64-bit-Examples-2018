program ASIOMP3;

uses
  FastMM4,
  FastMove,
  Forms,
  ASIOMP3GUI in 'ASIOMP3GUI.pas' {FmASIOMP3};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Buffered ASIO MP3 Player';
  Application.CreateForm(TFmASIOMP3, FmASIOMP3);
  Application.Run;
end.
