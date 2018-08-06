program AsioBufferdAudioFilePlayer;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  {$ENDIF}
  Forms,
  AsioBufferdAudioFilePlayerGUI in 'AsioBufferdAudioFilePlayerGUI.pas' {FmAsioBufferdAudioFilePlayer};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Buffered ASIO MP3 Player';
  Application.CreateForm(TFmAsioBufferdAudioFilePlayer, FmAsioBufferdAudioFilePlayer);
  Application.Run;
end.
