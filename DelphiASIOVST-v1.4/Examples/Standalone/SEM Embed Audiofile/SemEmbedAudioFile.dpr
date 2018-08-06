program SemEmbedAudioFile;

{$R 'DAV_AudioFileOscillator.res' 'DAV_AudioFileOscillator.rc'}

uses
  FastMM4,
  FastMove,
  Forms,
  ECImain in 'ECImain.pas' {FmSemEmbedAudioFile};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSemEmbedAudioFile, FmSemEmbedAudioFile);
  Application.Run;
end.
