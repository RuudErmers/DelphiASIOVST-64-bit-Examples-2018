program AudioEditor;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove,
  Forms,
  AEmain in 'AEmain.pas' {FmAudioEditor},
  DAV_WaveFileTypes in '..\..\..\Source\DAV_WaveFileTypes.pas',
  DAV_ChunkClasses in '..\..\..\Source\DAV_ChunkClasses.pas',
  DAV_AudioFileWAV in '..\..\..\Source\DAV_AudioFileWAV.pas',
  DAV_AudioFileAIFF in '..\..\..\Source\DAV_AudioFileAIFF.pas',
  DAV_AudioFileAU in '..\..\..\Source\DAV_AudioFileAU.pas',
  DAV_AudioData in '..\..\..\Source\DAV_AudioData.pas',
  AEAsioSetup in 'AEAsioSetup.pas' {FmSetup},
  AEVstSetup in 'AEVstSetup.pas' {FmVSTSetup};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmAudioEditor, FmAudioEditor);
  Application.CreateForm(TFmSetup, FmSetup);
  Application.CreateForm(TFmVSTSetup, FmVSTSetup);
  Application.Run;
end.
