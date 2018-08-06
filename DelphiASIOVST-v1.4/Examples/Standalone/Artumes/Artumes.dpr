program Artumes;

uses
  Forms,
  ArtMain in 'ArtMain.pas' {FmArtumes},
  ArtProject in 'ArtProject.pas' {FmProject},
  ArtAbout in 'ArtAbout.pas' {FmAbout},
  ArtItemSource in 'ArtItemSource.pas',
  ArtItemAnalysis in 'ArtItemAnalysis.pas',
  ArtItemDestination in 'ArtItemDestination.pas',
  ArtPropertiesDestination in 'ArtPropertiesDestination.pas' {FmDestinationProperties},
  ArtPropertiesSource in 'ArtPropertiesSource.pas' {FmSourceProperties},
  ArtPropertiesAnalysis in 'ArtPropertiesAnalysis.pas' {FmAnalysisProperties},
  ArtFrameAnalysisThirdOctave in 'ArtFrameAnalysisThirdOctave.pas' {FrAnalysisThirdOctave: TFrame},
  ArtPropertiesAnalysisFilters in 'ArtPropertiesAnalysisFilters.pas' {FrFilterProperties: TFrame};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFmArtumes, FmArtumes);
  Application.CreateForm(TFmAbout, FmAbout);
  Application.CreateForm(TFmDestinationProperties, FmDestinationProperties);
  Application.CreateForm(TFmSourceProperties, FmSourceProperties);
  Application.CreateForm(TFmAnalysisProperties, FmAnalysisProperties);
  Application.Run;
end.
