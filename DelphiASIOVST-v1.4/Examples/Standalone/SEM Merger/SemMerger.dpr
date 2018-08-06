program SemMerger;

{$R 'SEMerger.res' 'SEMerger.rc'}

uses
  Forms,
  SEmain in 'SEmain.pas' {FmSEModuleExplorer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSEModuleExplorer, FmSEModuleExplorer);
  Application.Run;
end.
