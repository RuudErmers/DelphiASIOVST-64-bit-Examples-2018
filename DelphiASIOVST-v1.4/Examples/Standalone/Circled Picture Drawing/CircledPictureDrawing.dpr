program CircledPictureDrawing;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF}
  {$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
  madListModules,
  {$ENDIF}
  Forms,
  MainUnit in 'MainUnit.pas' {FmPrimitivePictureEvolution},
  SettingsUnit in 'SettingsUnit.pas' {FmSettings},
  ProgressBarUnit in 'ProgressBarUnit.pas' {FmProgressBar},
  AdditionalChunks in 'AdditionalChunks.pas',
  CostLogUnit in 'CostLogUnit.pas' {FmCostLog},
  SaveAnimationUnit in 'SaveAnimationUnit.pas' {FmSaveAnimation};

{$IFDEF RELEASE}
{$SetPEFlags 1}
{$ENDIF}

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF DELPHI10_UP}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.Title := 'Primitive Picture Evolution';
  Application.CreateForm(TFmPrimitivePictureEvolution, FmPrimitivePictureEvolution);
  Application.CreateForm(TFmCostLog, FmCostLog);
  Application.Run;
end.

