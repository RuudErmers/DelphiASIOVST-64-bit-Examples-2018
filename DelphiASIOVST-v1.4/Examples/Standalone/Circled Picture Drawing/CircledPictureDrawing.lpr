program CircledPictureDrawing;

{$I DAV_Compiler.inc}

uses
  Interfaces, Forms,
  MainUnit in 'MainUnit.pas' {FmCircledPictureDialog},
  SettingsUnit in 'SettingsUnit.pas' {FmSettings},
  ProgressBarUnit in 'ProgressBarUnit.pas' {FmProgressBar},
  AdditionalChunks in 'AdditionalChunks.pas';

begin
  Application.Initialize;
  Application.Title := 'Circled Picture Optimizer';
  Application.CreateForm(TFmCircledPictureDialog, FmCircledPictureDialog);
  Application.Run;
end.

