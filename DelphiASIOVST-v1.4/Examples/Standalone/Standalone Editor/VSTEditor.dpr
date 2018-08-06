program VSTEditor;

{$I DAV_Compiler.inc}

{$R 'EmbeddedVST.res' 'EmbeddedVST.rc'}

uses
  FastMM4,
  Forms,
  EditorForm in 'EditorForm.pas' {FmVSTEditor},
  EditorSetup in 'EditorSetup.pas' {FmSetup};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VST Plugin Editor';
  Application.CreateForm(TFmVSTEditor, FmVSTEditor);
  Application.CreateForm(TFmSetup, FmSetup);
  Application.Run;
end.

