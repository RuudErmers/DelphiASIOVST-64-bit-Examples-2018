program VSTEditor;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  EditorForm in 'EditorForm.pas' {FmVSTEditor},
  EditorSetup in 'EditorSetup.pas' {FmSetup};

begin
  Application.Initialize;
  Application.CreateForm(TFmVSTEditor, FmVSTEditor);
  Application.CreateForm(TFmSetup, FmSetup);
  Application.Run;
end.

