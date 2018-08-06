program VSTEditor;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  EditorForm in 'EditorForm.pas' {FmVSTEditor},
  EditorSetup in 'EditorSetup.pas' {FmSetup}, HostVSTLaz, HostASIOLaz;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VST Plugin Editor';
  Application.CreateForm(TFmVSTEditor, FmVSTEditor);
  Application.CreateForm(TFmSetup, FmSetup);
  Application.Run;
end.

