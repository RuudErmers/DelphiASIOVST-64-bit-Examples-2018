program VSTEditor;

{$I DAV_Compiler.inc}

uses
//  FastMM4,  // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
//  FastMove,
  {$ENDIF}
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

