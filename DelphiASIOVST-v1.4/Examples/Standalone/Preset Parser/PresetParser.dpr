program PresetParser;

uses
  Forms,
  PPmain in 'PPmain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPresetParser, FmPresetParser);
  Application.Run;
end.
