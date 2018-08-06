program VstPresetDesigner;

{$I DAV_Compiler.inc}

uses
  Forms, Interfaces,
  VpdMain in 'VpdMain.pas' {FmVstPresetDesigner};

{$R *.res}

begin
  Application.Title:='VST Plugin Preset Designer';
  Application.Initialize;
  Application.CreateForm(TFmVstPresetDesigner, FmVstPresetDesigner);
  Application.Run;
end.
