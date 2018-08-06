program Generator;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  GenMain in 'GenMain.pas' {FmGenerator};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmGenerator, FmGenerator);
  Application.Run;
end.
