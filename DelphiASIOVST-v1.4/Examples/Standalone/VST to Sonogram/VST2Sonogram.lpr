program VST2Sonogram;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  VSmain in 'VSmain.pas' {FmSonogram};

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.
