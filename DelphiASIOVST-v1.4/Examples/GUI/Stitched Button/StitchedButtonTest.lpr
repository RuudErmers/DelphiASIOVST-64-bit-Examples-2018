program StitchedButtonTest;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {FmStitchedButtonTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmStitchedButtonTest, FmStitchedButtonTest);
  Application.Run;
end.

