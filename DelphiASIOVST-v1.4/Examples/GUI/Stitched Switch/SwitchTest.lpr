program SwitchTest;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {FmSwitchTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSwitchTest, FmSwitchTest);
  Application.Run;
end.

