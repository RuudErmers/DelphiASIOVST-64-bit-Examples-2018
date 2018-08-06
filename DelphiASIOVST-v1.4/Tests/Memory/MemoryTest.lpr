program MemoryTest;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  GUITestRunner,
  TestMemoryUtils in 'TestMemoryUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
