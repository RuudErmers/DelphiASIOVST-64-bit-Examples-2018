program ZLibUnitTest;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  GUITestRunner,
  TestDAV_ZLib in 'TestDAV_ZLib.pas',
  DAV_ZLib in '..\..\Source\DAV_ZLib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.


