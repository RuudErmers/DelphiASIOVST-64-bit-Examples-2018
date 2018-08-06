program ComplexFunctionTest;
{

  Delphi DUnit-Testprojekt
  -------------------------
  Dieses Projekt enthält das DUnit-Test-Framework und die GUI/Konsolen-Test-Runner.
  Zum Verwenden des Konsolen-Test-Runners fügen Sie den konditinalen Definitionen  
  in den Projektoptionen "CONSOLE_TESTRUNNER" hinzu. Ansonsten wird standardmäßig 
  der GUI-Test-Runner verwendet.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestDAV_Complex in 'TestDAV_Complex.pas',
  DAV_Complex in '..\..\..\Source\DAV_Complex.pas';

{$R *.RES}

begin
 Application.Initialize;
 if IsConsole
  then TextTestRunner.RunRegisteredTests
  else GUITestRunner.RunRegisteredTests;
end.

