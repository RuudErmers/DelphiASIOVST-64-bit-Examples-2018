program CommonFunctionTest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  FastMM4,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestDAV_AudioMemory in 'TestDAV_AudioMemory.pas',
  TestDAV_BlockConvert32 in 'TestDAV_BlockConvert32.pas',
  TestDAV_BlockConvert64 in 'TestDAV_BlockConvert64.pas',
  DAV_AudioMemory in '..\..\Source\DAV_AudioMemory.pas',
  DAV_BlockConvert in '..\..\Source\DAV_BlockConvert.pas',
  DAV_BlockConvert32 in '..\..\Source\DAV_BlockConvert32.pas',
  DAV_BlockConvert64 in '..\..\Source\DAV_BlockConvert64.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

