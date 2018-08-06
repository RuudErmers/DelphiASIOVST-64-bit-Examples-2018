program ZLibUnitTest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestDAV_ZLib in 'TestDAV_ZLib.pas',
  DAV_ZLib in '..\..\Source\DAV_ZLib.pas',
  DAV_Adler32 in '..\..\Source\DAV_Adler32.pas',
  DAV_CircularByteBuffer in '..\..\Source\DAV_CircularByteBuffer.pas',
  DAV_ZStream in '..\..\Source\DAV_ZStream.pas';

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.


