program PixelMapTest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestDAV_GuiPixelMap in 'TestDAV_GuiPixelMap.pas',
  DAV_GuiPixelMap in '..\..\Source\Gui\DAV_GuiPixelMap.pas';

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.


