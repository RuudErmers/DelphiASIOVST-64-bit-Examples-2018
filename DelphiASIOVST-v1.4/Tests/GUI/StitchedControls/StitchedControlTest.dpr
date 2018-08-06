program StitchedControlTest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestDAV_GuiStitchedControls in 'TestDAV_GuiStitchedControls.pas',
  DAV_GuiStitchedControls in '..\..\..\Source\GUI\DAV_GuiStitchedControls.pas',
  DAV_GuiStitchedDial in '..\..\..\Source\GUI\DAV_GuiStitchedDial.pas',
  DAV_GuiStitchedSwitch in '..\..\..\Source\GUI\DAV_GuiStitchedSwitch.pas',
  DAV_GuiStitchedImageList in '..\..\..\Source\GUI\DAV_GuiStitchedImageList.pas',
  DAV_GuiStitchedPngList in '..\..\..\Source\GUI\DAV_GuiStitchedPngList.pas';

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.


