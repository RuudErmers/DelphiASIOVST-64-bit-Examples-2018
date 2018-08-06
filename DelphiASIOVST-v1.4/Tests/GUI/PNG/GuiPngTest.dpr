program GuiPngTest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  FastMM4,
  FastMove,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  DAV_GuiPng in '..\..\..\Source\GUI\DAV_GuiPng.pas',
  DAV_GuiPngChunks in '..\..\..\Source\GUI\DAV_GuiPngChunks.pas',
  DAV_GuiPngClasses in '..\..\..\Source\GUI\DAV_GuiPngClasses.pas',
  DAV_GuiPngCoder in '..\..\..\Source\GUI\DAV_GuiPngCoder.pas',
  DAV_GuiPngTypes in '..\..\..\Source\GUI\DAV_GuiPngTypes.pas',
  DAV_GuiPngResourceStrings in '..\..\..\Source\GUI\DAV_GuiPngResourceStrings.pas',
  DAV_TestGuiPng in 'DAV_TestGuiPng.pas',
  DAV_TestGuiPngDisplay in 'DAV_TestGuiPngDisplay.pas' {FmDisplay};

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole
   then TextTestRunner.RunRegisteredTests
   else GUITestRunner.RunRegisteredTests;
end.

