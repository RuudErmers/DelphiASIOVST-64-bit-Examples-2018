program PixelMapTest;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  GUITestRunner,
  TestDAV_GuiPixelMap in 'TestDAV_GuiPixelMap.pas',
  DAV_GuiPixelMap in '..\..\Source\DAV_GuiPixelMap.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.


