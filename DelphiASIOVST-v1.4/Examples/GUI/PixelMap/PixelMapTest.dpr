program PixelMapTest;

uses
  Forms,
  BtMain in 'BtMain.pas' {FmPixelMapTest},
  DAV_GuiPixelMap in '..\..\..\Source\GUI\DAV_GuiPixelMap.pas',
  DAV_GuiBlend in '..\..\..\Source\GUI\DAV_GuiBlend.pas',
  DAV_MemoryUtils in '..\..\..\Source\DAV_MemoryUtils.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmPixelMapTest, FmPixelMapTest);
  Application.Run;
end.
