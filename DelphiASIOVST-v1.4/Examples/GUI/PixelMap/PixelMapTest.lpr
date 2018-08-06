program PixelMapTest;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  BtMain in 'BtMain.pas' {FmPixelMapTest},
  DAV_GuiPixelMap in '..\..\..\Source\GUI\DAV_GuiPixelMap.pas',
  DAV_GuiBlend in '..\..\..\Source\GUI\DAV_GuiBlend.pas',
  DAV_MemoryUtils in '..\..\..\Source\DAV_MemoryUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPixelMapTest, FmPixelMapTest);
  Application.Run;
end.
