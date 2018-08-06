program BytemapTest;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  BtMain in 'BtMain.pas' {FmBytemapTest},
  DAV_GuiPixelMap in '..\..\..\Source\GUI\DAV_GuiPixelMap.pas',
  DAV_GuiByteMap in '..\..\..\Source\GUI\DAV_GuiByteMap.pas',
  DAV_GuiBlend in '..\..\..\Source\GUI\DAV_GuiBlend.pas',
  DAV_MemoryUtils in '..\..\..\Source\DAV_MemoryUtils.pas',
  DAV_GuiCustomMap in '..\..\..\Source\GUI\DAV_GuiCustomMap.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmBytemapTest, FmBytemapTest);
  Application.Run;
end.
