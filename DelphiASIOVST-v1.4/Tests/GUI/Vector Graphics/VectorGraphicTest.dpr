program VectorGraphicTest;

uses
  FastMM4,
  Forms,
  MainUnit in 'MainUnit.pas' {FmVectorGraphicTest},
  DAV_GuiVectorPixel in '..\..\..\Source\GUI\DAV_GuiVectorPixel.pas',
  DAV_GuiFixedPoint in '..\..\..\Source\GUI\DAV_GuiFixedPoint.pas',
  DAV_GuiVectorPixelLine in '..\..\..\Source\GUI\DAV_GuiVectorPixelLine.pas',
  DAV_GuiVectorPixelCircle in '..\..\..\Source\GUI\DAV_GuiVectorPixelCircle.pas',
  DAV_GuiVectorPixelRectangle in '..\..\..\Source\GUI\DAV_GuiVectorPixelRectangle.pas';

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF DELPHI14_UP}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.CreateForm(TFmVectorGraphicTest, FmVectorGraphicTest);
  Application.Run;
end.

