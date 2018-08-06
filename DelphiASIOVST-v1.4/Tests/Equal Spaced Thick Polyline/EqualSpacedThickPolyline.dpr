program EqualSpacedThickPolyline;

uses
  FastMM4,
  Forms,
  MainUnit in 'MainUnit.pas' {FmESTP},
  Magnifier in 'Magnifier.pas' {FmMagnifier},
  DAV_GuiVectorPixelGraph in '..\..\Source\GUI\DAV_GuiVectorPixelGraph.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmESTP, FmESTP);
  Application.CreateForm(TFmMagnifier, FmMagnifier);
  Application.Run;
end.

