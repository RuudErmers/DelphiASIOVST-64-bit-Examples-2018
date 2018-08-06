program GroupBoxTest;

uses
  madExcept,
  madLinkDisAsm,
  Forms,
  GroupBoxTestMain in 'GroupBoxTestMain.pas' {Form1},
  DAV_GuiGroup in '..\..\..\Source\GUI\DAV_GuiGroup.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmGroupBoxTest, FmGroupBoxTest);
  Application.Run;
end.
