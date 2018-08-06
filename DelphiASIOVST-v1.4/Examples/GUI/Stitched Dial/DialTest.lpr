program DialTest;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  MainUnit in 'MainUnit.pas' {FmSwitchTest},
  DAV_GuiStitchedDial in '..\..\..\Source\GUI\DAV_GuiStitchedDial.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFmDialTest, FmDialTest);
  Application.Run;
end.

