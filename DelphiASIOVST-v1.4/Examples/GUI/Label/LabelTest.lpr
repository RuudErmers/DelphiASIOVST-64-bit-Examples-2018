program LabelTest;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  DAV_GUI_Lazarus,
  LabelTestMain in 'LabelTestMain.pas' {FmLabelTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmLabelTest, FmLabelTest);
  Application.Run;
end.
