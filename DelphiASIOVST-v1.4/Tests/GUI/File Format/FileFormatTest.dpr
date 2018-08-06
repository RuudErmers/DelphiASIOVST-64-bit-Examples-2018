program FileFormatTest;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FmFileFormatTest};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmFileFormatTest, FmFileFormatTest);
  Application.Run;
end.

