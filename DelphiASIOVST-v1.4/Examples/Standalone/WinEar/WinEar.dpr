program WinEar;

uses
  Forms,
  WeMain in 'WeMain.pas' {FmWinEar},
  WeRetry in 'WeRetry.pas' {FmRetry},
  WeEndOfTest in 'WeEndOfTest.pas' {FmEndOfTest},
  WeHelp in 'WeHelp.pas' {FmAbout};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'WinEar Trainer';
  Application.CreateForm(TFmWinEar, FmWinEar);
  Application.CreateForm(TFmRetry, FmRetry);
  Application.CreateForm(TFmEndOfTest, FmEndOfTest);
  Application.Run;
end.
