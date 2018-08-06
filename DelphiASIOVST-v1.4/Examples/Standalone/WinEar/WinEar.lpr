program WinEar;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms, DAV_Common_Lazarus,
  WeMain in 'WeMain.pas' {FmWinEar},
  WeRetry in 'WeRetry.pas' {FmRetry},
  WeEndOfTest in 'WeEndOfTest.pas' {FmEndOfTest},
  WeHelp in 'WeHelp.pas' {FmAbout};

begin
  Application.Initialize;
  Application.Run;
end.
