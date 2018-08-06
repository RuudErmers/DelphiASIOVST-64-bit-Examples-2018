program LunchBox;

{$MODE Delphi}

uses
  Interfaces,
  Forms, Dialogs, SysUtils,
  LunchBoxMain in 'LunchBoxMain.pas' {FmLunchBox},
  LunchBoxSetup in 'LunchBoxSetup.pas' {FmSetup},
  LunchBoxAbout in 'LunchBoxAbout.pas' {FmAbout},
  LunchBoxVST in 'LunchBoxVST.pas' {FmVST},
  LunchBoxEventList in 'LunchBoxEventList.pas',
  LunchBoxEvent in 'LunchBoxEvent.pas',
  LunchBoxInputFilter in 'LunchBoxInputFilter.pas', HostVSTLaz, HostASIOLaz;

{$R *.res}

begin
 if not DirectoryExists(ExtractFilePath(Application.ExeName)+'sounds\') then
  begin
   ShowMessage('Download the original Lunchbox Battle and copy the \sound directory to this folder');
   Exit;
  end;
 Application.Initialize;
 Application.Title := 'VST Plugin Editor';
 Application.CreateForm(TFmLunchBox, FmLunchBox);
 Application.CreateForm(TFmSetup, FmSetup);
 Application.CreateForm(TFmAbout, FmAbout);
 Application.CreateForm(TFmVST, FmVST);
 Application.Run;
end.

