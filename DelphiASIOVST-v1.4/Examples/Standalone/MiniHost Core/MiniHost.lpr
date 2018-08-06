program MiniHost;

{$I DAV_Compiler.inc}

uses
  Interfaces, Forms, LCLIntf, Dialogs,
  MiniHostForm {FmMiniHost},
  OptionsForm in 'OptionsForm.pas' {Options},
  aboutform in 'AboutForm.pas' {about},
  PlayerForm in 'PlayerForm.pas' {Player};

begin
 Application.Initialize;
 Application.Run;
end.
