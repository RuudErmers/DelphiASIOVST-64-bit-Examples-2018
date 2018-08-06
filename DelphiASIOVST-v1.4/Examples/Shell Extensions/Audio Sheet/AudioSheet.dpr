library AudioSheet;

{$R 'ASheet.res' 'ASheet.rc'}

uses
  FastMM4,
  ComServ,
  AudioSheet_TLB in 'AudioSheet_TLB.pas',
  AShellExt in 'AShellExt.pas' {AudioSheet: CoClass},
  AudioSheet_Page in 'AudioSheet_Page.pas' {FmPage};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}
{$R *.RES}

begin
end.
