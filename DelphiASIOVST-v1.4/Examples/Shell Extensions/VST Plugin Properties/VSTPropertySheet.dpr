library VSTPropertySheet;

{$R 'VSTPSheet.res' 'VSTPSheet.rc'}

uses
  ComServ,
  VSTPropertySheet_TLB in 'VSTPropertySheet_TLB.pas',
  VSTShellExt in 'VSTShellExt.pas' {VSTPluginPropertySheet: CoClass},
  VSTPropertySheet_Page in 'VSTPropertySheet_Page.pas' {FmPage};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}
{$R *.RES}

begin
end.
