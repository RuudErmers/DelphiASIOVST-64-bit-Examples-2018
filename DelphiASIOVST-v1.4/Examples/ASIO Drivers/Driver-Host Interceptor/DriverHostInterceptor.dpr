library DriverHostInterceptor;

uses
  ComServ,
  DrvrHostIntMain in 'DrvrHostIntMain.pas',
  DrvrHostIntCPanel in 'DrvrHostIntCPanel.pas' {DriverTestCP};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
end.
