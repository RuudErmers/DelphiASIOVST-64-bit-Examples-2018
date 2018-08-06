library AsioDriver;

uses
  ComServ,
  AsioDriverMain in 'AsioDriverMain.pas',
  AsioDriverMainCPanel in 'AsioDriverMainCPanel.pas' {DriverTestCP};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
end.
