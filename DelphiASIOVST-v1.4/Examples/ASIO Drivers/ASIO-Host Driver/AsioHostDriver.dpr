library AsioHostDriver;

uses
  ComServ,
  AsioHostDriverMain in 'AsioHostDriverMain.pas',
  AsioHostDriverControlPanel in 'AsioHostDriverControlPanel.pas' {FmAsioDriverControlPanel};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
end.
