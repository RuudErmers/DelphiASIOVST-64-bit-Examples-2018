library MultiChannelDriver;

uses
  ComServ,
  MultiChannelDriverMain in 'MultiChannelDriverMain.pas',
  MultiChannelDriverControlPanel in 'MultiChannelDriverControlPanel.pas' {FmAsioDriverControlPanel};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
end.
