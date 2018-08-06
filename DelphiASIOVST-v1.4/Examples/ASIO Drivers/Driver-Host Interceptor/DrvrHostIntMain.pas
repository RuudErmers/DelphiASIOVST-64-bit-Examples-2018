unit DrvrHostIntMain;

{$I DAV_Compiler.inc}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Classes, ComObj, DAV_ASIO, DAV_ASIODriver, DAV_ASIODriverInterceptor;

const
  DIntercept_guid: TGUID = '{A8DD45FD-34CC-4996-9695-CDD2AE483B48}';
  DIntercept_classname = 'DAV Interceptor';
  DIntercept_name = 'DAV Interceptor';
  DIntercept_version = 1;

type
  IInterceptorTest = interface(IDavASIODriverInterface)
    ['{A8DD45FD-34CC-4996-9695-CDD2AE483B48}']
  end;

  TInterceptorTest = class(TDavASIOInterceptor)
  protected
    procedure InitializeDriverParams; override;

    procedure LoadDriverSettings; override;
    procedure SaveDriverSettings; override;
  end;

  TTestTCWrapper = class(TDavASIOTCWrapper, IInterceptorTest)
  protected
    function GetDriverClass: TTDavASIODriver; override;
  end;


implementation

uses
  ComServ,Registry,DrvrHostIntCPanel;

function TTestTCWrapper.GetDriverClass: TTDavASIODriver;
begin
  result := TInterceptorTest;
end;


procedure TInterceptorTest.InitializeDriverParams;
begin
  SetDriverName(DIntercept_name);
  SetDriverVersion(DIntercept_version);

  SetControlPanelClass(TInterceptorTestCP);
  //DriverIndex := 1; //Test
end;

procedure TInterceptorTest.LoadDriverSettings;
begin
  with TRegistry.Create do
  try
    RootKey:=HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly('software\asio\' + DIntercept_name) then
      if ValueExists('DriverIndex') then DriverIndex := ReadInteger('DriverIndex');
  finally
    Free;
  end;
end;

procedure TInterceptorTest.SaveDriverSettings;
begin
  with TRegistry.Create do
  try
    RootKey:=HKEY_LOCAL_MACHINE;
    if OpenKey('software\asio\' + DIntercept_name, true) then WriteInteger('DriverIndex', DriverIndex);
  finally
    Free;
  end;
end;

initialization
  TDavAsioDriverFactory.Create(ComServer, TTestTCWrapper, DIntercept_guid,
    DIntercept_classname, DIntercept_name, ciSingleInstance, tmApartment);

end.
