unit DAV_AsioDriverBasicModule;

// This unit implements the basic AsioDriver-Plugin <--> Host communications

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Common, DAV_Asio;

type
  TBasicAsioDriverModuleClass = class of TBasicAsioDriverModule;
  TBasicAsioDriverModule = class({$IFDEF UseDelphi}TDataModule{$ELSE}TComponent{$ENDIF})
  private
    {$IFNDEF UseDelphi}
    FOnDestroy: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    {$ENDIF}
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFNDEF UseDelphi}
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    {$ENDIF}
  end;

  EAsioDriverError = class(Exception);

implementation

{ TBasicAsioDriverModule }

constructor TBasicAsioDriverModule.Create(AOwner: TComponent);
begin
 inherited;
 // not implemented
end;

destructor TBasicAsioDriverModule.Destroy;
begin
 // not implemented
 inherited;
end;

end.
