unit DrvrHostIntCPanel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_ASIODriver, DAV_ASIODriverInterceptor, StdCtrls;

type
  TInterceptorTestCP = class(TDavASIODriverCP)
    btnDone: TButton;
    cbDrivers: TComboBox;
    btnControlPanel: TButton;
    procedure cbDriversChange(Sender: TObject);
    procedure btnControlPanelClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    procedure PanelLoaded; override;
  end;

implementation

{$R *.dfm}

procedure TInterceptorTestCP.btnControlPanelClick(Sender: TObject);
begin
  if assigned(Driver) then TDavASIOInterceptor(Driver).DriverControlPanel;
end;

procedure TInterceptorTestCP.cbDriversChange(Sender: TObject);
begin
  TDavASIOInterceptor(Driver).DriverIndex := cbDrivers.ItemIndex;
end;

procedure TInterceptorTestCP.PanelLoaded;
begin
  if not assigned(Driver) then exit;
  Caption:=Driver.GetDriverName + ' (Version ' + inttostr(Driver.GetDriverVersion) + ')';
  cbDrivers.Items:=TDavASIOInterceptor(Driver).DriverNames;

  cbDrivers.ItemIndex := TDavASIOInterceptor(Driver).DriverIndex;
end;

end.
