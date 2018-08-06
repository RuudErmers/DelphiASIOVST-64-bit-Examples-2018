unit AsioHostDriverControlPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  ToolWin, ComCtrls, StdCtrls, Menus, DAV_ASIODriver, XPMan;

type
  TFmAsioDriverControlPanel = class(TDavASIODriverCP)
    LbDriver: TLabel;
    CbDriver: TComboBox;
    BtControlPanel: TButton;
    procedure BtControlPanelClick(Sender: TObject);
    procedure CbDriverChange(Sender: TObject);
  public
    procedure PanelLoaded; override;
  end;

implementation

uses
  Math, Dialogs, Registry, DAV_AsioHost, AsioHostDriverMain;

{$R *.dfm}

procedure TFmAsioDriverControlPanel.PanelLoaded;     
begin
  if not assigned(Driver) then exit;
  Caption:=Driver.GetDriverName + ' (Version ' + inttostr(Driver.GetDriverVersion) + ')';
  cbDriver.Items:=TAsioHostDriver(Driver).AsioHost.DriverList;

  CbDriver.ItemIndex := TAsioHostDriver(Driver).AsioHost.DriverIndex;
end;

procedure TFmAsioDriverControlPanel.CbDriverChange(Sender: TObject);
begin
 if not assigned(Driver) then exit;
 
 BtControlPanel.Enabled := False;
 CbDriver.ItemIndex := CbDriver.Items.IndexOf(CbDriver.Text);
 if CbDriver.ItemIndex >= 0 then
  begin
   TAsioHostDriver(Driver).AsioHost.DriverIndex := CbDriver.ItemIndex;

   BtControlPanel.Enabled := True;
  end;
end;

procedure TFmAsioDriverControlPanel.BtControlPanelClick(Sender: TObject);
begin   
  if not assigned(Driver) then exit;
  TAsioHostDriver(Driver).AsioHost.ControlPanel;
end;

end.
