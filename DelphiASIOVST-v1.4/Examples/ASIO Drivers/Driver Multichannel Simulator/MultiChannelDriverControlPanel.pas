unit MultiChannelDriverControlPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  ToolWin, ComCtrls, StdCtrls, Menus, DAV_ASIODriver;

type
  TFmAsioDriverControlPanel = class(TDavASIODriverCP)
    BtControlPanel: TButton;
    btnApply: TButton;
    btnClose: TButton;
    CbDriver: TComboBox;
    cbIn01: TComboBox;
    cbIn02: TComboBox;
    cbIn03: TComboBox;
    cbIn04: TComboBox;
    cbIn05: TComboBox;
    cbIn06: TComboBox;
    cbIn07: TComboBox;
    cbIn08: TComboBox;
    cbIn09: TComboBox;
    cbIn10: TComboBox;
    cbIn11: TComboBox;
    cbIn12: TComboBox;
    cbIn13: TComboBox;
    cbIn14: TComboBox;
    cbIn15: TComboBox;
    cbIn16: TComboBox;
    cbOut01: TComboBox;
    cbOut02: TComboBox;
    cbOut03: TComboBox;
    cbOut04: TComboBox;
    cbOut05: TComboBox;
    cbOut06: TComboBox;
    cbOut07: TComboBox;
    cbOut08: TComboBox;
    cbOut09: TComboBox;
    cbOut10: TComboBox;
    cbOut11: TComboBox;
    cbOut12: TComboBox;
    cbOut13: TComboBox;
    cbOut14: TComboBox;
    cbOut15: TComboBox;
    cbOut16: TComboBox;
    LbDriver: TLabel;
    lbIn01: TLabel;
    lbIn02: TLabel;
    lbIn03: TLabel;
    lbIn04: TLabel;
    lbIn05: TLabel;
    lbIn06: TLabel;
    lbIn07: TLabel;
    lbIn08: TLabel;
    lbIn09: TLabel;
    lbIn10: TLabel;
    lbIn11: TLabel;
    lbIn12: TLabel;
    lbIn13: TLabel;
    lbIn14: TLabel;
    lbIn15: TLabel;
    lbIn16: TLabel;
    lbOut01: TLabel;
    lbOut02: TLabel;
    lbOut03: TLabel;
    lbOut04: TLabel;
    lbOut05: TLabel;
    lbOut06: TLabel;
    lbOut07: TLabel;
    lbOut08: TLabel;
    lbOut09: TLabel;
    lbOut10: TLabel;
    lbOut11: TLabel;
    lbOut12: TLabel;
    lbOut13: TLabel;
    lbOut14: TLabel;
    lbOut15: TLabel;
    lbOut16: TLabel;
    PageInputs: TTabSheet;
    PageOutputs: TTabSheet;
    pcAssignments: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure CbDriverChange(Sender: TObject);
    procedure InputSettingsChanged(Sender: TObject);
    procedure OutputSettingsChanged(Sender: TObject);
  public
    InputBoxes: array [1..16] of TComboBox;
    OutputBoxes: array [1..16] of TComboBox;
    ModifiedSelections: Boolean;
    procedure PanelLoaded; override;
    procedure UpdateSelection;
    procedure CopyToSettings;
  end;

implementation

uses
  Math, Dialogs, Registry, DAV_AsioHost, MultiChannelDriverMain;

{$R *.dfm}

procedure TFmAsioDriverControlPanel.FormCreate(Sender: TObject);
var
  i: Integer;
begin
 // inputs page
 with PageInputs do
  for i := 0 to ControlCount - 1 do
   if Controls[i] is TComboBox then
    if Controls[i].Tag in [1..16]
     then InputBoxes[Controls[i].Tag] := TComboBox(Controls[i]);

 // outputs page
 with PageOutputs do
  for i := 0 to ControlCount - 1 do
   if Controls[i] is TComboBox then
    if Controls[i].Tag in [1..16]
     then OutputBoxes[Controls[i].Tag] := TComboBox(Controls[i]);
end;

procedure TFmAsioDriverControlPanel.PanelLoaded;
var
  i: Integer;
begin
  if not Assigned(Driver) then Exit;

  Caption := Driver.GetDriverName + ' (Version ' + IntToStr(Driver.GetDriverVersion) + ')';
  cbDriver.Items := TAsioHostDriver(Driver).AsioHost.DriverList;

  pcAssignments.Visible := False;  // no redraw for 16 comboboxes

  cbIn01.Clear;
  cbIn01.Items.Add('- none -');
  cbIn01.Items.Add('- zero -');

  cbOut01.Clear;
  cbOut01.Items.Add('- none -');
  cbOut01.Items.Add('- zero -');
  with TAsioHostDriver(Driver).Asiohost do
  begin
   for i := 0 to InputChannelCount - 1 do cbIn01.Items.Add(string(InputChannelInfos[i].Name));
   for i := 0 to OutputChannelCount - 1 do cbOut01.Items.Add(string(OutputChannelInfos[i].Name));
  end;

  for i := 2 to 16 do InputBoxes[i].Items.Assign(cbIn01.Items);
  for i := 2 to 16 do OutputBoxes[i].Items.Assign(cbOut01.Items);

  UpdateSelection;
  pcAssignments.Visible := True;
end;

procedure TFmAsioDriverControlPanel.InputSettingsChanged(Sender: TObject);
var
  i: Integer;
begin
 if not (Sender is TComboBox) then Exit;

 with TComboBox(Sender) do
  if ItemIndex < 1
   then for i := Tag + 1 to 16 do InputBoxes[i].Enabled := False
   else for i := Tag + 1 to 16 do
    if (InputBoxes[i - 1].ItemIndex > 0) and OutputBoxes[i - 1].Enabled
     then InputBoxes[i].Enabled := True;

  ModifiedSelections := True;
end;

procedure TFmAsioDriverControlPanel.OutputSettingsChanged(Sender: TObject);
var
  i : Integer;
begin
  if not (Sender is TComboBox) then Exit;

  with TComboBox(Sender) do
    if ItemIndex < 1 then
    begin
      for i := Tag + 1 to 16 do OutputBoxes[i].Enabled := False;
    end else
      for i := Tag + 1 to 16 do
       if (OutputBoxes[i - 1].ItemIndex > 0) and OutputBoxes[i - 1].Enabled then OutputBoxes[i].Enabled := True;

  ModifiedSelections := True;
end;

procedure TFmAsioDriverControlPanel.UpdateSelection;
var
  i: Integer;
begin
  with TAsioHostDriver(Driver).Settings do
  begin
    CbDriver.ItemIndex := DriverIndex;

    for i := 1 to 16 do
    begin
      InputBoxes[i].ItemIndex := InputAssignment[i];
      InputBoxes[i].Enabled := (i = 1) or ( (InputAssignment[i - 1] > 0) and InputBoxes[i - 1].Enabled);

      OutputBoxes[i].ItemIndex := OutputAssignment[i];
      OutputBoxes[i].Enabled := (i = 1) or ( (OutputAssignment[i - 1] > 0) and OutputBoxes[i - 1].Enabled);
    end;
  end;
  ModifiedSelections := false;
end;

procedure TFmAsioDriverControlPanel.btnCloseClick(Sender: TObject);
begin
  UpdateSelection;
  Close;
end;

procedure TFmAsioDriverControlPanel.CopyToSettings;
var
  i: Integer;
begin
  with TAsioHostDriver(Driver).Settings do
  begin
    DriverIndex := CbDriver.ItemIndex;

    for i := 1 to 16 do
    begin
      InputAssignment[i] := InputBoxes[i].ItemIndex;
      OutputAssignment[i] := OutputBoxes[i].ItemIndex;
    end;
  end;
end;

procedure TFmAsioDriverControlPanel.btnApplyClick(Sender: TObject);
begin
  if not Assigned(Driver) then exit;

  CopyToSettings;
  TAsioHostDriver(Driver).SaveAndReset(self);
end;

procedure TFmAsioDriverControlPanel.CbDriverChange(Sender: TObject);
begin
 if not Assigned(Driver) then exit;

 CopyToSettings;
 TAsioHostDriver(Driver).SaveAndReset(self);
end;

procedure TFmAsioDriverControlPanel.BtControlPanelClick(Sender: TObject);
var
  r: Integer;
begin
  if not Assigned(Driver) then exit;

  if ModifiedSelections then
  begin
   r := MessageBox(handle,'Save modified settings?', 'Settings modified', MB_YESNOCANCEL);
   case r of
     IDYES: begin
       CopyToSettings;
       TAsioHostDriver(Driver).SaveDriverSettings;
     end;
     IDCANCEL: exit;
   end;
  end;
  
  TAsioHostDriver(Driver).AsioHost.ControlPanel;
end;

end.
