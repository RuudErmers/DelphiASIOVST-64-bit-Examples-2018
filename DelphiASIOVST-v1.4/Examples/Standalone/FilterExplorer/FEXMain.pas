unit FEXMain;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF}Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls, ToolWin, ComCtrls, ExtCtrls;

type
  TFmFilterExplorer = class(TForm)
    ToolBar: TToolBar;
    BtNew: TButton;
    BtLoad: TButton;
    BtSave: TButton;
    BtExport: TButton;
    BtReset: TButton;
    BtInfo: TButton;
    BtErrors: TButton;
    BtFunct: TButton;
    BtConfig: TButton;
    BtHelp: TButton;
    BtExit: TButton;
    GBCoefficients: TGroupBox;
    Lbt0: TLabel;
    Lbt1: TLabel;
    Lbt2: TLabel;
    Lbt3: TLabel;
    Lbt4: TLabel;
    Lbt5: TLabel;
    Lba0: TLabel;
    Lba1: TLabel;
    Lba2: TLabel;
    Lba3: TLabel;
    Lba4: TLabel;
    Lba5: TLabel;
    Lbb1: TLabel;
    Lbb2: TLabel;
    Lbb3: TLabel;
    Lbb4: TLabel;
    Lbb5: TLabel;
    Edt0: TEdit;
    Edt1: TEdit;
    Edt2: TEdit;
    Edt3: TEdit;
    Edt4: TEdit;
    Edt5: TEdit;
    Eda5: TEdit;
    Eda4: TEdit;
    Eda3: TEdit;
    Eda2: TEdit;
    Eda1: TEdit;
    Eda0: TEdit;
    Edb5: TEdit;
    Edb4: TEdit;
    Edb3: TEdit;
    Edb2: TEdit;
    Edb1: TEdit;
    LbTemp: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    GBControls: TGroupBox;
    SBc0: TScrollBar;
    SBc1: TScrollBar;
    SBc2: TScrollBar;
    SBc3: TScrollBar;
    SBc4: TScrollBar;
    SBc5: TScrollBar;
    Lbc0: TLabel;
    Lbc1: TLabel;
    Lbc2: TLabel;
    Lb3: TLabel;
    Lbc4: TLabel;
    Lbc5: TLabel;
    GBPlaneView: TGroupBox;
    PBPlaneView: TPaintBox;
    LbNumberOfZeros: TLabel;
    LbNumberOfPoles: TLabel;
    GBPlotWindow: TGroupBox;
    GBInfo: TGroupBox;
    InfoWindow: TMemo;
    procedure BtExitClick(Sender: TObject);
    procedure BtSaveClick(Sender: TObject);
    procedure BtLoadClick(Sender: TObject);
    procedure BtExportClick(Sender: TObject);
    procedure BtResetClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    procedure LoadFromFile(Filename: TFilename);
    procedure SaveToFile(Filename: TFilename);
  end;

var
  FmFilterExplorer: TFmFilterExplorer;

implementation

{$R *.dfm}

procedure TFmFilterExplorer.BtExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmFilterExplorer.BtExportClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := '.pas';
   Filter := 'Pascal Unit (*.pas)|*.pas';
   if Execute
    then SaveToFile(Filename);
  finally
   Free;
  end;
end;

procedure TFmFilterExplorer.BtLoadClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.fex';
   Filter := 'Filter Explorer (*.fex)|*.fex';
   Options := Options + [ofFileMustExist];
   if Execute
    then LoadFromFile(Filename);
  finally
   Free;
  end;
end;

procedure TFmFilterExplorer.BtResetClick(Sender: TObject);
begin
 Edt0.Text := '0';
 Edt1.Text := '0';
 Edt2.Text := '0';
 Edt3.Text := '0';
 Edt4.Text := '0';
 Edt5.Text := '0';

 Eda0.Text := '0';
 Eda1.Text := '0';
 Eda2.Text := '0';
 Eda3.Text := '0';
 Eda4.Text := '0';
 Eda5.Text := '0';

 Edb1.Text := '0';
 Edb2.Text := '0';
 Edb3.Text := '0';
 Edb4.Text := '0';
 Edb5.Text := '0';

 SBc0.Position := 0;
 SBc1.Position := 0;
 SBc2.Position := 0;
 SBc3.Position := 0;
 SBc4.Position := 0;
end;

procedure TFmFilterExplorer.BtSaveClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := '.fex';
   Filter := 'Filter Explorer (*.fex)|*.fex';
   if Execute
    then SaveToFile(Filename);
  finally
   Free;
  end;
end;

procedure TFmFilterExplorer.LoadFromFile(Filename: TFilename);
begin

end;

procedure TFmFilterExplorer.SaveToFile(Filename: TFilename);
begin

end;

end.
