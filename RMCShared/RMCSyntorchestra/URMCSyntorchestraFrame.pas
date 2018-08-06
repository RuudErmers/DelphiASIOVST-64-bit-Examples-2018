unit URMCSyntorchestraFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, URMCControls,
  URMCEmptyPanel;

type
  TOnUIChanged = procedure ( Sender: TObject; Index, Value: Integer) of object;

  TRMCSyntorchestraFrame = class(TForm)
    RMCElement1: TRMCElement;
    RMCEmptyPanel1: TRMCEmptyPanel;
    Label1: TLabel;
    RMCElement2: TRMCElement;
    RMCElement3: TRMCElement;
    RMCElement4: TRMCElement;
    RMCElement5: TRMCElement;
    RMCElement6: TRMCElement;
    RMCElement7: TRMCElement;
    RMCElement8: TRMCElement;
    RMCElement9: TRMCElement;
    RMCElement10: TRMCElement;
    RMCElement11: TRMCElement;
    RMCElement12: TRMCElement;
    RMCElement13: TRMCElement;
    RMCElement14: TRMCElement;
    RMCElement15: TRMCElement;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    RMCElement17: TRMCElement;
    RMCElement18: TRMCElement;
    Label18: TLabel;
    Label19: TLabel;
    RMCElement16: TRMCElement;
    Label20: TLabel;
    RMCElement19: TRMCElement;
    RMCElement20: TRMCElement;
    RMCElement21: TRMCElement;
    RMCElement22: TRMCElement;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    RMCElement23: TRMCElement;
    RMCElement24: TRMCElement;
    RMCElement25: TRMCElement;
    RMCElement26: TRMCElement;
    RMCElement27: TRMCElement;
    RMCElement28: TRMCElement;
    RMCElement29: TRMCElement;
    RMCElement30: TRMCElement;
    RMCElement31: TRMCElement;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
  private
    { Private declarations }
    procedure CBUIChanged(Sender: TObject; Index, Value: Integer);
  public
    { Public declarations }
    { property } OnUIChanged:TOnUIChanged;
  end;

var
  RMCSyntorchestraFrame: TRMCSyntorchestraFrame;

implementation

{$R *.dfm}

uses UVirtCC;

procedure TRMCSyntorchestraFrame.CBUIChanged( Sender: TObject; Index, Value: Integer);
begin
  if assigned(OnUIChanged) then
    OnUIChanged(Sender,Index,Value);
end;

end.
