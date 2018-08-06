unit URMCRomplerFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, URMCControls,
  URMCEmptyPanel;

type
  TOnUIChanged = procedure ( Sender: TObject; Index, Value: Integer) of object;

  TRMCRomplerFrame = class(TForm)
    RMCElement1: TRMCElement;
    Label1: TLabel;
    RMCEmptyPanel1: TRMCEmptyPanel;
    RMCElement2: TRMCElement;
    RMCElement3: TRMCElement;
    RMCElement4: TRMCElement;
    RMCElement5: TRMCElement;
    RMCElement6: TRMCElement;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
