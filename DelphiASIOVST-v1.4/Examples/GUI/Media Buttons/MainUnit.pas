unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DAV_GuiMediaButton;

type
  TForm1 = class(TForm)
    BtStop: TGuiMediaButton;
    BtPlay: TGuiMediaButton;
    BtPause: TGuiMediaButton;
    BtRecord: TGuiMediaButton;
    BtPrevious: TGuiMediaButton;
    BtNext: TGuiMediaButton;
    BtFastBackward: TGuiMediaButton;
    BtFastForward: TGuiMediaButton;
    BtStopSmall: TGuiMediaButton;
    BtPlaySmall: TGuiMediaButton;
    BtPauseSmall: TGuiMediaButton;
    BtRecordSmall: TGuiMediaButton;
    BtFastForwardSmall: TGuiMediaButton;
    BtFastBackwardSmall: TGuiMediaButton;
    BtNextSmall: TGuiMediaButton;
    BtPreviousSmall: TGuiMediaButton;
    procedure BtPauseClick(Sender: TObject);
    procedure BtPlayClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BtPauseClick(Sender: TObject);
begin
 BtPause.OutlineWidth := BtPause.OutlineWidth + 1;
end;

procedure TForm1.BtPlayClick(Sender: TObject);
begin
 BtPlay.OutlineWidth := BtPlay.OutlineWidth + 1;
end;

end.

