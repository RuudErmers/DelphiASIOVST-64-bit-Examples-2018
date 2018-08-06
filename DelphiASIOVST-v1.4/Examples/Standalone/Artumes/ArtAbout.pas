unit ArtAbout;

interface

uses 
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Classes, Graphics, Forms, 
  Controls, StdCtrls, Buttons, ExtCtrls;

type
  TFmAbout = class(TForm)
    Panel: TPanel;
    OKButton: TButton;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmAbout: TFmAbout;

implementation

{$R *.dfm}

end.
 
